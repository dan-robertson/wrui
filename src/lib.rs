#![recursion_limit = "128"]
#![feature(libc)]

extern crate libc;
extern crate gleam;
extern crate glutin;
extern crate serde;
#[macro_use]
extern crate serde_derive;
extern crate serde_json;

extern crate euclid;
extern crate app_units;

extern crate servo_arc;
extern crate ipc_channel;
extern crate range;
extern crate style;
extern crate net_traits;
extern crate gfx;

extern crate unicode_bidi;
extern crate unicode_script;
extern crate ordered_float;

extern crate webrender;
extern crate webrender_api;


pub mod fake_resource_thread;
pub mod serde_event;

use std::slice;
use std::mem;
use std::rc::Rc;
use std::cell::RefCell;
use libc::{c_char, uint32_t, int32_t, size_t};
use std::ffi::{CStr,CString};
use gleam::gl;

use app_units::Au;
use servo_arc::Arc as ServoArc;
use std::sync::Mutex;

use style::Atom;
use style::properties::style_structs::Font as FontStyle;
use gfx::font_context;
use gfx::font_context::FontContext;
use gfx::font_cache_thread::FontCacheThread;
use gfx::text;
use gfx::font;
use gfx::font::Font;

use webrender_api::{DocumentId, PipelineId, RenderApi, GlyphInstance,
                    RenderNotifier, DeviceUintSize, DeviceUintPoint,
                    DeviceUintRect, ResourceUpdates, Epoch,
                    LayoutSize, LayoutRect, LayoutPoint, ColorF,
                    BorderWidths, BorderDetails, NormalBorder,
                    BorderSide, BorderRadius, PrimitiveInfo,
                    EdgeAaSegmentMask, LocalClip, ComplexClipRegion,
                    ClipMode, ClipId, ScrollSensitivity,
                    LayoutVector2D, WorldPoint, ScrollLocation,
                    ScrollEventPhase, ScrollPolicy, TransformStyle,
                    MixBlendMode};

// used as render notifier
pub struct WRWindow {
    window_proxy: glutin::WindowProxy,
}

impl WRWindow {
    fn new(window_proxy: glutin::WindowProxy) -> WRWindow {
        WRWindow {
            window_proxy: window_proxy,
        }
    }
}

impl RenderNotifier for WRWindow {
    fn new_frame_ready(&self) {
        // maybe #[cfg(not(target_os = "android"))] ?
        self.window_proxy.wakeup_event_loop();
    }

    fn new_scroll_frame_ready(&self, _composite_needed: bool) {
        // and here
        self.window_proxy.wakeup_event_loop();
    }
    
    fn clone(&self) -> Box<RenderNotifier> {
        Box::new(WRWindow::new(self.window_proxy.clone()))
    }
}

pub struct WRUIWindow {
    renderer: webrender::Renderer,
    api: RenderApi,
    font_context: ServoArc<Mutex<FontContext>>,
    document: DocumentId,
    window: glutin::Window,
    epoch: Epoch,
    last_dl_length: usize,
    last_window_size: DeviceUintSize,
}

/// Create an object for a window. This should be passed a function to
/// handle events for the window and a function to generate a display
/// list.
#[no_mangle]
pub extern fn wrui_new_window(title: *const c_char, width: uint32_t, height: uint32_t)
                              -> *mut WRUIWindow {
    let title = unsafe {
        if title.is_null() {
            None
        } else {
            Some(CStr::from_ptr(title))
        }
    };
    let builder = glutin::WindowBuilder::new()
        .with_multitouch()
        .with_gl(glutin::GlRequest::GlThenGles {
            opengl_version:   (3,2),
            opengles_version: (3,0)
        });
    let builder = match title {
        None => builder,
        Some(s) => builder.with_title(s.to_str().unwrap()),
    };
    let builder = if width > 0 && height > 0 {
        builder.with_dimensions(width, height)
    } else {
        builder
    };
    let window = builder.build().unwrap();
    
    unsafe {
        window.make_current().ok();
    }
    let gl = match gl::GlType::default() {
        gl::GlType::Gl => unsafe { 
            gl::GlFns::load_with(|symbol| window.get_proc_address(symbol) as *const _) },
        gl::GlType::Gles => unsafe {
            gl::GlesFns::load_with(|symbol| window.get_proc_address(symbol) as *const _) },
    };

    let options = webrender::RendererOptions {
        device_pixel_ratio: window.hidpi_factor(),
        resource_override_path: None,
        enable_aa: true,
        debug: true,
        precache_shaders: true,
        clear_framebuffer: true, //maybe?
        ..Default::default()
    };

    let (mut renderer, sender) = webrender::Renderer::new(
        gl,
        Box::new(WRWindow::new(window.create_window_proxy())),
        options).unwrap();

    let fake_resource_thread = fake_resource_thread::new_fake_resource_thread();
    let font_cache_thread = FontCacheThread::new(fake_resource_thread, sender.create_api());

    let api = sender.create_api();
    let (width, height) = window.get_inner_size_pixels().unwrap();
    
    let size = DeviceUintSize::new(width, height);
    let document_id = api.add_document(size);
    let pipeline_id = PipelineId(0,0);

    api.set_root_pipeline(document_id, pipeline_id);

    let mut win = WRUIWindow {
        renderer: renderer,
        api: api,
        font_context: ServoArc::new(Mutex::new(FontContext::new(font_cache_thread))),
        document: document_id,
        window: window,
        epoch: Epoch(0),
        last_dl_length: 0,
        last_window_size: size,
    };

    Box::into_raw(Box::new(win))
}

#[no_mangle]
pub extern fn wrui_close_window(win: *mut WRUIWindow) -> () {
    let win = unsafe {
        if win.is_null() { return; }
        Box::from_raw(win)
    };
    win.api.shut_down();
    win.window.hide();
    win.renderer.deinit();
}

#[no_mangle]
pub extern fn wrui_get_event_json(win: *mut WRUIWindow, block: i32) -> *mut c_char {
    let window = unsafe {
        assert!(!win.is_null());
        &mut (*win).window
    };
    let event = if block != 0 {
        window.wait_events().next()
    } else {
        window.poll_events().next()
    };
    let str = serde_json::to_string(&event.map(serde_event::SerializableEvent))
        .ok().unwrap_or(String::from(""));
    let c_str = CString::new(str).unwrap();
    c_str.into_raw()
}

#[no_mangle]
pub extern fn wrui_get_event_sexp(win: *mut WRUIWindow, block: i32) -> *mut c_char {
    use serde::Serialize;
    let window = unsafe {
        assert!(!win.is_null());
        &mut (*win).window
    };
    let event = if block != 0 {
        window.wait_events().next()
    } else {
        window.poll_events().next()
    };
    let x = event.map(serde_event::SerializableEvent);
    let mut writer = Vec::with_capacity(128);
    let c_str = match x.serialize(&mut serde_event::SExprSerializer::new(&mut writer)) {
        Ok(_) => unsafe { CString::from_vec_unchecked(writer) },
        Err(_) => CString::new("").unwrap(),
    };
    c_str.into_raw()
}

#[no_mangle]
pub extern fn wrui_free_event(s: *mut c_char) -> () {
    unsafe {
        if !s.is_null() {
            CString::from_raw(s);
        }
    }
}

pub struct DisplayListBuilder {
    builder: webrender_api::DisplayListBuilder,
    font_context: ServoArc<Mutex<FontContext>>,
    epoch: Epoch,
}

impl DisplayListBuilder {
    fn new(dlb: webrender_api::DisplayListBuilder, font_context: ServoArc<Mutex<FontContext>>,
           epoch: Epoch)
           -> DisplayListBuilder {
        DisplayListBuilder {
            builder: dlb,
            font_context: font_context,
            epoch: epoch,
        }
    }
}

#[no_mangle]
pub extern fn wrui_display_list_builder(window: *mut WRUIWindow, width: *mut f32, height: *mut f32) -> *mut DisplayListBuilder {
    let win = unsafe {
        assert!(!window.is_null());
        assert!(!width.is_null());
        assert!(!height.is_null());
        &mut *window
    };
    let window = &win.window;
    let (lwidth, lheight) = window.get_inner_size_points().unwrap();
    let lsize = LayoutSize::new(lwidth as f32, lheight as f32);
    unsafe {
        *width = lwidth as f32;
        *height = lheight as f32;
    }
    let mut dlb = DisplayListBuilder::new(
        webrender_api::DisplayListBuilder::with_capacity(PipelineId(0,0), lsize, win.last_dl_length),
        win.font_context.clone(),
        win.epoch);
    Box::into_raw(Box::new(dlb))
}

#[no_mangle]
pub extern fn display_list_builder_free(ptr: *mut DisplayListBuilder) -> () {
    if !ptr.is_null() {
        unsafe {
            Box::from_raw(ptr);
        }
    }
}

#[no_mangle]
pub extern fn display_list_builder_build(window: *mut WRUIWindow, ptr: *mut DisplayListBuilder) -> () {
    let win = unsafe {
        assert!(!window.is_null());
        &mut *window
    };
    let dlb = unsafe {
        assert!(!ptr.is_null());
        Box::from_raw(ptr)
    };

    let (dwidth, dheight) = win.window.get_inner_size_pixels().unwrap();
    let dsize = DeviceUintSize::new(dwidth, dheight);
    let (lwidth, lheight) = win.window.get_inner_size_points().unwrap();
    let lsize = LayoutSize::new(lwidth as f32, lheight as f32);
    if dsize != win.last_window_size {
        win.last_window_size = dsize;
        win.api.set_window_parameters(win.document, dsize,
                                      DeviceUintRect::new(
                                          DeviceUintPoint::zero(),
                                          dsize),
                                      win.window.hidpi_factor());
    }
    win.last_dl_length = dlb.builder.data.len();
    win.epoch = Epoch(win.epoch.0 + 1);
    win.api.set_display_list(win.document,
                             dlb.epoch,
                             Some(ColorF::new(1.0, 1.0, 1.0, 1.0)),
                             lsize,
                             dlb.builder.finalize(),
                             true,
                             ResourceUpdates::new());
    win.api.generate_frame(win.document, None);
    win.renderer.update();
    win.renderer.render(dsize);
    win.window.swap_buffers().ok();
}

pub struct ShapedText {
    run: text::TextRun,
    range: range::Range<text::glyph::ByteIndex>,
}

impl ShapedText {
    fn new(run: text::TextRun, range: range::Range<text::glyph::ByteIndex>) -> ShapedText {
        ShapedText {
            run: run,
            range: range,
        }
    }
}

fn layout_rect(x: f32, y: f32, w: f32, h: f32) -> LayoutRect {
    LayoutRect::new(LayoutPoint::new(x,y), LayoutSize::new(w,h))
}


/// # with_clip!{
/// #   display_list_builder_push_name, display_list_builder_push_name_c,
/// #   display_list_builder_push_name_cc, display_list_builder_push_name_t
/// #   display_list_builder_push_name_tc, display_list_builder_push_name_tcc,
/// #   fn (dlb, primitive_info, args...) -> result {
/// #     body
/// #   }
/// # }
///
/// the macro mutably binds `primitive_info` to function that takes a rect and returns the
/// appropriate PrimitiveInfo struct. (i.e. fn primitive_info(x: LayoutRect) -> PrimitiveInfo)
///
/// generates public external non-mangled functions:
/// # display_list_builder_push_name(ptr: *mut DisplayListBuilder, args...)
/// # display_list_builder_push_name_c(ptr: *mut DisplayListBuilder, cx:f32, cy:f32, cw:f32, ch:f32,
/// #                                  args...)
/// # display_list_builder_push_name_cc(ptr: *mut DisplayListBuilder, cx:f32, cy:f32, cw:f32, ch:f32,
/// #                                   ccx: f32, ccy: f32, ccw: f32, cch: f32,
/// #                                   cctlw: f32, cctlh: f32, cctrw: f32, cctrh: f32,
/// #                                   ccblw: f32, ccblh: f32, ccbrw: f32, ccbrh: f32,
/// #                                   args...)
/// # display_list_builder_push_name_t(ptr: *mut DisplayListBuilder, tag1: u64, tag2: u8, args...)
/// # display_list_builder_push_name_tc(ptr: *mut DisplayListBuilder, tag1: u64, tag2: u8,
/// #                                   cx:f32, cy:f32, cw:f32, ch:f32,
/// #                                   args...)
/// # display_list_builder_push_name_tcc(ptr: *mut DisplayListBuilder, tag1: u64, tag2: u8,
/// #                                    cx:f32, cy:f32, cw:f32, ch:f32,
/// #                                    ccx: f32, ccy: f32, ccw: f32, cch: f32,
/// #                                    cctlw: f32, cctlh: f32, cctrw: f32, cctrh: f32,
/// #                                    ccblw: f32, ccblh: f32, ccbrw: f32, ccbrh: f32,
/// #                                    args...)
macro_rules! with_clip {
    ($name:ident , $namec:ident , $namecc:ident , $namet:ident , $nametc:ident , $nametcc:ident ,
     fn ( $dlb:ident , $primitive_info:ident , $($arg:ident : $argt:ty),* )
          -> $result:ty $body:block) =>
    {
        with_clip!{ fn $name(ptr: *mut DisplayListBuilder, $($arg : $argt),*) -> $result {
            let $dlb = unsafe {
                assert!(!ptr.is_null());
                &mut *ptr
            };
            let $primitive_info = |rect: LayoutRect| {
                PrimitiveInfo {
                    rect: rect,
                    local_clip: LocalClip::from(rect),
                    edge_aa_segment_mask: EdgeAaSegmentMask::all(),
                    is_backface_visible: true,
                    tag: None,
                }
            };
            $body
        }
        }
        with_clip!{ fn $namec(ptr: *mut DisplayListBuilder, cx: f32, cy:f32, cw:f32, ch:f32,
                             $($arg : $argt),*) -> $result {
            let $dlb = unsafe {
                assert!(!ptr.is_null());
                &mut *ptr
            };
            let $primitive_info = |rect: LayoutRect| {
                PrimitiveInfo {
                    rect: rect,
                    local_clip: LocalClip::from(layout_rect(cx,cy,cw,ch)),
                    edge_aa_segment_mask: EdgeAaSegmentMask::all(),
                    is_backface_visible: true,
                    tag: None,
                }
            };
            $body
        }
        }
        with_clip!{ fn $namecc(ptr: *mut DisplayListBuilder, cx: f32, cy:f32, cw:f32, ch:f32,
                              ccx: f32, ccy: f32, ccw: f32, cch: f32,
                              cctlw: f32, cctlh: f32, cctrw: f32, cctrh: f32,
                              ccblw: f32, ccblh: f32, ccbrw: f32, ccbrh: f32,
                         $($arg : $argt),*) -> $result {
            let $dlb = unsafe {
                assert!(!ptr.is_null());
                &mut *ptr
            };
            let $primitive_info = |rect: LayoutRect| {
                PrimitiveInfo {
                    rect: rect,
                    local_clip: LocalClip::RoundedRect(
                        layout_rect(cx, cy, cw, ch),
                        ComplexClipRegion::new(layout_rect(ccx, ccy, ccw, cch),
                                               border_radius(cctlw, cctlh, cctrw, cctrh,
                                                             ccblw, ccblh, ccbrw, ccbrh),
                                               ClipMode::Clip)),
                    edge_aa_segment_mask: EdgeAaSegmentMask::all(),
                    is_backface_visible: true,
                    tag: None,
                }
            };
            $body
        }
        }
        with_clip!{ fn $namet(ptr: *mut DisplayListBuilder, tag1: u64, tag2: u8,
                             $($arg : $argt),*) -> $result {
            let $dlb = unsafe {
                assert!(!ptr.is_null());
                &mut *ptr
            };
            let $primitive_info = |rect: LayoutRect| {
                PrimitiveInfo {
                    rect: rect,
                    local_clip: LocalClip::from(rect),
                    edge_aa_segment_mask: EdgeAaSegmentMask::all(),
                    is_backface_visible: true,
                    tag: Some((tag1, tag2)),
                }
            };
            $body
        }
        }
        with_clip!{ fn $nametc(ptr: *mut DisplayListBuilder, tag1: u64, tag2: u8,
                              cx: f32, cy:f32, cw:f32, ch:f32,
                              $($arg : $argt),*) -> $result {
            let $dlb = unsafe {
                assert!(!ptr.is_null());
                &mut *ptr
            };
            let $primitive_info = |rect: LayoutRect| {
                PrimitiveInfo {
                    rect: rect,
                    local_clip: LocalClip::from(layout_rect(cx,cy,cw,ch)),
                    edge_aa_segment_mask: EdgeAaSegmentMask::all(),
                    is_backface_visible: true,
                    tag: Some((tag1, tag2)),
                }
            };
            $body
        }
        }
        with_clip!{ fn $nametcc(ptr: *mut DisplayListBuilder, tag1: u64, tag2: u8,
                               cx: f32, cy:f32, cw:f32, ch:f32,
                               ccx: f32, ccy: f32, ccw: f32, cch: f32,
                               cctlw: f32, cctlh: f32, cctrw: f32, cctrh: f32,
                               ccblw: f32, ccblh: f32, ccbrw: f32, ccbrh: f32,
                               $($arg : $argt),*) -> $result {
            let $dlb = unsafe {
                assert!(!ptr.is_null());
                &mut *ptr
            };
            let $primitive_info = |rect: LayoutRect| {
                PrimitiveInfo {
                    rect: rect,
                    local_clip: LocalClip::RoundedRect(
                        layout_rect(cx, cy, cw, ch),
                        ComplexClipRegion::new(layout_rect(ccx, ccy, ccw, cch),
                                               border_radius(cctlw, cctlh, cctrw, cctrh,
                                                             ccblw, ccblh, ccbrw, ccbrh),
                                               ClipMode::Clip)),
                    edge_aa_segment_mask: EdgeAaSegmentMask::all(),
                    is_backface_visible: true,
                    tag: Some((tag1, tag2)),
                }
            };
            $body
        }
        }
    };
    (fn None ($($arg:ident : $argt:ty),* )
               -> $result:ty $body:block ) => { };
    (fn $name:ident ($($arg:ident : $argt:ty),* )
               -> $result:ty $body:block ) => {
        #[no_mangle]
        pub extern fn $name($($arg: $argt),*) -> $result $body
    }
}

with_clip! { 
    display_list_builder_push_rect,
    display_list_builder_push_rect_c,
    display_list_builder_push_rect_cc,
    display_list_builder_push_rect_t,
    display_list_builder_push_rect_tc,
    display_list_builder_push_rect_tcc,
    fn (dlb, primitive_info,
        x: f32, y: f32, w: f32, h: f32,
        r: f32, g: f32, b: f32, a: f32) -> () {
        dlb.builder.push_rect(&primitive_info(layout_rect(x,y,w,h)), ColorF::new(r,g,b,a));
    }
}

#[repr(C)]
pub enum BorderStyle {
    None = 0,    
    Solid = 1,
    Double = 2,
    Dotted = 3,
    Dashed = 4,
    Hidden = 5,
    Groove = 6,
    Ridge = 7,
    Inset = 8,
    Outset = 9,
}
impl BorderStyle {
    fn to_wr(self) -> webrender_api::BorderStyle {
        use webrender_api::BorderStyle as BS;
        match self {
            BorderStyle::None =>   BS::None,
            BorderStyle::Solid =>  BS::Solid,
            BorderStyle::Double => BS::Double,
            BorderStyle::Dotted => BS::Dotted,
            BorderStyle::Dashed => BS::Dashed,
            BorderStyle::Hidden => BS::Hidden,
            BorderStyle::Groove => BS::Groove,
            BorderStyle::Ridge =>  BS::Ridge,
            BorderStyle::Inset =>  BS::Inset,
            BorderStyle::Outset => BS::Outset,
        }
    }
}

fn border_radius(tlw: f32, tlh: f32, trw: f32, trh: f32,
                 blw: f32, blh: f32, brw: f32, brh: f32) -> BorderRadius {
    BorderRadius {
        top_left:     LayoutSize::new(tlw,tlh),
        top_right:    LayoutSize::new(trw,trh),
        bottom_left:  LayoutSize::new(blw,blh),
        bottom_right: LayoutSize::new(brw,brh),
    }
}

// normal border
with_clip! { 
    display_list_builder_push_border_n,
    display_list_builder_push_border_nc,
    display_list_builder_push_border_ncc,
    display_list_builder_push_border_nt,
    display_list_builder_push_border_ntc,
    display_list_builder_push_border_ntcc,
    fn (dlb, primitive_info,
        x: f32, y: f32, w: f32, h: f32,
        leftw: f32, topw: f32, rightw: f32, bottomw: f32,
        lr: f32, lg: f32, lb: f32, la: f32, ls: BorderStyle,
        tr: f32, tg: f32, tb: f32, ta: f32, ts: BorderStyle,
        rr: f32, rg: f32, rb: f32, ra: f32, rs: BorderStyle,
        br: f32, bg: f32, bb: f32, ba: f32, bs: BorderStyle,
        tlw: f32, tlh: f32, trw: f32, trh: f32,
        blw: f32, blh: f32, brw: f32, brh: f32) -> () {
        dlb.builder.push_border(&primitive_info(layout_rect(x,y,w,h)),
                                BorderWidths { left: leftw, top: topw, right: rightw, bottom: bottomw },
                                BorderDetails::Normal(NormalBorder {
                                    left:   b(lr,lg,lb,la,ls),
                                    top:    b(tr,tg,tb,ta,ts),
                                    right:  b(rr,rg,rb,ra,rs),
                                    bottom: b(br,bg,bb,ba,bs),
                                    radius: border_radius(tlw,tlh,trw,trh,blw,blh,brw,brh),
                                }));
        
        fn b(r: f32, g: f32, b: f32, a: f32, s: BorderStyle) -> BorderSide {
            BorderSide {
                color: ColorF::new(r,g,b,a),
                style: s.to_wr()
            }
        }
    }
}


/// Get a reference to an object representing the properties required of the font.
///
/// family: the family name
/// size: font size
/// italic: should the font be slanted (italic or oblique)
/// weight: boldness. 400 is normal, 700 is bold, etc.
/// stretch: stretchiness. 1 is ultra_condensed, 5 is normal, 9 is ultra_expanded
fn describe_font(family: &str, size: Au, italic: bool, weight: u16, stretch: u8) -> ServoArc<FontStyle> {
    use style::computed_values::{font_stretch, font_weight, font_variant_caps, font_style};
    use style::computed_values::font_family;
    use style::computed_values::font_family::{FontFamily, FamilyName, FamilyNameSyntax, FontFamilyList};
    use style::values::computed::font::FontSize;
    use style::values::computed::length::NonNegativeLength;

    let mut res = FontStyle {
        font_family: font_family::T(FontFamilyList::new(vec![
            FontFamily::FamilyName(FamilyName {
                name: Atom::from(family),
                syntax: FamilyNameSyntax::Quoted,
            })])),
        font_style: if italic { font_style::T::italic } else { font_style::T::normal },
        font_variant_caps: font_variant_caps::T::normal,
        font_weight: font_weight::T::from_gecko_weight(weight),
        font_size: FontSize {
            size: NonNegativeLength::from(size), keyword_info: None
        },
        font_stretch: match stretch {
            1 => font_stretch::T::ultra_condensed,
            2 => font_stretch::T::extra_condensed,
            3 => font_stretch::T::condensed,
            4 => font_stretch::T::semi_condensed,
            5 => font_stretch::T::normal,
            6 => font_stretch::T::semi_expanded,
            7 => font_stretch::T::expanded,
            8 => font_stretch::T::extra_expanded,
            9 => font_stretch::T::ultra_expanded,
            _ => font_stretch::T::normal
        },
        hash: 0
    };
    res.compute_font_hash();
    ServoArc::new(res)
}

type WRUIFont = Rc<RefCell<Font>>;

fn get_font(font_context: &mut FontContext,
            family: &str, size: Au, italic: bool, weight: u16, stretch: u8) -> WRUIFont {
    let fonts = font_context.layout_font_group_for_style(
        describe_font(family, size, italic, weight, stretch));
    fonts.fonts[0].clone()
}


#[no_mangle]
pub extern fn wrui_find_font(dlb: *mut DisplayListBuilder,
                             family: *const c_char, ptsize: f32, italic: i32,
                             weight: u16, stretch: u8)
                             -> *mut WRUIFont {
    use std::borrow::Borrow;
    let dlb = unsafe {
        assert!(!dlb.is_null());
        &mut *dlb
    };
    let family = unsafe {
        assert!(!family.is_null());
        CStr::from_ptr(family).to_str().unwrap()
    };

    let fc : &Mutex<FontContext> = dlb.font_context.borrow();
    let font = get_font(&mut fc.lock().unwrap(), family, Au::from_f32_px(ptsize),
                        italic != 0, weight, stretch);
    let f = Box::into_raw(Box::new(font));
    f
}

#[no_mangle]
pub extern fn wrui_free_font(font: *mut WRUIFont) -> () {
    if !font.is_null() {
        unsafe {
            Box::from_raw(font);
        }
    }
}

with_clip! {
    display_list_builder_push_text,
    display_list_builder_push_text_c,
    display_list_builder_push_text_cc,
    display_list_builder_push_text_t,
    display_list_builder_push_text_tc,
    display_list_builder_push_text_tcc,
    fn (dlb, primitive_info,
        x: f32, y: f32, w: f32, h: f32, // box containing text
        font: *mut WRUIFont,
        r: f32, g: f32, b: f32, a: f32,
        x0: f32, y0: f32, //origin of first character
        text: *const c_char) -> () {

        let font = unsafe {
            assert!(!font.is_null());
            &mut *font
        };
        let text = unsafe {
            assert!(!text.is_null());
            CStr::from_ptr(text).to_str().unwrap()
        };
        
        //first shape text
        let options = font::ShapingOptions {
            letter_spacing: None,
            // word spacing is (fixed amount + % of width of ' ')
            word_spacing: (Au::new(0), ordered_float::NotNaN::new(1.0).unwrap()),
            script: unicode_script::Script::Unknown,
            flags: font::ShapingFlags::empty(),
        };
        let run = text::TextRun::new(&mut font.borrow_mut(),
                                     text.to_string(), &options, unicode_bidi::Level::ltr());
        let len: text::glyph::ByteIndex =
            run.glyphs.iter().fold(text::glyph::ByteIndex(0),
                                   |sum, x| sum + x.range.length());
        let range = range::Range::new(text::glyph::ByteIndex(0),len);
        
        //now collect glyphs
        let mut origin = LayoutPoint::new(x0, y0);
        let mut glyphs = Vec::new(); // we really ought to know how big to make this
        for slice in run.natural_word_slices_in_visual_order(&range) {
            for glyph in slice.glyphs.iter_glyphs_for_byte_range(&slice.range) {
                let advance = glyph.advance().to_f32_px();
                let offset = glyph.offset().unwrap_or(euclid::Point2D::zero());
                let x = origin.x + offset.x.to_f32_px();
                let y = origin.y + offset.y.to_f32_px();
                let pos = LayoutPoint::new(x,y);
                let glyph = GlyphInstance {
                    index: glyph.id(),
                    point: pos
                };
                glyphs.push(glyph);
                origin.x = origin.x + advance;
            }
        }
        dlb.builder.push_text(&primitive_info(layout_rect(x,y,w,h)),
                              &glyphs, run.font_key,
                              ColorF::new(r,g,b,a),
                              None);
    }
}

#[no_mangle]
pub extern fn wrui_shape_text(font: *mut WRUIFont,
                              text: *const c_char) -> *mut ShapedText {
    let font = unsafe {
        assert!(!font.is_null());
        &mut *font
    };
    let text = unsafe {
        assert!(!text.is_null());
        CStr::from_ptr(text).to_str().unwrap()
    };

    //first shape text
    let options = font::ShapingOptions {
        letter_spacing: None,
        // word spacing is (fixed amount, % of width of ' ')
        word_spacing: (Au::new(0), ordered_float::NotNaN::new(1.0).unwrap()),
        script: unicode_script::Script::Unknown,
        flags: font::ShapingFlags::empty(),
    };
    let run = text::TextRun::new(&mut font.borrow_mut(), text.to_string(),
                                 &options, unicode_bidi::Level::ltr());
    let len: text::glyph::ByteIndex =
        run.glyphs.iter().fold(text::glyph::ByteIndex(0),
                               |sum, x| sum + x.range.length());
    let range = range::Range::new(text::glyph::ByteIndex(0),len);

    Box::into_raw(Box::new(ShapedText::new(run, range)))
}

#[no_mangle]
pub extern fn wrui_free_shaped_text(text: *mut ShapedText) -> () {
    let _ = unsafe {
        Box::from_raw(text)
    };
}

#[repr(C)]
pub struct ShapedRunInfo {
    pub width: f32,
    pub is_whitespace: uint32_t, // C boolean (i.e. true if non-zero). May add flags to this later
}
#[repr(C)]
pub struct ShapedRunPosition {
    pub x: f32,
    pub y: f32,
}
#[no_mangle]
/// given some shaped text determine the widths of each part
/// (separated by the unicode linebreaking algorithm). The function
/// returns a pointer to an array of the widths and sets 'len' to the
/// length of the array. The result should be freed with
/// 'shaped_text_widths_free'
pub extern fn shaped_text_get_widths(text: *mut ShapedText, len: *mut size_t) -> *mut ShapedRunInfo {
    let text = unsafe {
        assert!(!text.is_null());
        &*text
    };
    let mut result: Vec<ShapedRunInfo> = 
        text.run.natural_word_slices_in_visual_order(&text.range).map(|slice| {
            ShapedRunInfo {
                width: slice.glyphs.advance_for_byte_range(&slice.range, Au::from_px(0)).to_f32_px(),
                is_whitespace: if slice.glyphs.is_whitespace() { 1 } else { 0 },
            }
        }).collect();
    result.shrink_to_fit(); // we need to shrink it as we will only be passed a length to free it
    unsafe {
        assert!(!len.is_null());
        *len = result.len() as size_t;
    }
    let ptr = result.as_mut_ptr();
    mem::forget(result); // don't drop it here as we pass ownership to C
    ptr
}
#[no_mangle]
pub extern fn shaped_text_widths_free(info: *mut ShapedRunInfo, len: size_t) -> () {
    let len = len as usize;
    unsafe {
        drop(Vec::from_raw_parts(info, len, len));
    }
}

/// Computes the character which extends beyond a certain distance.
///
/// first_part and last_part are zero-based indices for the slices of
/// the text in natural order. Theses are the same slices whose widths
/// are given by shaped_text_widths. The advance should be relative to
/// the start of the first_part'th slice. The last_part'th slice is
/// included in the range.
///
/// The function returns the byte index for character which advances
/// over advance for the entire string that was passed to shape_text.
///
/// last_part may be larger than the number of slices (so -1 can be
/// used for the end of the run)
#[no_mangle]
pub extern fn shaped_text_char_for_advance(text: *mut ShapedText, advance: f32, 
                                           first_part: u64, last_part: u64) -> u64 {
    let text = unsafe {
        assert!(!text.is_null());
        &*text // {run, range}
    };
    let mut i = 0;
    let mut index = text::glyph::ByteIndex(0);
    let mut remaining = Au::from_f32_px(advance);
    for slice in text.run.natural_word_slices_in_visual_order(&text.range) {
        if i >= first_part && i <= last_part {
            let (slice_index, slice_advance) = 
                slice.glyphs.range_index_of_advance(&slice.range, remaining, Au(0));
            remaining -= slice_advance;
            index = index + text::glyph::ByteIndex(slice_index as isize);
            if remaining < Au(0) {
                break;
            }
        } else {
            index = index + slice.range.length();
            if i > last_part {
                break;
            }
        }
        i = i + 1;
    }
    index.to_usize() as u64
}

with_clip! {
    display_list_builder_push_shaped_text,
    display_list_builder_push_shaped_text_c,
    display_list_builder_push_shaped_text_cc,
    display_list_builder_push_shaped_text_t,
    display_list_builder_push_shaped_text_tc,
    display_list_builder_push_shaped_text_tcc,
    fn (dlb, primitive_info,
        x: f32, y: f32, w: f32, h: f32, // box containing text
        r: f32, g: f32, b: f32, a: f32,
        text: *mut ShapedText,
        // positions is a vector of positions for slices of length len
        // positions are relative to the passed rectangle
        positions: *mut ShapedRunPosition, len: size_t) -> () {
    
        let text = unsafe {
            assert!(!text.is_null());
            &*text
        };
        let positions = unsafe {
            slice::from_raw_parts(positions, len)
        };
        let mut glyphs = Vec::new();
        let topleft = LayoutPoint::new(x,y);
        for (slice,pos) in text.run.natural_word_slices_in_visual_order(&text.range).zip(positions) {
            let mut origin = LayoutPoint::new(topleft.x + pos.x, topleft.y +pos.y);
            for glyph in slice.glyphs.iter_glyphs_for_byte_range(&slice.range) {
                let advance = glyph.advance().to_f32_px();
                let offset = glyph.offset().unwrap_or(euclid::Point2D::zero());
                let x = origin.x + offset.x.to_f32_px();
                let y = origin.y + offset.y.to_f32_px();
                let pos = LayoutPoint::new(x,y);
                let glyph = GlyphInstance {
                    index: glyph.id(),
                    point: pos
                };
                glyphs.push(glyph);
                origin.x = origin.x + advance;
            }
        }
        dlb.builder.push_text(&primitive_info(layout_rect(x,y,w,h)),
                              &glyphs, text.run.font_key,
                              ColorF::new(r,g,b,a),
                              None);
    }
}

// define scroll frame
// push scroll frame ?
fn optional_clip_id(pipeline: PipelineId, id: u64) -> Option<ClipId> {
    if id == 0 {
        None
    } else if (id & 0x8000000000000000) != 0 {
        Some(ClipId::Clip(id & 0x7fffffffffffffff, pipeline))
    } else {
        Some(ClipId::ClipExternalId(id, pipeline))
    }
}
fn clip_id(pipeline: PipelineId, id: u64) -> ClipId {
    if (id & 0x8000000000000000) != 0 {
        ClipId::Clip(id & 0x7fffffffffffffff, pipeline)
    } else {
        ClipId::ClipExternalId(id, pipeline)
    }
}
fn from_clip_id(id: ClipId) -> u64 {
    match id {
        ClipId::Clip(id, _) => id | 0x8000000000000000,
        ClipId::ClipExternalId(id, _) => id,
        ClipId::DynamicallyAddedNode(id, _) => panic!("Don't know what to do here!")
    }
}

/// id should be the requested id of the frame, or zero for an
/// automatically asigned one. the id passed should be greater than
/// zero but have the most significant bit unset. The id assigned to
/// the frame is returned. If scroll_with_scroll_events is true then
/// the frame is sensitive to being scrolled by the mouse.
with_clip! {
    display_list_builder_define_scroll_frame,
    display_list_builder_define_scroll_frame_c,
    display_list_builder_define_scroll_frame_cc,
    None, None, None,
    fn (dlb, primitive_info, x: f32, y: f32, w: f32, h: f32, 
        id: u64, scroll_with_scroll_events: i32) -> u64 {
        let id = optional_clip_id(dlb.builder.pipeline_id, id);
        let info = primitive_info(layout_rect(x,y,w,h));
        
        let clip = dlb.builder.define_scroll_frame(
            id,
            info.rect,
            *info.local_clip.clip_rect(),
            match info.local_clip {
                LocalClip::Rect(_) => None,
                LocalClip::RoundedRect(_,r) => Some(r),
            },
            None, // image mask
            if scroll_with_scroll_events != 0 {
                ScrollSensitivity::ScriptAndInputEvents
            } else {
                ScrollSensitivity::Script
            });
        from_clip_id(clip)
    }
}

with_clip!{
    display_list_builder_push_stacking_context,
    display_list_builder_push_stacking_context_c,
    display_list_builder_push_stacking_context_cc,
    display_list_builder_push_stacking_context_t,
    display_list_builder_push_stacking_context_tc,
    display_list_builder_push_stacking_context_tcc,
    fn (dlb, primitive_info, x: f32, y: f32, w: f32, h: f32) -> () {
        dlb.builder.push_stacking_context(
            &primitive_info(layout_rect(x,y,w,h)),
            ScrollPolicy::Scrollable,
            None,
            TransformStyle::Flat,
            None,
            MixBlendMode::Normal,
            Vec::new(),
        );
    }
}

#[no_mangle]
pub extern fn display_list_builder_pop_stacking_context(dlb: *mut DisplayListBuilder) -> () {
    let dlb = unsafe {
        assert!(!dlb.is_null());
        &mut *dlb
    };
    dlb.builder.pop_stacking_context();
}

#[no_mangle]
pub extern fn display_list_builder_push_scroll_frame(dlb: *mut DisplayListBuilder, id: u64) -> () {
    let dlb = unsafe {
        assert!(!dlb.is_null());
        &mut *dlb
    };
    let id = clip_id(dlb.builder.pipeline_id, id);
    dlb.builder.push_clip_id(id);
}

#[no_mangle]
pub extern fn display_list_builder_pop_scroll_frame(dlb: *mut DisplayListBuilder) -> () {
    let dlb = unsafe {
        assert!(!dlb.is_null());
        &mut *dlb
    };
    dlb.builder.pop_clip_id();
}

#[no_mangle]
pub extern fn wrui_window_scroll(window: *mut WRUIWindow, mousex: f32, mousey: f32,
                                 deltax: f32, deltay: f32) -> () {
    let win = unsafe {
        assert!(!window.is_null());
        &mut *window
    };
    win.api.scroll(win.document, ScrollLocation::Delta(LayoutVector2D::new(deltax, deltay)),
                   WorldPoint::new(mousex, mousey),
                   ScrollEventPhase::Start);
    
}
