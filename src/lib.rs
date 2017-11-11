
#![feature(cfg_target_feature)]
#![feature(box_syntax)]
#![feature(step_trait)]

#![feature(unique)]
#![feature(range_contains)]
#![feature(fn_must_use)]

#![feature(alloc)]
#![feature(allocator_api)]
#[cfg(any(target_os = "linux", target_os = "android"))]
extern crate alloc;

#[macro_use]
extern crate bitflags;

// Mac OS-specific library dependencies
#[cfg(target_os = "macos")] extern crate byteorder;
#[cfg(target_os = "macos")] extern crate core_foundation;
#[cfg(target_os = "macos")] extern crate core_graphics;
#[cfg(target_os = "macos")] extern crate core_text;

// Windows-specific library dependencies
#[cfg(target_os = "windows")] extern crate dwrote;
#[cfg(target_os = "windows")] extern crate truetype;

#[cfg(target_os = "linux")]
extern crate fontconfig;
//extern crate fontsan;
#[cfg(any(target_os = "linux", target_os = "android"))]
extern crate freetype;
extern crate harfbuzz_sys as harfbuzz;

extern crate heapsize;
#[macro_use] extern crate heapsize_derive;
//extern crate ipc_channel;
#[macro_use]
extern crate lazy_static;
extern crate libc;
#[macro_use]
extern crate log;
extern crate ordered_float;
#[macro_use] extern crate serde;
#[cfg(any(target_feature = "sse2", target_feature = "neon"))]
extern crate simd;
extern crate smallvec;
extern crate unicode_bidi;
extern crate unicode_script;
extern crate xi_unicode;
#[cfg(target_os = "android")]
extern crate xml5ever;
extern crate num_traits;
extern crate string_cache;

extern crate euclid;
extern crate app_units;
extern crate gleam;
extern crate glutin;
extern crate webrender;
extern crate webrender_api;


#[macro_use]
mod range;
mod atoms;
#[macro_use]
mod font;
mod font_template;
mod platform;
mod text;

use libc::{c_char, uint32_t, int32_t, size_t};
use std::ffi::CStr;
use std::slice;
use std::mem;
use webrender_api::{PipelineId,GlyphInstance,RenderNotifier,
                    DeviceUintSize,DeviceUintPoint,DeviceUintRect,
                    ResourceUpdates,Epoch,
                    LayoutSize,LayoutRect,LayoutPoint,
                    ColorF,BorderWidths,BorderDetails,
                    NormalBorder,BorderSide,BorderRadius,
                    LocalClip, ComplexClipRegion};
use gleam::gl;
use app_units::Au;
use atoms::Atom;

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
    fn new_frame_ready(&mut self) {
        // maybe #[cfg(not(target_os = "android"))] ?
        self.window_proxy.wakeup_event_loop();
    }

    fn new_scroll_frame_ready(&mut self, _composite_needed: bool) {
        // and here
        self.window_proxy.wakeup_event_loop();
    }
}

/// A function, given a display list and the (layout) width and height
/// of the are to display, which builds the display list
type RenderFunction = unsafe extern "C" fn(*mut DisplayListBuilder, f32, f32) -> ();
/// A function called for every mouse event. The first two parameters
/// are the (x,y) coordinates. The third represents the button
/// pressed: 0 if no button was pressed/released, positive for
/// pressed, negative for released. The absolute value is the button.
type MouseHandler = unsafe extern "C" fn(f32, f32, int32_t) -> ();

/// Create an object for a window. This should be passed a function to
/// handle events for the window and a function to generate a display
/// list.
#[no_mangle]
pub extern fn new_window(title: *const c_char, width: uint32_t, height: uint32_t,
                         render: RenderFunction,
                         mouse:  MouseHandler) -> () {
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

    let (mut renderer, sender) = webrender::Renderer::new(gl, options).unwrap();
    let api = sender.create_api();
    let (width, height) = window.get_inner_size_pixels().unwrap();
    let size = DeviceUintSize::new(width, height);
    let document_id = api.add_document(size);
    let pipeline_id = PipelineId(0,0);

    api.set_root_pipeline(document_id, pipeline_id);
    renderer.set_render_notifier(Box::new(WRWindow::new(window.create_window_proxy())));

    //set up a font
    //todo: should probably look for best template match and try to use the font the system wants.
    let fc = platform::font_context::FontContextHandle::new();
    let mut have_match = false;
    let mut template_data = None;
    let desc = font_template::FontTemplateDescriptor::new(
        font_template::font_weight::T::normal(),
        font_template::font_stretch::T::normal,
        false);
    platform::font_list::for_each_variation("DejaVu Sans", |variation| {
        if have_match { return; }
        let template = font_template::FontTemplate::new(Atom::from(&*variation), None);
        match template {
            Err(_) => (),
            Ok(mut template) => {
                match template.data_for_descriptor(&fc, &desc) {
                    None => (),
                    x => { have_match = true; template_data = x; }
                }
            }
        }
    });
    if template_data.is_none() { panic!("Can't find font DejaVu Sans"); }
    let fdtemplate = template_data.unwrap();
    let key = api.generate_font_key();
    let updates = {
        let mut u = ResourceUpdates::new();
        let temp = &*fdtemplate;
        match temp.native_font() {
            Some(x) => u.add_native_font(key.clone(), x),
            None => u.add_raw_font(key.clone(), temp.bytes(), 0),
        }
        u
    };
    api.update_resources(updates);
    let pt_size = Au::from_px(20);
    let handle = font::FontHandleMethods::new_from_template(&fc, fdtemplate, Some(pt_size));
    if handle.is_err() { panic!("Couldn't get font handle for Cantarell"); }
    let handle = handle.unwrap();
    let mut font = font::Font::new(handle, font_template::font_variant_caps::T::normal,
                               desc, pt_size, pt_size, key);
    let mut data_estimate = None;
    
    let mut epoch = Epoch(0);
    'event_loop: for event in window.wait_events() {
        let mut events = Vec::new();
        events.push(event);
        for event in window.poll_events() {
            events.push(event);
        }
        for event in events {
            match event {
                glutin::Event::Closed |
                glutin::Event::KeyboardInput(_,_,Some(glutin::VirtualKeyCode::Escape)) 
                    => break 'event_loop,
                glutin::Event::Awakened => {
                },
                glutin::Event::MouseMoved(x,y) => {
                    unsafe {
                        let f = window.hidpi_factor();
                        mouse(x as f32 / f, y as f32 / f, 0);
                    }
                },
                glutin::Event::MouseInput(s,b,place) => {
                    unsafe {
                        let f = window.hidpi_factor();
                        // this is not very nice. Total hack
                        let (x,y) = place.unwrap_or((-99999,-99999));
                        let (x,y) = (x as f32 / f, y as f32 / f);
                        mouse(x, y,
                              match s {
                                  glutin::ElementState::Pressed => 1,
                                  glutin::ElementState::Released => -1,
                              } * match b {
                                  glutin::MouseButton::Left => 1,
                                  glutin::MouseButton::Right => 2,
                                  glutin::MouseButton::Middle => 3,
                                  glutin::MouseButton::Other(n) => 4+n as i32,
                              });
                    }
                },
                _ => println!("{:#?}", event),
            }
        }
        
        // get window dimensions
        let (width, height) = window.get_inner_size_pixels().unwrap();
        let size = DeviceUintSize::new(width, height);
        let (lwidth, lheight) = window.get_inner_size_points().unwrap();
        let lsize = LayoutSize::new(lwidth as f32, lheight as f32);

        // set up DisplayListBuilder
        let mut dlb = DisplayListBuilder::new(
            match data_estimate {
                None => webrender_api::DisplayListBuilder::new(pipeline_id, lsize),
                Some(n) => webrender_api::DisplayListBuilder::with_capacity(pipeline_id, lsize, n),
            },
            font);
        // generate display list
        unsafe {
            let ptr: *mut DisplayListBuilder = &mut dlb;
            render(ptr,lwidth as f32, lheight as f32);
        };
        data_estimate = Some(dlb.builder.data.len());

        api.set_display_list(document_id,
                             epoch,
                             Some(ColorF::new(0.0,0.0,0.0,1.0)),
                             lsize,
                             dlb.builder.finalize(),
                             false,
                             ResourceUpdates::new());
        api.generate_frame(document_id, None);
        renderer.update();
        api.set_window_parameters(document_id, size,
                                  DeviceUintRect::new(DeviceUintPoint::zero(), size));
        renderer.render(size);
        window.swap_buffers().ok();
        font = dlb.font;
        {let Epoch(n) = epoch; epoch = Epoch(n+1)}
    }

    api.shut_down();
    renderer.deinit();
    window.hide();
//    window.close(); // ??

}

pub struct DisplayListBuilder {
    builder: webrender_api::DisplayListBuilder,
    font: font::Font,
}

impl DisplayListBuilder {
    fn new(dlb: webrender_api::DisplayListBuilder, font: font::Font) -> DisplayListBuilder {
        DisplayListBuilder {
            builder: dlb,
            font: font,
        }
    }
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

/*
#[no_mangle]
pub extern fn display_list_builder_new() -> *mut DisplayListBuilder {
    //FIXME: get the right parameters
    let builder = webrender_api::DisplayListBuilder::new(PipelineId(0,0), LayoutSize::new(100.0,100.0));
    let builder
    Box::into_raw(Box::new(builder))
}
//TODO: ...new_with_capacity
*/

#[no_mangle]
pub extern fn display_list_builder_free(ptr: *mut DisplayListBuilder) -> () {
    if !ptr.is_null() {
        unsafe {
            Box::from_raw(ptr);
        }
    }
}

fn layout_rect(x: f32, y: f32, w: f32, h: f32) -> LayoutRect {
    LayoutRect::new(LayoutPoint::new(x,y), LayoutSize::new(w,h))
}


/// # with_clip!{
/// #   display_list_builder_push_name, display_list_builder_push_name_c,
/// #   display_list_builder_push_name_cc,
/// #   fn (dlb, localClip, args...) -> result {
/// #     body
/// #   }
/// # }
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
macro_rules! with_clip {
    ($name:ident , $namec:ident , $namecc:ident ,
     fn ( $dlb:ident , $local_clip:ident , $($arg:ident : $argt:ty),* ) -> $result:ty $body:block) =>
    {
        #[no_mangle]
        pub extern fn $name(ptr: *mut DisplayListBuilder, $($arg : $argt),*) -> $result {
            let $dlb = unsafe {
                assert!(!ptr.is_null());
                &mut *ptr
            };
            let $local_clip = None;
            $body
        }
        #[no_mangle]
        pub extern fn $namec(ptr: *mut DisplayListBuilder, cx: f32, cy:f32, cw:f32, ch:f32,
                          $($arg : $argt),*) -> $result {
            let $dlb = unsafe {
                assert!(!ptr.is_null());
                &mut *ptr
            };
            let $local_clip = Some(LocalClip::from(layout_rect(cx,cy,cw,ch)));
            $body
        }
        #[no_mangle]
        pub extern fn $namecc(ptr: *mut DisplayListBuilder, cx: f32, cy:f32, cw:f32, ch:f32,
                         ccx: f32, ccy: f32, ccw: f32, cch: f32,
                         cctlw: f32, cctlh: f32, cctrw: f32, cctrh: f32,
                         ccblw: f32, ccblh: f32, ccbrw: f32, ccbrh: f32,
                         $($arg : $argt),*) -> $result {
            let $dlb = unsafe {
                assert!(!ptr.is_null());
                &mut *ptr
            };
            let $local_clip = Some(LocalClip::RoundedRect(
                layout_rect(cx, cy, cw, ch),
                ComplexClipRegion::new(layout_rect(ccx, ccy, ccw, cch),
                                       border_radius(cctlw, cctlh, cctrw, cctrh,
                                                     ccblw, ccblh, ccbrw, ccbrh))));
            $body
        }
    }
}

with_clip! { 
    display_list_builder_push_rect,
    display_list_builder_push_rect_c,
    display_list_builder_push_rect_cc,
    fn (dlb, local_clip,
        x: f32, y: f32, w: f32, h: f32,
        r: f32, g: f32, b: f32, a: f32) -> () {
        dlb.builder.push_rect(layout_rect(x,y,w,h), local_clip, ColorF::new(r,g,b,a));
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
    fn (dlb, local_clip,
        x: f32, y: f32, w: f32, h: f32,
        leftw: f32, topw: f32, rightw: f32, bottomw: f32,
        lr: f32, lg: f32, lb: f32, la: f32, ls: BorderStyle,
        tr: f32, tg: f32, tb: f32, ta: f32, ts: BorderStyle,
        rr: f32, rg: f32, rb: f32, ra: f32, rs: BorderStyle,
        br: f32, bg: f32, bb: f32, ba: f32, bs: BorderStyle,
        tlw: f32, tlh: f32, trw: f32, trh: f32,
        blw: f32, blh: f32, brw: f32, brh: f32) -> () {
        dlb.builder.push_border(layout_rect(x,y,w,h), local_clip,
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

with_clip! {
    display_list_builder_push_text,
    display_list_builder_push_text_c,
    display_list_builder_push_text_cc,
    fn (dlb, local_clip,
        x: f32, y: f32, w: f32, h: f32, // box containing text
        r: f32, g: f32, b: f32, a: f32,
        x0: f32, y0: f32, //origin of first character
        text: *const c_char) -> () {
        
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
        let run = text::TextRun::new(&mut dlb.font, text.to_string(), &options, unicode_bidi::Level::ltr());
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
        dlb.builder.push_text(layout_rect(x,y,w,h), local_clip,
                              &glyphs, run.font_key,
                              ColorF::new(r,g,b,a),
                              run.actual_pt_size,
                              None);
    }
}

#[no_mangle]
pub extern fn display_list_builder_shape_text(ptr: *mut DisplayListBuilder,
                                              text: *const c_char) -> *mut ShapedText {
   let dlb = unsafe {
        assert!(!ptr.is_null());
        &mut *ptr
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
    let run = text::TextRun::new(&mut dlb.font, text.to_string(), &options, unicode_bidi::Level::ltr());
    let len: text::glyph::ByteIndex =
        run.glyphs.iter().fold(text::glyph::ByteIndex(0),
                               |sum, x| sum + x.range.length());
    let range = range::Range::new(text::glyph::ByteIndex(0),len);

    Box::into_raw(Box::new(ShapedText::new(run, range)))
}

#[no_mangle]
pub extern fn shaped_text_free(text: *mut ShapedText) -> () {
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

with_clip! {
    display_list_builder_push_shaped_text,
    display_list_builder_push_shaped_text_c,
    display_list_builder_push_shaped_text_cc,
    fn (dlb, local_clip,
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
        dlb.builder.push_text(layout_rect(x,y,w,h), local_clip,
                              &glyphs, text.run.font_key,
                              ColorF::new(r,g,b,a),
                              text.run.actual_pt_size,
                              None);
    }
}
