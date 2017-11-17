// crates and stuff from servo_font

#![feature(cfg_target_feature)]
#![feature(box_syntax)]
#![feature(step_trait)]

#![feature(unique)]
#![feature(range_contains)]
#![feature(fn_must_use)]

#![feature(alloc)]
#![feature(allocator_api)]

extern crate euclid;
extern crate gleam;
extern crate glutin;
extern crate webrender;
extern crate webrender_api;
extern crate app_units;
extern crate ipc_channel;
extern crate net_traits;
extern crate gfx;
extern crate style;
extern crate servo_arc;
extern crate range;

extern crate unicode_bidi;
extern crate unicode_script;
extern crate ordered_float;

//use glutin;
use gleam::gl;
use webrender_api::*;
use std::boxed::Box;
use app_units::Au;
use gfx::font_cache_thread::FontCacheThread;
use gfx::font_context::FontContext;
use gfx::text;
use gfx::font::{ShapingOptions, ShapingFlags};
use style::properties::style_structs::Font as FontStyle;
use style::Atom;
use servo_arc::Arc as ServoArc;

mod fake_resource_thread; 

fn main() {

    let window = glutin::WindowBuilder::new()
        .with_title("Test")
        .with_multitouch()
        .with_gl(glutin::GlRequest::GlThenGles {
            opengl_version:   (3,2),
            opengles_version: (3,0)
        })
        .with_min_dimensions(800,400)
        .build()
        .unwrap();

    unsafe {
        window.make_current().ok();
    }

    let gl = match gl::GlType::default() {
        gl::GlType::Gl => unsafe { 
            gl::GlFns::load_with(|symbol| window.get_proc_address(symbol) as *const _) },
        gl::GlType::Gles => unsafe {
            gl::GlesFns::load_with(|symbol| window.get_proc_address(symbol) as *const _) },
    };

    println!("OpenGL version {}", gl.get_string(gl::VERSION));


    let opts = webrender::RendererOptions {
        device_pixel_ratio: window.hidpi_factor(),
        resource_override_path: None,
        enable_aa: true,
        debug: true,
        precache_shaders: true,
        clear_framebuffer: true,
        ..Default::default()
    };

    let fake_resource_thread = fake_resource_thread::new_fake_resource_thread();

    let (mut renderer, sender) =
        webrender::Renderer::new(gl,
                                 Box::new(Notifier::new(window.create_window_proxy())),
                                 opts).unwrap();
    let api = sender.create_api();

    let font_cache_thread = FontCacheThread::new(fake_resource_thread, sender.create_api());
    let mut font_context = FontContext::new(font_cache_thread.clone());

    let (width, height) = window.get_inner_size_pixels().unwrap();
    let size = DeviceUintSize::new(width,height);

    let document_id = api.add_document(size);
    // remove below?
    api.set_root_pipeline(document_id.clone(), PipelineId(0,0));

//    build_simple(&api, PipelineId(0,0), Epoch(0), 800, 480);

    let mut i: u64;
    i = 0;

    'main_loop: for event in window.wait_events() {

        let eventstart = std::time::Instant::now();
        let mut events = Vec::new();
        events.push(event);
        for event in window.poll_events() {
            events.push(event);
        }
        for event in events {
            match event {
                glutin::Event::Closed |
                glutin::Event::KeyboardInput(_,_,Some(glutin::VirtualKeyCode::Escape)) 
                    => break 'main_loop,
                _ => {}
            }
        }

        let eventtime = eventstart.elapsed();
        let buildstart = std::time::Instant::now();

        let (width, height) = window.get_inner_size_pixels().unwrap();
        let size = DeviceUintSize::new(width, height);

        build_simple(&api, (document_id.clone(), PipelineId(0,0)),
                     renderer_next_epoch(&renderer, PipelineId(0,0)),
                     &mut font_context,
                     width, height);

        api.generate_frame(document_id.clone(), None);

        let buildtime = buildstart.elapsed();
        let updatestart = std::time::Instant::now();

        renderer.update();

        let updatetime = updatestart.elapsed();
        let renderstart = std::time::Instant::now();

        api.set_window_parameters(document_id.clone(),
                                  size, DeviceUintRect::new(DeviceUintPoint::zero(), size),
                                  window.hidpi_factor());

        renderer.render(size);

        let rendertime = renderstart.elapsed();

        window.swap_buffers().ok();


        i = i + 1;
        let frametime = eventstart.elapsed();
        if i % 100 == 0 {
            println!("");
            println!("frame took {} ({})", format_duration(frametime), format_duration_fps(frametime));
            println!("events: {}", format_duration(eventtime));
            println!("update: {}", format_duration(updatetime));
            println!("build:  {}", format_duration(buildtime));
            println!("render: {}", format_duration(rendertime));
        }

    }
    renderer.deinit();

    println!("Hello, World!");
}

fn format_duration(d : std::time::Duration) -> String {
    let secs = d.as_secs();
    let nanos = d.subsec_nanos();
        
    if secs == 0 {
        format!("{:09}ns", nanos)
    } else {
        format!("{}.{:09}s", secs, nanos)
    }
}
fn format_duration_fps(d: std::time::Duration) -> String {
    if d.as_secs() > 0 {
        format!("<1fps")
    } else {
        let nanos = d.subsec_nanos() as f64;
        let persec = 1.0e9 / nanos;
        format!("{:02.2}fps", persec)
    }
}

#[inline]
fn primitive_info(rect: LayoutRect) -> LayoutPrimitiveInfo {
    PrimitiveInfo {
        rect: rect,
        local_clip: LocalClip::Rect(rect),
        edge_aa_segment_mask: EdgeAaSegmentMask::all(),
        is_backface_visible: true,
        tag: None,
    }
}

#[inline]
fn primitive_info2(rect: LayoutRect, w: &BorderWidths) -> LayoutPrimitiveInfo {
    let rect2 = LayoutRect::new(LayoutPoint::new(rect.origin.x - w.left / 2.0,
                                                 rect.origin.y - w.top / 2.0),
                                LayoutSize::new(rect.size.width + w.left / 2.0 + w.right / 2.0,
                                                rect.size.height + w.top / 2.0 + w.bottom / 2.0));
    primitive_info(rect2)
}

fn build_simple(api: &RenderApi, (document_id, pipeline_id): (DocumentId, PipelineId),
                epoch: Epoch, font_context: &mut FontContext,
                width: u32, height: u32) {
    let n = { let Epoch(n) = epoch; ((n % 200) as f32) / 200.0 };
    let size = LayoutSize::new(width as f32, height as f32);
    let mut builder = DisplayListBuilder::new(pipeline_id, size);

    let clip = builder.define_clip(
        None,
        LayoutRect::new(LayoutPoint::new(30.0,40.0), LayoutSize::new(100.0,200.0)),
        vec![ComplexClipRegion {
            rect: LayoutRect::new(LayoutPoint::new(30.0,40.0),
                                  LayoutSize::new(100.0,200.0)),
            radii: BorderRadius {
                top_left: LayoutSize::new(5.0,5.0),
                top_right: LayoutSize::new(50.0,100.0),
                bottom_left: LayoutSize::zero(),
                bottom_right: LayoutSize::new(15.0,15.0)
            },
            mode: ClipMode::Clip}],
        None);
    builder.push_clip_id(clip);

    builder.push_rect(&primitive_info(LayoutRect::new(LayoutPoint::new(30.0,40.0),
                                                      LayoutSize::new(100.0,200.0))),
                      ColorF{r:1.0,g:1.0,b:1.0,a:1.0});

    builder.pop_clip_id();
    
    // yuck!
    let widths = BorderWidths { left: 1.0, top: 2.0, right: 8.0, bottom: 4.0 };

    builder.push_border(&primitive_info2(LayoutRect::new(LayoutPoint::new(30.0,40.0),
                                                         LayoutSize::new(100.0,200.0)),
                                         &widths),
                        widths,
                        BorderDetails::Normal(NormalBorder {
                            left: BorderSide { color: ColorF { r: 1.0, g: 1.0, b:0.0, a: 0.7},
                                               style: BorderStyle::Solid },
                            right: BorderSide { color: ColorF { r: 1.0, g: 0.0, b:1.0, a: 0.7},
                                                style: BorderStyle::Dotted },
                            top: BorderSide { color: ColorF { r: 0.0, g: 1.0, b:0.0, a: 0.7},
                                              style: BorderStyle::Dotted },
                            bottom: BorderSide { color: ColorF { r: 0.5, g: 1.0, b:0.5, a: 0.7},
                                                 style: BorderStyle::Solid },
                            radius: BorderRadius {
                                top_left: LayoutSize::new(5.0 + widths.left / 2.0, 5.0 + widths.top / 2.0),
                                top_right: LayoutSize::new(50.0 + widths.right / 2.0, 100.0 + widths.top / 2.0),
                                bottom_left: LayoutSize::zero(),
                                bottom_right: LayoutSize::new(15.0 + widths.right / 2.0,15.0 + widths.bottom / 2.0)
                            }
                        }));
    
    let fonts = font_context.layout_font_group_for_style(
        describe_font("DejaVu Sans", Au::from_px(30), false, 400, 5));
    let mut font = fonts.fonts[0].borrow_mut();

    let mut origin = euclid::Point2D::new(Au::from_px(200), Au::from_px(100));
    let mut glyphs = Vec::new();
    let options = ShapingOptions { letter_spacing: None, 
                                   word_spacing: (Au::new(0),
                                                  ordered_float::NotNaN::new(1.0).unwrap()),
                                   script: unicode_script::Script::Unknown,
                                   flags: ShapingFlags::empty()};
    let run = text::TextRun::new(&mut font, "Hello, World!".to_string(), &options, unicode_bidi::Level::ltr());
    let len: text::glyph::ByteIndex = (*run.glyphs).iter().fold(text::glyph::ByteIndex(0),
                                                                |sum,x| sum + x.range.length());
    for slice in run.natural_word_slices_in_visual_order(&range::Range::new(text::glyph::ByteIndex(0),len)) {
        for glyph in slice.glyphs.iter_glyphs_for_byte_range(&slice.range) {
            let advance = glyph.advance();
            let glyph_offset = glyph.offset().unwrap_or(euclid::Point2D::zero());
            let x = (origin.x + glyph_offset.x).to_f32_px();
            let y = (origin.y + glyph_offset.y).to_f32_px();
            let pos = LayoutPoint::new(x,y);
            let glyph = GlyphInstance {
                index: glyph.id(),
                point: pos,
            };
            glyphs.push(glyph);
            origin.x = origin.x + advance;
        }
    }
    builder.push_shadow(&primitive_info(
        LayoutRect::new(LayoutPoint::new(150.0, 50.0), LayoutSize::new(500.0, 100.0))),
                        Shadow {
                            offset: LayoutVector2D::new(-20.0*n,20.0*n),
                            color: ColorF{r:0.0, g: 0.0, b: 0.0, a: 1.0},
                            blur_radius: 5.0*n
                        });
    builder.push_text(&primitive_info(
        LayoutRect::new(LayoutPoint::new(150.0, 50.0), LayoutSize::new(500.0, 100.0))),
                      &glyphs,
                      run.font_key,
                      ColorF {r: 1.0, g: 1.0, b: 1.0, a: 1.0 },
                      None);
    builder.pop_all_shadows();

    builder.push_clear_rect(&primitive_info(
        LayoutRect::new(LayoutPoint::new(150.0, 150.0), LayoutSize::new(200.,200.))));

    api.set_display_list(document_id,
                         epoch,
                         Some(ColorF{r:0.5,g:0.3,b:0.3,a:1.0}),
                         size,
                         builder.finalize(),
                         true,
                         ResourceUpdates::new());

}

fn renderer_next_epoch(renderer : &webrender::Renderer, pipeline : PipelineId) -> Epoch {
    match renderer.current_epoch(pipeline) {
        None => Epoch(0),
        Some(Epoch(n)) => Epoch(n+1)
    }
}

struct Notifier {
    window_proxy: glutin::WindowProxy,
}

impl Notifier {
    fn new(window_proxy: glutin::WindowProxy) -> Notifier {
        Notifier {
            window_proxy: window_proxy,
        }
    }
}

impl RenderNotifier for Notifier {
    fn new_frame_ready(&self) {
        #[cfg(not(target_os = "android"))]
        self.window_proxy.wakeup_event_loop();
    }
    
    fn new_scroll_frame_ready(&self, _composite_needed: bool){
        #[cfg(not(target_os = "android"))]
        self.window_proxy.wakeup_event_loop();
    }
    fn clone(&self) -> Box<RenderNotifier> {
        Box::new(Notifier::new(self.window_proxy.clone()))
    }
}

unsafe impl Send for Notifier { }

fn describe_font(name: &str, size: Au, italic: bool, weight: u16, stretch: u8) -> ServoArc<FontStyle> {
    use style::computed_values::{font_stretch, font_weight, font_variant_caps, font_style};
    use style::computed_values::font_family;
    use style::computed_values::font_family::{FontFamily, FamilyName, FamilyNameSyntax, FontFamilyList};
    use style::values::computed::font::FontSize;
    use style::values::computed::length::NonNegativeLength;
    let mut res = FontStyle {
        font_family: font_family::T(FontFamilyList::new(vec![
            FontFamily::FamilyName(FamilyName {
                name: Atom::from(name),
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

/*
fn test_details(api: &RenderApi) -> Option<font::Font> {
    use platform::font_list::{for_each_variation, for_each_available_family};
    use font_template::{FontTemplate, FontTemplateDescriptor, font_weight, font_stretch, font_variant_caps};
    use font::{FontHandleMethods, Font};
    use atoms::Atom;
    let fc = platform::font_context::FontContextHandle::new();
    let mut have_match = false;
    let mut dat = None;
    let desc = FontTemplateDescriptor::new(font_weight::T::normal(), font_stretch::T::normal, false);
    for_each_available_family(|f| println!("{}", f));
    for_each_variation("DejaVu Sans", |variation| {
        println!("variation: {}", variation);
        if have_match { return; }
        let template = FontTemplate::new(Atom::from(&*variation), None);
        match template {
            Err(x) => println!("error for {}: {}", variation, x),
            Ok(mut template) => {
                match template.data_for_descriptor(&fc, &desc) {
                    None => println!("No match: {}", variation),
                    x => { println!("Selected: {}", variation); have_match = true; dat = x; }
                }
            }
        }
    });
    if dat.is_none() { return None; }
    let fdtemplate = dat.unwrap();
    let key = api.generate_font_key();

    {
        let temp = &*fdtemplate;
        let mut updates = ResourceUpdates::new();
        match temp.native_font() {
            Some(x) => updates.add_native_font(key.clone(), x),
            None => updates.add_raw_font(key.clone(), temp.bytes(), 0),
        }
        api.update_resources(updates);
    }
    let pt_size = Au::from_px(30);
    let handle = FontHandleMethods::new_from_template(&fc, fdtemplate, Some(pt_size.clone()));
    if handle.is_err() { return None; }
    let handle = handle.unwrap();
    let font = Font::new(handle, font_variant_caps::T::normal, desc, pt_size.clone(), pt_size.clone(), key);
    Some(font)
}
*/

