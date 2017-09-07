// crates and stuff from servo_font

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

extern crate euclid;
//extern crate fnv;

#[cfg(target_os = "linux")]
extern crate fontconfig;
//extern crate fontsan;
#[cfg(any(target_os = "linux", target_os = "android"))]
extern crate freetype;
//extern crate gfx_traits;

// Eventually we would like the shaper to be pluggable, as many operating systems have their own
// shapers. For now, however, this is a hard dependency.
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
//extern crate style;
//extern crate style_traits;
extern crate unicode_bidi;
extern crate unicode_script;
extern crate xi_unicode;
#[cfg(target_os = "android")]
extern crate xml5ever;
extern crate num_traits;
extern crate string_cache;

//other crates

extern crate gleam;
extern crate sdl2;
extern crate webrender;
extern crate webrender_api;
extern crate app_units;

use sdl2::event::Event;
use sdl2::keyboard::Keycode;
use gleam::gl;
use webrender_api::*;
use std::boxed::Box;
use app_units::Au;

//mod servo_font;
#[macro_use] pub mod range;
pub mod atoms;

#[macro_use] pub mod font;
pub mod font_template;
pub mod platform;
pub mod text;


fn main() {

    let sdl_context = sdl2::init().unwrap();
    let video_subsystem = sdl_context.video().unwrap();

    let gl_attr = video_subsystem.gl_attr();
    gl_attr.set_stencil_size(8);
    gl_attr.set_depth_size(24);
    gl_attr.set_context_major_version(3);
    gl_attr.set_context_minor_version(2);
    gl_attr.set_context_profile(sdl2::video::GLProfile::Core);

    //late swap tearing or vsync
    video_subsystem.gl_set_swap_interval(-1) || video_subsystem.gl_set_swap_interval(1);


    let window = video_subsystem
        .window("Test", 800, 480)
        .opengl()
        .resizable()
        .build()
        .unwrap();

    let context = window.gl_create_context().unwrap();
    window.gl_make_current(&context).unwrap();

    let gl = unsafe {
        gl::GlFns::load_with(|symbol| video_subsystem.gl_get_proc_address(symbol) as *const _)
    };


    let opts = webrender::RendererOptions {
        device_pixel_ratio: 1.0,
        resource_override_path: None,
        enable_aa: true,
        debug: false,
        clear_framebuffer: true,
        ..Default::default()
    };

    let (mut renderer, sender) =
        webrender::Renderer::new(gl, opts).unwrap();
    let api = sender.create_api();
    
    let document_id = api.add_document(DeviceUintSize::zero());
    api.set_root_pipeline(document_id.clone(), PipelineId(0,0));

//    build_simple(&api, PipelineId(0,0), Epoch(0), 800, 480);

    renderer.set_render_notifier(Box::new(DummyNotifier {}));

    let mut font_data = test_details(&api).unwrap();

    let mut event_pump = sdl_context.event_pump().unwrap();
    let mut i: u64;
    i = 0;

    'main_loop: loop {

        let eventstart = std::time::Instant::now();
        for event in event_pump.poll_iter() {
            match event {
                Event::Quit { .. } |
                Event::KeyDown { keycode: Some(Keycode::Escape), .. } => {
                    break 'main_loop;
                }
                _ => {}
            }
        }

        let eventtime = eventstart.elapsed();
        let buildstart = std::time::Instant::now();

        let (width, height) = window.drawable_size();
        let size = DeviceUintSize::new(width, height);

        build_simple(&api, (document_id.clone(), PipelineId(0,0)),
                     renderer_next_epoch(&renderer, PipelineId(0,0)),
                     &mut font_data,
                     width, height);

        api.generate_frame(document_id.clone(), None);

        let buildtime = buildstart.elapsed();
        let updatestart = std::time::Instant::now();

        renderer.update();

        let updatetime = updatestart.elapsed();
        let renderstart = std::time::Instant::now();

        api.set_window_parameters(document_id.clone(),
                                  size, DeviceUintRect::new(DeviceUintPoint::zero(), size));

        renderer.render(size);

        let rendertime = renderstart.elapsed();

        window.gl_swap_window();


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

fn build_simple(api: &RenderApi, (document_id, pipeline_id): (DocumentId, PipelineId),
                epoch: Epoch, font: &mut font::Font,
                width: u32, height: u32) {
    let n = { let Epoch(n) = epoch; ((n % 200) as f32) / 200.0 };
    let size = LayoutSize::new(width as f32, height as f32);
    let mut builder = DisplayListBuilder::new(pipeline_id, size);

    let clip = builder.define_clip(
        None,
        LayoutRect::new(LayoutPoint::new(30.0,40.0), LayoutSize::new(100.0,200.0)),
        vec![ComplexClipRegion {
            rect: LayoutRect::new(LayoutPoint::new(30.0 + 0.5,40.0 + 1.0),
                                  LayoutSize::new(100.0-0.5-4.0,200.0-1.0-2.0)),
            radii: BorderRadius {
                top_left: LayoutSize::new(5.0,5.0),
                top_right: LayoutSize::new(50.0,100.0),
                bottom_left: LayoutSize::zero(),
                bottom_right: LayoutSize::new(15.0,15.0)
            }}],
        None);
    builder.push_clip_id(clip);

    builder.push_rect(LayoutRect::new(LayoutPoint::new(30.0,40.0), LayoutSize::new(100.0,200.0)),
                      None,
                      ColorF{r:1.0,g:1.0,b:1.0,a:1.0});

    builder.pop_clip_id();
    
    // yuck!
    builder.push_border(LayoutRect::new(LayoutPoint::new(30.0,40.0), LayoutSize::new(100.0,200.0)),
                        None,
                        BorderWidths { left: 1.0, top: 2.0, right: 8.0, bottom: 4.0 },
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
                                top_left: LayoutSize::new(5.0,5.0),
                                top_right: LayoutSize::new(50.0,100.0),
                                bottom_left: LayoutSize::zero(),
                                bottom_right: LayoutSize::new(15.0,15.0)
                            }
                        }));

    let mut origin = euclid::Point2D::new(Au::from_px(200), Au::from_px(100));
    let mut glyphs = Vec::new();
    let options = font::ShapingOptions { letter_spacing: None, 
                                         word_spacing: (Au::new(0),ordered_float::NotNaN::new(1.0).unwrap()),
                                         script: unicode_script::Script::Unknown,
                                         flags: font::ShapingFlags::empty()};
    let run = text::TextRun::new(font, "Hello, World!".to_string(), &options, unicode_bidi::Level::ltr());
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
    builder.push_text_shadow(LayoutRect::new(LayoutPoint::new(150.0, 50.0), LayoutSize::new(500.0, 100.0)),
                             None,
                             TextShadow {
                                 offset: LayoutVector2D::new(-20.0*n,20.0*n),
                                 color: ColorF{r:0.0, g: 0.0, b: 0.0, a: 1.0},
                                 blur_radius: 5.0*n
                             });
    builder.push_text(LayoutRect::new(LayoutPoint::new(150.0, 50.0), LayoutSize::new(500.0, 100.0)),
                      None,
                      &glyphs,
                      run.font_key,
                      ColorF {r: 1.0, g: 1.0, b: 1.0, a: 1.0 },
                      run.actual_pt_size,
                      None);
    builder.pop_text_shadow();

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

struct DummyNotifier {
}

impl RenderNotifier for DummyNotifier {
    fn new_frame_ready(&mut self) {
        //do nothing
    }
    
    fn new_scroll_frame_ready(&mut self, _composite_needed: bool){
        //do nothing ??
    }
}

fn test_details(api: &RenderApi) -> Option<font::Font> {
    use platform::font_list::{for_each_variation};
    use font_template::{FontTemplate, FontTemplateDescriptor, font_weight, font_stretch, font_variant_caps};
    use font::{FontHandleMethods, Font};
    use atoms::Atom;
    let fc = platform::font_context::FontContextHandle::new();
    let mut have_match = false;
    let mut dat = None;
    let desc = FontTemplateDescriptor::new(font_weight::T::normal(), font_stretch::T::normal, false);
    for_each_variation("Cantarell", |variation| {
        if have_match { return; }
        let template = FontTemplate::new(Atom::from(&*variation), None);
        match template {
            Err(x) => println!("error for {}: {}", variation, x),
            Ok(mut template) => {
                match template.data_for_descriptor(&fc, &desc) {
                    None => println!("No match: {}", variation),
                    x => { have_match = true; dat = x; }
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

