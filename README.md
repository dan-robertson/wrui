# WebRender UI

This is a simple wrapper library for some parts
of [WebRender](https://github.com/servo/webrender) along with a few
bits from [Servo](https://github.com/servo/servo) for dealing with
text. Currently it is designed to provide a simple C API that can be
easily called through other languages FFIs. For example this means
that functions may not take as parameters or return structs.

The features supported are currently minimal and I add them as I come
to need them. Currently they are:

* Creating a window which may handle mouse events and renders itself
  every frame.
* Specifying all coordinates as layout coordinates (so a 200x200
  window on a high density screen may use 400x400 pixels on the
  screen).
* Drawing coloured rectangles.
* Drawing borders (made of four sides of varying widths and styles and
  with ellipse-shaped corners where the major and minor axes may be
  specified).
* Drawing text in one line straight from a string.
* Taking a string, breaking it up into pieces, shaping each of those
  pieces and determining their widths, then drawing those pieces at
  specified locations. (i.e. allowing for centring text).
* Currently only one specific font (family and style) is supported at
  a time. This is hardcoded to _DejaVu Sans_ at 20 pixels.
* There is theoretical multiplatform support as all the libraries used
  are multiplatform however that is probably cancelled out by the fact
  that the library requests a specific font be installed on the
  system. It definitely runs on Linux (so long as it can find _DejaVu
  Sans_).

The interface is mostly straightforward except creating a new window
is currently quite stupid.

Conceptually the library lies somewhere between using OpenGL/Cairo
with SDL/GLFW and using Javascript inside a web browser and
"rendering" everything to HTML elements.

# Building

To build:

    $ cargo build

For release:

    $ cargo build --release
    
This will put `libwrui.so` into `target/release/libwrui.so`.

You may find you need a nightly build of `rustc` to compile. You can
get this with [`rustup`](https://www.rustup.rs) as

    $ rustup install nightly

and then doing

    $ rustup default nightly
    $ cargo ...

or

    $ rustup run nightly cargo ...



