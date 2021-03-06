# Some notes about KF2 internals

This file contains some notes about the data structures used by KF2.

Help wanted!

## Parameterization machinery

We now have a radically reworked, unified parameter / location / settings handling.

On-disk parameter descriptions and the code reading and writing them is
auto-generated by `gen_settings.py` (Python 3, no external dependencies)
from the description in `fraktal_sft/Settings.tab` which is a simple table
with tab-separated columns.

The table supports various data types and parameter categories, including
enums, bitmasks, and arrays with externally-stored length). There's also
columns for renaming tables and Get/Set accessors.

Thus, adding or changing parameters requires no manual code to be written,
and there's less chance for obscure errors to creep in.

In code, settings live in Settings objects. There's always one active
Settings object; this object must not be modified. The `Set*` wrappers
apply their changes to a copy, which is activated by calling
`ApplyNewSettings`. (This is a no-op if nothing changed, and it ensures
to not generate work if not necessary.)

Applying a Settings object copies its values to read-only variables. We don't
refer to it via the `m_Settings` pointer because that'd be slow.

All pointers to Settings are sared pointers because they're also held in
the Unod/Redo lists. To keep track how many references point to them
manually would needlessly duplicate standard library code.

Handling limits on values and other minor considerations is possible via
`Settings.tab`. For more involved considerations you can introduce separate
variables for reading/writing, or disable some steps of the process and
write the relevant code by hand.

TODO: don't store the data in a home-grown string list; we should use
C++ I/O directly.

## Differences between embedded and Windows code

The Windows version does some low-level locking on the main bitmap. The
embedded code doesn't do that, it's the responsibility of the application
to ensure that there's exactly one GUI thread, and to prevent that thread
from stepping onto anything while a render / Newton / whatnot is in
progress.

On Windows, Render calls PostMessage(WM\_USER+199) to lop back to the GUI
so that it can calculate a new reference, which auto-starts a new Render.
Embedded, that's currently the responsibility of the external code.

TODO: this needs to be unified into a self-contained Render thread that
only ends when the image is finished.

## TODO

There are various XXX and TODO comments in there.

The boost::multiprecision wrapper of `mpfr_t` seems to be, umm, entirely superfluous.

The whole precision-setting and -getting machinery seems not to be
required. All calculations originate with the center point. Set their
precision correctly (i.e. when zooming) and you should be OK.

## Basic rendering process

KF2 uses the center of the image as a reference, which gets computed the
old-fashioned way. Then it calculates the points around it with faster
arithmetic. 

See [Wikipedia](https://en.wikipedia.org/wiki/Plotting_algorithms_for_the_Mandelbrot_set#Perturbation_theory_and_series_approximation)
for a more maths-centered (and probably more accurate) overview of this technique.

This process may break down for various math reasons. This problem is
called a glitch, and it's fixed by finding reference within the glitched
region (randomly or by choosing the region's center) and re-rendering the
glitched area with the new reference. Repeat until either done or we get
bored (i.e. exceed a set limit).

The "repeat" part of this is signalled by posting a "WM\_USER+199" message.
This may have been a good idea at some time in the distant past, but
already doesn't really work when writing batch images, leading to code
duplication, and totally breaks down when embedding.

Since this stuff *does* depend on the user interface somewhat and (more
importantly) this author's desire to write C++ code is severely limited,
you need to take care of the render / find glitch / repeat loop yourself
if you embed KF2 in your own code.

## Window, image etc. sizes et al.

KF2 knows four different sizes.

- the bitmap to be calculated

- the output, always scaled down from the bitmap by an integer factor

- the intended window, scaled to view the output by some ratio

- the actual window, if the user manually changed its size

These are stored in the settings file thus:

- ImageWidth/Height: the bitmap size.

- TargetWidth/Height: the scaled output.

- TargetSupersample is the ratio between Image and Target.

- WindowWidth/Height: the window size.

- WindowTop/Left/Bottom/Right describe the window's position on the screen,
  thus also (implicitly) its size.

This is redundant and somewhat confusing. Thus,

- the settings' ImageWidth/Height are ignored unless no target dimensions
  are given.

- the Get/SetTargetDimensions() function returns/affects both image width and target width.


## high-precision floating point formats

KF2 has somewhat too many wrappers for these.


### decNumber

This is an alias for a Boost-wrapped `mpfr_t` (multi-precision floating-point real).

### FixedFloat

This is merely an alias for decNumber.

### CFixedFloat

Wraps FixedFloat (.m\_f) with a heap of code that sets the global default
precision from the source precision(s) before doing its operations.

Thus, to access the `mpfr_t` structure hidden in a `CFixedFloat`, use
`.m_f.backend().data()`.

### CDecNumber

Wraps decNumber (.m\_dec) with a heap of code that sets the target's
precision from the global default precision before doing its operations.


## somewhat-standard-precision floating-point formats

except with larger exponents. Or rather, smaller exponents, since they are
used to calculate the deviation(s) from a reference.

### floatexp

Packages a standard "double" plus a separate int64\_t exponent.

### floatexpf

Packages a standard "float" plus a separate int32\_t exponent.

### tfloatexp

That's the template "floatexp" and "floatexpf" are built from.


## Building

### C preprocessor flags

At least one of WINVER and KF\_EMBED must be set.

- WINVER is defined on Windows builds, no matter whether embedded or not.
  
  It selects various Windows-specific APIs instead of their C++ equivalent.

- KF\_EMBED is set for building a shared library. It turns off all
  Windows-specific APIs that are not affected by WINVER.
  
  If KF\_EMBED is not set, WINVER must be.

- KF\_OPENCL controls whether support for computation with OpenCL is included.

