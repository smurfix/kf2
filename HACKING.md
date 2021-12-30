# KF2 and its data

This file contains some notes about the data structures used by KF2.

Help wanted!


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

This is redundant and confusing. Thus,

- The settings' ImageWidth/Height are ignored.

- the Get/SetTargetDimensions() function returns/affects both image width and target width.


## floating point formats

KF2 has somewhat too many wrappers for these.

### decNumber

This is an alias for a Boost-wrapped MPFR (multiprecision real).

### FixedFloat

This is merely an alias for decNumber.

### CFixedFloat

Wraps FixedFloat (.m_f) with a heap of code that sets the global default
precision from the source precision(s) before doing its operations.

### CDecNumber

Wraps decNumber (.m_dec) with a heap of code that sets the target's
precision from the global default precision before doing its operations.

### floatexp

Packages a standard "double" plus a separate int64_t exponent.

### floatexpf

Packages a standard "float" plus a separate int32_t exponent.

### tfloatexp

That's the template "floatexp" and "floatexpf" are built from.

