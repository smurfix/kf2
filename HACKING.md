# KF2 and its data

This file contains some notes about the data structures used by KF2.

Help wanted!

## floating point formats

KF2 has *way* too many wrappers for these.

## decNumber

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


# Improvements?

## Numbers

Drop all those precision-adjusting wrappers and just set the global
precision to whatever maximum we need. There's only one place where this is
adjusted, which is when a number is read and we set the precision based on
how many digits it has. There shouldn't be any need for either more
precision (it's not incremented anywhere) nor less.

Set that as the global precision and we're done.
