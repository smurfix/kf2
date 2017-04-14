#include "Mandelbrot4.h"
#define GLITCH 0.00001
#define REFERENCE(T) \
  Xrn = Xr2.Square() - 6 * Xr2 * Xi2 + Xi2.Square() + Cr; \
  Xin = 4 * Xr * Xi * (Xr2 - Xi2) + Ci;
#define CPERTURBATION(T) \
  xn = 4 * (X ^ 3) * x + 6 * (X ^ 2) * (x ^ 2) + 4 * X * (x ^ 3) + (x ^ 4) + c;
#include "formula.inc"
