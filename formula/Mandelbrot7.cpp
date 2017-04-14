#include "Mandelbrot7.h"
#define GLITCH 0.001
#define CREFERENCE(T) \
  Xn = (X ^ 7) + C;
#define CPERTURBATION(T) \
  xn = 7 * (X ^ 6) * x + 21 * (X ^ 5) * (x ^ 2) + 35 * (X ^ 4) * (x ^ 3) + 35 * (X ^ 3) * (x ^ 4) + 21 * (X ^ 2) * (x ^ 5) + 7 * X * (x ^ 6) + (x ^ 7) + c;
#include "formula.inc"
