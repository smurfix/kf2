#include "Mandelbrot8.h"
#define GLITCH 0.001
#define CREFERENCE(T) \
  Xn = (X ^ 8) + C;
#define CPERTURBATION(T) \
  xn = 8 * (X ^ 7) * x + 28 * (X ^ 6) * (x ^ 2) + 56 * (X ^ 5) * (x ^ 3) + 70 * (X ^ 4) * (x ^ 4) + 56 * (X ^ 3) * (x ^ 5) + 28 * (X ^ 2) * (x ^ 6) + 8 * X * (x ^ 7) + (x ^ 8) + c;
#include "formula.inc"
