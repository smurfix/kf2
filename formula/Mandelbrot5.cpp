#include "Mandelbrot5.h"
#define GLITCH 0.0001
#define CREFERENCE(T) \
  Xn = (X ^ 5) + C;
#define CPERTURBATION(T) \
  xn = 5 * (X ^ 4) * x + 10 * (X ^ 3) * (x ^ 2) + 10 * (X ^ 2) * (x ^ 3) + 5 * X * (x ^ 4) + (x ^ 5) + c;
#include "formula.inc"
