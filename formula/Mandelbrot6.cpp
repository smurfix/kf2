#include "Mandelbrot6.h"
#define GLITCH 0.0001
#define CREFERENCE(T) \
  Xn = (X ^ 6) + C;
#define CPERTURBATION(T) \
  xn = 6 * (X ^ 5) * x + 15 * (X ^ 4) * (x ^ 2) + 20 * (X ^ 3) * (x ^ 3) + 15 * (X ^ 2) * (x ^ 4) + 6 * X * (x ^ 5) + (x ^ 6) + c;
#include "formula.inc"
