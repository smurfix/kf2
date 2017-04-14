#include "Mandelbrot9.h"
#define GLITCH 0.01
#define CREFERENCE(T) \
  Xn = (X ^ 9) + C;
#define CPERTURBATION(T) \
  xn = 9 * (X ^ 8) * x + 36 * (X ^ 7) * (x ^ 2) + 84 * (X ^ 6) * (x ^ 3) + 126 * (X ^ 5) * (x ^ 4) + 126 * (X ^ 4) * (x ^ 5) + 84 * (X ^ 3) * (x ^ 6) + 36 * (X ^ 2) * (x ^ 7) + 9 * X * (x ^ 8) + (x ^ 9) + c;
#include "formula.inc"
