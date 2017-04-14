#include "Mandelbrot10.h"
#define GLITCH 0.01
#define CREFERENCE(T) \
  Xn = (X ^ 10) + C;
#define CPERTURBATION(T) \
	xn = 10 * (X ^ 9) * x + 45 * (X ^ 8) * (x ^ 2) + 120 * (X ^ 7) * (x ^ 3) + 210 * (X ^ 6) * (x ^ 4) + 252 * (X ^ 5) * (x ^ 5) + 210 * (X ^ 4) * (x ^ 6) + 120 * (X ^ 3) * (x ^ 7) + 45 * (X ^ 2) * (x ^ 8) + 10 * X * (x ^ 9) + (x ^ 10) + c;
#include "formula.inc"
