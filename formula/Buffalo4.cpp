#include "Buffalo4.h"
#define GLITCH 0.00001
#define REFERENCE(T) \
  Xrn = (Xr2 * Xr2 + Xi2 * Xi2 - 6 * Xr2 * Xi2).Abs() + Cr; \
  Xin = (4 * Xr * Xi * (Xr2 - Xi2)).Abs() + Ci;
#define PERTURBATION(T) \
  T x(Xr), y(Xi), x2(x * x), y2(y * y), a(xr), b(xi), a2(a * a), b2(b * b); \
  xrn = diffabs(x2*x2+y2*y2-6*x2*y2, 4*x2*x*a+6*x2*a2+4*x*a2*a+a2*a2+4*y2*y*b+6*y2*b2+4*y*b2*b+b2*b2-12*x2*y*b-6*x2*b2-12*x*a*y2-24*x*a*y*b-12*x*a*b2-6*a2*y2-12*a2*y*b-6*a2*b2) + cr; \
  xin = diffabs(4*x2*x*y-4*x*y2*y, -4*a*y2*y-12*b*x*y2-12*a*b*y2+12*a*x2*y-12*b2*x*y+12*a2*x*y-12*a*b2*y+4*a2*a*y+4*b*x2*x+12*a*b*x2-4*b2*b*x+12*a2*b*x-4*a*b2*b+4*a2*a*b) + ci;
#include "formula.inc"
