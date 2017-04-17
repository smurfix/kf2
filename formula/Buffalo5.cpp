#include "Buffalo5.h"
#define GLITCH 0.0001
#define REFERENCE(T) \
  Xrn = (Xr * (Xr2*Xr2 - 10 * Xr2*Xi2 + 5 * Xi2*Xi2)).Abs() + Cr; \
  Xin = (Xi * (5 * Xr2*Xr2 - 10 * Xr2*Xi2 + Xi2*Xi2)).Abs() + Ci;
#define PERTURBATION(T) \
  T x(Xr), y(Xi), x2(x * x), y2(y * y), a(xr), b(xi), a2(a * a), b2(b * b); \
  xrn = diffabs(5*x*y2*y2-10*x2*x*y2+x2*x2*x, 20*x*b*y2*y-30*x2*a*y2+30*x*b2*y2-30*x*a2*y2-20*x2*x*b*y-60*x2*a*b*y+20*x*b2*b*y-60*x*a2*b*y+5*x2*x2*a-10*x2*x*b2+10*x2*x*a2-30*x2*a*b2+10*x2*a2*a+5*x*b2*b2-30*x*a2*b2+5*x*a2*a2+5*a*y2*y2+20*a*b*y2*y+30*a*b2*y2-10*a2*a*y2+20*a*b2*b*y-20*a2*a*b*y+5*a*b2*b2-10*a2*a*b2+a2*a2*a) + cr; \
  xin = diffabs(y2*y2*y-10*y2*y*x2+5*y*x2*x2, 5*y2*y2*b-20*y2*y*a*x+10*y2*y*b2-10*y2*y*a2-30*y2*b*x2-60*y2*a*b*x+10*y2*b2*b-30*y2*a2*b+20*y*a*x2*x-30*y*b2*x2+30*y*a2*x2-60*y*a*b2*x+20*y*a2*a*x+5*y*b2*b2-30*y*a2*b2+5*y*a2*a2+5*b*x2*x2+20*b*a*x2*x-10*b2*b*x2+30*b*a2*x2-20*b2*b*a*x+20*b*a2*a*x+b2*b2*b-10*b2*b*a2+5*b*a2*a2) + ci;
#include "formula.inc"
