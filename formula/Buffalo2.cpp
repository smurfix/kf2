#include "Buffalo2.h"
#define GLITCH 0.0000001
#define REFERENCE(T) \
  Xrn = (Xr2 - Xi2).Abs() + Cr; \
  Xin = -2.0 * (Xr * Xi).Abs() + Ci;
#define PERTURBATION(T) \
  xrn = diffabs(Xr * Xr - Xi * Xi, 2 * Xr * xr + xr * xr - 2 * Xi * xi - xi * xi) + cr; \
  xin = ci - 2 * diffabs(Xr * Xi, Xr * xi + xr * Xi + xr * xi);
#include "formula.inc"
