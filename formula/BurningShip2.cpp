#include "BurningShip2.h"
#define GLITCH 0.0000001
#define REFERENCE(T) \
  Xrn = Xr2 - Xi2 + Cr; \
  Xin = (2 * Xr * Xi).Abs() + Ci;
#define PERTURBATION(T) \
  xrn = 2 * xr * Xr + xr * xr - 2 * Xi * xi - xi * xi + cr; \
  xin = 2 * diffabs(Xr * Xi, Xr * xi + xr * Xi + xr * xi) + ci;
#include "formula.inc"
