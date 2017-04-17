#include "BurningShip3.h"
#define GLITCH 0.0000001
#define REFERENCE(T) \
  Xrn = (Xr2 - 3 * Xi2) * Xr.Abs() + Cr; \
  Xin = (3 * Xr2 - Xi2) * Xi.Abs() + Ci;
#define PERTURBATION(T) \
  xrn = (Xr * Xr - 3 * Xi * Xi) * diffabs(Xr, xr) + (2 * Xr * xr + xr * xr - 6 * Xi * xi - 3 * xi * xi) * abs(Xr + xr) + cr; \
  xin = (3 * Xr * Xr - Xi * Xi) * diffabs(Xi, xi) + (6 * Xr * xr + 3 * xr * xr - 2 * Xi * xi - xi * xi) * abs(Xi + xi) + ci;
#include "formula.inc"
