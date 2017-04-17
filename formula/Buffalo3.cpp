#include "Buffalo3.h"
#define GLITCH 0.0000001
#define REFERENCE(T) \
  Xrn = ((Xr2 - Xi2 * 3) * Xr).Abs() + Cr; \
  Xin = ((Xr2 * 3 - Xi2) * Xi).Abs() + Ci;
#define PERTURBATION(T) \
  xrn = diffabs(Xr * (Xr * Xr - 3 * Xi * Xi), xr * (3 * Xr * Xr + xr * xr) + 3 * Xr * (xr  * xr - 2 * Xi * xi - xi * xi) - 3 * xr * (Xi * Xi + 2 * Xi * xi + xi * xi)) + cr; \
  xin = diffabs(Xi * (3 * Xr * Xr - Xi * Xi), 3 * Xi * (2 * Xr * xr + xr * xr - xi * xi) + 3 * xi * (Xr * Xr + 2 * Xr * xr + xr * xr) - xi * (3 * Xi * Xi + xi * xi)) + ci;
#include "formula.inc"
