#include "BurningShip4.h"
#define GLITCH 0.00001
#define REFERENCE(T) \
  Xrn = Xr2 * Xr2 + Xi2 * Xi2 - 6 * Xr2 * Xi2 + Cr; \
  Xin = 4 * (Xr * Xi).Abs() * (Xr2 - Xi2) + Ci;
#define PERTURBATION(T) \
  xrn = 4*Xr*Xr*Xr*xr + 6*Xr*Xr*xr*xr + 4*Xr*xr*xr*xr + xr*xr*xr*xr + 4*Xi*Xi*Xi*xi + 6*Xi*Xi*xi*xi + 4*Xi*xi*xi*xi + xi*xi*xi*xi - 12*Xr*Xr*Xi*xi-6*Xr*Xr*xi*xi-12*Xr*xr*Xi*Xi-24*Xr*xr*Xi*xi-12*Xr*xr*xi*xi-6*xr*xr*Xi*Xi-12*xr*xr*Xi*xi-6*xr*xr*xi*xi + cr; \
  xin = 4*(Xr*Xr-Xi*Xi)*diffabs(Xr * Xi, Xr * xi + xr * Xi + xr * xi) + 4 * abs(Xr*Xi + Xr*xi + Xr*Xi + xr*xi)*(2*xr*Xr+xr*xr-2*xi*Xi-xi*xi) + ci;
#include "formula.inc"
