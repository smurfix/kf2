#include "BurningShip5.h"
#define GLITCH 0.0001
#define REFERENCE(T) \
  Xrn = Xr.Abs() * (Xr2 * Xr2 - 10 * Xr2 * Xi2 + 5 * Xi2 * Xi2) + Cr; \
  Xin = Xi.Abs() * (5 * Xr2 * Xr2 - 10 * Xr2 * Xi2 + Xi2 * Xi2) + Ci;
#define PERTURBATION(T) \
  xrn = diffabs(Xr, xr) * (Xr*Xr*Xr*Xr - 10*Xr*Xr*Xi*Xi + 5*Xi*Xi*Xi*Xi) + abs(Xr+xr) * (4*Xr*Xr*Xr*xr+6*Xr*Xr*xr*xr+4*Xr*xr*xr*xr+xr*xr*xr*xr -20*Xr*Xr*Xi*xi-10*Xr*Xr*xi*xi-20*Xr*xr*Xi*Xi-40*Xr*xr*Xi*xi-20*Xr*xr*xi*xi-10*xr*xr*Xi*Xi-20*xr*xr*Xi*xi-10*xr*xr*xi*xi + 20*Xi*Xi*Xi*xi+30*Xi*Xi*xi*xi+20*Xi*xi*xi*xi+5*xi*xi*xi*xi) + cr; \
  xin = diffabs(Xi, xi) * (5*Xr*Xr*Xr*Xr - 10*Xr*Xr*Xi*Xi + Xi*Xi*Xi*Xi) + abs(Xi+xi) * (20*Xr*Xr*Xr*xr+30*Xr*Xr*xr*xr+20*Xr*xr*xr*xr+5*xr*xr*xr*xr - 20*Xr*Xr*Xi*xi-10*Xr*Xr*xi*xi-20*Xr*xr*Xi*Xi-40*Xr*xr*Xi*xi-20*Xr*xr*xi*xi-10*xr*xr*Xi*Xi-20*xr*xr*Xi*xi-10*xr*xr*xi*xi +4*Xi*Xi*Xi*xi+6*Xi*Xi*xi*xi+4*Xi*xi*xi*xi+xi*xi*xi*xi) + ci;
#include "formula.inc"
