#include "Mandelbrot2.h"
#define GLITCH 0.0000001
#define REFERENCE(T) \
	Xrn = Xr2 - Xi2 + Cr; \
	Xin = (Xr + Xi).Square() - Xr2 - Xi2 + Ci;
#define PERTURBATION(T) \
	xrn = (2 * Xr + xr) * xr - (2 * Xi + xi) * xi + cr; \
	xin = 2 * ((Xr + xr) * xi + Xi * xr) + ci;
#include "formula.inc"
