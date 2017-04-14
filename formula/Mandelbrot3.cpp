#include "Mandelbrot3.h"
#define GLITCH 0.000001
#define REFERENCE(T) \
	Xrn = Xr*(Xr2 - 3 * Xi2) + Cr; \
	Xin = (3 * Xr2 - Xi2)*Xi + Ci;
#define PERTURBATION(T) \
	xrn = 3 * Xr * Xr * xr - 6 * Xr * Xi * xi - 3 * Xi * Xi * xr + 3 * Xr * xr * xr - 3 * Xr * xi * xi - 3 * Xi * 2 * xr * xi + xr * xr * xr - 3 * xr * xi * xi + cr; \
	xin = 3 * Xr * Xr * xi + 6 * Xr * Xi * xr - 3 * Xi * Xi * xi + 3 * Xr * 2 * xr * xi + 3 * Xi * xr * xr - 3 * Xi * xi * xi + 3 * xr * xr * xi - xi * xi * xi + ci;
#include "formula.inc"
