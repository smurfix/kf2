#ifndef MANDELBROT2_H
#define MANDELBROT2_H

#include "formula0.h"

#define NAME "Mandelbrot"
#define TYPE 0
#define POWER 2
#define GLITCH 0.0000001
#define REFERENCE \
	Xrn = Xr2 - Xi2 + Cr; \
	Xin = (Xr + Xi).Square() - Xr2 - Xi2 + Ci;
#define PERTURBATION \
	xrn = (2 * Xr + xr) * xr - (2 * Xi + xi) * xi + cr; \
	xin = 2 * ((Xr + xr) * xi + Xi * xr) + ci;

#include "formula.h"

#endif
