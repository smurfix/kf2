

#ifndef NANOMB2_H
#define NANOMB2_H 1

#include "CDecNumber.h"
#include "complex.h"
#include "floatexp.h"

struct NanoMB2_Reference;
NanoMB2_Reference *NanoMB2_Reference_Calculate(const complex<decNumber> &c, int bm, int bn, int maxperiod, floatexp r0);
void NanoMB2_Pixel(const NanoMB2_Reference *ctx, complex<floatexp> dc, floatexp pixel_spacing, int maxsi, int maxiters, int &bGlitch, int &antal, double &test1, double &test2, double &de);

#endif
