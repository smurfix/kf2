/*
Kalles Fraktaler 2
Copyright (C) 2013-2017 Karl Runmo
Copyright (C) 2017-2018 Claude Heiland-Allen

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU Affero General Public License as
published by the Free Software Foundation, either version 3 of the
License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Affero General Public License for more details.

You should have received a copy of the GNU Affero General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.
*/

#ifndef NANOMB2_H
#define NANOMB2_H 1

#include "CDecNumber.h"
#include "complex.h"
#include "floatexp.h"

struct NanoMB2_Reference;
NanoMB2_Reference *NanoMB2_Reference_Calculate(const complex<decNumber> &c, int bm, int bn, int maxperiod, floatexp r0, volatile BOOL &stop);
void NanoMB2_Pixel(const NanoMB2_Reference *ctx, complex<floatexp> dc, floatexp pixel_spacing, int maxsi, int maxiters, int &bGlitch, int &antal, double &test1, double &test2, double &de);

#endif
