/*
Kalles Fraktaler 2
Copyright (C) 2013-2017 Karl Runmo
Copyright (C) 2017-2020 Claude Heiland-Allen

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

NanoMB2_Reference *NanoMB2_Reference_Calculate
  ( const complex<decNumber> &c
  , int64_t bm
  , int64_t bn
  , int64_t maxperiod
  , floatexp r0
  , volatile bool &stop
  , int64_t &m_nRDone
  , floatexp radius_scale
  );

void NanoMB2_Pixel
  ( const NanoMB2_Reference *ctx
  , complex<floatexp> dc
  , floatexp pixel_spacing
  , int64_t maxsi
  , int64_t maxiters
  , bool &bGlitch
  , int64_t &antal
  , double &test1
  , double &test2
  , double &phase
  , complex<double> &de
  , bool interior_checking
  , double g_real
  , double g_imag
  , double pval
  , double nBailout2
  );

#endif
