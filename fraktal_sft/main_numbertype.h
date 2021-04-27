/*
Kalles Fraktaler 2
Copyright (C) 2021 Claude Heiland-Allen

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

#ifndef KF_MAIN_NUMBERTYPE_H
#define KF_MAIN_NUMBERTYPE_H 1

#include <windows.h>

extern INT_PTR WINAPI NumberTypeProc(HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam);

enum NumberType_Bit
{
  NumberType_Single         = 0,
  NumberType_Double         = 1,
  NumberType_LongDouble     = 2,
  NumberType_Quad           = 3,
  NumberType_FloatExpSingle = 4,
  NumberType_FloatExpDouble = 5,
  NumberType_RescaledSingle = 6,
  NumberType_RescaledDouble = 7
};

struct NumberType
{
  bool Single, Double, LongDouble, Quad, FloatExpSingle, FloatExpDouble, RescaledSingle, RescaledDouble;
};

static inline int64_t pack_number_type(NumberType c)
{
  return
    ((int64_t) c.Single << NumberType_Single) |
    ((int64_t) c.Double << NumberType_Double) |
    ((int64_t) c.LongDouble << NumberType_LongDouble) |
    ((int64_t) c.Quad << NumberType_Quad) |
    ((int64_t) c.FloatExpSingle << NumberType_FloatExpSingle) |
    ((int64_t) c.FloatExpDouble << NumberType_FloatExpDouble) |
    ((int64_t) c.RescaledSingle << NumberType_RescaledSingle) |
    ((int64_t) c.RescaledDouble << NumberType_RescaledDouble);
}

static inline NumberType unpack_number_type(int64_t x)
{
  NumberType r =
    { bool(x & (1 << NumberType_Single))
    , bool(x & (1 << NumberType_Double))
    , bool(x & (1 << NumberType_LongDouble))
    , bool(x & (1 << NumberType_Quad))
    , bool(x & (1 << NumberType_FloatExpSingle))
    , bool(x & (1 << NumberType_FloatExpDouble))
    , bool(x & (1 << NumberType_RescaledSingle))
    , bool(x & (1 << NumberType_RescaledDouble))
    };
  return r;
}

#endif
