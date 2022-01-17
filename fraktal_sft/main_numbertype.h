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

#include "StringHelper.h"

#ifndef KF_EMBED
extern INT_PTR WINAPI NumberTypeProc(HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam);
#endif

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

  NumberType() {
    Single=Double=LongDouble=Quad=FloatExpSingle=FloatExpDouble=RescaledSingle=RescaledDouble=false;
  }
  NumberType(unsigned int x) : NumberType() { unpack(x); }
  NumberType(std::string_view x) : NumberType(str_atoi(x)) { }
  void unpack(unsigned int x) {
    Single = bool(x & (1 << NumberType_Single));
    Double = bool(x & (1 << NumberType_Double));
    LongDouble = bool(x & (1 << NumberType_LongDouble));
    Quad = bool(x & (1 << NumberType_Quad));
    FloatExpSingle = bool(x & (1 << NumberType_FloatExpSingle));
    FloatExpDouble = bool(x & (1 << NumberType_FloatExpDouble));
    RescaledSingle = bool(x & (1 << NumberType_RescaledSingle));
    RescaledDouble = bool(x & (1 << NumberType_RescaledDouble));
  }
  unsigned int pack() const {
    return
    ((unsigned int) Single << NumberType_Single) |
    ((unsigned int) Double << NumberType_Double) |
    ((unsigned int) LongDouble << NumberType_LongDouble) |
    ((unsigned int) Quad << NumberType_Quad) |
    ((unsigned int) FloatExpSingle << NumberType_FloatExpSingle) |
    ((unsigned int) FloatExpDouble << NumberType_FloatExpDouble) |
    ((unsigned int) RescaledSingle << NumberType_RescaledSingle) |
    ((unsigned int) RescaledDouble << NumberType_RescaledDouble);
  }
  std::string to_string() const { return std::to_string(pack()); }
  inline bool operator==(const NumberType &other) const {
     return pack() == other.pack();
  }
  inline bool operator==(const NumberType &&other) const {
     return pack() == other.pack();
  }
};

#endif
