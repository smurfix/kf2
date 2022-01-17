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

#ifndef KF_EXR_H
#define KF_EXR_H 1

#include <string>

struct itercount_array;

// select which channels should be saved into EXR files

enum EXRChannel_Bit {
  EXRChannel_R = 0,
  EXRChannel_G = 1,
  EXRChannel_B = 2,
  EXRChannel_N = 3,
  EXRChannel_NF = 4,
  EXRChannel_DEX = 5,
  EXRChannel_DEY = 6,
  EXRChannel_T = 7,
  EXRChannel_Preview = 8
};

struct EXRChannels
{
  bool R, G, B, N, NF, DEX, DEY, T, Preview;

  EXRChannels() {
    R=G=B=N=NF=DEX=DEY=T=Preview=false;
  }
  EXRChannels(unsigned int x) : EXRChannels() { unpack(x); }
  EXRChannels(std::string_view x) : EXRChannels(str_atoi(x)) { }

  void unpack(unsigned int x) {
    R = bool(x & (1 << EXRChannel_R));
    G = bool(x & (1 << EXRChannel_G));
    B = bool(x & (1 << EXRChannel_B));
    N = bool(x & (1 << EXRChannel_N));
    NF = bool(x & (1 << EXRChannel_NF));
    DEX = bool(x & (1 << EXRChannel_DEX));
    DEY = bool(x & (1 << EXRChannel_DEY));
    T = bool(x & (1 << EXRChannel_T));
    Preview = bool(x & (1 << EXRChannel_Preview));
  }
  unsigned int pack() const {
    return
    ((unsigned int) R << EXRChannel_R) |
    ((unsigned int) G << EXRChannel_G) |
    ((unsigned int) B << EXRChannel_B) |
    ((unsigned int) N << EXRChannel_N) |
    ((unsigned int) NF << EXRChannel_NF) |
    ((unsigned int) DEX << EXRChannel_DEX) |
    ((unsigned int) DEY << EXRChannel_DEY) |
    ((unsigned int) T << EXRChannel_T) |
    ((unsigned int) Preview << EXRChannel_Preview);
  }
  inline std::string to_string() const { return std::to_string(pack()); }
  inline bool operator==(const EXRChannels &other) const {
     return pack() == other.pack();
  }
  inline bool operator==(const EXRChannels &&other) const {
     return pack() == other.pack();
  }
};

static inline unsigned int pack_EXRChannels(EXRChannels c) { return c.pack(); }
static inline EXRChannels unpack_EXRChannels(unsigned int x) { return EXRChannels(x); }

extern std::string ReadEXRComment(const std::string &filename);

#endif
