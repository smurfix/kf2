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
};

static inline int64_t pack_exr_channels(EXRChannels c)
{
  return
    ((int64_t) c.R << EXRChannel_R) |
    ((int64_t) c.G << EXRChannel_G) |
    ((int64_t) c.B << EXRChannel_B) |
    ((int64_t) c.N << EXRChannel_N) |
    ((int64_t) c.NF << EXRChannel_NF) |
    ((int64_t) c.DEX << EXRChannel_DEX) |
    ((int64_t) c.DEY << EXRChannel_DEY) |
    ((int64_t) c.T << EXRChannel_T) |
    ((int64_t) c.Preview << EXRChannel_Preview);
}

static inline EXRChannels unpack_exr_channels(int64_t x)
{
  EXRChannels r =
    { bool(x & (1 << EXRChannel_R))
    , bool(x & (1 << EXRChannel_G))
    , bool(x & (1 << EXRChannel_B))
    , bool(x & (1 << EXRChannel_N))
    , bool(x & (1 << EXRChannel_NF))
    , bool(x & (1 << EXRChannel_DEX))
    , bool(x & (1 << EXRChannel_DEY))
    , bool(x & (1 << EXRChannel_T))
    , bool(x & (1 << EXRChannel_Preview))
    };
  return r;
}

extern int SaveEXR
( const std::string &filename
, const unsigned char *Data
, int nWidth
, int nHeight
, int nColors
, const std::string &comment
, int64_t maxiter
, int arrWidth
, int arrHeight
, const itercount_array &count
, const float *trans
, const float *phase
, const float *dex
, const float *dey
, const EXRChannels channels
, int threads
);

extern std::string ReadEXRComment(const std::string &filename);

extern bool ReadEXRMapFile(const std::string &filename, int threads);

#endif
