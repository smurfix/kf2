/*
Kalles Fraktaler 2
Copyright (C) 2013-2017 Karl Runmo
Copyright (C) 2017-2019 Claude Heiland-Allen

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

#ifndef KF_ITERCOUNT_ARRAY_H
#define KF_ITERCOUNT_ARRAY_H 1

// a[x][y] behaves like int64_t& but with potentially less memory usage

struct itercount_array_item_ref
{
private:
  uint32_t *lsb;
  uint32_t *msb; // may be nullptr
public:
  inline itercount_array_item_ref(uint32_t *lsb, uint32_t *msb = nullptr)
  : lsb(lsb)
  , msb(msb)
  {
  }
  inline itercount_array_item_ref &operator=(const int64_t &n)
  {
    if (msb)
      *msb = (n >> 32) & 0xFFffFFffU;
    *lsb = n & 0xFFffFFffU;
    return *this;
  }
  inline operator int64_t() const
  {
    int64_t n = *lsb;
    if (msb)
      n |= int64_t(*msb) << 32;
    else
      // sign extend
      n = int64_t(int32_t(n));
    return n;
  }
};

struct itercount_array_column_ref
{
private:
  ptrdiff_t stridey;
  uint32_t *lsb;
  uint32_t *msb; // may be nullptr
public:
  inline itercount_array_column_ref(ptrdiff_t stridey, uint32_t *lsb, uint32_t *msb = nullptr)
  : stridey(stridey)
  , lsb(lsb)
  , msb(msb)
  {
  }
  inline const itercount_array_item_ref operator[](int i) const
  {
    ptrdiff_t y = i * stridey;
    return itercount_array_item_ref(lsb + y, msb ? msb + y : msb);
  }
  inline itercount_array_item_ref operator[](int i)
  {
    ptrdiff_t y = i * stridey;
    return itercount_array_item_ref(lsb + y, msb ? msb + y : msb);
  }
};

struct itercount_array
{
private:
  ptrdiff_t stridex;
  ptrdiff_t stridey;
  uint32_t *lsb;
  uint32_t *msb; // may be nullptr
public:
  inline itercount_array(ptrdiff_t stridex, ptrdiff_t stridey, uint32_t *lsb, uint32_t *msb = nullptr)
  : stridex(stridex)
  , stridey(stridey)
  , lsb(lsb)
  , msb(msb)
  {
  }
  inline const itercount_array_column_ref operator[](int i) const
  {
    ptrdiff_t x = i * stridex;
    return itercount_array_column_ref(stridey, lsb + x, msb ? msb + x : msb);
  }
  inline itercount_array_column_ref operator[](int i)
  {
    ptrdiff_t x = i * stridex;
    return itercount_array_column_ref(stridey, lsb + x, msb ? msb + x : msb);
  }
  inline operator bool() const
  {
    return lsb;
  }
  // for EXR IO, to avoid allocating another 4 or 8 bytes per pixel
  inline uint32_t *get_lsb_array() { return lsb; };
  inline uint32_t *get_msb_array() { return msb; };
};

#endif
