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

#include <iostream>
#include "bitmap.h"

// based on: https://source.winehq.org/git/wine.git/blob/39935fe5ad889d537d828cc82771bdb969cdb2d4:/dlls/gdi32/gdi_private.h#l561
static inline long long get_bitmap_stride(long long width, long long bpp)
{
  return ((width * bpp + 15LL) >> 3LL) & ~1LL;
}

// workaround https://bugs.winehq.org/show_bug.cgi?id=42727
// "obsolete 128MB bitmap size limit"
// see also: http://www.fractalforums.com/windows-fractal-software/windows-bitmap-size-test/
HBITMAP create_bitmap(HDC hdc, int width, int height)
{
  long long stride = get_bitmap_stride(width, 24LL);
  long long bytes = stride * height;
  if (bytes >= 2LL * 1024LL * 1024LL * 1024LL || bytes <= 0LL)
  {
    std::cerr << "ERROR image too large (" << bytes << " bytes)" << std::endl;
    return 0;
  }
  BITMAPINFO bmi;
  bmi.bmiHeader.biSize = sizeof(bmi);
  bmi.bmiHeader.biWidth = width;
  bmi.bmiHeader.biHeight = height;
  bmi.bmiHeader.biPlanes = 1;
  bmi.bmiHeader.biBitCount = 24;
  bmi.bmiHeader.biCompression = BI_RGB;
  bmi.bmiHeader.biSizeImage = bytes;
  bmi.bmiHeader.biXPelsPerMeter = 2835; // 72 dpi
  bmi.bmiHeader.biYPelsPerMeter = 2835; // 72 dpi
  bmi.bmiHeader.biClrUsed = 0;
  bmi.bmiHeader.biClrImportant = 0;
  return CreateDIBSection(hdc, &bmi, DIB_RGB_COLORS, 0, 0, 0);
}
