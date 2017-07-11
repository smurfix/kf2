#include <stdio.h>
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
    fprintf(stderr, "ERROR image too large (%lld bytes)\n", bytes);
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
