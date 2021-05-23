/*
Kalles Fraktaler 2
Copyright (C) 2013-2017 Karl Runmo
Copyright (C) 2017-2021 Claude Heiland-Allen

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


This file based in part on `pixman-0.38.4/demos/scale.c`:
*/
/*
 * Copyright 2012, Red Hat, Inc.
 * Copyright 2012, Soren Sandmann
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice (including the next
 * paragraph) shall be included in all copies or substantial portions of the
 * Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 *
 * Author: Soren Sandmann <soren.sandmann@gmail.com>
 */

#include <math.h>
#include <pixman.h>
#include <stdlib.h>

// <https://docs.microsoft.com/en-us/windows/win32/api/wingdi/ns-wingdi-bitmapinfoheader#calculating-surface-stride>
static inline long long get_bitmap_stride(long long width, long long bpp)
{
  return ((width * bpp + 31LL) & ~31LL) >> 3LL;
}

extern bool scale_bitmap_rgb8
  ( unsigned char *dp, int dw, int dh
  , const unsigned char *sp, int sw, int sh
  , bool sRGB
  )
{
  bool retval = false;

  int sstride = get_bitmap_stride(sw, 24);
  pixman_image_t *src = pixman_image_create_bits(sRGB ? PIXMAN_r8g8b8_sRGB : PIXMAN_r8g8b8, sw, sh, (uint32_t *) sp, sstride);
  int dstride = get_bitmap_stride(dw, 24);
  pixman_image_t *dst = pixman_image_create_bits(sRGB ? PIXMAN_r8g8b8_sRGB : PIXMAN_r8g8b8, dw, dh, (uint32_t *) dp, dstride);

  double fscale_x = dw / (double) sw;
  double fscale_y = dh / (double) sh;
  pixman_f_transform_t ftransform;
  pixman_f_transform_init_identity(&ftransform);
  pixman_f_transform_scale(&ftransform, nullptr, fscale_x, fscale_y);
  pixman_f_transform_invert(&ftransform, &ftransform);
  pixman_transform_t transform;
  pixman_transform_from_pixman_f_transform(&transform, &ftransform);
  pixman_image_set_transform(src, &transform);

  double sx = hypot(ftransform.m[0][0], ftransform.m[0][1]) / ftransform.m[2][2];
  double sy = hypot(ftransform.m[1][0], ftransform.m[1][1]) / ftransform.m[2][2];
  int n_params = 0;
  pixman_kernel_t k = PIXMAN_KERNEL_LANCZOS3;
  pixman_fixed_t *params = pixman_filter_create_separable_convolution
    ( &n_params
    , sx * 65536.0 + 0.5
    , sy * 65536.0 + 0.5
    , k, k // reconstruct x,y
    , k, k // sample x,y
	  , 4, 4 // subsample x,y
    );
  if (! params) goto cleanup;
  if (! dst) goto cleanup;
  if (! src) goto cleanup;
  pixman_image_set_filter(src, PIXMAN_FILTER_SEPARABLE_CONVOLUTION, params, n_params);
  pixman_image_set_repeat(src, PIXMAN_REPEAT_PAD);

  pixman_image_composite
    ( PIXMAN_OP_SRC
    , src, nullptr, dst
    , 0, 0,  0, 0,  0, 0
    , dw, dh
    );

  retval = true;
cleanup:
  if (dst) pixman_image_unref(dst);
  if (src) pixman_image_unref(src);
  if (params) free(params);
  return retval;
}
