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

#include <stdio.h>
#include <string.h>
#include <time.h>
#include <string>
#include <png.h>
#include <zlib.h>

#include "png.h"

static void kf_png_error_handler(png_structp png, png_const_charp msg)
{
	// FIXME make this display in the GUI or something
	fprintf(stderr, "PNG ERROR: %s\n", msg);
	longjmp(*static_cast<jmp_buf *>(png_get_error_ptr(png)), 1);
}

static void kf_png_warning_handler(png_structp png, png_const_charp msg)
{
	(void) png;
	// FIXME make this display in the GUI or something
	fprintf(stderr, "PNG WARNING: %s\n", msg);
}

extern int SavePNG(const std::string &szFileName, char *Data, int nHeight, int nWidth, int nColors, const std::string &comment)
{
	jmp_buf jmpbuf;
	if (nColors != 3)
		return 0;
	FILE *file = fopen(szFileName.c_str(), "wb");
	if (! file)
		return 0;
	png_structp png = png_create_write_struct(PNG_LIBPNG_VER_STRING, &jmpbuf, kf_png_error_handler, kf_png_warning_handler);
	if (! png)
		return 0;
	png_infop info = png_create_info_struct(png);
	if (! info)
	{
		png_destroy_write_struct(&png, 0);
		fclose(file);
		return 0;
	}
	if (setjmp(jmpbuf))
	{
		png_destroy_write_struct(&png, &info);
		fclose(file);
		return 0;
	}
	png_init_io(png, file);
	png_set_compression_level(png, Z_BEST_COMPRESSION);
	png_set_IHDR(png, info, nWidth, nHeight, 8, PNG_COLOR_TYPE_RGB, PNG_INTERLACE_ADAM7, PNG_COMPRESSION_TYPE_DEFAULT, PNG_FILTER_TYPE_DEFAULT);
	png_time mtime;
	png_convert_from_time_t(&mtime, time(0));
	png_set_tIME(png, info, &mtime);
	png_text text;
	text.compression = PNG_TEXT_COMPRESSION_NONE;
	const std::string &key = "Comment";
	text.key = const_cast<char *>(key.c_str());
	text.text = const_cast<char *>(comment.c_str());
	png_set_text(png, info, &text, 1);
	png_write_info(png, info);
	png_bytepp row = new png_bytep[nHeight];
	for (int y = 0; y < nHeight; ++y)
		row[y] = (png_bytep)(Data + nWidth * nColors * y);
	png_write_image(png, row);
	png_write_end(png, 0);
	delete [] row;
	fclose(file);
	return 1;
}

extern std::string ReadPNGComment(const std::string &filename)
{
	jmp_buf jmpbuf;
	FILE *file = fopen(filename.c_str(), "rb");
	if (! file)
		return "";
	png_structp png = png_create_read_struct(PNG_LIBPNG_VER_STRING, &jmpbuf, kf_png_error_handler, kf_png_warning_handler);
	if (! png)
	{
		fclose(file);
		return "";
	}
	png_infop info = png_create_info_struct(png);
	if (! info)
	{
		png_destroy_read_struct(&png, 0, 0);
		fclose(file);
		return "";
	}
	if (setjmp(jmpbuf))
	{
		png_destroy_read_struct(&png, &info, 0);
		fclose(file);
		return "";
	}
	png_init_io(png, file);
	png_read_info(png, info);
	png_textp text;
	int count = 0;
	std::string comment = "";
	if (png_get_text(png, info, &text, &count) > 0)
		for (int t = 0; t < count; t++)
			if (0 == strcmp("Comment", text[t].key))
				comment = text[t].text; // copy
	// FIXME pngmeta (eg) skips the image data chunks and does png_read_info again
	// FIXME but we only write out comments before the image data, so we should be fine
	png_destroy_read_struct(&png, &info, 0);
	fclose(file);
  return comment;
}
