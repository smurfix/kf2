#include <stdio.h>
#include <time.h>
#include <png.h>
#include <zlib.h>

static void save_png_error_handler(png_structp png, png_const_charp msg)
{
	// FIXME make this display in the GUI or something
	fprintf(stderr, "PNG ERROR: %s\n", msg);
	longjmp(*static_cast<jmp_buf *>(png_get_error_ptr(png)), 1);
}

static void save_png_warning_handler(png_structp png, png_const_charp msg)
{
	// FIXME make this display in the GUI or something
	fprintf(stderr, "PNG WARNING: %s\n", msg);
}

int SavePNG(char *szFileName, char *Data, int nHeight, int nWidth, int nColors)
{
	jmp_buf jmpbuf;
	if (nColors != 3)
		return 0;
	FILE *file = fopen(szFileName, "wb");
	if (! file)
		return 0;
	png_structp png = png_create_write_struct(PNG_LIBPNG_VER_STRING, &jmpbuf, save_png_error_handler, save_png_warning_handler);
	if (! png)
		return 0;
	png_infop info = png_create_info_struct(png);
	if (! info)
	{
		png_destroy_write_struct(&png, 0);
		return 0;
	}
	if (setjmp(jmpbuf))
	{
		png_destroy_write_struct(&png, &info);
		return 0;
	}
	png_init_io(png, file);
	png_set_compression_level(png, Z_BEST_COMPRESSION);
	png_set_IHDR(png, info, nWidth, nHeight, 8, PNG_COLOR_TYPE_RGB, PNG_INTERLACE_ADAM7, PNG_COMPRESSION_TYPE_DEFAULT, PNG_FILTER_TYPE_DEFAULT);
	png_time mtime;
	png_convert_from_time_t(&mtime, time(0));
	png_set_tIME(png, info, &mtime);
/*
	// FIXME save .kfr text inside image file
	png_text text;
	text.compression = PNG_TEXT_COMPRESSION_NONE;
	text.key = "Description";
	text.text = "...";
	png_set_text(png, info, &text, 1);
*/
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
