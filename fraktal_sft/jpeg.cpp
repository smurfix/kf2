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

#include <assert.h>
#include <stdio.h>
#include <string.h>
#include <string>
extern "C"
{
#include <jpeglib.h>
}
#include <setjmp.h>
#include <memory.h>
#include <malloc.h>

#include "jpeg.h"

#if 0

#if defined(__GNUC__) && !defined(__clang__)
#pragma GCC diagnostic ignored "-Wclobbered"
#endif

struct my_error_mgr {
  struct jpeg_error_mgr pub;	/* "public" fields */

  jmp_buf setjmp_buffer;	/* for return to caller */
};

typedef struct my_error_mgr * my_error_ptr;
METHODDEF(void)
my_error_exit (j_common_ptr cinfo)
{
  /* cinfo->err really points to a my_error_mgr struct, so coerce pointer */
  my_error_ptr myerr = (my_error_ptr) cinfo->err;

  /* Always display the message. */
  /* We could postpone this until after returning, if we chose. */
  (*cinfo->err->output_message) (cinfo);

  /* Return control to the setjmp point */
  longjmp(myerr->setjmp_buffer, 1);
}

int ReadJPG (char * filename,char **ppData, int *pnWidth, int *pnHeight,int *pnComponents)
{
  /* This struct contains the JPEG decompression parameters and pointers to
   * working space (which is allocated as needed by the JPEG library).
   */
  struct jpeg_decompress_struct cinfo;
  int nBuffSize=0;
  char *ReturnBuff=NULL;
  /* We use our private extension JPEG error handler.
   * Note that this struct must live as long as the main JPEG parameter
   * struct, to avoid dangling-pointer problems.
   */
  struct my_error_mgr jerr;
  /* More stuff */
  FILE * infile;		/* source file */
  JSAMPARRAY buffer;		/* Output row buffer */
  int row_stride;		/* physical row width in output buffer */

  /* In this example we want to open the input file before doing anything else,
   * so that the setjmp() error recovery below can assume the file is open.
   * VERY IMPORTANT: use "b" option to fopen() if you are on a machine that
   * requires it in order to read binary files.
   */

  if ((infile = fopen(filename, "rb")) == NULL)
    return 0;

  /* Step 1: allocate and initialize JPEG decompression object */

  /* We set up the normal JPEG error routines, then override error_exit. */
  cinfo.err = jpeg_std_error(&jerr.pub);
  jerr.pub.error_exit = my_error_exit;
  /* Establish the setjmp return context for my_error_exit to use. */
  if (setjmp(jerr.setjmp_buffer)) {
    /* If we get here, the JPEG code has signaled an error.
     * We need to clean up the JPEG object, close the input file, and return.
     */
    jpeg_destroy_decompress(&cinfo);
    fclose(infile);
    return 0;
  }
  /* Now we can initialize the JPEG decompression object. */
  jpeg_create_decompress(&cinfo);

  /* Step 2: specify data source (eg, a file) */

  jpeg_stdio_src(&cinfo, infile);

  /* Step 3: read file parameters with jpeg_read_header() */

  (void) jpeg_read_header(&cinfo, TRUE);
  *pnWidth = cinfo.image_width;
  *pnHeight = cinfo.image_height;
  *pnComponents = cinfo.num_components;
  /* We can ignore the return value from jpeg_read_header since
   *   (a) suspension is not possible with the stdio data source, and
   *   (b) we passed TRUE to reject a tables-only JPEG file as an error.
   * See libjpeg.doc for more info.
   */

  /* Step 4: set parameters for decompression */

  /* In this example, we don't need to change any of the defaults set by
   * jpeg_read_header(), so we do nothing here.
   */

  /* Step 5: Start decompressor */

  (void) jpeg_start_decompress(&cinfo);
  /* We can ignore the return value since suspension is not possible
   * with the stdio data source.
   */

  /* We may need to do some setup of our own at this point before reading
   * the data.  After jpeg_start_decompress() we have the correct scaled
   * output image dimensions available, as well as the output colormap
   * if we asked for color quantization.
   * In this example, we need to make an output work buffer of the right size.
   */ 
  /* JSAMPLEs per row in output buffer */
  row_stride = cinfo.output_width * cinfo.output_components;
  /* Make a one-row-high sample array that will go away when done with image */
  buffer = (*cinfo.mem->alloc_sarray)
		((j_common_ptr) &cinfo, JPOOL_IMAGE, row_stride, 1);

  /* Step 6: while (scan lines remain to be read) */
  /*           jpeg_read_scanlines(...); */

  /* Here we use the library's state variable cinfo.output_scanline as the
   * loop counter, so that we don't have to keep track ourselves.
   */
  while (cinfo.output_scanline < cinfo.output_height) {
    /* jpeg_read_scanlines expects an array of pointers to scanlines.
     * Here the array is only one element long, but you could ask for
     * more than one scanline at a time if that's more convenient.
     */
    (void) jpeg_read_scanlines(&cinfo, buffer, 1);
    /* Assume put_scanline_someplace wants a pointer and sample count. */
	ReturnBuff = (char*)realloc(ReturnBuff,nBuffSize+row_stride);
    memcpy(&ReturnBuff[nBuffSize],buffer[0],row_stride);
	nBuffSize+=row_stride;
  }
  *ppData = ReturnBuff;

  /* Step 7: Finish decompression */

  (void) jpeg_finish_decompress(&cinfo);
  /* We can ignore the return value since suspension is not possible
   * with the stdio data source.
   */

  /* Step 8: Release JPEG decompression object */

  /* This is an important step since it will release a good deal of memory. */
  jpeg_destroy_decompress(&cinfo);

  /* After finish_decompress, we can close the input file.
   * Here we postpone it until after no more JPEG errors are possible,
   * so as to simplify the setjmp error logic above.  (Actually, I don't
   * think that jpeg_destroy can do an error exit, but why assume anything...)
   */
  fclose(infile);

  /* At this point you may want to check to see whether any corrupt-data
   * warnings occurred (test whether jerr.pub.num_warnings is nonzero).
   */

  /* And we're done! */
  return 1;
}

#endif


int SaveJPG(const std::string &szFileName, char *Data, int nHeight, int nWidth, int nColors, int nQuality, const std::string &comment)
{
  if (nColors != 3)
    return 0;
  struct jpeg_compress_struct cinfo;
  struct jpeg_error_mgr jerr;

  FILE * outfile;		/* target file */
  JSAMPROW row_pointer[1];	/* pointer to JSAMPLE row[s] */
  int row_stride;		/* physical row width in image buffer */

  cinfo.err = jpeg_std_error(&jerr);
  int nStructSize = sizeof(jpeg_compress_struct);
  jpeg_CreateCompress(&cinfo,JPEG_LIB_VERSION,nStructSize);
  if ((outfile = fopen(szFileName.c_str(), "wb")) == NULL) {
    return 0;//exit(1);
  }
  jpeg_stdio_dest(&cinfo, outfile);
  cinfo.image_width = nWidth; 	/* image width and height, in pixels */
  cinfo.image_height = nHeight;
  cinfo.input_components = 3;		/* # of color components per pixel */
  cinfo.in_color_space = JCS_RGB; 	/* colorspace of input image */
  jpeg_set_defaults(&cinfo);
  jpeg_set_quality(&cinfo, nQuality, TRUE /* limit to baseline-JPEG values */);
  jpeg_start_compress(&cinfo, TRUE);
  size_t length = comment.length();
  const char *comment_str = comment.c_str();
  do {
    size_t wlength = length;
    if (wlength > 65533)
      wlength = 65533;
    if (wlength > 0)
      jpeg_write_marker(&cinfo, JPEG_COM, (const unsigned char *) comment_str, wlength);
    comment_str += wlength;
    length -= wlength;
  } while (length > 0);
  row_stride = nWidth * 3;	/* JSAMPLEs per row in image_buffer */
  while (cinfo.next_scanline < cinfo.image_height) {
    row_pointer[0] = (unsigned char*)& Data[cinfo.next_scanline * row_stride];
    (void) jpeg_write_scanlines(&cinfo, row_pointer, 1);
  }

  jpeg_finish_compress(&cinfo);
  fclose(outfile);
  jpeg_destroy_compress(&cinfo);
  return 1;
}


std::string ReadJPEGComment(const std::string &filename)
{
  std::string comment = "";
  FILE *file = fopen(filename.c_str(), "rb");
  if (! file)
  {
    return "";
  }
  int c = fgetc(file);
  if (c != 0xFF)
  {
    fclose(file);
    return "";
  }
  c = fgetc(file);
  if (c != 0xD8) // SOI
  {
    fclose(file);
    return "";
  }
  do {
    do {
      c = fgetc(file);
    } while (0 <= c && c <= 0xFF && c != 0xFF);
    if (! (0 <= c && c <= 0xFF))
      break;
    // c is 0xFF
    do {
      c = fgetc(file);
    } while (0 <= c && c <= 0xFF && c == 0xFF);
    if (! (0 <= c && c <= 0xFF))
      break;
    // c is now the marker byte
    if (c == 0xFE) // COM
    {
      // read 2 byte length
      int l1 = fgetc(file);
      if (! (0 <= l1 && l1 <= 0xFF))
        break;
      int l2 = fgetc(file);
      if (! (0 <= l2 && l2 <= 0xFF))
        break;
      int length = 0;
      length = ((l1 << 8) | l2) - 2; // length includes itself
      if (length > 0)
      {
        // read comment
        char *buffer = (char *) malloc(length + 1);
        if (! buffer)
        {
          fclose(file);
          return "";
        }
        buffer[length] = 0;
        if (1 != fread(buffer, length, 1, file))
        {
          free(buffer);
          fclose(file);
          return "";
        }
        comment += buffer;
        free(buffer);
      }
      else if (length < 0)
      {
        fclose(file);
        return "";
      }
    }
    else if (c == 0xD9 || c == 0xDA) // EOI SOS
    {
      // end of file, start of compressed data, or so
      break;
    }
    else
    {
      // read 2 byte length
      int l1 = fgetc(file);
      if (! (0 <= l1 && l1 <= 0xFF))
        break;
      int l2 = fgetc(file);
      if (! (0 <= l2 && l2 <= 0xFF))
        break;
      int length = 0;
      length = ((l1 << 8) | l2) - 2;
      if (length >= 0)
      {
        if (0 != fseek(file, length, SEEK_CUR))
        {
          fclose(file);
          return "";
        }
      }
      else
      {
        fclose(file);
        return "";
      }
    }
  } while(true);
  fclose(file);
  return comment;
}
