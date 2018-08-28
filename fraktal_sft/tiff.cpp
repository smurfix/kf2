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
#include <tiffio.h>

#include "tiff.h"

extern int SaveTIFF(const std::string &szFileName, char *Data, int nHeight, int nWidth, int nColors, const std::string &comment)
{
	bool ok = true;
	TIFF *tif = TIFFOpen(szFileName.c_str(), "w");
	if (! tif) return 0;
	ok &= 1 == TIFFSetField(tif, TIFFTAG_IMAGEDESCRIPTION, comment.c_str());
	ok &= 1 == TIFFSetField(tif, TIFFTAG_IMAGEWIDTH, nWidth);
	ok &= 1 == TIFFSetField(tif, TIFFTAG_IMAGELENGTH, nHeight);
	ok &= 1 == TIFFSetField(tif, TIFFTAG_SAMPLESPERPIXEL, nColors);
	ok &= 1 == TIFFSetField(tif, TIFFTAG_BITSPERSAMPLE, 8);
	ok &= 1 == TIFFSetField(tif, TIFFTAG_ORIENTATION, ORIENTATION_TOPLEFT);
	ok &= 1 == TIFFSetField(tif, TIFFTAG_PLANARCONFIG, PLANARCONFIG_CONTIG);
	ok &= 1 == TIFFSetField(tif, TIFFTAG_PHOTOMETRIC, PHOTOMETRIC_RGB);
	ok &= 1 == TIFFSetField(tif, TIFFTAG_ROWSPERSTRIP, TIFFDefaultStripSize(tif, nWidth * nColors));
	for (int y = 0; y < nHeight; ++y)
	{
		ok &= 1 == TIFFWriteScanline(tif, Data + y * nWidth * nColors, y, 0);
	}
	TIFFClose(tif);
	return ok ? 1 : 0;
}

extern std::string ReadTIFFComment(const std::string &filename)
{
	auto e = TIFFSetErrorHandler(0);
	auto w = TIFFSetWarningHandler(0);
	bool ok = true;
	TIFF *tif = TIFFOpen(filename.c_str(), "r");
	if (! tif)
	{
		TIFFSetErrorHandler(e);
		TIFFSetWarningHandler(w);
		return "";
	}
	void *data = nullptr;
	ok &= 1 == TIFFGetField(tif, TIFFTAG_IMAGEDESCRIPTION, &data);
	if (ok)
	{
		std::string comment((const char *) data);
		TIFFClose(tif);
		TIFFSetErrorHandler(e);
		TIFFSetWarningHandler(w);
		return comment;
	}
	TIFFClose(tif);
	TIFFSetErrorHandler(e);
	TIFFSetWarningHandler(w);
	return "";
}
