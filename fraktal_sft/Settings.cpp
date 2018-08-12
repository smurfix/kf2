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

#include "Settings.h"

#include <cstring>
#include <cstdlib>

#include <windows.h>
#include "../common/StringVector.h"
#include "jpeg.h"
#include "png.h"
#include "tiff.h"

bool Settings::FromText(const std::string &text)
{
  char *data = strdup(text.c_str());
  CStringTable s(data, ": ", "\r\n");
#define DOUBLE(KEY) { int n = s.FindString(0, #KEY); if (n != -1) { m_ ## KEY = atof(s[n][1]); } }
#define INT(KEY)    { int n = s.FindString(0, #KEY); if (n != -1) { m_ ## KEY = atoi(s[n][1]); } }
#define BOOL(KEY)   { int n = s.FindString(0, #KEY); if (n != -1) { m_ ## KEY = atoi(s[n][1]); } }
  DOUBLE(ZoomSize)
  INT(MaxReferences)
  BOOL(GlitchLowTolerance)
  BOOL(ApproxLowTolerance)
  BOOL(AutoApproxTerms)
  INT(ApproxTerms)
  INT(WindowWidth)
  INT(WindowHeight)
  INT(WindowTop)
  INT(WindowLeft)
  INT(WindowBottom)
  INT(WindowRight)
  INT(ImageWidth)
  INT(ImageHeight)
  INT(ThreadsPerCore)
  BOOL(AnimateZoom)
  BOOL(ArbitrarySize)
  BOOL(ReuseReference)
  BOOL(AutoSolveGlitches)
  BOOL(Guessing)
  BOOL(SolveGlitchNear)
  BOOL(NoApprox)
  BOOL(Mirror)
  BOOL(LongDoubleAlways)
  BOOL(FloatExpAlways)
  BOOL(AutoIterations)
  BOOL(ShowGlitches)
  BOOL(NoReuseCenter)
  INT(IsolatedGlitchNeighbourhood)
  INT(JitterSeed)
  INT(JitterShape)
  DOUBLE(JitterScale)
  BOOL(Derivatives)
#undef DOUBLE
#undef INT
#undef BOOL
  std::free(data);
  return true;
}

std::string Settings::ToText() const
{
  CStringTable s;
#define DOUBLE(KEY) { char d[100]; snprintf(d, 100, "%.18g", Get ## KEY ()); s.AddRow(); s.AddString(s.GetCount() - 1, #KEY); s.AddString(s.GetCount() - 1, d); }
#define INT(KEY)    { s.AddRow(); s.AddString(s.GetCount() - 1, #KEY); s.AddInt(s.GetCount() - 1, Get ## KEY ()); }
#define BOOL(KEY)   { s.AddRow(); s.AddString(s.GetCount() - 1, #KEY); s.AddInt(s.GetCount() - 1, Get ## KEY ()); }
  DOUBLE(ZoomSize)
  INT(MaxReferences)
  BOOL(GlitchLowTolerance)
  BOOL(ApproxLowTolerance)
  BOOL(AutoApproxTerms)
  INT(ApproxTerms)
  INT(WindowWidth)
  INT(WindowHeight)
  INT(WindowTop)
  INT(WindowLeft)
  INT(WindowBottom)
  INT(WindowRight)
  INT(ImageWidth)
  INT(ImageHeight)
  INT(ThreadsPerCore)
  BOOL(AnimateZoom)
  BOOL(ArbitrarySize)
  BOOL(ReuseReference)
  BOOL(AutoSolveGlitches)
  BOOL(Guessing)
  BOOL(SolveGlitchNear)
  BOOL(NoApprox)
  BOOL(Mirror)
  BOOL(LongDoubleAlways)
  BOOL(FloatExpAlways)
  BOOL(AutoIterations)
  BOOL(ShowGlitches)
  BOOL(NoReuseCenter)
  INT(IsolatedGlitchNeighbourhood)
  INT(JitterSeed)
  INT(JitterShape)
  DOUBLE(JitterScale)
  BOOL(Derivatives)
#undef DOUBLE
#undef INT
#undef BOOL
  char *data = s.ToText(": ", "\r\n");
  std::string r(data);
  s.DeleteToText(data);
  return r;
}

bool Settings::OpenFile(const std::string &filename)
{
  const char *szFile = filename.c_str();
	char *szData = 0;
	char *extension = strrchr(szFile, '.');
	if (extension && 0 == strcmp(".png", extension))
	{
		std::string filename = szFile;
		std::string comment = ReadPNGComment(filename);
		if (comment == "")
		  return false;
		size_t n = comment.length() + 1;
		szData = new char[n];
		strncpy(szData, comment.c_str(), n);
		szData[n-1] = 0;
	}
	else if (extension && (0 == strcmp(".tif", extension) || 0 == strcmp(".tiff", extension)))
	{
		std::string filename = szFile;
		std::string comment = ReadTIFFComment(filename);
		if (comment == "")
		  return false;
		size_t n = comment.length() + 1;
		szData = new char[n];
		strncpy(szData, comment.c_str(), n);
		szData[n-1] = 0;
	}
	else if (extension && (0 == strcmp(".jpg", extension) || 0 == strcmp(".jpeg", extension)))
	{
		std::string filename = szFile;
		std::string comment = ReadJPEGComment(filename);
		if (comment == "")
		  return false;
		size_t n = comment.length() + 1;
		szData = new char[n];
		strncpy(szData, comment.c_str(), n);
		szData[n-1] = 0;
	}
	else // anything else, probably .kfr/.kfs
	{
		DWORD dw;
		HANDLE hFile = CreateFile(szFile, GENERIC_READ, 0, NULL, OPEN_EXISTING, 0, NULL);
		if (hFile == INVALID_HANDLE_VALUE)
			return false;
		int nData = GetFileSize(hFile, NULL);
		szData = new char[nData + 1];
		ReadFile(hFile, szData, nData, &dw, NULL);
		CloseHandle(hFile);
		szData[nData] = 0;
	}
  std::string szText(szData);
  delete[] szData;
  return FromText(szText);
}

bool Settings::SaveFile(const std::string &filename) const
{
	std::string szText(ToText());
	const char *szData = szText.c_str();
  const char *szFile = filename.c_str();
	DWORD dw;
	HANDLE hFile = CreateFile(szFile, GENERIC_WRITE, 0, NULL, CREATE_ALWAYS, 0, NULL);
	if (hFile == INVALID_HANDLE_VALUE)
		return false;
	WriteFile(hFile, szData, strlen(szData), &dw, NULL);
	CloseHandle(hFile);
	return true;
}
