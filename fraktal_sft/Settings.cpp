#include "Settings.h"

#include <cstring>
#include <cstdlib>

#include <windows.h>
#include "../common/StringVector.h"
#include "jpeg.h"
#include "png.h"

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
