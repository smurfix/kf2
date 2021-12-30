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
*/

#include "Settings.h"

#include <cstring>
#include <cstdlib>
#include <fstream>
#include <sstream>

#include <windows.h>
#include "../common/StringVector.h"
#include "cmdline.h"
#include "jpeg.h"
#include "png.h"
#include "tiff.h"
#include "exr.h"
#include "defs.h"

#include <iostream>
#if __cplusplus >= 201703L
#include <filesystem>
#elif defined(WINVER)
#include "shlwapi.h" // PathFileExists
#endif

bool Settings::FromText(const std::string &text)
{
  char *data = strdup(text.c_str());
  CStringTable s(data, ": ", "\n");
  {
    int nv = s.FindString(0, "SettingsVersion");
    if (nv != -1)
    {
      int str_version_number = atoi(s[nv][1]);
      if (str_version_number > kfs_version_number)
      {
        fprintf(stderr, "WARNING: file format is newer than this EXE version\n");
      }
    }
  }
#define DOUBLE(KEY) { int n = s.FindString(0, #KEY); if (n != -1) { m_ ## KEY = atof(s[n][1]); } }
#define INT(KEY)    { int n = s.FindString(0, #KEY); if (n != -1) { m_ ## KEY = atoll(s[n][1]); } }
#define BOOL(KEY)   { int n = s.FindString(0, #KEY); if (n != -1) { m_ ## KEY = atoll(s[n][1]); } }
  DOUBLE(ZoomSize)
  INT(MaxReferences)
  DOUBLE(GlitchLowTolerance)
  DOUBLE(ApproxLowTolerance)
  BOOL(AutoApproxTerms)
  INT(ApproxTerms)
  INT(WindowWidth)
  INT(WindowHeight)
  INT(WindowTop)
  INT(WindowLeft)
  INT(WindowBottom)
  INT(WindowRight)
  DOUBLE(ThreadsPerCore)
  INT(ThreadsReserveCore)
  BOOL(AnimateZoom)
  BOOL(ArbitrarySize)
  BOOL(ReuseReference)
  BOOL(AutoSolveGlitches)
  BOOL(Guessing)
  BOOL(SolveGlitchNear)
  BOOL(NoApprox)
  BOOL(Mirror)
  BOOL(AutoIterations)
  BOOL(ShowGlitches)
  BOOL(NoReuseCenter)
  INT(IsolatedGlitchNeighbourhood)
  INT(JitterSeed)
  INT(JitterShape)
  DOUBLE(JitterScale)
  BOOL(Derivatives)
  BOOL(ShowCrossHair)
  BOOL(UseNanoMB1)
  BOOL(UseNanoMB2)
  INT(OrderM)
  INT(OrderN)
  BOOL(InteriorChecking)
  DOUBLE(RadiusScale)
  INT(Shrink)
  BOOL(HalfColour)
  BOOL(SaveOverwrites)
  BOOL(ThreadedReference)
  INT(SIMDVectorSize)
  SetSIMDVectorSize(GetSIMDVectorSize()); // validate to prevent assert fail in perturbation
  INT(SIMDChunkSize)
  int m_UseArgMinAbsZAsGlitchCenter = 0;
  INT(UseArgMinAbsZAsGlitchCenter)
  if (m_UseArgMinAbsZAsGlitchCenter) m_GlitchCenterMethod = 1;
  INT(GlitchCenterMethod)
  BOOL(UseOpenCL)
  BOOL(OpenCLThreaded)
  INT(OpenCLPlatform)
  INT(EXRChannels)
  BOOL(EXRParallel)
  BOOL(SaveNewtonProgress)
  BOOL(ExponentialMap)
  BOOL(DerivativeGlitch)
  BOOL(ReferenceStrictZero)
  INT(NumberTypes)
  BOOL(UseRescaledSeries)
  BOOL(OpenResetsParameters)
  INT(TargetWidth)
  INT(TargetHeight)
  INT(TargetSupersample)
#undef DOUBLE
#undef INT
#undef BOOL
  {
    int iw = 0;
    int ih = 0;
    int n;

    n = s.FindString(0, "ImageWidth");
    if (n != -1)
      iw = atof(s[n][1]);

    n = s.FindString(0, "ImageHeight");
    if (n != -1)
      ih = atof(s[n][1]);

    n = s.FindString(0, "TargetSupersample");
    if (n == -1 && (iw || ih)) {
      // old config file
      m_TargetSupersample = iw ? iw/m_TargetWidth : ih/m_TargetHeight;
    }
  }

  std::free(data);
  return true;
}

std::string Settings::ToText() const
{
  CStringTable s;
#define DOUBLE(KEY) { char d[100]; snprintf(d, 100, "%.18g", Get ## KEY ()); s.AddRow(); s.AddString(s.GetCount() - 1, #KEY); s.AddString(s.GetCount() - 1, d); }
#define INT(KEY)    { s.AddRow(); s.AddString(s.GetCount() - 1, #KEY); s.AddInt(s.GetCount() - 1, m_ ## KEY); }
#define BOOL(KEY)   { s.AddRow(); s.AddString(s.GetCount() - 1, #KEY); s.AddInt(s.GetCount() - 1, m_ ## KEY); }
  DOUBLE(ZoomSize)
  INT(MaxReferences)
  DOUBLE(GlitchLowTolerance)
  DOUBLE(ApproxLowTolerance)
  BOOL(AutoApproxTerms)
  INT(ApproxTerms)
  INT(WindowWidth)
  INT(WindowHeight)
  INT(WindowTop)
  INT(WindowLeft)
  INT(WindowBottom)
  INT(WindowRight)
  DOUBLE(ThreadsPerCore)
  INT(ThreadsReserveCore)
  BOOL(AnimateZoom)
  BOOL(ArbitrarySize)
  BOOL(ReuseReference)
  BOOL(AutoSolveGlitches)
  BOOL(Guessing)
  BOOL(SolveGlitchNear)
  BOOL(NoApprox)
  BOOL(Mirror)
  BOOL(AutoIterations)
  BOOL(ShowGlitches)
  BOOL(NoReuseCenter)
  INT(IsolatedGlitchNeighbourhood)
  INT(JitterSeed)
  INT(JitterShape)
  DOUBLE(JitterScale)
  BOOL(Derivatives)
  BOOL(ShowCrossHair)
  BOOL(UseNanoMB1)
  BOOL(UseNanoMB2)
  INT(OrderM)
  INT(OrderN)
  BOOL(InteriorChecking)
  DOUBLE(RadiusScale)
  INT(Shrink)
  BOOL(HalfColour)
  BOOL(SaveOverwrites)
  BOOL(ThreadedReference)
  INT(SIMDVectorSize)
  INT(SIMDChunkSize)
  { s.AddRow(); s.AddString(s.GetCount() - 1, "UseArgMinAbsZAsGlitchCenter"); s.AddInt(s.GetCount() - 1, GetUseArgMinAbsZAsGlitchCenter()); }
  INT(GlitchCenterMethod)
  BOOL(UseOpenCL)
  BOOL(OpenCLThreaded)
  INT(OpenCLPlatform)
  { s.AddRow(); s.AddString(s.GetCount() - 1, "EXRChannels"); s.AddInt(s.GetCount() - 1, pack_exr_channels(GetEXRChannels())); }
  BOOL(EXRParallel)
  BOOL(SaveNewtonProgress)
  BOOL(ExponentialMap)
  BOOL(DerivativeGlitch)
  BOOL(ReferenceStrictZero)
  { s.AddRow(); s.AddString(s.GetCount() - 1, "NumberTypes"); s.AddInt(s.GetCount() - 1, pack_number_type(GetNumberTypes())); }
  BOOL(UseRescaledSeries)
  BOOL(OpenResetsParameters)
  INT(TargetWidth)
  INT(TargetHeight)
  INT(TargetSupersample)
#undef DOUBLE
#undef INT
#undef BOOL
  { s.AddRow(); s.AddString(s.GetCount() - 1, "ImageWidth"); s.AddInt(s.GetCount() - 1, m_TargetWidth*m_TargetSupersample); }
  { s.AddRow(); s.AddString(s.GetCount() - 1, "ImageHeight"); s.AddInt(s.GetCount() - 1, m_TargetHeight*m_TargetSupersample); }

  { s.AddRow(); s.AddString(s.GetCount() - 1, "SettingsVersion"); s.AddInt(s.GetCount() - 1, kfs_version_number); }
  char *data = s.ToText(": ", "\n");
  std::string r(data);
  s.DeleteToText(data);
  return r;
}

bool Settings::OpenFile(const std::string &filename)
{
        std::string data;
	const char *extension = strrchr(filename.c_str(), '.');
	if (extension && 0 == strcmp(".png", extension))
	{
		data = ReadPNGComment(filename);
	}
	else if (extension && (0 == strcmp(".tif", extension) || 0 == strcmp(".tiff", extension)))
	{
		data = ReadTIFFComment(filename);
	}
	else if (extension && (0 == strcmp(".jpg", extension) || 0 == strcmp(".jpeg", extension)))
	{
		data = ReadJPEGComment(filename);
	}
	else if (extension && (0 == strcmp(".exr", extension)))
	{
		data = ReadEXRComment(filename);
	}
	else // anything else, probably .kfr/.kfs
	{
                std::ifstream hFile(filename, std::ios::in);
                if (!hFile)
                        return FALSE;

                std::stringstream buffer;
                buffer << hFile.rdbuf();
                data = buffer.str();
	}
  return FromText(data);
}

bool Settings::SaveFile(const std::string &filename, bool overwrite) const
{
    std::string szText(ToText());
#if __cplusplus >= 201703L
    if(!overwrite && std::filesystem::exists(filename))
        return false;
#elif defined(WINVER)
    if(!overwrite && PathFileExists(filename.c_str()))
        return false;
#endif
    std::ofstream hFile(filename, std::ios::trunc);
    if(!hFile)
        return false;

    hFile << szText;
    bool good = hFile.good();
    hFile.close();
    return good;
}
