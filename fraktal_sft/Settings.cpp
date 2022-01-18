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
#include <string>

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

static const double deg = 360 / 6.283185307179586;

SettingsEntry SettingsData[] = {
#include "Settings.sed.inc"
#include "Settings.ped.inc"
#include "Settings.led.inc"
    {0}
};

Settings::Settings()
: is_default(true)
, parent(nullptr)
#include "Settings.sdv.inc"
#include "Settings.pdv.inc"
#include "Settings.ldv.inc"
{
#include "Settings.sdi.inc"
#include "Settings.pdi.inc"
#include "Settings.ldi.inc"
    {
        // Mandelbrot power 2
        hybrid_line l =
            { { false, false, false, false, 2, 1.0, 0.0 }
            , { false, false, false, false, 0, 0.0, 0.0 }
            , hybrid_combine_add
            };
            hybrid_stanza s;
        s.lines.push_back(l);
        s.repeats = 1;
        hybrid_formula h;
        h.stanzas.push_back(s);
        h.loop_start = 0;
        m_HybridFormula = h;
    }

    SetTransformMatrix(mat2(1.0, 0.0, 0.0, 1.0));

    if(m_nX < 5)
        m_nX = m_TargetWidth*m_TargetSupersample;
    if(m_nY < 5)
        m_nY = m_TargetHeight*m_TargetSupersample;
    if(m_nX < 5 || m_nY < 5)
        throw_invalid("Settings init","size broken");
}

#include "Settings.sgc.inc"
#include "Settings.pgc.inc"
#include "Settings.lgc.inc"

#include "Settings.ssc.inc"
#include "Settings.psc.inc"
#include "Settings.lsc.inc"

void Settings::SetParent(CFraktalSFT *p)
{
    if(parent && p && parent != p)
        throw std::logic_error("parent already set");
    parent = p;
}

bool Settings::operator==(const SP_Settings &other) const
{
#include "Settings.seq.inc"
#include "Settings.peq.inc"
#include "Settings.leq.inc"
    return true;
}

std::string Settings::GetZoom() const
{
       CFixedFloat zoom = CFixedFloat(2) / m_ZoomRadius;
       floatexp zoomFE = floatexp(zoom);
       return zoomFE.toString();
}
std::string Settings::GetRe() const
{
       return m_CenterRe.ToText();
}
std::string Settings::GetIm() const
{
       return m_CenterIm.ToText();
}


void Settings::GenerateColors(int nParts, int nSeed)
{
    m_nParts = nParts;
    m_nSeed = nSeed;
    if (m_nSeed == -1)
        m_nSeed = GetTickCount();
    srand(m_nSeed);
    m_cKeys[0].r = m_cKeys[0].g = m_cKeys[0].b = m_cKeys[1024].r = m_cKeys[1024].g = m_cKeys[1024].b = 0;
    int i;
    for (i = (m_nSeed == 1 ? 1 : 0); i<1024; i++){
        m_cKeys[i].r = rand() % 256;
        m_cKeys[i].g = rand() % 256;
        m_cKeys[i].b = rand() % 256;
    }
    m_cKeys[m_nParts].r = m_cKeys[0].r;
    m_cKeys[m_nParts].g = m_cKeys[0].g;
    m_cKeys[m_nParts].b = m_cKeys[0].b;
}

void Settings::AddWave(int nColor, int nP, int nS)
{
    srand(GetTickCount());
    int period;
    if (nP == 0)
        return;
    if (nP == -1){
        period = rand() % (m_nParts<4 ? m_nParts / 2 : m_nParts);
        period = MakePrime(period);
    }
    else {
        period = nP;
    }
    int start;
    if (nS == -1)
        start = rand() % period;
    else
        start = nS;
    int i;
    for (i = 0; i<m_nParts; i++){
        int val = nP ? 127 + 127 * sin((double)(i + start) * 2 * pi*((double)period / (double)m_nParts)) : 0;
        switch (nColor){
        case 0:
            m_cKeys[i].r = val;
            break;
        case 1:
            m_cKeys[i].g = val;
            break;
        case 2:
            m_cKeys[i].b = val;
            break;
        case 3:
            m_cKeys[i].r = val;
            m_cKeys[i].g = val;
            m_cKeys[i].b = val;
            break;
        case 4:
            m_cKeys[i].r += (int)(m_cKeys[i].r + val) / 2;
            break;
        case 5:
            m_cKeys[i].g += (int)(m_cKeys[i].g + val) / 2;
            break;
        case 6:
            m_cKeys[i].b += (int)(m_cKeys[i].b + val) / 2;
            break;
        case 7:
            m_cKeys[i].r = (int)(m_cKeys[i].r + val) / 2;
            m_cKeys[i].g = (int)(m_cKeys[i].g + val) / 2;
            m_cKeys[i].b = (int)(m_cKeys[i].b + val) / 2;
            break;
        }
    }
}

COLOR14 Settings::GetKeyColor(int i)
{
    if (i<0 || i >= m_nParts)
        return m_cKeys[0];
    else
        return m_cKeys[i];
}
void Settings::SetKeyColor(COLOR14 col, int i)
{
    if (i<0 || i >= m_nParts)
        return;
    m_cKeys[i] = col;
}

int Settings::GetMWCount()
{
    return m_nMW;
}
bool Settings::GetMW(int index, int &period, int &start, int &type)
{
    if (index<0 || index >= m_nMW)
        return false;
    period = m_MW[index].nPeriod;
    start = m_MW[index].nStart;  
    type = m_MW[index].nType;  
    return true;
}
bool Settings::AddMW(int period, int start, int type)
{
    if (m_nMW == MULTIWAVE_MAX - 1) 
        return false;
    int i = m_nMW++;
    m_MW[i].nPeriod = period;
    m_MW[i].nStart = start;
    m_MW[i].nType = type;
    return true;
}
bool Settings::UpdateMW(int index, int period, int start, int type)
{
    if (index<0 || index >= m_nMW)
        return false;
    m_MW[index].nPeriod = period;
    m_MW[index].nStart = start;
    m_MW[index].nType = type;
    return true;
}
bool Settings::DeleteMW(int index)
{
    int i;
    if (!m_nMW)
        return false;
    m_nMW--;
    for (i = index; i<m_nMW; i++)
        m_MW[i] = m_MW[i + 1];
    return true;
}


void Settings::SetPosition(const CDecNumber &re, const CDecNumber &im, const CDecNumber &zoom,unsigned digits10)
{

    long e = 0;
    mpfr_get_d_2exp(&e, zoom.m_dec.backend().data(), MPFR_RNDN);

    m_nZoom = std::max(long(double(e)*0.30103), 1L);
    if(digits10 == 0)
        digits10 = 20 + std::max(20, m_nZoom);
    m_digits10 = digits10;
    Precision pHi(digits10);

    m_CenterRe.m_f.precision(digits10);
    m_CenterIm.m_f.precision(digits10);
    m_ZoomRadius.m_f.precision(20u);

    m_CenterRe = re.m_dec;
    m_CenterIm = im.m_dec;
    m_ZoomRadius = (2/zoom).m_dec;
}

void Settings::SetPosition(const std::string &szR, const std::string &szI, const std::string &szZ)
{
    /*
        one must get the required precision from the zoom level
        before using that precision when converting re,im from string
        otherwise the precision is likely to be too low (e.g. unzoomed)
        leading to incorrect images (precision loss from rounding)
    */
    try
    {
        Precision pLo(20u);
        CDecNumber z(szZ); // throws on bad string

        long e = 0;
        mpfr_get_d_2exp(&e, z.m_dec.backend().data(), MPFR_RNDN);
        unsigned digits10 = std::max(20L, long(20 + 0.30103 * e));
        Precision pHi(digits10);

        CDecNumber re(szR); // throws on bad string
        CDecNumber im(szI); // throws on bad string

        SetPosition(re,im,z, digits10);
    }
    catch (...)
    {
        std::cerr << "ERROR: SetPosition(): couldn't parse float (ignored)" << std::endl;
        /*
            if a float could not be parsed, the previous value will be used
            because all the parsing is done before the state is modified
        */
    }
}

void Settings::SetPosition(const char *szR, const char *szI, const char *szZ)
{
    return SetPosition(std::string(szR), std::string(szI), std::string(szZ));
}


static std::string glsl_escape(const std::string &s)
{
    std::ostringstream o;
    for (auto p = s.begin(); p != s.end(); ++p)
    {
        switch (*p)
        {
            case '\\': o << '\\' << '\\'; break;
            case ' ': o << '\\' << ' '; break;
            case '\t': o << '\\' << 't'; break;
            case '\r': o << '\\' << 'r'; break;
            case '\n': o << '\\' << 'n'; break;
            default: o << *p; break;
        }
    }
    return o.str();
}

static std::string glsl_unescape(const std::string &s)
{
    std::ostringstream o;
    for (auto p = s.begin(); p != s.end(); ++p)
    {
        switch (*p)
        {
            case '\\':
            {
                ++p;
                if (p != s.end())
                {
                    switch (*p)
                    {
                        case '\\': o << '\\'; break;
                        case ' ': o << ' '; break;
                        case 't': o << '\t'; break;
                        case 'r': o << '\r'; break;
                        case 'n': o << '\n'; break;
                        default: o << *p; break; // FIXME
                    }
                }
                else
                {
                    // FIXME
                }
                break;
            }
            default: o << *p; break;
        }
    }
    return o.str();
}

bool Settings::FromText(const std::string &text, bool useSettings, bool useParams, bool useLocation)
{
  char *data = strdup(text.c_str());
  CStringTable s(data, ": ", "\r\n");

  if(useParams && GetOpenResetsParameters()) {
    // XXX do we want to trigger that off the version number instead?
    ResetParameters();
  }

  std::string v_Re;
  std::string v_Im;
  std::string v_Zoom;

  std::string v_GLSL;

  int v_UseArgMinAbsZAsGlitchCenter = 0;
  int v_SmoothMethod = -1;
  int v_SmoothingMethod = -1;

  int v_nX = -1;
  int v_nY = -1;

  double v_RotateAngle = 999;
  double v_StretchAngle = 0;
  double v_StretchAmount = 0;
  bool v_ImagPointsUp = false;

  int version = -1;
  int settings_version = -1;

  if(useSettings) {
#   include "Settings.shr.inc"
  }
  if(useParams) {
#   include "Settings.phr.inc"
  }
  if(useLocation) {
#   include "Settings.lhr.inc"
  }

  if(useSettings) {
    if (settings_version == -1)
        fprintf(stderr, "WARNING: file without SettingsVersion tag\n");
    else if (settings_version > kfs_version_number)
        fprintf(stderr, "WARNING: file format is newer than this EXE version\n");

    if (v_UseArgMinAbsZAsGlitchCenter)
      m_GlitchCenterMethod = 1;

    if (v_nX != -1) {
      if (s.FindString(0, "TargetHeight") == -1)
        m_nX = v_nX;
    }
    if (v_nY != -1) {
      if (s.FindString(0, "TargetWidth") == -1)
        m_nY = v_nY;
    }

    if (s.FindString(0, "TargetSupersample") == -1 && (v_nY > 0 || v_nX > 0)) {
      // old config file
      m_TargetSupersample = (v_nY > 0) ? v_nY/m_nX : v_nX/m_nY;
    }
  }

  if(useParams) {
    if (version == -1)
        fprintf(stderr, "WARNING: file without Version tag\n");
    else if (version > kfs_version_number)
        fprintf(stderr, "WARNING: file format is newer than this EXE version\n");

    if (v_SmoothingMethod == -1)
      switch(v_SmoothMethod) {
        case -1: // neither is set: ignore
          break;
        default:
        case 0:
          SetBailoutRadiusPreset(BailoutRadius_High);
          SetSmoothMethod(SmoothMethod_Log);
          break;
        case 1:
          SetBailoutRadiusPreset(BailoutRadius_2);
          SetSmoothMethod(SmoothMethod_Sqrt);
          break;
        case 2:
          SetBailoutRadiusPreset(BailoutRadius_Low);
          SetSmoothMethod(SmoothMethod_Sqrt);
          break;
      }
    else
      SetSmoothMethod(v_SmoothingMethod);

    if (m_bTriangleInequalityAverage)
      SetNoApprox(true);
  }
  if(useLocation) {
    if(v_RotateAngle != 999)
      SetTransformPolar(polar2(v_ImagPointsUp ? -1 : 1, 1, v_RotateAngle / deg, std::exp2(v_StretchAmount), v_StretchAngle / deg));

    if(v_Re.length() && v_Im.length()) {
      if(v_Zoom.length() == 0)
        v_Zoom = "2";
      SetPosition(v_Re, v_Im, v_Zoom);
    }

  }
  if(v_GLSL.length())
    m_sGLSL = glsl_unescape(v_GLSL);

  std::free(data);
  return true;
}

void Settings::SetFractalType(int nFractalType)
{
    if (nFractalType < 0 || nFractalType > 102)
        nFractalType = 0;
    m_nFractalType = nFractalType;
    if ((1 <= m_nFractalType && m_nFractalType <= 4) && !(2 <= m_nPower && m_nPower <= 5))
        m_nPower = 2;
    if (m_nFractalType>2 && m_nPower>2)
        m_nPower = 2;

    SetReferenceStrictZero(nFractalType != 0);
}

void Settings::ResetParameters()
{
    // required for backward compatibility with older parameter files
    SetIterDiv(1);
    SetColorMethod(ColorMethod_Standard);
    SetDifferences(Differences_Traditional);    
    SetColorOffset(0);
    SetPhaseColorStrength(0);
    SetFractalType(0);  
    SetPower(2);  
    SetSlopes(false);
    SetSlopePower(50);
    SetSlopeRatio(20);
    SetSlopeAngle(45);
    m_real = 1;
    m_imag = 1;
    SetBailoutNormPreset(BailoutNorm_2);
    SetBailoutNormCustom(2);
    m_SeedR = 0;
    m_SeedI = 0;
    m_FactorAR = 1;
    m_FactorAI = 0;
}    

std::string Settings::ToText(bool useSettings, bool useParams, bool useLocation) const
{
  CStringTable s;

  int v_SmoothMethod;
  switch(GetSmoothMethod()) {
    case BailoutRadius_High:
      v_SmoothMethod = 0;
      break;
    case BailoutRadius_2:
      v_SmoothMethod = 1;
      break;
    case BailoutRadius_Low:
      v_SmoothMethod = 2;
      break;
    default:
      v_SmoothMethod = 0;
      break;
  }

  std::string v_GLSL = glsl_escape(m_sGLSL);

  polar2 P = GetTransformPolar();

  double v_RotateAngle = P.rotate * deg;
  double v_StretchAngle = P.stretch_angle * deg;
  double v_StretchAmount = std::log2(P.stretch_factor);
  bool v_ImagPointsUp = P.sign < 0;

  const std::string v_Re = GetRe();
  const std::string v_Im = GetIm();
  const std::string v_Zoom = GetZoom();

  if(useSettings) {
    int64_t settings_version = kfs_version_number;
#   include "Settings.shw.inc"
  }
  if(useParams) {
    int64_t version = kfs_version_number;
#   include "Settings.phw.inc"
  }
  if(useLocation) {
#   include "Settings.lhw.inc"
  }

  { s.AddRow(); s.AddString(s.GetCount() - 1, "UseArgMinAbsZAsGlitchCenter"); s.AddInt(s.GetCount() - 1, GetUseArgMinAbsZAsGlitchCenter()); }
  //{ s.AddRow(); s.AddString(s.GetCount() - 1, "EXRChannels"); s.AddInt(s.GetCount() - 1, pack_exr_channels(GetEXRChannels())); }
  //{ s.AddRow(); s.AddString(s.GetCount() - 1, "NumberTypes"); s.AddInt(s.GetCount() - 1, pack_number_type(GetNumberTypes())); }

  { s.AddRow(); s.AddString(s.GetCount() - 1, "ImageWidth"); s.AddInt(s.GetCount() - 1, m_nX*m_TargetSupersample); }
  { s.AddRow(); s.AddString(s.GetCount() - 1, "ImageHeight"); s.AddInt(s.GetCount() - 1, m_nY*m_TargetSupersample); }

  { s.AddRow(); s.AddString(s.GetCount() - 1, "SettingsVersion"); s.AddInt(s.GetCount() - 1, kfs_version_number); }
  char *data = s.ToText(": ", "\r\n");
  std::string r(data);
  s.DeleteToText(data);
  return r;
}

bool Settings::OpenFile(const std::string &filename, bool useSettings, bool useParams, bool useLocation)
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
                std::ifstream hFile(filename, std::ios::in | std::ios::binary);
                if (!hFile)
                        return FALSE;

                std::stringstream buffer;
                buffer << hFile.rdbuf();
                data = buffer.str();
	}
  return FromText(data, useSettings, useParams, useLocation);
}

bool Settings::SaveFile(const std::string &filename, bool overwrite, bool useSettings, bool useParams, bool useLocation) const
{
    std::string szText(ToText(useSettings, useParams, useLocation));

#if __cplusplus >= 201703L
    if(!overwrite && std::filesystem::exists(filename))
        return false;
#elif defined(WINVER)
    if(!overwrite && PathFileExists(filename.c_str()))
        return false;
#endif
    std::ofstream hFile(filename, std::ios::out | std::ios::binary | std::ios::trunc);
    if(!hFile)
        return false;

    hFile << szText;
    bool good = hFile.good();
    hFile.close();
    return good;
}
