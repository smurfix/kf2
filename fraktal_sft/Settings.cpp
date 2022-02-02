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

const SettingsEntry SettingsData[] = {
#include "Settings.ed.inc"
};
const unsigned int nSettings = sizeof(SettingsData) / sizeof(SettingsData[0]);

std::map<std::string_view, int> SettingsPos;

static void init_pos() __attribute__ ((constructor));
static void init_pos() {
    for (unsigned int n = 0; n < nSettings; n++) {
        SettingsPos.insert({SettingsData[n].P.name, n});
    }
}

enum {
    seen_TargetHeight = 1<<16,
    seen_TargetWidth = 1<<17,
    seen_TargetSupersample = 1<<18,
};

const SettingsEntry *LookupParam(std::string_view name)
{
    auto pos = SettingsPos.find(name);
    if(pos == SettingsPos.end()) {
        std::cerr << "Param not found: " << name << std::endl;
        return nullptr;
    }
    return &SettingsData[pos->second];
}

std::string Settings::GetValue(const std::string_view name)
{
    auto param = LookupParam(name);
    if(param == nullptr)
        throw_invalid("Unknown param",name);
    return (this->*(param->get))();
}

void Settings::SetValue(const std::string_view name, const std::string_view value)
{
    auto param = LookupParam(name);
    if(param == nullptr)
        throw_invalid("Unknown param",name);
    if(param->set != nullptr)
        (this->*(param->set))(value);
    v_flags |= ((unsigned int)param->P.flags)<<16;
}

Settings::Settings()
: is_default(true)
#include "Settings.dv.inc"
{
#include "Settings.di.inc"
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

Settings::Settings(Settings &orig)
: is_default(false)
{
#include "Settings.cc.inc"
}

#include "Settings.gc.inc"
#include "Settings.sc.inc"

bool Settings::operator==(const SP_Settings &other) const
{
#include "Settings.eq.inc"
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

    // calculate di first, in low precision
    // avoids m_ZoomRadius becoming accidentally high precision
    Precision pLo(20u);
    CDecNumber di(2/zoom);

    // cannot be zero obviously
    long e = mpfr_get_exp(zoom.m_dec.backend().data());

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
    m_ZoomRadius = di.m_dec;
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

        long e = mpfr_get_exp(z.m_dec.backend().data());
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

void Settings::StartSetting(unsigned int flags)
{
  v_Re = "";
  v_Im = "";
  v_Zoom = "";
  v_GLSL = "";
  v_UseArgMinAbsZAsGlitchCenter = 0;
  v_SmoothMethod = -1;
  v_SmoothingMethod = -1;
  v_nX = -1;
  v_nY = -1;
  v_RotateAngle = 999;
  v_StretchAngle = 0;
  v_StretchAmount = 0;
  v_ImagPointsUp = false;
  v_version = -1;
  v_settings_version = -1;
  v_flags = flags;

  if((v_flags & KF_use_Params) && m_OpenResetsParameters) {
    // XXX do we want to trigger that off the version number instead?
    ResetParameters();
  }
}

bool Settings::FromText(const std::string &text, unsigned int flags)
{
  StartSetting(flags);

  for (auto line : str_iter(text, "\r\n")) {
    auto kv = str_keyval(line, ": ");
    auto param = LookupParam(kv.first);
    if(param == nullptr)
      continue;
    if(!(flags & (1<<param->P.type)))
      continue;

    if(param->set != nullptr)
        (this->*(param->set))(kv.second);
    v_flags |= ((unsigned int)param->P.flags)<<16;
  }

  return FinishSetting();
}

bool Settings::FinishSetting()
{
  if(v_flags & KF_use_Settings) {
    if (v_settings_version == -1)
        fprintf(stderr, "WARNING: file without SettingsVersion tag\n");
    else if (v_settings_version > kfs_version_number)
        fprintf(stderr, "WARNING: file format is newer than this EXE version\n");

    if (v_UseArgMinAbsZAsGlitchCenter)
      m_GlitchCenterMethod = 1;

    // old config file
    if (!(v_flags & seen_TargetSupersample)) {
      m_TargetSupersample = 0;
      if ((v_nX > 0) && (v_flags & seen_TargetWidth))
        m_TargetSupersample = v_nX/m_nX;
      else if ((v_nY > 0) && (v_flags & seen_TargetHeight))
        m_TargetSupersample = v_nY/m_nY;
      if (m_TargetSupersample == 0)
        m_TargetSupersample = 1;
    }

    if (v_flags & seen_TargetHeight)
      m_nX = m_TargetHeight*m_TargetSupersample;
    else if (v_nX != -1)
      m_nX = v_nX;

    if (v_flags & seen_TargetWidth)
      m_nY = m_TargetWidth*m_TargetSupersample;
    else if (v_nY != -1)
      m_nY = v_nY;
  }

  if(v_flags & KF_use_Params) {
    if (v_version == -1)
        fprintf(stderr, "WARNING: file without Version tag\n");
    else if (v_version > kfs_version_number)
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
  if(v_flags & KF_use_Location) {
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
    SetBailoutReal(1);
    SetBailoutImag(1);
    SetBailoutNormPreset(BailoutNorm_2);
    SetBailoutNormCustom(2);
    SetSeedR(0);
    SetSeedI(0);
    SetFactorAR(1);
    SetFactorAI(0);
}    

std::string Settings::ToText(unsigned int flags)
{
  std::ostringstream os;

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

  polar2 P = GetTransformPolar();

  v_RotateAngle = P.rotate * deg;
  v_StretchAngle = P.stretch_angle * deg;
  v_StretchAmount = std::log2(P.stretch_factor);
  v_ImagPointsUp = P.sign < 0;
  v_settings_version = kfs_version_number;
  v_version = kfs_version_number;
  v_flags = flags;

  if(v_flags & KF_use_Params) {
    v_GLSL = glsl_escape(m_sGLSL);
  }
  if(v_flags & KF_use_Location) {
    v_Re = GetRe();
    v_Im = GetIm();
    v_Zoom = GetZoom();
  }

  for (auto kv : SettingsPos) {
    auto param = &SettingsData[kv.second];
    if(!(v_flags & (1<<param->P.type)))
      continue;
    if(param->get == nullptr)
      continue;
    std::string res = (this->*(param->get))();
    os << kv.first << ": " << res << "\r\n";
  }

  return os.str();
}

bool Settings::OpenFile(const std::string &filename, unsigned int flags)
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
        return FromText(data, flags);
}

bool Settings::SaveFile(const std::string &filename, bool overwrite, unsigned int flags)
{
    std::string szText(ToText(flags));

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
