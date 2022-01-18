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

#ifndef KF_SETTINGS_H
#define KF_SETTINGS_H 1

#include <string>
#include <memory>

#include "defs.h"
#include "hybrid_def.h"
#include "exr.h"
#include "matrix.h"
#include "main_numbertype.h"

struct SettingsEntry
{
    const char type;
    const char *const ctype;
    const char *const name;
    const char *const initial;
    const char *const descr;
};

class CFraktalSFT;

class Settings;
#if defined(WINVER) && defined(_DEBUG)
typedef Settings* SP_Settings;
#define NEW_SETTINGS new Settings
#else
typedef std::shared_ptr<Settings> SP_Settings;
#define NEW_SETTINGS std::make_shared<Settings>
#endif

class Settings
{

private:
  bool is_default;
  CFraktalSFT *parent;

#include "Settings.scv.inc"
#include "Settings.pcv.inc"
#include "Settings.lcv.inc"

public:
  Settings();
  Settings(Settings &) = default;

  inline CFraktalSFT *GetParent() { return parent; }
  void SetParent(CFraktalSFT *p);
  inline bool is_active() { return parent != nullptr; }

  bool FromText(const std::string &text, bool useSettings, bool useParams, bool useLocation);
  std::string ToText(bool useSettings, bool useParams, bool useLocation) const;

  bool OpenFile(const std::string &filename, bool useSettings, bool useParams, bool useLocation);
  bool SaveFile(const std::string &filename, bool overwrite, bool useSettings, bool useParams, bool useLocation) const;

  void SetPosition(const CDecNumber &re, const CDecNumber &im, const CDecNumber &zoom, unsigned digits10);

  void SetPosition(const std::string &szR, const std::string &szI, const std::string &szZ);
  void SetPosition(const char *const szR, const char *const szI, const char *const szZ);

  std::string GetRe() const;
  std::string GetIm() const;
  std::string GetZoom() const;

  void ResetParameters(); // compat with old files when reading params

#include "Settings.sgt.inc"
#include "Settings.pgt.inc"
#include "Settings.lgt.inc"

#include "Settings.sst.inc"
#include "Settings.pst.inc"
#include "Settings.lst.inc"

  inline void SetTransformPolar(const polar2 &P) {                                
    m_TransformPolar = P;
    m_TransformMatrix = polar_composition(P);
  }
 
  inline void SetTransformMatrix(const mat2 &M) {
    m_TransformMatrix = M;
    m_TransformPolar = polar_decomposition(M);
  }

  void SetFractalType(int nFractalType);

  void GenerateColors(int parts, int seed);
  void AddWave(int color, int parts, int seed);

  int GetMWCount();
  bool GetMW(int index, int &period, int &start, int &type);
  bool AddMW(int period, int start, int type);
  bool UpdateMW(int index, int period, int start, int type);
  bool DeleteMW(int index);

  COLOR14 GetKeyColor(int i);
  void SetKeyColor(COLOR14 col, int i);

  inline bool GetUseArgMinAbsZAsGlitchCenter() const { return m_GlitchCenterMethod == 1; }

  inline void GetTargetDimensions(int64_t *w, int64_t *h, int64_t *s) const
  {
    if (w) *w = m_TargetWidth;
    if (h) *h = m_TargetHeight;
    if (s) *s = m_TargetSupersample;
  };

  inline void SetTargetDimensions(int64_t w, int64_t h, int64_t s)
  {
    if (w < 5 || w > 100000) throw_invalid("TargetWidth",w);
    if (h < 5 || h > 100000) throw_invalid("TargetHeight",h);
    if (s < 1 || s > 256) throw_invalid("TargetSuperSample",s);
    m_TargetWidth = w;
    m_TargetHeight = h;
    m_TargetSupersample = s;
  };

  inline void SetImageSize(int nX, int nY)
  {
    if (nX/m_TargetSupersample < 5 || nX > 100000) throw_invalid("ImageWidth",nX);
    if (nY/m_TargetSupersample < 5 || nY > 100000) throw_invalid("ImageHeight",nY);
    m_TargetWidth = nX/m_TargetSupersample;
    m_TargetHeight = nY/m_TargetSupersample;
  }

  bool operator==(const SP_Settings &) const;
};

#endif
