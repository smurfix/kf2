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

#ifndef KF_SETTINGS_H
#define KF_SETTINGS_H 1

#include <string>

// this sets the maximum number of references per image
#define OLD_GLITCH 10000

class Settings
{

private:

  double m_ZoomSize;
  int m_MaxReferences;
  bool m_GlitchLowTolerance;
  bool m_ApproxLowTolerance;
  bool m_AutoApproxTerms;
  int m_ApproxTerms;
  int m_WindowWidth;
  int m_WindowHeight;
  int m_WindowTop;
  int m_WindowLeft;
  int m_WindowBottom;
  int m_WindowRight;
  int m_ImageWidth;
  int m_ImageHeight;
  int m_ThreadsPerCore;
  bool m_AnimateZoom;
  bool m_ArbitrarySize;
  bool m_ReuseReference;
  bool m_AutoSolveGlitches;
  bool m_Guessing;
  bool m_SolveGlitchNear;
  bool m_NoApprox;
  bool m_Mirror;
  bool m_LongDoubleAlways;
  bool m_FloatExpAlways;
  bool m_AutoIterations;
  bool m_ShowGlitches;
  bool m_NoReuseCenter;
  int m_IsolatedGlitchNeighbourhood;
  int m_JitterSeed;
  int m_JitterShape;
  double m_JitterScale;
  bool m_Derivatives;
  bool m_ShowCrossHair;

public:

  Settings()
  : m_ZoomSize(4.0)
  , m_MaxReferences(10000)
  , m_GlitchLowTolerance(true)
  , m_ApproxLowTolerance(true)
  , m_AutoApproxTerms(true)
  , m_ApproxTerms(10)
  , m_WindowWidth(640)
  , m_WindowHeight(360)
  , m_WindowTop(-1)
  , m_WindowLeft(-1)
  , m_WindowBottom(-1)
  , m_WindowRight(-1)
  , m_ImageWidth(640)
  , m_ImageHeight(360)
  , m_ThreadsPerCore(1)
  , m_AnimateZoom(true)
  , m_ArbitrarySize(true)
  , m_ReuseReference(false)
  , m_AutoSolveGlitches(true)
  , m_Guessing(false)
  , m_SolveGlitchNear(false)
  , m_NoApprox(false)
  , m_Mirror(false)
  , m_LongDoubleAlways(false)
  , m_FloatExpAlways(false)
  , m_AutoIterations(true)
  , m_ShowGlitches(true)
  , m_NoReuseCenter(true)
  , m_IsolatedGlitchNeighbourhood(0)
  , m_JitterSeed(0)
  , m_JitterShape(0)
  , m_JitterScale(1)
  , m_Derivatives(true)
  , m_ShowCrossHair(false)
  { };

  bool FromText(const std::string &text);
  std::string ToText() const;

  bool OpenFile(const std::string &filename);
  bool SaveFile(const std::string &filename) const;

  inline double GetZoomSize() const { return m_ZoomSize; };
  inline void   SetZoomSize(double z) { m_ZoomSize = z; };

  inline int    GetMaxReferences() const { return m_MaxReferences; };
  inline void   SetMaxReferences(int r) {
    if (r < 0) r = 0;
    if (r > OLD_GLITCH) r = OLD_GLITCH;
    m_MaxReferences = r;
  };

  inline bool   GetGlitchLowTolerance() const { return m_GlitchLowTolerance; };
  inline void   SetGlitchLowTolerance(bool b) { m_GlitchLowTolerance = b; };

  inline bool   GetApproxLowTolerance() const { return m_ApproxLowTolerance; };
  inline void   SetApproxLowTolerance(bool b) { m_ApproxLowTolerance = b; }

  inline bool   GetAutoApproxTerms() const { return m_AutoApproxTerms; };
  inline void   SetAutoApproxTerms(bool b) { m_AutoApproxTerms = b; };

  inline int    GetApproxTerms() const { return m_ApproxTerms; };
  inline void   SetApproxTerms(int t) {
    if (t < 5) t = 5;
    if (t > 60) t = 60;
    m_ApproxTerms = t;
  };

  inline int    GetWindowWidth() const { return m_WindowWidth; };
  inline void   SetWindowWidth(int w) { m_WindowWidth = w; };

  inline int    GetWindowHeight() const { return m_WindowHeight; };
  inline void   SetWindowHeight(int h) { m_WindowHeight = h; };

  inline int    GetWindowTop() const { return m_WindowTop; };
  inline void   SetWindowTop(int w) { m_WindowTop = w; };

  inline int    GetWindowLeft() const { return m_WindowLeft; };
  inline void   SetWindowLeft(int w) { m_WindowLeft = w; };

  inline int    GetWindowBottom() const { return m_WindowBottom; };
  inline void   SetWindowBottom(int w) { m_WindowBottom = w; };

  inline int    GetWindowRight() const { return m_WindowRight; };
  inline void   SetWindowRight(int w) { m_WindowRight = w; };

  inline int    GetImageWidth() const { return m_ImageWidth; };
  inline void   SetImageWidth(int w) { m_ImageWidth = w; };

  inline int    GetImageHeight() const { return m_ImageHeight; };
  inline void   SetImageHeight(int h) { m_ImageHeight = h; };

  inline int    GetThreadsPerCore() const { return m_ThreadsPerCore; };
  inline void   SetThreadsPerCore(int t) { m_ThreadsPerCore = t; };

  inline bool   GetAnimateZoom() const { return m_AnimateZoom; };
  inline void   SetAnimateZoom(bool b) { m_AnimateZoom = b; };

  inline bool   GetArbitrarySize() const { return m_ArbitrarySize; };
  inline void   SetArbitrarySize(bool b) { m_ArbitrarySize = b; };

  inline bool   GetReuseReference() const { return m_ReuseReference; };
  inline void   SetReuseReference(bool b) { m_ReuseReference = b; };

  inline bool   GetAutoSolveGlitches() const { return m_AutoSolveGlitches; };
  inline void   SetAutoSolveGlitches(bool b) { m_AutoSolveGlitches = b; };

  inline bool   GetGuessing() const { return m_Guessing; };
  inline void   SetGuessing(bool b) { m_Guessing = b; };

  inline bool   GetSolveGlitchNear() const { return m_SolveGlitchNear; };
  inline void   SetSolveGlitchNear(bool b) { m_SolveGlitchNear = b; };

  inline bool   GetNoApprox() const { return m_NoApprox; };
  inline void   SetNoApprox(bool b) { m_NoApprox = b; };

  inline bool   GetMirror() const { return m_Mirror; };
  inline void   SetMirror(bool b) { m_Mirror = b; };

  inline bool   GetLongDoubleAlways() const { return m_LongDoubleAlways; };
  inline void   SetLongDoubleAlways(bool b) { m_LongDoubleAlways = b; };

  inline bool   GetFloatExpAlways() const { return m_FloatExpAlways; };
  inline void   SetFloatExpAlways(bool b) { m_FloatExpAlways = b; };

  inline bool   GetAutoIterations() const { return m_AutoIterations; };
  inline void   SetAutoIterations(bool b) { m_AutoIterations = b; };

  inline bool   GetShowGlitches() const { return m_ShowGlitches; };
  inline void   SetShowGlitches(bool b) { m_ShowGlitches = b; };

  inline bool   GetNoReuseCenter() const { return m_NoReuseCenter; };
  inline void   SetNoReuseCenter(bool b) { m_NoReuseCenter = b; };

  inline int    GetIsolatedGlitchNeighbourhood() const { return m_IsolatedGlitchNeighbourhood; };
  inline void   SetIsolatedGlitchNeighbourhood(int n) { m_IsolatedGlitchNeighbourhood = n; };

  inline int    GetJitterSeed() const { return m_JitterSeed; };
  inline void   SetJitterSeed(int n) { m_JitterSeed = n; };

  inline int    GetJitterShape() const { return m_JitterShape; };
  inline void   SetJitterShape(int n) { m_JitterShape = 0 <= n && n <= 1 ? n : 0; };

  inline double GetJitterScale() const { return m_JitterScale; };
  inline void   SetJitterScale(double n) { m_JitterScale = n; };

  inline bool   GetDerivatives() const { return m_Derivatives; };
  inline void   SetDerivatives(bool b) { m_Derivatives = b; };

  inline bool   GetShowCrossHair() const { return m_ShowCrossHair; };
  inline void   SetShowCrossHair(bool b) { m_ShowCrossHair = b; };
};

#endif
