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
  int m_ImageWidth;
  int m_ImageHeight;
  bool m_AnimateZoom;
  bool m_ArbitrarySize;
  bool m_ReuseReference;
  bool m_AutoSolveGlitches;
  bool m_SolveGlitchNear;
  bool m_NoApprox;
  bool m_Mirror;
  bool m_LongDoubleAlways;
  bool m_FloatExpAlways;
  bool m_AutoIterations;
  bool m_ShowGlitches;

public:

  Settings()
  : m_ZoomSize(4.0)
  , m_MaxReferences(69)
  , m_GlitchLowTolerance(false)
  , m_ApproxLowTolerance(true)
  , m_AutoApproxTerms(true)
  , m_ApproxTerms(10)
  , m_WindowWidth(640)
  , m_WindowHeight(360)
  , m_ImageWidth(640)
  , m_ImageHeight(360)
  , m_AnimateZoom(true)
  , m_ArbitrarySize(true)
  , m_ReuseReference(false)
  , m_AutoSolveGlitches(true)
  , m_SolveGlitchNear(false)
  , m_NoApprox(false)
  , m_Mirror(false)
  , m_LongDoubleAlways(false)
  , m_FloatExpAlways(false)
  , m_AutoIterations(true)
  , m_ShowGlitches(true)
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

  inline int    GetImageWidth() const { return m_ImageWidth; };
  inline void   SetImageWidth(int w) { m_ImageWidth = w; };

  inline int    GetImageHeight() const { return m_ImageHeight; };
  inline void   SetImageHeight(int h) { m_ImageHeight = h; };

  inline bool   GetAnimateZoom() const { return m_AnimateZoom; };
  inline void   SetAnimateZoom(bool b) { m_AnimateZoom = b; };

  inline bool   GetArbitrarySize() const { return m_ArbitrarySize; };
  inline void   SetArbitrarySize(bool b) { m_ArbitrarySize = b; };

  inline bool   GetReuseReference() const { return m_ReuseReference; };
  inline void   SetReuseReference(bool b) { m_ReuseReference = b; };

  inline bool   GetAutoSolveGlitches() const { return m_AutoSolveGlitches; };
  inline void   SetAutoSolveGlitches(bool b) { m_AutoSolveGlitches = b; };

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

};

#endif