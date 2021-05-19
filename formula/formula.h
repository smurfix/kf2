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

#ifndef KF_FORMULA_H
#define KF_FORMULA_H 1

#include <algorithm>
#include <cstdint>
#include <cmath>

#ifndef M_PI
#define M_PI 3.141592653589793
#endif

#include "../fraktal_sft/floatexp.h"

// https://fractalforums.org/fractal-mathematics-and-new-theories/28/perturbation-theory/487/msg3170#msg3170
// |2w'(w+z)+1|/|delta0|+|w|(|w+2z|+|w|+2|z|)<epsilon/h
template <typename R>
static inline R mag(const R &x, const R &y)
{
  using std::abs;
  return abs(x) + abs(y);
}

template <typename R>
static inline bool type_0_power_2_pixel_has_glitched(R cr, R ci, R zr, R zi, R Zr, R Zi, R dr, R di, R e, R h)
{
  R Zzr = Zr + zr;
  R Zzi = Zi + zi;
  R a = mag(2 * (dr * Zzr - di * Zzi) + 1, 2 * (dr * Zzi + di * Zzr));
  R b = mag(cr, ci) + mag(zr, zi) * (mag(zr + 2 * Zr, zi + 2 * Zi) + mag(zr, zi) + 2 * mag(Zr, Zi));
  return a * h < b * e;
}

static inline double diffabs(const double &c, const double &d)
{
  const double cd = c + d;
  const double c2d = 2.0 * c + d;
  return c >= 0.0 ? cd >= 0.0 ? d : -c2d : cd > 0.0 ? c2d : -d;
}

static inline long double diffabs(const long double &c, const long double &d)
{
  const long double cd = c + d;
  const long double c2d = 2.0 * c + d;
  return c >= 0.0 ? cd >= 0.0 ? d : -c2d : cd > 0.0 ? c2d : -d;
}

template<typename R>
static inline R pnorm(const double g_real, const double g_imag, const double p, const R x, const R y)
{
  using std::abs;
  using std::max;
  using std::pow;
  if (g_real == 1.0 && g_imag == 1.0 && p == 2.0)
  {
    return x * x + y * y;
  }
  if (p == 1.0)
  {
    return abs(g_real * abs(x) + g_imag * abs(y));
  }
  if (p == 1.0/0.0)
  {
    return abs(max(g_real * abs(x), g_imag * abs(y)));
  }
  return abs(g_real * pow(abs(x), p) + g_imag * pow(abs(y), p));
}

template <>
inline floatexp pnorm(const double g_real, const double g_imag, const double p, const floatexp xx, const floatexp yy)
{
  using std::abs;
  using std::max;
  using std::pow;
  double x(xx);
  double y(yy);
  if (g_real == 1.0 && g_imag == 1.0 && p == 2.0)
  {
    return x * x + y * y;
  }
  if (p == 1.0)
  {
    return abs(g_real * abs(x) + g_imag * abs(y));
  }
  if (p == 1.0/0.0)
  {
    return abs(max(g_real * abs(x), g_imag * abs(y)));
  }
  return abs(g_real * pow(abs(x), p) + g_imag * pow(abs(y), p));
}


using std::isinf;
using std::copysign;
using std::isnan;

#define I(T) inline T infnan_to_zero(const T &a) { return isinf(a) ? copysign((T)(1e30), a) : isnan(a) ? (T)(0) : a; }
I(float)
I(double)
I(long double)
#undef I

static inline float sgn(const float &a)
{
  return (a > 0) - (a < 0);
}
static inline double sgn(const double &a)
{
  return (a > 0) - (a < 0);
}
static inline long double sgn(const long double &a)
{
  return (a > 0) - (a < 0);
}
static inline floatexp sgn(const floatexp &a)
{
  return (a.val > 0) - (a.val < 0);
}

#include "simd1.h"
#include "simd2.h"
#include "simd3.h"
#include "simd4.h"

// reference

template <typename T>
bool reference
  ( const int m_nFractalType, const int m_nPower
  , T *m_db_dxr, T *m_db_dxi, double *m_db_z
  , bool &m_bStop, int64_t &m_nRDone, int64_t &m_nGlitchIter, int64_t &m_nMaxIter
  , const CFixedFloat &Cr, const CFixedFloat &Ci
  , const double g_SeedR, const double g_SeedI
  , const double g_FactorAR, const double g_FactorAI
  , const double terminate
  , const bool m_bGlitchLowTolerance
  );

// perturbation

template <typename T>
bool perturbation
  ( const int m_nFractalType, const int m_nPower
  , const T *m_dxr, const T *m_dxi, const double *m_db_z
  , int64_t &antal, double &test1, double &test2, double &phase, bool &bGlitch
  , const double m_nBailout2, const int64_t nMaxIter
  , const bool m_bNoGlitchDetection, const double g_real, const double g_imag, const double p
  , const double g_FactorAR, const double g_FactorAI
  , T &xr, T &xi
  , const T &cr, const T &ci
  );

// perturbation with derivatives

template <typename T>
bool perturbation
  ( const int m_nFractalType, const int m_nPower
  , const T *m_dxr, const T *m_dxi, const double *m_db_z
  , int64_t &antal, double &test1, double &test2, double &phase, bool &bGlitch
  , const double m_nBailout2, const int64_t nMaxIter
  , const bool m_bNoGlitchDetection, const double g_real, const double g_imag, const double p
  , const double g_FactorAR, const double g_FactorAI
  , T &xr, T &xi
  , const T &cr, const T &ci
  , T &Jxa, T &Jxb, T &Jya, T &Jyb
  , const T &e, const T &h
  , const T &daa, const T &dab, const T &dba, const T &dbb
  , const bool noDerivativeGlitch
  );

// perturbation with SIMD

template <typename intN, typename doubleN>
bool perturbation
  ( const int m_nFractalType, const int m_nPower
  , const double *m_db_dxr, const double *m_db_dxi, const double *m_db_z
  , intN &antal, doubleN &test1, doubleN &test2, doubleN &phase, intN &bGlitch
  , const double m_nBailout2, const int64_t nMaxIter
  , const intN &m_bNoGlitchDetection, const double g_real, const double g_imag, const double p
  , const double g_FactorAR, const double g_FactorAI
  , doubleN &xr, doubleN &xi
  , const doubleN &cr, const doubleN &ci
  , const int64_t chunksize
  );

// perturbation with SIMD and derivatives

template <typename intN, typename doubleN>
bool perturbation
  ( const int m_nFractalType, const int m_nPower
  , const double *m_dxr, const double *m_dxi, const double *m_db_z
  , intN &antal, doubleN &test1, doubleN &test2, doubleN &phase, intN &bGlitch
  , const double m_nBailout2, const int64_t nMaxIter
  , const intN &m_bNoGlitchDetection, const double g_real, const double g_imag, const double p
  , const double g_FactorAR, const double g_FactorAI
  , doubleN &xr, doubleN &xi
  , const doubleN &cr, const doubleN &ci
  , doubleN &Jxa, doubleN &Jxb, doubleN &Jya, doubleN &Jyb
  , const double e, const double h
  , const doubleN &daa, const doubleN &dab, const doubleN &dba, const doubleN &dbb
  , const int64_t chunksize
  , const bool noDerivativeGlitch
  );

// perturbation with scaling

template <typename T>
bool perturbation
  ( const int m_nFractalType, const int m_nPower
  , const T *m_db_dxr, const T *m_db_dxi, const double *m_db_z
  , int64_t &antal, double &test1, double &test2, double &phase, bool &bGlitch
  , const double m_nBailout2, const int64_t nMaxIter
  , const bool m_bNoGlitchDetection, const double g_real, const double g_imag, const double p
  , const double g_FactorAR, const double g_FactorAI
  , T &xr, T &xi
  , const T &cr, const T &ci
  , const T &s, const T &S
  );

// perturbation with SIMD and scaling

template <typename intN, typename doubleN>
bool perturbation
  ( int m_nFractalType, int m_nPower
  , const double *m_db_dxr, const double *m_db_dxi, const double *m_db_z
  , intN &antal0, doubleN &test10, doubleN &test20, doubleN &phase0, intN &bGlitch0
  , double m_nBailout2, const int64_t nMaxIter
  , const intN &m_bNoGlitchDetection, const double g_real, const double g_imag, const double p
  , const double g_FactorAR, const double g_FactorAI
  , doubleN &xr00, doubleN &xi00
  , const doubleN &cr0, const doubleN &ci0
  , const int64_t chunksize
  , const double s, const double S
  );

// perturbation with derivatives and scaling

template <typename D, typename Z>
bool perturbation
  ( int m_nFractalType, int m_nPower
  , const Z *m_db_dxr, const Z *m_db_dxi, const double *m_db_z
  , int64_t &antal0, double &test10, double &test20, double &phase0, bool &bGlitch
  , double m_nBailout2, const int64_t nMaxIter
  , const bool m_bNoGlitchDetection, const double g_real, const double g_imag, const double p
  , const double g_FactorAR, const double g_FactorAI
  , Z &xr0, Z &xi0
  , const Z &cr, const Z &ci
  , D &Jxa, D &Jxb, D &Jya, D &Jyb
  , const D &e, const D &h
  , const D &daa, const D &dab, const D &dba, const D &dbb
  , const Z &s, const Z &S
  , const bool noDerivativeGlitch
  );

// miscellaneous

typedef struct HWND__* HWND;
bool scaling_supported(const int m_nFractalType, const int m_nPower, const bool derivatives);
void combo5_addstrings(HWND hWnd, const int combo);
int validate_power_for_fractal_type(const int m_nFractalType, const int m_nPower);
void update_power_dropdown_for_fractal_type(HWND hWnd, const int combo, const int m_nFractalType, const int m_nPower);

#endif
