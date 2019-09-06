/*
Kalles Fraktaler 2
Copyright (C) 2013-2017 Karl Runmo
Copyright (C) 2017-2019 Claude Heiland-Allen

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

#include <cstdint>

class CFixedFloat;
class floatexp;

// https://fractalforums.org/fractal-mathematics-and-new-theories/28/perturbation-theory/487/msg3170#msg3170
// |2w′(w+z)+1|/|δ0|+|w|(|w+2z|+|w|+2|z|)<ϵ/h
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

template <typename R, typename T> inline R broadcast(T x) { return R(x); }

typedef int64_t int2 __attribute__ ((vector_size (16)));
typedef double double2 __attribute__ ((vector_size (16)));
template <> inline double2 broadcast<double2,double>(double x) { double2 r = { x, x }; return r; }

typedef int64_t int4 __attribute__ ((vector_size (32)));
typedef double double4 __attribute__ ((vector_size (32)));
template <> inline double4 broadcast<double4,double>(double x) { double4 r = { x, x, x, x }; return r; }

typedef int64_t int8 __attribute__ ((vector_size (64)));
typedef double double8 __attribute__ ((vector_size (64)));
template <> inline double8 broadcast<double8,double>(double x) { double8 r = { x, x, x, x, x, x, x, x }; return r; }

typedef int64_t int16 __attribute__ ((vector_size (128)));
typedef double double16 __attribute__ ((vector_size (128)));
template <> inline double16 broadcast<double16,double>(double x) { double16 r = { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x }; return r; }

// reference

template <typename T>
bool reference
  ( const int m_nFractalType, const int m_nPower
  , T *m_db_dxr, T *m_db_dxi, double *m_db_z
  , int &m_bStop, int &m_nRDone, int &m_nGlitchIter, int &m_nMaxIter
  , const CFixedFloat &Cr, const CFixedFloat &Ci
  , const double g_SeedR, const double g_SeedI
  , const double g_FactorAR, const double g_FactorAI
  , const double terminate, const double g_real, const double g_imag
  , const bool m_bGlitchLowTolerance
  , int &antal, double &test1, double &test2
  );

// reference with derivatives

template <typename S, typename T>
bool reference
  ( const int m_nFractalType, const int m_nPower
  , T *m_db_dxr, T *m_db_dxi, double *m_db_z
  , int &m_bStop, int &m_nRDone, int &m_nGlitchIter, int &m_nMaxIter
  , const CFixedFloat &Cr0, const CFixedFloat &Ci0
  , const double g_SeedR, const double g_SeedI
  , const double g_FactorAR, const double g_FactorAI
  , const double terminate, const double g_real, const double g_imag
  , const bool m_bGlitchLowTolerance
  , int &antal, double &test1, double &test2
  , S &dr0, S &di0
  , const S &daa, const S &dab, const S &dba, const S &dbb
  );

// perturbation

template <typename T>
bool perturbation
  ( const int m_nFractalType, const int m_nPower
  , const T *m_dxr, const T *m_dxi, const double *m_db_z
  , int &antal, double &test1, double &test2, int &bGlitch
  , const double m_nBailout2, const int nMaxIter
  , const int m_bNoGlitchDetection, const double g_real, const double g_imag
  , const double g_FactorAR, const double g_FactorAI
  , T &xr, T &xi
  , const T &cr, const T &ci
  );

// perturbation with derivatives

template <typename T>
bool perturbation
  ( const int m_nFractalType, const int m_nPower
  , const T *m_dxr, const T *m_dxi, const double *m_db_z
  , int &antal, double &test1, double &test2, int &bGlitch
  , const double m_nBailout2, const int nMaxIter
  , const int m_bNoGlitchDetection, const double g_real, const double g_imag
  , const double g_FactorAR, const double g_FactorAI
  , T &xr, T &xi
  , const T &cr, const T &ci
  , T &dr, T &di
  , const T &e, const T &h
  , const T &daa, const T &dab, const T &dba, const T &dbb
  );

// perturbation with SIMD

template <typename intN, typename doubleN>
bool perturbation
  ( const int m_nFractalType, const int m_nPower
  , const double *m_db_dxr, const double *m_db_dxi, const double *m_db_z
  , intN &antal, doubleN &test1, doubleN &test2, intN &bGlitch
  , const double m_nBailout2, const int nMaxIter
  , const int m_bNoGlitchDetection, const double g_real, const double g_imag
  , const double g_FactorAR, const double g_FactorAI
  , doubleN &xr, doubleN &xi
  , const doubleN &cr, const doubleN &ci
  , const int &chunksize
  );

// perturbation with scaling

template <typename T>
bool perturbation
  ( const int m_nFractalType, const int m_nPower
  , const T *m_db_dxr, const T *m_db_dxi, const double *m_db_z
  , int &antal, double &test1, double &test2, int &bGlitch
  , const double m_nBailout2, const int nMaxIter
  , const int m_bNoGlitchDetection, const double g_real, const double g_imag
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
  , intN &antal0, doubleN &test10, doubleN &test20, intN &bGlitch0
  , double m_nBailout2, const int nMaxIter
  , const int m_bNoGlitchDetection, const double g_real, const double g_imag
  , const double g_FactorAR, const double g_FactorAI
  , doubleN &xr00, doubleN &xi00
  , const doubleN &cr0, const doubleN &ci0
  , const int &chunksize
  , const double s, const double S
  );

// perturbation with derivatives and scaling

template <typename D, typename Z>
bool perturbation
  ( int m_nFractalType, int m_nPower
  , const Z *m_db_dxr, const Z *m_db_dxi, const double *m_db_z
  , int &antal0, double &test10, double &test20, int &bGlitch
  , double m_nBailout2, const int nMaxIter
  , const int m_bNoGlitchDetection, const double g_real, const double g_imag
  , const double g_FactorAR, const double g_FactorAI
  , Z &xr0, Z &xi0
  , const Z &cr, const Z &ci
  , D &dr0, D &di0
  , const D &e, const D &h
  , const D &daa, const D &dab, const D &dba, const D &dbb
  , const Z &s, const Z &S
  );

// miscellaneous

bool scaling_supported(const int m_nFractalType, const int m_nPower, const bool derivatives);
void combo5_addstrings(HWND hWnd, const int combo);
int validate_power_for_fractal_type(const int m_nFractalType, const int m_nPower);
void update_power_dropdown_for_fractal_type(HWND hWnd, const int combo, const int m_nFractalType, const int m_nPower);

#endif
