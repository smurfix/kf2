/*
Kalles Fraktaler 2
Copyright (C) 2013-2017 Karl Runmo
Copyright (C) 2017-2020 Claude Heiland-Allen

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

#define I(T) inline T infnan_to_zero(const T &a) { return isinf(a) ? copysign(1e30, a) : isnan(a) ? 0 : a; }
I(float)
I(double)
I(long double)
#undef I

typedef int64_t int2 __attribute__ ((vector_size (16)));
typedef double vdouble2 __attribute__ ((vector_size (16)));
struct double2
{
  vdouble2 v;
  inline double2() { vdouble2 r = { 0, 0 }; v = r; };
  inline double2(double x) { vdouble2 r = { x, x }; v = r;};
  inline double2(double x, double y) { vdouble2 r = { x, y }; v = r;};
  inline double2(const int2 &x) { vdouble2 r = { double(x[0]), double(x[1]) }; v = r; };
  inline double2(const double2 &x) { v = x.v; };
  inline double2(const vdouble2 &x) { v = x; };
  inline operator vdouble2() const { return v; };
  inline const double& operator[](int ix) const { return v[ix]; }
  inline double& operator[](int ix) { return v[ix]; }
};
static inline double2 operator-(const double2 &a) { return double2(-a.v); }
static inline double2 operator+(const double2 &a, const double2 &b) { return double2(a.v + b.v); }
static inline double2 operator-(const double2 &a, const double2 &b) { return double2(a.v - b.v); }
static inline double2 operator*(const double2 &a, const double2 &b) { return double2(a.v * b.v); }
static inline double2 operator/(const double2 &a, const double2 &b) { return double2(a.v / b.v); }
static inline int2 operator<(const double2 &a, const double &b) { return a.v < b; }
static inline int2 operator>(const double2 &a, const double &b) { return a.v > b; }
static inline int2 operator<=(const double2 &a, const double &b) { return a.v <= b; }
static inline int2 operator>=(const double2 &a, const double &b) { return a.v >= b; }
static inline int2 operator<(const double2 &a, const double2 &b) { return a.v < b.v; }
static inline int2 operator>(const double2 &a, const double2 &b) { return a.v > b.v; }
static inline int2 operator<=(const double2 &a, const double2 &b) { return a.v <= b.v; }
static inline int2 operator>=(const double2 &a, const double2 &b) { return a.v >= b.v; }
static inline double2 operator+(const double &a, const double2 &b) { return double2(a + b.v); }
static inline double2 abs(const double2 &a) { return double2(a.v < 0.0 ? -a.v : a.v); }
static inline double2 diffabs(const double &c, const double2 &d)
{
  const double2 cd = c + d;
  const double2 c2d = 2.0 * c + d;
  return double2(c >= 0.0 ? cd.v >= 0.0 ? d.v : -c2d.v : cd.v > 0.0 ? c2d.v : -d.v);
}
static inline int2 isnan(const double2 &a) { int2 r = { isnan(a[0]), isnan(a[1]) }; return r; }
static inline int2 isinf(const double2 &a) { int2 r = { isinf(a[0]), isinf(a[1]) }; return r; }
static inline double2 infnan_to_zero(const double2 &a) { double2 r = { infnan_to_zero(a[0]), infnan_to_zero(a[1]) }; return r; }
static inline double2 pow(const double2 &a, const double p) { using std::pow; double2 r = { pow(a[0], p), pow(a[1], p) }; return r; }
static inline double2 max(const double2 &a, const double2 &b) { return double2(a.v > b.v ? a.v : b.v); }
static inline double2 sqr(const double2 &a) { return a * a; }

typedef int64_t int4 __attribute__ ((vector_size (32)));
typedef double vdouble4 __attribute__ ((vector_size (32)));
struct double4
{
  vdouble4 v;
  inline double4() { vdouble4 r = { 0, 0, 0, 0 }; v = r; };
  inline double4(double x) { vdouble4 r = { x, x, x, x }; v = r;};
  inline double4(double x0, double x1, double x2, double x3) { vdouble4 r = { x0, x1, x2, x3 }; v = r;};
  inline double4(const int4 &x) { vdouble4 r = { double(x[0]), double(x[1]), double(x[2]), double(x[3]) }; v = r; };
  inline double4(const double4 &x) { v = x.v; };
  inline double4(const vdouble4 &x) { v = x; };
  inline operator vdouble4() const { return v; };
  inline const double& operator[](int ix) const { return v[ix]; }
  inline double& operator[](int ix) { return v[ix]; }
};
static inline double4 operator-(const double4 &a) { return double4(-a.v); }
static inline double4 operator+(const double4 &a, const double4 &b) { return double4(a.v + b.v); }
static inline double4 operator-(const double4 &a, const double4 &b) { return double4(a.v - b.v); }
static inline double4 operator*(const double4 &a, const double4 &b) { return double4(a.v * b.v); }
static inline double4 operator/(const double4 &a, const double4 &b) { return double4(a.v / b.v); }
static inline int4 operator<(const double4 &a, const double &b) { return a.v < b; }
static inline int4 operator>(const double4 &a, const double &b) { return a.v > b; }
static inline int4 operator<=(const double4 &a, const double &b) { return a.v <= b; }
static inline int4 operator>=(const double4 &a, const double &b) { return a.v >= b; }
static inline int4 operator<(const double4 &a, const double4 &b) { return a.v < b.v; }
static inline int4 operator>(const double4 &a, const double4 &b) { return a.v > b.v; }
static inline int4 operator<=(const double4 &a, const double4 &b) { return a.v <= b.v; }
static inline int4 operator>=(const double4 &a, const double4 &b) { return a.v >= b.v; }
static inline double4 operator+(const double &a, const double4 &b) { return double4(a + b.v); }
static inline double4 abs(const double4 &a) { return double4(a.v < 0.0 ? -a.v : a.v); }
static inline double4 diffabs(const double &c, const double4 &d)
{
  const double4 cd = c + d;
  const double4 c2d = 2.0 * c + d;
  return double4(c >= 0.0 ? cd.v >= 0.0 ? d.v : -c2d.v : cd.v > 0.0 ? c2d.v : -d.v);
}
static inline int4 isnan(const double4 &a) { int4 r = { isnan(a[0]), isnan(a[1]), isnan(a[2]), isnan(a[3]) }; return r; }
static inline int4 isinf(const double4 &a) { int4 r = { isinf(a[0]), isinf(a[1]), isinf(a[2]), isinf(a[3]) }; return r; }
static inline double4 infnan_to_zero(const double4 &a) { double4 r = { infnan_to_zero(a[0]), infnan_to_zero(a[1]), infnan_to_zero(a[2]), infnan_to_zero(a[3]) }; return r; }
static inline double4 pow(const double4 &a, const double p) { using std::pow; double4 r = { pow(a[0], p), pow(a[1], p), pow(a[2], p), pow(a[3], p) }; return r; }
static inline double4 max(const double4 &a, const double4 &b) { return double4(a.v > b.v ? a.v : b.v); }
static inline double4 sqr(const double4 &a) { return a * a; }

typedef int64_t int8 __attribute__ ((vector_size (64)));
typedef double vdouble8 __attribute__ ((vector_size (64)));
struct double8
{
  vdouble8 v;
  inline double8() { vdouble8 r = { 0, 0, 0, 0, 0, 0, 0, 0 }; v = r; };
  inline double8(double x) { vdouble8 r = { x, x, x, x, x, x, x, x }; v = r;};
  inline double8(double x0, double x1, double x2, double x3, double x4, double x5, double x6, double x7) { vdouble8 r = { x0, x1, x2, x3, x4, x5, x6, x7 }; v = r;};
  inline double8(const int8 &x) { vdouble8 r = { double(x[0]), double(x[1]), double(x[2]), double(x[3]), double(x[4]), double(x[5]), double(x[6]), double(x[7]) }; v = r; };
  inline double8(const double8 &x) { v = x.v; };
  inline double8(const vdouble8 &x) { v = x; };
  inline operator vdouble8() const { return v; };
  inline const double& operator[](int ix) const { return v[ix]; }
  inline double& operator[](int ix) { return v[ix]; }
};
static inline double8 operator-(const double8 &a) { return double8(-a.v); }
static inline double8 operator+(const double8 &a, const double8 &b) { return double8(a.v + b.v); }
static inline double8 operator-(const double8 &a, const double8 &b) { return double8(a.v - b.v); }
static inline double8 operator*(const double8 &a, const double8 &b) { return double8(a.v * b.v); }
static inline double8 operator/(const double8 &a, const double8 &b) { return double8(a.v / b.v); }
static inline int8 operator<(const double8 &a, const double &b) { return a.v < b; }
static inline int8 operator>(const double8 &a, const double &b) { return a.v > b; }
static inline int8 operator<=(const double8 &a, const double &b) { return a.v <= b; }
static inline int8 operator>=(const double8 &a, const double &b) { return a.v >= b; }
static inline int8 operator<(const double8 &a, const double8 &b) { return a.v < b.v; }
static inline int8 operator>(const double8 &a, const double8 &b) { return a.v > b.v; }
static inline int8 operator<=(const double8 &a, const double8 &b) { return a.v <= b.v; }
static inline int8 operator>=(const double8 &a, const double8 &b) { return a.v >= b.v; }
static inline double8 operator+(const double &a, const double8 &b) { return double8(a + b.v); }
static inline double8 abs(const double8 &a) { return double8(a.v < 0.0 ? -a.v : a.v); }
static inline double8 diffabs(const double &c, const double8 &d)
{
  const double8 cd = c + d;
  const double8 c2d = 2.0 * c + d;
  return double8(c >= 0.0 ? cd.v >= 0.0 ? d.v : -c2d.v : cd.v > 0.0 ? c2d.v : -d.v);
}
static inline int8 isnan(const double8 &a) { int8 r = { isnan(a[0]), isnan(a[1]), isnan(a[2]), isnan(a[3]), isnan(a[4]), isnan(a[5]), isnan(a[6]), isnan(a[7]) }; return r; }
static inline int8 isinf(const double8 &a) { int8 r = { isinf(a[0]), isinf(a[1]), isinf(a[2]), isinf(a[3]), isinf(a[4]), isinf(a[5]), isinf(a[6]), isinf(a[7]) }; return r; }
static inline double8 infnan_to_zero(const double8 &a) { double8 r = { infnan_to_zero(a[0]), infnan_to_zero(a[1]), infnan_to_zero(a[2]), infnan_to_zero(a[3]), infnan_to_zero(a[4]), infnan_to_zero(a[5]), infnan_to_zero(a[6]), infnan_to_zero(a[7]) }; return r; }
static inline double8 pow(const double8 &a, const double p) { using std::pow; double8 r = { pow(a[0], p), pow(a[1], p), pow(a[2], p), pow(a[3], p), pow(a[4], p), pow(a[5], p), pow(a[6], p), pow(a[7], p) }; return r; }
static inline double8 max(const double8 &a, const double8 &b) { return double8(a.v > b.v ? a.v : b.v); }
static inline double8 sqr(const double8 &a) { return a * a; }

typedef int64_t int16 __attribute__ ((vector_size (128)));
typedef double vdouble16 __attribute__ ((vector_size (128)));
struct double16
{
  vdouble16 v;
  inline double16() { vdouble16 r = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 }; v = r; };
  inline double16(double x) { vdouble16 r = { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x }; v = r;};
  inline double16(double x0, double x1, double x2, double x3, double x4, double x5, double x6, double x7, double x8, double x9, double x10, double x11, double x12, double x13, double x14, double x15) { vdouble16 r = { x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15 }; v = r;};
  inline double16(const int16 &x) { vdouble16 r = { double(x[0]), double(x[1]), double(x[2]), double(x[3]), double(x[4]), double(x[5]), double(x[6]), double(x[7]), double(x[8]), double(x[9]), double(x[10]), double(x[11]), double(x[12]), double(x[13]), double(x[14]), double(x[15]) }; v = r; };
  inline double16(const double16 &x) { v = x.v; };
  inline double16(const vdouble16 &x) { v = x; };
  inline operator vdouble16() const { return v; };
  inline const double& operator[](int ix) const { return v[ix]; }
  inline double& operator[](int ix) { return v[ix]; }
};
static inline double16 operator-(const double16 &a) { return double16(-a.v); }
static inline double16 operator+(const double16 &a, const double16 &b) { return double16(a.v + b.v); }
static inline double16 operator-(const double16 &a, const double16 &b) { return double16(a.v - b.v); }
static inline double16 operator*(const double16 &a, const double16 &b) { return double16(a.v * b.v); }
static inline double16 operator/(const double16 &a, const double16 &b) { return double16(a.v / b.v); }
static inline int16 operator<(const double16 &a, const double &b) { return a.v < b; }
static inline int16 operator>(const double16 &a, const double &b) { return a.v > b; }
static inline int16 operator<=(const double16 &a, const double &b) { return a.v <= b; }
static inline int16 operator>=(const double16 &a, const double &b) { return a.v >= b; }
static inline int16 operator<(const double16 &a, const double16 &b) { return a.v < b.v; }
static inline int16 operator>(const double16 &a, const double16 &b) { return a.v > b.v; }
static inline int16 operator<=(const double16 &a, const double16 &b) { return a.v <= b.v; }
static inline int16 operator>=(const double16 &a, const double16 &b) { return a.v >= b.v; }
static inline double16 operator+(const double &a, const double16 &b) { return double16(a + b.v); }
static inline double16 abs(const double16 &a) { return double16(a.v < 0.0 ? -a.v : a.v); }
static inline double16 diffabs(const double &c, const double16 &d)
{
  const double16 cd = c + d;
  const double16 c2d = 2.0 * c + d;
  return double16(c >= 0.0 ? cd.v >= 0.0 ? d.v : -c2d.v : cd.v > 0.0 ? c2d.v : -d.v);
}
static inline int16 isnan(const double16 &a) { int16 r = { isnan(a[0]), isnan(a[1]), isnan(a[2]), isnan(a[3]), isnan(a[4]), isnan(a[5]), isnan(a[6]), isnan(a[7]), isnan(a[8]), isnan(a[9]), isnan(a[10]), isnan(a[11]), isnan(a[12]), isnan(a[13]), isnan(a[14]), isnan(a[15]) }; return r; }
static inline int16 isinf(const double16 &a) { int16 r = { isinf(a[0]), isinf(a[1]), isinf(a[2]), isinf(a[3]), isinf(a[4]), isinf(a[5]), isinf(a[6]), isinf(a[7]), isinf(a[8]), isinf(a[9]), isinf(a[10]), isinf(a[11]), isinf(a[12]), isinf(a[13]), isinf(a[14]), isinf(a[15]) }; return r; }
static inline double16 infnan_to_zero(const double16 &a) { double16 r = { infnan_to_zero(a[0]), infnan_to_zero(a[1]), infnan_to_zero(a[2]), infnan_to_zero(a[3]), infnan_to_zero(a[4]), infnan_to_zero(a[5]), infnan_to_zero(a[6]), infnan_to_zero(a[7]), infnan_to_zero(a[8]), infnan_to_zero(a[9]), infnan_to_zero(a[10]), infnan_to_zero(a[11]), infnan_to_zero(a[12]), infnan_to_zero(a[13]), infnan_to_zero(a[14]), infnan_to_zero(a[15]) }; return r; }
static inline double16 pow(const double16 &a, const double p) { using std::pow; double16 r = { pow(a[0], p), pow(a[1], p), pow(a[2], p), pow(a[3], p), pow(a[4], p), pow(a[5], p), pow(a[6], p), pow(a[7], p), pow(a[8], p), pow(a[9], p), pow(a[10], p), pow(a[11], p), pow(a[12], p), pow(a[13], p), pow(a[14], p), pow(a[15], p) }; return r; }
static inline double16 max(const double16 &a, const double16 &b) { return double16(a.v > b.v ? a.v : b.v); }
static inline double16 sqr(const double16 &a) { return a * a; }

#define F2(F) static inline double2 F(const double2 &x) { return double2(F(x[0]), F(x[1])); }
F2(sin)
F2(cos)
F2(exp)
F2(expm1)
#undef F2

#define F4(F) static inline double4 F(const double4 &x) { return double4(F(x[0]), F(x[1]), F(x[2]), F(x[3])); }
F4(sin)
F4(cos)
F4(exp)
F4(expm1)
#undef F4

#define F8(F) static inline double8 F(const double8 &x) { return double8(F(x[0]), F(x[1]), F(x[2]), F(x[3]), F(x[4]), F(x[5]), F(x[6]), F(x[7])); }
F8(sin)
F8(cos)
F8(exp)
F8(expm1)
#undef F8

#define F16(F) static inline double16 F(const double16 &x) { return double16(F(x[0]), F(x[1]), F(x[2]), F(x[3]), F(x[4]), F(x[5]), F(x[6]), F(x[7]), F(x[8]), F(x[9]), F(x[10]), F(x[11]), F(x[12]), F(x[13]), F(x[14]), F(x[15])); }
F16(sin)
F16(cos)
F16(exp)
F16(expm1)
#undef F16

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
  );

// miscellaneous

typedef struct HWND__* HWND;
bool scaling_supported(const int m_nFractalType, const int m_nPower, const bool derivatives);
void combo5_addstrings(HWND hWnd, const int combo);
int validate_power_for_fractal_type(const int m_nFractalType, const int m_nPower);
void update_power_dropdown_for_fractal_type(HWND hWnd, const int combo, const int m_nFractalType, const int m_nPower);

#endif
