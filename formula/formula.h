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

template <typename R, typename T> inline R broadcast(T x) { return x; }

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

bool reference_double     (const int m_nFractalType, const int m_nPower, double      *m_db_dxr, double      *m_db_dxi, double *m_db_z, int &m_bStop, int &m_nRDone, int &m_nGlitchIter, int &m_nMaxIter, const CFixedFloat &Cr, const CFixedFloat &Ci, const double g_SeedR, const double g_SeedI, const double g_FactorAR, const double g_FactorAI, const double terminate, const double g_real, const double g_imag, const bool m_bGlitchLowTolerance, int &antal, double &test1, double &test2);
bool reference_long_double(const int m_nFractalType, const int m_nPower, long double *m_ldxr,   long double *m_ldxi,   double *m_db_z, int &m_bStop, int &m_nRDone, int &m_nGlitchIter, int &m_nMaxIter, const CFixedFloat &Cr, const CFixedFloat &Ci, const double g_SeedR, const double g_SeedI, const double g_FactorAR, const double g_FactorAI, const double terminate, const double g_real, const double g_imag, const bool m_bGlitchLowTolerance, int &antal, double &test1, double &test2);
bool reference_floatexp   (const int m_nFractalType, const int m_nPower, floatexp    *m_dxr,    floatexp    *m_dxi,    double *m_db_z, int &m_bStop, int &m_nRDone, int &m_nGlitchIter, int &m_nMaxIter, const CFixedFloat &Cr, const CFixedFloat &Ci, const double g_SeedR, const double g_SeedI, const double g_FactorAR, const double g_FactorAI, const double terminate, const double g_real, const double g_imag, const bool m_bGlitchLowTolerance, int &antal, double &test1, double &test2);

bool reference_double     (const int m_nFractalType, const int m_nPower, double      *m_db_dxr, double      *m_db_dxi, double *m_db_z, int &m_bStop, int &m_nRDone, int &m_nGlitchIter, int &m_nMaxIter, const CFixedFloat &Cr, const CFixedFloat &Ci, const double g_SeedR, const double g_SeedI, const double g_FactorAR, const double g_FactorAI, const double terminate, const double g_real, const double g_imag, const bool m_bGlitchLowTolerance, int &antal, double &test1, double &test2, double      &dr, double      &di, const      double &daa, const      double &dab, const      double &dba, const      double &dbb);
bool reference_long_double(const int m_nFractalType, const int m_nPower, long double *m_ldxr,   long double *m_ldxi,   double *m_db_z, int &m_bStop, int &m_nRDone, int &m_nGlitchIter, int &m_nMaxIter, const CFixedFloat &Cr, const CFixedFloat &Ci, const double g_SeedR, const double g_SeedI, const double g_FactorAR, const double g_FactorAI, const double terminate, const double g_real, const double g_imag, const bool m_bGlitchLowTolerance, int &antal, double &test1, double &test2, long double &dr, long double &di, const long double &daa, const long double &dab, const long double &dba, const long double &dbb);
bool reference_floatexp   (const int m_nFractalType, const int m_nPower, floatexp    *m_dxr,    floatexp    *m_dxi,    double *m_db_z, int &m_bStop, int &m_nRDone, int &m_nGlitchIter, int &m_nMaxIter, const CFixedFloat &Cr, const CFixedFloat &Ci, const double g_SeedR, const double g_SeedI, const double g_FactorAR, const double g_FactorAI, const double terminate, const double g_real, const double g_imag, const bool m_bGlitchLowTolerance, int &antal, double &test1, double &test2, floatexp    &dr, floatexp    &di, const floatexp    &daa, const floatexp    &dab, const floatexp    &dba, const floatexp    &dbb);

bool perturbation_double     (const int m_nFractalType, const int m_nPower, const double      *m_db_dxr, const double      *m_db_dxi, const double *m_db_z, int &antal, double &test1, double &test2, int &bGlitch, const double m_nBailout2, const int nMaxIter, const int m_bNoGlitchDetection, const double g_real, const double g_imag, const double g_FactorAR, const double g_FactorAI, double      &xr, double      &xi, const double      cr, const double      ci);
bool perturbation_long_double(const int m_nFractalType, const int m_nPower, const long double *dxr,      const long double *dxi,      const double *m_db_z, int &antal, double &test1, double &test2, int &bGlitch, const double m_nBailout2, const int nMaxIter, const int m_bNoGlitchDetection, const double g_real, const double g_imag, const double g_FactorAR, const double g_FactorAI, long double &xr, long double &xi, const long double cr, const long double ci);
bool perturbation_floatexp   (const int m_nFractalType, const int m_nPower, const floatexp    *m_dxr,    const floatexp    *m_dxi,    const double *m_db_z, int &antal, double &test1, double &test2, int &bGlitch, const double m_nBailout2, const int nMaxIter, const int m_bNoGlitchDetection, const double g_real, const double g_imag, const double g_FactorAR, const double g_FactorAI, floatexp    &xr, floatexp    &xi, const floatexp    cr, const floatexp    ci);

bool perturbation_double     (const int m_nFractalType, const int m_nPower, const double      *m_db_dxr, const double      *m_db_dxi, const double *m_db_z, int &antal, double &test1, double &test2, int &bGlitch, const double m_nBailout2, const int nMaxIter, const int m_bNoGlitchDetection, const double g_real, const double g_imag, const double g_FactorAR, const double g_FactorAI, double      &xr, double      &xi, const double      cr, const double      ci, double      &dr, double      &di, const double      &e, const double      &h, const      double &daa, const      double &dab, const      double &dba, const      double &dbb);
bool perturbation_long_double(const int m_nFractalType, const int m_nPower, const long double *dxr,      const long double *dxi,      const double *m_db_z, int &antal, double &test1, double &test2, int &bGlitch, const double m_nBailout2, const int nMaxIter, const int m_bNoGlitchDetection, const double g_real, const double g_imag, const double g_FactorAR, const double g_FactorAI, long double &xr, long double &xi, const long double cr, const long double ci, long double &dr, long double &di, const long double &e, const long double &h, const long double &daa, const long double &dab, const long double &dba, const long double &dbb);
bool perturbation_floatexp   (const int m_nFractalType, const int m_nPower, const floatexp    *m_dxr,    const floatexp    *m_dxi,    const double *m_db_z, int &antal, double &test1, double &test2, int &bGlitch, const double m_nBailout2, const int nMaxIter, const int m_bNoGlitchDetection, const double g_real, const double g_imag, const double g_FactorAR, const double g_FactorAI, floatexp    &xr, floatexp    &xi, const floatexp    cr, const floatexp    ci, floatexp    &dr, floatexp    &di, const floatexp    &e, const floatexp    &h, const floatexp    &daa, const floatexp    &dab, const floatexp    &dba, const floatexp    &dbb);

bool perturbation2_double    (const int m_nFractalType, const int m_nPower, const double      *m_db_dxr, const double      *m_db_dxi, const double *m_db_z, int2  &antal, double2  &test1, double2  &test2, int2  &bGlitch, const double m_nBailout2, const int nMaxIter, const int m_bNoGlitchDetection, const double g_real, const double g_imag, const double g_FactorAR, const double g_FactorAI, double2       &xr, double2       &xi, const double2       &cr, const double2       &ci, const int &chunksize);
bool perturbation4_double    (const int m_nFractalType, const int m_nPower, const double      *m_db_dxr, const double      *m_db_dxi, const double *m_db_z, int4  &antal, double4  &test1, double4  &test2, int4  &bGlitch, const double m_nBailout2, const int nMaxIter, const int m_bNoGlitchDetection, const double g_real, const double g_imag, const double g_FactorAR, const double g_FactorAI, double4       &xr, double4       &xi, const double4       &cr, const double4       &ci, const int &chunksize);
bool perturbation8_double    (const int m_nFractalType, const int m_nPower, const double      *m_db_dxr, const double      *m_db_dxi, const double *m_db_z, int8  &antal, double8  &test1, double8  &test2, int8  &bGlitch, const double m_nBailout2, const int nMaxIter, const int m_bNoGlitchDetection, const double g_real, const double g_imag, const double g_FactorAR, const double g_FactorAI, double8       &xr, double8       &xi, const double8       &cr, const double8       &ci, const int &chunksize);
bool perturbation16_double   (const int m_nFractalType, const int m_nPower, const double      *m_db_dxr, const double      *m_db_dxi, const double *m_db_z, int16 &antal, double16 &test1, double16 &test2, int16 &bGlitch, const double m_nBailout2, const int nMaxIter, const int m_bNoGlitchDetection, const double g_real, const double g_imag, const double g_FactorAR, const double g_FactorAI, double16      &xr, double16      &xi, const double16      &cr, const double16      &ci, const int &chunksize);

void combo5_addstrings(HWND hWnd, const int combo);
int validate_power_for_fractal_type(const int m_nFractalType, const int m_nPower);
void update_power_dropdown_for_fractal_type(HWND hWnd, const int combo, const int m_nFractalType, const int m_nPower);

#endif
