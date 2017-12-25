#ifndef KF_FORMULA_H
#define KF_FORMULA_H 1

class CFixedFloat;
class floatexp;

bool reference_double     (const int m_nFractalType, const int m_nPower, double      *m_db_dxr, double      *m_db_dxi, double *m_db_z, int &m_bStop, int &m_nRDone, int &m_nGlitchIter, int &m_nMaxIter, const CFixedFloat &Cr, const CFixedFloat &Ci, const double g_SeedR, const double g_SeedI, const double g_FactorAR, const double g_FactorAI, const double terminate, const double g_real, const double g_imag, const bool m_bGlitchLowTolerance, int &antal, double &test1, double &test2, double      &dr, double      &di);
bool reference_long_double(const int m_nFractalType, const int m_nPower, long double *m_ldxr,   long double *m_ldxi,   double *m_db_z, int &m_bStop, int &m_nRDone, int &m_nGlitchIter, int &m_nMaxIter, const CFixedFloat &Cr, const CFixedFloat &Ci, const double g_SeedR, const double g_SeedI, const double g_FactorAR, const double g_FactorAI, const double terminate, const double g_real, const double g_imag, const bool m_bGlitchLowTolerance, int &antal, double &test1, double &test2, long double &dr, long double &di);
bool reference_floatexp   (const int m_nFractalType, const int m_nPower, floatexp    *m_dxr,    floatexp    *m_dxi,    double *m_db_z, int &m_bStop, int &m_nRDone, int &m_nGlitchIter, int &m_nMaxIter, const CFixedFloat &Cr, const CFixedFloat &Ci, const double g_SeedR, const double g_SeedI, const double g_FactorAR, const double g_FactorAI, const double terminate, const double g_real, const double g_imag, const bool m_bGlitchLowTolerance, int &antal, double &test1, double &test2, floatexp    &dr, floatexp    &di);

bool perturbation_double     (const int m_nFractalType, const int m_nPower, const double      *m_db_dxr, const double      *m_db_dxi, const double *m_db_z, int &antal, double &test1, double &test2, int &bGlitch, const double m_nBailout2, const int nMaxIter, const int m_bNoGlitchDetection, const double g_real, const double g_imag, const double g_FactorAR, const double g_FactorAI, double      &xr, double      &xi, const double      cr, const double      ci, double      &dr, double      &di);
bool perturbation_long_double(const int m_nFractalType, const int m_nPower, const long double *dxr,      const long double *dxi,      const double *m_db_z, int &antal, double &test1, double &test2, int &bGlitch, const double m_nBailout2, const int nMaxIter, const int m_bNoGlitchDetection, const double g_real, const double g_imag, const double g_FactorAR, const double g_FactorAI, long double &xr, long double &xi, const long double cr, const long double ci, long double &dr, long double &di);
bool perturbation_floatexp   (const int m_nFractalType, const int m_nPower, const floatexp    *m_dxr,    const floatexp    *m_dxi,    const double *m_db_z, int &antal, double &test1, double &test2, int &bGlitch, const double m_nBailout2, const int nMaxIter, const int m_bNoGlitchDetection, const double g_real, const double g_imag, const double g_FactorAR, const double g_FactorAI, floatexp    &xr, floatexp    &xi, const floatexp    cr, const floatexp    ci, floatexp    &dr, floatexp    &di);

void combo5_addstrings(HWND hWnd, const int combo);
int validate_power_for_fractal_type(const int m_nFractalType, const int m_nPower);
void update_power_dropdown_for_fractal_type(HWND hWnd, const int combo, const int m_nFractalType, const int m_nPower);

#endif
