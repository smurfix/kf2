#ifndef KF_FORMULA_H
#define KF_FORMULA_H 1

class CFixedFloat;
class floatexp;

bool reference_double(int m_nFractalType, int m_nPower, double *m_db_dxr, double *m_db_dxi, double *m_db_z, int &m_bStop, int &m_nRDone, int &m_nGlitchIter, int &m_nMaxIter, const CFixedFloat &Cr, const CFixedFloat &Ci, double g_SeedR, double g_SeedI, double g_FactorAR, double g_FactorAI, double terminate, double g_real, double g_imag, bool m_bGlitchLowTolerance, int &antal, double &test1, double &test2);
bool reference_long_double(int m_nFractalType, int m_nPower, long double *m_ldxr, long double *m_ldxi, double *m_db_z, int &m_bStop, int &m_nRDone, int &m_nGlitchIter, int &m_nMaxIter, const CFixedFloat &Cr, const CFixedFloat &Ci, double g_SeedR, double g_SeedI, double g_FactorAR, double g_FactorAI, double terminate, double g_real, double g_imag, bool m_bGlitchLowTolerance, int &antal, double &test1, double &test2);
bool reference_floatexp(int m_nFractalType, int m_nPower, floatexp *m_dxr, floatexp *m_dxi, double *m_db_z, int &m_bStop, int &m_nRDone, int &m_nGlitchIter, int &m_nMaxIter, const CFixedFloat &Cr, const CFixedFloat &Ci, double g_SeedR, double g_SeedI, double g_FactorAR, double g_FactorAI, double terminate, floatexp real, floatexp imag, bool m_bGlitchLowTolerance, int &antal, double &test1, double &test2);

bool perturbation_double(int m_nFractalType, int m_nPower, const double *m_db_dxr, const double *m_db_dxi, const double *m_db_z, int &antal, double &test1, double &test2, int &bGlitch, double m_nBailout2, int nMaxIter, int m_bNoGlitchDetection, double g_real, double g_imag, double g_FactorAR, double g_FactorAI, double xr, double xi, const double cr, const double ci);
bool perturbation_long_double(int m_nFractalType, int m_nPower, const long double *dxr, const long double *dxi, const double *m_db_z, int &antal, double &test1, double &test2, int &bGlitch, double m_nBailout2, int nMaxIter, int m_bNoGlitchDetection, double g_real, double g_imag, double g_FactorAR, double g_FactorAI, long double xr, long double xi, const long double cr, const long double ci);
bool perturbation_floatexp(int m_nFractalType, int m_nPower, const floatexp *m_dxr, const floatexp *m_dxi, const double *m_db_z, int &antal, double &test1, double &test2, int &bGlitch, double m_nBailout2, int nMaxIter, int m_bNoGlitchDetection, floatexp real, floatexp imag, double g_FactorAR, double g_FactorAI, floatexp xr, floatexp xi, const floatexp cr, const floatexp ci);

void combo5_addstrings(HWND hWnd, int combo);
int validate_power_for_fractal_type(int m_nFractalType, int m_nPower);
void update_power_dropdown_for_fractal_type(HWND hWnd, int combo, int m_nFractalType, int m_nPower);

#endif
