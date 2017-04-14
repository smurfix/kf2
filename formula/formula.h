class CFixedFloat;
class floatexp;
#ifdef FORMULA
#undef FORMULA
#endif
#ifdef FORMULA2
#undef FORMULA2
#endif
#define FORMULA2(function,type,power) function ## _ ## type ## _ ## power
#define FORMULA(a,b,c) FORMULA2(a,b,c)
bool FORMULA(reference_double,TYPE,POWER)(int m_nFractalType, int m_nPower, double *m_db_dxr, double *m_d_dxi, double *m_db_z, int &m_bStop, int &m_nRDone, int &m_nGlitchIter, int &m_nMaxIter, const CFixedFloat &Cr, const CFixedFloat &Ci, double g_SeedR, double g_SeedI, double terminate, double g_real, double g_imag);
bool FORMULA(reference_long_double,TYPE,POWER)(int m_nFractalType, int m_nPower, long double *m_ldxr, long double *m_ldxi, double *m_db_z, int &m_bStop, int &m_nRDone, int &m_nGlitchIter, int &m_nMaxIter, const CFixedFloat &Cr, const CFixedFloat &Ci, double g_SeedR, double g_SeedI, double terminate, double g_real, double g_imag);
bool FORMULA(reference_floatexp,TYPE,POWER)(int m_nFractalType, int m_nPower, floatexp *m_dxr, floatexp *m_dxi, double *m_db_z, int &m_bStop, int &m_nRDone, int &m_nGlitchIter, int &m_nMaxIter, const CFixedFloat &Cr, const CFixedFloat &Ci, double g_SeedR, double g_SeedI, double terminate, floatexp real, floatexp imag);
bool FORMULA(perturbation_double,TYPE,POWER)(int m_nFractalType, int m_nPower, const double *m_db_dxr, const double *m_db_dxi, const double *m_db_z, int &antal, double &test1, double &test2, int &bGlitch, double m_nBailout2, int nMaxIter, int m_bNoGlitchDetection, double g_real, double g_imag, double xr, double xi, const double cr, const double ci);
bool FORMULA(perturbation_long_double,TYPE,POWER)(int m_nFractalType, int m_nPower, const long double *dxr, const long double *dxi, const double *m_db_z, int &antal, double &test1, double &test2, int &bGlitch, double m_nBailout2, int nMaxIter, int m_bNoGlitchDetection, double g_real, double g_imag, long double xr, long double xi, const long double cr, const long double ci);
bool FORMULA(perturbation_floatexp,TYPE,POWER)(int m_nFractalType, int m_nPower, const floatexp *m_dxr, const floatexp *m_dxi, const double *m_db_z, int &antal, double &test1, double &test2, int &bGlitch, double m_nBailout2, int nMaxIter, int m_bNoGlitchDetection, floatexp real, floatexp imag, floatexp xr, floatexp xi, const floatexp cr, const floatexp ci);
