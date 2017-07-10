    /* Replace "dll.h" with the name of your header */
    #include <windows.h>
    #include <stdio.h>
    #include <float.h>
    #include <math.h>
    #define _GCC_
    #include "../fraktal_sft/floatexp.h"
    //#include "C:\pojects\ltest\fraktal_sft\ldbl_exp.h"
    #define TERM4
    #define TERM5
    #define TERM6
    #define TERM7
    #define _abs(a) ((_abs_val=(a))>0?_abs_val:-_abs_val)

#ifdef KF_LONG_DOUBLE_DLL
#define EXPORT extern "C" __declspec(dllexport)
#else
#define EXPORT
#define g_real dll_g_real
#define g_imag dll_g_imag
#define ISFLOATOK dll_ISFLOATOK
#define ConvertFromFixedFloat DLLConvertFromFixedFloat
#endif

#ifdef KF_FLOAT_BACKEND_MPFR
#include <mpfr.h>
#else
#include <gmp.h>
#endif

    double g_real=1;
    double g_imag=1;

    EXPORT void SetParts(double real,double imag)
    {
    	g_real=real;
    	g_imag=imag;
    }

    class complex
    {
    public:
    	long double m_r, m_i;
    	complex()
    	{
    		m_r = 0;
    		m_i = 0;
    	}
    	complex(long double r, long double i)
    	{
    		m_r = r;
    		m_i = i;
    	}
    	__inline complex &operator =(const complex &a)
    	{
    		m_r = a.m_r;
    		m_i = a.m_i;
    		return *this;
    	}
    	__inline complex operator *(const complex &a)
    	{
    		complex r;
    		r.m_r = m_r*a.m_r - m_i*a.m_i;
    		r.m_i = m_r*a.m_i + m_i*a.m_r;
    		return r;
    	}
    	__inline complex operator +(const complex &a)
    	{
    		complex r;
    		r.m_r = m_r + a.m_r;
    		r.m_i = m_i + a.m_i;
    		return r;
    	}
    	__inline complex operator -(const complex &a)
    	{
    		complex r;
    		r.m_r = m_r - a.m_r;
    		r.m_i = m_i - a.m_i;
    		return r;
    	}
    	__inline complex &operator +=(complex a)
    	{
    		m_r += a.m_r;
    		m_i += a.m_i;
    		return *this;
    	}
    	__inline complex operator ^(int exp)
    	{
    		complex r;
    		long double tmp;
    		r.m_r = m_r;
    		r.m_i = m_i;
    		int i;
    		for(i=1;i<exp;i++){
    			tmp = r.m_r*m_r - r.m_i*m_i;
    			r.m_i = r.m_r*m_i + r.m_i*m_r;
    			r.m_r=tmp;
    		}
    		return r;
    	}
    };

    BOOL APIENTRY DllMain (HINSTANCE hInst     /* Library instance handle. */ ,
                           DWORD reason        /* Reason this function is being called. */ ,
                           LPVOID reserved     /* Not used. */ )
    {
        switch (reason)
        {
          case DLL_PROCESS_ATTACH:
            break;

          case DLL_PROCESS_DETACH:
            break;

          case DLL_THREAD_ATTACH:
            break;

          case DLL_THREAD_DETACH:
            break;
        }

        /* Returns TRUE on success, FALSE on failure */
        return TRUE;
    }
    EXPORT int SizeOfLD()
    {
           return sizeof(long double);
    }
    EXPORT void *AllocateArray(int nSize)
    {
           long double *ret = new long double[nSize];
           return ret;
    }
    EXPORT void ReleaseArray(void *p)
    {
           long double *del = (long double *)p;
           delete [] del;
    }
    EXPORT void AssignInt(void *p,int nValue)
    {
           *((long double*)p) = nValue;
    }
    EXPORT void AssignDouble(void *p,double nDouble)
    {
           *((long double*)p) = nDouble;
    }
    EXPORT void AssignLD(void *p,void *ld)
    {
           *((long double*)p) = *((long double*)ld);
    }
    EXPORT void ToInt(void *p,int *pnValue)
    {
           *pnValue = (int)*((long double*)p);
    }
    EXPORT void ToDouble(void *p,double *pnDouble)
    {
           *pnDouble = *((long double*)p);
    }
    EXPORT void ToFloatExp(void *p,floatexp *pnFloatExp)
    {
           pnFloatExp->setLongDouble(*((long double*)p));
    }
    EXPORT void AssignFloatExp(void *p,floatexp *fe)
    {
    	*((long double*)p) = fe->toLongDouble();
    }

    EXPORT void Multiply(void *a,void *b,void *ret)
    {
           (*((long double*)ret)) = (*((long double*)a)) * (*((long double*)b));
    }
    EXPORT double SquareAdd(void *a,void *b)
    {
           return (*((long double*)a)) * (*((long double*)a)) + (*((long double*)b)) * (*((long double*)b));
    }
    EXPORT void Divide(void *a,void *b,void *ret)
    {
           (*((long double*)ret)) = (*((long double*)a)) / (*((long double*)b));
    }
    EXPORT void Add(void *a,void *b,void *ret)
    {
           (*((long double*)ret)) = (*((long double*)a)) + (*((long double*)b));
    }
    EXPORT void Subtract(void *a,void *b,void *ret)
    {
           (*((long double*)ret)) = (*((long double*)a)) - (*((long double*)b));
    }
    EXPORT void Negative(void *a)
    {
           (*((long double*)a)) = -(*((long double*)a));
    }
    EXPORT int GT(void *a,void *b)
    {
           return (*((long double*)a)) > (*((long double*)b));
    }
    EXPORT int LT(void *a,void *b)
    {
           return (*((long double*)a)) < (*((long double*)b));
    }
    EXPORT int Equal(void *a,void *b)
    {
           return (*((long double*)a)) == (*((long double*)b));
    }
    EXPORT void Print(void *a,char *szRet)
    {
           sprintf(szRet,"%ld",*((long double*)a));
    }
    EXPORT int Version()
    {
       return 20170309;
    }

    #define	FIXEDFLOAT_TYPE __int64
    #define FIXEDFLOAT_DIGITS 8
    #define FIXEDFLOAT_PARTMAX (FIXEDFLOAT_TYPE)100000000

#ifdef KF_FLOAT_BACKEND_MPFR
    EXPORT void ConvertFromFixedFloat(void *p,const mpfr_t value)
    {
        *((long double *)p) = mpfr_get_ld(value, MPFR_RNDN);
    }
#else
    EXPORT void ConvertFromFixedFloat(void *p,const mpf_t value)
    {
        using std::ldexp;
        signed long int e = 0;
        long double l = mpf_get_d_2exp(&e, value);
        l = ldexp(l, e);
        if ((mpf_sgn(value) >= 0) != (l >= 0))
            l = -l;
        *((long double *)p) = l;
    }
#endif

    //#define LDBL_MAX 3e4932L

    BOOL ISFLOATOK(long double a)
    {
    	__int64 *val = (__int64*)&a;
    	if((val[1]&0x7FFF)==0x7FFF || (val[1]&0x7FFF)==0)
    		return FALSE;
    	return TRUE;
    }
    BOOL ISFLOATOK(double a)
    {
    	if(a < DBL_MAX && a > -DBL_MAX)
    		return TRUE;
    	return 0;
    }

    EXPORT int Perturbation_Var(int antal,void *pdxr,void *pdxi, void* pDr, void*pDi, void* pD0r, void*pD0i,double *ptest1, double *ptest2, int m_nBailout2, int m_nMaxIter,double *m_db_z,BOOL *pGlitch,int m_nPower,int *m_pnExpConsts)
    {
    	long double *dxr = (long double*)pdxr;
    	long double *dxi = (long double*)pdxi;
    	long double Dr = *(long double*)pDr;
    	long double Di = *(long double*)pDi;
    	long double D0r = *(long double*)pD0r;
    	long double D0i = *(long double*)pD0i;
    	double &test1 = *ptest1;
    	double &test2 = *ptest2;
    	long double yr, yi;
        BOOL &bGlitch=*pGlitch;
        bGlitch=FALSE;
    	if(antal<m_nMaxIter && test1 <= m_nBailout2){
    		for(;antal<m_nMaxIter;antal++){
    			yr=dxr[antal]+Dr;
    			yi=dxi[antal]+Di;
    			test2=test1;
    			test1 = g_real*yr*yr + g_imag*yi*yi;
    		    if(test1<m_db_z[antal]){
                    test1 = m_nBailout2*2;
    	      		bGlitch=TRUE;
    		    }
    			if(test1 > m_nBailout2)
    				break;
    			complex X(dxr[antal],dxi[antal]);
    			complex D(Dr,Di);
    			complex D0(D0r,D0i);
    			complex c(m_pnExpConsts[0],0);
    			int nXExp=m_nPower-2, nDExp=2, ci=1;
    			complex Dn = c*(X^m_nPower-1)*D;
    			while(nXExp){
    				c.m_r = m_pnExpConsts[ci++];
    				Dn += c*(X^nXExp)*(D^nDExp);
    				nXExp--;
    				nDExp++;
    			}
    			Di = Dn.m_i;
    			Dr = Dn.m_r;
    		}
    	}
    	return antal;
    }
