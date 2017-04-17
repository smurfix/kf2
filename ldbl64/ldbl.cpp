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

#ifndef KF_FLOAT_BACKEND_CUSTOM
#ifdef KF_FLOAT_BACKEND_MPFR
#include <mpfr.h>
#else
#include <gmp.h>
#endif
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
    
#ifdef KF_FLOAT_BACKEND_CUSTOM
    EXPORT void ConvertFromFixedFloat(void *p,int nValues, __int64 *pValues, BOOL bSign)
    {
    	long double a=0;
    	int n;
    	int nStart = 0;
    	for(nStart=0;nStart<nValues;nStart++)
    		if(pValues[nStart])
    			break;
    	nStart+=32/FIXEDFLOAT_DIGITS;
    	if(nStart>nValues)
    		nStart=nValues;
    	for(n=nStart-1;n>=0;n--)
    		a = a/FIXEDFLOAT_PARTMAX + pValues[n];
    	if(bSign)
    		a=-a;
    	*((long double *)p) = a;
    }
#else
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
    
long double lb_abs_ldb(long double c, long double d)
{
    long double _abs_val;
	long double abs_val;
	if (c>0){
		if (c + d>0)
			abs_val = d;
		else if (d == -c)
			abs_val = d;
		else if (d<-c)
			abs_val = -d - 2 * c;
	}
	else if (c == 0)
		abs_val = _abs(d);
	else if (c < 0){
		if (c + d>0)
			abs_val = d + 2 * c;
		else if (d == -c)
			abs_val = -d;
		else if (d < -c)
			abs_val = -d;
	}
	return abs_val;
}

EXPORT int LDBL_MandelCalc(int m_nFractalType, int m_nPower, int antal,void *pdxr,void *pdxi, void* pDr, void*pDi, void* pD0r, void*pD0i,double *ptest1, double *ptest2, int m_nBailout2, int m_nMaxIter,double *m_db_z,BOOL *pGlitch,double dbFactorAR, double dbFactorAI)
{
    long double _abs_val;
	long double *dxr = (long double*)pdxr;
	long double *dxi = (long double*)pdxi;
	long double Dr = *(long double*)pDr;
	long double Di = *(long double*)pDi;
	long double D0r = *(long double*)pD0r;
	long double D0i = *(long double*)pD0i;
	long double Dnr, Dni;
	double &test1 = *ptest1;
	double &test2 = *ptest2;
	long double yr, yi;
    BOOL &bGlitch=*pGlitch;
    bGlitch=FALSE;
    int nMaxIter = m_nMaxIter;

			// Buffalo
			if (m_nFractalType == 2 && m_nPower == 2){
				if (antal<nMaxIter && test1 <= m_nBailout2){
					for (; antal<nMaxIter && test1 <= m_nBailout2; antal++){
						yr = dxr[antal] + Dr;
						yi = dxi[antal] + Di;
						test2 = test1;
						test1 = g_real*yr*yr + g_imag*yi*yi;
						if (test1<m_db_z[antal]){
							
								test1 = m_nBailout2 * 2;
							bGlitch = TRUE;
						}
						if(test1 > m_nBailout2)
							break;
						long double &r = dxr[antal];
						long double &i = dxi[antal];
						long double &a = Dr;
						long double &b = Di;
						long double a2 = a*a;
						long double b2 = b*b;
						long double &a0 = D0r;
						long double &b0 = D0i;

						long double c = r*r - i*i;
						long double d = 2 * r*a + a2 - 2 * i*b - b2;
						if (c>0){
							if (c + d>0)
								Dnr = d;
							else if (d == -c)
								Dnr = d;
							else if (d<-c)
								Dnr = -d - 2 * c;
						}
						else if (c == 0)
							Dnr = _abs(d);
						else if (c < 0){
							if (c + d>0)
								Dnr = d + 2 * c;
							else if (d == -c)
								Dnr = -d;
							else if (d < -c)
								Dnr = -d;
						}
						Dnr = Dnr + a0;

						c = r*i;
						d = r*b + a*i + a*b;
						if (c>0){
							if (c + d>0)
								Dni = d;
							else if (d == -c)
								Dni = d;
							else if (d<-c)
								Dni = -d - 2 * c;
						}
						else if (c == 0)
							Dni = _abs(d);
						else if (c < 0){
							if (c + d>0)
								Dni = d + 2 * c;
							else if (d == -c)
								Dni = -d;
							else if (d < -c)
								Dni = -d;
						}
						Dni = b0 - 2 * Dni;


						Di = Dni;
						Dr = Dnr;
					}
				}
			}
			// Cubic Buffalo
			else if (m_nFractalType == 2 && m_nPower == 3){
				if (antal<nMaxIter && test1 <= m_nBailout2){
					for (; antal<nMaxIter && test1 <= m_nBailout2; antal++){
						yr = dxr[antal] + Dr;
						yi = dxi[antal] + Di;
						test2 = test1;
						test1 = g_real*yr*yr + g_imag*yi*yi;
						if (test1<m_db_z[antal]){
							
								test1 = m_nBailout2 * 2;
							bGlitch = TRUE;
						}
						if(test1 > m_nBailout2)
							break;
						long double &r = dxr[antal];
						long double &i = dxi[antal];
						long double &a = Dr;
						long double &b = Di;
						long double &a0 = D0r;
						long double &b0 = D0i;
						long double r2 = r*r;
						long double i2 = i*i;
						long double a2 = a*a;
						long double b2 = b*b;

						long double c = r*(r2 - 3 * i2);
						long double d = a*(3 * r2 + a2) + 3 * r*(a2 - 2 * i*b - b2) - 3 * a*(i2 + 2 * i*b + b2);
						if (c>0){
							if (c + d>0)
								Dnr = d;
							else if (d == -c)
								Dnr = d;
							else if (d<-c)
								Dnr = -d - 2 * c;
						}
						else if (c == 0)
							Dnr = _abs(d);
						else if (c < 0){
							if (c + d>0)
								Dnr = d + 2 * c;
							else if (d == -c)
								Dnr = -d;
							else if (d < -c)
								Dnr = -d;
						}
						Dnr = Dnr + a0;

						c = i*(3 * r2 - i2);
						d = 3 * i*(2 * r*a + a2 - b2) + 3 * b*(r2 + 2 * r*a + a2) - b*(3 * i2 + b2);
						if (c>0){
							if (c + d>0)
								Dni = d;
							else if (d == -c)
								Dni = d;
							else if (d<-c)
								Dni = -d - 2 * c;
						}
						else if (c == 0)
							Dni = _abs(d);
						else if (c < 0){
							if (c + d>0)
								Dni = d + 2 * c;
							else if (d == -c)
								Dni = -d;
							else if (d < -c)
								Dni = -d;
						}
						Dni = Dni + b0;

						Di = Dni;
						Dr = Dnr;
					}
				}
			}
			// 4th Power Buffalo
			else if (m_nFractalType == 2 && m_nPower == 4){
				if (antal<nMaxIter && test1 <= m_nBailout2){
					for (; antal<nMaxIter && test1 <= m_nBailout2; antal++){
						yr = dxr[antal] + Dr;
						yi = dxi[antal] + Di;
						test2 = test1;
						test1 = g_real*yr*yr + g_imag*yi*yi;
						if (test1<m_db_z[antal]){
							
								test1 = m_nBailout2 * 2;
							bGlitch = TRUE;
						}
						if(test1 > m_nBailout2)
							break;
						long double &x = dxr[antal];
						long double &y = dxi[antal];
						long double &a = Dr;
						long double &b = Di;
						long double &a0 = D0r;
						long double &b0 = D0i;
						long double x2 = x*x;
						long double y2 = y*y;
						long double a2 = a*a;
						long double b2 = b*b;
						long double xd = x+a;
						long double xd2 = xd*xd;
						long double yd = y+b;
						long double yd2 = yd*yd;

						//Dnr = _abs(xd2*xd2 + yd2*yd2 - 6*xd2*yd2) - _abs(x2*x2 + y2*y2 - 6*x2*y2) + a0;
						//Dnr = _abs(x2*x2+y2*y2-6*x2*y2 + 4*x2*x*a+6*x2*a2+4*x*a2*a+a2*a2+4*y2*y*b+6*y2*b2+4*y*b2*b+b2*b2-12*x2*y*b-6*x2*b2-12*x*a*y2-24*x*a*y*b-12*x*a*b2-6*a2*y2-12*a2*y*b-6*a2*b2) - _abs(x2*x2+y2*y2-6*x2*y2) + a0;
						long double c = x2*x2+y2*y2-6*x2*y2;
						long double d = 4*x2*x*a+6*x2*a2+4*x*a2*a+a2*a2+4*y2*y*b+6*y2*b2+4*y*b2*b+b2*b2-12*x2*y*b-6*x2*b2-12*x*a*y2-24*x*a*y*b-12*x*a*b2-6*a2*y2-12*a2*y*b-6*a2*b2;
						if (c>0){
							if (c + d>0)
								Dnr = d;
							else if (d == -c)
								Dnr = d;
							else if (d<-c)
								Dnr = -d - 2 * c;
						}
						else if (c == 0)
							Dnr = _abs(d);
						else if (c < 0){
							if (c + d>0)
								Dnr = d + 2 * c;
							else if (d == -c)
								Dnr = -d;
							else if (d < -c)
								Dnr = -d;
						}
						Dnr+=a0;

						//Dni = _abs(-4*x*y2*y-4*a*y2*y-12*b*x*y2-12*a*b*y2+4*x2*x*y+12*a*x2*y-12*b2*x*y+12*a2*x*y-12*a*b2*y+4*a2*a*y+4*b*x2*x+12*a*b*x2-4*b2*b*x+12*a2*b*x-4*a*b2*b+4*a2*a*b) - _abs(4*x2*x*y-4*x*y2*y) + b0;
						c = 4*x2*x*y-4*x*y2*y;
						d = -4*a*y2*y-12*b*x*y2-12*a*b*y2+12*a*x2*y-12*b2*x*y+12*a2*x*y-12*a*b2*y+4*a2*a*y+4*b*x2*x+12*a*b*x2-4*b2*b*x+12*a2*b*x-4*a*b2*b+4*a2*a*b;
						if (c>0){
							if (c + d>0)
								Dni = d;
							else if (d == -c)
								Dni = d;
							else if (d<-c)
								Dni = -d - 2 * c;
						}
						else if (c == 0)
							Dni = _abs(d);
						else if (c < 0){
							if (c + d>0)
								Dni = d + 2 * c;
							else if (d == -c)
								Dni = -d;
							else if (d < -c)
								Dni = -d;
						}
						Dni+=b0;

						Di = Dni;
						Dr = Dnr;
					}
				}
			}
			// 5th Power Buffalo
			else if (m_nFractalType == 2 && m_nPower == 5){
				if (antal<nMaxIter && test1 <= m_nBailout2){
					for (; antal<nMaxIter && test1 <= m_nBailout2; antal++){
						yr = dxr[antal] + Dr;
						yi = dxi[antal] + Di;
						test2 = test1;
						test1 = g_real*yr*yr + g_imag*yi*yi;
						if (test1<m_db_z[antal]){
							
								test1 = m_nBailout2 * 2;
							bGlitch = TRUE;
						}
						if(test1 > m_nBailout2)
							break;
						long double &x = dxr[antal];
						long double &y = dxi[antal];
						long double &a = Dr;
						long double &b = Di;
						long double &a0 = D0r;
						long double &b0 = D0i;
						long double x2 = x*x;
						long double y2 = y*y;
						long double a2 = a*a;
						long double b2 = b*b;
						long double xd = x+a;
						long double xd2 = xd*xd;
						long double yd = y+b;
						long double yd2 = yd*yd;

						//Dnr = _abs(5*x*y2*y2+20*x*b*y2*y-10*x2*x*y2-30*x2*a*y2+30*x*b2*y2-30*x*a2*y2-20*x2*x*b*y-60*x2*a*b*y+20*x*b2*b*y-60*x*a2*b*y+x2*x2*x+5*x2*x2*a-10*x2*x*b2+10*x2*x*a2-30*x2*a*b2+10*x2*a2*a+5*x*b2*b2-30*x*a2*b2+5*x*a2*a2+5*a*y2*y2+20*a*b*y2*y+30*a*b2*y2-10*a2*a*y2+20*a*b2*b*y-20*a2*a*b*y+5*a*b2*b2-10*a2*a*b2+a2*a2*a) - _abs(5*x*y2*y2-10*x2*x*y2+x2*x2*x) + a0;
						long double c = 5*x*y2*y2-10*x2*x*y2+x2*x2*x;
						long double d = 20*x*b*y2*y-30*x2*a*y2+30*x*b2*y2-30*x*a2*y2-20*x2*x*b*y-60*x2*a*b*y+20*x*b2*b*y-60*x*a2*b*y+5*x2*x2*a-10*x2*x*b2+10*x2*x*a2-30*x2*a*b2+10*x2*a2*a+5*x*b2*b2-30*x*a2*b2+5*x*a2*a2+5*a*y2*y2+20*a*b*y2*y+30*a*b2*y2-10*a2*a*y2+20*a*b2*b*y-20*a2*a*b*y+5*a*b2*b2-10*a2*a*b2+a2*a2*a;
						if (c>0){
							if (c + d>0)
								Dnr = d;
							else if (d == -c)
								Dnr = d;
							else if (d<-c)
								Dnr = -d - 2 * c;
						}
						else if (c == 0)
							Dnr = _abs(d);
						else if (c < 0){
							if (c + d>0)
								Dnr = d + 2 * c;
							else if (d == -c)
								Dnr = -d;
							else if (d < -c)
								Dnr = -d;
						}
						Dnr+=a0;

						//Dni = _abs(y2*y2*y+5*y2*y2*b-10*y2*y*x2-20*y2*y*a*x+10*y2*y*b2-10*y2*y*a2-30*y2*b*x2-60*y2*a*b*x+10*y2*b2*b-30*y2*a2*b+5*y*x2*x2+20*y*a*x2*x-30*y*b2*x2+30*y*a2*x2-60*y*a*b2*x+20*y*a2*a*x+5*y*b2*b2-30*y*a2*b2+5*y*a2*a2+5*b*x2*x2+20*b*a*x2*x-10*b2*b*x2+30*b*a2*x2-20*b2*b*a*x+20*b*a2*a*x+b2*b2*b-10*b2*b*a2+5*b*a2*a2) - _abs(y2*y2*y-10*y2*y*x2+5*y*x2*x2) + b0;
						c = y2*y2*y-10*y2*y*x2+5*y*x2*x2;
						d = 5*y2*y2*b-20*y2*y*a*x+10*y2*y*b2-10*y2*y*a2-30*y2*b*x2-60*y2*a*b*x+10*y2*b2*b-30*y2*a2*b+20*y*a*x2*x-30*y*b2*x2+30*y*a2*x2-60*y*a*b2*x+20*y*a2*a*x+5*y*b2*b2-30*y*a2*b2+5*y*a2*a2+5*b*x2*x2+20*b*a*x2*x-10*b2*b*x2+30*b*a2*x2-20*b2*b*a*x+20*b*a2*a*x+b2*b2*b-10*b2*b*a2+5*b*a2*a2;
						if (c>0){
							if (c + d>0)
								Dni = d;
							else if (d == -c)
								Dni = d;
							else if (d<-c)
								Dni = -d - 2 * c;
						}
						else if (c == 0)
							Dni = _abs(d);
						else if (c < 0){
							if (c + d>0)
								Dni = d + 2 * c;
							else if (d == -c)
								Dni = -d;
							else if (d < -c)
								Dni = -d;
						}
						Dni+=b0;

						Di = Dni;
						Dr = Dnr;
					}
				}
			}
			//Celtic
			else if (m_nFractalType == 3 && m_nPower == 2){
				if (antal<nMaxIter && test1 <= m_nBailout2){
					for (; antal<nMaxIter && test1 <= m_nBailout2; antal++){
						yr = dxr[antal] + Dr;
						yi = dxi[antal] + Di;
						test2 = test1;
						test1 = g_real*yr*yr + g_imag*yi*yi;
						if (test1<m_db_z[antal]){
							
								test1 = m_nBailout2 * 2;
							bGlitch = TRUE;
						}
						if(test1 > m_nBailout2)
							break;
						long double &r = dxr[antal];
						long double &i = dxi[antal];
						long double &a = Dr;
						long double &b = Di;
						long double &a0 = D0r;
						long double &b0 = D0i;

						long double c = r*r - i*i;
						long double d = (2 * r + a)*a - (2 * i + b)*b;
						if (c>0){
							if (c + d>0)
								Dnr = d;
							else if (d == -c)
								Dnr = d;
							else if (d<-c)
								Dnr = -d - 2 * c;
						}
						else if (c == 0)
							Dnr = _abs(d);
						else if (c < 0){
							if (c + d>0)
								Dnr = d + 2 * c;
							else if (d == -c)
								Dnr = -d;
							else if (d < -c)
								Dnr = -d;
						}
						Dnr = Dnr + a0;

						Dni = 2 * r*b + 2 * a*(i + b) + b0;


						Di = Dni;
						Dr = Dnr;
					}
				}
			}
			//Cubic Celtic
			else if (m_nFractalType == 3 && m_nPower == 3){
				if (antal<nMaxIter && test1 <= m_nBailout2){
					for (; antal<nMaxIter && test1 <= m_nBailout2; antal++){
						yr = dxr[antal] + Dr;
						yi = dxi[antal] + Di;
						test2 = test1;
						test1 = g_real*yr*yr + g_imag*yi*yi;
						if (test1<m_db_z[antal]){
							
								test1 = m_nBailout2 * 2;
							bGlitch = TRUE;
						}
						if(test1 > m_nBailout2)
							break;
						long double &r = dxr[antal];
						long double &i = dxi[antal];
						long double &a = Dr;
						long double &b = Di;
						long double &a0 = D0r;
						long double &b0 = D0i;
						long double r2 = r*r;
						long double i2 = i*i;
						long double a2 = a*a;
						long double b2 = b*b;

						long double c = r*(r2 - 3 * i2);
						long double d = a*(3 * r2 + a2) + 3 * r*(a2 - 2 * i*b - b2) - 3 * a*(i2 + 2 * i*b + b2);
						if (c>0){
							if (c + d>0)
								Dnr = d;
							else if (d == -c)
								Dnr = d;
							else if (d<-c)
								Dnr = -d - 2 * c;
						}
						else if (c == 0)
							Dnr = _abs(d);
						else if (c < 0){
							if (c + d>0)
								Dnr = d + 2 * c;
							else if (d == -c)
								Dnr = -d;
							else if (d < -c)
								Dnr = -d;
						}
						Dnr = Dnr + a0;

						Dni = 3 * i*(2 * r*a + a2 - b2) + 3 * b*(r2 + 2 * r*a + a2) - b*(b2 + 3 * i2) + b0;


						Di = Dni;
						Dr = Dnr;
					}
				}
			}
			// 4th Celtic Buffalo
			else if (m_nFractalType == 3 && m_nPower == 4){
				if (antal<nMaxIter && test1 <= m_nBailout2){
					for (; antal<nMaxIter && test1 <= m_nBailout2; antal++){
						yr = dxr[antal] + Dr;
						yi = dxi[antal] + Di;
						test2 = test1;
						test1 = g_real*yr*yr + g_imag*yi*yi;
						if (test1<m_db_z[antal]){
							
								test1 = m_nBailout2 * 2;
							bGlitch = TRUE;
						}
						if(test1 > m_nBailout2)
							break;
						long double &x = dxr[antal];
						long double &y = dxi[antal];
						long double &a = Dr;
						long double &b = Di;
						long double &a0 = D0r;
						long double &b0 = D0i;
						long double x2 = x*x;
						long double y2 = y*y;
						long double a2 = a*a;
						long double b2 = b*b;
						long double xd = x+a;
						long double xd2 = xd*xd;
						long double yd = y+b;
						long double yd2 = yd*yd;

						long double c = x2*x2+y2*y2-6*x2*y2;
						long double d = 4*x2*x*a+6*x2*a2+4*x*a2*a+a2*a2 + 4*y2*y*b+6*y2*b2+4*y*b2*b+b2*b2 -12*a*x*y2-6*a2*y2-12*b*x2*y-24*a*b*x*y-12*a2*b*y-6*b2*x2-12*a*b2*x-6*a2*b2;
						if (c>0){
							if (c + d>0)
								Dnr = d;
							else if (d == -c)
								Dnr = d;
							else if (d<-c)
								Dnr = -d - 2 * c;
						}
						else if (c == 0)
							Dnr = _abs(d);
						else if (c < 0){
							if (c + d>0)
								Dnr = d + 2 * c;
							else if (d == -c)
								Dnr = -d;
							else if (d < -c)
								Dnr = -d;
						}
						Dnr+=a0;
						

						Dni = 12*x2*y*a+12*x*y*a2-12*x*y2*b-12*x*y*b2+4*x2*x*b+12*x2*b*a+12*x*b*a2-4*x*b2*b+4*a2*a*y-4*a*y2*y-12*a*y2*b-12*a*y*b2+4*a2*a*b-4*a*b2*b + b0;

						Di = Dni;
						Dr = Dnr;
					}
				}
			}
			// 5th Celtic
			else if (m_nFractalType == 3 && m_nPower == 5){
				if (antal<nMaxIter && test1 <= m_nBailout2){
					for (; antal<nMaxIter && test1 <= m_nBailout2; antal++){
						yr = dxr[antal] + Dr;
						yi = dxi[antal] + Di;
						test2 = test1;
						test1 = g_real*yr*yr + g_imag*yi*yi;
						if (test1<m_db_z[antal]){
							
								test1 = m_nBailout2 * 2;
							bGlitch = TRUE;
						}
						if(test1 > m_nBailout2)
							break;
						long double &x = dxr[antal];
						long double &y = dxi[antal];
						long double &a = Dr;
						long double &b = Di;
						long double &a0 = D0r;
						long double &b0 = D0i;
						long double x2 = x*x;
						long double y2 = y*y;
						long double a2 = a*a;
						long double b2 = b*b;
						long double xd = x+a;
						long double xd2 = xd*xd;
						long double yd = y+b;
						long double yd2 = yd*yd;

						//Dnr = _abs((x+a) * ((x+a)*(x+a)*(x+a)*(x+a) - 10 * (x+a)*(x+a)*(y+b)*(y+b) + 5 * (y+b)*(y+b)*(y+b)*(y+b))) - _abs(x2*x2*x - 10*x2*x*y2 + 5*x*y2*y2) +a0;
						//Dnr = _abs(5*x*y2*y2+20*x*b*y2*y-10*x2*x*y2-30*x2*a*y2+30*x*b2*y2-30*x*a2*y2-20*x2*x*b*y-60*x2*a*b*y+20*x*b2*b*y-60*x*a2*b*y+x2*x2*x+5*x2*x2*a-10*x2*x*b2+10*x2*x*a2-30*x2*a*b2+10*x2*a2*a+5*x*b2*b2-30*x*a2*b2+5*x*a2*a2+5*a*y2*y2+20*a*b*y2*y+30*a*b2*y2-10*a2*a*y2+20*a*b2*b*y-20*a2*a*b*y+5*a*b2*b2-10*a2*a*b2+a2*a2*a) - _abs(x2*x2*x - 10*x2*x*y2 + 5*x*y2*y2) + a0;
						long double c = x2*x2*x - 10*x2*x*y2 + 5*x*y2*y2;
						long double d = 20*x*b*y2*y-30*x2*a*y2+30*x*b2*y2-30*x*a2*y2-20*x2*x*b*y-60*x2*a*b*y+20*x*b2*b*y-60*x*a2*b*y+5*x2*x2*a-10*x2*x*b2+10*x2*x*a2-30*x2*a*b2+10*x2*a2*a+5*x*b2*b2-30*x*a2*b2+5*x*a2*a2+5*a*y2*y2+20*a*b*y2*y+30*a*b2*y2-10*a2*a*y2+20*a*b2*b*y-20*a2*a*b*y+5*a*b2*b2-10*a2*a*b2+a2*a2*a;
						if (c>0){
							if (c + d>0)
								Dnr = d;
							else if (d == -c)
								Dnr = d;
							else if (d<-c)
								Dnr = -d - 2 * c;
						}
						else if (c == 0)
							Dnr = _abs(d);
						else if (c < 0){
							if (c + d>0)
								Dnr = d + 2 * c;
							else if (d == -c)
								Dnr = -d;
							else if (d < -c)
								Dnr = -d;
						}
						Dnr+=a0;
						
						Dni = 20*y*x2*x*a+30*y*x2*a2+20*y*x*a2*a+5*y*a2*a2-30*y2*x2*b-30*y*x2*b2-20*y2*y*x*a-60*y2*x*a*b-60*y*x*a*b2-10*y2*y*a2-30*y2*a2*b-30*y*a2*b2+5*y2*y2*b+10*y2*y*b2+10*y2*b2*b+5*y*b2*b2+5*b*x2*x2+20*b*x2*x*a+30*b*x2*a2+20*b*x*a2*a+5*b*a2*a2-10*b2*b*x2-20*b2*b*x*a-10*b2*b*a2+b2*b2*b +b0;
						Di = Dni;
						Dr = Dnr;
					}
				}
			}
			// Mandelbar
			else if (m_nFractalType == 4 && m_nPower == 2){
				if (antal<nMaxIter && test1 <= m_nBailout2){
					for (; antal<nMaxIter && test1 <= m_nBailout2; antal++){
						yr = dxr[antal] + Dr;
						yi = dxi[antal] + Di;
						test2 = test1;
						test1 = g_real*yr*yr + g_imag*yi*yi;
						if (test1<m_db_z[antal]){
							
								test1 = m_nBailout2 * 2;
							bGlitch = TRUE;
						}
						if(test1 > m_nBailout2)
							break;
						long double &r = dxr[antal];
						long double &i = dxi[antal];
						long double &a = Dr;
						long double &b = Di;
						long double a2 = a*a;
						long double b2 = b*b;
						long double &a0 = D0r;
						long double &b0 = D0i;

						Dnr = 2 * r*a + a2 - b2 - 2 * b*i + a0;
						Dni = (r*b + a*i + a*b)*-2 + b0;


						Di = Dni;
						Dr = Dnr;
					}
				}
			}
			// Cubic Mandelbar
			else if (m_nFractalType == 4 && m_nPower == 3){
				if (antal<nMaxIter && test1 <= m_nBailout2){
					for (; antal<nMaxIter && test1 <= m_nBailout2; antal++){
						yr = dxr[antal] + Dr;
						yi = dxi[antal] + Di;
						test2 = test1;
						test1 = g_real*yr*yr + g_imag*yi*yi;
						if (test1<m_db_z[antal]){
							
								test1 = m_nBailout2 * 2;
							bGlitch = TRUE;
						}
						if(test1 > m_nBailout2)
							break;
						long double &r = dxr[antal];
						long double &i = dxi[antal];
						long double &a = Dr;
						long double &b = Di;
						long double &a0 = D0r;
						long double &b0 = D0i;
						long double r2 = r*r;
						long double i2 = i*i;
						long double a2 = a*a;
						long double b2 = b*b;

						Dnr = a0 - a*(3 * r2 + a2) + 3 * r*(b2 + 2 * i*b - a2) + 3 * a*(i2 + 2 * i*b + b2);
						Dni = 6 * r*i*a + 3 * i*a2 - 3 * i2*b - 3 * i*b2 + 3 * r2*b + 6 * r*a*b + 3 * a2*b - b2*b + b0;


						Di = Dni;
						Dr = Dnr;
					}
				}
			}
			// 4th Power Mandelbar
			else if (m_nFractalType == 4 && m_nPower == 4){
				if (antal<nMaxIter && test1 <= m_nBailout2){
					for (; antal<nMaxIter && test1 <= m_nBailout2; antal++){
						yr = dxr[antal] + Dr;
						yi = dxi[antal] + Di;
						test2 = test1;
						test1 = g_real*yr*yr + g_imag*yi*yi;
						if (test1<m_db_z[antal]){
							
								test1 = m_nBailout2 * 2;
							bGlitch = TRUE;
						}
						if(test1 > m_nBailout2)
							break;
						long double &x = dxr[antal];
						long double &y = dxi[antal];
						long double &a = Dr;
						long double &b = Di;
						long double &a0 = D0r;
						long double &b0 = D0i;
						long double x2 = x*x;
						long double y2 = y*y;
						long double a2 = a*a;
						long double b2 = b*b;
						long double xd = x+a;
						long double xd2 = xd*xd;
						long double yd = y+b;
						long double yd2 = yd*yd;

						Dnr = 4*x2*x*a+6*x2*a2+4*x*a2*a+a2*a2+4*y2*y*b+6*y2*b2+4*y*b2*b+b2*b2-12*x2*y*b-6*x2*b2-12*x*a*y2-24*x*a*y*b-12*x*a*b2-6*a2*y2-12*a2*y*b-6*a2*b2 + a0;
						Dni = -12*x2*y*a-12*x*y*a2+12*x*y2*b+12*x*y*b2-4*x2*x*b-12*x2*b*a-12*x*b*a2+4*x*b2*b-4*a2*a*y+4*a*y2*y+12*a*y2*b+12*a*y*b2-4*a2*a*b+4*a*b2*b + b0;


						Di = Dni;
						Dr = Dnr;
					}
				}
			}
			// 5th Power Mandelbar
			else if (m_nFractalType == 4 && m_nPower == 5){
				if (antal<nMaxIter && test1 <= m_nBailout2){
					for (; antal<nMaxIter && test1 <= m_nBailout2; antal++){
						yr = dxr[antal] + Dr;
						yi = dxi[antal] + Di;
						test2 = test1;
						test1 = g_real*yr*yr + g_imag*yi*yi;
						if (test1<m_db_z[antal]){
							
								test1 = m_nBailout2 * 2;
							bGlitch = TRUE;
						}
						if(test1 > m_nBailout2)
							break;
						long double &x = dxr[antal];
						long double &y = dxi[antal];
						long double &a = Dr;
						long double &b = Di;
						long double &a0 = D0r;
						long double &b0 = D0i;
						long double x2 = x*x;
						long double y2 = y*y;
						long double a2 = a*a;
						long double b2 = b*b;
						long double xd = x+a;
						long double xd2 = xd*xd;
						long double yd = y+b;
						long double yd2 = yd*yd;

						Dnr = 5*x2*x2*a+10*x2*x*a2+10*x2*a2*a+5*x*a2*a2-20*x2*x*y*b-10*x2*x*b2-30*x2*a*y2-60*x2*a*y*b-30*x2*a*b2-30*x*a2*y2-60*x*a2*y*b-30*x*a2*b2+20*x*y2*y*b+30*x*y2*b2+20*x*y*b2*b+5*x*b2*b2+a2*a2*a-10*a2*a*y2-20*a2*a*y*b-10*a2*a*b2+5*a*y2*y2+20*a*y2*y*b+30*a*y2*b2+20*a*y*b2*b+5*a*b2*b2 + a0;
						Dni = -20*y*x2*x*a-30*y*x2*a2-20*y*x*a2*a-5*y*a2*a2+30*y2*x2*b+30*y*x2*b2+20*y2*y*x*a+60*y2*x*a*b+60*y*x*a*b2+10*y2*y*a2+30*y2*a2*b+30*y*a2*b2-5*y2*y2*b-10*y2*y*b2-10*y2*b2*b-5*y*b2*b2-5*b*x2*x2-20*b*x2*x*a-30*b*x2*a2-20*b*x*a2*a-5*b*a2*a2+10*b2*b*x2+20*b2*b*x*a+10*b2*b*a2-b2*b2*b + b0;

						Di = Dni;
						Dr = Dnr;
					}
				}
			}
			//Mandelbar Celtic
			else if (m_nFractalType == 5){
				if (antal<nMaxIter && test1 <= m_nBailout2){
					for (; antal<nMaxIter && test1 <= m_nBailout2; antal++){
						yr = dxr[antal] + Dr;
						yi = dxi[antal] + Di;
						test2 = test1;
						test1 = g_real*yr*yr + g_imag*yi*yi;
						if (test1<m_db_z[antal]){
							
								test1 = m_nBailout2 * 2;
							bGlitch = TRUE;
						}
						if(test1 > m_nBailout2)
							break;
						long double &r = dxr[antal];
						long double &i = dxi[antal];
						long double &a = Dr;
						long double &b = Di;
						long double a2 = a*a;
						long double b2 = b*b;
						long double &a0 = D0r;
						long double &b0 = D0i;

						long double c = r*r - i*i;
						long double d = 2 * r*a + a2 - 2 * i*b - b2;
						if (c>0){
							if (c + d>0)
								Dnr = d;
							else if (d == -c)
								Dnr = d;
							else if (d<-c)
								Dnr = -d - 2 * c;
						}
						else if (c == 0)
							Dnr = _abs(d);
						else if (c < 0){
							if (c + d>0)
								Dnr = d + 2 * c;
							else if (d == -c)
								Dnr = -d;
							else if (d < -c)
								Dnr = -d;
						}
						Dnr = Dnr + a0;

						Dni = (r*b + a*i + a*b)*-2 + b0;


						Di = Dni;
						Dr = Dnr;
					}
				}
			}
			// Perpendicular Mandelbrot
			else if (m_nFractalType == 6){
				if (antal<nMaxIter && test1 <= m_nBailout2){
					for (; antal<nMaxIter && test1 <= m_nBailout2; antal++){
						yr = dxr[antal] + Dr;
						yi = dxi[antal] + Di;
						test2 = test1;
						test1 = g_real*yr*yr + g_imag*yi*yi;
						if (test1<m_db_z[antal]){
							
								test1 = m_nBailout2 * 2;
							bGlitch = TRUE;
						}
						if(test1 > m_nBailout2)
							break;
						long double &r = dxr[antal];
						long double &i = dxi[antal];
						long double &a = Dr;
						long double &b = Di;
						long double a2 = a*a;
						long double b2 = b*b;
						long double &a0 = D0r;
						long double &b0 = D0i;

						Dnr = 2 * r*a + a2 - b2 - 2 * b*i + a0;

						long double c = r;
						long double d = a;
						if (c>0){
							if (c + d>0)
								Dni = d;
							else if (d == -c)
								Dni = d;
							else if (d<-c)
								Dni = -d - 2 * c;
						}
						else if (c == 0)
							Dni = _abs(d);
						else if (c < 0){
							if (c + d>0)
								Dni = d + 2 * c;
							else if (d == -c)
								Dni = -d;
							else if (d < -c)
								Dni = -d;
						}
						Dni = _abs((r + a))*b*-2 + Dni*i*-2 + b0;


						Di = Dni;
						Dr = Dnr;
					}
				}
			}
			//Perpendicular Burning Ship
			else if (m_nFractalType == 7){
				if (antal<nMaxIter && test1 <= m_nBailout2){
					for (; antal<nMaxIter && test1 <= m_nBailout2; antal++){
						yr = dxr[antal] + Dr;
						yi = dxi[antal] + Di;
						test2 = test1;
						test1 = g_real*yr*yr + g_imag*yi*yi;
						if (test1<m_db_z[antal]){
							
								test1 = m_nBailout2 * 2;
							bGlitch = TRUE;
						}
						if(test1 > m_nBailout2)
							break;
						long double &r = dxr[antal];
						long double &i = dxi[antal];
						long double &a = Dr;
						long double &b = Di;
						long double a2 = a*a;
						long double b2 = b*b;
						long double &a0 = D0r;
						long double &b0 = D0i;

						Dnr = 2 * r*a + a2 - b2 - 2 * b*i + a0;

						long double c = i;
						long double d = b;
						if (c>0){
							if (c + d>0)
								Dni = d;
							else if (d == -c)
								Dni = d;
							else if (d<-c)
								Dni = -d - 2 * c;
						}
						else if (c == 0)
							Dni = _abs(d);
						else if (c < 0){
							if (c + d>0)
								Dni = d + 2 * c;
							else if (d == -c)
								Dni = -d;
							else if (d < -c)
								Dni = -d;
						}
						Dni = Dni*r*-2 + a*_abs((i + b))*-2 + b0;


						Di = Dni;
						Dr = Dnr;
					}
				}
			}
			//Perpendicular Celtic
			else if (m_nFractalType == 8){
				if (antal<nMaxIter && test1 <= m_nBailout2){
					for (; antal<nMaxIter && test1 <= m_nBailout2; antal++){
						yr = dxr[antal] + Dr;
						yi = dxi[antal] + Di;
						test2 = test1;
						test1 = g_real*yr*yr + g_imag*yi*yi;
						if (test1<m_db_z[antal]){
							
								test1 = m_nBailout2 * 2;
							bGlitch = TRUE;
						}
						if(test1 > m_nBailout2)
							break;
						long double &r = dxr[antal];
						long double &i = dxi[antal];
						long double &a = Dr;
						long double &b = Di;
						long double a2 = a*a;
						long double b2 = b*b;
						long double &a0 = D0r;
						long double &b0 = D0i;

						long double c = r*r - i*i;
						long double d = 2 * r*a + a2 - 2 * i*b - b2;
						if (c>0){
							if (c + d>0)
								Dnr = d;
							else if (d == -c)
								Dnr = d;
							else if (d<-c)
								Dnr = -d - 2 * c;
						}
						else if (c == 0)
							Dnr = _abs(d);
						else if (c < 0){
							if (c + d>0)
								Dnr = d + 2 * c;
							else if (d == -c)
								Dnr = -d;
							else if (d < -c)
								Dnr = -d;
						}
						Dnr = Dnr + a0;

						c = r;
						d = a;
						if (c>0){
							if (c + d>0)
								Dni = d;
							else if (d == -c)
								Dni = d;
							else if (d<-c)
								Dni = -d - 2 * c;
						}
						else if (c == 0)
							Dni = _abs(d);
						else if (c < 0){
							if (c + d>0)
								Dni = d + 2 * c;
							else if (d == -c)
								Dni = -d;
							else if (d < -c)
								Dni = -d;
						}
						Dni = _abs((r + a))*b*-2 + Dni*i*-2 + b0;

						Di = Dni;
						Dr = Dnr;
					}
				}
			}
			//Perpendicular Buffalo
			else if (m_nFractalType == 9){
				if (antal<nMaxIter && test1 <= m_nBailout2){
					for (; antal<nMaxIter && test1 <= m_nBailout2; antal++){
						yr = dxr[antal] + Dr;
						yi = dxi[antal] + Di;
						test2 = test1;
						test1 = g_real*yr*yr + g_imag*yi*yi;
						if (test1<m_db_z[antal]){
							
								test1 = m_nBailout2 * 2;
							bGlitch = TRUE;
						}
						if(test1 > m_nBailout2)
							break;
						long double &r = dxr[antal];
						long double &i = dxi[antal];
						long double &a = Dr;
						long double &b = Di;
						long double a2 = a*a;
						long double b2 = b*b;
						long double &a0 = D0r;
						long double &b0 = D0i;

						long double c = r*r - i*i;
						long double d = 2 * r*a + a2 - 2 * i*b - b2;
						if (c>0){
							if (c + d>0)
								Dnr = d;
							else if (d == -c)
								Dnr = d;
							else if (d<-c)
								Dnr = -d - 2 * c;
						}
						else if (c == 0)
							Dnr = _abs(d);
						else if (c < 0){
							if (c + d>0)
								Dnr = d + 2 * c;
							else if (d == -c)
								Dnr = -d;
							else if (d < -c)
								Dnr = -d;
						}
						Dnr = Dnr + a0;

						c = i;
						d = b;
						if (c>0){
							if (c + d>0)
								Dni = d;
							else if (d == -c)
								Dni = d;
							else if (d<-c)
								Dni = -d - 2 * c;
						}
						else if (c == 0)
							Dni = _abs(d);
						else if (c < 0){
							if (c + d>0)
								Dni = d + 2 * c;
							else if (d == -c)
								Dni = -d;
							else if (d < -c)
								Dni = -d;
						}
						Dni = Dni*r*-2 + a*_abs((i + b))*-2 + b0;

						Di = Dni;
						Dr = Dnr;
					}
				}
			}
			// Cubic Quasi Burning Ship
			else if (m_nFractalType == 10){
				if (antal<nMaxIter && test1 <= m_nBailout2){
					for (; antal<nMaxIter && test1 <= m_nBailout2; antal++){
						yr = dxr[antal] + Dr;
						yi = dxi[antal] + Di;
						test2 = test1;
						test1 = g_real*yr*yr + g_imag*yi*yi;
						if (test1<m_db_z[antal]){
							
								test1 = m_nBailout2 * 2;
							bGlitch = TRUE;
						}
						if(test1 > m_nBailout2)
							break;
						long double &r = dxr[antal];
						long double &i = dxi[antal];
						long double &a = Dr;
						long double &b = Di;
						long double &a0 = D0r;
						long double &b0 = D0i;
						long double r2 = r*r;
						long double i2 = i*i;
						long double a2 = a*a;
						long double b2 = b*b;
						long double ar = a*r;
						long double ib = i*b;
						long double ab;

						if (r>0){
							if (r + a>0)
								Dnr = a;
							else if (a == -r)
								Dnr = a;
							else if (a<-r)
								Dnr = -a - 2 * r;
						}
						else if (r == 0)
							Dnr = _abs(a);
						else if (r < 0){
							if (r + a>0)
								Dnr = a + 2 * r;
							else if (a == -r)
								Dnr = -a;
							else if (a < -r)
								Dnr = -a;
						}
						ab = r + a;
						Dnr = (r2 - 3 * i2) * Dnr + (2 * ar + a2 - 6 * ib - 3 * b2)*_abs(ab) + a0;

						long double c = i*(3 * r2 - i2);
						long double d = 3 * i*(2 * r*a + a2 - b2) + 3 * b*(r2 + 2 * r*a + a2) - b*(3 * i2 + b2);
						if (c>0){
							if (c + d>0)
								Dni = d;
							else if (d == -c)
								Dni = d;
							else if (d<-c)
								Dni = -d - 2 * c;
						}
						else if (c == 0)
							Dni = _abs(d);
						else if (c < 0){
							if (c + d>0)
								Dni = d + 2 * c;
							else if (d == -c)
								Dni = -d;
							else if (d < -c)
								Dni = -d;
						}
						Dni = b0 - Dni;

						Di = Dni;
						Dr = Dnr;
					}
				}
			}
			// Cubic Partial BS Real
			else if (m_nFractalType == 11){
				if (antal<nMaxIter && test1 <= m_nBailout2){
					for (; antal<nMaxIter && test1 <= m_nBailout2; antal++){
						yr = dxr[antal] + Dr;
						yi = dxi[antal] + Di;
						test2 = test1;
						test1 = g_real*yr*yr + g_imag*yi*yi;
						if (test1<m_db_z[antal]){
							
								test1 = m_nBailout2 * 2;
							bGlitch = TRUE;
						}
						if(test1 > m_nBailout2)
							break;
						long double &r = dxr[antal];
						long double &i = dxi[antal];
						long double &a = Dr;
						long double &b = Di;
						long double &a0 = D0r;
						long double &b0 = D0i;
						long double r2 = r*r;
						long double i2 = i*i;
						long double a2 = a*a;
						long double b2 = b*b;
						long double ar = a*r;
						long double ib = i*b;
						long double ab;

						if (r>0){
							if (r + a>0)
								Dnr = a;
							else if (a == -r)
								Dnr = a;
							else if (a<-r)
								Dnr = -a - 2 * r;
						}
						else if (r == 0)
							Dnr = _abs(a);
						else if (r < 0){
							if (r + a>0)
								Dnr = a + 2 * r;
							else if (a == -r)
								Dnr = -a;
							else if (a < -r)
								Dnr = -a;
						}
						ab = r + a;
						Dnr = (r2 - 3 * i2) * Dnr + (2 * ar + a2 - 6 * ib - 3 * b2)*_abs(ab) + a0;
						Dni = 6 * r*(i*a + a*b) + 3 * i*(a2 - b2) + 3 * b*(r2 - i2) + b*(3 * a2 - b2) + b0;

						Di = Dni;
						Dr = Dnr;
					}
				}
			}
			// Cubic Partial BS Imag
			else if (m_nFractalType == 12){
				if (antal<nMaxIter && test1 <= m_nBailout2){
					for (; antal<nMaxIter && test1 <= m_nBailout2; antal++){
						yr = dxr[antal] + Dr;
						yi = dxi[antal] + Di;
						test2 = test1;
						test1 = g_real*yr*yr + g_imag*yi*yi;
						if (test1<m_db_z[antal]){
							
								test1 = m_nBailout2 * 2;
							bGlitch = TRUE;
						}
						if(test1 > m_nBailout2)
							break;
						long double &r = dxr[antal];
						long double &i = dxi[antal];
						long double &a = Dr;
						long double &b = Di;
						long double &a0 = D0r;
						long double &b0 = D0i;
						long double r2 = r*r;
						long double i2 = i*i;
						long double a2 = a*a;
						long double b2 = b*b;
						long double ar = a*r;
						long double ib = i*b;
						long double ab;

						Dnr = 3 * r2*a + 3 * r*a2 - 6 * r*i*b - 3 * r*b2 + a*a2 - 3 * i2*a - 6 * i*a*b - 3 * a*b2 + a0;

						if (i>0){
							if (i + b>0)
								Dni = b;
							else if (b == -i)
								Dni = b;
							else if (b<-i)
								Dni = -b - 2 * i;
						}
						else if (i == 0)
							Dni = _abs(b);
						else if (i < 0){
							if (i + b>0)
								Dni = b + 2 * i;
							else if (b == -i)
								Dni = -b;
							else if (b < -i)
								Dni = -b;
						}
						ab = i + b;
						Dni = (3 * r2 - i2) * Dni + (6 * ar + 3 * a2 - 2 * ib - b2) * _abs(ab) + b0;

						Di = Dni;
						Dr = Dnr;
					}
				}
			}
			// Cubic Flying Squirrel
			else if (m_nFractalType == 13){
				if (antal<nMaxIter && test1 <= m_nBailout2){
					for (; antal<nMaxIter && test1 <= m_nBailout2; antal++){
						yr = dxr[antal] + Dr;
						yi = dxi[antal] + Di;
						test2 = test1;
						test1 = g_real*yr*yr + g_imag*yi*yi;
						if (test1<m_db_z[antal]){
							
								test1 = m_nBailout2 * 2;
							bGlitch = TRUE;
						}
						if(test1 > m_nBailout2)
							break;
						long double &r = dxr[antal];
						long double &i = dxi[antal];
						long double &a = Dr;
						long double &b = Di;
						long double &a0 = D0r;
						long double &b0 = D0i;
						long double r2 = r*r;
						long double i2 = i*i;
						long double a2 = a*a;
						long double b2 = b*b;

						Dnr = 3 * r2*a + 3 * r*a2 - 6 * r*i*b - 3 * r*b2 + a*a2 - 3 * i2*a - 6 * i*a*b - 3 * a*b2 + a0;

						long double c = i*(3 * r2 - i2);
						long double d = 3 * i*(2 * r*a + a2 - b2) + 3 * b*(r2 + 2 * r*a + a2) - b*(3 * i2 + b2);
						if (c>0){
							if (c + d>0)
								Dni = d;
							else if (d == -c)
								Dni = d;
							else if (d<-c)
								Dni = -d - 2 * c;
						}
						else if (c == 0)
							Dni = _abs(d);
						else if (c < 0){
							if (c + d>0)
								Dni = d + 2 * c;
							else if (d == -c)
								Dni = -d;
							else if (d < -c)
								Dni = -d;
						}
						Dni = Dni + b0;

						Di = Dni;
						Dr = Dnr;
					}
				}
			}
			// Cubic Quasi Perpendicular
			else if (m_nFractalType == 14){
				if (antal<nMaxIter && test1 <= m_nBailout2){
					for (; antal<nMaxIter && test1 <= m_nBailout2; antal++){
						yr = dxr[antal] + Dr;
						yi = dxi[antal] + Di;
						test2 = test1;
						test1 = g_real*yr*yr + g_imag*yi*yi;
						if (test1<m_db_z[antal]){
							
								test1 = m_nBailout2 * 2;
							bGlitch = TRUE;
						}
						if(test1 > m_nBailout2)
							break;
						long double &r = dxr[antal];
						long double &i = dxi[antal];
						long double &a = Dr;
						long double &b = Di;
						long double &a0 = D0r;
						long double &b0 = D0i;
						long double r2 = r*r;
						long double i2 = i*i;
						long double a2 = a*a;
						long double b2 = b*b;
						long double ar = a*r;
						long double ib = i*b;
						long double ab;

						if (r>0){
							if (r + a>0)
								Dnr = a;
							else if (a == -r)
								Dnr = a;
							else if (a<-r)
								Dnr = -a - 2 * r;
						}
						else if (r == 0)
							Dnr = _abs(a);
						else if (r < 0){
							if (r + a>0)
								Dnr = a + 2 * r;
							else if (a == -r)
								Dnr = -a;
							else if (a < -r)
								Dnr = -a;
						}
						ab = r + a;
						Dnr = (r2 - 3 * i2) * Dnr + (2 * ar + a2 - 6 * ib - 3 * b2)*_abs(ab) + a0;

						long double c = 3 * r2 - i2;
						long double d = 6 * r*a + 3 * a2 - 2 * i*b - b2;
						if (c>0){
							if (c + d>0)
								Dni = d;
							else if (d == -c)
								Dni = d;
							else if (d<-c)
								Dni = -d - 2 * c;
						}
						else if (c == 0)
							Dni = _abs(d);
						else if (c < 0){
							if (c + d>0)
								Dni = d + 2 * c;
							else if (d == -c)
								Dni = -d;
							else if (d < -c)
								Dni = -d;
						}
						ab = 3 * r2 + 6 * r*a + 3 * a2 - i2 - 2 * i*b - b2;
						Dni = b0 - Dni*i - _abs(ab)*b;

						Di = Dni;
						Dr = Dnr;
					}
				}
			}
			// 4th Burning Ship Partial Imag
			else if (m_nFractalType == 15){
				if (antal<nMaxIter && test1 <= m_nBailout2){
					for (; antal<nMaxIter && test1 <= m_nBailout2; antal++){
						yr = dxr[antal] + Dr;
						yi = dxi[antal] + Di;
						test2 = test1;
						test1 = g_real*yr*yr + g_imag*yi*yi;
						if (test1<m_db_z[antal]){
							
								test1 = m_nBailout2 * 2;
							bGlitch = TRUE;
						}
						if(test1 > m_nBailout2)
							break;
						long double &x = dxr[antal];
						long double &y = dxi[antal];
						long double &a = Dr;
						long double &b = Di;
						long double &a0 = D0r;
						long double &b0 = D0i;
						long double x2 = x*x;
						long double y2 = y*y;
						long double a2 = a*a;
						long double b2 = b*b;
						long double xd = x+a;
						long double xd2 = xd*xd;
						long double yd = y+b;
						long double yd2 = yd*yd;

						//func2
						Dnr = 4*x2*x*a+6*x2*a2+4*x*a2*a+a2*a2+4*y2*y*b+6*y2*b2+4*y*b2*b+b2*b2-12*x2*y*b-6*x2*b2-12*x*a*y2-24*x*a*y*b-12*x*a*b2-6*a2*y2-12*a2*y*b-6*a2*b2 + a0;

						//func1
						long double c = y;
						long double d = b;
						if (c>0){
							if (c + d>0)
								Dni = d;
							else if (d == -c)
								Dni = d;
							else if (d<-c)
								Dni = -d - 2 * c;
						}
						else if (c == 0)
							Dni = _abs(d);
						else if (c < 0){
							if (c + d>0)
								Dni = d + 2 * c;
							else if (d == -c)
								Dni = -d;
							else if (d < -c)
								Dni = -d;
						}
						Dni = _abs(y+b)*(12*x2*a+12*x*a2+4*a2*a - 4*a*y2-8*b*x*y-8*a*b*y-4*b2*x-4*a*b2) + Dni*(4*x2*x - 4*x*y2) + b0;
						
						Di = Dni;
						Dr = Dnr;
					}
				}
			}
			// 4th Burning Ship Partial Real
			else if (m_nFractalType == 16){
				if (antal<nMaxIter && test1 <= m_nBailout2){
					for (; antal<nMaxIter && test1 <= m_nBailout2; antal++){
						yr = dxr[antal] + Dr;
						yi = dxi[antal] + Di;
						test2 = test1;
						test1 = g_real*yr*yr + g_imag*yi*yi;
						if (test1<m_db_z[antal]){
							
								test1 = m_nBailout2 * 2;
							bGlitch = TRUE;
						}
						if(test1 > m_nBailout2)
							break;
						long double &x = dxr[antal];
						long double &y = dxi[antal];
						long double &a = Dr;
						long double &b = Di;
						long double &a0 = D0r;
						long double &b0 = D0i;
						long double x2 = x*x;
						long double y2 = y*y;
						long double a2 = a*a;
						long double b2 = b*b;
						long double xd = x+a;
						long double xd2 = xd*xd;
						long double yd = y+b;
						long double yd2 = yd*yd;

						//func2
						Dnr = 4*x2*x*a+6*x2*a2+4*x*a2*a+a2*a2+4*y2*y*b+6*y2*b2+4*y*b2*b+b2*b2-12*x2*y*b-6*x2*b2-12*x*a*y2-24*x*a*y*b-12*x*a*b2-6*a2*y2-12*a2*y*b-6*a2*b2 + a0;

						//func3
						long double c = x;
						long double d = a;
						if (c>0){
							if (c + d>0)
								Dni = d;
							else if (d == -c)
								Dni = d;
							else if (d<-c)
								Dni = -d - 2 * c;
						}
						else if (c == 0)
							Dni = _abs(d);
						else if (c < 0){
							if (c + d>0)
								Dni = d + 2 * c;
							else if (d == -c)
								Dni = -d;
							else if (d < -c)
								Dni = -d;
						}
						Dni = _abs(x+a)*(4*x2*b+8*x*a*y+8*x*a*b+4*a2*y+4*a2*b - 12*b*y2-12*b2*y-4*b2*b) + Dni*(4*x2*y - 4*y2*y) + b0;
						
						Di = Dni;
						Dr = Dnr;
					}
				}
			}
			// 4th Burning Ship Partial Real Mbar
			else if (m_nFractalType == 17){
				if (antal<nMaxIter && test1 <= m_nBailout2){
					for (; antal<nMaxIter && test1 <= m_nBailout2; antal++){
						yr = dxr[antal] + Dr;
						yi = dxi[antal] + Di;
						test2 = test1;
						test1 = g_real*yr*yr + g_imag*yi*yi;
						if (test1<m_db_z[antal]){
							
								test1 = m_nBailout2 * 2;
							bGlitch = TRUE;
						}
						if(test1 > m_nBailout2)
							break;
						long double &x = dxr[antal];
						long double &y = dxi[antal];
						long double &a = Dr;
						long double &b = Di;
						long double &a0 = D0r;
						long double &b0 = D0i;
						long double x2 = x*x;
						long double y2 = y*y;
						long double a2 = a*a;
						long double b2 = b*b;
						long double xd = x+a;
						long double xd2 = xd*xd;
						long double yd = y+b;
						long double yd2 = yd*yd;

						//func2
						Dnr = 4*x2*x*a+6*x2*a2+4*x*a2*a+a2*a2+4*y2*y*b+6*y2*b2+4*y*b2*b+b2*b2-12*x2*y*b-6*x2*b2-12*x*a*y2-24*x*a*y*b-12*x*a*b2-6*a2*y2-12*a2*y*b-6*a2*b2 + a0;

						//func4
						long double c = x;
						long double d = a;
						if (c>0){
							if (c + d>0)
								Dni = d;
							else if (d == -c)
								Dni = d;
							else if (d<-c)
								Dni = -d - 2 * c;
						}
						else if (c == 0)
							Dni = _abs(d);
						else if (c < 0){
							if (c + d>0)
								Dni = d + 2 * c;
							else if (d == -c)
								Dni = -d;
							else if (d < -c)
								Dni = -d;
						}
						Dni = _abs(x+a)*(12*y2*b+12*y*b2+4*b2*b - 8*a*x*y-4*a2*y-4*b*x2-8*a*b*x-4*a2*b) + Dni*(4*y2*y - 4*x2*y) + b0;

						Di = Dni;
						Dr = Dnr;
					}
				}
			}
			// 4th Celtic Burning Ship Partial Imag
			else if (m_nFractalType == 18){
				if (antal<nMaxIter && test1 <= m_nBailout2){
					for (; antal<nMaxIter && test1 <= m_nBailout2; antal++){
						yr = dxr[antal] + Dr;
						yi = dxi[antal] + Di;
						test2 = test1;
						test1 = g_real*yr*yr + g_imag*yi*yi;
						if (test1<m_db_z[antal]){
							
								test1 = m_nBailout2 * 2;
							bGlitch = TRUE;
						}
						if(test1 > m_nBailout2)
							break;
						long double &x = dxr[antal];
						long double &y = dxi[antal];
						long double &a = Dr;
						long double &b = Di;
						long double &a0 = D0r;
						long double &b0 = D0i;
						long double x2 = x*x;
						long double y2 = y*y;
						long double a2 = a*a;
						long double b2 = b*b;
						long double xd = x+a;
						long double xd2 = xd*xd;
						long double yd = y+b;
						long double yd2 = yd*yd;

						//func5
						long double c = x2*x2 + y2*y2 - 6*x2*y2;
						long double d = 4*x2*x*a+6*x2*a2+4*x*a2*a+a2*a2 + 4*y2*y*b+6*y2*b2+4*y*b2*b+b2*b2 - 12*a*x*y2-6*a2*y2-12*b*x2*y-24*a*b*x*y-12*a2*b*y-6*b2*x2-12*a*b2*x-6*a2*b2;
						if (c>0){
							if (c + d>0)
								Dnr = d;
							else if (d == -c)
								Dnr = d;
							else if (d<-c)
								Dnr = -d - 2 * c;
						}
						else if (c == 0)
							Dnr = _abs(d);
						else if (c < 0){
							if (c + d>0)
								Dnr = d + 2 * c;
							else if (d == -c)
								Dnr = -d;
							else if (d < -c)
								Dnr = -d;
						}
						Dnr = Dnr + a0;

						//func1
						c = y;
						d = b;
						if (c>0){
							if (c + d>0)
								Dni = d;
							else if (d == -c)
								Dni = d;
							else if (d<-c)
								Dni = -d - 2 * c;
						}
						else if (c == 0)
							Dni = _abs(d);
						else if (c < 0){
							if (c + d>0)
								Dni = d + 2 * c;
							else if (d == -c)
								Dni = -d;
							else if (d < -c)
								Dni = -d;
						}
						Dni = _abs(y+b)*(12*x2*a+12*x*a2+4*a2*a - 4*a*y2-8*b*x*y-8*a*b*y-4*b2*x-4*a*b2) + Dni*(4*x2*x - 4*x*y2) + b0;

						Di = Dni;
						Dr = Dnr;
					}
				}
			}
			// 4th Celtic Burning Ship Partial Real
			else if (m_nFractalType == 19){
				if (antal<nMaxIter && test1 <= m_nBailout2){
					for (; antal<nMaxIter && test1 <= m_nBailout2; antal++){
						yr = dxr[antal] + Dr;
						yi = dxi[antal] + Di;
						test2 = test1;
						test1 = g_real*yr*yr + g_imag*yi*yi;
						if (test1<m_db_z[antal]){
							
								test1 = m_nBailout2 * 2;
							bGlitch = TRUE;
						}
						if(test1 > m_nBailout2)
							break;
						long double &x = dxr[antal];
						long double &y = dxi[antal];
						long double &a = Dr;
						long double &b = Di;
						long double &a0 = D0r;
						long double &b0 = D0i;
						long double x2 = x*x;
						long double y2 = y*y;
						long double a2 = a*a;
						long double b2 = b*b;
						long double xd = x+a;
						long double xd2 = xd*xd;
						long double yd = y+b;
						long double yd2 = yd*yd;

						//func5
						long double c = x2*x2 + y2*y2 - 6*x2*y2;
						long double d = 4*x2*x*a+6*x2*a2+4*x*a2*a+a2*a2 + 4*y2*y*b+6*y2*b2+4*y*b2*b+b2*b2 - 12*a*x*y2-6*a2*y2-12*b*x2*y-24*a*b*x*y-12*a2*b*y-6*b2*x2-12*a*b2*x-6*a2*b2;
						if (c>0){
							if (c + d>0)
								Dnr = d;
							else if (d == -c)
								Dnr = d;
							else if (d<-c)
								Dnr = -d - 2 * c;
						}
						else if (c == 0)
							Dnr = _abs(d);
						else if (c < 0){
							if (c + d>0)
								Dnr = d + 2 * c;
							else if (d == -c)
								Dnr = -d;
							else if (d < -c)
								Dnr = -d;
						}
						Dnr = Dnr + a0;

						//func3
						c = x;
						d = a;
						if (c>0){
							if (c + d>0)
								Dni = d;
							else if (d == -c)
								Dni = d;
							else if (d<-c)
								Dni = -d - 2 * c;
						}
						else if (c == 0)
							Dni = _abs(d);
						else if (c < 0){
							if (c + d>0)
								Dni = d + 2 * c;
							else if (d == -c)
								Dni = -d;
							else if (d < -c)
								Dni = -d;
						}
						Dni = _abs(x+a)*(4*x2*b+8*x*a*y+8*x*a*b+4*a2*y+4*a2*b - 12*b*y2-12*b2*y-4*b2*b) + Dni*(4*x2*y - 4*y2*y) + b0;

						Di = Dni;
						Dr = Dnr;
					}
				}
			}
			// 4th Celtic Burning Ship Partial Real Mbar
			else if (m_nFractalType == 20){
				if (antal<nMaxIter && test1 <= m_nBailout2){
					for (; antal<nMaxIter && test1 <= m_nBailout2; antal++){
						yr = dxr[antal] + Dr;
						yi = dxi[antal] + Di;
						test2 = test1;
						test1 = g_real*yr*yr + g_imag*yi*yi;
						if (test1<m_db_z[antal]){
							
								test1 = m_nBailout2 * 2;
							bGlitch = TRUE;
						}
						if(test1 > m_nBailout2)
							break;
						long double &x = dxr[antal];
						long double &y = dxi[antal];
						long double &a = Dr;
						long double &b = Di;
						long double &a0 = D0r;
						long double &b0 = D0i;
						long double x2 = x*x;
						long double y2 = y*y;
						long double a2 = a*a;
						long double b2 = b*b;
						long double xd = x+a;
						long double xd2 = xd*xd;
						long double yd = y+b;
						long double yd2 = yd*yd;

						//func5
						long double c = x2*x2 + y2*y2 - 6*x2*y2;
						long double d = 4*x2*x*a+6*x2*a2+4*x*a2*a+a2*a2 + 4*y2*y*b+6*y2*b2+4*y*b2*b+b2*b2 - 12*a*x*y2-6*a2*y2-12*b*x2*y-24*a*b*x*y-12*a2*b*y-6*b2*x2-12*a*b2*x-6*a2*b2;
						if (c>0){
							if (c + d>0)
								Dnr = d;
							else if (d == -c)
								Dnr = d;
							else if (d<-c)
								Dnr = -d - 2 * c;
						}
						else if (c == 0)
							Dnr = _abs(d);
						else if (c < 0){
							if (c + d>0)
								Dnr = d + 2 * c;
							else if (d == -c)
								Dnr = -d;
							else if (d < -c)
								Dnr = -d;
						}
						Dnr = Dnr + a0;

						//func4
						c = x;
						d = a;
						if (c>0){
							if (c + d>0)
								Dni = d;
							else if (d == -c)
								Dni = d;
							else if (d<-c)
								Dni = -d - 2 * c;
						}
						else if (c == 0)
							Dni = _abs(d);
						else if (c < 0){
							if (c + d>0)
								Dni = d + 2 * c;
							else if (d == -c)
								Dni = -d;
							else if (d < -c)
								Dni = -d;
						}
						Dni = _abs(x+a)*(12*y2*b+12*y*b2+4*b2*b - 8*a*x*y-4*a2*y-4*b*x2-8*a*b*x-4*a2*b) + Dni*(4*y2*y - 4*x2*y) + b0;

						Di = Dni;
						Dr = Dnr;
					}
				}
			}
			// 4th Buffalo Partial Imag
			else if (m_nFractalType == 21){
				if (antal<nMaxIter && test1 <= m_nBailout2){
					for (; antal<nMaxIter && test1 <= m_nBailout2; antal++){
						yr = dxr[antal] + Dr;
						yi = dxi[antal] + Di;
						test2 = test1;
						test1 = g_real*yr*yr + g_imag*yi*yi;
						if (test1<m_db_z[antal]){
							
								test1 = m_nBailout2 * 2;
							bGlitch = TRUE;
						}
						if(test1 > m_nBailout2)
							break;
						long double &x = dxr[antal];
						long double &y = dxi[antal];
						long double &a = Dr;
						long double &b = Di;
						long double &a0 = D0r;
						long double &b0 = D0i;
						long double x2 = x*x;
						long double y2 = y*y;
						long double a2 = a*a;
						long double b2 = b*b;
						long double xd = x+a;
						long double xd2 = xd*xd;
						long double yd = y+b;
						long double yd2 = yd*yd;
						//func2
						Dnr = 4*x2*x*a+6*x2*a2+4*x*a2*a+a2*a2+4*y2*y*b+6*y2*b2+4*y*b2*b+b2*b2-12*x2*y*b-6*x2*b2-12*x*a*y2-24*x*a*y*b-12*x*a*b2-6*a2*y2-12*a2*y*b-6*a2*b2 + a0;

						//func6
						long double c = 4*x2*x*y-4*x*y2*y;
						long double d = -4*a*y2*y-12*b*x*y2-12*a*b*y2+12*a*x2*y-12*b2*x*y+12*a2*x*y-12*a*b2*y+4*a2*a*y+4*b*x2*x+12*a*b*x2-4*b2*b*x+12*a2*b*x-4*a*b2*b+4*a2*a*b;
						if (c>0){
							if (c + d>0)
								Dni = d;
							else if (d == -c)
								Dni = d;
							else if (d<-c)
								Dni = -d - 2 * c;
						}
						else if (c == 0)
							Dni = _abs(d);
						else if (c < 0){
							if (c + d>0)
								Dni = d + 2 * c;
							else if (d == -c)
								Dni = -d;
							else if (d < -c)
								Dni = -d;
						}
						Dni=Dni+b0;

						Di = Dni;
						Dr = Dnr;
					}
				}
			}
			// 4th Celtic Mbar
			else if (m_nFractalType == 22){
				if (antal<nMaxIter && test1 <= m_nBailout2){
					for (; antal<nMaxIter && test1 <= m_nBailout2; antal++){
						yr = dxr[antal] + Dr;
						yi = dxi[antal] + Di;
						test2 = test1;
						test1 = g_real*yr*yr + g_imag*yi*yi;
						if (test1<m_db_z[antal]){
							
								test1 = m_nBailout2 * 2;
							bGlitch = TRUE;
						}
						if(test1 > m_nBailout2)
							break;
						long double &x = dxr[antal];
						long double &y = dxi[antal];
						long double &a = Dr;
						long double &b = Di;
						long double &a0 = D0r;
						long double &b0 = D0i;
						long double x2 = x*x;
						long double y2 = y*y;
						long double a2 = a*a;
						long double b2 = b*b;
						long double xd = x+a;
						long double xd2 = xd*xd;
						long double yd = y+b;
						long double yd2 = yd*yd;

						//func5
						long double c = x2*x2 + y2*y2 - 6*x2*y2;
						long double d = 4*x2*x*a+6*x2*a2+4*x*a2*a+a2*a2 + 4*y2*y*b+6*y2*b2+4*y*b2*b+b2*b2 - 12*a*x*y2-6*a2*y2-12*b*x2*y-24*a*b*x*y-12*a2*b*y-6*b2*x2-12*a*b2*x-6*a2*b2;
						if (c>0){
							if (c + d>0)
								Dnr = d;
							else if (d == -c)
								Dnr = d;
							else if (d<-c)
								Dnr = -d - 2 * c;
						}
						else if (c == 0)
							Dnr = _abs(d);
						else if (c < 0){
							if (c + d>0)
								Dnr = d + 2 * c;
							else if (d == -c)
								Dnr = -d;
							else if (d < -c)
								Dnr = -d;
						}
						Dnr = Dnr + a0;

						//func8
						Dni = b0 - (12*x2*y*a+12*x*y*a2-12*x*y2*b-12*x*y*b2+4*x2*x*b+12*x2*b*a+12*x*b*a2-4*x*b2*b+4*a2*a*y-4*a*y2*y-12*a*y2*b-12*a*y*b2+4*a2*a*b-4*a*b2*b);
						
						Di = Dni;
						Dr = Dnr;
					}
				}
			}
			// 4th False Quasi Perpendicular
			else if (m_nFractalType == 23){
				if (antal<nMaxIter && test1 <= m_nBailout2){
					for (; antal<nMaxIter && test1 <= m_nBailout2; antal++){
						yr = dxr[antal] + Dr;
						yi = dxi[antal] + Di;
						test2 = test1;
						test1 = g_real*yr*yr + g_imag*yi*yi;
						if (test1<m_db_z[antal]){
							
								test1 = m_nBailout2 * 2;
							bGlitch = TRUE;
						}
						if(test1 > m_nBailout2)
							break;
						long double &x = dxr[antal];
						long double &y = dxi[antal];
						long double &a = Dr;
						long double &b = Di;
						long double &a0 = D0r;
						long double &b0 = D0i;
						long double x2 = x*x;
						long double y2 = y*y;
						long double a2 = a*a;
						long double b2 = b*b;
						long double xd = x+a;
						long double xd2 = xd*xd;
						long double yd = y+b;
						long double yd2 = yd*yd;
						//func2
						Dnr = 4*x2*x*a+6*x2*a2+4*x*a2*a+a2*a2+4*y2*y*b+6*y2*b2+4*y*b2*b+b2*b2-12*x2*y*b-6*x2*b2-12*x*a*y2-24*x*a*y*b-12*x*a*b2-6*a2*y2-12*a2*y*b-6*a2*b2 + a0;

						//func9
						long double c = x2-y2;
						long double d = -2*b*y+2*a*x-b2+a2;
						if (c>0){
							if (c + d>0)
								Dni = d;
							else if (d == -c)
								Dni = d;
							else if (d<-c)
								Dni = -d - 2 * c;
						}
						else if (c == 0)
							Dni = _abs(d);
						else if (c < 0){
							if (c + d>0)
								Dni = d + 2 * c;
							else if (d == -c)
								Dni = -d;
							else if (d < -c)
								Dni = -d;
						}
						Dni = -(4*x*y)*Dni - (4*x*b + 4*a*y + 4*a*b)*_abs(-y2-2*b*y+x2+2*a*x-b2+a2) + b0;

						Di = Dni;
						Dr = Dnr;
					}
				}
			}
			// 4th False Quasi Heart
			else if (m_nFractalType == 24){
				if (antal<nMaxIter && test1 <= m_nBailout2){
					for (; antal<nMaxIter && test1 <= m_nBailout2; antal++){
						yr = dxr[antal] + Dr;
						yi = dxi[antal] + Di;
						test2 = test1;
						test1 = g_real*yr*yr + g_imag*yi*yi;
						if (test1<m_db_z[antal]){
							
								test1 = m_nBailout2 * 2;
							bGlitch = TRUE;
						}
						if(test1 > m_nBailout2)
							break;
						long double &x = dxr[antal];
						long double &y = dxi[antal];
						long double &a = Dr;
						long double &b = Di;
						long double &a0 = D0r;
						long double &b0 = D0i;
						long double x2 = x*x;
						long double y2 = y*y;
						long double a2 = a*a;
						long double b2 = b*b;
						long double xd = x+a;
						long double xd2 = xd*xd;
						long double yd = y+b;
						long double yd2 = yd*yd;
						//func2
						Dnr = 4*x2*x*a+6*x2*a2+4*x*a2*a+a2*a2+4*y2*y*b+6*y2*b2+4*y*b2*b+b2*b2-12*x2*y*b-6*x2*b2-12*x*a*y2-24*x*a*y*b-12*x*a*b2-6*a2*y2-12*a2*y*b-6*a2*b2 + a0;

						//func10
						long double c = x2-y2;
						long double d = -2*b*y+2*a*x-b2+a2;
						if (c>0){
							if (c + d>0)
								Dni = d;
							else if (d == -c)
								Dni = d;
							else if (d<-c)
								Dni = -d - 2 * c;
						}
						else if (c == 0)
							Dni = _abs(d);
						else if (c < 0){
							if (c + d>0)
								Dni = d + 2 * c;
							else if (d == -c)
								Dni = -d;
							else if (d < -c)
								Dni = -d;
						}
						Dni = (4*x*y)*Dni + (4*x*b + 4*a*y + 4*a*b)*_abs(-y2-2*b*y+x2+2*a*x-b2+a2) + b0;

						Di = Dni;
						Dr = Dnr;
					}
				}
			}
			// 4th Celtic False Quasi Perpendicular
			else if (m_nFractalType == 25){
				if (antal<nMaxIter && test1 <= m_nBailout2){
					for (; antal<nMaxIter && test1 <= m_nBailout2; antal++){
						yr = dxr[antal] + Dr;
						yi = dxi[antal] + Di;
						test2 = test1;
						test1 = g_real*yr*yr + g_imag*yi*yi;
						if (test1<m_db_z[antal]){
							
								test1 = m_nBailout2 * 2;
							bGlitch = TRUE;
						}
						if(test1 > m_nBailout2)
							break;
						long double &x = dxr[antal];
						long double &y = dxi[antal];
						long double &a = Dr;
						long double &b = Di;
						long double &a0 = D0r;
						long double &b0 = D0i;
						long double x2 = x*x;
						long double y2 = y*y;
						long double a2 = a*a;
						long double b2 = b*b;
						long double xd = x+a;
						long double xd2 = xd*xd;
						long double yd = y+b;
						long double yd2 = yd*yd;

						//func5
						long double c = x2*x2 + y2*y2 - 6*x2*y2;
						long double d = 4*x2*x*a+6*x2*a2+4*x*a2*a+a2*a2 + 4*y2*y*b+6*y2*b2+4*y*b2*b+b2*b2 - 12*a*x*y2-6*a2*y2-12*b*x2*y-24*a*b*x*y-12*a2*b*y-6*b2*x2-12*a*b2*x-6*a2*b2;
						if (c>0){
							if (c + d>0)
								Dnr = d;
							else if (d == -c)
								Dnr = d;
							else if (d<-c)
								Dnr = -d - 2 * c;
						}
						else if (c == 0)
							Dnr = _abs(d);
						else if (c < 0){
							if (c + d>0)
								Dnr = d + 2 * c;
							else if (d == -c)
								Dnr = -d;
							else if (d < -c)
								Dnr = -d;
						}
						Dnr = Dnr + a0;

						//func9
						c = x2-y2;
						d = -2*b*y+2*a*x-b2+a2;
						if (c>0){
							if (c + d>0)
								Dni = d;
							else if (d == -c)
								Dni = d;
							else if (d<-c)
								Dni = -d - 2 * c;
						}
						else if (c == 0)
							Dni = _abs(d);
						else if (c < 0){
							if (c + d>0)
								Dni = d + 2 * c;
							else if (d == -c)
								Dni = -d;
							else if (d < -c)
								Dni = -d;
						}
						Dni = -(4*x*y)*Dni - (4*x*b + 4*a*y + 4*a*b)*_abs(-y2-2*b*y+x2+2*a*x-b2+a2) + b0;

						Di = Dni;
						Dr = Dnr;
					}
				}
			}
			// 4th Celtic False Quasi Heart
			else if (m_nFractalType == 26){
				if (antal<nMaxIter && test1 <= m_nBailout2){
					for (; antal<nMaxIter && test1 <= m_nBailout2; antal++){
						yr = dxr[antal] + Dr;
						yi = dxi[antal] + Di;
						test2 = test1;
						test1 = g_real*yr*yr + g_imag*yi*yi;
						if (test1<m_db_z[antal]){
							
								test1 = m_nBailout2 * 2;
							bGlitch = TRUE;
						}
						if(test1 > m_nBailout2)
							break;
						long double &x = dxr[antal];
						long double &y = dxi[antal];
						long double &a = Dr;
						long double &b = Di;
						long double &a0 = D0r;
						long double &b0 = D0i;
						long double x2 = x*x;
						long double y2 = y*y;
						long double a2 = a*a;
						long double b2 = b*b;
						long double xd = x+a;
						long double xd2 = xd*xd;
						long double yd = y+b;
						long double yd2 = yd*yd;
						//func5
						long double c = x2*x2 + y2*y2 - 6*x2*y2;
						long double d = 4*x2*x*a+6*x2*a2+4*x*a2*a+a2*a2 + 4*y2*y*b+6*y2*b2+4*y*b2*b+b2*b2 - 12*a*x*y2-6*a2*y2-12*b*x2*y-24*a*b*x*y-12*a2*b*y-6*b2*x2-12*a*b2*x-6*a2*b2;
						if (c>0){
							if (c + d>0)
								Dnr = d;
							else if (d == -c)
								Dnr = d;
							else if (d<-c)
								Dnr = -d - 2 * c;
						}
						else if (c == 0)
							Dnr = _abs(d);
						else if (c < 0){
							if (c + d>0)
								Dnr = d + 2 * c;
							else if (d == -c)
								Dnr = -d;
							else if (d < -c)
								Dnr = -d;
						}
						Dnr = Dnr + a0;
						//func10
						c = x2-y2;
						d = -2*b*y+2*a*x-b2+a2;
						if (c>0){
							if (c + d>0)
								Dni = d;
							else if (d == -c)
								Dni = d;
							else if (d<-c)
								Dni = -d - 2 * c;
						}
						else if (c == 0)
							Dni = _abs(d);
						else if (c < 0){
							if (c + d>0)
								Dni = d + 2 * c;
							else if (d == -c)
								Dni = -d;
							else if (d < -c)
								Dni = -d;
						}
						Dni = (4*x*y)*Dni + (4*x*b + 4*a*y + 4*a*b)*_abs(-y2-2*b*y+x2+2*a*x-b2+a2) + b0;

						Di = Dni;
						Dr = Dnr;
					}
				}
			}
			//5th Burning Ship Partial
			else if (m_nFractalType == 27){
				if (antal<nMaxIter && test1 <= m_nBailout2){
					for (; antal<nMaxIter && test1 <= m_nBailout2; antal++){
						yr = dxr[antal] + Dr;
						yi = dxi[antal] + Di;
						test2 = test1;
						test1 = g_real*yr*yr + g_imag*yi*yi;
						if (test1<m_db_z[antal]){
							
								test1 = m_nBailout2 * 2;
							bGlitch = TRUE;
						}
						if(test1 > m_nBailout2)
							break;
						long double &x = dxr[antal];
						long double &y = dxi[antal];
						long double &a = Dr;
						long double &b = Di;
						long double &a0 = D0r;
						long double &b0 = D0i;
						long double x2 = x*x;
						long double y2 = y*y;
						long double a2 = a*a;
						long double b2 = b*b;
						long double xd = x+a;
						long double xd2 = xd*xd;
						long double yd = y+b;
						long double yd2 = yd*yd;

						//func14
						long double c = x;
						long double d = a;
						if (c>0){
							if (c + d>0)
								Dnr = d;
							else if (d == -c)
								Dnr = d;
							else if (d<-c)
								Dnr = -d - 2 * c;
						}
						else if (c == 0)
							Dnr = _abs(d);
						else if (c < 0){
							if (c + d>0)
								Dnr = d + 2 * c;
							else if (d == -c)
								Dnr = -d;
							else if (d < -c)
								Dnr = -d;
						}
						Dnr = (Dnr) * (x2*x2- 10*x2*y2 + 5*y2*y2) + _abs(x+a) * (4*x2*x*a+6*x2*a2+4*x*a2*a+a2*a2 -20*x2*y*b-10*x2*b2-20*x*a*y2-40*x*a*y*b-20*x*a*b2-10*a2*y2-20*a2*y*b-10*a2*b2 + 20*y2*y*b+30*y2*b2+20*y*b2*b+5*b2*b2) + a0;

						//func15
						Dni = 20*y*x2*x*a+30*y*x2*a2+20*y*x*a2*a+5*y*a2*a2-30*y2*x2*b-30*y*x2*b2-20*y2*y*x*a-60*y2*x*a*b-60*y*x*a*b2-10*y2*y*a2-30*y2*a2*b-30*y*a2*b2+5*y2*y2*b+10*y2*y*b2+10*y2*b2*b+5*y*b2*b2+5*b*x2*x2+20*b*x2*x*a+30*b*x2*a2+20*b*x*a2*a+5*b*a2*a2-10*b2*b*x2-20*b2*b*x*a-10*b2*b*a2+b2*b2*b + b0;

						Di = Dni;
						Dr = Dnr;
					}
				}
			}
			//5th Burning Ship Partial Mbar
			else if (m_nFractalType == 28){
				if (antal<nMaxIter && test1 <= m_nBailout2){
					for (; antal<nMaxIter && test1 <= m_nBailout2; antal++){
						yr = dxr[antal] + Dr;
						yi = dxi[antal] + Di;
						test2 = test1;
						test1 = g_real*yr*yr + g_imag*yi*yi;
						if (test1<m_db_z[antal]){
							
								test1 = m_nBailout2 * 2;
							bGlitch = TRUE;
						}
						if(test1 > m_nBailout2)
							break;
						long double &x = dxr[antal];
						long double &y = dxi[antal];
						long double &a = Dr;
						long double &b = Di;
						long double &a0 = D0r;
						long double &b0 = D0i;
						long double x2 = x*x;
						long double y2 = y*y;
						long double a2 = a*a;
						long double b2 = b*b;
						long double xd = x+a;
						long double xd2 = xd*xd;
						long double yd = y+b;
						long double yd2 = yd*yd;

						//func14
						long double c = x;
						long double d = a;
						if (c>0){
							if (c + d>0)
								Dnr = d;
							else if (d == -c)
								Dnr = d;
							else if (d<-c)
								Dnr = -d - 2 * c;
						}
						else if (c == 0)
							Dnr = _abs(d);
						else if (c < 0){
							if (c + d>0)
								Dnr = d + 2 * c;
							else if (d == -c)
								Dnr = -d;
							else if (d < -c)
								Dnr = -d;
						}
						Dnr = (Dnr) * (x2*x2- 10*x2*y2 + 5*y2*y2) + _abs(x+a) * (4*x2*x*a+6*x2*a2+4*x*a2*a+a2*a2 -20*x2*y*b-10*x2*b2-20*x*a*y2-40*x*a*y*b-20*x*a*b2-10*a2*y2-20*a2*y*b-10*a2*b2 + 20*y2*y*b+30*y2*b2+20*y*b2*b+5*b2*b2) + a0;

						//func16
						Dni = b0 - (20*y*x2*x*a+30*y*x2*a2+20*y*x*a2*a+5*y*a2*a2-30*y2*x2*b-30*y*x2*b2-20*y2*y*x*a-60*y2*x*a*b-60*y*x*a*b2-10*y2*y*a2-30*y2*a2*b-30*y*a2*b2+5*y2*y2*b+10*y2*y*b2+10*y2*b2*b+5*y*b2*b2+5*b*x2*x2+20*b*x2*x*a+30*b*x2*a2+20*b*x*a2*a+5*b*a2*a2-10*b2*b*x2-20*b2*b*x*a-10*b2*b*a2+b2*b2*b);

						Di = Dni;
						Dr = Dnr;
					}
				}
			}
			//5th Celtic Mbar
			else if (m_nFractalType == 29){
				if (antal<nMaxIter && test1 <= m_nBailout2){
					for (; antal<nMaxIter && test1 <= m_nBailout2; antal++){
						yr = dxr[antal] + Dr;
						yi = dxi[antal] + Di;
						test2 = test1;
						test1 = g_real*yr*yr + g_imag*yi*yi;
						if (test1<m_db_z[antal]){
							
								test1 = m_nBailout2 * 2;
							bGlitch = TRUE;
						}
						if(test1 > m_nBailout2)
							break;
						long double &x = dxr[antal];
						long double &y = dxi[antal];
						long double &a = Dr;
						long double &b = Di;
						long double &a0 = D0r;
						long double &b0 = D0i;
						long double x2 = x*x;
						long double y2 = y*y;
						long double a2 = a*a;
						long double b2 = b*b;
						long double xd = x+a;
						long double xd2 = xd*xd;
						long double yd = y+b;
						long double yd2 = yd*yd;

						//func17
						long double c = 5*x*y2*y2-10*x2*x*y2+x2*x2*x;
						long double d = 20*x*b*y2*y-30*x2*a*y2+30*x*b2*y2-30*x*a2*y2-20*x2*x*b*y-60*x2*a*b*y+20*x*b2*b*y-60*x*a2*b*y+5*x2*x2*a-10*x2*x*b2+10*x2*x*a2-30*x2*a*b2+10*x2*a2*a+5*x*b2*b2-30*x*a2*b2+5*x*a2*a2+5*a*y2*y2+20*a*b*y2*y+30*a*b2*y2-10*a2*a*y2+20*a*b2*b*y-20*a2*a*b*y+5*a*b2*b2-10*a2*a*b2+a2*a2*a;
						if (c>0){
							if (c + d>0)
								Dnr = d;
							else if (d == -c)
								Dnr = d;
							else if (d<-c)
								Dnr = -d - 2 * c;
						}
						else if (c == 0)
							Dnr = _abs(d);
						else if (c < 0){
							if (c + d>0)
								Dnr = d + 2 * c;
							else if (d == -c)
								Dnr = -d;
							else if (d < -c)
								Dnr = -d;
						}
						Dnr+=a0;

						//func16
						Dni = b0 - (20*y*x2*x*a+30*y*x2*a2+20*y*x*a2*a+5*y*a2*a2-30*y2*x2*b-30*y*x2*b2-20*y2*y*x*a-60*y2*x*a*b-60*y*x*a*b2-10*y2*y*a2-30*y2*a2*b-30*y*a2*b2+5*y2*y2*b+10*y2*y*b2+10*y2*b2*b+5*y*b2*b2+5*b*x2*x2+20*b*x2*x*a+30*b*x2*a2+20*b*x*a2*a+5*b*a2*a2-10*b2*b*x2-20*b2*b*x*a-10*b2*b*a2+b2*b2*b);

						Di = Dni;
						Dr = Dnr;
					}
				}
			}
			//5th Quasi Burning Ship (BS/Buffalo Hybrid)
			else if (m_nFractalType == 30){
				if (antal<nMaxIter && test1 <= m_nBailout2){
					for (; antal<nMaxIter && test1 <= m_nBailout2; antal++){
						yr = dxr[antal] + Dr;
						yi = dxi[antal] + Di;
						test2 = test1;
						test1 = g_real*yr*yr + g_imag*yi*yi;
						if (test1<m_db_z[antal]){
							
								test1 = m_nBailout2 * 2;
							bGlitch = TRUE;
						}
						if(test1 > m_nBailout2)
							break;
						long double &x = dxr[antal];
						long double &y = dxi[antal];
						long double &a = Dr;
						long double &b = Di;
						long double &a0 = D0r;
						long double &b0 = D0i;
						long double x2 = x*x;
						long double y2 = y*y;
						long double a2 = a*a;
						long double b2 = b*b;
						long double xd = x+a;
						long double xd2 = xd*xd;
						long double yd = y+b;
						long double yd2 = yd*yd;

						//func14
						long double c = x;
						long double d = a;
						if (c>0){
							if (c + d>0)
								Dnr = d;
							else if (d == -c)
								Dnr = d;
							else if (d<-c)
								Dnr = -d - 2 * c;
						}
						else if (c == 0)
							Dnr = _abs(d);
						else if (c < 0){
							if (c + d>0)
								Dnr = d + 2 * c;
							else if (d == -c)
								Dnr = -d;
							else if (d < -c)
								Dnr = -d;
						}
						Dnr = (Dnr) * (x2*x2- 10*x2*y2 + 5*y2*y2) + _abs(x+a) * (4*x2*x*a+6*x2*a2+4*x*a2*a+a2*a2 -20*x2*y*b-10*x2*b2-20*x*a*y2-40*x*a*y*b-20*x*a*b2-10*a2*y2-20*a2*y*b-10*a2*b2 + 20*y2*y*b+30*y2*b2+20*y*b2*b+5*b2*b2) + a0;

						//func18
						c = y2*y2*y-10*y2*y*x2+5*y*x2*x2;
						d = 5*y2*y2*b-20*y2*y*a*x+10*y2*y*b2-10*y2*y*a2-30*y2*b*x2-60*y2*a*b*x+10*y2*b2*b-30*y2*a2*b+20*y*a*x2*x-30*y*b2*x2+30*y*a2*x2-60*y*a*b2*x+20*y*a2*a*x+5*y*b2*b2-30*y*a2*b2+5*y*a2*a2+5*b*x2*x2+20*b*a*x2*x-10*b2*b*x2+30*b*a2*x2-20*b2*b*a*x+20*b*a2*a*x+b2*b2*b-10*b2*b*a2+5*b*a2*a2;
						if (c>0){
							if (c + d>0)
								Dni = d;
							else if (d == -c)
								Dni = d;
							else if (d<-c)
								Dni = -d - 2 * c;
						}
						else if (c == 0)
							Dni = _abs(d);
						else if (c < 0){
							if (c + d>0)
								Dni = d + 2 * c;
							else if (d == -c)
								Dni = -d;
							else if (d < -c)
								Dni = -d;
						}
						Dni=b0-Dni;

						Di = Dni;
						Dr = Dnr;
					}
				}
			}
			//5th Quasi Perpendicular
			else if (m_nFractalType == 31){
				if (antal<nMaxIter && test1 <= m_nBailout2){
					for (; antal<nMaxIter && test1 <= m_nBailout2; antal++){
						yr = dxr[antal] + Dr;
						yi = dxi[antal] + Di;
						test2 = test1;
						test1 = g_real*yr*yr + g_imag*yi*yi;
						if (test1<m_db_z[antal]){
							
								test1 = m_nBailout2 * 2;
							bGlitch = TRUE;
						}
						if(test1 > m_nBailout2)
							break;
						long double &x = dxr[antal];
						long double &y = dxi[antal];
						long double &a = Dr;
						long double &b = Di;
						long double &a0 = D0r;
						long double &b0 = D0i;
						long double x2 = x*x;
						long double y2 = y*y;
						long double a2 = a*a;
						long double b2 = b*b;
						long double xd = x+a;
						long double xd2 = xd*xd;
						long double yd = y+b;
						long double yd2 = yd*yd;

						//func14
						long double c = x;
						long double d = a;
						if (c>0){
							if (c + d>0)
								Dnr = d;
							else if (d == -c)
								Dnr = d;
							else if (d<-c)
								Dnr = -d - 2 * c;
						}
						else if (c == 0)
							Dnr = _abs(d);
						else if (c < 0){
							if (c + d>0)
								Dnr = d + 2 * c;
							else if (d == -c)
								Dnr = -d;
							else if (d < -c)
								Dnr = -d;
						}
						Dnr = (Dnr) * (x2*x2- 10*x2*y2 + 5*y2*y2) + _abs(x+a) * (4*x2*x*a+6*x2*a2+4*x*a2*a+a2*a2 -20*x2*y*b-10*x2*b2-20*x*a*y2-40*x*a*y*b-20*x*a*b2-10*a2*y2-20*a2*y*b-10*a2*b2 + 20*y2*y*b+30*y2*b2+20*y*b2*b+5*b2*b2) + a0;

						//func19
						c = 5*x2*x2 - 10*x2*y2 + y2*y2;
						d = 4*b*y2*y-20*a*x*y2+6*b2*y2-10*a2*y2-20*b*x2*y-40*a*b*x*y+4*b2*b*y-20*a2*b*y+20*a*x2*x-10*b2*x2+30*a2*x2-20*a*b2*x+20*a2*a*x+b2*b2-10*a2*b2+5*a2*a2;
						if (c>0){
							if (c + d>0)
								Dni = d;
							else if (d == -c)
								Dni = d;
							else if (d<-c)
								Dni = -d - 2 * c;
						}
						else if (c == 0)
							Dni = _abs(d);
						else if (c < 0){
							if (c + d>0)
								Dni = d + 2 * c;
							else if (d == -c)
								Dni = -d;
							else if (d < -c)
								Dni = -d;
						}
						Dni = -y * Dni - b * _abs(y2*y2+4*b*y2*y-10*x2*y2-20*a*x*y2+6*b2*y2-10*a2*y2-20*b*x2*y-40*a*b*x*y+4*b2*b*y-20*a2*b*y+5*x2*x2+20*a*x2*x-10*b2*x2+30*a2*x2-20*a*b2*x+20*a2*a*x+b2*b2-10*a2*b2+5*a2*a2) + b0;

						Di = Dni;
						Dr = Dnr;
					}
				}
			}
			//5th Quasi Heart
			else if (m_nFractalType == 32){
				if (antal<nMaxIter && test1 <= m_nBailout2){
					for (; antal<nMaxIter && test1 <= m_nBailout2; antal++){
						yr = dxr[antal] + Dr;
						yi = dxi[antal] + Di;
						test2 = test1;
						test1 = g_real*yr*yr + g_imag*yi*yi;
						if (test1<m_db_z[antal]){
							
								test1 = m_nBailout2 * 2;
							bGlitch = TRUE;
						}
						if(test1 > m_nBailout2)
							break;
						long double &x = dxr[antal];
						long double &y = dxi[antal];
						long double &a = Dr;
						long double &b = Di;
						long double &a0 = D0r;
						long double &b0 = D0i;
						long double x2 = x*x;
						long double y2 = y*y;
						long double a2 = a*a;
						long double b2 = b*b;
						long double xd = x+a;
						long double xd2 = xd*xd;
						long double yd = y+b;
						long double yd2 = yd*yd;

						//func14
						long double c = x;
						long double d = a;
						if (c>0){
							if (c + d>0)
								Dnr = d;
							else if (d == -c)
								Dnr = d;
							else if (d<-c)
								Dnr = -d - 2 * c;
						}
						else if (c == 0)
							Dnr = _abs(d);
						else if (c < 0){
							if (c + d>0)
								Dnr = d + 2 * c;
							else if (d == -c)
								Dnr = -d;
							else if (d < -c)
								Dnr = -d;
						}
						Dnr = (Dnr) * (x2*x2- 10*x2*y2 + 5*y2*y2) + _abs(x+a) * (4*x2*x*a+6*x2*a2+4*x*a2*a+a2*a2 -20*x2*y*b-10*x2*b2-20*x*a*y2-40*x*a*y*b-20*x*a*b2-10*a2*y2-20*a2*y*b-10*a2*b2 + 20*y2*y*b+30*y2*b2+20*y*b2*b+5*b2*b2) + a0;

						//func19
						c = 5*x2*x2 - 10*x2*y2 + y2*y2;
						d = 4*b*y2*y-20*a*x*y2+6*b2*y2-10*a2*y2-20*b*x2*y-40*a*b*x*y+4*b2*b*y-20*a2*b*y+20*a*x2*x-10*b2*x2+30*a2*x2-20*a*b2*x+20*a2*a*x+b2*b2-10*a2*b2+5*a2*a2;
						if (c>0){
							if (c + d>0)
								Dni = d;
							else if (d == -c)
								Dni = d;
							else if (d<-c)
								Dni = -d - 2 * c;
						}
						else if (c == 0)
							Dni = _abs(d);
						else if (c < 0){
							if (c + d>0)
								Dni = d + 2 * c;
							else if (d == -c)
								Dni = -d;
							else if (d < -c)
								Dni = -d;
						}
						Dni = y * Dni + b * _abs(y2*y2+4*b*y2*y-10*x2*y2-20*a*x*y2+6*b2*y2-10*a2*y2-20*b*x2*y-40*a*b*x*y+4*b2*b*y-20*a2*b*y+5*x2*x2+20*a*x2*x-10*b2*x2+30*a2*x2-20*a*b2*x+20*a2*a*x+b2*b2-10*a2*b2+5*a2*a2) + b0;

						Di = Dni;
						Dr = Dnr;
					}
				}
			}
			//FT_Simon100A_plain
			else if (m_nFractalType == 33){
				if (antal<nMaxIter && test1 <= m_nBailout2){
					for (; antal<nMaxIter && test1 <= m_nBailout2; antal++){
						yr = dxr[antal] + Dr;
						yi = dxi[antal] + Di;
						test2 = test1;
						test1 = g_real*yr*yr + g_imag*yi*yi;
						if (test1<m_db_z[antal]){
							
								test1 = m_nBailout2 * 2;
							bGlitch = TRUE;
						}
						if(test1 > m_nBailout2)
							break;
						long double &x = dxr[antal];
						long double &y = dxi[antal];
						long double &a = Dr;
						long double &b = Di;
						long double &a0 = D0r;
						long double &b0 = D0i;
						long double x2 = x*x;
						long double y2 = y*y;
						long double a2 = a*a;
						long double b2 = b*b;
						long double xd = x+a;
						long double xd2 = xd*xd;
						long double yd = y+b;
						long double yd2 = yd*yd;


						//Dnr = b*b*abs(b*b)-4*b*abs(a*b)*a-a*a*abs(b*b)-b*b*abs(a*a)+a*a*abs(a*a) + a0;
						//Dni = - 2*a*b*abs(b*b)-2*b*b*abs(a*b)+2*a*a*abs(a*b)+2*a*b*abs(a*a) + b0;

						Dnr = (y2)*lb_abs_ldb(y2,2*b*y+b2)-4*y*x*lb_abs_ldb(x*y,x*b+a*y+a*b)-x2*lb_abs_ldb(y2,2*b*y+b2)-y2*lb_abs_ldb(x2,2*x*a+a2)+x2*lb_abs_ldb(x2,2*x*a+a2) 
							+ (2*b*y+b2)*_abs(y2+2*b*y+b2)-4*(y*a+b*x+b*a)*_abs(x*y+x*b+a*y+a*b)-(2*x*a+a2)*_abs(y2+2*b*y+b2)-(2*b*y+b2)*_abs(x2+2*x*a+a2)+(2*x*a+a2)*_abs(x2+2*x*a+a2) + a0;


						Dni = 2*x2*lb_abs_ldb(x*y,x*b+a*y+a*b)+2*x*y*lb_abs_ldb(x2,2*x*a+a2)-2*x*y*lb_abs_ldb(y2,2*b*y+b2)-2*y2*lb_abs_ldb(x*y,x*b+a*y+a*b) 
							+ 2*(2*x*a+a2)*_abs(x*y+x*b+a*y+a*b) +2*(x*b+a*y+a*b)*_abs(x2+2*x*a+a2)-2*(x*b+a*y+a*b)*_abs(y2+2*b*y+b2)-2*(2*b*y+b2)*_abs(x*y+x*b+a*y+a*b)  + b0;

						Dr=Dnr;
						Di=Dni;
					}
				}
			}
			//4th Imag Quasi Perpendicular / Heart
			else if (m_nFractalType == 34){
				if (antal<nMaxIter && test1 <= m_nBailout2){
					for (; antal<nMaxIter && test1 <= m_nBailout2; antal++){
						yr = dxr[antal] + Dr;
						yi = dxi[antal] + Di;
						test2 = test1;
						test1 = g_real*yr*yr + g_imag*yi*yi;
						if (test1<m_db_z[antal]){
							
								test1 = m_nBailout2 * 2;
							bGlitch = TRUE;
						}
						if(test1 > m_nBailout2)
							break;
						long double &x = dxr[antal];
						long double &y = dxi[antal];
						long double &a = Dr;
						long double &b = Di;
						long double &a0 = D0r;
						long double &b0 = D0i;
						long double x2 = x*x;
						long double y2 = y*y;
						long double a2 = a*a;
						long double b2 = b*b;
						long double xd = x+a;
						long double xd2 = xd*xd;
						long double yd = y+b;
						long double yd2 = yd*yd;

						Dnr = 4*x2*x*a+6*x2*a2+4*x*a2*a+a2*a2+4*y2*y*b+6*y2*b2+4*y*b2*b+b2*b2-12*x2*y*b-6*x2*b2-12*x*a*y2-24*x*a*y*b-12*x*a*b2-6*a2*y2-12*a2*y*b-6*a2*b2 +a0;
						Dni = 4 * (a) * _abs(- y2*y-3*b*y2+x2*y+2*a*x*y-3*b2*y+a2*y+b*x2+2*a*b*x-b2*b+a2*b) + 4 * x * lb_abs_ldb(- y2*y+x2*y,-3*b*y2+2*a*x*y-3*b2*y+a2*y+b*x2+2*a*b*x-b2*b+a2*b) + b0;	//func21

						Dr=Dnr;
						Di=Dni;
					}
				}
			}
			//4th Real Quasi Perpendicular
			else if (m_nFractalType == 35){
				if (antal<nMaxIter && test1 <= m_nBailout2){
					for (; antal<nMaxIter && test1 <= m_nBailout2; antal++){
						yr = dxr[antal] + Dr;
						yi = dxi[antal] + Di;
						test2 = test1;
						test1 = g_real*yr*yr + g_imag*yi*yi;
						if (test1<m_db_z[antal]){
							
								test1 = m_nBailout2 * 2;
							bGlitch = TRUE;
						}
						long double &x = dxr[antal];
						long double &y = dxi[antal];
						long double &a = Dr;
						long double &b = Di;
						long double &a0 = D0r;
						long double &b0 = D0i;
						long double x2 = x*x;
						long double y2 = y*y;
						long double a2 = a*a;
						long double b2 = b*b;
						long double xd = x+a;
						long double xd2 = xd*xd;
						long double yd = y+b;
						long double yd2 = yd*yd;

						Dnr = 4*x2*x*a+6*x2*a2+4*x*a2*a+a2*a2+4*y2*y*b+6*y2*b2+4*y*b2*b+b2*b2-12*x2*y*b-6*x2*b2-12*x*a*y2-24*x*a*y*b-12*x*a*b2-6*a2*y2-12*a2*y*b-6*a2*b2 +a0;
						Dni = -4*y*lb_abs_ldb(x2*x-x*y2,-a*y2-2*b*x*y-2*a*b*y+3*a*x2-b2*x+3*a2*x-a*b2+a2*a) - 4*b*_abs(- x*y2-a*y2-2*b*x*y-2*a*b*y+x2*x+3*a*x2-b2*x+3*a2*x-a*b2+a2*a) + b0;	//func22

						Dr=Dnr;
						Di=Dni;
					}
				}
			}
			//4th Real Quasi Heart
			else if (m_nFractalType == 36){
				if (antal<nMaxIter && test1 <= m_nBailout2){
					for (; antal<nMaxIter && test1 <= m_nBailout2; antal++){
						yr = dxr[antal] + Dr;
						yi = dxi[antal] + Di;
						test2 = test1;
						test1 = g_real*yr*yr + g_imag*yi*yi;
						if (test1<m_db_z[antal]){
							
								test1 = m_nBailout2 * 2;
							bGlitch = TRUE;
						}
						if(test1 > m_nBailout2)
							break;
						long double &x = dxr[antal];
						long double &y = dxi[antal];
						long double &a = Dr;
						long double &b = Di;
						long double &a0 = D0r;
						long double &b0 = D0i;
						long double x2 = x*x;
						long double y2 = y*y;
						long double a2 = a*a;
						long double b2 = b*b;
						long double xd = x+a;
						long double xd2 = xd*xd;
						long double yd = y+b;
						long double yd2 = yd*yd;

						Dnr = 4*x2*x*a+6*x2*a2+4*x*a2*a+a2*a2+4*y2*y*b+6*y2*b2+4*y*b2*b+b2*b2-12*x2*y*b-6*x2*b2-12*x*a*y2-24*x*a*y*b-12*x*a*b2-6*a2*y2-12*a2*y*b-6*a2*b2 +a0;
						Dni = 4*y*lb_abs_ldb(x2*x-x*y2,-a*y2-2*b*x*y-2*a*b*y+3*a*x2-b2*x+3*a2*x-a*b2+a2*a) + 4*b*_abs(- x*y2-a*y2-2*b*x*y-2*a*b*y+x2*x+3*a*x2-b2*x+3*a2*x-a*b2+a2*a) + b0;	//func22

						Dr=Dnr;
						Di=Dni;
					}
				}
			}
			//4th Celtic Imag Quasi Perpendicular / Heart
			else if (m_nFractalType == 37){
				if (antal<nMaxIter && test1 <= m_nBailout2){
					for (; antal<nMaxIter && test1 <= m_nBailout2; antal++){
						yr = dxr[antal] + Dr;
						yi = dxi[antal] + Di;
						test2 = test1;
						test1 = g_real*yr*yr + g_imag*yi*yi;
						if (test1<m_db_z[antal]){
							
								test1 = m_nBailout2 * 2;
							bGlitch = TRUE;
						}
						if(test1 > m_nBailout2)
							break;
						long double &x = dxr[antal];
						long double &y = dxi[antal];
						long double &a = Dr;
						long double &b = Di;
						long double &a0 = D0r;
						long double &b0 = D0i;
						long double x2 = x*x;
						long double y2 = y*y;
						long double a2 = a*a;
						long double b2 = b*b;
						long double xd = x+a;
						long double xd2 = xd*xd;
						long double yd = y+b;
						long double yd2 = yd*yd;

						Dnr = lb_abs_ldb(x2*x2 + y2*y2 - 6*x2*y2,4*y2*y*b-12*y2*a*x+6*y2*b2-6*y2*a2-12*x2*y*b-24*x*y*a*b+4*b2*b*y-12*b*y*a2+4*x2*x*a-6*x2*b2+6*x2*a2-12*b2*x*a+4*a2*a*x+b2*b2-6*b2*a2+a2*a2) +a0;
						Dni = 4 * a * _abs(- y2*y-3*b*y2+x2*y+2*a*x*y-3*b2*y+a2*y+b*x2+2*a*b*x-b2*b+a2*b) + 4 * x * lb_abs_ldb(- y2*y+x2*y,-3*b*y2+2*a*x*y-3*b2*y+a2*y+b*x2+2*a*b*x-b2*b+a2*b) + b0;	//func21

						Dr=Dnr;
						Di=Dni;
					}
				}
			}
			//4th Celtic Real Quasi Perpendicular
			else if (m_nFractalType == 38){
				if (antal<nMaxIter && test1 <= m_nBailout2){
					for (; antal<nMaxIter && test1 <= m_nBailout2; antal++){
						yr = dxr[antal] + Dr;
						yi = dxi[antal] + Di;
						test2 = test1;
						test1 = g_real*yr*yr + g_imag*yi*yi;
						if (test1<m_db_z[antal]){
							
								test1 = m_nBailout2 * 2;
							bGlitch = TRUE;
						}
						if(test1 > m_nBailout2)
							break;
						long double &x = dxr[antal];
						long double &y = dxi[antal];
						long double &a = Dr;
						long double &b = Di;
						long double &a0 = D0r;
						long double &b0 = D0i;
						long double x2 = x*x;
						long double y2 = y*y;
						long double a2 = a*a;
						long double b2 = b*b;
						long double xd = x+a;
						long double xd2 = xd*xd;
						long double yd = y+b;
						long double yd2 = yd*yd;

						Dnr = lb_abs_ldb(x2*x2 + y2*y2 - 6*x2*y2,4*y2*y*b-12*y2*a*x+6*y2*b2-6*y2*a2-12*x2*y*b-24*x*y*a*b+4*b2*b*y-12*b*y*a2+4*x2*x*a-6*x2*b2+6*x2*a2-12*b2*x*a+4*a2*a*x+b2*b2-6*b2*a2+a2*a2) +a0;
						Dni = -4*y*lb_abs_ldb(x2*x-x*y2,-a*y2-2*b*x*y-2*a*b*y+3*a*x2-b2*x+3*a2*x-a*b2+a2*a) - 4*b*_abs(- x*y2-a*y2-2*b*x*y-2*a*b*y+x2*x+3*a*x2-b2*x+3*a2*x-a*b2+a2*a) + b0;	//func22

						Dr=Dnr;
						Di=Dni;
					}
				}
			}
			//4th Celtic Real Quasi Heart
			else if (m_nFractalType == 39){
				if (antal<nMaxIter && test1 <= m_nBailout2){
					for (; antal<nMaxIter && test1 <= m_nBailout2; antal++){
						yr = dxr[antal] + Dr;
						yi = dxi[antal] + Di;
						test2 = test1;
						test1 = g_real*yr*yr + g_imag*yi*yi;
						if (test1<m_db_z[antal]){
							
								test1 = m_nBailout2 * 2;
							bGlitch = TRUE;
						}
						if(test1 > m_nBailout2)
							break;
						long double &x = dxr[antal];
						long double &y = dxi[antal];
						long double &a = Dr;
						long double &b = Di;
						long double &a0 = D0r;
						long double &b0 = D0i;
						long double x2 = x*x;
						long double y2 = y*y;
						long double a2 = a*a;
						long double b2 = b*b;
						long double xd = x+a;
						long double xd2 = xd*xd;
						long double yd = y+b;
						long double yd2 = yd*yd;

						Dnr = lb_abs_ldb(x2*x2 + y2*y2 - 6*x2*y2,4*y2*y*b-12*y2*a*x+6*y2*b2-6*y2*a2-12*x2*y*b-24*x*y*a*b+4*b2*b*y-12*b*y*a2+4*x2*x*a-6*x2*b2+6*x2*a2-12*b2*x*a+4*a2*a*x+b2*b2-6*b2*a2+a2*a2) +a0;
						Dni = 4*y*lb_abs_ldb(x2*x-x*y2,-a*y2-2*b*x*y-2*a*b*y+3*a*x2-b2*x+3*a2*x-a*b2+a2*a) + 4*b*_abs(- x*y2-a*y2-2*b*x*y-2*a*b*y+x2*x+3*a*x2-b2*x+3*a2*x-a*b2+a2*a) + b0;	//func22

						Dr=Dnr;
						Di=Dni;
					}
				}
			}
			//Cubic FT_Simon100A_plain
			else if (m_nFractalType == 40){
				if (antal<nMaxIter && test1 <= m_nBailout2){
					for (; antal<nMaxIter && test1 <= m_nBailout2; antal++){
						yr = dxr[antal] + Dr;
						yi = dxi[antal] + Di;
						test2 = test1;
						test1 = g_real*yr*yr + g_imag*yi*yi;
						if (test1<m_db_z[antal]){
							
								test1 = m_nBailout2 * 2;
							bGlitch = TRUE;
						}
						if(test1 > m_nBailout2)
							break;
						long double &x = dxr[antal];
						long double &y = dxi[antal];
						long double &a = Dr;
						long double &b = Di;
						long double &a0 = D0r;
						long double &b0 = D0i;
						long double x2 = x*x;
						long double y2 = y*y;
						long double a2 = a*a;
						long double b2 = b*b;
						long double xd = x+a;
						long double xd2 = xd*xd;
						long double yd = y+b;
						long double yd2 = yd*yd;

						Dnr= 3*lb_abs_ldb(x2*y,x2*b+2*x*a*y+2*x*a*b+a2*y+a2*b)*(y2*y) - lb_abs_ldb(y2*y,3*y2*b+3*y*b2+b2*b)*(y2*y) + 9*lb_abs_ldb(x*y2,2*x*y*b+x*b2+a*y2+2*a*y*b+a*b2)*(x*y2) - 3*lb_abs_ldb(x2*x,3*x2*a+3*x*a2+a2*a)*(x*y2) + 3*lb_abs_ldb(y2*y,3*y2*b+3*y*b2+b2*b)*(x2*y) - 9*lb_abs_ldb(x2*y,x2*b+2*x*a*y+2*x*a*b+a2*y+a2*b)*(x2*y) - 3*lb_abs_ldb(x*y2,2*x*y*b+x*b2+a*y2+2*a*y*b+a*b2)*(x2*x) + lb_abs_ldb(x2*x,3*x2*a+3*x*a2+a2*a)*(x2*x)
							+ 3*_abs(x2*y+x2*b+2*x*a*y+2*x*a*b+a2*y+a2*b)*(3*y2*b+3*y*b2+b2*b) - _abs(y2*y+3*y2*b+3*y*b2+b2*b)*(3*y2*b+3*y*b2+b2*b) + 9*_abs(x*y2+2*x*y*b+x*b2+a*y2+2*a*y*b+a*b2)*(2*x*y*b+x*b2+a*y2+2*a*y*b+a*b2) - 3*_abs(x2*x+3*x2*a+3*x*a2+a2*a)*(2*x*y*b+x*b2+a*y2+2*a*y*b+a*b2) + 3*_abs(y2*y+3*y2*b+3*y*b2+b2*b)*(x2*b+2*x*a*y+2*x*a*b+a2*y+a2*b) - 9*_abs(x2*y+x2*b+2*x*a*y+2*x*a*b+a2*y+a2*b)*(x2*b+2*x*a*y+2*x*a*b+a2*y+a2*b) - 3*_abs(x*y2+2*x*y*b+x*b2+a*y2+2*a*y*b+a*b2)*(3*x2*a+3*x*a2+a2*a) + _abs(x2*x+3*x2*a+3*x*a2+a2*a)*(3*x2*a+3*x*a2+a2*a)
							+ a0;
						Dni= 3*lb_abs_ldb(x*y2,2*x*y*b+x*b2+a*y2+2*a*y*b+a*b2)*(y2*y) - lb_abs_ldb(x2*x,3*x2*a+3*x*a2+a2*a)*(y2*y) - 9*lb_abs_ldb(x2*y,x2*b+2*x*a*y+2*x*a*b+a2*y+a2*b)*(x*y2) - 9*lb_abs_ldb(x*y2,2*x*y*b+x*b2+a*y2+2*a*y*b+a*b2)*(x2*y) + 3*lb_abs_ldb(x2*x,3*x2*a+3*x*a2+a2*a)*(x2*y) + 3*lb_abs_ldb(y2*y,3*y2*b+3*y*b2+b2*b)*(x*y2) - lb_abs_ldb(y2*y,3*y2*b+3*y*b2+b2*b)*(x2*x) + 3*lb_abs_ldb(x2*y,x2*b+2*x*a*y+2*x*a*b+a2*y+a2*b)*(x2*x)
							+ 3*_abs(x*y2+2*x*y*b+x*b2+a*y2+2*a*y*b+a*b2)*(3*y2*b+3*y*b2+b2*b) - _abs(x2*x+3*x2*a+3*x*a2+a2*a)*(3*y2*b+3*y*b2+b2*b) - 9*_abs(x2*y+x2*b+2*x*a*y+2*x*a*b+a2*y+a2*b)*(2*x*y*b+x*b2+a*y2+2*a*y*b+a*b2) - 9*_abs(x*y2+2*x*y*b+x*b2+a*y2+2*a*y*b+a*b2)*(x2*b+2*x*a*y+2*x*a*b+a2*y+a2*b) + 3*_abs(x2*x+3*x2*a+3*x*a2+a2*a)*(x2*b+2*x*a*y+2*x*a*b+a2*y+a2*b) + 3*_abs(y2*y+3*y2*b+3*y*b2+b2*b)*(2*x*y*b+x*b2+a*y2+2*a*y*b+a*b2) - _abs(y2*y+3*y2*b+3*y*b2+b2*b)*(3*x2*a+3*x*a2+a2*a) + 3*_abs(x2*y+x2*b+2*x*a*y+2*x*a*b+a2*y+a2*b)*(3*x2*a+3*x*a2+a2*a)
							+ b0;

						Dr=Dnr;
						Di=Dni;
					}
				}
			}
			//HPDZ Buffalo
			else if (m_nFractalType == 41){
				if (antal<nMaxIter && test1 <= m_nBailout2){
					for (; antal<nMaxIter && test1 <= m_nBailout2; antal++){
						yr = dxr[antal] + Dr;
						yi = dxi[antal] + Di;
						test2 = test1;
						test1 = g_real*yr*yr + g_imag*yi*yi;
						if (test1<m_db_z[antal]){
							test1 = m_nBailout2 * 2;
							bGlitch = TRUE;
						}
						if(test1 > m_nBailout2)
							break;
						long double &x = dxr[antal];
						long double &y = dxi[antal];
						long double &a = Dr;
						long double &b = Di;
						long double &a0 = D0r;
						long double &b0 = D0i;
						long double a2 = a*a;
						long double b2 = b*b;

						Dnr = (-2*b*y+2*a*x-b2+a2) - lb_abs_ldb(x,a) + a0;
						Dni = lb_abs_ldb(x*y,x*b+a*y+a*b) * 2.0 - lb_abs_ldb(y,b) + b0;

						Dr=Dnr;
						Di=Dni;
					}
				}
			}
			//TheRedshiftRider 1
			else if (m_nFractalType == 42){
                complex a(dbFactorAR,dbFactorAI);
				complex _2(2,0),_3(3,0);
				if (antal<nMaxIter && test1 <= m_nBailout2){
					for (; antal<nMaxIter && test1 <= m_nBailout2; antal++){
						yr = dxr[antal] + Dr;
						yi = dxi[antal] + Di;
						test2 = test1;
						test1 = g_real*yr*yr + g_imag*yi*yi;
						if (test1<m_db_z[antal]){
							test1 = m_nBailout2 * 2;
							bGlitch = TRUE;
						}
						if(test1 > m_nBailout2)
							break;
						long double &x = dxr[antal];
						long double &y = dxi[antal];

						complex z(x,y);
						complex d(Dr,Di);
						complex d2=d*d;
						complex d0(D0r,D0i);
						d = a*(_2*z*d+d2)+_3*z*z*d+_3*z*d2+d2*d + d0;

						Dr=d.m_r;
						Di=d.m_i;
					}
				}
			}
			//TheRedshiftRider 2
			else if (m_nFractalType == 43){
                complex a(dbFactorAR,dbFactorAI);
				complex _2(2,0),_3(3,0);
				if (antal<nMaxIter && test1 <= m_nBailout2){
					for (; antal<nMaxIter && test1 <= m_nBailout2; antal++){
						yr = dxr[antal] + Dr;
						yi = dxi[antal] + Di;
						test2 = test1;
						test1 = g_real*yr*yr + g_imag*yi*yi;
						if (test1<m_db_z[antal]){
							test1 = m_nBailout2 * 2;
							bGlitch = TRUE;
						}
						if(test1 > m_nBailout2)
							break;
						long double &x = dxr[antal];
						long double &y = dxi[antal];

						complex z(x,y);
						complex d(Dr,Di);
						complex d2=d*d;
						complex d0(D0r,D0i);
						d = a*(_2*z*d+d2)-_3*z*z*d-_3*z*d2-d2*d + d0;

						Dr=d.m_r;
						Di=d.m_i;
					}
				}
			}
			//TheRedshiftRider 3
			else if (m_nFractalType == 44){
				complex _2(2,0),_3(3,0),_4(4,0), a(2,0);
				if (antal<nMaxIter && test1 <= m_nBailout2){
					for (; antal<nMaxIter && test1 <= m_nBailout2; antal++){
						yr = dxr[antal] + Dr;
						yi = dxi[antal] + Di;
						test2 = test1;
						test1 = g_real*yr*yr + g_imag*yi*yi;
						if (test1<m_db_z[antal]){
							test1 = m_nBailout2 * 2;
							bGlitch = TRUE;
						}
						if(test1 > m_nBailout2)
							break;
						long double &x = dxr[antal];
						long double &y = dxi[antal];

						complex z(x,y);
						complex d(Dr,Di);
						complex d2=d*d;
						complex d0(D0r,D0i);
						d = a*(_2*z*d+d2)-_3*z*z*d-_3*z*d2-d2*d + d0;

						Dr=d.m_r;
						Di=d.m_i;
					}
				}
			}
			//TheRedshiftRider 4
			else if (m_nFractalType == 45){
                complex a(dbFactorAR,dbFactorAI);
				complex _2(2,0),_4(4,0),_6(6,0);
				if (antal<nMaxIter && test1 <= m_nBailout2){
					for (; antal<nMaxIter && test1 <= m_nBailout2; antal++){
						yr = dxr[antal] + Dr;
						yi = dxi[antal] + Di;
						test2 = test1;
						test1 = g_real*yr*yr + g_imag*yi*yi;
						if (test1<m_db_z[antal]){
							test1 = m_nBailout2 * 2;
							bGlitch = TRUE;
						}
						if(test1 > m_nBailout2)
							break;
						long double &x = dxr[antal];
						long double &y = dxi[antal];

						complex z(x,y);
						complex d(Dr,Di);
						complex d2=d*d;
						complex d0(D0r,D0i);
						d = a*(_2*z*d+d2)+_4*(z^3)*d+_6*(z^2)*d2+_4*z*(d2*d)+(d2^2) + d0;

						Dr=d.m_r;
						Di=d.m_i;
					}
				}
			}
			//TheRedshiftRider 5
			else if (m_nFractalType == 46){
                complex a(dbFactorAR,dbFactorAI);
				complex _2(2,0),_4(4,0),_6(6,0);
				if (antal<nMaxIter && test1 <= m_nBailout2){
					for (; antal<nMaxIter && test1 <= m_nBailout2; antal++){
						yr = dxr[antal] + Dr;
						yi = dxi[antal] + Di;
						test2 = test1;
						test1 = g_real*yr*yr + g_imag*yi*yi;
						if (test1<m_db_z[antal]){
							test1 = m_nBailout2 * 2;
							bGlitch = TRUE;
						}
						if(test1 > m_nBailout2)
							break;
						long double &x = dxr[antal];
						long double &y = dxi[antal];

						complex z(x,y);
						complex d(Dr,Di);
						complex d2=d*d;
						complex d0(D0r,D0i);
						d = a*(_2*z*d+d2)-_4*(z^3)*d-_6*(z^2)*d2-_4*z*d2*d-(d2^2) + d0;

						Dr=d.m_r;
						Di=d.m_i;
					}
				}
			}
			//TheRedshiftRider 6
			else if (m_nFractalType == 47){
                complex a(dbFactorAR,dbFactorAI);
				complex _2(2,0),_5(5,0),_10(10,0);
				if (antal<nMaxIter && test1 <= m_nBailout2){
					for (; antal<nMaxIter && test1 <= m_nBailout2; antal++){
						yr = dxr[antal] + Dr;
						yi = dxi[antal] + Di;
						test2 = test1;
						test1 = g_real*yr*yr + g_imag*yi*yi;
						if (test1<m_db_z[antal]){
							test1 = m_nBailout2 * 2;
							bGlitch = TRUE;
						}
						if(test1 > m_nBailout2)
							break;
						long double &x = dxr[antal];
						long double &y = dxi[antal];

						complex z(x,y);
						complex d(Dr,Di);
						complex d2=d*d;
						complex d0(D0r,D0i);
						d = a*(_2*z*d+d2)+_5*(z^4)*d+_10*(z^3)*d2+_10*(z^2)*d2*d+_5*z*(d2^2)+(d2*d2*d) + d0;

						Dr=d.m_r;
						Di=d.m_i;
					}
				}
			}
			//TheRedshiftRider 7
			else if (m_nFractalType == 48){
                complex a(dbFactorAR,dbFactorAI);
				complex _2(2,0),_5(5,0),_10(10,0);
				if (antal<nMaxIter && test1 <= m_nBailout2){
					for (; antal<nMaxIter && test1 <= m_nBailout2; antal++){
						yr = dxr[antal] + Dr;
						yi = dxi[antal] + Di;
						test2 = test1;
						test1 = g_real*yr*yr + g_imag*yi*yi;
						if (test1<m_db_z[antal]){
							test1 = m_nBailout2 * 2;
							bGlitch = TRUE;
						}
						if(test1 > m_nBailout2)
							break;
						long double &x = dxr[antal];
						long double &y = dxi[antal];

						complex z(x,y);
						complex d(Dr,Di);
						complex d2=d*d;
						complex d0(D0r,D0i);
						d = a*(_2*z*d+d2)-_5*(z^4)*d-_10*(z^3)*d2-_10*(z^2)*d2*d-_5*z*(d2^2)-(d2*d2*d) + d0;

						Dr=d.m_r;
						Di=d.m_i;
					}
				}
			}
			//TheRedshiftRider 8
			else if (m_nFractalType == 49){
                complex a(dbFactorAR,dbFactorAI);
				complex _2(2,0),_6(6,0),_15(15,0),_20(20,0);
				if (antal<nMaxIter && test1 <= m_nBailout2){
					for (; antal<nMaxIter && test1 <= m_nBailout2; antal++){
						yr = dxr[antal] + Dr;
						yi = dxi[antal] + Di;
						test2 = test1;
						test1 = g_real*yr*yr + g_imag*yi*yi;
						if (test1<m_db_z[antal]){
							test1 = m_nBailout2 * 2;
							bGlitch = TRUE;
						}
						if(test1 > m_nBailout2)
							break;
						long double &x = dxr[antal];
						long double &y = dxi[antal];


						complex z(x,y);
						complex d(Dr,Di);
						complex d2=d*d;
						complex d0(D0r,D0i);
						d = a*(_2*z*d+d2)+_6*(z^5)*d+_15*(z^4)*d2+_20*(z^3)*(d2*d)+_15*(z^2)*(d2^2)+_6*z*(d2*d2*d)+(d2^3) + d0;

						Dr=d.m_r;
						Di=d.m_i;
					}
				}
			}
			//TheRedshiftRider 9
			else if (m_nFractalType == 50){
                complex a(dbFactorAR,dbFactorAI);
				complex _2(2,0),_6(6,0),_15(15,0),_20(20,0);
				if (antal<nMaxIter && test1 <= m_nBailout2){
					for (; antal<nMaxIter && test1 <= m_nBailout2; antal++){
						yr = dxr[antal] + Dr;
						yi = dxi[antal] + Di;
						test2 = test1;
						test1 = g_real*yr*yr + g_imag*yi*yi;
						if (test1<m_db_z[antal]){
							test1 = m_nBailout2 * 2;
							bGlitch = TRUE;
						}
						if(test1 > m_nBailout2)
							break;
						long double &x = dxr[antal];
						long double &y = dxi[antal];

						complex z(x,y);
						complex d(Dr,Di);
						complex d0(D0r,D0i);
						d = a*(_2*z*d+(d^2))-_6*(z^5)*d-_15*(z^4)*(d^2)-_20*(z^3)*(d^3)-_15*(z^2)*(d^4)-_6*z*(d^5)-(d^6) + d0;

						Dr=d.m_r;
						Di=d.m_i;
					}
				}
			}
			// SimonBrot2 4th
			else if (m_nFractalType == 51){
				if (antal<nMaxIter && test1 <= m_nBailout2){
					for (; antal<nMaxIter && test1 <= m_nBailout2; antal++){
						yr = dxr[antal] + Dr;
						yi = dxi[antal] + Di;
						test2 = test1;
						test1 = g_real*yr*yr + g_imag*yi*yi;
						if (test1<m_db_z[antal]){
							test1 = m_nBailout2 * 2;
							bGlitch = TRUE;
						}
						if(test1 > m_nBailout2)
							break;
						long double &x = dxr[antal];
						long double &y = dxi[antal];
						long double &a = Dr;
						long double &b = Di;
						long double &a0 = D0r;
						long double &b0 = D0i;
						long double x2 = x*x;
						long double y2 = y*y;
						long double a2 = a*a;
						long double b2 = b*b;

						//d = (z*z)*(z*z+_2*z*d+d*d).abs() + (_2*z*d+d*d)*(z*z+_2*z*d+d*d).abs() - (z*z)*(z*z).abs() + d0;
						//Dr=d.m_r;
						//Di=d.m_i;
						Dnr=(x2-y2)*lb_abs_ldb(x2-y2,+2*x*a+a2-2*y*b-b2) + (2*x*a+a2-2*y*b-b2)*_abs(x2+2*x*a+a2-y2-2*y*b-b2) 
							- (2*x*y)*lb_abs_ldb(2*x*y,2*x*b+2*a*y+2*a*b) - (2*x*b+2*a*y+2*a*b)*_abs(2*x*y+2*x*b+2*a*y+2*a*b)
							+ a0;
						Dni=(x2-y2)*lb_abs_ldb(2*x*y,2*x*b+2*a*y+2*a*b) + (2*x*a+a2-2*y*b-b2)*_abs(2*x*y+2*x*b+2*a*y+2*a*b) 
							+ (2*x*y)*lb_abs_ldb(x2-y2,2*x*a+a2-2*y*b-b2) + (2*x*b+2*a*y+2*a*b)*_abs(x2-y2+2*x*a+a2-2*y*b-b2) 
							+ b0;
						Dr=Dnr;
						Di=Dni;
					}
				}
			}
	return antal;
}

