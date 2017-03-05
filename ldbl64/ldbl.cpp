/* Replace "dll.h" with the name of your header */
#include <windows.h>
#include <stdio.h>
#include <float.h>
#include <math.h>
#define _GCC_
#include "C:\pojects\ltest\fraktal_sft\floatexp.h"
//#include "C:\pojects\ltest\fraktal_sft\ldbl_exp.h"
#define TERM4
#define TERM5
#define TERM6
#define TERM7
#define _abs(a) (a>0?a:-a)

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
extern "C" __declspec(dllexport) int SizeOfLD()
{
       return sizeof(long double);
}
extern "C" __declspec(dllexport) void *AllocateArray(int nSize)
{
       long double *ret = new long double[nSize];
       return ret;
}
extern "C" __declspec(dllexport) void ReleaseArray(void *p)
{
       long double *del = (long double *)p;
       delete [] del;
}
extern "C" __declspec(dllexport) void AssignInt(void *p,int nValue)
{
       *((long double*)p) = nValue;
}
extern "C" __declspec(dllexport) void AssignDouble(void *p,double nDouble)
{
       *((long double*)p) = nDouble;
}
extern "C" __declspec(dllexport) void AssignLD(void *p,void *ld)
{
       *((long double*)p) = *((long double*)ld);
}
extern "C" __declspec(dllexport) void ToInt(void *p,int *pnValue)
{
       *pnValue = (int)*((long double*)p);
}
extern "C" __declspec(dllexport) void ToDouble(void *p,double *pnDouble)
{
       *pnDouble = *((long double*)p);
}
extern "C" __declspec(dllexport) void ToFloatExp(void *p,floatexp *pnFloatExp)
{
       pnFloatExp->setLongDouble(*((long double*)p));
}
extern "C" __declspec(dllexport) void AssignFloatExp(void *p,floatexp *fe)
{
	*((long double*)p) = fe->toLongDouble();
}

extern "C" __declspec(dllexport) void Multiply(void *a,void *b,void *ret)
{
       (*((long double*)ret)) = (*((long double*)a)) * (*((long double*)b));
}
extern "C" __declspec(dllexport) double SquareAdd(void *a,void *b)
{
       return (*((long double*)a)) * (*((long double*)a)) + (*((long double*)b)) * (*((long double*)b));
}
extern "C" __declspec(dllexport) void Divide(void *a,void *b,void *ret)
{
       (*((long double*)ret)) = (*((long double*)a)) / (*((long double*)b));
}
extern "C" __declspec(dllexport) void Add(void *a,void *b,void *ret)
{
       (*((long double*)ret)) = (*((long double*)a)) + (*((long double*)b));
}
extern "C" __declspec(dllexport) void Subtract(void *a,void *b,void *ret)
{
       (*((long double*)ret)) = (*((long double*)a)) - (*((long double*)b));
}
extern "C" __declspec(dllexport) void Negative(void *a)
{
       (*((long double*)a)) = -(*((long double*)a));
}
extern "C" __declspec(dllexport) int GT(void *a,void *b)
{
       return (*((long double*)a)) > (*((long double*)b));
}
extern "C" __declspec(dllexport) int LT(void *a,void *b)
{
       return (*((long double*)a)) < (*((long double*)b));
}
extern "C" __declspec(dllexport) int Equal(void *a,void *b)
{
       return (*((long double*)a)) == (*((long double*)b));
}
extern "C" __declspec(dllexport) void Print(void *a,char *szRet)
{
       sprintf(szRet,"%ld",*((long double*)a));
}
extern "C" __declspec(dllexport) int Version()
{
	return 7;
}

#define	FIXEDFLOAT_TYPE __int64
#define FIXEDFLOAT_DIGITS 8
#define FIXEDFLOAT_PARTMAX (FIXEDFLOAT_TYPE)100000000

extern "C" __declspec(dllexport) void ConvertFromFixedFloat(void *p,int nValues, __int64 *pValues, BOOL bSign)
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

extern "C" __declspec(dllexport) int Perturbation4(int antal,void *pdxr,void *pdxi, void* pDr, void*pDi, void* pD0r, void*pD0i,double *ptest1, double *ptest2, int m_nBailout2, int m_nMaxIter,double *m_db_z,BOOL *pGlitch)
{
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
	if(antal<m_nMaxIter && test1 <= m_nBailout2){
		for(;antal<m_nMaxIter;antal++){
			yr=dxr[antal]+Dr;
			yi=dxi[antal]+Di;
			test2=test1;
			test1 = yr*yr+yi*yi;
		    if(test1<m_db_z[antal]){
                test1 = m_nBailout2*2;
	      		bGlitch=TRUE;
		    }
			if(test1 > m_nBailout2)
				break;
			Dnr = (2*dxr[antal] + Dr)*Dr - (2*dxi[antal] + Di)*Di + D0r;
			Dni = 2*((dxr[antal] + Dr)*Di + dxi[antal]*Dr) + D0i;
			Di = Dni;
			Dr = Dnr;
		}
	}
	return antal;
}
extern "C" __declspec(dllexport) int Perturbation_3rd(int antal,void *pdxr,void *pdxi, void* pDr, void*pDi, void* pD0r, void*pD0i,double *ptest1, double *ptest2, int m_nBailout2, int m_nMaxIter,double *m_db_z,BOOL *pGlitch)
{
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
	if(antal<m_nMaxIter && test1 <= m_nBailout2){
		for(;antal<m_nMaxIter;antal++){
			yr=dxr[antal]+Dr;
			yi=dxi[antal]+Di;
			test2=test1;
			test1 = yr*yr+yi*yi;
		    if(test1<m_db_z[antal]){
                test1 = m_nBailout2*2;
	      		bGlitch=TRUE;
		    }
			if(test1 > m_nBailout2)
				break;
			Dnr = 3*dxr[antal]*dxr[antal]*Dr - 6*dxr[antal]*dxi[antal]*Di - 3*dxi[antal]*dxi[antal]*Dr + 3*dxr[antal]*Dr*Dr - 3*dxr[antal]*Di*Di - 3*dxi[antal]*2*Dr*Di + Dr*Dr*Dr - 3*Dr*Di*Di + D0r;
			Dni = 3*dxr[antal]*dxr[antal]*Di + 6*dxr[antal]*dxi[antal]*Dr - 3*dxi[antal]*dxi[antal]*Di + 3*dxr[antal]*2*Dr*Di + 3*dxi[antal]*Dr*Dr - 3*dxi[antal]*Di*Di + 3*Dr*Dr*Di - Di*Di*Di + D0i;
			
			//Dnr = (2*dxr[antal] + Dr)*Dr - (2*dxi[antal] + Di)*Di + D0r;
			//Dni = 2*((dxr[antal] + Dr)*Di + dxi[antal]*Dr) + D0i;
			Di = Dni;
			Dr = Dnr;
		}
	}
	return antal;
}
extern "C" __declspec(dllexport) int Perturbation_4th(int antal,void *pdxr,void *pdxi, void* pDr, void*pDi, void* pD0r, void*pD0i,double *ptest1, double *ptest2, int m_nBailout2, int m_nMaxIter,double *m_db_z,BOOL *pGlitch)
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
			test1 = yr*yr+yi*yi;
		    if(test1<m_db_z[antal]){
                test1 = m_nBailout2*2;
	      		bGlitch=TRUE;
		    }
			if(test1 > m_nBailout2)
				break;
			complex X(dxr[antal],dxi[antal]);
			complex D(Dr,Di);
			complex D0(D0r,D0i);
			complex _4(4,0), _6(6,0);
			complex Dn = _4*(X^3)*D + _6*(X^2)*(D^2) + _4*X*(D^3) + (D^4) + D0;
			Di = Dn.m_i;
			Dr = Dn.m_r;
		}
	}
	return antal;
}
extern "C" __declspec(dllexport) int Perturbation_5th(int antal,void *pdxr,void *pdxi, void* pDr, void*pDi, void* pD0r, void*pD0i,double *ptest1, double *ptest2, int m_nBailout2, int m_nMaxIter,double *m_db_z,BOOL *pGlitch)
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
			test1 = yr*yr+yi*yi;
		    if(test1<m_db_z[antal]){
                test1 = m_nBailout2*2;
	      		bGlitch=TRUE;
		    }
			if(test1 > m_nBailout2)
				break;
			complex X(dxr[antal],dxi[antal]);
			complex D(Dr,Di);
			complex D0(D0r,D0i);
			complex _5(5,0), _10(10,0);
			complex Dn = _5*(X^4)*D + _10*(X^3)*(D^2) + _10*(X^2)*(D^3) + _5*X*(D^4) + (D^5) + D0;
			Di = Dn.m_i;
			Dr = Dn.m_r;
		}
	}
	return antal;
}
extern "C" __declspec(dllexport) int Perturbation_6th(int antal,void *pdxr,void *pdxi, void* pDr, void*pDi, void* pD0r, void*pD0i,double *ptest1, double *ptest2, int m_nBailout2, int m_nMaxIter,double *m_db_z,BOOL *pGlitch)
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
			test1 = yr*yr+yi*yi;
		    if(test1<m_db_z[antal]){
                test1 = m_nBailout2*2;
	      		bGlitch=TRUE;
		    }
			if(test1 > m_nBailout2)
				break;
			complex X(dxr[antal],dxi[antal]);
			complex D(Dr,Di);
			complex D0(D0r,D0i);
      		complex _6(6,0), _15(15,0), _20(20,0);
			complex Dn = _6*(X^5)*D + _15*(X^4)*(D^2) + _20*(X^3)*(D^3) + _15*(X^2)*(D^4) + _6*X*(D^5) + (D^6) + D0;
			Di = Dn.m_i;
			Dr = Dn.m_r;
		}
	}
	return antal;
}
extern "C" __declspec(dllexport) int Perturbation_7th(int antal,void *pdxr,void *pdxi, void* pDr, void*pDi, void* pD0r, void*pD0i,double *ptest1, double *ptest2, int m_nBailout2, int m_nMaxIter,double *m_db_z,BOOL *pGlitch)
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
			test1 = yr*yr+yi*yi;
		    if(test1<m_db_z[antal]){
                test1 = m_nBailout2*2;
	      		bGlitch=TRUE;
		    }
			if(test1 > m_nBailout2)
				break;
			complex X(dxr[antal],dxi[antal]);
			complex D(Dr,Di);
			complex D0(D0r,D0i);
			complex _7(7,0), _21(21,0), _35(35,0);
			complex Dn = _7*(X^6)*D + _21*(X^5)*(D^2) + _35*(X^4)*(D^3) + _35*(X^3)*(D^4) + _21*(X^2)*(D^5) + _7*X*(D^6) + (D^7) + D0;
			Di = Dn.m_i;
			Dr = Dn.m_r;
		}
	}
	return antal;
}
extern "C" __declspec(dllexport) int Perturbation_8th(int antal,void *pdxr,void *pdxi, void* pDr, void*pDi, void* pD0r, void*pD0i,double *ptest1, double *ptest2, int m_nBailout2, int m_nMaxIter,double *m_db_z,BOOL *pGlitch)
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
			test1 = yr*yr+yi*yi;
		    if(test1<m_db_z[antal]){
                test1 = m_nBailout2*2;
	      		bGlitch=TRUE;
		    }
			if(test1 > m_nBailout2)
				break;
			complex X(dxr[antal],dxi[antal]);
			complex D(Dr,Di);
			complex D0(D0r,D0i);
			complex _8(8,0), _28(28,0), _56(56,0), _70(70,0);
			complex Dn = _8*(X^7)*D + _28*(X^6)*(D^2) + _56*(X^5)*(D^3) + _70*(X^4)*(D^4) + _56*(X^3)*(D^5) + _28*(X^2)*(D^6) + _8*X*(D^7) + (D^8) + D0;
			Di = Dn.m_i;
			Dr = Dn.m_r;
		}
	}
	return antal;
}
extern "C" __declspec(dllexport) int Perturbation_9th(int antal,void *pdxr,void *pdxi, void* pDr, void*pDi, void* pD0r, void*pD0i,double *ptest1, double *ptest2, int m_nBailout2, int m_nMaxIter,double *m_db_z,BOOL *pGlitch)
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
			test1 = yr*yr+yi*yi;
		    if(test1<m_db_z[antal]){
                test1 = m_nBailout2*2;
	      		bGlitch=TRUE;
		    }
			if(test1 > m_nBailout2)
				break;
			complex X(dxr[antal],dxi[antal]);
			complex D(Dr,Di);
			complex D0(D0r,D0i);
			complex _9(9,0), _36(36,0), _84(84,0), _126(126,0);
			complex Dn = _9*(X^8)*D + _36*(X^7)*(D^2) + _84*(X^6)*(D^3) + _126*(X^5)*(D^4) + _126*(X^4)*(D^5) + _84*(X^3)*(D^6) + _36*(X^2)*(D^7) + _9*X*(D^8) + (D^9) + D0;
			Di = Dn.m_i;
			Dr = Dn.m_r;
		}
	}
	return antal;
}
extern "C" __declspec(dllexport) int Perturbation_10th(int antal,void *pdxr,void *pdxi, void* pDr, void*pDi, void* pD0r, void*pD0i,double *ptest1, double *ptest2, int m_nBailout2, int m_nMaxIter,double *m_db_z,BOOL *pGlitch)
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
			test1 = yr*yr+yi*yi;
		    if(test1<m_db_z[antal]){
                test1 = m_nBailout2*2;
	      		bGlitch=TRUE;
		    }
			if(test1 > m_nBailout2)
				break;
			complex X(dxr[antal],dxi[antal]);
			complex D(Dr,Di);
			complex D0(D0r,D0i);
			complex _10(10,0), _45(45,0), _120(120,0), _210(210,0), _252(252,0);
			complex Dn = _10*(X^9)*D + _45*(X^8)*(D^2) + _120*(X^7)*(D^3) + _210*(X^6)*(D^4) + _252*(X^5)*(D^5) + _210*(X^4)*(D^6) + _120*(X^3)*(D^7) + _45*(X^2)*(D^8) + _10*X*(D^9) + (D^10) + D0;
			Di = Dn.m_i;
			Dr = Dn.m_r;
		}
	}
	return antal;
}

extern "C" __declspec(dllexport) int Perturbation_Var(int antal,void *pdxr,void *pdxi, void* pDr, void*pDi, void* pD0r, void*pD0i,double *ptest1, double *ptest2, int m_nBailout2, int m_nMaxIter,double *m_db_z,BOOL *pGlitch,int m_nPower,int *m_pnExpConsts)
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
			test1 = yr*yr+yi*yi;
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

extern "C" __declspec(dllexport) int BurningShip(int antal,void *pdxr,void *pdxi, void* pDr, void*pDi, void* pD0r, void*pD0i,double *ptest1, double *ptest2, int m_nBailout2, int m_nMaxIter,double *m_db_z,BOOL *pGlitch)
{
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
	if(antal<m_nMaxIter && test1 <= m_nBailout2){
		for(;antal<m_nMaxIter;antal++){
			yr=dxr[antal]+Dr;
			yi=dxi[antal]+Di;
			test2=test1;
			test1 = yr*yr+yi*yi;
		    if(test1<m_db_z[antal]){
                test1 = m_nBailout2*2;
	      		bGlitch=TRUE;
		    }
			if(test1 > m_nBailout2)
				break;
			Dnr = 2*Dr*dxr[antal] + Dr*Dr - 2*dxi[antal]*Di - Di*Di + D0r;
			long double c = dxr[antal]*dxi[antal];
			long double d = dxr[antal]*Di + Dr*dxi[antal] + Dr*Di;
			if(c>0){
				if(c+d>0)
					Dni = d;
				else if(d==-c)
					Dni = d;
				else if(d<-c)
					Dni = -d-2*c;
			}
			else if (c==0)
				Dni = _abs(d);
			else if (c < 0){
				if (c+d>0)
					Dni = d + 2*c;
				else if (d == -c)
					Dni = -d;
				else if (d < -c)
					Dni = -d;
			}
			Dni = 2*Dni+D0i;

			Di = Dni;
			Dr = Dnr;
		}
	}
	return antal;
}

extern "C" __declspec(dllexport) int BurningShip3(int antal,void *pdxr,void *pdxi, void* pDr, void*pDi, void* pD0r, void*pD0i,double *ptest1, double *ptest2, int m_nBailout2, int m_nMaxIter,double *m_db_z,BOOL *pGlitch)
{
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
	if(antal<m_nMaxIter && test1 <= m_nBailout2){
		for(;antal<m_nMaxIter;antal++){
			yr=dxr[antal]+Dr;
			yi=dxi[antal]+Di;
			test2=test1;
			test1 = yr*yr+yi*yi;
		    if(test1<m_db_z[antal]){
                test1 = m_nBailout2*2;
	      		bGlitch=TRUE;
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

						if(r>0){
							if(r+a>0)
								Dnr = a;
							else if(a==-r)
								Dnr = a;
							else if(a<-r)
								Dnr = -a-2*r;
						}
						else if (r==0)
							Dnr = _abs(a);
						else if (r < 0){
							if (r+a>0)
								Dnr = a + 2*r;
							else if (a == -r)
								Dnr = -a;
							else if (a < -r)
								Dnr = -a;
						}
						ab = r+a;
						Dnr = (r2-3*i2) * Dnr + (2*ar+a2-6*ib-3*b2)*_abs(ab) + a0;

						if(i>0){
							if(i+b>0)
								Dni = b;
							else if(b==-i)
								Dni = b;
							else if(b<-i)
								Dni = -b-2*i;
						}
						else if (i==0)
							Dni = _abs(b);
						else if (i < 0){
							if (i+b>0)
								Dni = b + 2*i;
							else if (b == -i)
								Dni = -b;
							else if (b < -i)
								Dni = -b;
						}
						ab = i+b;
						Dni = (3*r2 - i2) * Dni + (6*ar+3*a2-2*ib-b2) * _abs(ab) + b0;

						Di = Dni;
						Dr = Dnr;
		}
	}
	return antal;
}

extern "C" __declspec(dllexport) int Celtic(int antal,void *pdxr,void *pdxi, void* pDr, void*pDi, void* pD0r, void*pD0i,double *ptest1, double *ptest2, int m_nBailout2, int m_nMaxIter,double *m_db_z,BOOL *pGlitch)
{
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
	if(antal<m_nMaxIter && test1 <= m_nBailout2){
		for(;antal<m_nMaxIter;antal++){
			yr=dxr[antal]+Dr;
			yi=dxi[antal]+Di;
			test2=test1;
			test1 = yr*yr+yi*yi;
		    if(test1<m_db_z[antal]){
                test1 = m_nBailout2*2;
	      		bGlitch=TRUE;
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
						long double d = 2*r*a + a*a - 2*i*b - b*b;
						if(c>0){
							if(c+d>0)
								Dnr = d;
							else if(d==-c)
								Dnr = d;
							else if(d<-c)
								Dnr = -d-2*c;
						}
						else if (c==0)
							Dnr = _abs(d);
						else if (c < 0){
							if (c+d>0)
								Dnr = d + 2*c;
							else if (d == -c)
								Dnr = -d;
							else if (d < -c)
								Dnr = -d;
						}
						Dnr = Dnr + a0;

						Dni = 2*r*b + 2*a*i + 2*a*b + b0;


						Di = Dni;
						Dr = Dnr;
		}
	}
	return antal;
}

extern "C" __declspec(dllexport) int Celtic3(int antal,void *pdxr,void *pdxi, void* pDr, void*pDi, void* pD0r, void*pD0i,double *ptest1, double *ptest2, int m_nBailout2, int m_nMaxIter,double *m_db_z,BOOL *pGlitch)
{
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
	if(antal<m_nMaxIter && test1 <= m_nBailout2){
		for(;antal<m_nMaxIter;antal++){
			yr=dxr[antal]+Dr;
			yi=dxi[antal]+Di;
			test2=test1;
			test1 = yr*yr+yi*yi;
		    if(test1<m_db_z[antal]){
                test1 = m_nBailout2*2;
	      		bGlitch=TRUE;
		    }
			if(test1 > m_nBailout2)
				break;
						long double &r = dxr[antal];
						long double &i = dxi[antal];
						long double &a = Dr;
						long double &b = Di;
						long double &a0 = D0r;
						long double &b0 = D0i;
						long double r2=r*r;
						long double i2=i*i;
						long double a2=a*a;
						long double b2=b*b;

						long double c = r*(r2 - 3*i2);
						long double d = a*(3*r2 + a2) + 3*r*(a2 - 2*i*b - b2) - 3*a*(i2 + 2*i*b + b2);
						if(c>0){
							if(c+d>0)
								Dnr = d;
							else if(d==-c)
								Dnr = d;
							else if(d<-c)
								Dnr = -d-2*c;
						}
						else if (c==0)
							Dnr = _abs(d);
						else if (c < 0){
							if (c+d>0)
								Dnr = d + 2*c;
							else if (d == -c)
								Dnr = -d;
							else if (d < -c)
								Dnr = -d;
						}
						Dnr = Dnr + a0;

						Dni = 3*i*(2*r*a + a2 - b2) + 3*b*(r2 + 2*r*a + a2) - b*(b2 + 3*i2) + b0;


						Di = Dni;
						Dr = Dnr;
		}
	}
	return antal;
}

extern "C" __declspec(dllexport) int Buffalo(int antal,void *pdxr,void *pdxi, void* pDr, void*pDi, void* pD0r, void*pD0i,double *ptest1, double *ptest2, int m_nBailout2, int m_nMaxIter,double *m_db_z,BOOL *pGlitch)
{
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
	if(antal<m_nMaxIter && test1 <= m_nBailout2){
		for(;antal<m_nMaxIter;antal++){
			yr=dxr[antal]+Dr;
			yi=dxi[antal]+Di;
			test2=test1;
			test1 = yr*yr+yi*yi;
		    if(test1<m_db_z[antal]){
                test1 = m_nBailout2*2;
	      		bGlitch=TRUE;
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
						long double d = 2*r*a + a*a - 2*i*b - b*b;
						if(c>0){
							if(c+d>0)
								Dnr = d;
							else if(d==-c)
								Dnr = d;
							else if(d<-c)
								Dnr = -d-2*c;
						}
						else if (c==0)
							Dnr = _abs(d);
						else if (c < 0){
							if (c+d>0)
								Dnr = d + 2*c;
							else if (d == -c)
								Dnr = -d;
							else if (d < -c)
								Dnr = -d;
						}
						Dnr = Dnr + a0;

						c = r*i;
						d = r*b+a*i+a*b;
						if(c>0){
							if(c+d>0)
								Dni = d;
							else if(d==-c)
								Dni = d;
							else if(d<-c)
								Dni = -d-2*c;
						}
						else if (c==0)
							Dni = _abs(d);
						else if (c < 0){
							if (c+d>0)
								Dni = d + 2*c;
							else if (d == -c)
								Dni = -d;
							else if (d < -c)
								Dni = -d;
						}
						Dni = b0 - 2*Dni;


						Di = Dni;
						Dr = Dnr;
		}
	}
	return antal;
}

extern "C" __declspec(dllexport) int CubicBuffalo(int antal,void *pdxr,void *pdxi, void* pDr, void*pDi, void* pD0r, void*pD0i,double *ptest1, double *ptest2, int m_nBailout2, int m_nMaxIter,double *m_db_z,BOOL *pGlitch)
{
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
	if(antal<m_nMaxIter && test1 <= m_nBailout2){
		for(;antal<m_nMaxIter;antal++){
			yr=dxr[antal]+Dr;
			yi=dxi[antal]+Di;
			test2=test1;
			test1 = yr*yr+yi*yi;
		    if(test1<m_db_z[antal]){
                test1 = m_nBailout2*2;
	      		bGlitch=TRUE;
		    }
			if(test1 > m_nBailout2)
				break;
						long double &r = dxr[antal];
						long double &i = dxi[antal];
						long double &a = Dr;
						long double &b = Di;
						long double &a0 = D0r;
						long double &b0 = D0i;
						long double r2=r*r;
						long double i2=i*i;
						long double a2=a*a;
						long double b2=b*b;

						long double c = r*(r2-3*i2);
						long double d = a*(3*r2 + a2) + 3*r*(a2 - 2*i*b - b2) - 3*a*(i2 + 2*i*b + b2);
						if(c>0){
							if(c+d>0)
								Dnr = d;
							else if(d==-c)
								Dnr = d;
							else if(d<-c)
								Dnr = -d-2*c;
						}
						else if (c==0)
							Dnr = _abs(d);
						else if (c < 0){
							if (c+d>0)
								Dnr = d + 2*c;
							else if (d == -c)
								Dnr = -d;
							else if (d < -c)
								Dnr = -d;
						}
						Dnr = Dnr + a0;

						c = i*(3*r2-i2);
						d = 3*i*(2*r*a + a2 - b2) + 3*b*(r2 + 2*r*a + a2) - b*(3*i2 + b2);
						if(c>0){
							if(c+d>0)
								Dni = d;
							else if(d==-c)
								Dni = d;
							else if(d<-c)
								Dni = -d-2*c;
						}
						else if (c==0)
							Dni = _abs(d);
						else if (c < 0){
							if (c+d>0)
								Dni = d + 2*c;
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
	return antal;
}

extern "C" __declspec(dllexport) int Mandelbar(int antal,void *pdxr,void *pdxi, void* pDr, void*pDi, void* pD0r, void*pD0i,double *ptest1, double *ptest2, int m_nBailout2, int m_nMaxIter,double *m_db_z,BOOL *pGlitch)
{
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
	if(antal<m_nMaxIter && test1 <= m_nBailout2){
		for(;antal<m_nMaxIter;antal++){
			yr=dxr[antal]+Dr;
			yi=dxi[antal]+Di;
			test2=test1;
			test1 = yr*yr+yi*yi;
		    if(test1<m_db_z[antal]){
                test1 = m_nBailout2*2;
	      		bGlitch=TRUE;
		    }
			if(test1 > m_nBailout2)
				break;
						long double &r = dxr[antal];
						long double &i = dxi[antal];
						long double &a = Dr;
						long double &b = Di;
						long double &a0 = D0r;
						long double &b0 = D0i;

						Dnr = 2*r*a + a*a - b*b - 2*b*i + a0;
						Dni = (r*b + a*i + a*b)*-2 + b0;

						Di = Dni;
						Dr = Dnr;
		}
	}
	return antal;
}

extern "C" __declspec(dllexport) int MandelbarCeltic(int antal,void *pdxr,void *pdxi, void* pDr, void*pDi, void* pD0r, void*pD0i,double *ptest1, double *ptest2, int m_nBailout2, int m_nMaxIter,double *m_db_z,BOOL *pGlitch)
{
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
	if(antal<m_nMaxIter && test1 <= m_nBailout2){
		for(;antal<m_nMaxIter;antal++){
			yr=dxr[antal]+Dr;
			yi=dxi[antal]+Di;
			test2=test1;
			test1 = yr*yr+yi*yi;
		    if(test1<m_db_z[antal]){
                test1 = m_nBailout2*2;
	      		bGlitch=TRUE;
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
						long double d = 2*r*a + a*a - 2*i*b - b*b;
						if(c>0){
							if(c+d>0)
								Dnr = d;
							else if(d==-c)
								Dnr = d;
							else if(d<-c)
								Dnr = -d-2*c;
						}
						else if (c==0)
							Dnr = _abs(d);
						else if (c < 0){
							if (c+d>0)
								Dnr = d + 2*c;
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
	return antal;
}

extern "C" __declspec(dllexport) int PerpendicularMandelbrot(int antal,void *pdxr,void *pdxi, void* pDr, void*pDi, void* pD0r, void*pD0i,double *ptest1, double *ptest2, int m_nBailout2, int m_nMaxIter,double *m_db_z,BOOL *pGlitch)
{
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
	if(antal<m_nMaxIter && test1 <= m_nBailout2){
		for(;antal<m_nMaxIter;antal++){
			yr=dxr[antal]+Dr;
			yi=dxi[antal]+Di;
			test2=test1;
			test1 = yr*yr+yi*yi;
		    if(test1<m_db_z[antal]){
                test1 = m_nBailout2*2;
	      		bGlitch=TRUE;
		    }
			if(test1 > m_nBailout2)
				break;
						long double &r = dxr[antal];
						long double &i = dxi[antal];
						long double &a = Dr;
						long double &b = Di;
						long double &a0 = D0r;
						long double &b0 = D0i;

						Dnr = 2*r*a + a*a - b*b - 2*b*i + a0;

						long double c = r;
						long double d = a;
						if(c>0){
							if(c+d>0)
								Dni = d;
							else if(d==-c)
								Dni = d;
							else if(d<-c)
								Dni = -d-2*c;
						}
						else if (c==0)
							Dni = _abs(d);
						else if (c < 0){
							if (c+d>0)
								Dni = d + 2*c;
							else if (d == -c)
								Dni = -d;
							else if (d < -c)
								Dni = -d;
						}
						Dni = _abs((r+a))*b*-2 + Dni*i*-2 + b0;

						Di = Dni;
						Dr = Dnr;
		}
	}
	return antal;
}

extern "C" __declspec(dllexport) int PerpendicularBurningShip(int antal,void *pdxr,void *pdxi, void* pDr, void*pDi, void* pD0r, void*pD0i,double *ptest1, double *ptest2, int m_nBailout2, int m_nMaxIter,double *m_db_z,BOOL *pGlitch)
{
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
	if(antal<m_nMaxIter && test1 <= m_nBailout2){
		for(;antal<m_nMaxIter;antal++){
			yr=dxr[antal]+Dr;
			yi=dxi[antal]+Di;
			test2=test1;
			test1 = yr*yr+yi*yi;
		    if(test1<m_db_z[antal]){
                test1 = m_nBailout2*2;
	      		bGlitch=TRUE;
		    }
			if(test1 > m_nBailout2)
				break;
						long double &r = dxr[antal];
						long double &i = dxi[antal];
						long double &a = Dr;
						long double &b = Di;
						long double &a0 = D0r;
						long double &b0 = D0i;

						Dnr = 2*r*a + a*a - b*b - 2*b*i + a0;

						long double c = i;
						long double d = b;
						if(c>0){
							if(c+d>0)
								Dni = d;
							else if(d==-c)
								Dni = d;
							else if(d<-c)
								Dni = -d-2*c;
						}
						else if (c==0)
							Dni = _abs(d);
						else if (c < 0){
							if (c+d>0)
								Dni = d + 2*c;
							else if (d == -c)
								Dni = -d;
							else if (d < -c)
								Dni = -d;
						}
						Dni = Dni*r*-2 + a*_abs((i+b))*-2 + b0;

						Di = Dni;
						Dr = Dnr;
		}
	}
	return antal;
}

extern "C" __declspec(dllexport) int PerpendicularCeltic(int antal,void *pdxr,void *pdxi, void* pDr, void*pDi, void* pD0r, void*pD0i,double *ptest1, double *ptest2, int m_nBailout2, int m_nMaxIter,double *m_db_z,BOOL *pGlitch)
{
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
	if(antal<m_nMaxIter && test1 <= m_nBailout2){
		for(;antal<m_nMaxIter;antal++){
			yr=dxr[antal]+Dr;
			yi=dxi[antal]+Di;
			test2=test1;
			test1 = yr*yr+yi*yi;
		    if(test1<m_db_z[antal]){
                test1 = m_nBailout2*2;
	      		bGlitch=TRUE;
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
						long double d = 2*r*a + a*a - 2*i*b - b*b;
						if(c>0){
							if(c+d>0)
								Dnr = d;
							else if(d==-c)
								Dnr = d;
							else if(d<-c)
								Dnr = -d-2*c;
						}
						else if (c==0)
							Dnr = _abs(d);
						else if (c < 0){
							if (c+d>0)
								Dnr = d + 2*c;
							else if (d == -c)
								Dnr = -d;
							else if (d < -c)
								Dnr = -d;
						}
						Dnr = Dnr + a0;

						c = r;
						d = a;
						if(c>0){
							if(c+d>0)
								Dni = d;
							else if(d==-c)
								Dni = d;
							else if(d<-c)
								Dni = -d-2*c;
						}
						else if (c==0)
							Dni = _abs(d);
						else if (c < 0){
							if (c+d>0)
								Dni = d + 2*c;
							else if (d == -c)
								Dni = -d;
							else if (d < -c)
								Dni = -d;
						}
						Dni = _abs((r+a))*b*-2 + Dni*i*-2 + b0;

						Di = Dni;
						Dr = Dnr;
		}
	}
	return antal;
}

extern "C" __declspec(dllexport) int PerpendicularBuffalo(int antal,void *pdxr,void *pdxi, void* pDr, void*pDi, void* pD0r, void*pD0i,double *ptest1, double *ptest2, int m_nBailout2, int m_nMaxIter,double *m_db_z,BOOL *pGlitch)
{
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
	if(antal<m_nMaxIter && test1 <= m_nBailout2){
		for(;antal<m_nMaxIter;antal++){
			yr=dxr[antal]+Dr;
			yi=dxi[antal]+Di;
			test2=test1;
			test1 = yr*yr+yi*yi;
		    if(test1<m_db_z[antal]){
                test1 = m_nBailout2*2;
	      		bGlitch=TRUE;
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
						long double d = 2*r*a + a*a - 2*i*b - b*b;
						if(c>0){
							if(c+d>0)
								Dnr = d;
							else if(d==-c)
								Dnr = d;
							else if(d<-c)
								Dnr = -d-2*c;
						}
						else if (c==0)
							Dnr = _abs(d);
						else if (c < 0){
							if (c+d>0)
								Dnr = d + 2*c;
							else if (d == -c)
								Dnr = -d;
							else if (d < -c)
								Dnr = -d;
						}
						Dnr = Dnr + a0;

						c = i;
						d = b;
						if(c>0){
							if(c+d>0)
								Dni = d;
							else if(d==-c)
								Dni = d;
							else if(d<-c)
								Dni = -d-2*c;
						}
						else if (c==0)
							Dni = _abs(d);
						else if (c < 0){
							if (c+d>0)
								Dni = d + 2*c;
							else if (d == -c)
								Dni = -d;
							else if (d < -c)
								Dni = -d;
						}
						Dni = Dni*r*-2 + a*_abs((i+b))*-2 + b0;

						Di = Dni;
						Dr = Dnr;
		}
	}
	return antal;
}

extern "C" __declspec(dllexport) int CubicQuasiBurningShip(int antal,void *pdxr,void *pdxi, void* pDr, void*pDi, void* pD0r, void*pD0i,double *ptest1, double *ptest2, int m_nBailout2, int m_nMaxIter,double *m_db_z,BOOL *pGlitch)
{
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
	if(antal<m_nMaxIter && test1 <= m_nBailout2){
		for(;antal<m_nMaxIter;antal++){
			yr=dxr[antal]+Dr;
			yi=dxi[antal]+Di;
			test2=test1;
			test1 = yr*yr+yi*yi;
		    if(test1<m_db_z[antal]){
                test1 = m_nBailout2*2;
	      		bGlitch=TRUE;
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

						if(r>0){
							if(r+a>0)
								Dnr = a;
							else if(a==-r)
								Dnr = a;
							else if(a<-r)
								Dnr = -a-2*r;
						}
						else if (r==0)
							Dnr = _abs(a);
						else if (r < 0){
							if (r+a>0)
								Dnr = a + 2*r;
							else if (a == -r)
								Dnr = -a;
							else if (a < -r)
								Dnr = -a;
						}
						ab = r+a;
						Dnr = (r2-3*i2) * Dnr + (2*ar+a2-6*ib-3*b2)*_abs(ab) + a0;

						double c = i*(3*r2-i2);
						double d = 3*i*(2*r*a + a2 - b2) + 3*b*(r2 + 2*r*a + a2) - b*(3*i2 + b2);
						if(c>0){
							if(c+d>0)
								Dni = d;
							else if(d==-c)
								Dni = d;
							else if(d<-c)
								Dni = -d-2*c;
						}
						else if (c==0)
							Dni = _abs(d);
						else if (c < 0){
							if (c+d>0)
								Dni = d + 2*c;
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
	return antal;
}

