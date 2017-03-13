#ifdef KF_FLOAT_BACKEND_CUSTOM

#include <windows.h>

#ifndef _FIXEDFLOAT_H_
#define _FIXEDFLOAT_H_

#ifdef _WIN64
#include "CDecNumber.h"
#include <intrin.h>
#else
#include "CDecNumber_old.h"
#endif

#define	FIXEDFLOAT_TYPE __int64
#define FIXEDFLOAT_ENTRIES (DECNUMDIGITS/8+16)
#define FIXEDFLOAT_DIGITS 8
#define FIXEDFLOAT_PARTMAX (FIXEDFLOAT_TYPE)100000000

class floatexp;
class floatexp2;

class CFixedFloat
{
//protected:
public:
	FIXEDFLOAT_TYPE m_pValues[FIXEDFLOAT_ENTRIES];
	int m_nValues;
	char *m_szValue;
	int m_bSign;

#ifdef _EXPANDED
	void MLoop1(FIXEDFLOAT_TYPE *pRetValues,CFixedFloat &A);
	void MLoop2(FIXEDFLOAT_TYPE *pRetValues,CFixedFloat &A);
	void MLoop3(FIXEDFLOAT_TYPE *pRetValues,CFixedFloat &A);
	void MLoop4(FIXEDFLOAT_TYPE *pRetValues,CFixedFloat &A);
	void MLoop5(FIXEDFLOAT_TYPE *pRetValues,CFixedFloat &A);
	void MLoop6(FIXEDFLOAT_TYPE *pRetValues,CFixedFloat &A);
	void MLoop7(FIXEDFLOAT_TYPE *pRetValues,CFixedFloat &A);
	void MLoop8(FIXEDFLOAT_TYPE *pRetValues,CFixedFloat &A);
	void MLoop9(FIXEDFLOAT_TYPE *pRetValues,CFixedFloat &A);
	void MLoop10(FIXEDFLOAT_TYPE *pRetValues,CFixedFloat &A);
	void MLoop11(FIXEDFLOAT_TYPE *pRetValues,CFixedFloat &A);
	void MLoop12(FIXEDFLOAT_TYPE *pRetValues,CFixedFloat &A);
	void MLoop13(FIXEDFLOAT_TYPE *pRetValues,CFixedFloat &A);
	void MLoop14(FIXEDFLOAT_TYPE *pRetValues,CFixedFloat &A);
	void MLoop15(FIXEDFLOAT_TYPE *pRetValues,CFixedFloat &A);
	void MLoop16(FIXEDFLOAT_TYPE *pRetValues,CFixedFloat &A);
	void MLoop17(FIXEDFLOAT_TYPE *pRetValues,CFixedFloat &A);
	void MLoop18(FIXEDFLOAT_TYPE *pRetValues,CFixedFloat &A);
	void MLoop19(FIXEDFLOAT_TYPE *pRetValues,CFixedFloat &A);
	void MLoop20(FIXEDFLOAT_TYPE *pRetValues,CFixedFloat &A);
	void MLoop21(FIXEDFLOAT_TYPE *pRetValues,CFixedFloat &A);
	void MLoop22(FIXEDFLOAT_TYPE *pRetValues,CFixedFloat &A);
	void MLoop23(FIXEDFLOAT_TYPE *pRetValues,CFixedFloat &A);
	void MLoop24(FIXEDFLOAT_TYPE *pRetValues,CFixedFloat &A);
	void MLoop25(FIXEDFLOAT_TYPE *pRetValues,CFixedFloat &A);
	void MLoop26(FIXEDFLOAT_TYPE *pRetValues,CFixedFloat &A);
	void MLoop27(FIXEDFLOAT_TYPE *pRetValues,CFixedFloat &A);
	void MLoop28(FIXEDFLOAT_TYPE *pRetValues,CFixedFloat &A);
	void MLoop29(FIXEDFLOAT_TYPE *pRetValues,CFixedFloat &A);
	void MLoop30(FIXEDFLOAT_TYPE *pRetValues,CFixedFloat &A);
	void MLoop31(FIXEDFLOAT_TYPE *pRetValues,CFixedFloat &A);
	void MLoop32(FIXEDFLOAT_TYPE *pRetValues,CFixedFloat &A);
	void MLoop33(FIXEDFLOAT_TYPE *pRetValues,CFixedFloat &A);
	void MLoop34(FIXEDFLOAT_TYPE *pRetValues,CFixedFloat &A);
	void MLoop35(FIXEDFLOAT_TYPE *pRetValues,CFixedFloat &A);
	void MLoop36(FIXEDFLOAT_TYPE *pRetValues,CFixedFloat &A);
	void MLoop37(FIXEDFLOAT_TYPE *pRetValues,CFixedFloat &A);
	void MLoop38(FIXEDFLOAT_TYPE *pRetValues,CFixedFloat &A);
	void MLoop39(FIXEDFLOAT_TYPE *pRetValues,CFixedFloat &A);
	void MLoop40(FIXEDFLOAT_TYPE *pRetValues,CFixedFloat &A);
	void MLoop41(FIXEDFLOAT_TYPE *pRetValues,CFixedFloat &A);
	void MLoop42(FIXEDFLOAT_TYPE *pRetValues,CFixedFloat &A);
	void MLoop43(FIXEDFLOAT_TYPE *pRetValues,CFixedFloat &A);
	void MLoop44(FIXEDFLOAT_TYPE *pRetValues,CFixedFloat &A);
	void MLoop45(FIXEDFLOAT_TYPE *pRetValues,CFixedFloat &A);
	void MLoop46(FIXEDFLOAT_TYPE *pRetValues,CFixedFloat &A);
	void MLoop47(FIXEDFLOAT_TYPE *pRetValues,CFixedFloat &A);
	void MLoop48(FIXEDFLOAT_TYPE *pRetValues,CFixedFloat &A);
	void MLoop49(FIXEDFLOAT_TYPE *pRetValues,CFixedFloat &A);
	void MLoop50(FIXEDFLOAT_TYPE *pRetValues,CFixedFloat &A);
	void MLoop51(FIXEDFLOAT_TYPE *pRetValues,CFixedFloat &A);
	void MLoop52(FIXEDFLOAT_TYPE *pRetValues,CFixedFloat &A);
	void MLoop53(FIXEDFLOAT_TYPE *pRetValues,CFixedFloat &A);
	void MLoop54(FIXEDFLOAT_TYPE *pRetValues,CFixedFloat &A);
	void MLoop55(FIXEDFLOAT_TYPE *pRetValues,CFixedFloat &A);
	void MLoop56(FIXEDFLOAT_TYPE *pRetValues,CFixedFloat &A);
	void MLoop57(FIXEDFLOAT_TYPE *pRetValues,CFixedFloat &A);
	void MLoop58(FIXEDFLOAT_TYPE *pRetValues,CFixedFloat &A);
	void MLoop59(FIXEDFLOAT_TYPE *pRetValues,CFixedFloat &A);
	void MLoop60(FIXEDFLOAT_TYPE *pRetValues,CFixedFloat &A);
	void MLoop61(FIXEDFLOAT_TYPE *pRetValues,CFixedFloat &A);
	void MLoop62(FIXEDFLOAT_TYPE *pRetValues,CFixedFloat &A);
	void MLoop63(FIXEDFLOAT_TYPE *pRetValues,CFixedFloat &A);
	void MLoop64(FIXEDFLOAT_TYPE *pRetValues,CFixedFloat &A);
	void MLoop65(FIXEDFLOAT_TYPE *pRetValues,CFixedFloat &A);
	void MLoop66(FIXEDFLOAT_TYPE *pRetValues,CFixedFloat &A);
	void MLoop67(FIXEDFLOAT_TYPE *pRetValues,CFixedFloat &A);
	void MLoop68(FIXEDFLOAT_TYPE *pRetValues,CFixedFloat &A);
	void MLoop69(FIXEDFLOAT_TYPE *pRetValues,CFixedFloat &A);
	void MLoop70(FIXEDFLOAT_TYPE *pRetValues,CFixedFloat &A);
	void MLoop71(FIXEDFLOAT_TYPE *pRetValues,CFixedFloat &A);
	void MLoop72(FIXEDFLOAT_TYPE *pRetValues,CFixedFloat &A);
	void MLoop73(FIXEDFLOAT_TYPE *pRetValues,CFixedFloat &A);
	void MLoop74(FIXEDFLOAT_TYPE *pRetValues,CFixedFloat &A);
	void MLoop75(FIXEDFLOAT_TYPE *pRetValues,CFixedFloat &A);
	void MLoop76(FIXEDFLOAT_TYPE *pRetValues,CFixedFloat &A);
	void MLoop77(FIXEDFLOAT_TYPE *pRetValues,CFixedFloat &A);
	void MLoop78(FIXEDFLOAT_TYPE *pRetValues,CFixedFloat &A);
	void MLoop79(FIXEDFLOAT_TYPE *pRetValues,CFixedFloat &A);
	void MLoop80(FIXEDFLOAT_TYPE *pRetValues,CFixedFloat &A);
	void MLoop81(FIXEDFLOAT_TYPE *pRetValues,CFixedFloat &A);
	void MLoop82(FIXEDFLOAT_TYPE *pRetValues,CFixedFloat &A);
	void MLoop83(FIXEDFLOAT_TYPE *pRetValues,CFixedFloat &A);
	void MLoop84(FIXEDFLOAT_TYPE *pRetValues,CFixedFloat &A);
	void MLoop85(FIXEDFLOAT_TYPE *pRetValues,CFixedFloat &A);
	void MLoop86(FIXEDFLOAT_TYPE *pRetValues,CFixedFloat &A);
	void MLoop87(FIXEDFLOAT_TYPE *pRetValues,CFixedFloat &A);
	void MLoop88(FIXEDFLOAT_TYPE *pRetValues,CFixedFloat &A);
	void MLoop89(FIXEDFLOAT_TYPE *pRetValues,CFixedFloat &A);
	void MLoop90(FIXEDFLOAT_TYPE *pRetValues,CFixedFloat &A);
	void MLoop91(FIXEDFLOAT_TYPE *pRetValues,CFixedFloat &A);
	void MLoop92(FIXEDFLOAT_TYPE *pRetValues,CFixedFloat &A);
	void MLoop93(FIXEDFLOAT_TYPE *pRetValues,CFixedFloat &A);
	void MLoop94(FIXEDFLOAT_TYPE *pRetValues,CFixedFloat &A);
	void MLoop95(FIXEDFLOAT_TYPE *pRetValues,CFixedFloat &A);
	void MLoop96(FIXEDFLOAT_TYPE *pRetValues,CFixedFloat &A);
	void MLoop97(FIXEDFLOAT_TYPE *pRetValues,CFixedFloat &A);
	void MLoop98(FIXEDFLOAT_TYPE *pRetValues,CFixedFloat &A);
	void MLoop99(FIXEDFLOAT_TYPE *pRetValues,CFixedFloat &A);
	void MLoop100(FIXEDFLOAT_TYPE *pRetValues,CFixedFloat &A);
	void MLoop101(FIXEDFLOAT_TYPE *pRetValues,CFixedFloat &A);
	void MLoop102(FIXEDFLOAT_TYPE *pRetValues,CFixedFloat &A);
	void MLoop103(FIXEDFLOAT_TYPE *pRetValues,CFixedFloat &A);
	void MLoop104(FIXEDFLOAT_TYPE *pRetValues,CFixedFloat &A);
	void MLoop105(FIXEDFLOAT_TYPE *pRetValues,CFixedFloat &A);
	void MLoop106(FIXEDFLOAT_TYPE *pRetValues,CFixedFloat &A);
	void MLoop107(FIXEDFLOAT_TYPE *pRetValues,CFixedFloat &A);
	void MLoop108(FIXEDFLOAT_TYPE *pRetValues,CFixedFloat &A);
	void MLoop109(FIXEDFLOAT_TYPE *pRetValues,CFixedFloat &A);
	void MLoop110(FIXEDFLOAT_TYPE *pRetValues,CFixedFloat &A);
	void MLoop111(FIXEDFLOAT_TYPE *pRetValues,CFixedFloat &A);
	void MLoop112(FIXEDFLOAT_TYPE *pRetValues,CFixedFloat &A);
	void MLoop113(FIXEDFLOAT_TYPE *pRetValues,CFixedFloat &A);
	void MLoop114(FIXEDFLOAT_TYPE *pRetValues,CFixedFloat &A);
	void MLoop115(FIXEDFLOAT_TYPE *pRetValues,CFixedFloat &A);
	void MLoop116(FIXEDFLOAT_TYPE *pRetValues,CFixedFloat &A);
	void MLoop117(FIXEDFLOAT_TYPE *pRetValues,CFixedFloat &A);
	void MLoop118(FIXEDFLOAT_TYPE *pRetValues,CFixedFloat &A);
	void MLoop119(FIXEDFLOAT_TYPE *pRetValues,CFixedFloat &A);
	void MLoop120(FIXEDFLOAT_TYPE *pRetValues,CFixedFloat &A);
	void MLoop121(FIXEDFLOAT_TYPE *pRetValues,CFixedFloat &A);
	void MLoop122(FIXEDFLOAT_TYPE *pRetValues,CFixedFloat &A);
	void MLoop123(FIXEDFLOAT_TYPE *pRetValues,CFixedFloat &A);
	void MLoop124(FIXEDFLOAT_TYPE *pRetValues,CFixedFloat &A);
	void MLoop125(FIXEDFLOAT_TYPE *pRetValues,CFixedFloat &A);
	void MLoop126(FIXEDFLOAT_TYPE *pRetValues,CFixedFloat &A);
	void MLoop127(FIXEDFLOAT_TYPE *pRetValues,CFixedFloat &A);
	void MLoop128(FIXEDFLOAT_TYPE *pRetValues,CFixedFloat &A);
	void MLoop129(FIXEDFLOAT_TYPE *pRetValues,CFixedFloat &A);
	void MLoop130(FIXEDFLOAT_TYPE *pRetValues,CFixedFloat &A);
	void MLoop131(FIXEDFLOAT_TYPE *pRetValues,CFixedFloat &A);
	void MLoop132(FIXEDFLOAT_TYPE *pRetValues,CFixedFloat &A);
	void MLoop133(FIXEDFLOAT_TYPE *pRetValues,CFixedFloat &A);
	void MLoop134(FIXEDFLOAT_TYPE *pRetValues,CFixedFloat &A);
	void MLoop135(FIXEDFLOAT_TYPE *pRetValues,CFixedFloat &A);
	void MLoop136(FIXEDFLOAT_TYPE *pRetValues,CFixedFloat &A);
	void MLoop137(FIXEDFLOAT_TYPE *pRetValues,CFixedFloat &A);
	void MLoop138(FIXEDFLOAT_TYPE *pRetValues,CFixedFloat &A);
	void MLoop139(FIXEDFLOAT_TYPE *pRetValues,CFixedFloat &A);
	void MLoop140(FIXEDFLOAT_TYPE *pRetValues,CFixedFloat &A);
	void MLoop141(FIXEDFLOAT_TYPE *pRetValues,CFixedFloat &A);
	void MLoop142(FIXEDFLOAT_TYPE *pRetValues,CFixedFloat &A);
	void MLoop143(FIXEDFLOAT_TYPE *pRetValues,CFixedFloat &A);
	void MLoop144(FIXEDFLOAT_TYPE *pRetValues,CFixedFloat &A);
	void MLoop145(FIXEDFLOAT_TYPE *pRetValues,CFixedFloat &A);
	void MLoop146(FIXEDFLOAT_TYPE *pRetValues,CFixedFloat &A);
	void MLoop147(FIXEDFLOAT_TYPE *pRetValues,CFixedFloat &A);
	void MLoop148(FIXEDFLOAT_TYPE *pRetValues,CFixedFloat &A);
	void MLoop149(FIXEDFLOAT_TYPE *pRetValues,CFixedFloat &A);
	void MLoop150(FIXEDFLOAT_TYPE *pRetValues,CFixedFloat &A);
	void MLoop151(FIXEDFLOAT_TYPE *pRetValues,CFixedFloat &A);

	void SLoop1(FIXEDFLOAT_TYPE *pResValues);
	void SLoop2(FIXEDFLOAT_TYPE *pResValues);
	void SLoop3(FIXEDFLOAT_TYPE *pResValues);
	void SLoop4(FIXEDFLOAT_TYPE *pResValues);
	void SLoop5(FIXEDFLOAT_TYPE *pResValues);
	void SLoop6(FIXEDFLOAT_TYPE *pResValues);
	void SLoop7(FIXEDFLOAT_TYPE *pResValues);
	void SLoop8(FIXEDFLOAT_TYPE *pResValues);
	void SLoop9(FIXEDFLOAT_TYPE *pResValues);
	void SLoop10(FIXEDFLOAT_TYPE *pResValues);
	void SLoop11(FIXEDFLOAT_TYPE *pResValues);
	void SLoop12(FIXEDFLOAT_TYPE *pResValues);
	void SLoop13(FIXEDFLOAT_TYPE *pResValues);
	void SLoop14(FIXEDFLOAT_TYPE *pResValues);
	void SLoop15(FIXEDFLOAT_TYPE *pResValues);
	void SLoop16(FIXEDFLOAT_TYPE *pResValues);
	void SLoop17(FIXEDFLOAT_TYPE *pResValues);
	void SLoop18(FIXEDFLOAT_TYPE *pResValues);
	void SLoop19(FIXEDFLOAT_TYPE *pResValues);
	void SLoop20(FIXEDFLOAT_TYPE *pResValues);
	void SLoop21(FIXEDFLOAT_TYPE *pResValues);
	void SLoop22(FIXEDFLOAT_TYPE *pResValues);
	void SLoop23(FIXEDFLOAT_TYPE *pResValues);
	void SLoop24(FIXEDFLOAT_TYPE *pResValues);
	void SLoop25(FIXEDFLOAT_TYPE *pResValues);
	void SLoop26(FIXEDFLOAT_TYPE *pResValues);
	void SLoop27(FIXEDFLOAT_TYPE *pResValues);
	void SLoop28(FIXEDFLOAT_TYPE *pResValues);
	void SLoop29(FIXEDFLOAT_TYPE *pResValues);
	void SLoop30(FIXEDFLOAT_TYPE *pResValues);
	void SLoop31(FIXEDFLOAT_TYPE *pResValues);
	void SLoop32(FIXEDFLOAT_TYPE *pResValues);
	void SLoop33(FIXEDFLOAT_TYPE *pResValues);
	void SLoop34(FIXEDFLOAT_TYPE *pResValues);
	void SLoop35(FIXEDFLOAT_TYPE *pResValues);
	void SLoop36(FIXEDFLOAT_TYPE *pResValues);
	void SLoop37(FIXEDFLOAT_TYPE *pResValues);
	void SLoop38(FIXEDFLOAT_TYPE *pResValues);
	void SLoop39(FIXEDFLOAT_TYPE *pResValues);
	void SLoop40(FIXEDFLOAT_TYPE *pResValues);
	void SLoop41(FIXEDFLOAT_TYPE *pResValues);
	void SLoop42(FIXEDFLOAT_TYPE *pResValues);
	void SLoop43(FIXEDFLOAT_TYPE *pResValues);
	void SLoop44(FIXEDFLOAT_TYPE *pResValues);
	void SLoop45(FIXEDFLOAT_TYPE *pResValues);
	void SLoop46(FIXEDFLOAT_TYPE *pResValues);
	void SLoop47(FIXEDFLOAT_TYPE *pResValues);
	void SLoop48(FIXEDFLOAT_TYPE *pResValues);
	void SLoop49(FIXEDFLOAT_TYPE *pResValues);
	void SLoop50(FIXEDFLOAT_TYPE *pResValues);
	void SLoop51(FIXEDFLOAT_TYPE *pResValues);
	void SLoop52(FIXEDFLOAT_TYPE *pResValues);
	void SLoop53(FIXEDFLOAT_TYPE *pResValues);
	void SLoop54(FIXEDFLOAT_TYPE *pResValues);
	void SLoop55(FIXEDFLOAT_TYPE *pResValues);
	void SLoop56(FIXEDFLOAT_TYPE *pResValues);
	void SLoop57(FIXEDFLOAT_TYPE *pResValues);
	void SLoop58(FIXEDFLOAT_TYPE *pResValues);
	void SLoop59(FIXEDFLOAT_TYPE *pResValues);
	void SLoop60(FIXEDFLOAT_TYPE *pResValues);
	void SLoop61(FIXEDFLOAT_TYPE *pResValues);
	void SLoop62(FIXEDFLOAT_TYPE *pResValues);
	void SLoop63(FIXEDFLOAT_TYPE *pResValues);
	void SLoop64(FIXEDFLOAT_TYPE *pResValues);
	void SLoop65(FIXEDFLOAT_TYPE *pResValues);
	void SLoop66(FIXEDFLOAT_TYPE *pResValues);
	void SLoop67(FIXEDFLOAT_TYPE *pResValues);
	void SLoop68(FIXEDFLOAT_TYPE *pResValues);
	void SLoop69(FIXEDFLOAT_TYPE *pResValues);
	void SLoop70(FIXEDFLOAT_TYPE *pResValues);
	void SLoop71(FIXEDFLOAT_TYPE *pResValues);
	void SLoop72(FIXEDFLOAT_TYPE *pResValues);
	void SLoop73(FIXEDFLOAT_TYPE *pResValues);
	void SLoop74(FIXEDFLOAT_TYPE *pResValues);
	void SLoop75(FIXEDFLOAT_TYPE *pResValues);
	void SLoop76(FIXEDFLOAT_TYPE *pResValues);
	void SLoop77(FIXEDFLOAT_TYPE *pResValues);
	void SLoop78(FIXEDFLOAT_TYPE *pResValues);
	void SLoop79(FIXEDFLOAT_TYPE *pResValues);
	void SLoop80(FIXEDFLOAT_TYPE *pResValues);
	void SLoop81(FIXEDFLOAT_TYPE *pResValues);
	void SLoop82(FIXEDFLOAT_TYPE *pResValues);
	void SLoop83(FIXEDFLOAT_TYPE *pResValues);
	void SLoop84(FIXEDFLOAT_TYPE *pResValues);
	void SLoop85(FIXEDFLOAT_TYPE *pResValues);
	void SLoop86(FIXEDFLOAT_TYPE *pResValues);
	void SLoop87(FIXEDFLOAT_TYPE *pResValues);
	void SLoop88(FIXEDFLOAT_TYPE *pResValues);
	void SLoop89(FIXEDFLOAT_TYPE *pResValues);
	void SLoop90(FIXEDFLOAT_TYPE *pResValues);
	void SLoop91(FIXEDFLOAT_TYPE *pResValues);
	void SLoop92(FIXEDFLOAT_TYPE *pResValues);
	void SLoop93(FIXEDFLOAT_TYPE *pResValues);
	void SLoop94(FIXEDFLOAT_TYPE *pResValues);
	void SLoop95(FIXEDFLOAT_TYPE *pResValues);
	void SLoop96(FIXEDFLOAT_TYPE *pResValues);
	void SLoop97(FIXEDFLOAT_TYPE *pResValues);
	void SLoop98(FIXEDFLOAT_TYPE *pResValues);
	void SLoop99(FIXEDFLOAT_TYPE *pResValues);
	void SLoop100(FIXEDFLOAT_TYPE *pResValues);
	void SLoop101(FIXEDFLOAT_TYPE *pResValues);
	void SLoop102(FIXEDFLOAT_TYPE *pResValues);
	void SLoop103(FIXEDFLOAT_TYPE *pResValues);
	void SLoop104(FIXEDFLOAT_TYPE *pResValues);
	void SLoop105(FIXEDFLOAT_TYPE *pResValues);
	void SLoop106(FIXEDFLOAT_TYPE *pResValues);
	void SLoop107(FIXEDFLOAT_TYPE *pResValues);
	void SLoop108(FIXEDFLOAT_TYPE *pResValues);
	void SLoop109(FIXEDFLOAT_TYPE *pResValues);
	void SLoop110(FIXEDFLOAT_TYPE *pResValues);
	void SLoop111(FIXEDFLOAT_TYPE *pResValues);
	void SLoop112(FIXEDFLOAT_TYPE *pResValues);
	void SLoop113(FIXEDFLOAT_TYPE *pResValues);
	void SLoop114(FIXEDFLOAT_TYPE *pResValues);
	void SLoop115(FIXEDFLOAT_TYPE *pResValues);
	void SLoop116(FIXEDFLOAT_TYPE *pResValues);
	void SLoop117(FIXEDFLOAT_TYPE *pResValues);
	void SLoop118(FIXEDFLOAT_TYPE *pResValues);
	void SLoop119(FIXEDFLOAT_TYPE *pResValues);
	void SLoop120(FIXEDFLOAT_TYPE *pResValues);
	void SLoop121(FIXEDFLOAT_TYPE *pResValues);
	void SLoop122(FIXEDFLOAT_TYPE *pResValues);
	void SLoop123(FIXEDFLOAT_TYPE *pResValues);
	void SLoop124(FIXEDFLOAT_TYPE *pResValues);
	void SLoop125(FIXEDFLOAT_TYPE *pResValues);
	void SLoop126(FIXEDFLOAT_TYPE *pResValues);
	void SLoop127(FIXEDFLOAT_TYPE *pResValues);
	void SLoop128(FIXEDFLOAT_TYPE *pResValues);
	void SLoop129(FIXEDFLOAT_TYPE *pResValues);
	void SLoop130(FIXEDFLOAT_TYPE *pResValues);
	void SLoop131(FIXEDFLOAT_TYPE *pResValues);
	void SLoop132(FIXEDFLOAT_TYPE *pResValues);
	void SLoop133(FIXEDFLOAT_TYPE *pResValues);
	void SLoop134(FIXEDFLOAT_TYPE *pResValues);
	void SLoop135(FIXEDFLOAT_TYPE *pResValues);
	void SLoop136(FIXEDFLOAT_TYPE *pResValues);
	void SLoop137(FIXEDFLOAT_TYPE *pResValues);
	void SLoop138(FIXEDFLOAT_TYPE *pResValues);
	void SLoop139(FIXEDFLOAT_TYPE *pResValues);
	void SLoop140(FIXEDFLOAT_TYPE *pResValues);
	void SLoop141(FIXEDFLOAT_TYPE *pResValues);
	void SLoop142(FIXEDFLOAT_TYPE *pResValues);
	void SLoop143(FIXEDFLOAT_TYPE *pResValues);
	void SLoop144(FIXEDFLOAT_TYPE *pResValues);
	void SLoop145(FIXEDFLOAT_TYPE *pResValues);
	void SLoop146(FIXEDFLOAT_TYPE *pResValues);
	void SLoop147(FIXEDFLOAT_TYPE *pResValues);
	void SLoop148(FIXEDFLOAT_TYPE *pResValues);
	void SLoop149(FIXEDFLOAT_TYPE *pResValues);
	void SLoop150(FIXEDFLOAT_TYPE *pResValues);
	void SLoop151(FIXEDFLOAT_TYPE *pResValues);
#endif
      void Copy(const CFixedFloat &bi);
public:
	CFixedFloat();
	CFixedFloat(const CFixedFloat &bi);
	CFixedFloat(const char *sz);
	CFixedFloat(int a);
	CFixedFloat(double a);
	~CFixedFloat();
	void SetMaxSignificant(int nMax);
	BOOL Parse(const char *sz);
	char *ToText();
	int ToInt();
	double ToDouble(int nScaling=0);

	CFixedFloat Add(const CFixedFloat &A) const;
	CFixedFloat Subtract(const CFixedFloat &A) const;
	CFixedFloat Multiply(const CFixedFloat &A) const;
	CFixedFloat Square() const;
	CFixedFloat Divide(const CFixedFloat &A) const;
	CFixedFloat Double();
	CFixedFloat &AbsAdd(const CFixedFloat &a, const CFixedFloat &b);
	CFixedFloat &Abs();


	BOOL operator >(const CFixedFloat &A) const;
	BOOL operator <(const CFixedFloat &A) const;
	BOOL operator ==(const CFixedFloat &A) const;

	CFixedFloat &operator =(const CFixedFloat &A);
	CFixedFloat &operator =(const char *sz);
	CFixedFloat &operator =(int a);
	CFixedFloat &operator =(double a);

	CFixedFloat operator *(const CFixedFloat &A) const;
	CFixedFloat operator /(const CFixedFloat &A) const;
	CFixedFloat operator +(const CFixedFloat &A) const;
	CFixedFloat operator -(const CFixedFloat &A) const;
	CFixedFloat operator -() const;
	CFixedFloat &operator*=(const CFixedFloat &A);
	CFixedFloat &operator/=(const CFixedFloat &A);
	CFixedFloat &operator+=(const CFixedFloat &A);
	CFixedFloat &operator-=(const CFixedFloat &A);
	CFixedFloat &operator*=(int A);
	CFixedFloat &operator/=(int A);
	CFixedFloat &operator+=(int A);
	CFixedFloat &operator-=(int A);

	friend class floatexp;
	friend class floatexp2;
};

BOOL operator==(const CFixedFloat &A,int nB);
BOOL operator==(int nB,const CFixedFloat &A);
BOOL operator>(const CFixedFloat &A,int nB);
BOOL operator>(int nB,const CFixedFloat &A);
BOOL operator>(const CFixedFloat &A,double nB);
BOOL operator>(double nB,const CFixedFloat &A);
BOOL operator<(const CFixedFloat &A,int nB);
BOOL operator<(int nB,const CFixedFloat &A);
BOOL operator<(const CFixedFloat &A,double nB);
BOOL operator<(double nB,const CFixedFloat &A);

CFixedFloat operator*(const CFixedFloat &A,long nB);
CFixedFloat operator*(long nB,const CFixedFloat &A);
CFixedFloat operator*(const CFixedFloat &A,int nB);
CFixedFloat operator*(int nB,const CFixedFloat &A);
CFixedFloat operator*(const CFixedFloat &A,double nB);
CFixedFloat operator*(double nB,const CFixedFloat &A);
CFixedFloat operator+(const CFixedFloat &A,int nB);
CFixedFloat operator+(int nB,const CFixedFloat &A);
CFixedFloat operator+(const CFixedFloat &A,double nB);
CFixedFloat operator+(double nB,const CFixedFloat &A);
CFixedFloat operator/(const CFixedFloat &A,int nB);
CFixedFloat operator/(int nB,const CFixedFloat &A);
CFixedFloat operator/(const CFixedFloat &A,double nB);
CFixedFloat operator/(double nB,const CFixedFloat &A);
CFixedFloat operator-(const CFixedFloat &A,int nB);
CFixedFloat operator-(int nB,const CFixedFloat &A);
CFixedFloat operator-(const CFixedFloat &A,double nB);
CFixedFloat operator-(double nB,const CFixedFloat &A);
CFixedFloat operator^(const CFixedFloat &A,long nB);

int operator+=(int &nB,const CFixedFloat &A);
int operator+=(double &nB,const CFixedFloat &A);
int operator+=(CFixedFloat &A,const double &nB);
CFixedFloat operator*(const CFixedFloat &A,int nB);

#endif //_FIXEDFLOAT_H_

#else

#ifndef KF_CFIXEDFLOAT_H
#define KF_CFIXEDFLOAT_H

#include "CDecNumber.h"

typedef decNumber FixedFloat;

class CFixedFloat
{
public:
	FixedFloat m_f;

	inline CFixedFloat()
	{
		unsigned p = LOW_PRECISION;
		Precision q(p);
		m_f.precision(p);
		m_f = 0;
	};

	inline CFixedFloat(const CFixedFloat &a)
	{
		unsigned p = a.m_f.precision();
		Precision q(p);
		m_f.precision(p);
		m_f = a.m_f;
	};

	inline CFixedFloat(const FixedFloat &a)
	{
		unsigned p = a.precision();
		Precision q(p);
		m_f.precision(p);
		m_f = a;
	};

	inline CFixedFloat(const char *sz)
	{
		unsigned p = strlen(sz) + LOW_PRECISION;
		Precision q(p);
		m_f.precision(p);
		m_f = FixedFloat(sz);
	};

	inline CFixedFloat(int a)
	{
		unsigned p = LOW_PRECISION;
		Precision q(p);
		m_f.precision(p);
		m_f = a;
	};

	inline CFixedFloat(double a)
	{
		unsigned p = LOW_PRECISION;
		Precision q(p);
		m_f.precision(p);
		m_f = a;
	};

	inline ~CFixedFloat()
	{
	};

	char *ToText();

	inline int ToInt()
	{
		return int(m_f);
	};

	inline double ToDouble(int nScaling = 0)
	{
		using std::pow;
		unsigned p = LOW_PRECISION;
		Precision q(p);
		FixedFloat f;
		f.precision(p);
		f = m_f;
		return double(FixedFloat(f * pow(FixedFloat(10), nScaling)));
	};

	inline CFixedFloat Add(const CFixedFloat &A) const
	{
		Precision p(std::max(m_f.precision(), A.m_f.precision()));
		return CFixedFloat(FixedFloat(m_f + A.m_f));
	};

	inline CFixedFloat Subtract(const CFixedFloat &A) const
	{
		Precision p(std::max(m_f.precision(), A.m_f.precision()));
		return CFixedFloat(FixedFloat(m_f - A.m_f));
	};

	inline CFixedFloat Multiply(const CFixedFloat &A) const
	{
		Precision p(std::max(m_f.precision(), A.m_f.precision()));
		return CFixedFloat(FixedFloat(m_f * A.m_f));
	};

	inline CFixedFloat Square() const
	{
		Precision p(m_f.precision());
		return CFixedFloat(FixedFloat(m_f * m_f));
	};

	inline CFixedFloat Divide(const CFixedFloat &A) const
	{
		Precision p(std::max(m_f.precision(), A.m_f.precision()));
		return CFixedFloat(FixedFloat(m_f / A.m_f));
	};

	inline CFixedFloat &Double()
	{
#ifdef KF_FLOAT_BACKEND_MPFR
		mpfr_mul_2exp(m_f.backend().data(), m_f.backend().data(), 1, MPFR_RNDN);
#else
		mpf_mul_2exp(m_f.backend().data(), m_f.backend().data(), 1);
#endif
		return *this;
	};

	inline CFixedFloat &AbsAdd(CFixedFloat &a, CFixedFloat &b) // TODO FIXME avoid copies
	{
		Precision p(std::max(a.m_f.precision(), b.m_f.precision()));
		using std::abs;
		m_f = abs(a.m_f) + abs(b.m_f);
		return *this;
	};

	inline CFixedFloat &Abs()
	{
#ifdef KF_FLOAT_BACKEND_MPFR
		mpfr_abs(m_f.backend().data(), m_f.backend().data(), MPFR_RNDN);
#else
		mpf_abs(m_f.backend().data(), m_f.backend().data());
#endif
		return *this;
	};

	inline bool operator>(const CFixedFloat &A) const
	{
		return m_f > A.m_f;
	};

	inline bool operator<(const CFixedFloat &A) const
	{
		return m_f < A.m_f;
	};

	inline bool operator==(const CFixedFloat &A) const
	{
		return m_f == A.m_f;
	};

	inline CFixedFloat &operator=(const CFixedFloat &A)
	{
		m_f.precision(A.m_f.precision());
		m_f = A.m_f;
		return *this;
	};

	inline CFixedFloat &operator=(const char *sz)
	{
		*this = CFixedFloat(sz);
		return *this;
	};

	inline CFixedFloat &operator=(int a)
	{
		*this = CFixedFloat(a);
		return *this;
	};

	inline CFixedFloat &operator=(double a)
	{
		*this = CFixedFloat(a);
		return *this;
	};

	inline CFixedFloat operator*(const CFixedFloat &A) const
	{
		Precision p(std::max(m_f.precision(), A.m_f.precision()));
		return CFixedFloat(FixedFloat(m_f * A.m_f));
	};

	inline CFixedFloat operator/(const CFixedFloat &A) const
	{
		Precision p(std::max(m_f.precision(), A.m_f.precision()));
		return CFixedFloat(FixedFloat(m_f / A.m_f));
	};

	inline CFixedFloat operator+(const CFixedFloat &A) const
	{
		Precision p(std::max(m_f.precision(), A.m_f.precision()));
		return CFixedFloat(FixedFloat(m_f + A.m_f));
	};

	inline CFixedFloat operator-(const CFixedFloat &A) const
	{
		Precision p(std::max(m_f.precision(), A.m_f.precision()));
		return CFixedFloat(FixedFloat(m_f - A.m_f));
	};

	inline CFixedFloat operator-() const
	{
		Precision p(m_f.precision());
		return CFixedFloat(FixedFloat(-m_f));
	};

	inline CFixedFloat &operator*=(const CFixedFloat &A)
	{
		*this = *this * A;
		return *this;
	};

	inline CFixedFloat &operator/=(const CFixedFloat &A)
	{
		*this = *this / A;
		return *this;
	};

	inline CFixedFloat &operator+=(const CFixedFloat &A)
	{
		*this = *this + A;
		return *this;
	};

	inline CFixedFloat &operator-=(const CFixedFloat &A)
	{
		*this = *this - A;
		return *this;
	};

	inline CFixedFloat &operator*=(int A)
	{
		m_f *= A;
		return *this;
	};

	inline CFixedFloat &operator/=(int A)
	{
		m_f /= A;
		return *this;
	};

	inline CFixedFloat &operator+=(int A)
	{
		m_f += A;
		return *this;
	};

	inline CFixedFloat &operator-=(int A)
	{
		m_f -= A;
		return *this;
	};

	friend class floatexp;
	friend class floatexp2;
};

inline bool operator==(const CFixedFloat &A,int nB)
{
	return A.m_f == nB;
}

inline bool operator==(int nB,const CFixedFloat &A)
{
	return nB == A.m_f;
}

inline bool operator>(const CFixedFloat &A,int nB)
{
	return A.m_f > nB;
}

inline bool operator>(int nB,const CFixedFloat &A)
{
	return nB > A.m_f;
}

inline bool operator>(const CFixedFloat &A,double nB)
{
	return A.m_f > nB;
}

inline bool operator>(double nB,const CFixedFloat &A)
{
	return nB > A.m_f;
}

inline bool operator<(const CFixedFloat &A,int nB)
{
	return A.m_f < nB;
}

inline bool operator<(int nB,const CFixedFloat &A)
{
	return nB < A.m_f;
}

inline bool operator<(const CFixedFloat &A,double nB)
{
	return A.m_f < nB;
}

inline bool operator<(double nB,const CFixedFloat &A)
{
	return nB < A.m_f;
}

inline CFixedFloat operator*(const CFixedFloat &A,long nB)
{
	Precision p(std::max(A.m_f.precision(), LOW_PRECISION));
	return CFixedFloat(FixedFloat(A.m_f * nB));
}

inline CFixedFloat operator*(long nB,const CFixedFloat &A)
{
	Precision p(std::max(LOW_PRECISION, A.m_f.precision()));
	return CFixedFloat(FixedFloat(nB * A.m_f));
}

inline CFixedFloat operator*(const CFixedFloat &A,int nB)
{
	Precision p(std::max(A.m_f.precision(), LOW_PRECISION));
	return CFixedFloat(FixedFloat(A.m_f * nB));
}

inline CFixedFloat operator*(int nB,const CFixedFloat &A)
{
	Precision p(std::max(LOW_PRECISION, A.m_f.precision()));
	return CFixedFloat(FixedFloat(nB * A.m_f));
}

inline CFixedFloat operator*(const CFixedFloat &A,double nB)
{
	Precision p(std::max(A.m_f.precision(), LOW_PRECISION));
	return CFixedFloat(FixedFloat(A.m_f * nB));
}

inline CFixedFloat operator*(double nB,const CFixedFloat &A)
{
	Precision p(std::max(LOW_PRECISION, A.m_f.precision()));
	return CFixedFloat(FixedFloat(nB * A.m_f));
}

inline CFixedFloat operator/(const CFixedFloat &A,int nB)
{
	Precision p(std::max(A.m_f.precision(), LOW_PRECISION));
	return CFixedFloat(FixedFloat(A.m_f / nB));
}

inline CFixedFloat operator/(int nB,const CFixedFloat &A)
{
	Precision p(std::max(LOW_PRECISION, A.m_f.precision()));
	return CFixedFloat(FixedFloat(nB / A.m_f));
}

inline CFixedFloat operator/(const CFixedFloat &A,double nB)
{
	Precision p(std::max(A.m_f.precision(), LOW_PRECISION));
	return CFixedFloat(FixedFloat(A.m_f / nB));
}

inline CFixedFloat operator/(double nB,const CFixedFloat &A)
{
	Precision p(std::max(LOW_PRECISION, A.m_f.precision()));
	return CFixedFloat(FixedFloat(nB / A.m_f));
}

inline CFixedFloat operator+(const CFixedFloat &A,int nB)
{
	Precision p(std::max(A.m_f.precision(), LOW_PRECISION));
	return CFixedFloat(FixedFloat(A.m_f + nB));
}

inline CFixedFloat operator+(int nB,const CFixedFloat &A)
{
	Precision p(std::max(LOW_PRECISION, A.m_f.precision()));
	return CFixedFloat(FixedFloat(nB + A.m_f));
}

inline CFixedFloat operator+(const CFixedFloat &A,double nB)
{
	Precision p(std::max(A.m_f.precision(), LOW_PRECISION));
	return CFixedFloat(FixedFloat(A.m_f * nB));
}

inline CFixedFloat operator+(double nB,const CFixedFloat &A)
{
	Precision p(std::max(LOW_PRECISION, A.m_f.precision()));
	return CFixedFloat(FixedFloat(nB * A.m_f));
}

inline CFixedFloat operator-(const CFixedFloat &A,int nB)
{
	Precision p(std::max(A.m_f.precision(), LOW_PRECISION));
	return CFixedFloat(FixedFloat(A.m_f * nB));
}

inline CFixedFloat operator-(int nB,const CFixedFloat &A)
{
	Precision p(std::max(LOW_PRECISION, A.m_f.precision()));
	return CFixedFloat(FixedFloat(nB * A.m_f));
}

inline CFixedFloat operator-(const CFixedFloat &A,double nB)
{
	Precision p(std::max(A.m_f.precision(), LOW_PRECISION));
	return CFixedFloat(FixedFloat(A.m_f - nB));
}

inline CFixedFloat operator-(double nB,const CFixedFloat &A)
{
	Precision p(std::max(LOW_PRECISION, A.m_f.precision()));
	return CFixedFloat(FixedFloat(nB - A.m_f));
}

inline CFixedFloat operator^(const CFixedFloat &A,long nB)
{
	using std::pow;
	Precision p(A.m_f.precision());
	return CFixedFloat(FixedFloat(pow(A.m_f, nB)));
}

#define FIXEDFLOAT_ENTRIES (DECNUMDIGITS/8+16)
#define FIXEDFLOAT_DIGITS 8
#define FIXEDFLOAT_TYPE __int64

#endif

#endif
