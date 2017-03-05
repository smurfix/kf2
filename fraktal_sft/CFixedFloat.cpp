// Kalles Fraktaler 2
//
// © 2014 Karl Runmo ,runmo@hotmail.com
//
// COVERED CODE IS PROVIDED UNDER THIS LICENSE ON AN "AS IS" BASIS, WITHOUT WARRANTY
// OF ANY KIND, EITHER EXPRESSED OR IMPLIED, INCLUDING, WITHOUT LIMITATION, WARRANTIES
// THAT THE COVERED CODE IS FREE OF DEFECTS, MERCHANTABLE, FIT FOR A PARTICULAR PURPOSE
// OR NON-INFRINGING. THE ENTIRE RISK AS TO THE QUALITY AND PERFORMANCE OF THE COVERED
// CODE IS WITH YOU. SHOULD ANY COVERED CODE PROVE DEFECTIVE IN ANY RESPECT, YOU (NOT
// THE INITIAL DEVELOPER OR ANY OTHER CONTRIBUTOR) ASSUME THE COST OF ANY NECESSARY
// SERVICING, REPAIR OR CORRECTION. THIS DISCLAIMER OF WARRANTY CONSTITUTES AN ESSENTIAL
// PART OF THIS LICENSE. NO USE OF ANY COVERED CODE IS AUTHORIZED HEREUNDER EXCEPT UNDER
// THIS DISCLAIMER.

#include "CFixedFloat.h"
#include <stdio.h>
#include "../common/StringVector.h"

static BOOL g_nMaxSignificant=0;
static BOOL isdigit(char c)
{
       return (c>='0' && c<='9');
}
CFixedFloat::CFixedFloat()
{
	m_bSign=FALSE;
	m_nValues=0;
	m_szValue=NULL;
}
CFixedFloat::CFixedFloat(const CFixedFloat &bi)
{
	m_bSign = bi.m_bSign;
	m_nValues = bi.m_nValues;
	memcpy(m_pValues,bi.m_pValues,sizeof(FIXEDFLOAT_TYPE)*bi.m_nValues);
	m_szValue=NULL;
}
CFixedFloat::CFixedFloat(const char *sz)
{
	m_bSign=FALSE;
	m_nValues=0;
	m_szValue=NULL;
	Parse(sz);
}
CFixedFloat::CFixedFloat(int a)
{
	if(a>=0){
		m_bSign=FALSE;
		m_pValues[0]=a;
	}
	else{
		m_bSign=TRUE;
		m_pValues[0]=-a;
	}
	m_nValues=1;
	m_szValue=NULL;
}
CFixedFloat::CFixedFloat(double a)
{
	m_bSign=(a<0);
	if(a<0)
		a=-a;
	m_nValues=0;
	m_szValue=NULL;
	int nStart=0;
	while((!g_nMaxSignificant || m_nValues<g_nMaxSignificant) && m_nValues<FIXEDFLOAT_ENTRIES && a){
		m_pValues[m_nValues]=(FIXEDFLOAT_TYPE)a;
		a-=m_pValues[m_nValues];
		a*=FIXEDFLOAT_PARTMAX;
		if(!nStart && m_pValues[m_nValues])
			nStart=1;
		else if(nStart)
			nStart++;
		m_nValues++;
		if(nStart>3)
			break;
	}
}
CFixedFloat::~CFixedFloat()
{
	if(m_szValue)
		delete m_szValue;
}
void CFixedFloat::SetMaxSignificant(int nMax)
{
	if(!nMax)
		g_nMaxSignificant=0;
	else{
		g_nMaxSignificant = 3+nMax/FIXEDFLOAT_DIGITS;
		if(g_nMaxSignificant>FIXEDFLOAT_ENTRIES)
			g_nMaxSignificant=FIXEDFLOAT_ENTRIES;
	}
}

BOOL CFixedFloat::Parse(const char *sz)
{
	while(*sz=='\t' || *sz==' ' || *sz=='\r' || *sz=='\n')
		sz++;
	memset(m_pValues,0,sizeof(FIXEDFLOAT_TYPE)*FIXEDFLOAT_ENTRIES);
	m_nValues=0;
	if(*sz=='-'){
		m_bSign=TRUE;
		sz++;
	}
	else
		m_bSign=FALSE;
	char *szNew = NULL;
	char *e = strstr(sz,"E");
	if(!e)
		e = strstr(sz,"e");
	if(e){
		e++;
		int nExp = atoi(e);
		if(nExp<0){
			nExp++;
			int nNew = strlen(sz)-nExp+10;
			szNew = new char[nNew+256];
			int n=2;
			strcpy(szNew,"0.");
			while(nExp<0 && n<nNew){
				szNew[n++]='0';
				nExp++;
			}
			while(*sz && n<nNew){
				if(*sz=='.'){
					sz++;
					continue;
				}
				else if(!isdigit(*sz))
					break;
				szNew[n++]=*sz;
				sz++;
			}
			szNew[n]=0;
			sz=szNew;
		}
	}
	while(isdigit(*sz)){
		m_nValues=1;
		m_pValues[0]=10*m_pValues[0]+*sz-'0';
		sz++;
	}
	if(*sz=='.'){
		sz++;
		int nMax = FIXEDFLOAT_ENTRIES;
		if(g_nMaxSignificant && nMax>g_nMaxSignificant)
			nMax=g_nMaxSignificant;
		if(m_nValues==0){
			m_pValues[0]=0;
			m_nValues++;
		}
		for(;m_nValues<nMax;m_nValues++){
			m_pValues[m_nValues]=0;
			int i=0;
			while(isdigit(*sz) && i<FIXEDFLOAT_DIGITS){
				m_pValues[m_nValues]=10*m_pValues[m_nValues]+*sz-'0';
				sz++;
				i++;
			}
			if(i<FIXEDFLOAT_DIGITS){
				while(i<FIXEDFLOAT_DIGITS){
					m_pValues[m_nValues]=10*m_pValues[m_nValues];
					i++;
				}
				m_nValues++;
				break;
			}
		}
	}
	if(szNew)
		delete szNew;
	return TRUE;
}
int g_nStrings=0;
char g_szStrings[4][FIXEDFLOAT_ENTRIES*FIXEDFLOAT_DIGITS+256];
char *CFixedFloat::ToText()
{
	int nLen = /*m_nValues*/FIXEDFLOAT_ENTRIES*FIXEDFLOAT_DIGITS+32;
	int nPrevLen=0;
	if(m_szValue){
		nPrevLen = strlen(m_szValue);
		delete m_szValue;
	}
	//m_szValue = new char[nLen+256];
	//char *szRet = m_szValue;
	unsigned int nString = (unsigned int)InterlockedIncrement((LPLONG)&g_nStrings);
	char *szRet = g_szStrings[nString%4];
	if(!m_nValues){
		szRet[0]='0';
		szRet[1]=0;
		return szRet;
	}
	int i, c=0;
	if(m_bSign)
		szRet[c++]='-';
	FIXEDFLOAT_TYPE p;
	for(i=0;i<m_nValues && c<nLen;i++){
		p=FIXEDFLOAT_PARTMAX;
		do{
			p/=10;
			FIXEDFLOAT_TYPE n = m_pValues[i]/p;
			if(!n && !i && p>1)
				continue;
			szRet[c++]=(char)(n%10)+'0';
		}while(p>1);
		if(!i && m_nValues>1)
			szRet[c++]='.';
	}
	if(m_nValues>1){
		while(c && szRet[c-1]=='0')
			c--;
		if(c && szRet[c-1]=='.')
			c--;
	}
	szRet[c]=0;
	return szRet;
}
int CFixedFloat::ToInt()
{
	if(!m_nValues)
		return 0;
	return (int)m_pValues[0];
}
void CFixedFloat::Copy(const CFixedFloat &bi)
{
	m_bSign = bi.m_bSign;
	m_nValues = bi.m_nValues;
	memcpy(m_pValues,bi.m_pValues,sizeof(FIXEDFLOAT_TYPE)*bi.m_nValues);
}
double CFixedFloat::ToDouble(int nScaling)
{
	double a=0;
	int n;
	int nStart = 0;
	for(nStart=0;nStart<m_nValues;nStart++)
		if(m_pValues[nStart])
			break;
	nStart+=24/FIXEDFLOAT_DIGITS;
	if(nStart>m_nValues)
		nStart=m_nValues;
	int nMul=0;
	if(nScaling){
		nMul = nScaling%FIXEDFLOAT_DIGITS;
		nScaling/=FIXEDFLOAT_DIGITS;
	}
	for(n=nStart-1;n>=nScaling;n--)
		a = a/FIXEDFLOAT_PARTMAX + m_pValues[n];
	while(nMul){
		a*=10;
		nMul--;
	}
	if(m_bSign)
		a=-a;
	return a;
}
CFixedFloat CFixedFloat::Add(const CFixedFloat &A) const
{
	CFixedFloat Ret;
	if(A.m_bSign){
		CFixedFloat &A2 = const_cast<CFixedFloat &>(A);
		A2.m_bSign=0;
		Ret.Copy(Subtract(A2));
		A2.m_bSign=1;
		return Ret;
	}
	else if(m_bSign){
		CFixedFloat &this2 = const_cast<CFixedFloat &>(*this);
		this2.m_bSign=0;
		Ret = A.Subtract(this2);
		this2.m_bSign=1;
		return Ret;
	}
	int nLen = m_nValues;
	if(nLen>A.m_nValues)
		nLen=A.m_nValues;
	if(g_nMaxSignificant && nLen>g_nMaxSignificant)
		nLen=g_nMaxSignificant;
	int i;
	for(i=0;i<nLen;i++)
		Ret.m_pValues[i] = m_pValues[i]+A.m_pValues[i];
	if(nLen<m_nValues)
		nLen=m_nValues;
	if(g_nMaxSignificant && nLen>g_nMaxSignificant)
		nLen=g_nMaxSignificant;
	for(;i<nLen;i++)
		Ret.m_pValues[i] = m_pValues[i];
	if(nLen<A.m_nValues)
		nLen=A.m_nValues;
	if(g_nMaxSignificant && nLen>g_nMaxSignificant)
		nLen=g_nMaxSignificant;
	for(;i<nLen;i++)
		Ret.m_pValues[i] = A.m_pValues[i];
	Ret.m_nValues=i;
	for(i=Ret.m_nValues-1;i>0;i--){
		if(Ret.m_pValues[i]>=FIXEDFLOAT_PARTMAX){
			Ret.m_pValues[i-1]+=Ret.m_pValues[i]/FIXEDFLOAT_PARTMAX;
			Ret.m_pValues[i]=Ret.m_pValues[i]%FIXEDFLOAT_PARTMAX;
		}
	}
	return Ret;
}
CFixedFloat CFixedFloat::Subtract(const CFixedFloat &A) const
{
	CFixedFloat Ret;
	if(m_bSign){
		CFixedFloat &this2 = const_cast<CFixedFloat &>(*this);
		this2.m_bSign=0;
		Ret = this2.Add(A);
		this2.m_bSign=1;
		Ret.m_bSign=1-Ret.m_bSign;
		return Ret;
	}
	else if(A.m_bSign){
		CFixedFloat &A2 = const_cast<CFixedFloat &>(A);
		A2.m_bSign=0;
		Ret = Add(A2);
		A2.m_bSign=1;
		return Ret;
	}
	if(A>*this){
		Ret = A.Subtract(*this);
		Ret.m_bSign=1;
		return Ret;
	}
	int nLen = m_nValues;
	if(nLen>A.m_nValues)
		nLen=A.m_nValues;
	if(g_nMaxSignificant && nLen>g_nMaxSignificant)
		nLen=g_nMaxSignificant;
	int i;
	for(i=0;i<nLen;i++)
		Ret.m_pValues[i] = m_pValues[i]-A.m_pValues[i];
	if(nLen<m_nValues)
		nLen=m_nValues;
	if(g_nMaxSignificant && nLen>g_nMaxSignificant)
		nLen=g_nMaxSignificant;
	for(;i<nLen;i++)
		Ret.m_pValues[i] = m_pValues[i];
	if(nLen<A.m_nValues)
		nLen=A.m_nValues;
	if(g_nMaxSignificant && nLen>g_nMaxSignificant)
		nLen=g_nMaxSignificant;
	for(;i<nLen;i++)
		Ret.m_pValues[i] = -A.m_pValues[i];
	Ret.m_nValues=i;
	for(i=Ret.m_nValues-1;i>0;i--){
		if(Ret.m_pValues[i]<0){
			Ret.m_pValues[i-1]--;
			Ret.m_pValues[i]+=FIXEDFLOAT_PARTMAX;
		}
	}
	return Ret;
}
extern "C" void multiplyAdd(FIXEDFLOAT_TYPE *dst, FIXEDFLOAT_TYPE src1, FIXEDFLOAT_TYPE src2);
extern "C" void multiplyAddTwice(FIXEDFLOAT_TYPE *dst, FIXEDFLOAT_TYPE src1, FIXEDFLOAT_TYPE src2);
extern "C" void divAdd(FIXEDFLOAT_TYPE *dst, FIXEDFLOAT_TYPE *src, FIXEDFLOAT_TYPE divisor);

CFixedFloat CFixedFloat::Multiply(const CFixedFloat &A) const
{
	CFixedFloat &this2 = const_cast<CFixedFloat &>(*this);
	CFixedFloat &A2 = const_cast<CFixedFloat &>(A);
	CFixedFloat Ret;
	if(g_nMaxSignificant){
		for(;this2.m_nValues<g_nMaxSignificant;this2.m_nValues++)
			this2.m_pValues[this2.m_nValues]=0;
		for(;A2.m_nValues<g_nMaxSignificant;A2.m_nValues++)
			A2.m_pValues[A2.m_nValues]=0;
	}
	Ret.m_nValues = m_nValues+A.m_nValues;
	if(g_nMaxSignificant && Ret.m_nValues>g_nMaxSignificant)
		Ret.m_nValues=g_nMaxSignificant;
	else if(Ret.m_nValues>FIXEDFLOAT_ENTRIES)
		Ret.m_nValues=FIXEDFLOAT_ENTRIES;
	memset(Ret.m_pValues,0,sizeof(FIXEDFLOAT_TYPE)*Ret.m_nValues);
	int a, b;
	for(a=0;a<m_nValues;a++){
		int nEnd = A.m_nValues;
		int nRetEnd = Ret.m_nValues-a;
		if(nEnd>nRetEnd)
			nEnd=nRetEnd;
		int nEndLoop = nEnd-nEnd%32;
		for(b=0;b<nEndLoop;){
			Ret.m_pValues[a+b] += m_pValues[a]*A.m_pValues[b];
			b++;
			Ret.m_pValues[a+b] += m_pValues[a]*A.m_pValues[b];
			b++;
			Ret.m_pValues[a+b] += m_pValues[a]*A.m_pValues[b];
			b++;
			Ret.m_pValues[a+b] += m_pValues[a]*A.m_pValues[b];
			b++;
			Ret.m_pValues[a+b] += m_pValues[a]*A.m_pValues[b];
			b++;
			Ret.m_pValues[a+b] += m_pValues[a]*A.m_pValues[b];
			b++;
			Ret.m_pValues[a+b] += m_pValues[a]*A.m_pValues[b];
			b++;
			Ret.m_pValues[a+b] += m_pValues[a]*A.m_pValues[b];
			b++;
			Ret.m_pValues[a+b] += m_pValues[a]*A.m_pValues[b];
			b++;
			Ret.m_pValues[a+b] += m_pValues[a]*A.m_pValues[b];
			b++;
			Ret.m_pValues[a+b] += m_pValues[a]*A.m_pValues[b];
			b++;
			Ret.m_pValues[a+b] += m_pValues[a]*A.m_pValues[b];
			b++;
			Ret.m_pValues[a+b] += m_pValues[a]*A.m_pValues[b];
			b++;
			Ret.m_pValues[a+b] += m_pValues[a]*A.m_pValues[b];
			b++;
			Ret.m_pValues[a+b] += m_pValues[a]*A.m_pValues[b];
			b++;
			Ret.m_pValues[a+b] += m_pValues[a]*A.m_pValues[b];
			b++;
			Ret.m_pValues[a+b] += m_pValues[a]*A.m_pValues[b];
			b++;
			Ret.m_pValues[a+b] += m_pValues[a]*A.m_pValues[b];
			b++;
			Ret.m_pValues[a+b] += m_pValues[a]*A.m_pValues[b];
			b++;
			Ret.m_pValues[a+b] += m_pValues[a]*A.m_pValues[b];
			b++;
			Ret.m_pValues[a+b] += m_pValues[a]*A.m_pValues[b];
			b++;
			Ret.m_pValues[a+b] += m_pValues[a]*A.m_pValues[b];
			b++;
			Ret.m_pValues[a+b] += m_pValues[a]*A.m_pValues[b];
			b++;
			Ret.m_pValues[a+b] += m_pValues[a]*A.m_pValues[b];
			b++;
			Ret.m_pValues[a+b] += m_pValues[a]*A.m_pValues[b];
			b++;
			Ret.m_pValues[a+b] += m_pValues[a]*A.m_pValues[b];
			b++;
			Ret.m_pValues[a+b] += m_pValues[a]*A.m_pValues[b];
			b++;
			Ret.m_pValues[a+b] += m_pValues[a]*A.m_pValues[b];
			b++;
			Ret.m_pValues[a+b] += m_pValues[a]*A.m_pValues[b];
			b++;
			Ret.m_pValues[a+b] += m_pValues[a]*A.m_pValues[b];
			b++;
			Ret.m_pValues[a+b] += m_pValues[a]*A.m_pValues[b];
			b++;
			Ret.m_pValues[a+b] += m_pValues[a]*A.m_pValues[b];
			b++;
		}
		for(;b<nEnd;b++)
			Ret.m_pValues[a+b] += m_pValues[a]*A.m_pValues[b];
	}
	Ret.m_bSign = !(m_bSign==A.m_bSign);
	int i;
	for(i=Ret.m_nValues-1;i>0;i--){
		if(Ret.m_pValues[i]>=FIXEDFLOAT_PARTMAX){
			Ret.m_pValues[i-1]+=Ret.m_pValues[i]/FIXEDFLOAT_PARTMAX;
			Ret.m_pValues[i]=Ret.m_pValues[i]%FIXEDFLOAT_PARTMAX;
		}
	}
	return Ret;
}
CFixedFloat CFixedFloat::Divide(const CFixedFloat &A) const
{
	CDecNumber T = ToText();
	CDecNumber N = A.ToText();
	CDecNumber K = T/N;
	char *sz = K.ToText();
	int i;
	for(i=0;sz[i];i++)
		if(sz[i]!='e' && sz[i]!='E' && isalpha(sz[i])){
			Beep(1000,10);
		}
	CFixedFloat Ret(sz);
	return Ret;
}
CFixedFloat CFixedFloat::Square() const
{
	CFixedFloat &this2 = const_cast<CFixedFloat &>(*this);
	CFixedFloat Ret;
	if(g_nMaxSignificant){
		for(;this2.m_nValues<g_nMaxSignificant;this2.m_nValues++)
			this2.m_pValues[this2.m_nValues]=0;
	}
	Ret.m_nValues = m_nValues+m_nValues;
	if(g_nMaxSignificant && Ret.m_nValues>g_nMaxSignificant)
		Ret.m_nValues=g_nMaxSignificant;
	else if(Ret.m_nValues>FIXEDFLOAT_ENTRIES)
		Ret.m_nValues=FIXEDFLOAT_ENTRIES;
	memset(Ret.m_pValues,0,sizeof(FIXEDFLOAT_TYPE)*Ret.m_nValues);
	int a, b;
	for(a=0;a<m_nValues;a++){
		int aa = a+a;
		if(aa<Ret.m_nValues)
			Ret.m_pValues[aa] += m_pValues[a]*m_pValues[a];
		int nEnd = m_nValues;
		if(a+nEnd>Ret.m_nValues)
			nEnd = Ret.m_nValues-a;
		int nEndLoop = nEnd;
		nEndLoop-=(nEndLoop-a-1)%32;
		for(b=a+1;b<nEndLoop;){
			Ret.m_pValues[a+b] += (m_pValues[a]*m_pValues[b])<<1;
			b++;
			Ret.m_pValues[a+b] += (m_pValues[a]*m_pValues[b])<<1;
			b++;
			Ret.m_pValues[a+b] += (m_pValues[a]*m_pValues[b])<<1;
			b++;
			Ret.m_pValues[a+b] += (m_pValues[a]*m_pValues[b])<<1;
			b++;
			Ret.m_pValues[a+b] += (m_pValues[a]*m_pValues[b])<<1;
			b++;
			Ret.m_pValues[a+b] += (m_pValues[a]*m_pValues[b])<<1;
			b++;
			Ret.m_pValues[a+b] += (m_pValues[a]*m_pValues[b])<<1;
			b++;
			Ret.m_pValues[a+b] += (m_pValues[a]*m_pValues[b])<<1;
			b++;
			Ret.m_pValues[a+b] += (m_pValues[a]*m_pValues[b])<<1;
			b++;
			Ret.m_pValues[a+b] += (m_pValues[a]*m_pValues[b])<<1;
			b++;
			Ret.m_pValues[a+b] += (m_pValues[a]*m_pValues[b])<<1;
			b++;
			Ret.m_pValues[a+b] += (m_pValues[a]*m_pValues[b])<<1;
			b++;
			Ret.m_pValues[a+b] += (m_pValues[a]*m_pValues[b])<<1;
			b++;
			Ret.m_pValues[a+b] += (m_pValues[a]*m_pValues[b])<<1;
			b++;
			Ret.m_pValues[a+b] += (m_pValues[a]*m_pValues[b])<<1;
			b++;
			Ret.m_pValues[a+b] += (m_pValues[a]*m_pValues[b])<<1;
			b++;
			Ret.m_pValues[a+b] += (m_pValues[a]*m_pValues[b])<<1;
			b++;
			Ret.m_pValues[a+b] += (m_pValues[a]*m_pValues[b])<<1;
			b++;
			Ret.m_pValues[a+b] += (m_pValues[a]*m_pValues[b])<<1;
			b++;
			Ret.m_pValues[a+b] += (m_pValues[a]*m_pValues[b])<<1;
			b++;
			Ret.m_pValues[a+b] += (m_pValues[a]*m_pValues[b])<<1;
			b++;
			Ret.m_pValues[a+b] += (m_pValues[a]*m_pValues[b])<<1;
			b++;
			Ret.m_pValues[a+b] += (m_pValues[a]*m_pValues[b])<<1;
			b++;
			Ret.m_pValues[a+b] += (m_pValues[a]*m_pValues[b])<<1;
			b++;
			Ret.m_pValues[a+b] += (m_pValues[a]*m_pValues[b])<<1;
			b++;
			Ret.m_pValues[a+b] += (m_pValues[a]*m_pValues[b])<<1;
			b++;
			Ret.m_pValues[a+b] += (m_pValues[a]*m_pValues[b])<<1;
			b++;
			Ret.m_pValues[a+b] += (m_pValues[a]*m_pValues[b])<<1;
			b++;
			Ret.m_pValues[a+b] += (m_pValues[a]*m_pValues[b])<<1;
			b++;
			Ret.m_pValues[a+b] += (m_pValues[a]*m_pValues[b])<<1;
			b++;
			Ret.m_pValues[a+b] += (m_pValues[a]*m_pValues[b])<<1;
			b++;
			Ret.m_pValues[a+b] += (m_pValues[a]*m_pValues[b])<<1;
			b++;
		}
		for(;b<nEnd;b++)
			Ret.m_pValues[a+b] += (m_pValues[a]*m_pValues[b])<<1;
	}
	Ret.m_bSign = 0;
	int i;
	for(i=Ret.m_nValues-1;i>0;i--){
		if(Ret.m_pValues[i]>=FIXEDFLOAT_PARTMAX){
			Ret.m_pValues[i-1]+=Ret.m_pValues[i]/FIXEDFLOAT_PARTMAX;
			Ret.m_pValues[i]=Ret.m_pValues[i]%FIXEDFLOAT_PARTMAX;
		}
	}
	return Ret;
}

CFixedFloat CFixedFloat::Double()
{
	int i;
	for(i=0;i<m_nValues;i++)
		m_pValues[i] = m_pValues[i]<<1;
	for(i=m_nValues-1;i>0;i--){
		if(m_pValues[i]>=FIXEDFLOAT_PARTMAX){
			m_pValues[i-1]+=m_pValues[i]/FIXEDFLOAT_PARTMAX;
			m_pValues[i]=m_pValues[i]%FIXEDFLOAT_PARTMAX;
		}
	}
	return *this;
}
CFixedFloat &CFixedFloat::AbsAdd(const CFixedFloat &ac, const CFixedFloat &bc)
{
	CFixedFloat &a = const_cast<CFixedFloat &>(ac);
	CFixedFloat &b = const_cast<CFixedFloat &>(bc);
	int as = a.m_bSign;
	a.m_bSign=0;
	int bs = b.m_bSign;
	b.m_bSign=0;
	*this = a+b;
	a.m_bSign=as;
	b.m_bSign=bs;
	return *this;
}
CFixedFloat &CFixedFloat::Abs()
{
	m_bSign=0;
	return *this;
}

BOOL CFixedFloat::operator >(const CFixedFloat &A) const
{
	if(!m_bSign && A.m_bSign)
		return TRUE;
	else if(m_bSign && !A.m_bSign)
		return FALSE;
	int i;
	int nLen = m_nValues;
	if(nLen>A.m_nValues)
		nLen=A.m_nValues;
	if(g_nMaxSignificant && nLen>g_nMaxSignificant)
		nLen=g_nMaxSignificant;
	for(i=0;i<nLen;i++){
		if(m_pValues[i]>A.m_pValues[i])
			return !m_bSign;
		else if(m_pValues[i]<A.m_pValues[i])
			return m_bSign;
	}
	if(g_nMaxSignificant && nLen==g_nMaxSignificant)
		return FALSE;
	if(m_nValues>A.m_nValues)
		return !m_bSign;
	else if(m_nValues<A.m_nValues)
		return m_bSign;
	return FALSE;
}
BOOL CFixedFloat::operator <(const CFixedFloat &A) const
{
	if(!m_bSign && A.m_bSign)
		return FALSE;
	else if(m_bSign && !A.m_bSign)
		return TRUE;
	int i;
	int nLen = m_nValues;
	if(nLen>A.m_nValues)
		nLen=A.m_nValues;
	if(g_nMaxSignificant && nLen>g_nMaxSignificant)
		nLen=g_nMaxSignificant;
	for(i=0;i<nLen;i++){
		if(m_pValues[i]<A.m_pValues[i])
			return !m_bSign;
		else if(m_pValues[i]>A.m_pValues[i])
			return m_bSign;
	}
	if(g_nMaxSignificant && nLen==g_nMaxSignificant)
		return FALSE;
	if(m_nValues<A.m_nValues)
		return !m_bSign;
	else if(m_nValues>A.m_nValues)
		return m_bSign;
	return FALSE;
}
BOOL CFixedFloat::operator ==(const CFixedFloat &A) const
{
	if(!m_bSign && A.m_bSign)
		return FALSE;
	else if(m_bSign && !A.m_bSign)
		return TRUE;
	int i;
	int nLen = m_nValues;
	if(nLen>A.m_nValues)
		nLen=A.m_nValues;
	if(g_nMaxSignificant && nLen>g_nMaxSignificant)
		nLen=g_nMaxSignificant;
	for(i=0;i<nLen;i++){
		if(m_pValues[i]!=A.m_pValues[i])
			return FALSE;
	}
	if(g_nMaxSignificant && nLen==g_nMaxSignificant)
		return TRUE;
	if(m_nValues<A.m_nValues)
		return FALSE;
	else if(m_nValues>A.m_nValues)
		return FALSE;
	return TRUE;
}

CFixedFloat &CFixedFloat::operator =(const CFixedFloat &bi)
{
	m_bSign = bi.m_bSign;
	m_nValues = bi.m_nValues;
	memcpy(m_pValues,bi.m_pValues,sizeof(FIXEDFLOAT_TYPE)*bi.m_nValues);
	return *this;
}
CFixedFloat &CFixedFloat::operator =(const char *sz)
{
	Parse(sz);
	return *this;
}
CFixedFloat &CFixedFloat::operator =(int a)
{
	if(a>=0){
		m_bSign=FALSE;
		m_pValues[0]=a;
	}
	else{
		m_bSign=TRUE;
		m_pValues[0]=-a;
	}
	m_nValues=1;
	return *this;
}
CFixedFloat &CFixedFloat::operator =(double a)
{
	m_bSign=(a<0);
	if(a<0)
		a=-a;
	m_nValues=0;
	int nStart=0;
	while((!g_nMaxSignificant || m_nValues<g_nMaxSignificant) && m_nValues<FIXEDFLOAT_ENTRIES && a){
		m_pValues[m_nValues]=(FIXEDFLOAT_TYPE)a;
		a-=m_pValues[m_nValues];
		a*=FIXEDFLOAT_PARTMAX;
		if(!nStart && m_pValues[m_nValues])
			nStart=1;
		else if(nStart)
			nStart++;
		m_nValues++;
		if(nStart>3)
			break;
	}
	return *this;
}

CFixedFloat CFixedFloat::operator *(const CFixedFloat &A) const
{
	if(this==&A)
		return Square();
	return Multiply(A);
}
CFixedFloat CFixedFloat::operator /(const CFixedFloat &A) const
{
	return Divide(A);
}
CFixedFloat CFixedFloat::operator +(const CFixedFloat &A) const
{
	return Add(A);
}
CFixedFloat CFixedFloat::operator -(const CFixedFloat &A) const
{
	return Subtract(A);
}
CFixedFloat CFixedFloat::operator -() const
{
	CFixedFloat Ret = *this;
	Ret.m_bSign=1-Ret.m_bSign;
	return Ret;
}

CFixedFloat &CFixedFloat::operator*=(const CFixedFloat &B)
{
	if(this==&B)
		*this = Square();
	else
		*this = Multiply(B);
	return *this;
}
CFixedFloat &CFixedFloat::operator/=(const CFixedFloat &B)
{
	*this = Divide(B);
	return *this;
}
CFixedFloat &CFixedFloat::operator+=(const CFixedFloat &B)
{
	*this = Add(B);
	return *this;
}
CFixedFloat &CFixedFloat::operator-=(const CFixedFloat &B)
{
	*this = Subtract(B);
	return *this;
}
CFixedFloat &CFixedFloat::operator*=(int nA)
{
	CFixedFloat A(nA);
	*this = Multiply(A);
	return *this;
}
CFixedFloat &CFixedFloat::operator/=(int nA)
{
	CFixedFloat A(nA);
	*this = Divide(A);
	return *this;
}
CFixedFloat &CFixedFloat::operator+=(int nA)
{
	CFixedFloat A(nA);
	*this = Add(A);
	return *this;
}
CFixedFloat &CFixedFloat::operator-=(int nA)
{
	CFixedFloat A(nA);
	*this = Subtract(A);
	return *this;
}

BOOL operator==(const CFixedFloat &A,int nB)
{
	CFixedFloat B(nB);
	return B==A;
}
BOOL operator==(int nB,const CFixedFloat &A)
{
	CFixedFloat B(nB);
	return B==A;
}
BOOL operator>(const CFixedFloat &A,int nB)
{
	CFixedFloat B(nB);
	return A>B;
}
BOOL operator>(int nB,const CFixedFloat &A)
{
	CFixedFloat B(nB);
	return B>A;
}
BOOL operator>(const CFixedFloat &A,double nB)
{
	CFixedFloat B(nB);
	return A>B;
}
BOOL operator>(double nB,const CFixedFloat &A)
{
	CFixedFloat B(nB);
	return B>A;
}
BOOL operator<(const CFixedFloat &A,int nB)
{
	CFixedFloat B(nB);
	return A<B;
}
BOOL operator<(int nB,const CFixedFloat &A)
{
	CFixedFloat B(nB);
	return B<A;
}
BOOL operator<(const CFixedFloat &A,double nB)
{
	CFixedFloat B(nB);
	return A<B;
}
BOOL operator<(double nB,const CFixedFloat &A)
{
	CFixedFloat B(nB);
	return B<A;
}

CFixedFloat operator*(const CFixedFloat &A,int nB)
{
	CFixedFloat B(nB);
	return A*B;
}
CFixedFloat operator*(int nB,const CFixedFloat &A)
{
	CFixedFloat B(nB);
	return A*B;
}
CFixedFloat operator*(const CFixedFloat &A,long nB)
{
	CFixedFloat B((int)nB);
	return A*B;
}
CFixedFloat operator*(long nB,const CFixedFloat &A)
{
	CFixedFloat B((int)nB);
	return A*B;
}
CFixedFloat operator*(const CFixedFloat &A,double nB)
{
	CFixedFloat B(nB);
	return A*B;
}
CFixedFloat operator*(double nB,const CFixedFloat &A)
{
	CFixedFloat B(nB);
	return A*B;
}
CFixedFloat operator+(const CFixedFloat &a,int nB)
{
	CFixedFloat b(nB);
	return a+b;
}
CFixedFloat operator+(int nB,const CFixedFloat &a)
{
	CFixedFloat b(nB);
	return a+b;
}
CFixedFloat operator+(const CFixedFloat &a,double nB)
{
	CFixedFloat b(nB);
	return a+b;
}
CFixedFloat operator+(double nB,const CFixedFloat &a)
{
	CFixedFloat b(nB);
	return a+b;
}
CFixedFloat operator/(const CFixedFloat &a,int nB)
{
	CFixedFloat b(nB);
	return a/b;
}
CFixedFloat operator/(int nB,const CFixedFloat &a)
{
	CFixedFloat b(nB);
	return b/a;
}
CFixedFloat operator/(const CFixedFloat &a,double nB)
{
	CFixedFloat b(nB);
	return a/b;
}
CFixedFloat operator/(double nB,const CFixedFloat &a)
{
	CFixedFloat b(nB);
	return b/a;
}
CFixedFloat operator-(const CFixedFloat &a,int nB)
{
	CFixedFloat b(nB);
	return a-b;
}
CFixedFloat operator-(int nB,const CFixedFloat &a)
{
	CFixedFloat b(nB);
	return b-a;
}
CFixedFloat operator-(const CFixedFloat &a,double nB)
{
	CFixedFloat b(nB);
	return a-b;
}
CFixedFloat operator-(double nB,const CFixedFloat &a)
{
	CFixedFloat b(nB);
	return b-a;
}

int operator+=(int &nB,const CFixedFloat &A)
{
	CFixedFloat B(nB);
	B+=A;
	return B.ToInt();
}
int operator+=(double &nB,const CFixedFloat &A)
{
	CFixedFloat B(nB);
	B+=A;
	return B.ToInt();
}
int operator+=(const CFixedFloat &A,double &nB)
{
	CFixedFloat B(nB);
	B+=A;
	return B.ToInt();
}
CFixedFloat operator*(const CFixedFloat &A,const CFixedFloat &B)
{
	CFixedFloat Ret = A;
	Ret*=B;
	return Ret;
}
