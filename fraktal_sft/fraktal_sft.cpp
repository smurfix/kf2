// Kalles Fraktaler 2
//
// © 2014-2015 Karl Runmo ,runmo@hotmail.com
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

#include "fraktal_sft.h"
#include "../common/parallell.h"
#include "../common/StringVector.h"
#include "../common/getimage.h"
#include <float.h>
#include <malloc.h>
#include "complex.h"
#include <iostream>
#include "../common/bitmap.h"
#include "../formula/formula.h"
#include "colour.h"

double g_real=1;
double g_imag=1;
double g_SeedR=0;
double g_SeedI=0;
double g_FactorAR=1;
double g_FactorAI=0;
#define _abs(a) ((_abs_val=(a))>0?_abs_val:-_abs_val)
double pi = 3.14159265;
#define _SMOOTH_COLORS_
#define SMOOTH_TOLERANCE 256
int g_nLDBL = 600;
int g_nEXP = 4900;
int g_nRefZero = 3;
#define APPROX_GRID 19
#define TERM4
#define TERM5
//#define TERM6
//#define TERM7
int g_nAddRefX, g_nAddRefY;

double g_Degree = 0;
BOOL g_LDBL = TRUE;
void(SetParts)(double,double);
int(SizeOfLD)();
int(Version)();
void *(AllocateArray)(int nSize);
void(ReleaseArray)(void *p);
void(AssignInt)(void *p, int nValue);
void(AssignDouble)(void *p, double nDouble);
void(AssignLD)(void *p, void *ld);
void(AssignFloatExp)(void *p, floatexp *fe);
void(ToInt)(void *p, int *pnValue);
void(ToDouble)(void *p, double *pnDouble);
void(ToFloatExp)(void *p, floatexp *pnFloatExp);
void(Multiply)(void *a, void *b, void *ret);
double(SquareAdd)(void *a, void *b);
void(Divide)(void *a, void *b, void *ret);
void(Add)(void *a, void *b, void *ret);
void(Subtract)(void *a, void *b, void *ret);
int(GT)(void *a, void *b);
int(LT)(void *a, void *b);
int(Equal)(void *a, void *b);
void(Print)(void *a, char *szRet);
void(DLLConvertFromFixedFloat)(void *p, const mpf_t value);
#define ConvertFromFixedFloat(p,x) DLLConvertFromFixedFloat((p),(x).m_f.backend().data())
int(Perturbation4)(int antal, void *pdxr, void *pdxi, void* pDr, void*pDi, void* pD0r, void*pD0i, double *ptest1, double *ptest2, int m_nBailout2, int m_nMaxIter, double *db_z, BOOL *pGlitch);
int(Perturbation_3rd)(int antal, void *pdxr, void *pdxi, void* pDr, void*pDi, void* pD0r, void*pD0i, double *ptest1, double *ptest2, int m_nBailout2, int m_nMaxIter, double *db_z, BOOL *pGlitch);
int(Perturbation_4th)(int antal, void *pdxr, void *pdxi, void* pDr, void*pDi, void* pD0r, void*pD0i, double *ptest1, double *ptest2, int m_nBailout2, int m_nMaxIter, double *db_z, BOOL *pGlitch);
int(Perturbation_5th)(int antal, void *pdxr, void *pdxi, void* pDr, void*pDi, void* pD0r, void*pD0i, double *ptest1, double *ptest2, int m_nBailout2, int m_nMaxIter, double *db_z, BOOL *pGlitch);
int(Perturbation_6th)(int antal, void *pdxr, void *pdxi, void* pDr, void*pDi, void* pD0r, void*pD0i, double *ptest1, double *ptest2, int m_nBailout2, int m_nMaxIter, double *db_z, BOOL *pGlitch);
int(Perturbation_7th)(int antal, void *pdxr, void *pdxi, void* pDr, void*pDi, void* pD0r, void*pD0i, double *ptest1, double *ptest2, int m_nBailout2, int m_nMaxIter, double *db_z, BOOL *pGlitch);
int(Perturbation_8th)(int antal, void *pdxr, void *pdxi, void* pDr, void*pDi, void* pD0r, void*pD0i, double *ptest1, double *ptest2, int m_nBailout2, int m_nMaxIter, double *db_z, BOOL *pGlitch);
int(Perturbation_9th)(int antal, void *pdxr, void *pdxi, void* pDr, void*pDi, void* pD0r, void*pD0i, double *ptest1, double *ptest2, int m_nBailout2, int m_nMaxIter, double *db_z, BOOL *pGlitch);
int(Perturbation_10th)(int antal, void *pdxr, void *pdxi, void* pDr, void*pDi, void* pD0r, void*pD0i, double *ptest1, double *ptest2, int m_nBailout2, int m_nMaxIter, double *db_z, BOOL *pGlitch);
int(Perturbation_Var)(int antal, void *pdxr, void *pdxi, void* pDr, void*pDi, void* pD0r, void*pD0i, double *ptest1, double *ptest2, int m_nBailout2, int m_nMaxIter, double *db_z, BOOL *pGlitch, int nPower, int *nExpConsts);
int(LDBL_MandelCalc)(int nFractal, int nPower, int antal, void *pdxr, void *pdxi, void* pDr, void*pDi, void* pD0r, void*pD0i, double *ptest1, double *ptest2, int m_nBailout2, int m_nMaxIter, double *db_z, BOOL *pGlitch,double dbFactorAR,double dbFactorAI);

void ErrorText()
{
	TCHAR szMsgBuf[500];

	DWORD dwError = GetLastError();
	FormatMessage(
		FORMAT_MESSAGE_FROM_SYSTEM,
		NULL,
		dwError,
		MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
		szMsgBuf,
		500,
		NULL );
	MessageBox(NULL,szMsgBuf,"Error",MB_ICONSTOP|MB_OK);
}

CFraktalSFT::CFraktalSFT()
{
#ifdef KF_OPENCL
	clid = -1;
	cl = NULL;
#endif

	m_bTexture=FALSE;
	m_nImgMerge=1;
	m_nImgPower=200;
	m_nImgRatio=100;
	*m_szTexture=0;

	g_bShowGlitches=TRUE;
	m_bSlopes = FALSE;
	m_nSlopePower = 50;
	m_nSlopeRatio = 50;
	m_nSlopeAngle = 45;
	m_bNoPostWhenDone = FALSE;
	m_scRatio.cx = 640;
	m_scRatio.cy = 360;
	m_nFractalType = 0;
	m_bMirrored = 0;
	m_bMW = 0;
	m_bBlend = 0;
	m_bNoGlitchDetection = FALSE;
	m_nPrevPower = m_nPower = 2;
	m_bLowTolerance = TRUE;
	m_bGlitchLowTolerance = FALSE;
	m_nMaxOldGlitches = 69;

	m_bAutoTerms = TRUE;
	m_nTerms = 10;
	m_APr = new floatexp[m_nTerms];
	m_APi = new floatexp[m_nTerms];

	m_hMutex = CreateMutex(NULL, 0, NULL);
	m_bRunning = FALSE;
	m_szPosition = NULL;
	m_rstart = -2;
	m_istart = -2;
	m_rstop = 2;
	m_istop = 2;
	m_dxr = NULL;
	m_dxi = NULL;
	m_ldxr = NULL;
	m_ldxi = NULL;
	m_nZoom = 0;
	m_nPixels = NULL;
	m_nTrans = NULL;
	m_bTrans = TRUE;
	m_bITrans = FALSE;
	m_bAddReference = FALSE;
	m_bNoApproximation = FALSE;
	m_nXPrev = m_nYPrev = -1;
	m_nSizeImage = -1;
	m_nTotal = -1;
	m_pDX = NULL;
	m_pDY = NULL;
	m_DX = NULL;
	m_DY = NULL;
	m_lDX = NULL;
	m_lDY = NULL;
	m_pnExpConsts = NULL;

	m_nBailout = SMOOTH_BAILOUT;
	m_nBailout2 = m_nBailout*m_nBailout;
	m_nSmoothMethod = 0;
	m_nColorMethod = ColorMethod_Standard;
	m_nDifferences = Differences_Traditional;

	m_db_dxr = NULL;
	m_db_dxi = NULL;

	m_lpBits = NULL;
	m_row = 0;
	m_nMaxIter = 200;
	m_bReuseRef = FALSE;
	m_nIterDiv = 1;
	memset(m_pOldGlitch, -1, sizeof(m_pOldGlitch));

	m_nInflections=0;
	m_pInflections=NULL;

	GenerateColors(128, 1);
	ApplyColors();
}
void CFraktalSFT::GenerateColors(int nParts, int nSeed)
{
	m_nParts = nParts;
	m_nSeed = nSeed;
	if (m_nSeed == -1)
		m_nSeed = GetTickCount();
	srand(m_nSeed);
	m_cKeys[0].r = m_cKeys[0].g = m_cKeys[0].b = m_cKeys[1024].r = m_cKeys[1024].g = m_cKeys[1024].b = 0;
	int i;
	for (i = (m_nSeed == 1 ? 1 : 0); i<1024; i++){
		m_cKeys[i].r = rand() % 256;
		m_cKeys[i].g = rand() % 256;
		m_cKeys[i].b = rand() % 256;
	}
	m_cKeys[m_nParts].r = m_cKeys[0].r;
	m_cKeys[m_nParts].g = m_cKeys[0].g;
	m_cKeys[m_nParts].b = m_cKeys[0].b;
}
int MakePrime(int n)
{
	int i;
	int nE = n / 2;
	while (1){
		BOOL bDone = TRUE;
		for (i = 2; i<nE; i++)
		if (n%i == 0){
			bDone = FALSE;
			break;
		}
		if (bDone)
			break;
		n++;
	}
	return n;
}
void CFraktalSFT::AddWave(int nColor, int nP, int nS)
{
	srand(GetTickCount());
	int nPeriod;
	if (nP == -1){
		nPeriod = rand() % (m_nParts<4 ? m_nParts / 2 : m_nParts);
		if (nPeriod == 0)
			nPeriod = 1;
	}
	else
		nPeriod = nP;
	if (nP == -1)
		nPeriod = MakePrime(nPeriod);
	int nStart;
	if (nS == -1)
		nStart = rand() % nPeriod;
	else
		nStart = nS;
	int i;
	for (i = 0; i<m_nParts; i++){
		int val = 127 + 127 * sin((double)(i + nStart) * 2 * pi*((double)nPeriod / (double)m_nParts));
		switch (nColor){
		case 0:
			m_cKeys[i].r = val;
			if (nP == 0)
				m_cKeys[i].r = 0;
			break;
		case 1:
			m_cKeys[i].g = val;
			if (nP == 0)
				m_cKeys[i].g = 0;
			break;
		case 2:
			m_cKeys[i].b = val;
			if (nP == 0)
				m_cKeys[i].b = 0;
			break;
		case 3:
			if (nP){
				m_cKeys[i].r = (int)(m_cKeys[i].r + 1 * val) / 2;
				m_cKeys[i].g = (int)(m_cKeys[i].g + 1 * val) / 2;
				m_cKeys[i].b = (int)(m_cKeys[i].b + 1 * val) / 2;
			}
			break;
		}
	}
}
void CFraktalSFT::GenerateColors2(int nParts, int nSeed, int nWaves)
{
	m_nParts = nParts;
	m_nSeed = nSeed;
	if (m_nSeed == -1)
		m_nSeed = GetTickCount();
	srand(m_nSeed);
	int i;
	for (i = 0; i<1024; i++)
		m_cKeys[i].r = m_cKeys[i].g = m_cKeys[i].b = 0;
	int nW;
	char szTmp[30];
	CStringTable stPeriods;
	for (nW = 0; nW<nWaves; nW++){
		int nTests, nPeriod;
		for (nTests = 0; nTests<20; nTests++){
			nPeriod = rand() % (nParts>4 ? nParts / 4 : nParts);
			if (nPeriod == 0)
				nPeriod = 1;
			nPeriod = MakePrime(nPeriod);
			itoa(nPeriod, szTmp, 10);
			if (stPeriods.FindString(0, szTmp))
				continue;
			stPeriods.AddRow();
			stPeriods.AddString(stPeriods.GetCount() - 1, szTmp);
		}
		int nStart = rand() % nPeriod;
		int nColor = nW % 4;
		if (nW<4){
			for (i = 0; i<1024; i++){
				int val = 127 + 127 * sin((double)(i + nStart) * 2 * pi*((double)nPeriod / (double)m_nParts));
				switch (nColor){
				case 0:
					m_cKeys[i].r += (int)(m_cKeys[i].r + val) % 256;
					break;
				case 1:
					m_cKeys[i].g += (int)(m_cKeys[i].g + val) % 256;
					break;
				case 2:
					m_cKeys[i].b += (int)(m_cKeys[i].b + val) % 256;
					break;
				case 3:
					m_cKeys[i].r = (int)(m_cKeys[i].r + val) / 2;
					m_cKeys[i].g = (int)(m_cKeys[i].g + val) / 2;
					m_cKeys[i].b = (int)(m_cKeys[i].b + val) / 2;
					break;
				}
			}
		}
		else{
			for (i = 0; i<1024; i++){
				int val = 127 + 127 * sin((double)(i + nStart) * 2 * pi*((double)nPeriod / (double)m_nParts));
				switch (nColor){
				case 0:
					m_cKeys[i].r = (int)(m_cKeys[i].r + val) / 2;
					break;
				case 1:
					m_cKeys[i].g = (int)(m_cKeys[i].g + val) / 2;
					break;
				case 2:
					m_cKeys[i].b = (int)(m_cKeys[i].b + val) / 2;
					break;
				case 3:
					m_cKeys[i].r = (int)(m_cKeys[i].r + val) / 2;
					m_cKeys[i].g = (int)(m_cKeys[i].g + val) / 2;
					m_cKeys[i].b = (int)(m_cKeys[i].b + val) / 2;
					break;
				}
			}
		}
	}
}
void CFraktalSFT::ChangeNumOfColors(int nParts)
{
	m_nParts = nParts;
}
int CFraktalSFT::GetNumOfColors()
{
	return m_nParts;
}
void CFraktalSFT::ApplyIterationColors()
{
	if (m_nPixels && m_lpBits){
		int nMin, nMax;
		GetIterations(nMin, nMax);
		if (nMin == nMax)
			nMax = nMin + 1;
		int x, y;
		for (x = 0; x<m_nX; x++){
			for (y = 0; y<m_nY; y++){
				int nIndex = x * 3 + (m_bmi->biHeight - 1 - y)*m_row;
				m_lpBits[nIndex] = 255 * (m_nPixels[x][y] - nMin) / (nMax - nMin);
				m_lpBits[nIndex + 1] = m_lpBits[nIndex];
				m_lpBits[nIndex + 2] = m_lpBits[nIndex];
			}
		}
	}
}
void CFraktalSFT::ApplySmoothColors()
{
	if (m_nTrans && m_lpBits){
		int x, y;
		for (x = 0; x<m_nX; x++){
			for (y = 0; y<m_nY; y++){
				float tr = m_nTrans[x][y];
				int nIndex = x * 3 + (m_bmi->biHeight - 1 - y)*m_row;
				m_lpBits[nIndex] = 255 * tr;
				m_lpBits[nIndex + 1] = 255 * tr;
				m_lpBits[nIndex + 2] = 255 * tr;
			}
		}
	}
}

void HSVToRGB(double hue, double sat, double bri, COLOR14 &cPos)
{
	hsv a;
	a.h = hue;
	a.s = sat;
	a.v = bri;
	srgb c = hsv2rgb(a);
	c.r *= 255.0;
	c.g *= 255.0;
	c.b *= 255.0;
	cPos.r = (byte) c.r;
	cPos.g = (byte) c.g;
	cPos.b = (byte) c.b;
}

HBITMAP CFraktalSFT::ShrinkBitmap(HBITMAP bmSrc,int nNewWidth,int nNewHeight,BOOL bHalfTone)
{
	HDC hDC = GetDC(NULL);
	BITMAP bm;
	GetObject(bmSrc,sizeof(BITMAP),&bm);
	HDC dcSrc = CreateCompatibleDC(hDC);
	HBITMAP bmOldSrc = (HBITMAP)SelectObject(dcSrc,bmSrc);
	HDC dcDst = CreateCompatibleDC(hDC);
	HBITMAP bmDst = create_bitmap(hDC,nNewWidth,nNewHeight);
	HBITMAP bmOldDst = (HBITMAP)SelectObject(dcDst,bmDst);
	if(bHalfTone)
		SetStretchBltMode(dcDst,HALFTONE);
	else
		SetStretchBltMode(dcDst,COLORONCOLOR);
	StretchBlt(dcDst,0,0,nNewWidth,nNewHeight,dcSrc,0,0,bm.bmWidth,bm.bmHeight,SRCCOPY);
	SelectObject(dcDst,bmOldDst);
	SelectObject(dcSrc,bmOldSrc);
	DeleteDC(dcDst);
	DeleteDC(dcSrc);
	ReleaseDC(NULL,hDC);
	return bmDst;
}
void CFraktalSFT::SetTexture(int nIndex, int x, int y)
{
	int nImgOffs=m_nImgPower/64;
	if(m_lpTextureBits==NULL){
		HBITMAP bmBitmapIn = GetImage(m_szTexture);
		SIZE scImg;
		scImg.cx = m_nX+(nImgOffs+m_nImgPower)/64;
		scImg.cy = m_nY+(m_nImgPower+nImgOffs)/64;
		HBITMAP bmBitmap = ShrinkBitmap(bmBitmapIn,scImg.cx,scImg.cy);

		HDC hDC = GetDC(NULL);
		memset(&m_bmiBkg,0,sizeof(BITMAPINFOHEADER));
		m_bmiBkg.biSize=sizeof(BITMAPINFOHEADER);
		if(!GetDIBits(hDC,bmBitmap,0,0,NULL,(LPBITMAPINFO)&m_bmiBkg,DIB_RGB_COLORS))
			Beep(1000,10);
		m_bmiBkg.biCompression=m_bmiBkg.biClrUsed=m_bmiBkg.biClrImportant=0;
		m_bmiBkg.biBitCount = 24;
		m_rowBkg = ((((m_bmiBkg.biWidth*(DWORD)m_bmiBkg.biBitCount)+31)&~31) >> 3);
		m_bmiBkg.biSizeImage=m_rowBkg*m_bmiBkg.biHeight;
		m_lpTextureBits = new BYTE[m_bmiBkg.biSizeImage];
		if(!GetDIBits(hDC,bmBitmap,0,m_bmiBkg.biHeight,m_lpTextureBits,
				(LPBITMAPINFO)&m_bmiBkg,DIB_RGB_COLORS))
			Beep(1000,10);
		DeleteObject(bmBitmap);
		DeleteObject(bmBitmapIn);
		ReleaseDC(NULL,hDC);
	}
	double p1,p2;

	double diffx, diffy;
	if(x){
		p1 = (double)m_nPixels[x - 1][y] + (double)1 - m_nTrans[x - 1][y];
		p2 = (double)m_nPixels[x][y] + (double)1 - m_nTrans[x][y];
	}
	else if(x<m_nX-1){
		p1 = (double)m_nPixels[x][y] + (double)1 - m_nTrans[x][y];
		p2 = (double)m_nPixels[x+1][y] + (double)1 - m_nTrans[x+1][y];
	}
	else
		p1=p2 = (double)m_nPixels[x][y] + (double)1 - m_nTrans[x][y];
	diffx = p1-p2;

	if(y){
		p1 = (double)m_nPixels[x][y-1] + (double)1 - m_nTrans[x][y-1];
		p2 = (double)m_nPixels[x][y] + (double)1 - m_nTrans[x][y];
	}
	else if(y<m_nY-1){
		p1 = (double)m_nPixels[x][y] + (double)1 - m_nTrans[x][y];
		p2 = (double)m_nPixels[x][y+1] + (double)1 - m_nTrans[x][y+1];
	}
	else
		p1=p2 = (double)m_nPixels[x][y] + (double)1 - m_nTrans[x][y];
	diffy = p1-p2;

	double diff = diffx*m_nSlopeX + diffy*m_nSlopeY;
	//p1 = (double)Bmps.ppBits[y][x] + (double)1-Bmps.ppTrans[y][x];
	//p1 = (double)Bmps.ppBits[y][x] + (double)1-Bmps.ppTrans[y][x] - g_nPrevMinIter;
	p1 = 1;
	diff = (p1+diff)/p1;
	diffx = (p1+diffx)/p1;
	diffy = (p1+diffy)/p1;

	double dbScale=1;
	double nImgPower = m_nImgPower*dbScale;
	diff = pow(diff,nImgPower);
	diffx = pow(diffx,nImgPower);
	diffy = pow(diffy,nImgPower);
	int nY=y;
	int nX=x;
	if(diff>1){
		diff = (atan(diff)-pi/4)/(pi/4);
		diff=diff*(double)m_nImgRatio/100;;
	}
	else{
		diff=1/diff;
		diff = (atan(diff)-pi/4)/(pi/4);
		diff=diff*(double)m_nImgRatio/100;;
	}
	if(diffy>1){
		diffy = (atan(diffy)-pi/4)/(pi/4);
		diffy=diffy*(double)m_nImgRatio/100;;
		nY = y+nImgOffs + nImgPower*diffy;
	}
	else{
		diffy=1/diffy;
		diffy = (atan(diffy)-pi/4)/(pi/4);
		diffy=diffy*(double)m_nImgRatio/100;;
		nY = y+nImgOffs - nImgPower*diffy;
	}
	if(diffx>1){
		diffx = (atan(diffx)-pi/4)/(pi/4);
		diffx=diffx*(double)m_nImgRatio/100;;
		nX = x+nImgOffs + nImgPower*diffx;
	}
	else{
		diffx=1/diffx;
		diffx = (atan(diffx)-pi/4)/(pi/4);
		diffx=diffx*(double)m_nImgRatio/100;;
		nX = x+nImgOffs - nImgPower*diffx;
	}

	int mr = (m_bmiBkg.biWidth)/2+nImgOffs/2;
	int mi = (m_bmiBkg.biHeight)/2+nImgOffs/2;
	nX = (nX - mr)/dbScale + mr;
	nY = (nY - mi)/dbScale + mi;

	if(nY<0)
		nY=0;
	else if(nY>m_bmiBkg.biHeight-1)
		nY=m_bmiBkg.biHeight-1;
	if(nX<0)
		nX=0;
	else if(nX>m_bmiBkg.biWidth-1)
		nX=m_bmiBkg.biWidth-1;
	int nIndexBkg = nX*3 + (m_bmiBkg.biHeight-1-nY)*m_rowBkg;
	m_lpBits[nIndex] = m_lpTextureBits[nIndexBkg]*m_nImgMerge + m_lpBits[nIndex]*(1-m_nImgMerge);
	m_lpBits[nIndex+1] = m_lpTextureBits[nIndexBkg+1]*m_nImgMerge + m_lpBits[nIndex+1]*(1-m_nImgMerge);
	m_lpBits[nIndex+2] = m_lpTextureBits[nIndexBkg+2]*m_nImgMerge + m_lpBits[nIndex+2]*(1-m_nImgMerge);
}

void CFraktalSFT::SetColor(int nIndex, int nIter, double offs, int x, int y)
{
	srgb s;
	if (nIter<0 || (!g_bShowGlitches && offs==2))
		return;
	if (nIter == m_nMaxIter)
	{
		s.r = 0;
		s.g = 0;
		s.b = 0;
	}
	else{
		double iter = (double)nIter + (double)1 - offs;
		/*		if(1){//DE
		double p1, p2;
		if(x){
		p1 = (double)m_nPixels[x-1][y] + (double)1-m_nTrans[x-1][y];
		p2 = (double)m_nPixels[x][y] + (double)1-m_nTrans[x][y];
		}
		else if(x<m_nX-1){
		p1 = (double)m_nPixels[x][y] + (double)1-m_nTrans[x][y];
		p2 = (double)m_nPixels[x+1][y] + (double)1-m_nTrans[x+1][y];
		}
		else
		p1=p2 = (double)m_nPixels[x][y] + (double)1-m_nTrans[x][y];
		iter = (p1>p2?p1-p2:p2-p1);
		}
		*/
		if (m_nColorMethod == ColorMethod_SquareRoot){
			iter = sqrt(iter);
		}
		else if (m_nColorMethod == ColorMethod_CubicRoot){
			iter = pow(iter, (double)1 / (double)3);
		}
		else if (m_nColorMethod == ColorMethod_Logarithm){
			iter = log(iter);
		}
		else if (m_nColorMethod == ColorMethod_Stretched){
			int nMin, nMax;
			GetIterations(nMin, nMax,NULL,NULL,TRUE);
			iter = (double)1024 * ((double)iter - (double)nMin) / ((double)nMax - (double)nMin);
		}
		else if (m_nColorMethod == ColorMethod_DistanceLinear ||
		         m_nColorMethod == ColorMethod_DEPlusStandard ||
		         m_nColorMethod == ColorMethod_DistanceLog ||
		         m_nColorMethod == ColorMethod_DistanceSqrt){
			iter=0;
			// load 3x3 stencil around the pixel
			int X = m_nX - 1;
			int Y = m_nY - 1;
			static const double ninf = 1.0 / 0.0;
			double p[3][3] = { { ninf, ninf, ninf }, { ninf, ninf, ninf }, { ninf, ninf, ninf } };
			if (0 < x && 0 < y) p[0][0] = m_nPixels[x - 1][y - 1] + 1.0 - m_nTrans[x - 1][y - 1];
			if (0 < x         ) p[0][1] = m_nPixels[x - 1][y    ] + 1.0 - m_nTrans[x - 1][y    ];
			if (0 < x && y < Y) p[0][2] = m_nPixels[x - 1][y + 1] + 1.0 - m_nTrans[x - 1][y + 1];
			if (         0 < y) p[1][0] = m_nPixels[x    ][y - 1] + 1.0 - m_nTrans[x    ][y - 1];
			                    p[1][1] = m_nPixels[x    ][y    ] + 1.0 - m_nTrans[x    ][y    ];
			if (         y < Y) p[1][2] = m_nPixels[x    ][y + 1] + 1.0 - m_nTrans[x    ][y + 1];
			if (x < X && 0 < y) p[2][0] = m_nPixels[x + 1][y - 1] + 1.0 - m_nTrans[x + 1][y - 1];
			if (x < X         ) p[2][1] = m_nPixels[x + 1][y    ] + 1.0 - m_nTrans[x + 1][y    ];
			if (x < X && y < Y) p[2][2] = m_nPixels[x + 1][y + 1] + 1.0 - m_nTrans[x + 1][y + 1];
			// reflect at boundaries if necessary
			// this will break (result is infinite or NaN) for image size of 1 pixel
			p[1][1] *= 2.0;
			if (ninf == p[0][0]) p[0][0] = p[1][1] - p[2][2];
			if (ninf == p[0][1]) p[0][1] = p[1][1] - p[2][1];
			if (ninf == p[0][2]) p[0][2] = p[1][1] - p[2][0];
			if (ninf == p[1][0]) p[1][0] = p[1][1] - p[1][2];
			if (ninf == p[1][2]) p[1][2] = p[1][1] - p[1][0];
			if (ninf == p[2][0]) p[2][0] = p[1][1] - p[0][2];
			if (ninf == p[2][1]) p[2][1] = p[1][1] - p[0][1];
			if (ninf == p[2][2]) p[2][2] = p[1][1] - p[0][0];
			p[1][1] *= 0.5;
			// do the differencing
			switch (m_nDifferences)
			{
			case Differences_Central3x3:
				{
					// gerrit's central difference formula
					double gx = (p[2][1] - p[0][1]) * 0.5;
					double gy = (p[1][2] - p[1][0]) * 0.5;
					double g1 = (p[2][2] - p[0][0]) * 0.35355339059327373; // 1/(2 sqrt(2))
					double g2 = (p[2][0] - p[2][0]) * 0.35355339059327373;
					double g = sqrt(0.5 * (gx*gx + gy*gy + g1*g1 + g2*g2));
					iter = g * 2.8284271247461903;
				}
				break;
			case Differences_Forward3x3:
				{
					// forward differencing in 8 directions from the target point
					double gx0 = (p[0][1] - p[1][1]);
					double gx2 = (p[2][1] - p[1][1]);
					double gy0 = (p[1][0] - p[1][1]);
					double gy2 = (p[1][2] - p[1][1]);
					double gu0 = (p[0][0] - p[1][1]) * 0.7071067811865475; // 1/sqrt(2)
					double gu2 = (p[2][2] - p[1][1]) * 0.7071067811865475;
					double gv0 = (p[2][0] - p[1][1]) * 0.7071067811865475;
					double gv2 = (p[0][2] - p[1][1]) * 0.7071067811865475;
					double g = sqrt(0.25 * (gx0*gx0 + gx2*gx2 + gy0*gy0 + gy2*gy2 + gu0*gu0 + gu2*gu2 + gv0*gv0 + gv2*gv2));
					iter = g * 2.8284271247461903;
				}
				break;
			case Differences_Diagonal2x2:
				{
					// forward differencing in 2 diagonals of a 2x2 substencil
					double gu = (p[0][0] - p[1][1]) * 0.7071067811865475; // 1/sqrt(2)
					double gv = (p[0][1] - p[1][0]) * 0.7071067811865475;
					double g = sqrt(gu * gu + gv * gv);
					iter = g * 2.8284271247461903;
				}
				break;
			case Differences_Traditional:
				{
					// traditional method reverse engineered from original code
					double gx = (p[0][1] - p[1][1]) * 1.414;
					double gy = (p[1][0] - p[1][1]) * 1.414;
					double gu = (p[0][0] - p[1][1]);
					double gv = (p[0][2] - p[1][1]);
					double g = fabs(gx) + fabs(gy) + fabs(gu) + fabs(gv);
					iter = g;
				}
				break;
			}
			// post differencing transfer functions
//			iter/=4;
//			iter*=iter;
			iter*=(double)m_nX / (double)640;
			if (m_nColorMethod == ColorMethod_DistanceSqrt || m_nColorMethod == ColorMethod_DEPlusStandard)
				iter=sqrt(iter);
			else if (m_nColorMethod == ColorMethod_DistanceLog)
				iter=log(iter+1);
			/*iter=log(iter);
			if(iter<0)
				iter=0;*/
			if(iter>1024)
				iter=1024;
			if(m_nColorMethod == ColorMethod_DEPlusStandard && iter>m_nIterDiv)
				iter = (double)nIter + (double)1 - offs;
		}
		if (m_nIterDiv != 1){
			iter /= m_nIterDiv;
		}
		if (m_nColorOffset)
			iter += m_nColorOffset;// = (nIter+m_nColorOffset)%1024;
		nIter = (int)iter;
		offs = 1 - (iter - (double)nIter);
		if (m_bITrans)
			offs = 1 - offs;
		if (m_bMW){
			double nH = 0, nS = 0, nB = 0;
			int nDR = 0, nDG = 0, nDB = 0;
			int i;
			for (i = 0; i<m_nMW; i++){
				double nPeriod;
				nPeriod = m_MW[i].nPeriod;
				double g;
				if (m_bTrans)
					g = sin((pi*iter) / nPeriod) / 2 + .5;
				else
					g = sin((pi*((int)iter)) / nPeriod) / 2 + .5;
				if (nPeriod<0)
					g = -(double)nPeriod / (double)100;
				if (m_MW[i].nType == 0){
					nH += g;
					nDR++;
				}
				if (m_MW[i].nType == 1){
					nS += g;
					nDG++;
				}
				if (m_MW[i].nType == 2){
					nB += g;
					nDB++;
				}
			}
			if (nDR)
				nH /= nDR;
			if (nDG)
				nS /= nDG;
			if (nDB)
				nB /= nDB;
			hsv nHSV;
			nHSV.h = nH;
			nHSV.s = nS;
			nHSV.v = nB;
			srgb nRGB = hsv2rgb(nHSV);
			if (m_bBlend){
				double nR, nG, nB;
				if (m_bTrans && offs){
					double g1 = (1 - offs);
					int col = nIter % 1024;
					int ncol = (col + 1) % 1024;
					nR = m_cPos[col].r*offs + m_cPos[ncol].r*g1;
					nG = m_cPos[col].g*offs + m_cPos[ncol].g*g1;
					nB = m_cPos[col].b*offs + m_cPos[ncol].b*g1;
				}
				else{
					int col = nIter % 1024;
					nR = m_cPos[col].r;//+n;
					nG = m_cPos[col].g;//+n;
					nB = m_cPos[col].b;//+n;
				}
				srgb nRGB2;
				nRGB2.r = nR / 255.0f;
				nRGB2.g = nG / 255.0f;
				nRGB2.b = nB / 255.0f;
				s.r = (nRGB.r + nRGB2.r) * 0.5f;
				s.g = (nRGB.g + nRGB2.g) * 0.5f;
				s.b = (nRGB.b + nRGB2.b) * 0.5f;
			}
		}
		else{
			if (m_bTrans && offs){
				double g1 = (1 - offs);
				int col = nIter % 1024;
				int ncol = (col + 1) % 1024;
				s.r = (m_cPos[col].r*offs + m_cPos[ncol].r*g1) / 255.0f;
				s.g = (m_cPos[col].g*offs + m_cPos[ncol].g*g1) / 255.0f;
				s.b = (m_cPos[col].b*offs + m_cPos[ncol].b*g1) / 255.0f;
			}
			else{
				int col = nIter % 1024;
				s.r = m_cPos[col].r / 255.0f;
				s.g = m_cPos[col].g / 255.0f;
				s.b = m_cPos[col].b / 255.0f;
			}
		}
	}
	if(m_bTexture)
		SetTexture(nIndex,x,y);
	if (m_bSlopes){
		double p1, p2;
		/*		if(x && y){
		p1 = (double)m_nPixels[x-1][y-1] + (double)1-m_nTrans[x-1][y-1];
		p2 = (double)m_nPixels[x][y] + (double)1-m_nTrans[x][y];
		}
		else if(x<m_nX-1 && y<m_nY-1){
		p1 = (double)m_nPixels[x][y] + (double)1-m_nTrans[x][y];
		p2 = (double)m_nPixels[x+1][y+1] + (double)1-m_nTrans[x+1][y+1];
		}
		else
		p1=p2 = (double)m_nPixels[x][y] + (double)1-m_nTrans[x][y];
		double diffCompare = p1/p2;
		*/
		double diffx, diffy;
		if (x){
			p1 = (double)m_nPixels[x - 1][y] + (double)1 - m_nTrans[x - 1][y];
			p2 = (double)m_nPixels[x][y] + (double)1 - m_nTrans[x][y];
		}
		else if (x<m_nX - 1){
			p1 = (double)m_nPixels[x][y] + (double)1 - m_nTrans[x][y];
			p2 = (double)m_nPixels[x + 1][y] + (double)1 - m_nTrans[x + 1][y];
		}
		else
			p1 = p2 = (double)m_nPixels[x][y] + (double)1 - m_nTrans[x][y];
		diffx = p1 - p2;
		if (y){
			p1 = (double)m_nPixels[x][y - 1] + (double)1 - m_nTrans[x][y - 1];
			p2 = (double)m_nPixels[x][y] + (double)1 - m_nTrans[x][y];
		}
		else if (y<m_nY - 1){
			p1 = (double)m_nPixels[x][y] + (double)1 - m_nTrans[x][y];
			p2 = (double)m_nPixels[x][y + 1] + (double)1 - m_nTrans[x][y + 1];
		}
		else
			p1 = p2 = (double)m_nPixels[x][y] + (double)1 - m_nTrans[x][y];
		diffy = p1 - p2;
		double diff = diffx*m_nSlopeX + diffy*m_nSlopeY;
		p1 = (double)m_nPixels[x][y] + (double)1 - m_nTrans[x][y];
		diff = (p1 + diff) / p1;
		diff = pow(diff, (double)m_nSlopePower*(double)(m_nZoom*1.75 + 1)*(double)m_nX / (double)640);
		if (diff>1){
			diff = (atan(diff) - pi / 4) / (pi / 4);
			diff = diff*(double)m_nSlopeRatio / 100;;
			s.r = (1 - diff)*s.r;
			s.g = (1 - diff)*s.g;
			s.b = (1 - diff)*s.b;
		}
		else{
			diff = 1 / diff;
			diff = (atan(diff) - pi / 4) / (pi / 4);
			diff = diff*(double)m_nSlopeRatio / 100;;
			s.r = (1 - diff)*s.r + diff;
			s.g = (1 - diff)*s.g + diff;
			s.b = (1 - diff)*s.b + diff;
		}
	}
	srgb8 s8 = dither(s, x, y);
	m_lpBits[nIndex    ] = s8.r;
	m_lpBits[nIndex + 1] = s8.g;
	m_lpBits[nIndex + 2] = s8.b;
}

void CFraktalSFT::ApplyColors()
{
	int i, p = 0;
	for (i = 0; i<1024; i++){
		double temp = (double)i*(double)m_nParts / (double)1024;
		p = (int)temp;
		int pn = (p + 1) % m_nParts;
		temp -= p;
		temp = sin((temp - .5)*pi) / 2 + .5;
		m_cPos[i].r = (unsigned char)(temp*m_cKeys[pn].r + (1 - temp)*m_cKeys[p].r);
		m_cPos[i].g = (unsigned char)(temp*m_cKeys[pn].g + (1 - temp)*m_cKeys[p].g);
		m_cPos[i].b = (unsigned char)(temp*m_cKeys[pn].b + (1 - temp)*m_cKeys[p].b);
	}
	if (m_nPixels && m_lpBits){
		int x, y;
		for (x = 0; x<m_nX; x++){
			for (y = 0; y<m_nY; y++){
				int nIndex = x * 3 + (m_bmi->biHeight - 1 - y)*m_row;
				SetColor(nIndex, m_nPixels[x][y], m_nTrans[x][y], x, y);
			}
		}
	}
}
int CFraktalSFT::GetSeed()
{
	return m_nSeed;
}
CFraktalSFT::~CFraktalSFT()
{
	delete[] m_APr;
	delete[] m_APi;
}
char *CFraktalSFT::ToZoom()
{
	CFixedFloat div = m_istop - m_istart;
	return ToZoom((CDecNumber)4 / ((CDecNumber)div.m_f), m_nZoom);
}
char *CFraktalSFT::ToZoom(const CDecNumber &z, int &zoom)
{
	static char szRet[40];
	std::string sszZoom = z.ToText();
	const char *szZoom = sszZoom.c_str();
	*szRet = 0;
	for (m_nZoom = 0; szZoom[m_nZoom] && szZoom[m_nZoom] != '.'; m_nZoom++);
	m_nZoom--;
	if (m_nZoom <= 0){
		strncpy(szRet, szZoom, 3);
		szRet[3] = 0;
		m_nZooms = 1+log10(atof(szRet)) / log10(2.0);
		return szRet;
	}
	szRet[0] = szZoom[0];
	if (szZoom[1]){
		szRet[1] = '.';
		szRet[2] = szZoom[1];
		if (szZoom[2] && szZoom[2] != '.'){
			szRet[3] = szZoom[2];
			wsprintf(szRet + 4, "e%03d", zoom);
		}
		else
			wsprintf(szRet + 3, "e%03d", zoom);
	}
	else
		szRet[1] = 0;
	zoom = m_nZoom;
	char szTmp[40];
	strcpy(szTmp, szRet);
	if (strlen(szTmp)>4)
		szTmp[4] = 0;
	m_nZooms = 1 + (log10(atof(szTmp)) + m_nZoom) / (log10(2.0));
	return szRet;
}
double CHECK_FLOAT(double a)
{
	if (a <= DBL_MAX && a >= -DBL_MAX)
		return a;
	return 0;
}
BOOL ISFLOATOK(double a)
{
	if (a <= DBL_MAX && a >= -DBL_MAX)
		return TRUE;
	return 0;
}
void CFraktalSFT::CalculateApproximation(int nType)
{
	m_nApprox = 0;
#if 0
	// disabled for now because of severe underskipping problems
	double nProbeRatio = 1.0 / 65536.0;
	double r = sqrt(nProbeRatio);
	int nProbeX = ceil(fabs(m_rApprox.right - m_rApprox.left) * r);
	int nProbeY = ceil(fabs(m_rApprox.bottom - m_rApprox.top) * r);
	if (nProbeX < 3) nProbeX = 3;
	if (nProbeY < 3) nProbeY = 3;
#else
	// one more probe point in the center of the image, compared to upstream
	int nProbeX = 3;
	int nProbeY = 3;
#endif
	int nProbe = nProbeX * nProbeY;
	floatexp _1 = 1;
	floatexp _3 = 3;
	floatexp _6 = 6;
	CFixedFloat cr, ci;
	int i, j, k;
	floatexp *dbTr = new floatexp[nProbe];
	floatexp *dbTi = new floatexp[nProbe];
	floatexp *dbTr0 = new floatexp[nProbe];
	floatexp *dbTi0 = new floatexp[nProbe];
	cr = m_rstart;
	ci = m_istart;

	POINT *p = new POINT[nProbe];
	for (j = 0; j < nProbeY; ++j)
	{
		int y = m_rApprox.top + j * (m_rApprox.bottom - m_rApprox.top) / (nProbeY - 1);
		if (y < m_rApprox.top) y = m_rApprox.top;
		if (y >= m_rApprox.bottom) y = m_rApprox.bottom - 1;
		for (i = 0; i < nProbeX; ++i)
		{
			int x = m_rApprox.left + i * (m_rApprox.right - m_rApprox.left) / (nProbeX - 1);
			if (x < m_rApprox.left) x = m_rApprox.left;
			if (x >= m_rApprox.right) x = m_rApprox.right - 1;
			p[k].x = x;
			p[k].y = y;
			++k;
		}
	}
	assert(k == nProbe);

	if (nType == 0){
		for (j = 0; j<nProbe; j++){
			dbTr0[j] = m_pDX[p[j].x];
			dbTi0[j] = m_pDY[p[j].y];
			if (m_nScalingOffset){
				dbTr0[j] *= m_nScaling;
				dbTi0[j] *= m_nScaling;
			}
			dbTr[j] = dbTr0[j];
			dbTi[j] = dbTi0[j];
		}
	}
	else if (nType == 1){
		for (j = 0; j<nProbe; j++){
			ToFloatExp(&m_lDX[p[j].x], &dbTr0[j]);
			dbTr[j] = dbTr0[j];
			ToFloatExp(&m_lDX[p[j].y], &dbTi0[j]);
			dbTi[j] = dbTi0[j];
		}
	}
	else{
		for (j = 0; j<nProbe; j++){
			dbTr[j] = dbTr0[j] = m_DX[p[j].x];
			dbTi[j] = dbTi0[j] = m_DY[p[j].y];
		}
	}
	m_nMaxApproximation = m_nMaxIter;
	//floatexp mindiff = (m_nBailout==2?0.000001:0.001);
	floatexp mindiff;
	if (m_bLowTolerance)
		mindiff = 0.00001;
	else
		mindiff = 0.001;
	//	if(dbTr[0]<0 && dbTi[0]<1e-16 && dbTi[0]>-1e-16)
	//		mindiff = 0.0000001;
	if (m_bNoApproximation)
		mindiff = 0;
	m_APr[0] = 1;
	m_APi[0] = 0;
	if (m_bAutoTerms){
		int nT = sqrt((double)m_nTotal)*0.021;
		if (nT<5)
			nT = 5;
		if (nT>60)
			nT = 60;
		SetTerms(nT);
	}
	for (i = 1; i<m_nTerms; i++){
		m_APr[i] = 0;
		m_APi[i] = 0;
	}

	if (m_nFractalType>0 || m_nZoom<g_nRefZero){
		m_nMaxApproximation = 0;
		return;
	}
	floatexp xr;
	floatexp xi;
	floatexp *APr = new floatexp[m_nTerms];
	floatexp *APi = new floatexp[m_nTerms];
	for (i = 0; i<m_nTerms; i++){
		APr[i] = m_APr[i];
		APi[i] = m_APi[i];
	}

	for (i = 0; i<m_nMaxIter - 1 && !m_bStop; i++){
		m_nApprox++;
		// Series approximation
		int n = i - 1;
		if (i == 0)
			xr = xi = 0;
		else if (nType == 0){
			xr = m_db_dxr[n];
			xi = m_db_dxi[n];
		}
		else if (nType == 1){
			ToFloatExp(&m_ldxr[n], &xr);
			ToFloatExp(&m_ldxi[n], &xi);
		}
		else{
			xr = m_dxr[n];
			xi = m_dxi[n];
		}
		int k;
		for (k = 0; k<m_nTerms; k++){
			APr[k] = m_APr[k];
			APi[k] = m_APi[k];
		}
		if (m_nPower == 2){
			int k;
			m_APr[0] = (xr*APr[0] - xi*APi[0]).mul2() + _1;
			m_APi[0] = (xi*APr[0] + xr*APi[0]).mul2();
			for (k = 1; k<m_nTerms; k++){
				m_APr[k] = (xr*APr[k] - xi*APi[k]).mul2();
				m_APi[k] = (xi*APr[k] + xr*APi[k]).mul2();
				int n = k / 2;
				int q = n;
				int f = 0;
				int r = k - 1;
				while (q){
					m_APr[k] += (APr[f] * APr[r] - APi[f] * APi[r]).mul2();
					m_APi[k] += (APi[f] * APr[r] + APr[f] * APi[r]).mul2();
					q--;
					f++;
					r--;
				}
				if (k % 2){
					m_APr[k] += APr[n] * APr[n] - APi[n] * APi[n];
					m_APi[k] += (APr[n] * APi[n]).mul2();
				}
			}
		}
		else{
			complex<floatexp> X(xr, xi), A(APr[0], APi[0]), B(APr[1], APi[1]), C(APr[2], APi[2]), fa(m_nPower, 0), fa_2(m_nPower / 2, 0), fa_6(m_nPower / 6, 0), fa1(m_nPower - 1, 0), fa2(m_nPower - 2, 0), f1(1, 0), f2(2, 0), f3(3, 0), f6(6, 0);
			//(4-6971)
			// p*X^(p-1)*A+1
			complex<floatexp> An = fa*(X ^ (m_nPower - 1))*A + f1;
			m_APr[0] = An.m_r;
			m_APi[0] = An.m_i;

			//(4-999)
			//                B=(p/2)* X^(p-2)         *((p-1)*A^2  + X*2*B)
			complex<floatexp> Bn = fa_2*(X ^ (m_nPower - 2)) * (fa1*(A ^ 2) + X*f2*B);
			m_APr[1] = Bn.m_r;
			m_APi[1] = Bn.m_i;

			//(4-999)
			//                C=(p/6) *  X^(p-3) *       ((p-1)*A* ((p-2)*  A^2 +   6*X*B) + X^2 *   6 *  C)
			complex<floatexp> Cn = fa_6 * (X ^ (m_nPower - 3))* (fa1* A * (fa2 * (A ^ 2) + f6*X*B) + (X ^ 2) * f6 * C);
			m_APr[2] = Cn.m_r;
			m_APi[2] = Cn.m_i;
		}
		/*
		An+1 = 2XnAn + 1
		Bn+1 = 2XnBn + An2
		Cn+1 = 2XnCn + 2AnBn (33097)
		Dn+1 = 2XnDn + 2AnCn + Bn2
		En+1 = 2XnEn + 2AnDn + 2BnCn (38613)
		Fn+1 = 2XnFn + 2AnEn + 2BnDn + Cn2 (38613)
		Gn+1 = 2XnGn + 2AnFn + 2BnEn + 2CnDn
		Hn+1 = 2XnHn + 2AnGn + 2BnFn + 2CnEn + Dn2
		In+1 = 2XnIn + 2AnHn + 2BnGn + 2CnFn + 2DnEn
		*/
		if (i<m_nMaxApproximation){
			floatexp dxr, dxi;
			if (nType == 0){
				dxr = m_db_dxr[i];
				dxi = m_db_dxi[i];
			}
			else if (nType == 1){
				ToFloatExp(&m_ldxr[i], &dxr);
				ToFloatExp(&m_ldxi[i], &dxi);
			}
			else{
				dxr = m_dxr[i];
				dxi = m_dxi[i];
			}
			for (j = 0; j<nProbe; j++){
				floatexp Dnr = m_APr[0] * dbTr0[j] - m_APi[0] * dbTi0[j];
				floatexp Dni = m_APr[0] * dbTi0[j] + m_APi[0] * dbTr0[j];
				floatexp D_r = dbTr0[j] * dbTr0[j] - dbTi0[j] * dbTi0[j];
				floatexp D_i = (dbTr0[j] * dbTi0[j]).mul2();
				Dnr += m_APr[1] * D_r - m_APi[1] * D_i;
				Dni += m_APr[1] * D_i + m_APi[1] * D_r;
				int k;
				for (k = 2; k<m_nTerms; k++){
					floatexp  t = D_r*dbTr0[j] - D_i*dbTi0[j];
					D_i = D_r*dbTi0[j] + D_i*dbTr0[j];
					D_r = t;
					Dnr += m_APr[k] * D_r - m_APi[k] * D_i;
					Dni += m_APr[k] * D_i + m_APi[k] * D_r;
				}
				floatexp diff = (Dnr - dbTr[j]) / dbTr[j];
				if (diff>mindiff || diff<-mindiff){
					m_nMaxApproximation = i;
					break;
				}
				diff = (Dni - dbTi[j]) / dbTi[j];
				if (diff>mindiff || diff<-mindiff){
					m_nMaxApproximation = i;
					break;
				}
				double yr = (dxr + Dnr).todouble();
				double yi = (dxi + Dni).todouble();
				if (g_real*yr*yr + g_imag*yi*yi>m_nBailout2){
					m_nMaxApproximation = i;
					break;
				}

				if (m_nPower == 2){
					Dnr = (dxr*dbTr[j] - dxi*dbTi[j]).mul2() + dbTr[j] * dbTr[j] - dbTi[j] * dbTi[j] + dbTr0[j];
					Dni = (dxr*dbTi[j] + dxi*dbTr[j] + dbTr[j] * dbTi[j]).mul2() + dbTi0[j];
				}
				else if (m_nPower == 3){
					Dnr = _3*((dxr*dxr - dxi*dxi)*dbTr[j] + dxr*(dbTr[j] * dbTr[j] - dbTi[j] * dbTi[j]) - dbTi[j] * (dxi*(dxr + dbTr[j]).mul2() + dbTr[j] * dbTi[j])) + dbTr[j] * dbTr[j] * dbTr[j] + dbTr0[j];
					Dni = _3*((dxr*dxr - dxi*dxi)*dbTi[j] + dxi*(dbTr[j] * dbTr[j] - dbTi[j] * dbTi[j]) + dbTr[j] * (dxr*(dxi + dbTi[j]).mul2() + dbTr[j] * dbTi[j])) - dbTi[j] * dbTi[j] * dbTi[j] + dbTi0[j];
				}
				else if (m_nPower == 4){
					complex<floatexp> X(dxr, dxi);
					complex<floatexp> D(dbTr[j], dbTi[j]);
					complex<floatexp> D0(dbTr0[j], dbTi0[j]);
					complex<floatexp> _4(4, 0), _6(6, 0);
					complex<floatexp> Dn = _4*(X ^ 3)*D + _6*(X ^ 2)*(D ^ 2) + _4*X*(D ^ 3) + (D ^ 4) + D0;
					Dni = Dn.m_i;
					Dnr = Dn.m_r;
				}
				else if (m_nPower == 5){
					complex<floatexp> X(dxr, dxi);
					complex<floatexp> D(dbTr[j], dbTi[j]);
					complex<floatexp> D0(dbTr0[j], dbTi0[j]);
					complex<floatexp> _5(5, 0), _10(10, 0);
					complex<floatexp> Dn = _5*(X ^ 4)*D + _10*(X ^ 3)*(D ^ 2) + _10*(X ^ 2)*(D ^ 3) + _5*X*(D ^ 4) + (D ^ 5) + D0;
					Dni = Dn.m_i;
					Dnr = Dn.m_r;
				}
				else if (m_nPower == 6){
					complex<floatexp> X(dxr, dxi);
					complex<floatexp> D(dbTr[j], dbTi[j]);
					complex<floatexp> D0(dbTr0[j], dbTi0[j]);
					complex<floatexp> _6(6, 0), _15(15, 0), _20(20, 0);
					complex<floatexp> Dn = _6*(X ^ 5)*D + _15*(X ^ 4)*(D ^ 2) + _20*(X ^ 3)*(D ^ 3) + _15*(X ^ 2)*(D ^ 4) + _6*X*(D ^ 5) + (D ^ 6) + D0;
					Dni = Dn.m_i;
					Dnr = Dn.m_r;
				}
				else if (m_nPower == 7){
					complex<floatexp> X(dxr, dxi);
					complex<floatexp> D(dbTr[j], dbTi[j]);
					complex<floatexp> D0(dbTr0[j], dbTi0[j]);
					complex<floatexp> _7(7, 0), _21(21, 0), _35(35, 0);
					complex<floatexp> Dn = _7*(X ^ 6)*D + _21*(X ^ 5)*(D ^ 2) + _35*(X ^ 4)*(D ^ 3) + _35*(X ^ 3)*(D ^ 4) + _21*(X ^ 2)*(D ^ 5) + _7*X*(D ^ 6) + (D ^ 7) + D0;
					Dni = Dn.m_i;
					Dnr = Dn.m_r;
				}
				else if (m_nPower == 8){
					complex<floatexp> X(dxr, dxi);
					complex<floatexp> D(dbTr[j], dbTi[j]);
					complex<floatexp> D0(dbTr0[j], dbTi0[j]);
					complex<floatexp> _8(8, 0), _28(28, 0), _56(56, 0), _70(70, 0);
					complex<floatexp> Dn = _8*(X ^ 7)*D + _28*(X ^ 6)*(D ^ 2) + _56*(X ^ 5)*(D ^ 3) + _70*(X ^ 4)*(D ^ 4) + _56*(X ^ 3)*(D ^ 5) + _28*(X ^ 2)*(D ^ 6) + _8*X*(D ^ 7) + (D ^ 8) + D0;
					Dni = Dn.m_i;
					Dnr = Dn.m_r;
				}
				else if (m_nPower == 9){
					complex<floatexp> X(dxr, dxi);
					complex<floatexp> D(dbTr[j], dbTi[j]);
					complex<floatexp> D0(dbTr0[j], dbTi0[j]);
					complex<floatexp> _9(9, 0), _36(36, 0), _84(84, 0), _126(126, 0);
					complex<floatexp> Dn = _9*(X ^ 8)*D + _36*(X ^ 7)*(D ^ 2) + _84*(X ^ 6)*(D ^ 3) + _126*(X ^ 5)*(D ^ 4) + _126*(X ^ 4)*(D ^ 5) + _84*(X ^ 3)*(D ^ 6) + _36*(X ^ 2)*(D ^ 7) + _9*X*(D ^ 8) + (D ^ 9) + D0;
					Dni = Dn.m_i;
					Dnr = Dn.m_r;
				}
				else if (m_nPower == 10){
					complex<floatexp> X(dxr, dxi);
					complex<floatexp> D(dbTr[j], dbTi[j]);
					complex<floatexp> D0(dbTr0[j], dbTi0[j]);
					complex<floatexp> _10(10, 0), _45(45, 0), _120(120, 0), _210(210, 0), _252(252, 0);
					complex<floatexp> Dn = _10*(X ^ 9)*D + _45*(X ^ 8)*(D ^ 2) + _120*(X ^ 7)*(D ^ 3) + _210*(X ^ 6)*(D ^ 4) + _252*(X ^ 5)*(D ^ 5) + _210*(X ^ 4)*(D ^ 6) + _120*(X ^ 3)*(D ^ 7) + _45*(X ^ 2)*(D ^ 8) + _10*X*(D ^ 9) + (D ^ 10) + D0;
					Dni = Dn.m_i;
					Dnr = Dn.m_r;
				}
				else{
					complex<floatexp> X(dxr, dxi);
					complex<floatexp> D(dbTr[j], dbTi[j]);
					complex<floatexp> D0(dbTr0[j], dbTi0[j]);
					complex<floatexp> c(m_pnExpConsts[0], 0);
					int nXExp = m_nPower - 2, nDExp = 2, ci = 1;
					complex<floatexp> Dn = c*(X^m_nPower - 1)*D;
					while (nXExp){
						c.m_r = m_pnExpConsts[ci++];
						Dn += c*(X^nXExp)*(D^nDExp);
						nXExp--;
						nDExp++;
					}
					Dn += (D^m_nPower) + D0;
					Dni = Dn.m_i;
					Dnr = Dn.m_r;
				}

				dbTr[j] = Dnr;
				dbTi[j] = Dni;
			}
			if (j<nProbe)
				break;
		}
	}
	for (i = 0; i<m_nTerms; i++){
		m_APr[i] = APr[i];
		m_APi[i] = APi[i];
	}
	if (m_bNoApproximation)
		m_nMaxApproximation = 0;
	delete[] APr;
	delete[] APi;
	delete[] dbTr;
	delete[] dbTi;
	delete[] dbTr0;
	delete[] dbTi0;
	delete[] p;
}

DWORD WINAPI ThMC(MC *pMC)
{
	HANDLE hW[2];
	hW[0] = pMC->hWait;
	hW[1] = pMC->hExit;
	while (WaitForMultipleObjects(2, hW, FALSE, INFINITE) == WAIT_OBJECT_0){
		if (pMC->nType == 0)
			*pMC->sr = pMC->xr->Square();
		else if (pMC->nType == 1)
			*pMC->si = pMC->xi->Square();
		else
			*pMC->xrxid = (*pMC->xr + *pMC->xi).Square();
		SetEvent(pMC->hDone);
	}
	SetEvent(pMC->hDone);
	return 0;
}
DWORD WINAPI ThMC2(MC2 *pMC)
{
	HANDLE hW[2];
	hW[0] = pMC->hWait;
	hW[1] = pMC->hExit;
	while (WaitForMultipleObjects(2, hW, FALSE, INFINITE) == WAIT_OBJECT_0){
		if (pMC->nType == 0)
			*pMC->xin = *pMC->xrxid - *pMC->sr - *pMC->si + *pMC->m_iref;
		else
			*pMC->xrn = *pMC->sr - *pMC->si + *pMC->m_rref;
		SetEvent(pMC->hDone);
	}
	SetEvent(pMC->hDone);
	return 0;
}

void CFraktalSFT::CalculateReference()
{
	int i;
	if (m_db_dxr)
		delete[] m_db_dxr;
	m_db_dxr = new double [m_nMaxIter];
	if (m_db_dxi)
		delete[] m_db_dxi;
	m_db_dxi = new double [m_nMaxIter];
	if (m_db_z)
		delete[] m_db_z;
	m_db_z = new double [m_nMaxIter];

	CFixedFloat xr = g_SeedR, xi = g_SeedI, xin, xrn, sr = xr.Square(), si = xi.Square(), xrxid = 0;
	double abs_val;
	double terminate = SMOOTH_BAILOUT*SMOOTH_BAILOUT;

	if (m_nInflections)
	{
		int inf;
		complex<CFixedFloat> c(m_rref,m_iref);
		for(inf=m_nInflections-1;inf>=0;inf--){
			complex<CFixedFloat> d = c-m_pInflections[inf];
			c=m_pInflections[inf]+d*d;
		}
		m_rref=c.m_r;
		m_iref=c.m_i;
	}

	m_nGlitchIter = m_nMaxIter + 1;
	int nMaxIter = m_nMaxIter;

  if (m_nFractalType == 0 && m_nPower > 10)
	{

		double threashold = 0.0001;
		for (i = 7; i <= m_nPower; i += 2)
			threashold *= 10;
		if (m_bGlitchLowTolerance) {
			threashold = sqrt(threashold);
		}
		if (threashold>.5)
			threashold = .5;
		complex<CFixedFloat> r(m_rref, m_iref);
		complex<CFixedFloat> X(g_SeedR, g_SeedI);
		for (i = 0; i<nMaxIter && !m_bStop; i++){
			X = (X^m_nPower) + r;
			m_db_dxr[i] = X.m_r.ToDouble();
			m_db_dxi[i] = X.m_i.ToDouble();
			double abs_val = (g_real * m_db_dxr[i] * m_db_dxr[i] + g_imag * m_db_dxi[i] * m_db_dxi[i]);
			m_db_z[i] = abs_val*threashold;
			if (abs_val >= terminate){
				if (nMaxIter == m_nMaxIter){
					nMaxIter = i + 3;
					if (nMaxIter>m_nMaxIter)
						nMaxIter = m_nMaxIter;
					m_nGlitchIter = nMaxIter;
				}
			}
			m_nRDone++;
		}

	}
	else
	{

		bool ok = reference_double(m_nFractalType, m_nPower, m_db_dxr, m_db_dxi, m_db_z, m_bStop, m_nRDone, m_nGlitchIter, m_nMaxIter, m_rref, m_iref, g_SeedR, g_SeedI, g_FactorAR, g_FactorAI, terminate, g_real, g_imag, m_bGlitchLowTolerance);
		assert(ok && "reference_double");

	}
}

int ThMandelCalc(TH_PARAMS *pMan)
{
#ifndef _DEBUG
	try{
#endif
		pMan->p->MandelCalc(pMan->nXStart, pMan->nXStop);
#ifndef _DEBUG
	}
	catch (...) {
//		pMan->p->m_bRunning=FALSE;
MessageBox(GetActiveWindow(),"Krash - 1","Krash",MB_OK);
	}
#endif
	return 0;
}
int ThMandelCalcEXP(TH_PARAMS *pMan)
{
	pMan->p->MandelCalcEXP(pMan->nXStart, pMan->nXStop);
	return 0;
}
int ThMandelCalcLDBL(TH_PARAMS *pMan)
{
	pMan->p->MandelCalcLDBL(pMan->nXStart, pMan->nXStop);
	return 0;
}
void CFraktalSFT::Mirror(int x, int y)
{
	if (!m_bMirrored)
		return;
	int ty = m_nY - y - 1;
	if (ty<y)
		return;
	int tx = m_nX - x - 1;

	m_nPixels[tx][ty] = m_nPixels[x][y];
	m_nTrans[tx][ty] = m_nTrans[x][y];
	int nIndex1 = tx * 3 + (m_bmi->biHeight - 1 - ty)*m_row;
	SetColor(nIndex1, m_nPixels[x][ty], m_nTrans[x][ty], x, ty);
}
int CFraktalSFT::GetMirror()
{
	return m_bMirrored;
}
void CFraktalSFT::SetMirror(BOOL bMirror)
{
	m_bMirrored = bMirror;
}

#define GET_EXP(val) ((*((__int64*)&val) & 0x7FF0000000000000)>>52)

#define GUESS
//#define HARD_GUESS_EXP
//60 2.5
/*
#define _lb_abs(c_in, d_in, res)	\
c = c_in;						\
d = d_in;						\
if(c>0){						\
if(c+d>0)					\
res = d;				\
else if(d==-c)				\
res = d;				\
else if(d<-c)				\
res = -d-2*c;			\
}								\
else if (c==0)					\
res = _abs(d);				\
else if (c < 0){				\
if (c+d>0)					\
res = d + 2*c;			\
else if (d == -c)			\
res = -d;				\
else if (d < -c)			\
res = -d;				\
}

*/
double lb_abs_db(double c, double d)
{
	double abs_val;
	if (c>0){
		if (c + d>0)
			abs_val = d;
		else if (d == -c)
			abs_val = d;
		else if (d<-c)
			abs_val = -d - 2 * c;
	}
	else if (c == 0)
		abs_val = abs(d);
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
void CFraktalSFT::MandelCalc(int nXStart, int nXStop)
{
	m_bIterChanged = TRUE;
	double Dnr, Dni, yr, yi, _abs_val;
	int antal, x, y;
	int nPStep, nStepSize;

	while (!m_bStop && m_P.GetPixel(x, y, m_bMirrored)){
		nStepSize = nPStep = m_P.GetStep();
		if (nPStep>1)
			nPStep = 0;
		else
			nPStep = 1;
		int nIndex = x * 3 + (m_bmi->biHeight - 1 - y)*m_row;
		if (m_nPixels[x][y] != -1){
			SetColor(nIndex, m_nPixels[x][y], m_nTrans[x][y], x, y);
			continue;
		}
#ifdef GUESS
		if (nPStep && nStepSize==1){
			if (x && x<m_nX - 1 && m_nPixels[x - 1][y] != -1 && m_nPixels[x - 1][y] == m_nPixels[x + 1][y]){
				m_nTrans[x][y] = (m_nTrans[x - 1][y] + m_nTrans[x + 1][y]) / 2;
				int nIndex1 = (x - 1) * 3 + (m_bmi->biHeight - 1 - (y))*m_row;
				int nIndex2 = (x + 1) * 3 + (m_bmi->biHeight - 1 - (y))*m_row;
				m_lpBits[nIndex] = (m_lpBits[nIndex1] + m_lpBits[nIndex2]) / 2;
				m_lpBits[nIndex + 1] = (m_lpBits[nIndex1 + 1] + m_lpBits[nIndex2 + 1]) / 2;
				m_lpBits[nIndex + 2] = (m_lpBits[nIndex1 + 2] + m_lpBits[nIndex2 + 2]) / 2;
				InterlockedIncrement((LPLONG)&m_nDone);
				InterlockedIncrement((LPLONG)&m_nGuessed);
				m_nPixels[x][y] = m_nPixels[x - 1][y];
				if (m_bMirrored)
					Mirror(x, y);
				continue;
			}
			if (y && y<m_nY - 1 && m_nPixels[x][y - 1] != -1 && m_nPixels[x][y - 1] == m_nPixels[x][y + 1]){
				m_nTrans[x][y] = (m_nTrans[x][y - 1] + m_nTrans[x][y + 1]) / 2;
				int nIndex1 = (x)* 3 + (m_bmi->biHeight - 1 - (y - 1))*m_row;
				int nIndex2 = (x)* 3 + (m_bmi->biHeight - 1 - (y + 1))*m_row;
				m_lpBits[nIndex] = (m_lpBits[nIndex1] + m_lpBits[nIndex2]) / 2;
				m_lpBits[nIndex + 1] = (m_lpBits[nIndex1 + 1] + m_lpBits[nIndex2 + 1]) / 2;
				m_lpBits[nIndex + 2] = (m_lpBits[nIndex1 + 2] + m_lpBits[nIndex2 + 2]) / 2;
				InterlockedIncrement((LPLONG)&m_nDone);
				InterlockedIncrement((LPLONG)&m_nGuessed);
				m_nPixels[x][y] = m_nPixels[x][y - 1];
				if (m_bMirrored)
					Mirror(x, y);
				continue;
			}
			if (y && y<m_nY - 1 && x && x<m_nX - 1 && m_nPixels[x - 1][y - 1] != -1 && m_nPixels[x - 1][y - 1] == m_nPixels[x + 1][y + 1]){
				m_nTrans[x][y] = (m_nTrans[x - 1][y - 1] + m_nTrans[x + 1][y + 1]) / 2;
				int nIndex1 = (x - 1) * 3 + (m_bmi->biHeight - 1 - (y - 1))*m_row;
				int nIndex2 = (x + 1) * 3 + (m_bmi->biHeight - 1 - (y + 1))*m_row;
				m_lpBits[nIndex] = (m_lpBits[nIndex1] + m_lpBits[nIndex2]) / 2;
				m_lpBits[nIndex + 1] = (m_lpBits[nIndex1 + 1] + m_lpBits[nIndex2 + 1]) / 2;
				m_lpBits[nIndex + 2] = (m_lpBits[nIndex1 + 2] + m_lpBits[nIndex2 + 2]) / 2;
				InterlockedIncrement((LPLONG)&m_nDone);
				InterlockedIncrement((LPLONG)&m_nGuessed);
				m_nPixels[x][y] = m_nPixels[x - 1][y - 1];
				if (m_bMirrored)
					Mirror(x, y);
				continue;
			}
			if (y && y<m_nY - 1 && x && x<m_nX - 1 && m_nPixels[x - 1][y + 1] != -1 && m_nPixels[x - 1][y + 1] == m_nPixels[x + 1][y - 1]){
				m_nTrans[x][y] = (m_nTrans[x - 1][y + 1] + m_nTrans[x + 1][y - 1]) / 2;
				int nIndex1 = (x - 1) * 3 + (m_bmi->biHeight - 1 - (y + 1))*m_row;
				int nIndex2 = (x + 1) * 3 + (m_bmi->biHeight - 1 - (y - 1))*m_row;
				m_lpBits[nIndex] = (m_lpBits[nIndex1] + m_lpBits[nIndex2]) / 2;
				m_lpBits[nIndex + 1] = (m_lpBits[nIndex1 + 1] + m_lpBits[nIndex2 + 1]) / 2;
				m_lpBits[nIndex + 2] = (m_lpBits[nIndex1 + 2] + m_lpBits[nIndex2 + 2]) / 2;
				InterlockedIncrement((LPLONG)&m_nDone);
				InterlockedIncrement((LPLONG)&m_nGuessed);
				m_nPixels[x][y] = m_nPixels[x - 1][y + 1];
				if (m_bMirrored)
					Mirror(x, y);
				continue;
			}
		}
#endif
		if (m_nPixels[x][y] != -1){
			SetColor(nIndex, m_nPixels[x][y], m_nTrans[x][y], x, y);
			if (m_bMirrored)
				Mirror(x, y);
			continue;
		}
		// Series approximation
		double dbD0r = m_pDX[m_nX / 2] + m_C*(m_pDX[x] - m_pDX[m_nX / 2]) + m_S*(m_pDY[y] - m_pDY[m_nY / 2]);
		double dbD0i = m_pDY[m_nY / 2] - m_S*(m_pDX[x] - m_pDX[m_nX / 2]) + m_C*(m_pDY[y] - m_pDY[m_nY / 2]);
		if (m_nInflections)
		{
			int inf;
			complex<CFixedFloat> c(dbD0r,dbD0i);
			for(inf=m_nInflections-1;inf>=0;inf--){
				complex<CFixedFloat> d = c-m_pInflections[inf];
				c=m_pInflections[inf]+d*d;
			}
			dbD0r=c.m_r.ToDouble();
			dbD0i=c.m_i.ToDouble();
		}

		floatexp D0r = dbD0r;
		D0r *= m_nScaling;
		floatexp D0i = dbD0i;
		D0i *= m_nScaling;
		floatexp TDnr;
		floatexp TDni;
		if (m_nMaxApproximation){
			antal = m_nMaxApproximation - 1;
			TDnr = m_APr[0] * D0r - m_APi[0] * D0i;
			TDni = m_APr[0] * D0i + m_APi[0] * D0r;
			floatexp D_r = D0r*D0r - D0i*D0i;
			floatexp D_i = (D0r*D0i).mul2();
			TDnr += m_APr[1] * D_r - m_APi[1] * D_i;
			TDni += m_APr[1] * D_i + m_APi[1] * D_r;
			int k;
			for (k = 2; k<m_nTerms; k++){
				floatexp  t = D_r*D0r - D_i*D0i;
				D_i = D_r*D0i + D_i*D0r;
				D_r = t;
				TDnr += m_APr[k] * D_r - m_APi[k] * D_i;
				TDni += m_APr[k] * D_i + m_APi[k] * D_r;
			}
		}
		else{
			antal = 0;
			TDnr = D0r;
			TDni = D0i;
		}

		double test1 = 0, test2 = 0;
		BOOL bGlitch = FALSE;
		int nMaxIter = (m_nGlitchIter<m_nMaxIter ? m_nGlitchIter : m_nMaxIter);
		if (m_nScalingOffset){
			double Dr = TDnr.todouble(m_nScalingOffset);
			double Di = TDni.todouble(m_nScalingOffset);
			if (m_nPower == 2){
				if (antal<nMaxIter && test1 <= m_nBailout2){
					for (; antal<nMaxIter && test1 <= m_nBailout2; antal++){
						yr = m_db_dxr[antal] + Dr*m_nScaling;
						yi = m_db_dxi[antal] + Di*m_nScaling;
						test2 = test1;
						test1 = g_real*yr*yr + g_imag*yi*yi;
						if (test1<m_db_z[antal]){
							if (!m_bNoGlitchDetection)
								test1 = m_nBailout2 * 2;
							bGlitch = TRUE;
						}
						Dnr = (2 * m_db_dxr[antal] + Dr*m_nScaling)*Dr - (2 * m_db_dxi[antal] + Di*m_nScaling)*Di + dbD0r;
						Dni = 2 * ((m_db_dxr[antal] + Dr*m_nScaling)*Di + m_db_dxi[antal] * Dr) + dbD0i;
						Di = Dni;
						Dr = Dnr;
					}
				}
			}
			if (m_nPower == 3){
				if (antal<nMaxIter && test1 <= m_nBailout2){
					for (; antal<nMaxIter && test1 <= m_nBailout2; antal++){
						yr = m_db_dxr[antal] + Dr*m_nScaling;
						yi = m_db_dxi[antal] + Di*m_nScaling;
						test2 = test1;
						test1 = g_real*yr*yr + g_imag*yi*yi;
						if (test1<m_db_z[antal]){
							if (!m_bNoGlitchDetection)
								test1 = m_nBailout2 * 2;
							bGlitch = TRUE;
						}
						//Dnr=3*((m_db_dxr[antal]*m_db_dxr[antal]-m_db_dxi[antal]*m_db_dxi[antal])*Dr+m_db_dxr[antal]*(Dr*Dr*m_nScaling-Di*Di*m_nScaling)-Di*(2*m_db_dxi[antal]*(m_db_dxr[antal]+Dr*m_nScaling)+Dr*m_nScaling*Di*m_nScaling))+Dr*m_nScaling*Dr*m_nScaling*Dr+dbD0r;
						//Dni=3*((m_db_dxr[antal]*m_db_dxr[antal]-m_db_dxi[antal]*m_db_dxi[antal])*Di+m_db_dxi[antal]*(Dr*Dr*m_nScaling-Di*Di*m_nScaling)+Dr*(2*m_db_dxr[antal]*(m_db_dxi[antal]+Di)+Dr*m_nScaling*Di*m_nScaling))-Di*Di*m_nScaling*Di*m_nScaling+dbD0i;
						Dnr = 3 * m_db_dxr[antal] * m_db_dxr[antal] * Dr - 6 * m_db_dxr[antal] * m_db_dxi[antal] * Di - 3 * m_db_dxi[antal] * m_db_dxi[antal] * Dr + 3 * m_db_dxr[antal] * Dr*Dr*m_nScaling - 3 * m_db_dxr[antal] * Di*Di*m_nScaling - 3 * m_db_dxi[antal] * 2 * Dr*Di*m_nScaling + Dr*Dr*Dr*m_nScaling*m_nScaling - 3 * Dr*Di*Di*m_nScaling*m_nScaling + dbD0r;
						Dni = 3 * m_db_dxr[antal] * m_db_dxr[antal] * Di + 6 * m_db_dxr[antal] * m_db_dxi[antal] * Dr - 3 * m_db_dxi[antal] * m_db_dxi[antal] * Di + 3 * m_db_dxr[antal] * 2 * Dr*Di*m_nScaling + 3 * m_db_dxi[antal] * Dr*Dr*m_nScaling - 3 * m_db_dxi[antal] * Di*Di*m_nScaling + 3 * Dr*Dr*Di*m_nScaling*m_nScaling - Di*Di*Di*m_nScaling*m_nScaling + dbD0i;
						Di = Dni;
						Dr = Dnr;
					}
				}
			}
		}
		else
		{

			double Dr = TDnr.todouble();
			double Di = TDni.todouble();

			if (m_nFractalType == 0 && m_nPower > 10)
			{

				if (antal<nMaxIter && test1 <= m_nBailout2){
					for (; antal<nMaxIter && test1 <= m_nBailout2; antal++){
						yr = m_db_dxr[antal] + Dr;
						yi = m_db_dxi[antal] + Di;
						test2 = test1;
						test1 = g_real*yr*yr + g_imag*yi*yi;
						if (test1<m_db_z[antal]){
							if (!m_bNoGlitchDetection)
								test1 = m_nBailout2 * 2;
							bGlitch = TRUE;
						}
						complex<double> X(m_db_dxr[antal], m_db_dxi[antal]);
						complex<double> D(Dr, Di);
						complex<double> D0(dbD0r, m_pDY[y]);
						complex<double> c(m_pnExpConsts[0], 0);
						int nXExp = m_nPower - 2, nDExp = 2, ci = 1;
						complex<double> Dn = c*(X^m_nPower - 1)*D;
						while (nXExp){
							c.m_r = m_pnExpConsts[ci++];
							Dn += c*(X^nXExp)*(D^nDExp);
							nXExp--;
							nDExp++;
						}
						Dn += (D^m_nPower) + D0;
						Di = Dn.m_i;
						Dr = Dn.m_r;
					}
				}

			}
			else
			{

				bool ok = perturbation_double(m_nFractalType, m_nPower, m_db_dxr, m_db_dxi, m_db_z, antal, test1, test2, bGlitch, m_nBailout2, nMaxIter, m_bNoGlitchDetection, g_real, g_imag, g_FactorAR, g_FactorAI, Dr, Di, dbD0r, dbD0i);
				assert(ok && "perturbation_double");

			}
		}
		if (antal == m_nGlitchIter)
			bGlitch = TRUE;
		if (antal == m_nMaxIter){
			m_nPixels[x][y] = antal;
			m_nTrans[x][y] = 0;
			m_lpBits[nIndex] = 0;
			m_lpBits[nIndex + 1] = 0;
			m_lpBits[nIndex + 2] = 0;
		}
		else{
			m_nPixels[x][y] = antal;
			if (!bGlitch && m_nSmoothMethod == 1){
				double div = sqrt(test1) - sqrt(test2);
				if (div != 0)
					m_nTrans[x][y] = (sqrt(test1) - m_nBailout) / div;
				else
					m_nTrans[x][y] = 0;
			}

			else if (!bGlitch && m_nSmoothMethod == 0){
				m_nTrans[x][y] = log(log(sqrt(test1))) / log((double)m_nPower);
				if (!ISFLOATOK(m_nTrans[x][y]))
					m_nTrans[x][y] = 0;
				while (m_nTrans[x][y]<0){
					int offs = 1 + (int)m_nTrans[x][y];
					m_nPixels[x][y] += offs;
					m_nTrans[x][y] += offs;
				}
				while (m_nTrans[x][y]>1){
					int offs = (int)m_nTrans[x][y];
					m_nPixels[x][y] -= offs;
					m_nTrans[x][y] -= offs;
				}
			}

			if (bGlitch && !m_bNoGlitchDetection){
				m_nTrans[x][y] = 2;
				m_nPixels[x][y] = m_nMaxIter - 1;//(m_nMaxApproximation?m_nMaxApproximation-1:0);
			}
			SetColor(nIndex, m_nPixels[x][y], m_nTrans[x][y], x, y);
		}
		if (m_bMirrored)
			Mirror(x, y);
		InterlockedIncrement((LPLONG)&m_nDone);
		if (!nPStep && (!bGlitch || g_bShowGlitches)){
			int q;
			int nE = nStepSize*nStepSize;
			for (q = 0; q<nE; q++){
				int tx = x + q%nStepSize;
				int ty = y + q / nStepSize;
				if (tx<m_nX - 1 && ty<m_nY - 1 && m_nPixels[tx][ty] == -1){
					int nIndex1 = tx * 3 + (m_bmi->biHeight - 1 - ty)*m_row;
					m_lpBits[nIndex1] = m_lpBits[nIndex];
					m_lpBits[nIndex1 + 1] = m_lpBits[nIndex + 1];
					m_lpBits[nIndex1 + 2] = m_lpBits[nIndex + 2];
				}
			}
		}
	}
}


void CFraktalSFT::MandelCalcLDBL(int nXStart, int nXStop)
{
	m_bIterChanged = TRUE;
	int antal, x, y;
	int nPStep, nStepSize;

	SetParts(g_real,g_imag);

	while (!m_bStop && m_P.GetPixel(x, y)){
		nStepSize = nPStep = m_P.GetStep();
		if (nPStep>1)
			nPStep = 0;
		else
			nPStep = 1;
		int nIndex = x * 3 + (m_bmi->biHeight - 1 - y)*m_row;
		if (m_nPixels[x][y] != -1){
			SetColor(nIndex, m_nPixels[x][y], m_nTrans[x][y], x, y);
			continue;
		}
#ifdef GUESS
		if (nPStep && nStepSize==1){
			if (x && x<m_nX - 1 && m_nPixels[x - 1][y] != -1 && m_nPixels[x - 1][y] == m_nPixels[x + 1][y]){
				m_nTrans[x][y] = (m_nTrans[x - 1][y] + m_nTrans[x + 1][y])*.5;
				if (m_nTrans[x][y] == 2)
					m_nTrans[x - 1][y] = 2;
				int nIndex1 = (x - 1) * 3 + (m_bmi->biHeight - 1 - (y))*m_row;
				int nIndex2 = (x + 1) * 3 + (m_bmi->biHeight - 1 - (y))*m_row;
				m_lpBits[nIndex] = (m_lpBits[nIndex1] + m_lpBits[nIndex2]) / 2;
				m_lpBits[nIndex + 1] = (m_lpBits[nIndex1 + 1] + m_lpBits[nIndex2 + 1]) / 2;
				m_lpBits[nIndex + 2] = (m_lpBits[nIndex1 + 2] + m_lpBits[nIndex2 + 2]) / 2;
				InterlockedIncrement((LPLONG)&m_nDone);
				InterlockedIncrement((LPLONG)&m_nGuessed);
				m_nPixels[x][y] = m_nPixels[x - 1][y];
				if (m_bMirrored)
					Mirror(x, y);
				continue;
			}
			if (y && y<m_nY - 1 && m_nPixels[x][y - 1] != -1 && m_nPixels[x][y - 1] == m_nPixels[x][y + 1]){
				m_nTrans[x][y] = (m_nTrans[x][y - 1] + m_nTrans[x][y + 1])*.5;
				int nIndex1 = (x)* 3 + (m_bmi->biHeight - 1 - (y - 1))*m_row;
				int nIndex2 = (x)* 3 + (m_bmi->biHeight - 1 - (y + 1))*m_row;
				m_lpBits[nIndex] = (m_lpBits[nIndex1] + m_lpBits[nIndex2]) / 2;
				m_lpBits[nIndex + 1] = (m_lpBits[nIndex1 + 1] + m_lpBits[nIndex2 + 1]) / 2;
				m_lpBits[nIndex + 2] = (m_lpBits[nIndex1 + 2] + m_lpBits[nIndex2 + 2]) / 2;
				InterlockedIncrement((LPLONG)&m_nDone);
				InterlockedIncrement((LPLONG)&m_nGuessed);
				m_nPixels[x][y] = m_nPixels[x][y - 1];
				if (m_bMirrored)
					Mirror(x, y);
				continue;
			}
			if (y && y<m_nY - 1 && x && x<m_nX - 1 && m_nPixels[x - 1][y - 1] != -1 && m_nPixels[x - 1][y - 1] == m_nPixels[x + 1][y + 1]){
				m_nTrans[x][y] = (m_nTrans[x - 1][y - 1] + m_nTrans[x + 1][y + 1])*.5;
				int nIndex1 = (x - 1) * 3 + (m_bmi->biHeight - 1 - (y - 1))*m_row;
				int nIndex2 = (x + 1) * 3 + (m_bmi->biHeight - 1 - (y + 1))*m_row;
				m_lpBits[nIndex] = (m_lpBits[nIndex1] + m_lpBits[nIndex2]) / 2;
				m_lpBits[nIndex + 1] = (m_lpBits[nIndex1 + 1] + m_lpBits[nIndex2 + 1]) / 2;
				m_lpBits[nIndex + 2] = (m_lpBits[nIndex1 + 2] + m_lpBits[nIndex2 + 2]) / 2;
				InterlockedIncrement((LPLONG)&m_nDone);
				InterlockedIncrement((LPLONG)&m_nGuessed);
				m_nPixels[x][y] = m_nPixels[x - 1][y - 1];
				if (m_bMirrored)
					Mirror(x, y);
				continue;
			}
			if (y && y<m_nY - 1 && x && x<m_nX - 1 && m_nPixels[x - 1][y + 1] != -1 && m_nPixels[x - 1][y + 1] == m_nPixels[x + 1][y - 1]){
				m_nTrans[x][y] = (m_nTrans[x - 1][y + 1] + m_nTrans[x + 1][y - 1])*.5;
				int nIndex1 = (x - 1) * 3 + (m_bmi->biHeight - 1 - (y + 1))*m_row;
				int nIndex2 = (x + 1) * 3 + (m_bmi->biHeight - 1 - (y - 1))*m_row;
				m_lpBits[nIndex] = (m_lpBits[nIndex1] + m_lpBits[nIndex2]) / 2;
				m_lpBits[nIndex + 1] = (m_lpBits[nIndex1 + 1] + m_lpBits[nIndex2 + 1]) / 2;
				m_lpBits[nIndex + 2] = (m_lpBits[nIndex1 + 2] + m_lpBits[nIndex2 + 2]) / 2;
				InterlockedIncrement((LPLONG)&m_nDone);
				InterlockedIncrement((LPLONG)&m_nGuessed);
				m_nPixels[x][y] = m_nPixels[x - 1][y + 1];
				if (m_bMirrored)
					Mirror(x, y);
				continue;
			}
#ifdef HARD_GUESS_EXP
			if (x && x<m_nX - 2 && m_nPixels[x - 1][y] != -1 && m_nPixels[x - 1][y] == m_nPixels[x + 2][y]){
				m_nTrans[x][y] = (m_nTrans[x - 1][y] + m_nTrans[x + 2][y])*.5;
				int nIndex1 = (x - 1) * 3 + (m_bmi->biHeight - 1 - y)*m_row;
				int nIndex2 = (x + 2) * 3 + (m_bmi->biHeight - 1 - y)*m_row;
				m_lpBits[nIndex] = (m_lpBits[nIndex1] + m_lpBits[nIndex2]) / 2;
				m_lpBits[nIndex + 1] = (m_lpBits[nIndex1 + 1] + m_lpBits[nIndex2 + 1]) / 2;
				m_lpBits[nIndex + 2] = (m_lpBits[nIndex1 + 2] + m_lpBits[nIndex2 + 2]) / 2;
				InterlockedIncrement((LPLONG)&m_nDone);
				InterlockedIncrement((LPLONG)&m_nGuessed);
				m_nPixels[x][y] = m_nPixels[x - 1][y];
				if (m_bMirrored)
					Mirror(x, y);
				continue;
			}
			if (y && y<m_nY - 2 && m_nPixels[x][y - 1] != -1 && m_nPixels[x][y - 1] == m_nPixels[x][y + 2]){
				m_nTrans[x][y] = (m_nTrans[x][y - 1] + m_nTrans[x][y + 2])*.5;
				int nIndex1 = (x)* 3 + (m_bmi->biHeight - 1 - (y - 1))*m_row;
				int nIndex2 = (x)* 3 + (m_bmi->biHeight - 1 - (y + 2))*m_row;
				m_lpBits[nIndex] = (m_lpBits[nIndex1] + m_lpBits[nIndex2]) / 2;
				m_lpBits[nIndex + 1] = (m_lpBits[nIndex1 + 1] + m_lpBits[nIndex2 + 1]) / 2;
				m_lpBits[nIndex + 2] = (m_lpBits[nIndex1 + 2] + m_lpBits[nIndex2 + 2]) / 2;
				InterlockedIncrement((LPLONG)&m_nDone);
				InterlockedIncrement((LPLONG)&m_nGuessed);
				m_nPixels[x][y] = m_nPixels[x][y - 1];
				if (m_bMirrored)
					Mirror(x, y);
				continue;
			}
			if (y && y<m_nY - 2 && x && x<m_nX - 2 && m_nPixels[x - 1][y - 1] != -1 && m_nPixels[x - 1][y - 1] == m_nPixels[x + 2][y + 2]){
				m_nTrans[x][y] = (m_nTrans[x - 1][y - 1] + m_nTrans[x + 2][y + 2])*.5;
				int nIndex1 = (x - 1) * 3 + (m_bmi->biHeight - 1 - (y - 1))*m_row;
				int nIndex2 = (x + 2) * 3 + (m_bmi->biHeight - 1 - (y + 2))*m_row;
				m_lpBits[nIndex] = (m_lpBits[nIndex1] + m_lpBits[nIndex2]) / 2;
				m_lpBits[nIndex + 1] = (m_lpBits[nIndex1 + 1] + m_lpBits[nIndex2 + 1]) / 2;
				m_lpBits[nIndex + 2] = (m_lpBits[nIndex1 + 2] + m_lpBits[nIndex2 + 2]) / 2;
				InterlockedIncrement((LPLONG)&m_nDone);
				InterlockedIncrement((LPLONG)&m_nGuessed);
				m_nPixels[x][y] = m_nPixels[x - 1][y - 1];
				if (m_bMirrored)
					Mirror(x, y);
				continue;
			}
			if (y && y<m_nY - 2 && x && x<m_nX - 2 && m_nPixels[x - 1][y + 2] != -1 && m_nPixels[x - 1][y + 2] == m_nPixels[x + 2][y - 1]){
				m_nTrans[x][y] = (m_nTrans[x - 1][y + 2] + m_nTrans[x + 2][y - 1])*.5;
				int nIndex1 = (x - 1) * 3 + (m_bmi->biHeight - 1 - (y + 2))*m_row;
				int nIndex2 = (x + 2) * 3 + (m_bmi->biHeight - 1 - (y - 1))*m_row;
				m_lpBits[nIndex] = (m_lpBits[nIndex1] + m_lpBits[nIndex2]) / 2;
				m_lpBits[nIndex + 1] = (m_lpBits[nIndex1 + 1] + m_lpBits[nIndex2 + 1]) / 2;
				m_lpBits[nIndex + 2] = (m_lpBits[nIndex1 + 2] + m_lpBits[nIndex2 + 2]) / 2;
				InterlockedIncrement((LPLONG)&m_nDone);
				InterlockedIncrement((LPLONG)&m_nGuessed);
				m_nPixels[x][y] = m_nPixels[x - 1][y + 2];
				if (m_bMirrored)
					Mirror(x, y);
				continue;
			}
			if (y>1 && y<m_nY - 2 && x && x<m_nX - 2 && m_nPixels[x - 1][y + 2] != -1 && m_nPixels[x - 1][y + 2] == m_nPixels[x + 2][y - 2]){
				m_nTrans[x][y] = (m_nTrans[x - 1][y + 2] + m_nTrans[x + 2][y - 2])*.5;
				int nIndex1 = (x - 1) * 3 + (m_bmi->biHeight - 1 - (y + 2))*m_row;
				int nIndex2 = (x + 2) * 3 + (m_bmi->biHeight - 1 - (y - 2))*m_row;
				m_lpBits[nIndex] = (m_lpBits[nIndex1] + m_lpBits[nIndex2]) / 2;
				m_lpBits[nIndex + 1] = (m_lpBits[nIndex1 + 1] + m_lpBits[nIndex2 + 1]) / 2;
				m_lpBits[nIndex + 2] = (m_lpBits[nIndex1 + 2] + m_lpBits[nIndex2 + 2]) / 2;
				InterlockedIncrement((LPLONG)&m_nDone);
				InterlockedIncrement((LPLONG)&m_nGuessed);
				m_nPixels[x][y] = m_nPixels[x - 1][y + 2];
				if (m_bMirrored)
					Mirror(x, y);
				continue;
			}
#endif
		}
#endif
		if (m_nPixels[x][y] != -1){
			SetColor(nIndex, m_nPixels[x][y], m_nTrans[x][y], x, y);
			if (m_bMirrored)
				Mirror(x, y);
			continue;
		}
		// Series approximation - Start
		floatexp D0r;
		floatexp D0i;

		ldbl lD0r, mmr, lD0i, mmi, c, s, t1, t2;
		AssignDouble(&c, m_C);
		AssignDouble(&s, m_S);
		Subtract(&m_lDX[x], &m_lDX[m_nX / 2], &mmr);
		Subtract(&m_lDY[y], &m_lDY[m_nY / 2], &mmi);

		Multiply(&c, &mmr, &t1);
		Multiply(&s, &mmi, &t2);
		Add(&m_lDX[m_nX / 2], &t1, &lD0r);
		Add(&lD0r, &t2, &lD0r);

		Multiply(&s, &mmr, &t1);
		Multiply(&c, &mmi, &t2);
		Subtract(&m_lDY[m_nY / 2], &t1, &lD0i);
		Add(&lD0i, &t2, &lD0i);

		ToFloatExp(&lD0r, &D0r);
		ToFloatExp(&lD0i, &D0i);
		floatexp TDnr;
		floatexp TDni;
		if (m_nMaxApproximation){
			antal = m_nMaxApproximation - 1;
			TDnr = m_APr[0] * D0r - m_APi[0] * D0i;
			TDni = m_APr[0] * D0i + m_APi[0] * D0r;
			floatexp D_r = D0r*D0r - D0i*D0i;
			floatexp D_i = (D0r*D0i).mul2();
			TDnr += m_APr[1] * D_r - m_APi[1] * D_i;
			TDni += m_APr[1] * D_i + m_APi[1] * D_r;
			int k;
			for (k = 2; k<m_nTerms; k++){
				floatexp  t = D_r*D0r - D_i*D0i;
				D_i = D_r*D0i + D_i*D0r;
				D_r = t;
				TDnr += m_APr[k] * D_r - m_APi[k] * D_i;
				TDni += m_APr[k] * D_i + m_APi[k] * D_r;
			}
		}
		else{
			antal = 0;
			TDnr = D0r;
			TDni = D0i;
		}
		ldbl Dr, Di;
		AssignFloatExp(&Dr, &TDnr);
		AssignFloatExp(&Di, &TDni);
		double test1 = 0, test2 = 0;
		BOOL bGlitch = FALSE;
		int nMaxIter = (m_nGlitchIter<m_nMaxIter ? m_nGlitchIter : m_nMaxIter);

    if (m_nFractalType == 0 && m_nPower > 10)
    {
			// FIXME check this is still ok around long double vs scaled double zoom threshold e600
			m_nPixels[x][y] = Perturbation_Var(antal, m_ldxr, m_ldxi, &Dr, &Di, &lD0r, &lD0i, &test1, &test2, m_nBailout2, nMaxIter, m_db_z, &bGlitch, m_nPower, m_pnExpConsts);
		}
		else
		{
			int antal2 = antal;
			bool ok = perturbation_long_double(m_nFractalType, m_nPower, m_ldxr, m_ldxi, m_db_z, antal2, test1, test2, bGlitch, m_nBailout2, nMaxIter, m_bNoGlitchDetection, g_real, g_imag, g_FactorAR, g_FactorAI, Dr, Di, lD0r, lD0i);
			assert(ok && "perturbation_long_double");
			m_nPixels[x][y] = antal2;
		}
		antal = m_nPixels[x][y];

		if (antal == m_nGlitchIter)
			bGlitch = TRUE;
		if (!bGlitch && m_nSmoothMethod == 1){
			if (m_nPixels[x][y] == m_nMaxIter)
				m_nTrans[x][y] = 0;
			else{
				double div = sqrt(test1) - sqrt(test2);
				if (div != 0)
					m_nTrans[x][y] = (sqrt(test1) - m_nBailout) / div;
				else
					m_nTrans[x][y] = 0;
			}

		}
		else if (!bGlitch && m_nSmoothMethod == 0){
			if (m_nPixels[x][y] == m_nMaxIter)
				m_nTrans[x][y] = 0;
			else{
				m_nTrans[x][y] = log(log(sqrt(test1))) / log((double)m_nPower);
				if (!ISFLOATOK(m_nTrans[x][y]))
					m_nTrans[x][y] = 0;
				while (m_nTrans[x][y]<0){
					int offs = 1 + (int)m_nTrans[x][y];
					m_nPixels[x][y] += offs;
					m_nTrans[x][y] += offs;
				}
				while (m_nTrans[x][y]>1){
					int offs = (int)m_nTrans[x][y];
					m_nPixels[x][y] -= offs;
					m_nTrans[x][y] -= offs;
				}
			}
		}
		if (bGlitch && !m_bNoGlitchDetection){
			m_nTrans[x][y] = 2;
			m_nPixels[x][y] = m_nMaxIter - 1;//(m_nMaxApproximation?m_nMaxApproximation-1:0);
		}

		InterlockedIncrement((LPLONG)&m_nDone);
		SetColor(nIndex, m_nPixels[x][y], m_nTrans[x][y], x, y);
		if (m_bMirrored)
			Mirror(x, y);
		if (!nPStep && (!bGlitch || g_bShowGlitches)){
			int q;
			int nE = nStepSize*nStepSize;
			for (q = 0; q<nE; q++){
				int tx = x + q%nStepSize;
				int ty = y + q / nStepSize;
				if (tx<m_nX - 1 && ty<m_nY - 1 && m_nPixels[tx][ty] == -1){
					int nIndex1 = tx * 3 + (m_bmi->biHeight - 1 - ty)*m_row;
					m_lpBits[nIndex1] = m_lpBits[nIndex];
					m_lpBits[nIndex1 + 1] = m_lpBits[nIndex + 1];
					m_lpBits[nIndex1 + 2] = m_lpBits[nIndex + 2];
				}
			}
		}
	}
}
int WINAPI ThRenderFractal(CFraktalSFT *p)
{
#ifndef _DEBUG
	try{
#endif
		p->RenderFractal();
#ifndef _DEBUG
	}
	catch (...) {
		p->m_bRunning=FALSE;
//MessageBox(GetActiveWindow(),"Krash - 2","Krash",MB_OK);
	}
#endif
	return 0;
}
void CFraktalSFT::RenderFractal(int nX, int nY, int nMaxIter, HWND hWnd, BOOL bNoThread, BOOL bResetOldGlitch)
{
	m_bStop = TRUE;
	double counter = 0;
	while(m_bRunning)
	{
		Sleep(4);
		counter += 4;
	}
#ifdef KF_DEBUG_SLEEP
	if (counter > 0)
		std::cerr << "RenderFractal() slept for " << counter << "ms" << std::endl;
#endif
	m_bStop = FALSE;
	if (hWnd)
		m_hWnd = hWnd;
	m_nX = nX;
	m_nY = nY;
	m_nMaxIter = nMaxIter;
	m_nRDone = m_nDone = m_nGuessed = 0;
	if (m_pDX){
		delete[] m_pDX;
		m_pDX = NULL;
	}
	if (m_pDY){
		delete[] m_pDY;
		m_pDY = NULL;
	}
	if (m_DX){
		delete[] m_DX;
		m_DX = NULL;
	}
	if (m_DY){
		delete[] m_DY;
		m_DY = NULL;
	}
	if (m_lDX){
		ReleaseArray(m_lDX);
		m_lDX = NULL;
	}
	if (m_lDY){
		ReleaseArray(m_lDY);
		m_lDY = NULL;
	}
	if (bResetOldGlitch)
		memset(m_pOldGlitch, -1, sizeof(m_pOldGlitch));
	if (m_nPower>10 && m_nPrevPower != m_nPower){
		m_nPrevPower = m_nPower;
		if (m_pnExpConsts){
			delete m_pnExpConsts;
			m_pnExpConsts = NULL;
		}
		CStringTable stVal("4,6", "", ",");
		int i, k;
		char szTmp[20];
		for (i = 5; i <= m_nPower; i++){
			stVal[0].InsertString(0, itoa(i, szTmp, 10));
			for (k = 1; k<stVal.GetCount(); k++)
				stVal[k].InsertString(0, itoa(atoi(stVal[k - 1][1]) + atoi(stVal[k][0]), szTmp, 10));
			if (i % 2 == 0){
				stVal.AddRow();
				stVal.AddInt(stVal.GetCount() - 1, atoi(stVal[stVal.GetCount() - 2][1]) * 2);
			}
		}
		m_pnExpConsts = new int[m_nPower - 1];
		k = 0;
		for (i = 0; i<stVal.GetCount(); i++)
			m_pnExpConsts[k++] = atoi(stVal[i][0]);
		if (m_nPower % 2 == 0)
			i--;
		for (i--; i >= 0; i--)
			m_pnExpConsts[k++] = atoi(stVal[i][0]);
	}
	else if (m_nPower <= 10 && m_pnExpConsts){
		delete m_pnExpConsts;
		m_pnExpConsts = NULL;
	}

	WaitForSingleObject(m_hMutex, INFINITE);
	int i;
	if (m_nXPrev != m_nX || m_nYPrev != m_nY){
		if (m_nPixels){
			for (i = 0; i<m_nXPrev; i++)
				delete[] m_nPixels[i];
			delete[] m_nPixels;
		}
		m_nPixels = new int*[m_nX];
		for (i = 0; i<m_nX; i++)
			m_nPixels[i] = new int[m_nY];

		if (m_nTrans){
			for (i = 0; i<m_nXPrev; i++)
				delete[] m_nTrans[i];
			delete[] m_nTrans;
		}
		m_nTrans = new float*[m_nX];
		for (i = 0; i<m_nX; i++){
			m_nTrans[i] = new float[m_nY];
			memset(m_nTrans[i], 0, sizeof(float)*m_nY);
		}

		m_nXPrev = m_nX;
		m_nYPrev = m_nY;
		DeleteObject(m_bmBmp);
		m_bmBmp = NULL;
	}

	HDC hDC = GetDC(NULL);
	if (!m_bmBmp)
		m_bmBmp = create_bitmap(hDC, m_nX, m_nY);
	if (!m_bAddReference){
		if (m_bmi)
			free(m_bmi);
		m_bmi = (BITMAPINFOHEADER *)malloc(sizeof(BITMAPINFOHEADER)+sizeof(RGBQUAD)* 256);
		memset(m_bmi, 0, sizeof(BITMAPINFOHEADER)+sizeof(RGBQUAD)* 256);
		m_bmi->biSize = sizeof(BITMAPINFOHEADER);
		if (!GetDIBits(hDC, m_bmBmp, 0, 0, NULL, (LPBITMAPINFO)m_bmi, DIB_RGB_COLORS))
			/*Beep(1000,10)*/;
		m_bmi->biCompression = m_bmi->biClrUsed = m_bmi->biClrImportant = 0;
		m_bmi->biBitCount = 24;
		if (m_bmi->biBitCount != 24)
			m_bmi->biClrUsed = 1 << m_bmi->biBitCount;
		m_row = ((((m_bmi->biWidth*(DWORD)m_bmi->biBitCount) + 31)&~31) >> 3);
		m_bmi->biSizeImage = m_row*m_bmi->biHeight;
		if (!m_lpBits || (int)m_bmi->biSizeImage != m_nSizeImage){
			m_nSizeImage = m_bmi->biSizeImage;
			if (m_lpBits)
				delete[] m_lpBits;
			m_lpBits = new BYTE[m_bmi->biSizeImage];

			if (!GetDIBits(hDC, m_bmBmp, 0, m_bmi->biHeight, m_lpBits,
				(LPBITMAPINFO)m_bmi, DIB_RGB_COLORS))
				/*Beep(1000,10)*/;
		}
	}
	ReleaseDC(NULL, hDC);
	ReleaseMutex(m_hMutex);

	if (bNoThread){
		SetTimer(m_hWnd, 0, 100, NULL);
		ThRenderFractal(this);
	}
	else{
		m_bRunning = TRUE;
		DWORD dw;
		HANDLE hThread = CreateThread(NULL, 0, (LPTHREAD_START_ROUTINE)ThRenderFractal, (LPVOID)this, 0, &dw);
		CloseHandle(hThread);
	}
}
void CFraktalSFT::RenderFractal()
{
	m_C = cos(g_Degree);
	m_S = sin(g_Degree);

	CFixedFloat div = m_istop - m_istart;

	int nZeroes = 0;
	std::string sszZoom = m_istop.ToText();
	const char *szZoom = sszZoom.c_str();
	if (*szZoom == '-')
		szZoom++;
	if (*szZoom == '0'){
		szZoom++;
		if (*szZoom == '.'){
			szZoom++;
			while (*szZoom == '0'){
				nZeroes++;
				szZoom++;
			}
		}
	}

	{
		Precision q(LOW_PRECISION);
		FixedFloat f(div.m_f);
		f.precision(LOW_PRECISION);
		ToZoom(CDecNumber(FixedFloat(4 / f)), m_nZoom);
	}
	if (m_bAddReference){
		int x, y;
		m_nTotal = 0;
		for (x = 0; x<m_nX; x++)
		for (y = 0; y<m_nY; y++)
		if (m_nPixels[x][y] == -1)
			m_nTotal++;
	}
	else
		m_nTotal = m_nX*m_nY;
	if (m_nZoom>=g_nLDBL && g_LDBL && m_nZoom <= g_nEXP && m_nPower<8){// && !(m_nFractalType==1 && m_nPower==3)){
		if (m_db_dxr){
			delete[] m_db_dxr;
			m_db_dxr = NULL;
		}
		if (m_db_dxi){
			delete[] m_db_dxi;
			m_db_dxi = NULL;
		}
		if (m_dxr){
			delete[] m_dxr;
			m_dxr = NULL;
		}
		if (m_dxi){
			delete[] m_dxi;
			m_dxi = NULL;
		}
#ifdef KF_OPENCL
		if (cl)
		{
			RenderFractalOpenCLEXP();
		}
		else
#endif
		{
			RenderFractalLDBL();
		}
		return;
	}
	else if (m_nZoom>=g_nLDBL){
		if (m_db_dxr){
			delete[] m_db_dxr;
			m_db_dxr = NULL;
		}
		if (m_db_dxi){
			delete[] m_db_dxi;
			m_db_dxi = NULL;
		}
		if (m_ldxr){
			ReleaseArray(m_ldxr);
			m_ldxr = NULL;
		}
		if (m_ldxi){
			ReleaseArray(m_ldxi);
			m_ldxi = NULL;
		}
#ifdef KF_OPENCL
		if (cl)
		{
			RenderFractalOpenCLEXP();
		}
		else
#endif
		{
			RenderFractalEXP();
		}
		return;
	}
	if (m_ldxr){
		ReleaseArray(m_ldxr);
		m_ldxr = NULL;
	}
	if (m_ldxi){
		ReleaseArray(m_ldxi);
		m_ldxi = NULL;
	}
	if (m_dxr){
		delete[] m_dxr;
		m_dxr = NULL;
	}
	if (m_dxi){
		delete[] m_dxi;
		m_dxi = NULL;
	}
	m_P.Init(2, m_nX, m_nY);
	int i;
	if (!m_bReuseRef || !m_db_dxr || m_nZoom<g_nRefZero){
		if (m_bAddReference != 1 || m_nZoom<g_nRefZero){
			if (m_nZoom >= g_nRefZero){
				m_rref = (m_rstop + m_rstart)*.5;
				m_iref = (m_istop + m_istart)*.5;
			}
			else{
				m_rref = 0;
				m_iref = 0;
			}
		}
		m_nScalingOffset = 0;
		m_nScaling = 1;
		for (i = 300; i<m_nZoom; i++){
			m_nScalingOffset++;
			m_nScaling = m_nScaling*.1;
		}
		CalculateReference();
	}
	int x, y;
	if (!m_pDX || !m_pDY){
		CFixedFloat c = m_rstart;
		CFixedFloat step = (m_rstop - m_rstart)*(1 / (double)m_nX);
		m_pDX = new double[m_nX];
		for (x = 0; x<m_nX; x++, c += step)
			m_pDX[x] = (c - m_rref).ToDouble(m_nScalingOffset);
		c = m_istart;
		step = (m_istop - m_istart)*(1 / (double)m_nY);
		m_pDY = new double[m_nY];
		for (y = 0; y<m_nY; y++, c += step)
			m_pDY[y] = (c - m_iref).ToDouble(m_nScalingOffset);
	}

	m_rApprox.left = 0;
	m_rApprox.top = 0;
	m_rApprox.right = m_nX;
	m_rApprox.bottom = m_nY;
/*
	if(m_bAddReference){
		m_rApprox.left = m_nX/2;
		m_rApprox.top = m_nY/2;
		m_rApprox.right = m_nX/2;
		m_rApprox.bottom = m_nY/2;
		for (y = 0; y<m_nY; y++){
			for (x = 0; x<m_nX; x++){
				if(m_nPixels[x][y]==-1){
					if(m_rApprox.left>x)
						m_rApprox.left=x;
					if(m_rApprox.top>y)
						m_rApprox.top=y;
					if(m_rApprox.right<x)
						m_rApprox.right=x;
					if(m_rApprox.bottom<y)
						m_rApprox.bottom=y;
				}
			}
		}
	}
	else{
		m_rApprox.left = 0;
		m_rApprox.top = 0;
		m_rApprox.right = m_nX;
		m_rApprox.bottom = m_nY;
	}
*/
	CalculateApproximation(0);

       if (m_nMaxOldGlitches && m_pOldGlitch[m_nMaxOldGlitches-1].x == -1)
               m_bNoGlitchDetection = FALSE;
       else
               m_bNoGlitchDetection = TRUE;

#ifdef KF_OPENCL
	if (cl)
	{
		RenderFractalOpenCL();
	}
	else
#endif
	{
		// CalcStart
		if (!m_bAddReference){
			for (x = 0; x<m_nX; x++){
				for (y = 0; y<m_nY; y++){
					m_nPixels[x][y] = -1;
					m_nTrans[x][y] = 0;
				}
			}
		}

		SYSTEM_INFO sysinfo;
		GetSystemInfo(&sysinfo);
		int nParallel = 1;
		if (sysinfo.dwNumberOfProcessors>1)
			nParallel = 4 * sysinfo.dwNumberOfProcessors;
		int nStep = m_nX / nParallel;
		int nXStart = 0;

		CParallell P(
#ifdef _DEBUG
			1
#else
			nParallel
#endif
			);
		TH_PARAMS *pMan = new TH_PARAMS[nParallel];
		nXStart = 0;
		for (i = 0; i<nParallel; i++){
			pMan[i].p = this;
			pMan[i].nXStart = nXStart;
			nXStart += nStep;
			if (nXStart>m_nX)
				nXStart = m_nX;
			if (i == nParallel - 1)
				pMan[i].nXStop = m_nX;
			else
				pMan[i].nXStop = nXStart;
			P.AddFunction((LPEXECUTE)ThMandelCalc, &pMan[i]);
			if (pMan[i].nXStop == m_nX && pMan[i].nXStop - pMan[i].nXStart>1 && i<nParallel - 1){
				pMan[i].nXStop--;
				nXStart = pMan[i].nXStop;
			}
			if (pMan[i].nXStop == m_nX){
				break;
			}
		}
		P.Execute();
		P.Reset();
		delete[] pMan;
	}
	m_bAddReference = FALSE;
	if (!m_bNoPostWhenDone)
		PostMessage(m_hWnd, WM_USER + 199, m_bStop, 0);
	m_bNoPostWhenDone = FALSE;
	m_bRunning = FALSE;
}

void CFraktalSFT::RenderFractalLDBL()
{
	m_P.Init(
#ifdef HARD_GUESS_EXP
		3
#else
		2
#endif
		, m_nX, m_nY);
	if (!m_bReuseRef || !m_ldxr){
		if (m_bAddReference != 1 || m_nZoom<g_nRefZero){
			if (m_nZoom >= g_nRefZero){
				m_rref = (m_rstop + m_rstart)*.5;
				m_iref = (m_istop + m_istart)*.5;
			}
			else{
				m_rref = 0;
				m_iref = 0;
			}
		}
		CalculateReferenceLDBL();
	}
	int i;
	int x, y;
	if (!m_lDX || !m_lDY){
		CFixedFloat c = m_rstart;
		CFixedFloat step = (m_rstop - m_rstart)*(1 / (double)m_nX);
		m_lDX = (ldbl*)AllocateArray(m_nX);
		CFixedFloat tmp;
		for (x = 0; x<m_nX; x++, c += step){
			tmp = c - m_rref;
			ConvertFromFixedFloat(&m_lDX[x], tmp);
		}
		c = m_istart;
		step = (m_istop - m_istart)*(1 / (double)m_nY);
		m_lDY = (ldbl*)AllocateArray(m_nY);
		for (y = 0; y<m_nY; y++, c += step){
			tmp = c - m_iref;
			ConvertFromFixedFloat(&m_lDY[y], tmp);
		}
	}
	m_rApprox.left = 0;
	m_rApprox.top = 0;
	m_rApprox.right = m_nX;
	m_rApprox.bottom = m_nY;
	CalculateApproximation(1);

	// CalcStart
	if (!m_bAddReference){
		for (x = 0; x<m_nX; x++){
			for (y = 0; y<m_nY; y++){
				m_nPixels[x][y] = -1;
				m_nTrans[x][y] = 0;
			}
		}
	}

	SYSTEM_INFO sysinfo;
	GetSystemInfo(&sysinfo);
	int nParallel = 1;
	if (sysinfo.dwNumberOfProcessors>1)
		nParallel = 4 * sysinfo.dwNumberOfProcessors;
	CParallell P(
#ifdef _DEBUG
		1
#else
		nParallel
#endif
		);
	TH_PARAMS *pMan = new TH_PARAMS[nParallel];
	int nStep = m_nX / nParallel;
	if (nStep<2)
		nStep = 2;
	else
		nStep++;
	int nXStart = 0;
	for (i = 0; i<nParallel; i++){
		pMan[i].p = this;
		pMan[i].nXStart = nXStart;
		nXStart += nStep;
		if (nXStart>m_nX)
			nXStart = m_nX;
		if (i == nParallel - 1)
			pMan[i].nXStop = m_nX;
		else
			pMan[i].nXStop = nXStart;
		P.AddFunction((LPEXECUTE)ThMandelCalcLDBL, &pMan[i]);
		if (pMan[i].nXStop == m_nX && pMan[i].nXStop - pMan[i].nXStart>1 && i<nParallel - 1){
			pMan[i].nXStop--;
			nXStart = pMan[i].nXStop;
		}
		if (pMan[i].nXStop == m_nX){
			break;
		}
	}
	P.Execute();
	P.Reset();
	delete[] pMan;
	m_bAddReference = FALSE;
	if (m_nMaxOldGlitches && m_pOldGlitch[m_nMaxOldGlitches-1].x == -1)
		m_bNoGlitchDetection = FALSE;
	else
		m_bNoGlitchDetection = TRUE;
	if (!m_bNoPostWhenDone)
		PostMessage(m_hWnd, WM_USER + 199, m_bStop, 0);
	m_bNoPostWhenDone = FALSE;
	m_bRunning = FALSE;
}

void CFraktalSFT::RenderFractalEXP()
{
	m_P.Init(
#ifdef HARD_GUESS_EXP
		3
#else
		2
#endif
		, m_nX, m_nY);
	if (!m_bReuseRef || !m_dxr){
		if (m_bAddReference != 1 || m_nZoom<g_nRefZero){
			if (m_nZoom >= g_nRefZero){
				m_rref = (m_rstop + m_rstart)*.5;
				m_iref = (m_istop + m_istart)*.5;
			}
			else{
				m_rref = 0;
				m_iref = 0;
			}
		}
		CalculateReferenceEXP();
	}
	int i;
	int x, y;
	if (!m_DX || !m_DY){
		CFixedFloat c = m_rstart;
		CFixedFloat step = (m_rstop - m_rstart)*(1 / (double)m_nX);
		m_DX = new floatexp[m_nX];
		for (x = 0; x<m_nX; x++, c += step)
			m_DX[x] = (c - m_rref);
		c = m_istart;
		step = (m_istop - m_istart)*(1 / (double)m_nY);
		m_DY = new floatexp[m_nY];
		for (y = 0; y<m_nY; y++, c += step)
			m_DY[y] = (c - m_iref);
	}
	m_rApprox.left = 0;
	m_rApprox.top = 0;
	m_rApprox.right = m_nX;
	m_rApprox.bottom = m_nY;
	CalculateApproximation(2);

	// CalcStart
	if (!m_bAddReference){
		for (x = 0; x<m_nX; x++){
			for (y = 0; y<m_nY; y++){
				m_nPixels[x][y] = -1;
				m_nTrans[x][y] = 0;
			}
		}
	}

	SYSTEM_INFO sysinfo;
	GetSystemInfo(&sysinfo);
	int nParallel = 1;
	if (sysinfo.dwNumberOfProcessors>1)
		nParallel = 4 * sysinfo.dwNumberOfProcessors;
	CParallell P(
#ifdef _DEBUG
		1
#else
		nParallel
#endif
		);
	TH_PARAMS *pMan = new TH_PARAMS[nParallel];
	int nStep = m_nX / nParallel;
	if (nStep<2)
		nStep = 2;
	else
		nStep++;
	int nXStart = 0;
	for (i = 0; i<nParallel; i++){
		pMan[i].p = this;
		pMan[i].nXStart = nXStart;
		nXStart += nStep;
		if (nXStart>m_nX)
			nXStart = m_nX;
		if (i == nParallel - 1)
			pMan[i].nXStop = m_nX;
		else
			pMan[i].nXStop = nXStart;
		P.AddFunction((LPEXECUTE)ThMandelCalcEXP, &pMan[i]);
		if (pMan[i].nXStop == m_nX && pMan[i].nXStop - pMan[i].nXStart>1 && i<nParallel - 1){
			pMan[i].nXStop--;
			nXStart = pMan[i].nXStop;
		}
		if (pMan[i].nXStop == m_nX){
			break;
		}
	}
	P.Execute();
	delete[] pMan;
	m_bAddReference = FALSE;
	if (m_nMaxOldGlitches && m_pOldGlitch[m_nMaxOldGlitches-1].x == -1)
		m_bNoGlitchDetection = FALSE;
	else
		m_bNoGlitchDetection = TRUE;
	if (!m_bNoPostWhenDone)
		PostMessage(m_hWnd, WM_USER + 199, m_bStop, 0);
	m_bNoPostWhenDone = FALSE;
	m_bRunning = FALSE;
}
void CFraktalSFT::SetPosition(const CFixedFloat &rstart, const CFixedFloat &rstop, const CFixedFloat &istart, const CFixedFloat &istop, int nX, int nY)
{
	m_rstart = rstart;
	m_rstop = rstop;
	m_istart = istart;
	m_istop = istop;
	CFixedFloat re = (rstop + rstart)*.5;
	if (m_nX != nX || m_nY != nY){
		int i;
		for (i = 0; i<m_nXPrev; i++)
			delete[] m_nPixels[i];
		delete[] m_nPixels;
		m_nPixels = NULL;

		for (i = 0; i<m_nXPrev; i++)
			delete[] m_nTrans[i];
		delete[] m_nTrans;
		m_nTrans = NULL;
	}
	m_nX = nX;
	m_nY = nY;

	CFixedFloat d = ((CFixedFloat)nX*(CFixedFloat)((double)1 / nY))*(istop - istart)*.5;
	m_rstart = re - d;
	m_rstop = re + d;
}
void CFraktalSFT::SetPosition(const char *szR, const char *szI, const char *szZ)
{
	Precision pLo(20u);
	CDecNumber z(szZ);
	CDecNumber di(2 / z);
	long e = 0;
	mpf_get_d_2exp(&e, z.m_dec.backend().data());
	unsigned digits10 = std::max(20L, long(20 + 0.30102999566398114 * e));
	Precision pHi(digits10);
	m_rref.m_f.precision(digits10);
	m_iref.m_f.precision(digits10);
	m_rstart.m_f.precision(digits10);
	m_rstop.m_f.precision(digits10);
	m_istart.m_f.precision(digits10);
	m_istop.m_f.precision(digits10);

	m_rref = szR;
	m_iref = szI;
	CDecNumber re(szR);
	CDecNumber im(szI);
	CDecNumber istart(im - di);
	CDecNumber istop(im + di);
	CDecNumber dr(((double)m_scRatio.cx / (double)(m_scRatio.cy))*(istop - istart)*.5);
	CDecNumber rstart(re - dr);
	CDecNumber rstop(re + dr);
	m_rstart = rstart.m_dec;
	m_rstop = rstop.m_dec;
	m_istart = istart.m_dec;
	m_istop = istop.m_dec;
}

#ifdef KF_OPENCL
void CFraktalSFT::RenderFractalOpenCL()
{
	int32_t antal = 0;
	if (m_nMaxApproximation)
	{
		antal = m_nMaxApproximation - 1;
	}
	cl->lock();
  cl->upload_config(m_nX * m_nY, m_nX, m_nY, m_nX, m_nY, antal, m_nMaxIter, m_nGlitchIter, m_bNoGlitchDetection, m_nSmoothMethod, m_nPower, m_nMaxApproximation, m_nTerms, m_nBailout, m_nBailout2, g_real, g_imag, g_FactorAR, g_FactorAI, m_C, m_S, m_bAddReference);
  cl->upload_approximation(m_pDX, m_nX, m_pDY, m_nY, m_APr, m_APi, m_nTerms);
  cl->upload_reference(m_db_dxr, m_db_dxi, m_db_z, m_nMaxIter);
  cl->execute_approximation(0, m_nX * m_nY);
  cl->execute_formula(0, m_nFractalType, m_nPower, m_nX * m_nY);
  cl->download_iterations(m_nPixels, m_nTrans, m_nX, m_nY);
	cl->unlock();
}

void CFraktalSFT::RenderFractalOpenCLEXP()
{
	m_P.Init(
#ifdef HARD_GUESS_EXP
		3
#else
		2
#endif
		, m_nX, m_nY);
	if (!m_bReuseRef || !m_dxr){
		if (m_bAddReference != 1 || m_nZoom<g_nRefZero){
			if (m_nZoom >= g_nRefZero){
				m_rref = (m_rstop + m_rstart)*.5;
				m_iref = (m_istop + m_istart)*.5;
			}
			else{
				m_rref = 0;
				m_iref = 0;
			}
		}
		CalculateReferenceEXP();
	}
	int i;
	int x, y;
	if (!m_DX || !m_DY){
		CFixedFloat c = m_rstart;
		CFixedFloat step = (m_rstop - m_rstart)*(1 / (double)m_nX);
		m_DX = new floatexp[m_nX];
		for (x = 0; x<m_nX; x++, c += step)
			m_DX[x] = (c - m_rref);
		c = m_istart;
		step = (m_istop - m_istart)*(1 / (double)m_nY);
		m_DY = new floatexp[m_nY];
		for (y = 0; y<m_nY; y++, c += step)
			m_DY[y] = (c - m_iref);
	}
	m_rApprox.left = 0;
	m_rApprox.top = 0;
	m_rApprox.right = m_nX;
	m_rApprox.bottom = m_nY;
	CalculateApproximation(2);

#if 0
	// CalcStart
	if (!m_bAddReference){
		for (x = 0; x<m_nX; x++){
			for (y = 0; y<m_nY; y++){
				m_nPixels[x][y] = -1;
				m_nTrans[x][y] = 0;
			}
		}
	}
#endif

	m_bAddReference = FALSE;
	if (m_nMaxOldGlitches && m_pOldGlitch[m_nMaxOldGlitches-1].x == -1)
		m_bNoGlitchDetection = FALSE;
	else
		m_bNoGlitchDetection = TRUE;

	int32_t antal = 0;
	if (m_nMaxApproximation)
	{
		antal = m_nMaxApproximation - 1;
	}
	cl->lock();
  cl->upload_config(m_nX * m_nY, m_nX, m_nY, m_nX, m_nY, antal, m_nMaxIter, m_nGlitchIter, m_bNoGlitchDetection, m_nSmoothMethod, m_nPower, m_nMaxApproximation, m_nTerms, m_nBailout, m_nBailout2, g_real, g_imag, g_FactorAR, g_FactorAI, m_C, m_S, m_bAddReference);
  cl->upload_approximation(m_DX, m_nX, m_DY, m_nY, m_APr, m_APi, m_nTerms);
  cl->upload_reference(m_dxr, m_dxi, m_db_z, m_nMaxIter);
  cl->execute_approximation(1, m_nX * m_nY);
  cl->execute_formula(1, m_nFractalType, m_nPower, m_nX * m_nY);
  cl->download_iterations(m_nPixels, m_nTrans, m_nX, m_nY);
	cl->unlock();

	if (!m_bNoPostWhenDone)
		PostMessage(m_hWnd, WM_USER + 199, m_bStop, 0);
	m_bNoPostWhenDone = FALSE;
	m_bRunning = FALSE;
}
#endif

void CFraktalSFT::SetPosition(const char *szR, const char *szI, const std::string &szZ)
{
	SetPosition(szR, szI, szZ.c_str());
}

HBITMAP CFraktalSFT::GetBitmap()
{
	WaitForSingleObject(m_hMutex, INFINITE);
	if (m_bmi && m_lpBits){
		HDC hDC = GetDC(NULL);
		if (!SetDIBits(hDC, m_bmBmp, 0, m_bmi->biHeight, m_lpBits,
			(LPBITMAPINFO)m_bmi, DIB_RGB_COLORS))
			Beep(1000, 10);
		ReleaseDC(NULL, hDC);
	}
	ReleaseMutex(m_hMutex);
	return m_bmBmp;
}
void CFraktalSFT::UpdateBitmap()
{
	WaitForSingleObject(m_hMutex, INFINITE);
	if (m_bmi && m_lpBits){
		HDC hDC = GetDC(NULL);
		if (!GetDIBits(hDC, m_bmBmp, 0, m_bmi->biHeight, m_lpBits,
			(LPBITMAPINFO)m_bmi, DIB_RGB_COLORS))
			/*Beep(1000,10)*/;
		ReleaseDC(NULL, hDC);
	}
	ReleaseMutex(m_hMutex);
}
int CFraktalSFT::GetWidth()
{
	return m_nX;
}
int CFraktalSFT::GetHeight()
{
	return m_nY;
}
void CFraktalSFT::Stop(BOOL bNoPostWhenDone)
{
	m_bNoPostWhenDone = bNoPostWhenDone;
	m_bStop = TRUE;
	m_bAddReference = FALSE;
	if (m_nMaxOldGlitches && m_pOldGlitch[m_nMaxOldGlitches - 1].x == -1)
		m_bNoGlitchDetection = FALSE;
	else
		m_bNoGlitchDetection = TRUE;
	double counter = 0;
	while (m_bRunning)
	{
		Sleep(1);
		counter += 1;
	}
#ifdef KF_DEBUG_SLEEP
	if (counter > 0)
		std::cerr << "Stop() slept for " << counter << "ms" << std::endl;
#endif
	m_bStop = FALSE;
	m_bNoPostWhenDone=0;
}

void CFraktalSFT::Zoom(int nXPos, int nYPos, double nZoomSize, int nWidth, int nHeight, BOOL bReuseCenter)
{
	Stop(TRUE);
	int **Org;
	float **OrgT;
	int nOX, nOY;
	int i;
	m_C = cos(g_Degree);
	m_S = sin(g_Degree);

	double mr = m_nX / 2;
	double mi = m_nY / 2;
//	if(g_Degree!=0 && (nXPos!=mr || nYPos!=mi))
//		bReuseCenter=FALSE;
	//double ratio = ((double)m_nX*((double)m_nY/(double)m_nX)) / (double)m_scRatio.cy;
	//double ratio = ((double)360/(double)m_nY) * ((double)m_scRatio.cx*((double)m_scRatio.cy/(double)m_scRatio.cx)) / (double)m_scRatio.cy;
	//double ratio = ((double)360/(double)m_nY) * (double)m_nX*((double)m_nY/(double)m_nX) / (double)m_scRatio.cy;
	double ratio = (((double)m_nY/(double)m_nX)/(360.0/640.0)) * ((double)360 / (double)m_scRatio.cy);
	double xpos = (nXPos - mr)*ratio + mr;
	double dbD0r = mr + m_C*(xpos - mr) + m_S*(nYPos - mi);
	double dbD0i = mi - m_S*(xpos - mr) + m_C*(nYPos - mi);
	dbD0r = (dbD0r - mr) / ratio + mr;
//	nXPos = dbD0r;
//	nYPos = dbD0i;

	m_bAddReference = FALSE;
	if (m_nMaxOldGlitches && m_pOldGlitch[m_nMaxOldGlitches-1].x == -1)
		m_bNoGlitchDetection = FALSE;
	else
		m_bNoGlitchDetection = TRUE;
	if (m_nX != nWidth || m_nY != nHeight){
		for (i = 0; i<m_nXPrev; i++)
			delete[] m_nPixels[i];
		delete[] m_nPixels;
		m_nPixels = NULL;

		for (i = 0; i<m_nXPrev; i++)
			delete[] m_nTrans[i];
		delete[] m_nTrans;
		m_nTrans = NULL;
	}
	else if (bReuseCenter && nZoomSize<=1){
		m_bAddReference = 2;
		nOX = nWidth*nZoomSize;
		nOY = nHeight*nZoomSize;
		Org = new int*[nOX];
		for (i = 0; i<nOX; i++)
			Org[i] = new int[nOY];
		OrgT = new float*[nOX];
		for (i = 0; i<nOX; i++)
			OrgT[i] = new float[nOY];
		int x, y, a, b;
		int nX2 = m_nX/2;
		int nY2 = m_nY/2;
		if(0 && nZoomSize<1 && nZoomSize>.8){
			for (x = 0; x<nOX; x++){
				for (y = 0; y<nOY; y++){
					Org[x][y]=-1;
				}
			}
		}
		else{
			for (x = 0; x<nOX; x++){
				for (y = 0; y<nOY; y++){
					a = (x + nXPos-nX2) / nZoomSize;
					b = (y + nYPos-nY2) / nZoomSize;
					if (a >= 0 && a<m_nX && b >= 0 && b<m_nY){
						Org[x][y] = m_nPixels[a][b];
						OrgT[x][y] = m_nTrans[a][b];
						if (Org[x][y]>m_nMaxIter)
							Org[x][y] = m_nMaxIter;
					}
					else
						Org[x][y] = -1;
				}
			}
		}
		a = (nWidth - nOX) / 2;
		b = (nHeight - nOY) / 2;
		for (x = 0; x<m_nX; x++){
			for (y = 0; y<m_nY; y++){
				if (x - a>0 && x - a<nOX - 1 && y - b>0 && y - b<nOY - 1){
					m_nPixels[x][y] = Org[x - a][y - b];
					m_nTrans[x][y] = OrgT[x - a][y - b];
				}
				else
					m_nPixels[x][y] = -1;
			}
		}
		for (i = 0; i<nOX; i++){
			delete[] Org[i];
			delete[] OrgT[i];
		}
		delete[] Org;
		delete[] OrgT;
	}
	m_nX = nWidth;
	m_nY = nHeight;
	unsigned digits10 = 20u;
	{
		using std::abs;
		using std::min;
		Precision pLo(20u);
		CFixedFloat pixelSpacing(min(abs((m_rstop - m_rstart) / m_nX), abs((m_rstop - m_rstart) / m_nY)));
		long e = 0;
		mpf_get_d_2exp(&e, pixelSpacing.m_f.backend().data());
		digits10 = std::max(20.0, 20 + 0.30102999566398114 * (log2(nZoomSize) - e));
	}
	Precision p(digits10);
	m_rstart.m_f.precision(digits10);
	m_rstop.m_f.precision(digits10);
	m_istart.m_f.precision(digits10);
	m_istop.m_f.precision(digits10);
	CFixedFloat nr = (m_rstop - m_rstart)*(CFixedFloat)((double)1 / ((double)nZoomSize * 2));
	CFixedFloat ni = (m_istop - m_istart)*(CFixedFloat)((double)1 / ((double)nZoomSize * 2));
	CFixedFloat offsr = (CFixedFloat)dbD0r*(m_rstop - m_rstart)*(CFixedFloat)((double)1 / m_nX) + m_rstart;
	CFixedFloat offsi = (CFixedFloat)dbD0i*(m_istop - m_istart)*(CFixedFloat)((double)1 / m_nY) + m_istart;
	m_rstart = offsr - nr;
	m_rstop = offsr + nr;
	m_istart = offsi - ni;
	m_istop = offsi + ni;
//	if (bReuseCenter && m_nZoom>g_nRefZero && !m_bReuseRef)
//		AddReference(nXPos + m_nY/10 - 1, nYPos + m_nY/10 - 1);
//	else
		RenderFractal(m_nX, m_nY, m_nMaxIter, m_hWnd);
}

int CFraktalSFT::GetProgress(int *pnGuessed, int *pnRDone, int *pnAP)
{
	int nG = m_nGuessed;
	int nD = m_nDone;
	int nR = m_nRDone;
	int nA = m_nApprox;
	int nMI = m_nMaxIter;
	int nT = m_nTotal;
	while (nD>1000){
		nD /= 10;
		nG /= 10;
		nR /= 10;
		nA /= 10;
		nMI /= 10;
		nT /= 10;
	}
	if (pnGuessed){
		*pnGuessed = nG * 100 / (nD ? nD : 1);
	}
	if (pnRDone)
		*pnRDone = nR * 100 / (nMI ? nMI : 1);
	if (pnAP)
		*pnAP = nA * 100 / (nMI ? nMI : 1);
	if (!m_bmi)
		return 0;
	if (!nT)
		return 100;
	return nD * 100 / nT;
}
char *CFraktalSFT::GetPosition()
{
	CStringTable st;
	if (m_szPosition)
		st.DeleteToText(m_szPosition);
	st.AddRow();
	st.AddString(st.GetCount() - 1, "MANDELBROT:");
	st.AddRow();
	st.AddString(st.GetCount() - 1, "R-Start");
	st.AddString(st.GetCount() - 1, m_rstart.ToText());
	st.AddRow();
	st.AddString(st.GetCount() - 1, "R-Stop");
	st.AddString(st.GetCount() - 1, m_rstop.ToText());
	st.AddRow();
	st.AddString(st.GetCount() - 1, "I-Start");
	st.AddString(st.GetCount() - 1, m_istart.ToText());
	st.AddRow();
	st.AddString(st.GetCount() - 1, "I-Stop");
	st.AddString(st.GetCount() - 1, m_istop.ToText());
	st.AddRow();
	st.AddString(st.GetCount() - 1, "Max-Iter");
	st.AddInt(st.GetCount() - 1, m_nMaxIter);
	if (m_nPixels){
		int x, y;
		int nMax = -1, nMin = -1;
		for (x = 0; x<m_nX; x++){
			for (y = 0; y<m_nY; y++){
				if (nMin == -1 || nMin>m_nPixels[x][y])
					nMin = m_nPixels[x][y];
				if (nMax == -1 || nMax<m_nPixels[x][y])
					nMax = m_nPixels[x][y];
			}
		}
		st.AddRow();
		st.AddString(st.GetCount() - 1, "Iter-Min");
		st.AddInt(st.GetCount() - 1, nMin);
		st.AddRow();
		st.AddString(st.GetCount() - 1, "Iter-Max");
		st.AddInt(st.GetCount() - 1, nMax);
	}
	m_szPosition = st.ToText(":", "\n");
	return m_szPosition;
}
int CFraktalSFT::CountFrames(int nProcent)
{
	char szTmp[10];
	strcpy(szTmp, ToZoom());
	if (strlen(szTmp)>4)
		szTmp[4] = 0;
	return (int)((log10(atof(szTmp)) + m_nZoom) / (log10(1 + (double)2 * (double)nProcent / (double)100))) + 1;

	CFixedFloat t_istart = m_istart;
	CFixedFloat t_istop = m_istop;
	if (nProcent == 0)
		nProcent = 1;

	int i;
	for (i = 0;; i++){
		t_istart = t_istart - (CFixedFloat)nProcent*(t_istop - t_istart)*(CFixedFloat)0.01;
		t_istop = t_istop + (CFixedFloat)nProcent*(t_istop - t_istart)*(CFixedFloat)0.01;
		if ((t_istop - t_istart)>4)
			break;
	}
	return i + 1;
}
void CFraktalSFT::GetIterations(int &nMin, int &nMax, int *pnCalculated, int *pnType, BOOL bSkipMaxIter)
{
	if (m_bIterChanged || pnCalculated){
		int nMA = (m_nMaxApproximation == m_nMaxIter ? 0 : m_nMaxApproximation);
		int nCalc1 = 0;
		int nCalc2 = 0;
		if (pnType)
			*pnType = 0;
		if (m_nPixels){
			int x, y;
			nMax = -1;
			nMin = -1;
			for (x = 0; x<m_nX; x++){
				for (y = 0; y<m_nY; y++){
					if (bSkipMaxIter && m_nPixels[x][y] >= m_nMaxIter - 1)
						continue;
					if (m_nPixels[x][y] != -1 && (nMin == -1 || nMin>m_nPixels[x][y]))
						nMin = m_nPixels[x][y];
					if (m_nPixels[x][y] != -1 && (nMax == -1 || nMax<m_nPixels[x][y]))
						nMax = m_nPixels[x][y];
					if (pnCalculated && m_nPixels[x][y] != -1){
						nCalc1 += m_nPixels[x][y] - nMA;
						if (nCalc1>1000000){
							nCalc2 += nCalc1 / 1000000;
							nCalc1 %= 1000000;
						}
					}
				}
			}
		}
		m_bIterChanged = FALSE;
		m_nMinI = nMin;
		m_nMaxI = nMax;
		if (pnCalculated){
			if (nCalc2>2000){
				if (pnType)
					*pnType = 1;
				*pnCalculated = nCalc2;
			}
			else{
				*pnCalculated = nCalc2 * 1000000 + nCalc1;
			}
		}
	}
	else{
		nMin = m_nMinI;
		nMax = m_nMaxI;
	}
}
int CFraktalSFT::GetIterations()
{
	return m_nMaxIter;
}
void CFraktalSFT::SetIterations(int nIterations)
{
	m_nMaxIter = nIterations;
}
char *CFraktalSFT::GetRe()
{
	CFixedFloat re = (m_rstop + m_rstart)*.5;
	CStringTable stRet;
	if (m_szPosition)
		stRet.DeleteToText(m_szPosition);
	stRet.AddRow();
	stRet.AddString(stRet.GetCount() - 1, re.ToText());
	m_szPosition = stRet.ToText("", "");
	return m_szPosition;
}
char *CFraktalSFT::GetRe(int nXPos, int nYPos, int width, int height)
{
	double mr = width / 2;
	double mi = height / 2;
	double ratio = (((double)height/(double)width)/(360.0/640.0)) * ((double)360 / (double)m_scRatio.cy);
	double xpos = (nXPos - mr)*ratio + mr;
	double dbD0r = mr + m_C*(xpos - mr) + m_S*(nYPos - mi);
	double dbD0i = mi - m_S*(xpos - mr) + m_C*(nYPos - mi);
	dbD0r = (dbD0r - mr) / ratio + mr;

	CFixedFloat re = m_rstart + ((double)dbD0r/(double)width)*(m_rstop - m_rstart);
	CStringTable stRet;
	if (m_szPosition)
		stRet.DeleteToText(m_szPosition);
	stRet.AddRow();
	stRet.AddString(stRet.GetCount() - 1, re.ToText());
	m_szPosition = stRet.ToText("", "");
	return m_szPosition;
}
char *CFraktalSFT::GetIm()
{
	CFixedFloat im = (m_istop + m_istart)*.5;
	CStringTable stRet;
	if (m_szPosition)
		stRet.DeleteToText(m_szPosition);
	stRet.AddRow();
	stRet.AddString(stRet.GetCount() - 1, im.ToText());
	m_szPosition = stRet.ToText("", "");
	return m_szPosition;
}
char *CFraktalSFT::GetIm(int nXPos, int nYPos, int width, int height)
{
	double mr = width / 2;
	double mi = height / 2;
	double ratio = (((double)height/(double)width)/(360.0/640.0)) * ((double)360 / (double)m_scRatio.cy);
	double xpos = (nXPos - mr)*ratio + mr;
	double dbD0r = mr + m_C*(xpos - mr) + m_S*(nYPos - mi);
	double dbD0i = mi - m_S*(xpos - mr) + m_C*(nYPos - mi);
	dbD0r = (dbD0r - mr) / ratio + mr;

	CFixedFloat im = m_istart + ((double)dbD0i/(double)height)*(m_istop - m_istart);
	CStringTable stRet;
	if (m_szPosition)
		stRet.DeleteToText(m_szPosition);
	stRet.AddRow();
	stRet.AddString(stRet.GetCount() - 1, im.ToText());
	m_szPosition = stRet.ToText("", "");
	return m_szPosition;
}
char *CFraktalSFT::GetZoom()
{
	CDecNumber zoom = (CDecNumber)4 / ((CDecNumber)m_istop.ToText() - (CDecNumber)m_istart.ToText());
	CStringTable stRet;
	if (m_szPosition)
		stRet.DeleteToText(m_szPosition);
	stRet.AddRow();
	stRet.AddString(stRet.GetCount() - 1, zoom.ToText());
	m_szPosition = stRet.ToText("", "");
	char *e = strstr(m_szPosition, "e");
	char szNum[20];
	if (!e)
		e = strstr(m_szPosition, "E");
	if (!e){
		char *szP = strstr(m_szPosition, ".");
		if (szP)
			*szP = 0;
		int exp = strlen(m_szPosition) - 1;
		if (exp>2){
			int end = 12;
			if (end>exp)
				end = exp;
			m_szPosition[end + 1] = 0;
			while (end>1){
				m_szPosition[end] = m_szPosition[end - 1];
				end--;
			}
			m_szPosition[end] = '.';
			stRet.SetString(0, 0, m_szPosition);
			wsprintf(szNum, "E%d", exp);
			stRet.AppendString(0, 0, szNum);
			stRet.DeleteToText(m_szPosition);
			m_szPosition = stRet.ToText("", "");
		}

	}
	else{
		m_szPosition[12] = 0;
		stRet.SetString(0, 0, m_szPosition);
		stRet.AppendString(0, 0, e);
	}
	return m_szPosition;
}
BOOL CFraktalSFT::HighestIteration(int &rx, int &ry)
{
	if (!m_nPixels)
		return FALSE;
	int x, y;
	int nMax = -1;
	for (x = 0; x<m_nX; x++)
	for (y = 0; y<m_nY; y++)
	if (m_nPixels[x][y] != -1 && m_nPixels[x][y]>nMax){
		nMax = m_nPixels[x][y];
		rx = x;
		ry = y;
	}
	return TRUE;
}
BOOL CFraktalSFT::Center(int &rx, int &ry, BOOL bSkipM, BOOL bQuick)
{
	if (!m_nPixels)
		return FALSE;
	int x, y;
	int nSegmentX = m_nX / 4;
	int nSegmentY = m_nY / 4;
	int nHalfX = m_nX / 2;
	int nHalfY = m_nY / 2;
	int tx, ty;
	double val, minval = 0;
	BOOL bFirst = TRUE;
	int nStep = (bQuick ? 6 : 4)*m_nX*m_nX / (640 * 640);
	if (nStep == 0)
		nStep = 1;
	GetBitmap();
	int nMin, nMax;
	GetIterations(nMin, nMax, NULL, NULL, TRUE);
	int nMinIter = nMin + (nMax - nMin) / 4;

	for (tx = nSegmentX; tx<nHalfX + nSegmentX; tx++){
		if (bSkipM && tx>nHalfX - nSegmentX / 3 && tx<nHalfX + nSegmentX / 3)
			continue;
		for (ty = nSegmentY; ty<nHalfY + nSegmentY; ty++){
			if(m_nPixels[tx][ty]<nMinIter)
				continue;
			val = 0;
			for (x = 0; x<nSegmentX; x += nStep){
				for (y = 0; y<nSegmentY; y += nStep){
					int x1 = tx - x;
					int x2 = tx + x;
					int y1 = ty - y;
					int y2 = ty + y;
					int nIndex1 = x1 * 3 + (m_bmi->biHeight - 1 - y1)*m_row;
					int nIndex2 = x2 * 3 + (m_bmi->biHeight - 1 - y2)*m_row;
					int	t = (m_lpBits[nIndex1]+m_lpBits[nIndex1+1]+m_lpBits[nIndex1+2])-(m_lpBits[nIndex2]+m_lpBits[nIndex2+1]+m_lpBits[nIndex2+2]);
					val += (t<0 ? -t : t);

					x1 = tx - x;
					x2 = tx + x;
					y1 = ty + y;
					y2 = ty - y;
					nIndex1 = x1 * 3 + (m_bmi->biHeight - 1 - y1)*m_row;
					nIndex2 = x2 * 3 + (m_bmi->biHeight - 1 - y2)*m_row;
					if(nIndex2>m_bmi->biSizeImage-3)
						continue;
					t = (m_lpBits[nIndex1]+m_lpBits[nIndex1+1]+m_lpBits[nIndex1+2])-(m_lpBits[nIndex2]+m_lpBits[nIndex2+1]+m_lpBits[nIndex2+2]);
					val += (t<0 ? -t : t);
				}
			}
			if (bFirst || minval>val){
				rx = tx;
				ry = ty;
				minval = val;
				bFirst = FALSE;
			}
		}
	}
	if (minval == 0)
		return FALSE;
	return TRUE;
}
void CFraktalSFT::ReuseReference(BOOL bReuse)
{
	m_bReuseRef = bReuse;
}

COLOR14 CFraktalSFT::GetKeyColor(int i)
{
	if (i<0 || i >= m_nParts)
		return m_cKeys[0];
	else
		return m_cKeys[i];
}
void CFraktalSFT::SetKeyColor(COLOR14 col, int i)
{
	if (i<0 || i >= m_nParts)
		return;
	m_cKeys[i] = col;
}
COLOR14 CFraktalSFT::GetColor(int i)
{
	if (i<0 || i >= 1024)
		return m_cPos[0];
	else
		return m_cPos[i];
}
BOOL CFraktalSFT::OpenFile(char *szFile, BOOL bNoLocation)
{
	DWORD dw;
	HANDLE hFile = CreateFile(szFile, GENERIC_READ, 0, NULL, OPEN_EXISTING, 0, NULL);
	if (hFile == INVALID_HANDLE_VALUE)
		return FALSE;
	int nData = GetFileSize(hFile, NULL);
	char *szData = new char[nData + 1];
	ReadFile(hFile, szData, nData, &dw, NULL);
	CloseHandle(hFile);
	szData[nData] = 0;
	CStringTable stParams(szData, ": ", "\r\n");
	delete[] szData;
	int nR = stParams.FindString(0, "Re");
	if (nR == -1)
		return FALSE;
	int nI = stParams.FindString(0, "Im");
	if (nI == -1)
		return FALSE;
	int nZ = stParams.FindString(0, "Zoom");
	if (nZ == -1)
		return FALSE;
	int nC = stParams.FindString(0, "Colors");
	if (nC == -1)
		return FALSE;
	int nIterations = stParams.FindString(0, "Iterations");
	if (nIterations == -1)
		return FALSE;
	int nID = stParams.FindString(0, "IterDiv");
	if (nID == -1)
		m_nIterDiv = 1;
	else
		m_nIterDiv = atof(stParams[nID][1]);

	nID = stParams.FindString(0, "Rotate");
	if (nID == -1)
		g_Degree = 0;
	else
		g_Degree = atof(stParams[nID][1]);

	nID = stParams.FindString(0, "Ratio");
	if (nID == -1)
		m_scRatio.cy = 360;
	else
		m_scRatio.cy = atof(stParams[nID][1]);
	SIZE size;
	size.cx = m_nX;
	size.cy = m_nY;
	double xRatio = 640.0/size.cx;
	size.cx = 640;
	size.cy = size.cy*xRatio;
	xRatio = (double)size.cy/(double)360;
	m_scRatio.cy*=xRatio;


	int nT = stParams.FindString(0, "Smooth");
	if (nT != -1)
		m_bTrans = atoi(stParams[nT][1]);
	nID = stParams.FindString(0, "SmoothMethod");
	if (nID != -1){
		m_nSmoothMethod = atoi(stParams[nID][1]);
		if (m_nSmoothMethod<0 || m_nSmoothMethod>1)
			m_nSmoothMethod = 0;
	}
	m_nBailout = m_nSmoothMethod == 1 ? 2 : SMOOTH_BAILOUT;
	m_nBailout2 = m_nBailout*m_nBailout;

	nID = stParams.FindString(0, "ColorMethod");
	if (nID != -1){
		int m = atoi(stParams[nID][1]);
		if (m<0 || m>8)
			m = 0;
		m_nColorMethod = ColorMethod(m);
	}
	else
		m_nColorMethod = ColorMethod_Standard;
	nID = stParams.FindString(0, "Differences");
	if (nID != -1)
	{
		int m = atoi(stParams[nID][1]);
		if (m < 0 || m > 3)
			m = 0;
		m_nDifferences = Differences(m);
	}
	else
		m_nDifferences = Differences_Traditional;
	nID = stParams.FindString(0, "ColorOffset");
	if (nID != -1){
		m_nColorOffset = atoi(stParams[nID][1]);
		if (m_nColorOffset<0 || m_nColorOffset>1023)
			m_nColorOffset = 0;
	}
	else
		m_nColorOffset = 0;
	nID = stParams.FindString(0, "Power");
	if (nID != -1){
		m_nPower = atoi(stParams[nID][1]);
		if (m_nPower<2)
			m_nPower = 2;
	}
	else
		m_nPower = 2;
	nID = stParams.FindString(0, "FractalType");
	if (nID != -1)
		m_nFractalType = atoi(stParams[nID][1]);
	else
		m_nFractalType = 0;

	nID = stParams.FindString(0, "Slopes");
	if (nID != -1)
		m_bSlopes = atoi(stParams[nID][1]);
	else
		m_bSlopes = FALSE;
	nID = stParams.FindString(0, "SlopePower");
	if (nID != -1){
		m_nSlopePower = atoi(stParams[nID][1]);
		if (m_nSlopePower<1)
			m_nSlopePower = 1;
	}
	else
		m_nSlopePower = 100;
	nID = stParams.FindString(0, "SlopeRatio");
	if (nID != -1){
		m_nSlopeRatio = atoi(stParams[nID][1]);
		if (m_nSlopeRatio<0)
			m_nSlopeRatio = 0;
	}
	else
		m_nSlopeRatio = 50;
	nID = stParams.FindString(0, "SlopeAngle");
	if (nID != -1)
		m_nSlopeAngle = atoi(stParams[nID][1]);
	else
		m_nSlopeAngle = 45;

	SetSlopes(m_bSlopes, m_nSlopePower, m_nSlopeRatio, m_nSlopeAngle);

	nID = stParams.FindString(0, "real");
	if (nID != -1)
		g_real = atoi(stParams[nID][1]);
	else
		g_real = 1;
	nID = stParams.FindString(0, "imag");
	if (nID != -1)
		g_imag = atoi(stParams[nID][1]);
	else
		g_imag = 1;

	nID = stParams.FindString(0, "SeedR");
	if (nID != -1)
		g_SeedR = atof(stParams[nID][1]);
	else
		g_SeedR = 0;
	nID = stParams.FindString(0, "SeedI");
	if (nID != -1)
		g_SeedI = atof(stParams[nID][1]);
	else
		g_SeedI = 0;
	nID = stParams.FindString(0, "FactorAR");
	if (nID != -1)
		g_FactorAR = atof(stParams[nID][1]);
	else
		g_FactorAR = 1;
	nID = stParams.FindString(0, "FactorAI");
	if (nID != -1)
		g_FactorAI = atof(stParams[nID][1]);
	else
		g_FactorAI = 0;

	if (g_nLDBL>100){
		if (m_nPower == 2 && !m_nFractalType)
			g_nLDBL = 600;
		else if (m_nPower == 3 && !m_nFractalType)
			g_nLDBL = 400;
		else
			g_nLDBL = 300;
	}

	if (!bNoLocation)
		SetPosition(stParams[nR][1], stParams[nI][1], stParams[nZ][1]);
	CStringTable stColors(stParams[nC][1], "", ",");
	m_nParts = stColors.GetCount() / 3;
	int i;
	for (i = 0; i<m_nParts; i++){
		m_cKeys[i].r = atoi(stColors[i * 3][0]);
		m_cKeys[i].g = atoi(stColors[i * 3 + 1][0]);
		m_cKeys[i].b = atoi(stColors[i * 3 + 2][0]);
	}
	i = stParams.FindString(0, "MultiColor");
	if (i != -1){
		m_bMW = atoi(stParams[i][1]);
		m_nMW = 0;
		i = stParams.FindString(0, "MultiColors");
		if (i != -1){
			stColors.Reset();
			stColors.SplitString(stParams[i][1], "\t", ",");
			for (i = 0; i<stColors.GetCount() && m_nMW<MULTIWAVE_MAX; i++){
				m_MW[m_nMW].nPeriod = atoi(stColors[i][0]);
				m_MW[m_nMW].nStart = atoi(stColors[i][1]);
				m_MW[m_nMW].nType = atoi(stColors[i][2]);
				m_nMW++;
			}
		}
	}
	else
		m_bMW = 0;
	i = stParams.FindString(0, "BlendMC");
	if (i != -1)
		m_bBlend = atoi(stParams[i][1]);
	ApplyColors();
	InvalidateRect(m_hWnd, NULL, FALSE);
	m_nMaxIter = atoi(stParams[nIterations][1]);
	return TRUE;
}
BOOL CFraktalSFT::OpenMapB(char *szFile, BOOL bReuseCenter, double nZoomSize)
{
	int **Org;
	float **OrgT;
	int nOX, nOY;
	int i;
	int x, y, a, b;
	if (bReuseCenter && nZoomSize<1){
		int i;
		nOX = m_nX*nZoomSize;
		nOY = m_nY*nZoomSize;
		Org = new int*[nOX];
		for (i = 0; i<nOX; i++)
			Org[i] = new int[nOY];
		OrgT = new float*[nOX];
		for (i = 0; i<nOX; i++)
			OrgT[i] = new float[nOY];
		for (x = 0; x<nOX; x++){
			for (y = 0; y<nOY; y++){
				a = x / nZoomSize;
				b = y / nZoomSize;
				if (a >= 0 && a<m_nX && b >= 0 && b<m_nY)
					Org[x][y] = m_nPixels[a][b];
				if (a >= 0 && a<m_nX && b >= 0 && b<m_nY)
					OrgT[x][y] = m_nTrans[a][b];
			}
		}
		a = (m_nX - nOX) / 2;
		b = (m_nY - nOY) / 2;
	}
	//HANDLE hFile = CreateFile(szFile,GENERIC_READ,0,NULL,OPEN_EXISTING,0,NULL);
	FILE *hFile = fopen(szFile, "rb");
	//if(hFile==INVALID_HANDLE_VALUE)
	if (hFile == NULL)
		return FALSE;
	DWORD dw;
	char szId[3];
	//ReadFile(hFile,szId,3,&dw,NULL);
	fread(szId, 1, 3, hFile);
	BOOL bNewFormat = FALSE;
	if (!strncmp(szId, "KFC", 3))
		bNewFormat = 1;
	if (!strncmp(szId, "KFD", 3))
		bNewFormat = 2;
	else if (strncmp(szId, "KFB", 3)){
		//CloseHandle(hFile);
		fclose(hFile);
		return FALSE;
	}
	//ReadFile(hFile,&m_nX,sizeof(int),&dw,NULL);
	//ReadFile(hFile,&m_nY,sizeof(int),&dw,NULL);
	fread(&m_nX, 1, sizeof(int), hFile);
	fread(&m_nY, 1, sizeof(int), hFile);
	if (m_nPixels){
		int i;
		for (i = 0; i<m_nXPrev; i++)
			delete[] m_nPixels[i];
		delete[] m_nPixels;

		for (i = 0; i<m_nXPrev; i++)
			delete[] m_nTrans[i];
		delete[] m_nTrans;
	}
	m_nXPrev = m_nX;
	m_nYPrev = m_nY;
	m_nPixels = new int*[m_nX];
	m_nTrans = new float*[m_nX];
	float *pLine = NULL;
	if (bNewFormat == 1)
		pLine = new float[m_nY];
	else if (bNewFormat == 2)
		pLine = new float[m_nX];
	if (bNewFormat == 2){
		for (y = 0; y<m_nY; y++){
			//ReadFile(hFile,pLine,sizeof(float)*m_nX,&dw,NULL);
			fread(pLine, 1, sizeof(float)*m_nX, hFile);
			for (x = 0; x<m_nX; x++){
				if (y == 0){
					m_nTrans[x] = new float[m_nY];
					m_nPixels[x] = new int[m_nY];
				}
				m_nPixels[x][y] = (int)pLine[x];
				m_nTrans[x][y] = pLine[x] - (int)m_nPixels[x][y];
			}
		}
	}
	else{
		for (x = 0; x<m_nX; x++){
			m_nPixels[x] = new int[m_nY];
			if (bNewFormat){
				m_nTrans[x] = new float[m_nY];
				//ReadFile(hFile,pLine,sizeof(float)*m_nY,&dw,NULL);
				fread(pLine, 1, sizeof(float)*m_nY, hFile);
				for (y = 0; y<m_nY; y++){
					m_nPixels[x][y] = (int)pLine[y];
					m_nTrans[x][y] = pLine[y] - (int)m_nPixels[x][y];
				}
			}
			else
				//ReadFile(hFile,m_nPixels[x],sizeof(int)*m_nY,&dw,NULL);
				fread(m_nPixels[x], 1, sizeof(float)*m_nY, hFile);
		}
	}
	if (pLine)
		delete[] pLine;
	//ReadFile(hFile,&m_nIterDiv,sizeof(int),&dw,NULL);
	int div = 1;
	fread(&div, 1, sizeof(int), hFile);
	m_nIterDiv = div;
	if (m_nIterDiv == 0)
		m_nIterDiv = 1;
	//ReadFile(hFile,&m_nParts,sizeof(int),&dw,NULL);
	fread(&m_nParts, 1, sizeof(int), hFile);
	//ReadFile(hFile,m_cKeys,sizeof(COLOR14)*m_nParts,&dw,NULL);
	fread(m_cKeys, 1, sizeof(COLOR14)*m_nParts, hFile);
	int nTest;
	//ReadFile(hFile,&nTest,sizeof(int),&dw,NULL);
	dw = fread(&nTest, 1, sizeof(int), hFile);
	if (sizeof(int) == dw)
		m_nMaxIter = nTest;

	if (!bNewFormat){
		for (x = 0; x<m_nX; x++){
			m_nTrans[x] = new float[m_nY];
			memset(m_nTrans[x], 0, sizeof(float)*m_nY);
			//ReadFile(hFile,m_nTrans[x],sizeof(float)*m_nY,&dw,NULL);
			fread(m_nTrans[x], 1, sizeof(float)*m_nY, hFile);
		}
	}
	//CloseHandle(hFile);
	fclose(hFile);

	if (bReuseCenter && nZoomSize<1){
		for (x = 0; x<m_nX; x++){
			for (y = 0; y<m_nY; y++){
				if (x - a>0 && x - a<nOX - 1 && y - b>0 && y - b<nOY - 1){
					m_nPixels[x][y] = Org[x - a][y - b];
					m_nTrans[x][y] = OrgT[x - a][y - b];
				}
			}
		}
		for (i = 0; i<nOX; i++){
			delete[] Org[i];
			delete[] OrgT[i];
		}
		delete[] Org;
		delete[] OrgT;
	}
	if (m_bmBmp)
		DeleteObject(m_bmBmp);
	HDC hDC = GetDC(NULL);
	m_bmBmp = create_bitmap(hDC, m_nX, m_nY);
	if (m_bmi)
		free(m_bmi);
	m_bmi = (BITMAPINFOHEADER *)malloc(sizeof(BITMAPINFOHEADER)+sizeof(RGBQUAD)* 256);
	memset(m_bmi, 0, sizeof(BITMAPINFOHEADER)+sizeof(RGBQUAD)* 256);
	m_bmi->biSize = sizeof(BITMAPINFOHEADER);
	if (!GetDIBits(hDC, m_bmBmp, 0, 0, NULL, (LPBITMAPINFO)m_bmi, DIB_RGB_COLORS))
		/*Beep(1000,10)*/;
	ReleaseDC(NULL, hDC);
	m_bmi->biCompression = m_bmi->biClrUsed = m_bmi->biClrImportant = 0;
	m_bmi->biBitCount = 24;
	if (m_bmi->biBitCount != 24)
		m_bmi->biClrUsed = 1 << m_bmi->biBitCount;
	m_row = ((((m_bmi->biWidth*(DWORD)m_bmi->biBitCount) + 31)&~31) >> 3);
	m_bmi->biSizeImage = m_row*m_bmi->biHeight;

	if (!m_lpBits || (int)m_bmi->biSizeImage != m_nSizeImage){
		m_nSizeImage = m_bmi->biSizeImage;
		if (m_lpBits)
			delete[] m_lpBits;
		m_lpBits = new BYTE[m_bmi->biSizeImage];

		if (!GetDIBits(hDC, m_bmBmp, 0, m_bmi->biHeight, m_lpBits,
			(LPBITMAPINFO)m_bmi, DIB_RGB_COLORS))
			/*Beep(1000,10)*/;
	}
	return TRUE;
}
BOOL CFraktalSFT::SaveFile(char *szFile)
{
	CStringTable stSave;
	stSave.AddRow();
	stSave.AddString(stSave.GetCount() - 1, "Re");
	stSave.AddString(stSave.GetCount() - 1, GetRe());
	stSave.AddRow();
	stSave.AddString(stSave.GetCount() - 1, "Im");
	stSave.AddString(stSave.GetCount() - 1, GetIm());
	stSave.AddRow();
	stSave.AddString(stSave.GetCount() - 1, "Zoom");
	stSave.AddString(stSave.GetCount() - 1, GetZoom());
	stSave.AddRow();
	stSave.AddString(stSave.GetCount() - 1, "Iterations");
	stSave.AddInt(stSave.GetCount() - 1, m_nMaxIter);
	stSave.AddRow();
	stSave.AddString(stSave.GetCount() - 1, "IterDiv");
	char szDiv[256];
	sprintf(szDiv, "%f", m_nIterDiv);
	stSave.AddString(stSave.GetCount() - 1, szDiv);
	stSave.AddRow();
	stSave.AddString(stSave.GetCount() - 1, "SmoothMethod");
	stSave.AddInt(stSave.GetCount() - 1, m_nSmoothMethod);
	stSave.AddRow();
	stSave.AddString(stSave.GetCount() - 1, "ColorMethod");
	stSave.AddInt(stSave.GetCount() - 1, m_nColorMethod);
	stSave.AddRow();
	stSave.AddString(stSave.GetCount() - 1, "Differences");
	stSave.AddInt(stSave.GetCount() - 1, m_nDifferences);
	stSave.AddRow();
	stSave.AddString(stSave.GetCount() - 1, "ColorOffset");
	stSave.AddInt(stSave.GetCount() - 1, m_nColorOffset);

	stSave.AddRow();
	stSave.AddString(stSave.GetCount() - 1, "Rotate");
	sprintf(szDiv, "%f", g_Degree);
	stSave.AddString(stSave.GetCount() - 1, szDiv);

	stSave.AddRow();
	stSave.AddString(stSave.GetCount() - 1, "Ratio");
	double nRatio = m_scRatio.cy;
	SIZE size;
	size.cx = m_nX;
	size.cy = m_nY;
	double xRatio = 640.0/size.cx;
	size.cx = 640;
	size.cy = size.cy*xRatio;
	xRatio = (double)360/(double)size.cy;
	nRatio*=xRatio;
	sprintf(szDiv, "%f", nRatio);
	stSave.AddString(stSave.GetCount() - 1, szDiv);

	CStringTable stColors;
	int i;
	for (i = 0; i<m_nParts; i++){
		stColors.AddRow();
		stColors.AddInt(stColors.GetCount() - 1, m_cKeys[i].r);
		stColors.AddRow();
		stColors.AddInt(stColors.GetCount() - 1, m_cKeys[i].g);
		stColors.AddRow();
		stColors.AddInt(stColors.GetCount() - 1, m_cKeys[i].b);
	}
	char *szColors = stColors.ToText("", ",");
	stSave.AddRow();
	stSave.AddString(stSave.GetCount() - 1, "Colors");
	stSave.AddString(stSave.GetCount() - 1, szColors);
	stColors.DeleteToText(szColors);
	stSave.AddRow();
	stSave.AddString(stSave.GetCount() - 1, "Smooth");
	stSave.AddInt(stSave.GetCount() - 1, m_bTrans);

	stSave.AddRow();
	stSave.AddString(stSave.GetCount() - 1, "MultiColor");
	stSave.AddInt(stSave.GetCount() - 1, m_bMW);
	stSave.AddRow();
	stSave.AddString(stSave.GetCount() - 1, "BlendMC");
	stSave.AddInt(stSave.GetCount() - 1, m_bBlend);
	stColors.Reset();
	for (i = 0; i<m_nMW; i++){
		stColors.AddRow();
		stColors.AddInt(stColors.GetCount() - 1, m_MW[i].nPeriod);
		stColors.AddInt(stColors.GetCount() - 1, m_MW[i].nStart);
		stColors.AddInt(stColors.GetCount() - 1, m_MW[i].nType);
	}
	szColors = stColors.ToText("\t", ",");
	stSave.AddRow();
	stSave.AddString(stSave.GetCount() - 1, "MultiColors");
	stSave.AddString(stSave.GetCount() - 1, szColors);
	stColors.DeleteToText(szColors);

	stSave.AddRow();
	stSave.AddString(stSave.GetCount() - 1, "Power");
	stSave.AddInt(stSave.GetCount() - 1, m_nPower);

	stSave.AddRow();
	stSave.AddString(stSave.GetCount() - 1, "FractalType");
	stSave.AddInt(stSave.GetCount() - 1, m_nFractalType);

	stSave.AddRow();
	stSave.AddString(stSave.GetCount() - 1, "Slopes");
	stSave.AddInt(stSave.GetCount() - 1, m_bSlopes);
	stSave.AddRow();
	stSave.AddString(stSave.GetCount() - 1, "SlopePower");
	stSave.AddInt(stSave.GetCount() - 1, m_nSlopePower);
	stSave.AddRow();
	stSave.AddString(stSave.GetCount() - 1, "SlopeRatio");
	stSave.AddInt(stSave.GetCount() - 1, m_nSlopeRatio);
	stSave.AddRow();
	stSave.AddString(stSave.GetCount() - 1, "SlopeAngle");
	stSave.AddInt(stSave.GetCount() - 1, m_nSlopeAngle);
	stSave.AddRow();
	stSave.AddString(stSave.GetCount() - 1, "imag");
	stSave.AddInt(stSave.GetCount() - 1, g_imag);
	stSave.AddRow();
	stSave.AddString(stSave.GetCount() - 1, "real");
	stSave.AddInt(stSave.GetCount() - 1, g_real);
	stSave.AddRow();
	char szTmp[50];
	sprintf(szTmp,"%g",g_SeedR);
	stSave.AddRow();
	stSave.AddString(stSave.GetCount() - 1, "SeedR");
	stSave.AddString(stSave.GetCount() - 1, szTmp);
	sprintf(szTmp,"%g",g_SeedI);
	stSave.AddRow();
	stSave.AddString(stSave.GetCount() - 1, "SeedI");
	stSave.AddString(stSave.GetCount() - 1, szTmp);
	sprintf(szTmp,"%g",g_FactorAR);
	stSave.AddRow();
	stSave.AddString(stSave.GetCount() - 1, "FactorAR");
	stSave.AddString(stSave.GetCount() - 1, szTmp);
	sprintf(szTmp,"%g",g_FactorAI);
	stSave.AddRow();
	stSave.AddString(stSave.GetCount() - 1, "FactorAI");
	stSave.AddString(stSave.GetCount() - 1, szTmp);

	DWORD dw;
	HANDLE hFile = CreateFile(szFile, GENERIC_WRITE, 0, NULL, CREATE_ALWAYS, 0, NULL);
	if (hFile == INVALID_HANDLE_VALUE)
		return FALSE;
	char *szData = stSave.ToText(": ", "\r\n");
	WriteFile(hFile, szData, strlen(szData), &dw, NULL);
	stSave.DeleteToText(szData);
	CloseHandle(hFile);
	return TRUE;
}
double CFraktalSFT::GetIterDiv()
{
	return m_nIterDiv;
}
void CFraktalSFT::SetIterDiv(double nIterDiv)
{
	if (nIterDiv>0)
		m_nIterDiv = nIterDiv;
}

int SaveImage(char *szFileName, HBITMAP bmBmp, int nQuality);

int CFraktalSFT::SaveJpg(char *szFile, int nQuality, int nWidth, int nHeight)
{
	if (nWidth == 0)
		nWidth = m_nX;
	if (nHeight == 0)
		nHeight = m_nY;
	if (m_nX == nWidth && m_nY == nHeight)
		return ::SaveImage(szFile, m_bmBmp, nQuality);
	else{
		HDC hDC = GetDC(NULL);
		HDC dcBmp = CreateCompatibleDC(hDC);
		HBITMAP bmOldBmp = (HBITMAP)SelectObject(dcBmp, m_bmBmp);
		HDC dcSave = CreateCompatibleDC(hDC);
		HBITMAP bmSave = create_bitmap(hDC, nWidth, nHeight);
		HBITMAP bmOldSave = (HBITMAP)SelectObject(dcSave, bmSave);
		SetStretchBltMode(dcSave, HALFTONE);
		StretchBlt(dcSave, 0, 0, nWidth, nHeight, dcBmp, 0, 0, m_nX, m_nY, SRCCOPY);
		SelectObject(dcBmp, bmOldBmp);
		SelectObject(dcSave, bmOldSave);
		DeleteDC(dcBmp);
		DeleteDC(dcSave);
		int nRet = ::SaveImage(szFile, bmSave, nQuality);
		DeleteObject(bmSave);
		ReleaseDC(NULL, hDC);
		return nRet;
	}
}
int CFraktalSFT::GetMaxApproximation()
{
	return m_nApprox;
}
int CFraktalSFT::GetIterationOnPoint(int x, int y)
{
	WaitForSingleObject(m_hMutex, INFINITE);
	if (!m_nPixels || m_nXPrev != m_nX || m_nYPrev != m_nY){
		ReleaseMutex(m_hMutex);
		return -1;
	}
	if (x<0 || x >= m_nX || y<0 || y >= m_nY){
		ReleaseMutex(m_hMutex);
		return -1;
	}
	int nRet = m_nPixels[x][y];
	ReleaseMutex(m_hMutex);
	return nRet;
}
int CFraktalSFT::GetTransOnPoint(int x, int y)
{
	if (x<0 || x >= m_nX || y<0 || y >= m_nY || !m_nTrans){
		return 0;
	}
	return (int)(SMOOTH_TOLERANCE*m_nTrans[x][y]);
}
BOOL IsEqual(int a, int b, int nSpan = 2, BOOL bGreaterThan = FALSE)
{
	//	return a==b;
	if (a == -1 || b == -1)
		return 0;
	if (bGreaterThan)
		return a>nSpan;
	int diff = a - b;
	if (diff<0)
		diff = -diff;
	return diff<nSpan;
}
BOOL CFraktalSFT::AddReference(int nXPos, int nYPos, BOOL bEraseAll, BOOL bNP, BOOL bNoGlitchDetection, BOOL bResuming)
{
g_nAddRefX=nXPos;g_nAddRefY=nYPos;

	if (!m_nPixels || (m_nZoom<g_nRefZero && !bEraseAll))
		return FALSE;

	m_C = cos(g_Degree);
	m_S = sin(g_Degree);
	m_bNoGlitchDetection = bNoGlitchDetection;
	if (m_nMaxOldGlitches && m_pOldGlitch[m_nMaxOldGlitches-1].x == -1)
		m_bNoGlitchDetection = FALSE;
	else
		m_bNoGlitchDetection = TRUE;
	int **Pixels = m_nPixels;
	if (m_nZoom >= g_nRefZero){
		double mr = m_nX / 2;
		double mi = m_nY / 2;
		double ratio = (((double)m_nY/(double)m_nX)/(360.0/640.0)) * ((double)360 / (double)m_scRatio.cy);
		//double ratio = ((double)m_nX*((double)m_nY/(double)m_nX)) / (double)m_scRatio.cy;
		//double ratio = ((double)360/(double)m_nY) * ((double)m_scRatio.cx*((double)m_scRatio.cy/(double)m_scRatio.cx)) / (double)m_scRatio.cy;
		//double ratio = ((double)360/(double)m_nY) * (double)m_nX*((double)m_nY/(double)m_nX) / (double)m_scRatio.cy;
		double xpos = (nXPos - mr)*ratio + mr;
		double dbD0r = mr + m_C*(xpos - mr) + m_S*(nYPos - mi);
		double dbD0i = mi - m_S*(xpos - mr) + m_C*(nYPos - mi);
		dbD0r = (dbD0r - mr) / ratio + mr;
		m_rref = (CFixedFloat)dbD0r*(m_rstop - m_rstart)*(CFixedFloat)((double)1 / m_nX) + m_rstart;
		m_iref = (CFixedFloat)dbD0i*(m_istop - m_istart)*(CFixedFloat)((double)1 / m_nY) + m_istart;
	}
	else{
		m_rref = 0;
		m_iref = 0;
	}
	int x, y;
	int i = 0;
	if(nXPos>=0 && nXPos<m_nX && nYPos>=0 && nYPos<m_nY)
		i = Pixels[nXPos][nYPos];
	else
		bResuming=TRUE;

	int nCount = 0;
	if (bEraseAll){
		for (x = 0; x<m_nX; x++)
		for (y = 0; y<m_nY; y++)
			m_nPixels[x][y] = -1;
	}
/*	else if (bNP){
		int **Node = new int*[m_nX];
		for (i = 0; i<m_nX; i++)
			Node[i] = new int[m_nY];
		for (x = 0; x<m_nX; x++)
		for (y = 0; y<m_nY; y++)
			Node[x][y] = m_nPixels[x][y];
		GetArea(Node, nXPos, nYPos, 2, NULL, -1);
		for (x = 0; x<m_nX; x++)
		for (y = 0; y<m_nY; y++)
		if (Node[x][y] == -1)
			m_nPixels[x][y] = -1;
	}
*/	else if (!bResuming){
		int t = SMOOTH_TOLERANCE*m_nTrans[nXPos][nYPos];
		for (x = 0; x<m_nX; x++){
			for (y = 0; y<m_nY; y++){
				if (IsEqual(i, Pixels[x][y], 1)){// && IsEqual(t, (int)(SMOOTH_TOLERANCE*m_nTrans[x][y]), 4)){
					m_nPixels[x][y] = -1;
					nCount++;
				}
			}
		}
	}

	BOOL bPrevReuseRef = m_bReuseRef;
	m_bAddReference = TRUE;
	RenderFractal(m_nX, m_nY, m_nMaxIter, m_hWnd, FALSE, FALSE);
	return TRUE;
}
#define SMOOTH_TO 7
int CFraktalSFT::GetArea(int **Node, int nXStart,int nYStart,int nEqSpan,int **Pixels,int nDone)
{
	int x, y;
	int nAreaC=0;
	int nTarget = m_nPixels[nXStart][nYStart];

	int nQSize = m_nX*m_nY;
	if(nQSize>230400)
		nQSize=230400;
	POINT *pQ = new POINT[nQSize];
	nQSize--;
	int nQ=1;
	pQ[nQ-1].x = nXStart;
	pQ[nQ-1].y = nYStart;
	int nValidate = nQSize*2;
	while(nQ && nValidate){
		x = pQ[nQ-1].x;
		y = pQ[nQ-1].y;
		nQ--;
		nValidate--;
		if(IsEqual(Node[x][y],nTarget,nEqSpan,Pixels?TRUE:FALSE) && nQ<nQSize){
			nAreaC++;
			Node[x][y]=nDone;
			if(Pixels)
				Pixels[x][y]=-1;
			int w=x, e=x;
			while(nValidate && w && IsEqual(Node[w-1][y],nTarget,nEqSpan,Pixels?TRUE:FALSE)){
				nAreaC++;
				nValidate--;
				w--;
				Node[w][y]=nDone;
				if(Pixels)
					Pixels[w][y]=-1;
				if(y &&
					IsEqual(Node[w][y-1],nTarget,nEqSpan,Pixels?TRUE:FALSE)
					&& nQ<nQSize-1){
					nQ++;
					pQ[nQ-1].x = w;
					pQ[nQ-1].y = y-1;
				}
				if(y<m_nY-1 &&
					IsEqual(Node[w][y+1],nTarget,nEqSpan,Pixels?TRUE:FALSE)
					&& nQ<nQSize-1){
					nQ++;
					pQ[nQ-1].x = w;
					pQ[nQ-1].y = y+1;
				}
			}
			while(nValidate && e<m_nX-1 && IsEqual(Node[e+1][y],nTarget,nEqSpan,Pixels?TRUE:FALSE)){
				nAreaC++;
				nValidate--;
				e++;
				Node[e][y]=nDone;
				if(Pixels)
					Pixels[e][y]=-1;
				if(y &&
					IsEqual(Node[e][y-1],nTarget,nEqSpan,Pixels?TRUE:FALSE)
					&& nQ<nQSize-1){
					nQ++;
					pQ[nQ-1].x = e;
					pQ[nQ-1].y = y-1;
				}
				if(y<m_nY-1 &&
					IsEqual(Node[e][y+1],nTarget,nEqSpan,Pixels?TRUE:FALSE)
					&& nQ<nQSize-1){
					nQ++;
					pQ[nQ-1].x = e;
					pQ[nQ-1].y = y+1;
				}
			}
			w=y;
			e=y;
			while(nValidate && w && IsEqual(Node[x][w-1],nTarget,nEqSpan,Pixels?TRUE:FALSE)){
				nAreaC++;
				nValidate--;
				w--;
				Node[x][w]=nDone;
				if(Pixels)
					Pixels[x][w]=-1;
				if(x &&
					IsEqual(Node[x-1][w],nTarget,nEqSpan,Pixels?TRUE:FALSE)
					&& nQ<nQSize-1){
					nQ++;
					pQ[nQ-1].x = x-1;
					pQ[nQ-1].y = w;
				}
				if(x<m_nX-1 &&
					IsEqual(Node[x+1][w],nTarget,nEqSpan,Pixels?TRUE:FALSE)
					&& nQ<nQSize-1){
					nQ++;
					pQ[nQ-1].x = x+1;
					pQ[nQ-1].y = w;
				}
			}
			while(nValidate && e<m_nY-1 && IsEqual(Node[x][e+1],nTarget,nEqSpan,Pixels?TRUE:FALSE)){
				nAreaC++;
				nValidate--;
				e++;
				Node[x][e]=nDone;
				if(Pixels)
					Pixels[x][e]=-1;
				if(x &&
					IsEqual(Node[x-1][e],nTarget,nEqSpan,Pixels?TRUE:FALSE)
					&& nQ<nQSize-1){
					nQ++;
					pQ[nQ-1].x = x-1;
					pQ[nQ-1].y = e;
				}
				if(x<m_nX-1 &&
					IsEqual(Node[x+1][e],nTarget,nEqSpan,Pixels?TRUE:FALSE)
					&& nQ<nQSize-1){
					nQ++;
					pQ[nQ-1].x = x+1;
					pQ[nQ-1].y = e;
				}
			}
		}
	}
	delete [] pQ;
	return nAreaC;
}
BOOL CFraktalSFT::FindCenterOfGlitch(int &ret_x, int &ret_y,BOOL bNP)
{
	int x, y, i=0, io;
	int rx, ry;

	int **Pixels = m_nPixels;
	int **Node = new int*[m_nX];
	for(i=0;i<m_nX;i++)
		Node[i] = new int[m_nY];
	for(x=0;x<m_nX;x++)
		for(y=0;y<m_nY;y++)
			Node[x][y]=Pixels[x][y];

	int nDistance=-1;

	int nHeight = m_nY;
	if(m_bMirrored)
		nHeight=(nHeight+1)/2;

	for(x=1;x<m_nX-1;x++){
		for(y=1;y<nHeight-1;y++){
			int nDone = - (x*m_nY+y);
			if(Node[x][y]>0 && m_nTrans[x][y]==2 && Pixels[x][y]!=m_nMaxIter){
				int nMatch=1;
				if(Pixels[x][y]==Pixels[x][y-1])nMatch++;
				if(Pixels[x][y]==Pixels[x][y+1])nMatch++;
				if(Pixels[x][y]==Pixels[x-1][y])nMatch++;
				if(Pixels[x][y]==Pixels[x+1][y])nMatch++;
/*				if(Pixels[x][y]==Pixels[x-1][y-1])nMatch++;
				if(Pixels[x][y]==Pixels[x+1][y-1])nMatch++;
				if(Pixels[x][y]==Pixels[x-1][y+1])nMatch++;
				if(Pixels[x][y]==Pixels[x+1][y+1])nMatch++;
*/				if(nMatch==1){
					m_nTrans[x][y]=m_nTrans[x-1][y];
					m_nPixels[x][y]=m_nPixels[x-1][y];
					if(m_bMirrored)
						Mirror(x,y);
					continue;
				}
				int nDist = GetArea(Node,x,y,1,NULL,nDone);
				if(nDistance<nDist){
					nDistance=nDist;
					rx=x;
					ry=y;
				}
			}
		}
	}

	if(nDistance!=-1){
		for(io=0;io<m_nMaxOldGlitches;io++){
			if(m_pOldGlitch[io].x==-1)
				break;
		}
/*		if(io<m_nMaxOldGlitches){
			m_pOldGlitch[io].x=rx;
			m_pOldGlitch[io].y=ry;
		}
*/		ret_x=rx;
		ret_y=ry;
		int nMaxDist=0;
		int offs = 1 + nDistance/100000;
		if(io%2==0 && io>3){
			for(x=1;x<m_nX-1;x+=offs){
				for(y=1;y<m_nY-1;y+=offs){
					if(Node[x][y]!=Node[ret_x][ret_y])
						continue;
					int tm=m_nX*m_nY, to;
					for(to=0;x-to>=0 && Node[x-to][y]==Node[ret_x][ret_y];to++);
					if(tm>to)
						tm=to;
					for(to=0;x+to<m_nX && Node[x+to][y]==Node[ret_x][ret_y];to++);
					if(tm>to)
						tm=to;
					for(to=0;y-to>=0 && Node[x][y-to]==Node[ret_x][ret_y];to++);
					if(tm>to)
						tm=to;
					for(to=0;y+to<m_nY && Node[x][y+to]==Node[ret_x][ret_y];to++);
					if(tm>to)
						tm=to;
					for(to=0;x-to>=0 && y-to>=0 && Node[x-to][y-to]==Node[ret_x][ret_y];to++);
					if(tm>to)
						tm=to;
					for(to=0;x+to<m_nX && y+to<m_nY && Node[x+to][y+to]==Node[ret_x][ret_y];to++);
					if(tm>to)
						tm=to;
					for(to=0;x-to>=0 && y+to<m_nY && Node[x-to][y+to]==Node[ret_x][ret_y];to++);
					if(tm>to)
						tm=to;
					for(to=0;x+to<m_nX && y-to>=0 && Node[x+to][y-to]==Node[ret_x][ret_y];to++);
					if(tm>to)
						tm=to;
					if(nMaxDist<tm){
						nMaxDist=tm;
						rx=x;
						ry=y;
					}
				}
			}
			ret_x=rx;
			ret_y=ry;
		}
		else{
			for(x=1;x<m_nX-1;x+=offs){
				for(y=1;y<m_nY-1;y+=offs){
					if(Node[x][y]!=Node[ret_x][ret_y])
						continue;
					int t=0, to, c, ct;
					ct=c=0;
					for(to=0;x-to>=0 && Node[x-to][y]==Node[ret_x][ret_y];to++){
						t++;
						ct++;
					}
					for(to=0;x+to<m_nX && Node[x+to][y]==Node[ret_x][ret_y];to++){
						t++;
						ct--;
					}
					c+=(ct<0?-ct:ct);
					ct=0;
					for(to=0;y-to>=0 && Node[x][y-to]==Node[ret_x][ret_y];to++){
						t++;
						ct++;
					}
					for(to=0;y+to<m_nY && Node[x][y+to]==Node[ret_x][ret_y];to++){
						t++;
						ct--;
					}

					c+=(ct<0?-ct:ct);
					ct=0;
					for(to=0;x-to>=0 && y-to>=0 && Node[x-to][y-to]==Node[ret_x][ret_y];to++){
						t++;
						ct++;
					}
					for(to=0;x+to<m_nX && y+to<m_nY && Node[x+to][y+to]==Node[ret_x][ret_y];to++){
						t++;
						ct--;
					}
					c+=(ct<0?-ct:ct);
					ct=0;
					for(to=0;x-to>=0 && y+to<m_nY && Node[x-to][y+to]==Node[ret_x][ret_y];to++){
						t++;
						ct++;
					}
					for(to=0;x+to<m_nX && y-to>=0 && Node[x+to][y-to]==Node[ret_x][ret_y];to++){
						t++;
						ct--;
					}
					c+=(ct<0?-ct:ct);

					t-=c;

					if(nMaxDist<t){
						nMaxDist=t;
						rx=x;
						ry=y;
					}
				}
			}
			ret_x=rx;
			ret_y=ry;
		}
	}

	if(nDistance!=-1){
		for(io=0;io<m_nMaxOldGlitches;io++)
			if(m_pOldGlitch[io].x==-1 || (m_pOldGlitch[io].x==ret_x && m_pOldGlitch[io].y==ret_y))
				break;
		if(io<1 && m_pOldGlitch[io].x==-1 && Center(rx,ry,FALSE,TRUE) && Pixels[ret_x][ret_y]==Pixels[rx][ry] && Pixels[rx][ry]!=m_nMaxIter){
			ret_x=rx;
			ret_y=ry;
		}
		if(io<m_nMaxOldGlitches && m_pOldGlitch[io].x!=-1){
			m_P.Init(3,m_nX,m_nY);
			while(m_P.GetPixel(x,y)){
				if(Node[x][y]==Node[ret_x][ret_y]){
					for(i=0;i<m_nMaxOldGlitches && m_pOldGlitch[i].x!=-1 && !(m_pOldGlitch[i].x==x && m_pOldGlitch[i].y==y);i++);
					if(i==m_nMaxOldGlitches || m_pOldGlitch[i].x==-1){
						ret_x=x;
						ret_y=y;
						break;
					}
				}
			}
		}
		for(;io<m_nMaxOldGlitches && m_pOldGlitch[io].x!=-1;io++);
		if(io<m_nMaxOldGlitches){
			m_pOldGlitch[io].x=ret_x;
			m_pOldGlitch[io].y=ret_y;
		}
	}
	for(i=0;i<m_nX;i++)
		delete [] Node[i];
	return nDistance!=-1;
}
BOOL CFraktalSFT::GetNoApproximation()
{
	return m_bNoApproximation;
}
void CFraktalSFT::SetNoApproximation(BOOL bNoApproximation)
{
	m_bNoApproximation = bNoApproximation;
}
BOOL CFraktalSFT::GetTransition()
{
	return m_bTrans;
}
void CFraktalSFT::SetTransition(BOOL bTransition)
{
	m_bTrans = bTransition;
}
BOOL CFraktalSFT::GetITransition()
{
	return m_bITrans;
}
void CFraktalSFT::SetITransition(BOOL bITransition)
{
	m_bITrans = bITransition;
}

int CFraktalSFT::GetColorIndex(int x, int y)
{
	if (x<0 || x >= m_nX || y<0 || y >= m_nY || !m_nPixels)
		return -1;
	return ((int)(m_nPixels[x][y] / m_nIterDiv)) % 1024;
}
void CFraktalSFT::SaveMap(char *szFile)
{
	if (!m_nPixels)
		return;
	DWORD dw;
	HANDLE hFile = CreateFile(szFile, GENERIC_WRITE, 0, NULL, CREATE_ALWAYS, 0, NULL);
	int x, y;
	char szNum[64];
	for (y = 0; y<m_nY; y++){
		if (y)
			WriteFile(hFile, "\r\n", 2, &dw, NULL);
		for (x = 0; x<m_nX; x++){
			if (x)
				WriteFile(hFile, " ", 1, &dw, NULL);
			itoa(m_nPixels[x][y], szNum, 10);
			WriteFile(hFile, szNum, strlen(szNum), &dw, NULL);
		}
	}
	char *szC = "\r\nColors: ";
	WriteFile(hFile, szC, strlen(szC), &dw, NULL);
	CStringTable stColors;
	int i;
	for (i = 0; i<m_nParts; i++){
		stColors.AddRow();
		stColors.AddInt(stColors.GetCount() - 1, m_cKeys[i].r);
		stColors.AddRow();
		stColors.AddInt(stColors.GetCount() - 1, m_cKeys[i].g);
		stColors.AddRow();
		stColors.AddInt(stColors.GetCount() - 1, m_cKeys[i].b);
	}
	szC = stColors.ToText("", ",");
	WriteFile(hFile, szC, strlen(szC), &dw, NULL);
	stColors.DeleteToText(szC);
	CloseHandle(hFile);
}
void CFraktalSFT::SaveMapB(char *szFile)
{
	if (!m_nPixels)
		return;
	DWORD dw;
	HANDLE hFile = CreateFile(szFile, GENERIC_WRITE, 0, NULL, CREATE_ALWAYS, 0, NULL);
	int x;
	WriteFile(hFile, "KFB", 3, &dw, NULL);
	WriteFile(hFile, &m_nX, sizeof(m_nX), &dw, NULL);
	WriteFile(hFile, &m_nY, sizeof(m_nY), &dw, NULL);
	for (x = 0; x<m_nX; x++)
		WriteFile(hFile, m_nPixels[x], m_nY*sizeof(int), &dw, NULL);
	int div = m_nIterDiv;
	WriteFile(hFile, &div, sizeof(m_nParts), &dw, NULL);
	WriteFile(hFile, &m_nParts, sizeof(m_nParts), &dw, NULL);
	WriteFile(hFile, m_cKeys, sizeof(COLOR14)*m_nParts, &dw, NULL);
	WriteFile(hFile, &m_nMaxIter, sizeof(int), &dw, NULL);
	for (x = 0; x<m_nX; x++)
		WriteFile(hFile, m_nTrans[x], m_nY*sizeof(float), &dw, NULL);
	CloseHandle(hFile);
}

int CFraktalSFT::GetSmoothMethod()
{
	return m_nSmoothMethod;
}
void CFraktalSFT::SetSmoothMethod(int nSmoothMethod)
{
	if (nSmoothMethod == 0){
		m_nSmoothMethod = 0;
		m_nBailout = SMOOTH_BAILOUT;
		m_nBailout2 = m_nBailout*m_nBailout;
	}
	else if (nSmoothMethod == 1){
		m_nSmoothMethod = 1;
		m_nBailout = 2;
		m_nBailout2 = m_nBailout*m_nBailout;
	}
}
int CFraktalSFT::GetPower()
{
	return m_nPower;
}
void CFraktalSFT::SetPower(int nPower)
{
	m_nPower = nPower;
	if (m_nPower<2)
		m_nPower = 2;
	if (m_nPower>70)
		m_nPower = 70;
//	if (m_nFractalType>4 && m_nPower>3)
//		m_nPower = 3;
	if (g_nLDBL>100){
		if (m_nPower == 2 && !m_nFractalType)
			g_nLDBL = 600;
		else if (m_nPower == 3 && !m_nFractalType)
			g_nLDBL = 400;
		else
			g_nLDBL = 300;
	}
}
BOOL CFraktalSFT::GetLowTolerance()
{
	return m_bLowTolerance;
}
void CFraktalSFT::SetLowTolerance(BOOL bLowTolerance)
{
	m_bLowTolerance = bLowTolerance;
}

BOOL CFraktalSFT::GetGlitchLowTolerance()
{
	return m_bGlitchLowTolerance;
}
void CFraktalSFT::SetGlitchLowTolerance(BOOL bGlitchLowTolerance)
{
	m_bGlitchLowTolerance = bGlitchLowTolerance;
}

void CFraktalSFT::SetDifferences(int nDifferences)
{
	if (nDifferences < 0) nDifferences = 0;
	if (nDifferences > 3) nDifferences = 0;
	m_nDifferences = Differences(nDifferences);
}
Differences CFraktalSFT::GetDifferences()
{
	return m_nDifferences;
}

void CFraktalSFT::SetColorMethod(int nColorMethod)
{
  if (nColorMethod < 0) nColorMethod = 0;
  if (nColorMethod > 8) nColorMethod = 0;
	m_nColorMethod = ColorMethod(nColorMethod);
}
ColorMethod CFraktalSFT::GetColorMethod()
{
	return m_nColorMethod;
}
void CFraktalSFT::SetColorOffset(int nColorOffset)
{
	while (nColorOffset<0)
		nColorOffset += 1024;
	while (nColorOffset >= 1024)
		nColorOffset -= 1024;
	m_nColorOffset = nColorOffset;
}
int CFraktalSFT::GetColorOffset()
{
	return m_nColorOffset;
}
void CFraktalSFT::ErasePixel(int x, int y)
{
	if (x >= 0 && y >= 0 && x<m_nX && y<m_nY){
		m_nPixels[x][y] = 1;
		m_nTrans[x][y] = 0;
		int nIndex = x * 3 + (m_bmi->biHeight - 1 - y)*m_row;
		SetColor(nIndex, m_nPixels[x][y], m_nTrans[x][y], x, y);
		m_nPixels[x][y] = -1;
	}
}
void CFraktalSFT::StoreLocation()
{
	m_storedr = m_rref;
	m_storedi = m_iref;
}
void CFraktalSFT::SetMW(BOOL bMW, BOOL bBlend)
{
	m_bMW = bMW;
	m_bBlend = bBlend;
}
int CFraktalSFT::GetMWCount()
{
	return m_nMW;
}
int CFraktalSFT::GetMW(BOOL *pbBlend)
{
	if (pbBlend)
		*pbBlend = m_bBlend;
	return m_bMW;
}
BOOL CFraktalSFT::GetMW(int nIndex, int &nPeriod, int &nStart, int &nType)
{
	if (nIndex<0 || nIndex >= m_nMW)
		return FALSE;
	nPeriod = m_MW[nIndex].nPeriod;
	nStart = m_MW[nIndex].nStart;
	nType = m_MW[nIndex].nType;
	return TRUE;
}
BOOL CFraktalSFT::AddMW(int nPeriod, int nStart, int nType)
{
	if (m_nMW == MULTIWAVE_MAX - 1)
		return FALSE;
	int i = m_nMW++;
	m_MW[i].nPeriod = nPeriod;
	m_MW[i].nStart = nStart;
	m_MW[i].nType = nType;
	return TRUE;
}
BOOL CFraktalSFT::UpdateMW(int nIndex, int nPeriod, int nStart, int nType)
{
	if (nIndex<0 || nIndex >= m_nMW)
		return FALSE;
	m_MW[nIndex].nPeriod = nPeriod;
	m_MW[nIndex].nStart = nStart;
	m_MW[nIndex].nType = nType;
	return TRUE;
}
BOOL CFraktalSFT::DeleteMW(int nIndex)
{
	int i;
	if (!m_nMW)
		return FALSE;
	m_nMW--;
	for (i = nIndex; i<m_nMW; i++)
		m_MW[i].nPeriod = m_MW[i + 1].nPeriod;
	return TRUE;
}
int CFraktalSFT::GetMaxExceptCenter()
{
	int nMax = 0;
	int x, y;
	int nXO = m_nX / 2 - 2;
	int nYO = m_nY / 2 - 2;
	for (x = 0; x<m_nX; x++)
	for (y = 0; y<m_nY; y++)
	if ((x<nXO || x>m_nX - nXO) && (y<m_nY - nYO || y>3 * nYO) && nMax<m_nPixels[x][y])
		nMax = m_nPixels[x][y];
	return nMax;
}
void CFraktalSFT::SetFractalType(int nFractalType)
{
	m_nFractalType = nFractalType;
	if ((m_nFractalType == 1 || m_nFractalType == 2) && (m_nPower<2 || m_nPower>3))
		m_nPower = 2;
	if (m_nFractalType>2 && m_nPower>2)
		m_nPower = 2;

	if (g_nLDBL>100){
		if (m_nPower == 2 && !m_nFractalType)
			g_nLDBL = 600;
		else if (m_nPower == 3 && !m_nFractalType)
			g_nLDBL = 400;
		else
			g_nLDBL = 300;
	}
}
int CFraktalSFT::GetFractalType()
{
	return m_nFractalType;
}
int CFraktalSFT::GetMaxOldGlitches()
{
	return m_nMaxOldGlitches;
}
void CFraktalSFT::SetMaxOldGlitches(int nMaxOldGlitches)
{
	m_nMaxOldGlitches = nMaxOldGlitches;
	if (m_nMaxOldGlitches<0)
		m_nMaxOldGlitches = 0;
	if (m_nMaxOldGlitches>OLD_GLITCH)
		m_nMaxOldGlitches = OLD_GLITCH;

}
int CFraktalSFT::GetExponent()
{
	return m_nZoom;
}
void CFraktalSFT::SetTerms(int nTerms)
{
	m_nTerms = nTerms;
	if (m_nTerms<1)
		m_nTerms = 1;
	delete[] m_APr;
	delete[] m_APi;
	m_APr = new floatexp[m_nTerms];
	m_APi = new floatexp[m_nTerms];
}
int CFraktalSFT::GetTerms()
{
	return m_nTerms;
}
void CFraktalSFT::SetAutoTerms(BOOL bAuto)
{
	m_bAutoTerms = bAuto;
}
BOOL CFraktalSFT::GetAutoTerms()
{
	return m_bAutoTerms;
}
void CFraktalSFT::SetShowGlitches(BOOL bShowGlitches)
{
	g_bShowGlitches = bShowGlitches;
}
BOOL CFraktalSFT::GetShowGlitches()
{
	return g_bShowGlitches;
}
double CFraktalSFT::GetRatioX()
{
	return m_scRatio.cx;
}
double CFraktalSFT::GetRatioY()
{
	return m_scRatio.cy;
}
void CFraktalSFT::SetRatio(double x, double y)
{
	double xRatio = 640.0/x;
	m_scRatio.cx = 640;
	m_scRatio.cy = y*xRatio;
	CStringTable st;
	st.AddRow();
	st.AddString(0, GetRe());
	st.AddString(0, GetIm());
	SetPosition(st[0][0], st[0][1], GetZoom());
}

BOOL CFraktalSFT::GetSlopes(int &nSlopePower, int &nSlopeRatio, int &nSlopeAngle)
{
	nSlopePower = m_nSlopePower;
	nSlopeRatio = m_nSlopeRatio;
	nSlopeAngle = m_nSlopeAngle;
	return m_bSlopes;
}
void CFraktalSFT::SetSlopes(BOOL bSlope, int nSlopePower, int nSlopeRatio, int nSlopeAngle)
{
	m_bSlopes = bSlope;
	m_nSlopePower = nSlopePower;
	if (m_nSlopePower<1)
		m_nSlopePower = 1;
	m_nSlopeRatio = nSlopeRatio;
	if (m_nSlopeRatio<0)
		m_nSlopeRatio = 0;
	m_nSlopeAngle = nSlopeAngle;

	double lightAngleRadians = pi*(double)m_nSlopeAngle / 180;
	m_nSlopeX = cos(lightAngleRadians);
	m_nSlopeY = sin(lightAngleRadians);
}
BOOL CFraktalSFT::GetTexture(double &nImgMerge,double &nImgPower,int &nImgRatio,char *szTexture)
{
	nImgMerge = m_nImgMerge;
	nImgPower = m_nImgPower;
	nImgRatio = m_nImgRatio;
	strcpy(szTexture,m_szTexture);
	return m_bTexture;
}
void CFraktalSFT::SetTexture(BOOL bTexture,double nImgMerge,double nImgPower,int nImgRatio,char *szTexture)
{
	delete m_lpTextureBits;
	m_lpTextureBits=NULL;
	m_bTexture = bTexture;
	m_nImgMerge = nImgMerge;
	m_nImgPower = nImgPower;
	m_nImgRatio = nImgRatio;
	strcpy(m_szTexture,szTexture);
}

void CFraktalSFT::AddInflectionPont(int nXPos, int nYPos)
{
	m_C = cos(g_Degree);
	m_S = sin(g_Degree);
	double mr = m_nX / 2;
	double mi = m_nY / 2;
	double ratio = (((double)m_nY/(double)m_nX)/(360.0/640.0)) * ((double)360 / (double)m_scRatio.cy);
	double xpos = (nXPos - mr)*ratio + mr;
	double dbD0r = mr + m_C*(xpos - mr) + m_S*(nYPos - mi);
	double dbD0i = mi - m_S*(xpos - mr) + m_C*(nYPos - mi);
	dbD0r = (dbD0r - mr) / ratio + mr;
	int i = m_nInflections++;
	m_pInflections = (complex<CFixedFloat> *)realloc(m_pInflections,sizeof(complex<CFixedFloat>)*m_nInflections);
	m_pInflections[i].m_r = (CFixedFloat)dbD0r*(m_rstop - m_rstart)*(CFixedFloat)((double)1 / m_nX) + m_rstart;
	m_pInflections[i].m_i = (CFixedFloat)dbD0i*(m_istop - m_istart)*(CFixedFloat)((double)1 / m_nY) + m_istart;
}
void CFraktalSFT::RemoveInflectionPoint()
{
	if(m_nInflections)
		m_nInflections--;
}

#ifdef KF_OPENCL
int CFraktalSFT::GetOpenCLDeviceIndex()
{
	return clid;
}
void CFraktalSFT::SetOpenCLDeviceIndex(int i)
{
	if (i != clid)
	{
		if (cl)
		{
			delete cl;
			cl = NULL;
			clid = -1;
		}
		if (0 <= i && i < cldevices.size())
		{
			clid = i;
			cl = new OpenCL(cldevices[i].pid, cldevices[i].did);
		}
	}
}
#endif

CPixels::CPixels()
{
	m_pPixels = NULL;
	m_hMutex = CreateMutex(NULL, 0, NULL);
}
void CPixels::Init(int nStep, int nX, int nY)
{
	BOOL bFirst = TRUE;
	m_nStep = 8;//nStep;
	m_nRectPos = -m_nStep;
	m_nRectPart = 3;
	m_nX = nX;
	m_nX2 = m_nX / 2;
	m_nY = nY;
	m_nY2 = m_nY / 2;
	m_rRect.left = m_rRect.right = m_nX / 2;
	m_rRect.top = m_rRect.bottom = m_nY / 2;
	m_nPixels = m_nX*m_nY;
	m_nNextPixel = -1;
	if (m_pPixels)
		delete[] m_pPixels;
	int *pnDone = new int[m_nPixels];
	memset(pnDone, -1, sizeof(int)*m_nPixels);
	m_pPixels = new POINT[m_nPixels];

	int i = 0;
	int rx, ry;
	CStringTable st;
	st.BuildHash(0);
	while (i<m_nPixels){
		while (1){
			m_nRectPos += m_nStep;
			if (m_nRectPart == 0){
				rx = m_rRect.left + m_nRectPos;
				ry = m_rRect.top;
				if (rx == m_rRect.right){
					m_nRectPart++;
					m_nRectPos = 0;
				}
			}
			else if (m_nRectPart == 1){
				rx = m_rRect.right;
				ry = m_rRect.top + m_nRectPos;
				if (ry == m_rRect.bottom){
					m_nRectPart++;
					m_nRectPos = 0;
				}
			}
			else if (m_nRectPart == 2){
				rx = m_rRect.right - m_nRectPos;
				ry = m_rRect.bottom;
				if (rx == m_rRect.left){
					m_nRectPart++;
					m_nRectPos = 0;
				}
			}
			else if (m_nRectPart == 3){
				rx = m_rRect.left;
				ry = m_rRect.bottom - m_nRectPos;
				if (ry == m_rRect.top){
					m_nRectPart = 0;
					m_nRectPos = -m_nStep;
					m_rRect.left -= m_nStep;
					m_rRect.top -= m_nStep;
					m_rRect.right += m_nStep;
					m_rRect.bottom += m_nStep;
					if (m_rRect.left<0 && m_rRect.top<0 && m_rRect.right >= m_nX && m_rRect.bottom >= m_nY){
						if (m_nStep>1){
							if (bFirst){
								m_nStep = nStep;
								bFirst = FALSE;
								m_nStepPos8 = i;
							}
							else{
								m_nStep = 1;
								m_nStepPos = i;
							}
							m_nRectPos = -m_nStep;
							m_nRectPart = 3;
							m_rRect.left = m_rRect.right = m_nX / 2;
							m_rRect.top = m_rRect.bottom = m_nY / 2;
							continue;
						}
					}
				}
			}
			if (rx >= 0 && ry >= 0 && rx<m_nX && ry<m_nY)
				break;
		}
		int nDoneIndex = m_nX*ry + rx;
		if (pnDone[nDoneIndex] == -1){
			m_pPixels[i].x = rx;
			m_pPixels[i].y = ry;
			pnDone[nDoneIndex] = i;
			i++;
		}
	}
	delete[] pnDone;
	m_nStep = nStep;
}
int CPixels::GetStep()
{
	if (m_nNextPixel<m_nStepPos8)
		return 8;
	if (m_nNextPixel<m_nStepPos)
		return m_nStep;
	return 1;
}
BOOL CPixels::GetPixel(int &rx, int &ry, BOOL bMirrored)
{
#ifdef KF_DEBUG_ALIGNMENT
	static int first = 1;
	if (first)
	{
		first = 0;
		intptr_t i = &m_nNextPixel;
		std::cerr << "alignment = " << (i & 3) << std::endl;
	}
#endif
	do{
		int nNext = InterlockedIncrement((LPLONG)&m_nNextPixel);
		if (nNext<m_nPixels){
			rx = m_pPixels[nNext].x;
			ry = m_pPixels[nNext].y;
			if (bMirrored && ry>m_nY2)
				continue;
			return TRUE;
		}
		return FALSE;
	} while (bMirrored);
	return FALSE;
}
BOOL CPixels::GetPixels(int *prx, int *pry, int &nCount)
{
	int i, bRet = 0;
	if (m_nNextPixel == m_nPixels)
		return FALSE;
	WaitForSingleObject(m_hMutex, INFINITE);
	for (i = 0; i<nCount; i++){
		prx[i] = m_pPixels[m_nNextPixel].x;
		pry[i] = m_pPixels[m_nNextPixel].y;
		m_nNextPixel++;
		if (m_nNextPixel == m_nPixels)
			break;
		bRet = 1;
		return TRUE;
	}
	nCount = i;
	ReleaseMutex(m_hMutex);
	return TRUE;
}
