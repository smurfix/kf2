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
#include <string>
#include <iostream>
#include <set>
#include "../common/bitmap.h"
#include "../formula/formula.h"
#include "colour.h"
#include "jpeg.h"
#include "png.h"
#include "main.h"

double g_real=1;
double g_imag=1;
double g_SeedR=0;
double g_SeedI=0;
double g_FactorAR=1;
double g_FactorAI=0;
#define _abs(a) ((_abs_val=(a))>0?_abs_val:-_abs_val)
#define _SMOOTH_COLORS_
#define SMOOTH_TOLERANCE 256
int g_nLDBL = 600;
int g_nEXP = 9800;
int g_nRefZero = 3;
#define APPROX_GRID 19
#define TERM4
#define TERM5
//#define TERM6
//#define TERM7
int g_nAddRefX = -1, g_nAddRefY = -1;

double g_Degree = 0;
BOOL g_LDBL = TRUE;
#if 0
void(SetParts)(double,double);
int(SizeOfLD)();
int(Version)();
void(AssignInt)(void *p, int nValue);
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
void(DLLConvertFromFixedFloat)(void *p, const mpfr_t value);
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
#endif

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
	m_szTexture="";

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
	m_nMaxOldGlitches = OLD_GLITCH;

	int m_nTerms = GetApproxTerms();
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
	m_nSmoothMethod = SmoothMethod_DE;
	m_nColorMethod = ColorMethod_Standard;
	m_nDifferences = Differences_Traditional;

	m_epsilon = 1.1102230246251565e-16 * (1 << 10);

	m_db_dxr = NULL;
	m_db_dxi = NULL;

	m_lpBits = NULL;
	m_row = 0;
	m_nMaxIter = 200;
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
extern int MakePrime(int n)
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
	std::set<int> stPeriods;
	for (nW = 0; nW<nWaves; nW++){
		int nTests, nPeriod;
		for (nTests = 0; nTests<20; nTests++){
			nPeriod = rand() % (nParts>4 ? nParts / 4 : nParts);
			if (nPeriod == 0)
				nPeriod = 1;
			nPeriod = MakePrime(nPeriod);
			if (stPeriods.count(nPeriod) != 0)
				continue;
			stPeriods.insert(nPeriod);
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

extern void HSVToRGB(double hue, double sat, double bri, COLOR14 &cPos)
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
	s.r = 0;
	s.g = 0;
	s.b = 0;
	if (nIter<0 || (!GetShowGlitches() && GET_TRANS_GLITCH(offs)))
		return;
	if (nIter == m_nMaxIter)
	{
		s.r = s.g = s.b = 1;
	}
	else if (m_nSmoothMethod == SmoothMethod_DE)
	{
		if (offs == TRANS_GLITCH)
		{
			s.b = 1;
		}
		else
		{
			s.r = s.g = s.b = tanh(fmax(0, 0.5 * offs));
		}
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
			if (         0 < y){p[1][0] = m_nPixels[x    ][y - 1] + 1.0 - m_nTrans[x    ][y - 1];}
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
					double g2 = (p[0][2] - p[2][0]) * 0.35355339059327373;
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
			else
			{
				s = nRGB;
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
std::string CFraktalSFT::ToZoom()
{
	CFixedFloat div = m_istop - m_istart;
	return ToZoom((CDecNumber)4 / ((CDecNumber)div.m_f), m_nZoom);
}
std::string CFraktalSFT::ToZoom(const CDecNumber &z, int &zoom)
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

static BOOL ISFLOATOK(double a)
{
	if (a <= DBL_MAX && a >= -DBL_MAX)
		return TRUE;
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

#define GET_EXP(val) ((*((__int64*)&val) & 0x7FF0000000000000)>>52)

//#define HARD_GUESS_EXP
//60 2.5

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
void CFraktalSFT::SetPosition(const std::string &szR, const std::string &szI, const std::string &szZ)
{
	Precision pLo(20u);
	CDecNumber z(szZ);
	CDecNumber di(2 / z);
	long e = 0;
	mpfr_get_d_2exp(&e, z.m_dec.backend().data(), MPFR_RNDN);
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
	m_P.Init(m_nX, m_nY);
	if (!GetReuseReference() || !m_dxr){
		if (m_bAddReference != 1 || m_nZoom<g_nRefZero){
			if (m_nZoom >= g_nRefZero){
				m_rref = (m_rstop + m_rstart)*.5;
				m_iref = (m_istop + m_istart)*.5;
				g_nAddRefX = -1;
				g_nAddRefY = -1;
			}
			else{
				m_rref = 0;
				m_iref = 0;
				g_nAddRefX = -1;
				g_nAddRefY = -1;
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
			{ /*Beep(1000,10)*/ }
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
	else if (bReuseCenter && !GetNoReuseCenter() && nZoomSize<=1){
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
					{
						Org[x][y] = -1;
						OrgT[x][y] = SET_TRANS_GLITCH(0);
					}
				}
			}
		}
		a = (nWidth - nOX) / 2;
		b = (nHeight - nOY) / 2;
		for (x = 0; x<m_nX; x++){
			for (y = 0; y<m_nY; y++){
				if (x - a>=0 && x - a<nOX && y - b>=0 && y - b<nOY){
					m_nPixels[x][y] = Org[x - a][y - b];
					m_nTrans[x][y] = OrgT[x - a][y - b];
				}
				else
				{
					m_nPixels[x][y] = -1;
					m_nTrans[x][y] = SET_TRANS_GLITCH(0);
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
	m_nX = nWidth;
	m_nY = nHeight;
	unsigned digits10 = 20u;
	{
		using std::abs;
		using std::min;
		Precision pLo(20u);
		CFixedFloat pixelSpacing(min(abs((m_rstop - m_rstart) / m_nX), abs((m_rstop - m_rstart) / m_nY)));
		long e = 0;
		mpfr_get_d_2exp(&e, pixelSpacing.m_f.backend().data(), MPFR_RNDN);
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
	if (pnGuessed)
		*pnGuessed = m_nGuessed * 100.0 / (m_nDone ? m_nDone : 1);
	if (pnRDone)
		*pnRDone = m_nRDone * 100.0 / (m_nMaxIter ? m_nMaxIter : 1);
	if (pnAP)
		*pnAP = m_nApprox * 100.0 / (m_nMaxIter ? m_nMaxIter : 1);
	if (!m_bmi)
		return 0;
	if (!m_nTotal)
		return 100;
	return m_nDone * 100.0 / m_nTotal;
}

std::string CFraktalSFT::GetPosition()
{
	CStringTable st;
	if (m_szPosition)
		st.DeleteToText(m_szPosition);
	m_szPosition = nullptr;
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
	double z = std::stod(ToZoom().substr(0, 4));
	return (int)((log10(z) + m_nZoom) / (log10(1 + (double)2 * (double)nProcent / (double)100))) + 1;
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
					if (bSkipMaxIter && m_nPixels[x][y] >= m_nMaxIter - 1) // FIXME what about glitches?
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
std::string CFraktalSFT::GetRe()
{
	CFixedFloat re = (m_rstop + m_rstart)*.5;
	return re.ToText();
}
std::string CFraktalSFT::GetRe(int nXPos, int nYPos, int width, int height)
{
	double mr = width / 2;
	double mi = height / 2;
	double ratio = (((double)height/(double)width)/(360.0/640.0)) * ((double)360 / (double)m_scRatio.cy);
	double xpos = (nXPos - mr)*ratio + mr;
	double dbD0r = mr + m_C*(xpos - mr) + m_S*(nYPos - mi);
	dbD0r = (dbD0r - mr) / ratio + mr;

	CFixedFloat re = m_rstart + ((double)dbD0r/(double)width)*(m_rstop - m_rstart);
	return re.ToText();
}
std::string CFraktalSFT::GetIm()
{
	CFixedFloat im = (m_istop + m_istart)*.5;
	return im.ToText();
}
std::string CFraktalSFT::GetIm(int nXPos, int nYPos, int width, int height)
{
	double mr = width / 2;
	double mi = height / 2;
	double ratio = (((double)height/(double)width)/(360.0/640.0)) * ((double)360 / (double)m_scRatio.cy);
	double xpos = (nXPos - mr)*ratio + mr;
	double dbD0r = mr + m_C*(xpos - mr) + m_S*(nYPos - mi);
	double dbD0i = mi - m_S*(xpos - mr) + m_C*(nYPos - mi);
	dbD0r = (dbD0r - mr) / ratio + mr;

	CFixedFloat im = m_istart + ((double)dbD0i/(double)height)*(m_istop - m_istart);
	return im.ToText();
}
std::string CFraktalSFT::GetZoom()
{
	CDecNumber zoom = (CDecNumber)4 / ((CDecNumber)m_istop.ToText() - (CDecNumber)m_istart.ToText());
	CStringTable stRet;
	if (m_szPosition)
		stRet.DeleteToText(m_szPosition);
	m_szPosition = nullptr;
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
	uint64_t nStep0 = uint64_t(bQuick ? 6 : 4)*uint64_t(m_nX)*uint64_t(m_nX) / uint64_t(640 * 640);
	if (nStep0 <= 0)
		nStep0 = 1;
	if (nStep0 > 1 << 30)
		nStep0 = 1 << 30;
	int nStep = nStep0;
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
					if(((unsigned int)(nIndex2))>m_bmi->biSizeImage-3)
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

void CFraktalSFT::SetImageSize(int nx, int ny)
{
	m_bResized = m_nXPrev != nx || m_nYPrev != ny;
	if (m_bResized && m_nPixels){
		int i;
		for (i = 0; i<m_nXPrev; i++)
			delete[] m_nPixels[i];
		delete[] m_nPixels;
		m_nPixels = 0;

		for (i = 0; i<m_nXPrev; i++)
			delete[] m_nTrans[i];
		delete[] m_nTrans;
		m_nTrans = 0;
	}
	m_nX = nx;
	m_nY = ny;
	m_nXPrev = m_nX;
	m_nYPrev = m_nY;
	if (! m_nPixels)
	{
		m_nPixels = new int*[m_nX];
		m_nTrans = new float*[m_nX];
		for (int x = 0; x<m_nX; x++){
			m_nPixels[x] = new int[m_nY];
			m_nTrans[x] = new float[m_nY];
		}
	}
	for (int x = 0; x<m_nX; x++){
		memset(m_nPixels[x], 0, sizeof(int) * m_nY);
		memset(m_nTrans[x], 0, sizeof(float) * m_nY);
	}
	SetImageWidth(nx);
	SetImageHeight(ny);
}

BOOL CFraktalSFT::OpenMapB(const std::string &szFile, BOOL bReuseCenter, double nZoomSize)
{
	int **Org = 0;
	float **OrgT = 0;
	int nOX = 0, nOY = 0;
	int i;
	int x, y, a = 0, b = 0;
	if (bReuseCenter && !GetNoReuseCenter() && nZoomSize<1){
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
	FILE *hFile = fopen(szFile.c_str(), "rb");
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
	int nx = -1, ny = -1;
	fread(&nx, 1, sizeof(int), hFile);
	fread(&ny, 1, sizeof(int), hFile);
	SetImageSize(nx, ny);
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
				m_nPixels[x][y] = (int)pLine[x];
				m_nTrans[x][y] = pLine[x] - (int)m_nPixels[x][y];
			}
		}
	}
	else{
		for (x = 0; x<m_nX; x++){
			if (bNewFormat){
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

	if (bReuseCenter && !GetNoReuseCenter() && nZoomSize<1){
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
		{ /*Beep(1000,10)*/ }
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
			{ /*Beep(1000,10)*/ }
	}
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

int CFraktalSFT::SaveJpg(const std::string &szFile, int nQuality, int nWidth, int nHeight)
{
	std::string comment1(ToText());
	std::string comment2(m_Settings.ToText());
	std::string comment = comment1 + comment2;
	if (nWidth == 0)
		nWidth = m_nX;
	if (nHeight == 0)
		nHeight = m_nY;
	if (m_nX == nWidth && m_nY == nHeight)
		return ::SaveImage(szFile, m_bmBmp, nQuality, comment);
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
		int nRet = ::SaveImage(szFile, bmSave, nQuality, comment);
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
static BOOL IsEqual(int a, int b, int nSpan = 2, BOOL bGreaterThan = FALSE)
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

#define KF_RERENDER_ONLY_ALL_GLITCHES
BOOL CFraktalSFT::AddReference(int nXPos, int nYPos, BOOL bEraseAll, BOOL bNoGlitchDetection, BOOL bResuming)
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
#ifdef KF_RERENDER_ONLY_ALL_GLITCHES
#else
	int **Pixels = m_nPixels;
#endif
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
		g_nAddRefX = nXPos;
		g_nAddRefY = nYPos;
	}
	else{
		m_rref = 0;
		m_iref = 0;
		g_nAddRefX = -1;
		g_nAddRefY = -1;
	}
	int x, y;
#ifdef KF_RERENDER_ONLY_ALL_GLITCHES
#else
	int i = 0;
#endif
	if(nXPos>=0 && nXPos<m_nX && nYPos>=0 && nYPos<m_nY)
#ifdef KF_RERENDER_ONLY_ALL_GLITCHES
		;
#else
		i = Pixels[nXPos][nYPos];
#endif
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
		for (x = 0; x<m_nX; x++){
			for (y = 0; y<m_nY; y++){
#ifdef KF_RERENDER_ONLY_ALL_GLITCHES
				// re-render all and only glitched pixels
				if (GET_TRANS_GLITCH(m_nTrans[x][y]))
				{
					m_nPixels[x][y] = -1;
					nCount++;
				}
#else
				// re-render all and only pixels with the same integer iteration count
				if (IsEqual(i, Pixels[x][y], 1)){// && IsEqual(t, (int)(SMOOTH_TOLERANCE*m_nTrans[x][y]), 4)){
					m_nPixels[x][y] = -1;
					nCount++;
				}
#endif
			}
		}
	}

	m_bAddReference = TRUE;
	RenderFractal(m_nX, m_nY, m_nMaxIter, m_hWnd, FALSE, FALSE);
	return TRUE;
}
#undef KF_RERENDER_ONLY_ALL_GLITCHES

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
		if(IsEqual(Node[x][y],nTarget,nEqSpan,Pixels?TRUE:FALSE) && nQ<nQSize && GET_TRANS_GLITCH(m_nTrans[x][y])){
			nAreaC++;
			Node[x][y]=nDone;
			if(Pixels && GET_TRANS_GLITCH(m_nTrans[x][y]))
				Pixels[x][y]=-1;
			int w=x, e=x;
			while(nValidate && w && IsEqual(Node[w-1][y],nTarget,nEqSpan,Pixels?TRUE:FALSE) && GET_TRANS_GLITCH(m_nTrans[w-1][y])){
				nAreaC++;
				nValidate--;
				w--;
				Node[w][y]=nDone;
				if(Pixels && GET_TRANS_GLITCH(m_nTrans[w][y]))
					Pixels[w][y]=-1;
				if(y &&
					IsEqual(Node[w][y-1],nTarget,nEqSpan,Pixels?TRUE:FALSE)
					&& nQ<nQSize-1 && GET_TRANS_GLITCH(m_nTrans[w][y-1])){
					nQ++;
					pQ[nQ-1].x = w;
					pQ[nQ-1].y = y-1;
				}
				if(y<m_nY-1 &&
					IsEqual(Node[w][y+1],nTarget,nEqSpan,Pixels?TRUE:FALSE)
					&& nQ<nQSize-1 && GET_TRANS_GLITCH(m_nTrans[w][y+1])){
					nQ++;
					pQ[nQ-1].x = w;
					pQ[nQ-1].y = y+1;
				}
			}
			while(nValidate && e<m_nX-1 && IsEqual(Node[e+1][y],nTarget,nEqSpan,Pixels?TRUE:FALSE) && GET_TRANS_GLITCH(m_nTrans[e+1][y])){
				nAreaC++;
				nValidate--;
				e++;
				Node[e][y]=nDone;
				if(Pixels && GET_TRANS_GLITCH(m_nTrans[e][y]))
					Pixels[e][y]=-1;
				if(y &&
					IsEqual(Node[e][y-1],nTarget,nEqSpan,Pixels?TRUE:FALSE)
					&& nQ<nQSize-1 && GET_TRANS_GLITCH(m_nTrans[e][y-1])){
					nQ++;
					pQ[nQ-1].x = e;
					pQ[nQ-1].y = y-1;
				}
				if(y<m_nY-1 &&
					IsEqual(Node[e][y+1],nTarget,nEqSpan,Pixels?TRUE:FALSE)
					&& nQ<nQSize-1 && GET_TRANS_GLITCH(m_nTrans[e][y+1])){
					nQ++;
					pQ[nQ-1].x = e;
					pQ[nQ-1].y = y+1;
				}
			}
			w=y;
			e=y;
			while(nValidate && w && IsEqual(Node[x][w-1],nTarget,nEqSpan,Pixels?TRUE:FALSE) && GET_TRANS_GLITCH(m_nTrans[x][w-1])){
				nAreaC++;
				nValidate--;
				w--;
				Node[x][w]=nDone;
				if(Pixels && GET_TRANS_GLITCH(m_nTrans[x][w]))
					Pixels[x][w]=-1;
				if(x &&
					IsEqual(Node[x-1][w],nTarget,nEqSpan,Pixels?TRUE:FALSE)
					&& nQ<nQSize-1 && GET_TRANS_GLITCH(m_nTrans[x-1][w])){
					nQ++;
					pQ[nQ-1].x = x-1;
					pQ[nQ-1].y = w;
				}
				if(x<m_nX-1 &&
					IsEqual(Node[x+1][w],nTarget,nEqSpan,Pixels?TRUE:FALSE)
					&& nQ<nQSize-1 && GET_TRANS_GLITCH(m_nTrans[x+1][w])){
					nQ++;
					pQ[nQ-1].x = x+1;
					pQ[nQ-1].y = w;
				}
			}
			while(nValidate && e<m_nY-1 && IsEqual(Node[x][e+1],nTarget,nEqSpan,Pixels?TRUE:FALSE) && GET_TRANS_GLITCH(m_nTrans[x][e+1])){
				nAreaC++;
				nValidate--;
				e++;
				Node[x][e]=nDone;
				if(Pixels && GET_TRANS_GLITCH(m_nTrans[x][e]))
					Pixels[x][e]=-1;
				if(x &&
					IsEqual(Node[x-1][e],nTarget,nEqSpan,Pixels?TRUE:FALSE)
					&& nQ<nQSize-1 && GET_TRANS_GLITCH(m_nTrans[x-1][e])){
					nQ++;
					pQ[nQ-1].x = x-1;
					pQ[nQ-1].y = e;
				}
				if(x<m_nX-1 &&
					IsEqual(Node[x+1][e],nTarget,nEqSpan,Pixels?TRUE:FALSE)
					&& nQ<nQSize-1 && GET_TRANS_GLITCH(m_nTrans[x+1][e])){
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

// old method works better, so don't use the gradient descent for now...
#undef KF_CENTER_VIA_TRANS
int CFraktalSFT::FindCenterOfGlitch(int &ret_x, int &ret_y)
{
	int x, y, i=0, io;
	int rx = -1, ry = -1;

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
	for(x=0;x<m_nX;x++){
		for(y=0;y<nHeight;y++){
			int nDone = - (x*m_nY+y);
			if(Node[x][y]>0 && GET_TRANS_GLITCH(m_nTrans[x][y]) && Pixels[x][y]!=m_nMaxIter){
				if (GetIsolatedGlitchNeighbourhood())
				{
					const double inf = 1.0 / 0.0;
					double p[3][3] = { { inf, inf, inf }, { inf, inf, inf }, { inf, inf, inf } };
					for (int dx = -1; dx <= 1; ++dx)
					{
						for (int dy = -1; dy <= 1; ++dy)
						{
							int x2 = x + dx;
							int y2 = y + dy;
							if (x2 < 0 || m_nX <= x2) x2 = x - dx;
							if (y2 < 0 || m_nY <= y2) y2 = y - dy;
							p[dx+1][dy+1] = GET_TRANS_GLITCH(m_nTrans[x2][y2]);
						}
					}
					int nMatch = 0;
					for (int dx = -1; dx <= 1; ++dx)
					{
						for (int dy = -1; dy <= 1; ++dy)
						{
							if (GetIsolatedGlitchNeighbourhood() == 4 && dx && dy)
							{
								continue;
							}
							nMatch += p[dx+1][dy+1] == p[1][1];
						}
					}
					// this seems to try to "fix" single pixel glitches by copying neighbour
					// this doesn't seem to be desirable for high quality rendering
					// but some like it fast
					if(nMatch==1){
						int x2 = x ? x - 1 : x + 1;
						m_nTrans[x][y]=m_nTrans[x2][y];
						m_nPixels[x][y]=m_nPixels[x2][y];
						if(m_bMirrored)
							Mirror(x,y);
						continue;
					}
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
	// now (rx,ry) is a point in the largest glitch of size (nDistance)
	// or (nDistance == -1) for no glitches

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
			// find the (rx, ry) that maximizes the minimum radius from the center to the edge of the glitch in 8 directions from the point
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
			// find the (rx, ry) that minimizes the total diameter between opposite ends of the glitch in 4 bi-directions from the point
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

#ifdef KF_CENTER_VIA_TRANS
	// find center of glitch via gradient descent
	if (nDistance != -1)
	{
	  // arbitrary loop count limit to help termination guarantee
		for (int k = 0; k < nDistance; ++k)
		{
			// find minimum of neighbourhood (with reflection at image edges)
			double m = 1.0 / 0.0;
			int mx = rx;
			int my = ry;
			for (int dx = -1; dx <= 1; ++dx)
			{
				int px = rx + dx;
				if (px < 0 || px >= m_nX) px = rx - dx;
				for (int dy = -1; dy <= 1; ++dy)
				{
					int py = ry + dy;
					if (py < 0 || py >= m_nY) py = ry - dy;
					if (m_nTrans[px][py] < m) // strict inequality for cycle prevention
					{
						m = m_nTrans[px][py];
						mx = px;
						my = py;
					}
				}
			}
			if (mx == rx && my == ry)
			{
				// found local minimum
				break;
			}
			rx = mx;
			ry = my;
		}
		ret_x = rx;
		ret_y = ry;
	}
#endif

	if(nDistance!=-1){
		for(io=0;io<m_nMaxOldGlitches;io++)
			if(m_pOldGlitch[io].x==-1 || (m_pOldGlitch[io].x==ret_x && m_pOldGlitch[io].y==ret_y))
				break;
		if(io<1 && m_pOldGlitch[io].x==-1 && Center(rx,ry,FALSE,TRUE) && Pixels[ret_x][ret_y]==Pixels[rx][ry] && Pixels[rx][ry]!=m_nMaxIter){
			ret_x=rx;
			ret_y=ry;
		}
		if(io<m_nMaxOldGlitches && m_pOldGlitch[io].x!=-1){
			m_P.Init(m_nX, m_nY);
			int w,h;
			while(m_P.GetPixel(x,y,w,h)){
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
	delete[] Node;
	return nDistance + 1; // -1 becomes 0
}
#undef KF_CENTER_VIA_TRANS

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
void CFraktalSFT::SaveMap(const std::string &szFile)
{
	if (!m_nPixels)
		return;
	DWORD dw;
	HANDLE hFile = CreateFile(szFile.c_str(), GENERIC_WRITE, 0, NULL, CREATE_ALWAYS, 0, NULL);
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
	const char *szC0 = "\r\nColors: ";
	WriteFile(hFile, szC0, strlen(szC0), &dw, NULL);
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
	char *szC = stColors.ToText("", ",");
	WriteFile(hFile, szC, strlen(szC), &dw, NULL);
	stColors.DeleteToText(szC);
	CloseHandle(hFile);
}
void CFraktalSFT::SaveMapB(const std::string &szFile)
{
	if (!m_nPixels)
		return;
	DWORD dw;
	HANDLE hFile = CreateFile(szFile.c_str(), GENERIC_WRITE, 0, NULL, CREATE_ALWAYS, 0, NULL);
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

SmoothMethod CFraktalSFT::GetSmoothMethod()
{
	return m_nSmoothMethod;
}
void CFraktalSFT::SetSmoothMethod(int nSmoothMethod)
{
	if (nSmoothMethod == 0 || nSmoothMethod == 2){
		m_nSmoothMethod = SmoothMethod(nSmoothMethod);
		m_nBailout = SMOOTH_BAILOUT;
		m_nBailout2 = m_nBailout*m_nBailout;
	}
	else if (nSmoothMethod == 1){
		m_nSmoothMethod = SmoothMethod(nSmoothMethod);
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
			g_nLDBL = LONG_DOUBLE_THRESHOLD_POWER_2_MANDELBROT;
		else if (m_nPower == 3 && !m_nFractalType)
			g_nLDBL = LONG_DOUBLE_THRESHOLD_POWER_3_MANDELBROT;
		else
			g_nLDBL = LONG_DOUBLE_THRESHOLD_DEFAULT;
	}
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
			g_nLDBL = LONG_DOUBLE_THRESHOLD_POWER_2_MANDELBROT;
		else if (m_nPower == 3 && !m_nFractalType)
			g_nLDBL = LONG_DOUBLE_THRESHOLD_POWER_3_MANDELBROT;
		else
			g_nLDBL = LONG_DOUBLE_THRESHOLD_DEFAULT;
	}
}
int CFraktalSFT::GetFractalType()
{
	return m_nFractalType;
}

int CFraktalSFT::GetExponent()
{
	return m_nZoom;
}

void CFraktalSFT::SetApproxTerms(int nTerms)
{
	m_Settings.SetApproxTerms(nTerms);
	int m_nTerms = GetApproxTerms();
	delete[] m_APr;
	delete[] m_APi;
	m_APr = new floatexp[m_nTerms];
	m_APi = new floatexp[m_nTerms];
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
BOOL CFraktalSFT::GetTexture(double &nImgMerge,double &nImgPower,int &nImgRatio,std::string &szTexture)
{
	nImgMerge = m_nImgMerge;
	nImgPower = m_nImgPower;
	nImgRatio = m_nImgRatio;
	szTexture = m_szTexture;
	return m_bTexture;
}
void CFraktalSFT::SetTexture(BOOL bTexture,double nImgMerge,double nImgPower,int nImgRatio,const std::string &szTexture)
{
	delete[] m_lpTextureBits;
	m_lpTextureBits=NULL;
	m_bTexture = bTexture;
	m_nImgMerge = nImgMerge;
	m_nImgPower = nImgPower;
	m_nImgRatio = nImgRatio;
	m_szTexture = szTexture;
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

struct CPixel
{
	uint16_t x, y; uint8_t w, h;
	CPixel(int x, int y, int w, int h) : x(x), y(y), w(w), h(h) { }
	CPixel() : CPixel(0, 0, 0, 0) { }
};

struct CPixelComparator {
	double x;
	double y;
	CPixelComparator(int x, int y) : x(x), y(y) { }
  bool operator() (const CPixel &a, const CPixel &b)
  {
		double dax = a.x - x;
		double day = a.y - y;
		double dbx = b.x - x;
		double dby = b.y - y;
		double da = dax * dax + day * day;
		double db = dbx * dbx + dby * dby;
		return da < db;
	}
};

void CPixels::Init(int width, int height)
{
	m_nNextPixel = -1;
	if (m_nX == width && m_nY == height && m_pPixels)
		return;
	m_nX = width;
	m_nY = height;
	m_nY2 = m_nY >> 1;
	m_nPixels = m_nX*m_nY;
	if (m_pPixels)
		delete[] m_pPixels;
	CPixel *pixels = m_pPixels = new CPixel[m_nPixels];

  // Adam7-style interlacing
  CPixelComparator cmp(width >> 1, height >> 1);
	int step = 1 << 7;
	int ix = 0;
	int begin = ix;
	for (int y = 0; y < height; y += step)
		for (int x = 0; x < width; x += step)
			pixels[ix++] = CPixel(x, y, step, step);
	int end = ix;
	std::sort(&pixels[begin], &pixels[end], cmp);
  for (; step > 1; step >>= 1)
  {
		begin = ix;
		for (int y = 0;  y < height;  y += step)
			for (int x = step >> 1; x < width; x += step)
				pixels[ix++] = CPixel(x, y, step >> 1, step);
		end = ix;
		std::sort(&pixels[begin], &pixels[end], cmp);
		begin = ix;
		for (int y = step >> 1; y < height; y += step)
			for (int x = 0; x < width; x += step >> 1)
				pixels[ix++] = CPixel(x, y, step >> 1, step >> 1);
		end = ix;
		std::sort(&pixels[begin], &pixels[end], cmp);
  }
	assert(ix == width * height);
}

BOOL CPixels::GetPixel(int &rx, int &ry, int &rw, int &rh, BOOL bMirrored)
{
	do{
		int nNext = InterlockedIncrement(&m_nNextPixel);
		if (nNext < m_nPixels){
			rx = m_pPixels[nNext].x;
			ry = m_pPixels[nNext].y;
			rw = m_pPixels[nNext].w;
			rh = m_pPixels[nNext].h;
			if (bMirrored && ry>m_nY2)
				continue;
			return TRUE;
		}
		return FALSE;
	} while (bMirrored);
	return FALSE;
}

void CFraktalSFT::OutputIterationData(int x, int y, int bGlitch, int antal, double test1, double test2, double de)
{
		int nIndex = x * 3 + (m_bmi->biHeight - 1 - y)*m_row;
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

			if (!bGlitch && m_nSmoothMethod == SmoothMethod_DE)
			{
				m_nTrans[x][y] = de;
			}

			else if (!bGlitch && m_nSmoothMethod == SmoothMethod_Sqrt){
				double div = sqrt(test1) - sqrt(test2);
				if (div != 0)
					m_nTrans[x][y] = (sqrt(test1) - m_nBailout) / div;
				else
					m_nTrans[x][y] = 0;
			}

			else if (!bGlitch && m_nSmoothMethod == SmoothMethod_Log){
				double t = log(log(sqrt(test1))) / log((double)m_nPower);
				if (!ISFLOATOK(t))
					t = 0;
				int i = floor(t);
				m_nPixels[x][y] -= i;
				m_nTrans[x][y] = t - i;
			}

			if (bGlitch && !m_bNoGlitchDetection){
				m_nTrans[x][y] = SET_TRANS_GLITCH(test1);
			}
			SetColor(nIndex, m_nPixels[x][y], m_nTrans[x][y], x, y);
		}
		if (m_bMirrored)
			Mirror(x, y);
}

void CFraktalSFT::OutputPixelData(int x, int y, int w, int h, int bGlitch)
{
		if (!bGlitch || GetShowGlitches())
    {
      int nIndex = x * 3 + (m_bmi->biHeight - 1 - y) * m_row;
      for (int ty = 0; ty < h; ++ty)
      {
        int y2 = y + ty;
        if (y2 < m_nY)
        {
          for (int tx = 0; tx < w; ++tx)
          {
            int x2 = x + tx;
            if (x2 < m_nX)
            {
              if (m_nPixels[x2][y2] == -1)
              {
                int index2 = x2 * 3 + (m_bmi->biHeight - 1 - y2) * m_row;
                m_lpBits[index2    ] = m_lpBits[nIndex    ];
                m_lpBits[index2 + 1] = m_lpBits[nIndex + 1];
                m_lpBits[index2 + 2] = m_lpBits[nIndex + 2];
              }
            }
          }
				}
			}
		}
}

bool CFraktalSFT::GuessPixel(int x, int y, int w, int h)
{
	int nIndex = x * 3 + (m_bmi->biHeight - 1 - y) * m_row;
	if (GetGuessing())
	{
		if (w == 1 && h <= 2)
		{
			if (x && x<m_nX - 1 && m_nPixels[x - 1][y] != -1 && m_nPixels[x - 1][y] == m_nPixels[x + 1][y] && GET_TRANS_GLITCH(m_nTrans[x - 1][y]) == GET_TRANS_GLITCH(m_nTrans[x + 1][y])){
				m_nTrans[x][y] = (m_nTrans[x - 1][y] + m_nTrans[x + 1][y])*.5;
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
				return true;
			}
		}
		if (w == 1 && h == 1)
		{
			if (y && y<m_nY - 1 && m_nPixels[x][y - 1] != -1 && m_nPixels[x][y - 1] == m_nPixels[x][y + 1] && GET_TRANS_GLITCH(m_nTrans[x][y - 1]) == GET_TRANS_GLITCH(m_nTrans[x][y + 1])){
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
				return true;
			}
			if (y && y<m_nY - 1 && x && x<m_nX - 1 && m_nPixels[x - 1][y - 1] != -1 && m_nPixels[x - 1][y - 1] == m_nPixels[x + 1][y + 1] && GET_TRANS_GLITCH(m_nTrans[x - 1][y - 1]) == GET_TRANS_GLITCH(m_nTrans[x + 1][y + 1])){
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
				return true;
			}
			if (y && y<m_nY - 1 && x && x<m_nX - 1 && m_nPixels[x - 1][y + 1] != -1 && m_nPixels[x - 1][y + 1] == m_nPixels[x + 1][y - 1] && GET_TRANS_GLITCH(m_nTrans[x - 1][y + 1]) == GET_TRANS_GLITCH(m_nTrans[x + 1][y - 1])){
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
				return true;
			}
#ifdef HARD_GUESS_EXP
FIXME TODO need to add GET_TRANS_GLITCH check to these when enabling the define...
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
				return true;
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
				return true;
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
				return true;
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
				return true;
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
				return true;
			}
#endif
	  }
	}
	return false;
}
