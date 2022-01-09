/*
Kalles Fraktaler 2
Copyright (C) 2013-2017 Karl Runmo
Copyright (C) 2017-2021 Claude Heiland-Allen

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU Affero General Public License as
published by the Free Software Foundation, either version 3 of the
License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Affero General Public License for more details.

You should have received a copy of the GNU Affero General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.
*/

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
#include "reference.h"
#include "../common/memory.h"
#include "../common/parallell.h"
#include "../common/StringVector.h"
#include "../common/getimage.h"
#include "../common/timer.h"
#include "../common/bitmap.h"
#include <float.h>
#include <malloc.h>
#include <stdio.h>
#include "complex.h"
#include <cmath>
#include <string>
#include <iostream>
#include <fstream>
#include <set>
#ifdef WINVER
#include "../common/bitmap.h"
#endif
#include "../formula/formula.h"
#include "colour.h"
#include "jpeg.h"
#include "png.h"
#include "tiff.h"
#include "exr.h"
#include "main.h"
#include "gradient.h"
#include "newton.h"
#include "scale_bitmap.h"
#include "dual.h"
#include "opengl.h"

#define _abs(a) ((_abs_val=(a))>0?_abs_val:-_abs_val)
#define _SMOOTH_COLORS_
#define SMOOTH_TOLERANCE 256
#define APPROX_GRID 19
#define TERM4
#define TERM5
//#define TERM6
//#define TERM7

#if 0
double g_Degree = 0;
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

// http://www.burtleburtle.net/bob/hash/integer.html
#if 0
static uint32_t wang_hash(uint32_t a)
{
    a = (a ^ 61) ^ (a >> 16);
    a = a + (a << 3);
    a = a ^ (a >> 4);
    a = a * 0x27d4eb2d;
    a = a ^ (a >> 15);
    return a;
}
#else
static uint32_t burtle_hash(uint32_t a)
{
    a = (a+0x7ed55d16) + (a<<12);
    a = (a^0xc761c23c) ^ (a>>19);
    a = (a+0x165667b1) + (a<<5);
    a = (a+0xd3a2646c) ^ (a<<9);
    a = (a+0xfd7046c5) + (a<<3);
    a = (a^0xb55a4f09) ^ (a>>16);
    return a;
}
#endif

// uniform in [0,1)
static double dither(uint32_t x, uint32_t y, uint32_t c)
{
  return burtle_hash(x + burtle_hash(y + burtle_hash(c))) / (double) (0x100000000LL);
}

CFraktalSFT::CFraktalSFT()
: m_Settings()
, m_HybridFormula()
, m_nPixels(0, 0, nullptr, nullptr)
, m_P()
, m_bStop(false)
#ifndef WINVER
, m_renderThread()
#endif
, m_bIsRendering(false)
#ifdef KF_OPENCL
, m_cldevices()
#endif
, N() // invalid array
#ifndef WINVER
, m_mutex()
#endif
, m_sGLSL(KF_DEFAULT_GLSL)
, m_sGLSLLog("")
#ifdef WINVER
, m_undo()
, m_redo()
#endif
{
#ifdef KF_OPENCL
	clid = -1;
	cl = NULL;
#endif

#ifdef KF_OPENCL
    m_cldevices = initialize_opencl(&cl_error
#ifdef WINVER
	                                , nullptr
#endif
									);
#endif
	m_bTexture=FALSE;
	m_nImgMerge=1;
	m_nImgPower=200;
	m_nImgRatio=100;
	m_szTexture="";
	m_bTextureResize = true;
	m_lpTextureBits = nullptr;
	m_rowBkg = 0;

	m_bSlopes = FALSE;
	m_nSlopePower = 50;
	m_nSlopeRatio = 50;
	m_nSlopeAngle = 45;
	m_nSlopeX = m_nSlopeY = 0;
	SetTransformMatrix(mat2(1.0, 0.0, 0.0, 1.0));
	m_nFractalType = 0;
	m_bmi = nullptr;
#ifdef WINVER
	m_bmBmp = nullptr;
	m_bNoPostWhenDone = FALSE;
#endif
	m_bMW = 0;
	m_bBlend = 0;
	m_bNoGlitchDetection = FALSE;
	m_nPower = 0;
	m_nMaxOldGlitches = OLD_GLITCH;

	int m_nTerms = GetApproxTerms();
	m_APr = new floatexp[m_nTerms];
	m_APi = new floatexp[m_nTerms];
	m_APs = new SeriesR2<double, int64_t>;

#ifdef WINVER
	m_hMutex = CreateMutex(NULL, 0, NULL);
#endif
	m_bStop = false;

	m_CenterRe = 0;
	m_CenterIm = 0;
	m_ZoomRadius = 2;
	m_nZoom = 0;
	m_nTrans = NULL;
	m_nPhase = nullptr;
	m_nDEx = nullptr;
	m_nDEy = nullptr;
	m_bFlat = FALSE;
	m_bTrans = TRUE;
	m_bITrans = FALSE;
	m_bAddReference = FALSE;
	m_nX = m_nY = 0;
	m_bResized = true;
	m_nSizeImage = -1;
	m_pnExpConsts = nullptr;
	m_fPixelSpacing = 0;
	m_nParts = 0;

	m_Reference = nullptr;
	m_ReferenceReuse = nullptr;
	m_rrefReuse = m_irefReuse = 0;
	m_NanoMB1Ref = nullptr;
	m_NanoMB2Ref = nullptr;
	m_nPixels_LSB = nullptr;
	m_nPixels_MSB = nullptr;

	m_nSmoothMethod = SmoothMethod_Log;
	m_nBailoutRadiusPreset = BailoutRadius_High;
	m_nBailoutRadiusCustom = 2;
	m_nBailoutNormPreset = BailoutNorm_2;
	m_nBailoutNormCustom = 2;
	m_nColorMethod = ColorMethod_DistanceLog;
	m_nDifferences = Differences_Analytic;
	m_nPhaseColorStrength = 0;

	m_epsilon = 1.1102230246251565e-16 * (1 << 10);

	m_imageHalf = nullptr;
	m_lpBits = nullptr;
	m_row = 0;
	m_nMaxIter = 200;
	m_nMinI = m_nMaxI = 0;
	m_bIterChanged = false;
	m_bNoApproximation = false;
	m_nMaxApproximation = 0;
	m_nApprox = 0;
	m_pixel_center_x = m_pixel_center_y = 0;
	m_pixel_scale = 0;

	m_nIterDiv = 0.1;
	m_nColorOffset = 0;
	m_nAddRefX = -1;
	m_nAddRefY = -1;
	m_SeedR = 0;
	m_SeedI = 0;
	m_FactorAR = 1;
	m_FactorAI = 0;
	m_real = 1;
	m_imag = 1;
	m_rref = m_iref = 0;
	m_bAutoGlitch = 1;
	m_nSeed = 1;
	m_nMW = 0;
	m_bMW = false;
	m_bBlend = false;
	m_bUseSRGB = false;
	m_bTriangleInequalityAverage = false;

	ResetGlitches();

	m_UseHybridFormula = false;
	{
		// Mandelbrot power 2
		hybrid_line l =
			{ { false, false, false, false, 2, 1.0, 0.0 }
			, { false, false, false, false, 0, 0.0, 0.0 }
			, hybrid_combine_add
			};
	        hybrid_stanza s;
		s.lines.push_back(l);
		s.repeats = 1;
		hybrid_formula h;
		h.stanzas.push_back(s);
		h.loop_start = 0;
		m_HybridFormula = h;
	}

	m_bUseOpenGL = false;
	m_bBadOpenGL = false;
	m_bGLSLChanged = true;
	m_bGLSLCompiled = false;
	m_sGLSL = KF_DEFAULT_GLSL;

#ifdef KF_OPENCL
	m_opengl_major = 0;
	m_opengl_minor = 0;
	m_OpenCL_Glitched = false;
	m_OpenCL_Glitched_X = m_OpenCL_Glitched_Y = 0;
	m_OpenCL_Glitched_Count = 0;
#endif
	m_bAddReference = 0;

#ifdef WINVER
	m_bIsRendering = false;
#endif
	m_bInhibitColouring = FALSE;
	m_bInteractive = true;
	m_nRDone = 0;
	GenerateColors(128, 1);
	ResetTimers();
	OpenString(
"Re: 0\n"
"Im: 0\n"
"Zoom: 1\n"
"Iterations: 200\n"
"IterDiv: 0.010000\n"
"SmoothMethod: 0\n"
"ColorMethod: 7\n"
"Differences: 3\n"
"ColorOffset: 0\n"
"Rotate: 0.000000\n"
"Ratio: 360.000000\n"
"Colors: 255,255,255,128,0,64,160,0,0,192,128,0,64,128,0,0,255,255,64,128,255,0,0,255,\n"
"InteriorColor: 0,0,0,\n"
"Smooth: 1\n"
"MultiColor: 0\n"
"BlendMC: 0\n"
"MultiColors: \n"
"Power: 2\n"
"FractalType: 0\n"
"Slopes: 1\n"
"SlopePower: 50\n"
"SlopeRatio: 20\n"
"SlopeAngle: 45\n"
"imag: 1\n"
"real: 1\n"
"SeedR: 0\n"
"SeedI: 0\n"
"FactorAR: 1\n"
"FactorAI: 0\n"
, FALSE);
	ApplyColors();
}

bool CFraktalSFT::UseOpenGL()
{
	if(!m_bUseOpenGL || m_bBadOpenGL)
		return false;
	if(m_OpenGL)
		return true;

	OpenGL_processor *opengl = new OpenGL_processor();

	response_init_t resp;
	opengl->init(resp);

	SetGLSLLog(resp.message);

	if(!resp.success) {
		delete opengl;
		m_bBadOpenGL = true;
		return false;
	}
	m_opengl_major = resp.major;
	m_opengl_minor = resp.minor;

	m_OpenGL.reset(opengl);
	return true;
}

void CFraktalSFT::SetUseOpenGL(bool gl)
{
	m_bUseOpenGL = gl;
	if(gl)
		return;

	// Disable OpenGL: delete the backend safely
	OpenGL_processor *openGL = m_OpenGL.release();
	if(openGL) {
		if (m_bBadOpenGL)
			m_bBadOpenGL = false;  // allow for retrying
		else
			openGL->deinit();
		delete openGL;
	}
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
	if (n < 1)
		return 1;
	else if (n < 4)
		return n;
	// poor man's integer square root
	int nE = 1<<((33-__builtin_clz(n))/2);

	n |= 1; // must be odd
	int i = 3;
	while (true){
		BOOL bDone = TRUE;
		for (i = 3; i<nE; i += 2)
			if (n%i == 0){
				bDone = FALSE;
				break;
			}
		if (bDone)
			break;
		n += 2;
	}
	return n;
}

void CFraktalSFT::AddWave(int nColor, int nP, int nS)
{
	srand(GetTickCount());
	int nPeriod;
	if (nP == 0)
		return;
	if (nP == -1){
		nPeriod = rand() % (m_nParts<4 ? m_nParts / 2 : m_nParts);
		nPeriod = MakePrime(nPeriod);
	}
	else {
		nPeriod = nP;
	}
	int nStart;
	if (nS == -1)
		nStart = rand() % nPeriod;
	else
		nStart = nS;
	int i;
	for (i = 0; i<m_nParts; i++){
		int val = nP ? 127 + 127 * sin((double)(i + nStart) * 2 * pi*((double)nPeriod / (double)m_nParts)) : 0;
		switch (nColor){
		case 0:
			m_cKeys[i].r = val;
			break;
		case 1:
			m_cKeys[i].g = val;
			break;
		case 2:
			m_cKeys[i].b = val;
			break;
		case 3:
			m_cKeys[i].r = val;
			m_cKeys[i].g = val;
			m_cKeys[i].b = val;
			break;
		case 4:
			m_cKeys[i].r += (int)(m_cKeys[i].r + val) / 2;
			break;
		case 5:
			m_cKeys[i].g += (int)(m_cKeys[i].g + val) / 2;
			break;
		case 6:
			m_cKeys[i].b += (int)(m_cKeys[i].b + val) / 2;
			break;
		case 7:
			m_cKeys[i].r = (int)(m_cKeys[i].r + val) / 2;
			m_cKeys[i].g = (int)(m_cKeys[i].g + val) / 2;
			m_cKeys[i].b = (int)(m_cKeys[i].b + val) / 2;
			break;
		}
	}
}

void CFraktalSFT::ApplyIterationColors()
{
	if (m_nPixels && m_lpBits){
		int64_t nMin, nMax;
		GetIterations(nMin, nMax);
		if (nMin == nMax)
			nMax = nMin + 1;
		int x, y;
		for (x = 0; x<m_nX; x++){
			for (y = 0; y<m_nY; y++){
				int nIndex = x * BM_WIDTH + (m_bmi->biHeight - 1 - y)*m_row;
				m_lpBits[nIndex] = 255 * (m_nPixels[x][y] - nMin) / (nMax - nMin);
				m_lpBits[nIndex + 1] = m_lpBits[nIndex];
				m_lpBits[nIndex + 2] = m_lpBits[nIndex];
			}
		}
	}
}
void CFraktalSFT::ApplyPhaseColors()
{
	if (m_nPhase && m_lpBits){
		for (int x = 0; x<m_nX; x++){
			for (int y = 0; y<m_nY; y++){
				int nIndex = x * BM_WIDTH + (m_bmi->biHeight - 1 - y)*m_row;
				m_lpBits[nIndex] = 256 * m_nPhase[x][y];
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
				int nIndex = x * BM_WIDTH + (m_bmi->biHeight - 1 - y)*m_row;
				m_lpBits[nIndex] = 255 * tr;
				m_lpBits[nIndex + 1] = 255 * tr;
				m_lpBits[nIndex + 2] = 255 * tr;
			}
		}
	}
}

#ifdef WINVER
HBITMAP CFraktalSFT::ShrinkBitmap(HBITMAP bmSrc,int nNewWidth,int nNewHeight,int mode)
{
	HDC hDC = GetDC(NULL);
	HBITMAP bmDst = create_bitmap(hDC,nNewWidth,nNewHeight);
  if (mode >= 2) // best
	{
		BITMAP s, d;
		GetObject(bmSrc, sizeof(BITMAP), &s);
		GetObject(bmDst, sizeof(BITMAP), &d);
		/*bool ok = */ scale_bitmap_rgb8((unsigned char *) d.bmBits, d.bmWidth, d.bmHeight, (const unsigned char *)s.bmBits, s.bmWidth, s.bmHeight, mode >= 3);
		//assert(ok && "scale_bitmap_rgb8");
	}
	else
	{
		BITMAP bm;
		GetObject(bmSrc,sizeof(BITMAP),&bm);
		HDC dcSrc = CreateCompatibleDC(hDC);
		HBITMAP bmOldSrc = (HBITMAP)SelectObject(dcSrc,bmSrc);
		HDC dcDst = CreateCompatibleDC(hDC);
		HBITMAP bmOldDst = (HBITMAP)SelectObject(dcDst,bmDst);
		if(mode == 1)
			SetStretchBltMode(dcDst,HALFTONE);
		else // mode == 0
			SetStretchBltMode(dcDst,COLORONCOLOR);
		StretchBlt(dcDst,0,0,nNewWidth,nNewHeight,dcSrc,0,0,bm.bmWidth,bm.bmHeight,SRCCOPY);
		SelectObject(dcDst,bmOldDst);
		SelectObject(dcSrc,bmOldSrc);
		DeleteDC(dcDst);
		DeleteDC(dcSrc);
	}
	ReleaseDC(NULL,hDC);
	return bmDst;
}
#endif // !WINVER

bool operator==(const TextureParams &a, const TextureParams &b)
{
  return
    a.m_bTexture == b.m_bTexture &&
    a.m_szTexture == b.m_szTexture &&
    a.m_nImgPower == b.m_nImgPower &&
    a.m_nX == b.m_nX &&
    a.m_nY == b.m_nY &&
    a.m_bTextureResize == b.m_bTextureResize;
}

#ifdef WINVER
void CFraktalSFT::LoadTexture()
{
	TextureParams currentTextureParams =
	  { m_bTexture, m_szTexture, m_nImgPower, m_nX, m_nY, m_bTextureResize };
	if (currentTextureParams == m_ActiveTextureParams)
	{
		return;
	}
	m_ActiveTextureParams = currentTextureParams;
	if (m_lpTextureBits)
	{
		delete[] m_lpTextureBits;
		m_lpTextureBits = nullptr;
	}
	if (! m_bTexture)
	{
		return;
	}
	int nImgOffs=m_nImgPower/64;
	HBITMAP bmBitmapIn = GetImage(m_szTexture);
	HBITMAP bmBitmap = nullptr;
	if (GetTextureResize())
	{
		SIZE scImg;
		scImg.cx = m_nX+(nImgOffs+m_nImgPower)/64;
		scImg.cy = m_nY+(m_nImgPower+nImgOffs)/64;
		bmBitmap = ShrinkBitmap(bmBitmapIn,scImg.cx,scImg.cy);
	}
	else
	{
		bmBitmap = bmBitmapIn;
		bmBitmapIn = nullptr;
	}
	HDC hDC = GetDC(NULL);
	memset(&m_bmiBkg,0,sizeof(BITMAPINFOHEADER));
	m_bmiBkg.biSize=sizeof(BITMAPINFOHEADER);
	if(!GetDIBits(hDC,bmBitmap,0,0,NULL,(LPBITMAPINFO)&m_bmiBkg,DIB_RGB_COLORS))
		Beep(1000,10);
	m_bmiBkg.biCompression=m_bmiBkg.biClrUsed=m_bmiBkg.biClrImportant=0;
	m_bmiBkg.biBitCount = 8*BM_WIDTH;
	m_rowBkg = ((((m_bmiBkg.biWidth*(DWORD)m_bmiBkg.biBitCount)+31)&~31) >> 3);
	m_bmiBkg.biSizeImage=m_rowBkg*m_bmiBkg.biHeight;
	m_lpTextureBits = new BYTE[m_bmiBkg.biSizeImage];
	if(!GetDIBits(hDC,bmBitmap,0,m_bmiBkg.biHeight,m_lpTextureBits,
			(LPBITMAPINFO)&m_bmiBkg,DIB_RGB_COLORS))
		Beep(1000,10);
	DeleteObject(bmBitmap);
	if (bmBitmapIn) DeleteObject(bmBitmapIn);
	ReleaseDC(NULL,hDC);
}
#endif // !WINVER

void CFraktalSFT::SetTexture(int x, int y, srgb &s)
{
	if (! m_lpTextureBits)
	{
		// can't load texture here (not thread safe)
		return;
	}
	int nImgOffs=m_nImgPower/64;
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

	double nImgPower = m_nImgPower;
	diff = pow(diff,nImgPower);
	diffx = pow(diffx,nImgPower);
	diffy = pow(diffy,nImgPower);
	int nY=y;
	int nX=x;
	static double pi4 = pi/4;
	if(diff>1){
		diff = (atan(diff)-pi4)/pi4;
		diff=diff*(double)m_nImgRatio/100;;
	}
	else{
		diff=1/diff;
		diff = (atan(diff)-pi4)/pi4;
		diff=diff*(double)m_nImgRatio/100;;
	}
	if(diffy>1){
		diffy = (atan(diffy)-pi4)/pi4;
		diffy=diffy*(double)m_nImgRatio/100;;
		nY = y+nImgOffs + nImgPower*diffy;
	}
	else{
		diffy=1/diffy;
		diffy = (atan(diffy)-pi4)/pi4;
		diffy=diffy*(double)m_nImgRatio/100;;
		nY = y+nImgOffs - nImgPower*diffy;
	}
	if(diffx>1){
		diffx = (atan(diffx)-pi4)/pi4;
		diffx=diffx*(double)m_nImgRatio/100;;
		nX = x+nImgOffs + nImgPower*diffx;
	}
	else{
		diffx=1/diffx;
		diffx = (atan(diffx)-pi4)/pi4;
		diffx=diffx*(double)m_nImgRatio/100;;
		nX = x+nImgOffs - nImgPower*diffx;
	}

	if(nY<0)
		nY=0;
	else if(nY>m_bmiBkg.biHeight-1)
		nY=m_bmiBkg.biHeight-1;
	if(nX<0)
		nX=0;
	else if(nX>m_bmiBkg.biWidth-1)
		nX=m_bmiBkg.biWidth-1;
	int nIndexBkg = nX*BM_WIDTH + (m_bmiBkg.biHeight-1-nY)*m_rowBkg;
	s.r = s.r * (1 - m_nImgMerge) + m_nImgMerge * m_lpTextureBits[nIndexBkg+0] / 255.0;
	s.g = s.g * (1 - m_nImgMerge) + m_nImgMerge * m_lpTextureBits[nIndexBkg+1] / 255.0;
	s.b = s.b * (1 - m_nImgMerge) + m_nImgMerge * m_lpTextureBits[nIndexBkg+2] / 255.0;
}

static inline double hypot2(double x, double y) { return x * x + y * y; }
static inline double hypot1(double x, double y) { return sqrt(x * x + y * y); }

void CFraktalSFT::SetColor(int x, int y, int w, int h)
{
	if (m_bInhibitColouring || UseOpenGL()) return;

	double offs = m_nTrans[x][y];
	if (!GetShowGlitches() && GET_TRANS_GLITCH(offs))
		return;

	int nIndex = x * BM_WIDTH + (m_bmi->biHeight - 1 - y)*m_row;
	int64_t nIter0 = m_nPixels[x][y];
	srgb s;
	if (nIter0 == m_nMaxIter)
	{
		// this is reset later to the precise RGB8 (unless image texture)
		// set it here anyway for to make image texture blending better
		s.r = m_cInterior.r / 255.0;
		s.g = m_cInterior.g / 255.0;
		s.b = m_cInterior.b / 255.0;
	}
	else{
		ColorMethod method = m_nColorMethod;
		Differences diffs = m_nDifferences;
		int64_t nIter = nIter0;

		double iter = (double)nIter;
		if (! m_bFlat)
		{
			iter += (double)1 - offs;
		}
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
		if (method == ColorMethod_SquareRoot){
			iter = sqrt(fmax(0, iter));
		}
		else if (method == ColorMethod_CubicRoot){
			iter = pow(fmax(0, iter), (double)1 / (double)3);
		}
		else if (method == ColorMethod_Logarithm){
			iter = log(fmax(1, iter));
		}
		else if (method == ColorMethod_LogLog){
			iter = log(1 + log(1 + iter));
		}
		else if (method == ColorMethod_ATan){
			iter = atan(iter);
		}
		else if (method == ColorMethod_FourthRoot){
			iter = sqrt(sqrt(fmax(0, iter)));
		}
		else if (method == ColorMethod_Stretched){
			int64_t nMin, nMax;
			GetIterations(nMin, nMax,NULL,NULL,TRUE);
			iter = (double)1024 * ((double)iter - (double)nMin) / ((double)nMax - (double)nMin);
		}
		else if (method == ColorMethod_DistanceLinear ||
		         method == ColorMethod_DEPlusStandard ||
		         method == ColorMethod_DistanceLog ||
		         method == ColorMethod_DistanceSqrt){
			iter=0;
			if (diffs == Differences_Analytic)
			{
				iter = 0;
				if (m_nDEx && m_nDEy)
				{
					float dex = m_nDEx[x][y];
					float dey = m_nDEy[x][y];
					iter = 1 / sqrt(dex * dex + dey * dey);
				}
			}
			else
			{
				// load 3x3 stencil around the pixel
				int X = m_nX - 1;
				int Y = m_nY - 1;
				static const double ninf = 1.0 / 0.0;
				double p[3][3] = { { ninf, ninf, ninf }, { ninf, ninf, ninf }, { ninf, ninf, ninf } };
				double px[3][3] = { { 0, 0, 0 }, { 0, 0, 0 }, { 0, 0, 0 } };
				double py[3][3] = { { 0, 0, 0 }, { 0, 0, 0 }, { 0, 0, 0 } };
				if (w <= x && h <= y && m_nPixels[x - w][y - h] != PIXEL_UNEVALUATED){p[0][0] = m_nPixels[x - w][y - h] + 1.0 - m_nTrans[x - w][y - h];GetPixelOffset(x - w, y - h, px[0][0], py[0][0]); px[0][0] -= w; py[0][0] -= h;}
				if (w <= x           && m_nPixels[x - w][y    ] != PIXEL_UNEVALUATED){p[0][1] = m_nPixels[x - w][y    ] + 1.0 - m_nTrans[x - w][y    ];GetPixelOffset(x - w, y    , px[0][1], py[0][1]); px[0][1] -= w;               }
				if (w <= x && y+h<=Y && m_nPixels[x - w][y + h] != PIXEL_UNEVALUATED){p[0][2] = m_nPixels[x - w][y + h] + 1.0 - m_nTrans[x - w][y + h];GetPixelOffset(x - w, y + h, px[0][2], py[0][2]); px[0][2] -= w; py[0][2] += h;}
				if (          h <= y && m_nPixels[x    ][y - h] != PIXEL_UNEVALUATED){p[1][0] = m_nPixels[x    ][y - h] + 1.0 - m_nTrans[x    ][y - h];GetPixelOffset(x    , y - h, px[1][0], py[1][0]);                py[1][0] -= h;}
				if (                    m_nPixels[x    ][y    ] != PIXEL_UNEVALUATED){p[1][1] = m_nPixels[x    ][y    ] + 1.0 - m_nTrans[x    ][y    ];GetPixelOffset(x    , y    , px[1][1], py[1][1]);                              }
				if (          y+h<=Y && m_nPixels[x    ][y + h] != PIXEL_UNEVALUATED){p[1][2] = m_nPixels[x    ][y + h] + 1.0 - m_nTrans[x    ][y + h];GetPixelOffset(x    , y + h, px[1][2], py[1][2]);                py[1][2] += h;}
				if (x+w<=X && h <= y && m_nPixels[x + w][y - h] != PIXEL_UNEVALUATED){p[2][0] = m_nPixels[x + w][y - h] + 1.0 - m_nTrans[x + w][y - h];GetPixelOffset(x + w, y - h, px[2][0], py[2][0]); px[2][0] += w; py[2][0] -= h;}
				if (x+w<=X           && m_nPixels[x + w][y    ] != PIXEL_UNEVALUATED){p[2][1] = m_nPixels[x + w][y    ] + 1.0 - m_nTrans[x + w][y    ];GetPixelOffset(x + w, y    , px[2][1], py[2][1]); px[2][1] += w;               }
				if (x+w<=X && y+h<=Y && m_nPixels[x + w][y + h] != PIXEL_UNEVALUATED){p[2][2] = m_nPixels[x + w][y + h] + 1.0 - m_nTrans[x + w][y + h];GetPixelOffset(x + w, y + h, px[2][2], py[2][2]); px[2][2] += w; py[2][2] += h;}
				// reflect at boundaries if necessary
				// this will break (result is infinite or NaN) for image size of 1 pixel
				p[1][1] *= 2.0;
				px[1][1] *= 2.0;
				py[1][1] *= 2.0;
				if (ninf == p[0][0]) {p[0][0] = p[1][1] - p[2][2];px[0][0] = px[1][1] - px[2][2];py[0][0] = py[1][1] - py[2][2];}
				if (ninf == p[0][1]) {p[0][1] = p[1][1] - p[2][1];px[0][1] = px[1][1] - px[2][1];py[0][1] = py[1][1] - py[2][1];}
				if (ninf == p[0][2]) {p[0][2] = p[1][1] - p[2][0];px[0][2] = px[1][1] - px[2][0];py[0][2] = py[1][1] - py[2][0];}
				if (ninf == p[1][0]) {p[1][0] = p[1][1] - p[1][2];px[1][0] = px[1][1] - px[1][2];py[1][0] = py[1][1] - py[1][2];}
				if (ninf == p[1][2]) {p[1][2] = p[1][1] - p[1][0];px[1][2] = px[1][1] - px[1][0];py[1][2] = py[1][1] - py[1][0];}
				if (ninf == p[2][0]) {p[2][0] = p[1][1] - p[0][2];px[2][0] = px[1][1] - px[0][2];py[2][0] = py[1][1] - py[0][2];}
				if (ninf == p[2][1]) {p[2][1] = p[1][1] - p[0][1];px[2][1] = px[1][1] - px[0][1];py[2][1] = py[1][1] - py[0][1];}
				if (ninf == p[2][2]) {p[2][2] = p[1][1] - p[0][0];px[2][2] = px[1][1] - px[0][0];py[2][2] = py[1][1] - py[0][0];}
				p[1][1] *= 0.5;
				px[1][1] *= 0.5;
				py[1][1] *= 0.5;
				// do the differencing
				switch (diffs)
				{
				case Differences_Analytic:
					//assert(!"analytic case reached, should be unreachable");
					break;
				case Differences_Laplacian3x3:
					{
						// gerrit: Laplacian is proportional to g^2: https://fractalforums.org/kalles-fraktaler/15/gaussian-jitter-for-moire-reduction/891/msg5563#msg5563
						double L = 0;
						L +=  1 * p[0][0];
						L +=  4 * p[0][1];
						L +=  1 * p[0][2];
						L +=  4 * p[1][0];
						L -= 20 * p[1][1];
						L +=  4 * p[1][2];
						L +=  1 * p[2][0];
						L +=  4 * p[2][1];
						L +=  1 * p[2][2];
						L /=  6;
	#define INV_LOG_2 1.4426950408889634
						double g = sqrt(fabs(L * INV_LOG_2));
	#undef INV_LOG_2
						iter = g * 2.8284271247461903;
					}
					break;
				case Differences_LeastSquares3x3:
					{
						double gx = 0;
						double gy = 0;
						compute_gradient_3x3(p, px, py, gx, gy);
						double g = hypot1(gx, gy);
						iter = g * 2.8284271247461903;
					}
					break;
				case Differences_LeastSquares2x2:
					{
						double gx = 0;
						double gy = 0;
						compute_gradient_2x2(p, px, py, gx, gy);
						double g = hypot1(gx, gy);
						iter = g * 2.8284271247461903;
					}
					break;
				case Differences_Central3x3:
					{
						// gerrit's central difference formula
						double gx = sqr(p[2][1] - p[0][1]) / hypot2(px[2][1] - px[0][1], py[2][1] - py[0][1]);
						double gy = sqr(p[1][2] - p[1][0]) / hypot2(px[1][2] - px[1][0], py[1][2] - py[1][0]);
						double g1 = sqr(p[2][2] - p[0][0]) / hypot2(px[2][2] - px[0][0], py[2][2] - py[0][0]);
						double g2 = sqr(p[0][2] - p[2][0]) / hypot2(px[0][2] - px[2][0], py[0][2] - py[2][0]);
						double g = sqrt(0.5 * (gx + gy + g1 + g2));
						iter = g * 2.8284271247461903;
					}
					break;
				case Differences_Forward3x3:
					{
						// forward differencing in 8 directions from the target point
						double gx0 = sqr(p[0][1] - p[1][1]) / hypot2(px[0][1] - px[1][1], py[0][1] - py[1][1]);
						double gx2 = sqr(p[2][1] - p[1][1]) / hypot2(px[2][1] - px[1][1], py[2][1] - py[1][1]);
						double gy0 = sqr(p[1][0] - p[1][1]) / hypot2(px[1][0] - px[1][1], py[1][0] - py[1][1]);
						double gy2 = sqr(p[1][2] - p[1][1]) / hypot2(px[1][2] - px[1][1], py[1][2] - py[1][1]);
						double gu0 = sqr(p[0][0] - p[1][1]) / hypot2(px[0][0] - px[1][1], py[0][0] - py[1][1]);
						double gu2 = sqr(p[2][2] - p[1][1]) / hypot2(px[2][2] - px[1][1], py[2][2] - py[1][1]);
						double gv0 = sqr(p[2][0] - p[1][1]) / hypot2(px[2][0] - px[1][1], py[2][0] - py[1][1]);
						double gv2 = sqr(p[0][2] - p[1][1]) / hypot2(px[0][2] - px[1][1], py[0][2] - py[1][1]);
						double g = sqrt(0.25 * (gx0 + gx2 + gy0 + gy2 + gu0 + gu2 + gv0 + gv2));
						iter = g * 2.8284271247461903;
					}
					break;
				case Differences_Diagonal2x2: // aka Roberts Cross
					{
						// forward differencing in 2 diagonals of a 2x2 substencil
						if (GetJitterSeed() == 0)
						{
							double gu = sqr(p[0][0] - p[1][1]) / hypot2(px[0][0] - px[1][1], py[0][0] - py[1][1]);
							double gv = sqr(p[0][1] - p[1][0]) / hypot2(px[0][1] - px[1][0], py[0][1] - py[1][0]);
							double g = sqrt(gu + gv);
							iter = g * 2.8284271247461903;
						}
						else
						{
							// with displacement correction by gerrit
							double nux = px[0][0] - px[1][1];
							double nuy = py[0][0] - py[1][1];
							double nvx = px[1][0] - px[0][1];
							double nvy = py[1][0] - py[0][1];
							double nu = hypot1(nux, nuy);
							double nv = hypot1(nvx, nvy);
							nux /= nu;
							nuy /= nu;
							nvx /= nv;
							nvy /= nv;
							double u = (p[0][0] - p[1][1]) / nu;
							double v = (p[1][0] - p[0][1]) / nv;
							double dotnunv = nux * nvx + nuy * nvy;
							double crossnunv = nux * nvy - nuy * nvx;
							double g = sqrt((u * u + v * v - 2 * u * v * dotnunv) / sqr(crossnunv));
							iter = g * 2.8284271247461903;
						}
					}
					break;
				case Differences_Traditional:
					{
						// traditional method reverse engineered from original code
						double gx = (p[0][1] - p[1][1]) * 1.414 / hypot(px[0][1] - px[1][1], py[0][1] - py[1][1]);
						double gy = (p[1][0] - p[1][1]) * 1.414 / hypot(px[1][0] - px[1][1], py[1][0] - py[1][1]);
						double gu = (p[0][0] - p[1][1]) * 1.414 / hypot(px[0][0] - px[1][1], py[0][0] - py[1][1]);
						double gv = (p[0][2] - p[1][1]) * 1.414 / hypot(px[0][2] - px[1][1], py[0][2] - py[1][1]);
						double g = fabs(gx) + fabs(gy) + fabs(gu) + fabs(gv);
						iter = g;
					}
					break;
				}
			}
			// post differencing transfer functions
//			iter/=4;
//			iter*=iter;
			iter*=(double)m_nX / (double)640;
			if (method == ColorMethod_DistanceSqrt || method == ColorMethod_DEPlusStandard)
				iter=sqrt(fmax(0, iter));
			else if (method == ColorMethod_DistanceLog)
				iter=log(fmax(1, iter+1));
			/*iter=log(iter);
			if(iter<0)
				iter=0;*/
			if(iter>1024)
				iter=1024;
			if(method == ColorMethod_DEPlusStandard && iter>m_nIterDiv)
				iter = (double)nIter + (double)1 - offs;
		}
		if (m_nIterDiv != 1){
			iter /= m_nIterDiv;
		}
		if (m_nColorOffset)
			iter += m_nColorOffset;// = (nIter+m_nColorOffset)%1024;
		nIter = (int64_t)floor(iter);
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
					g = sin((pi*nIter) / nPeriod) / 2 + .5;
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
				if (m_nPhaseColorStrength && m_nPhase)
					iter += m_nPhaseColorStrength / 100 * 1024 * m_nPhase[x][y];
				nIter = (int64_t)floor(iter);
				offs = 1 - (iter - (double)nIter);
				if (m_bITrans)
					offs = 1 - offs;
				double nR, nG, nB;
				int col = nIter % 1024;
				if (m_bTrans && offs){
					double g1 = (1 - offs);
					int ncol = (col + 1) % 1024;
					nR = m_cPos[col].r*offs + m_cPos[ncol].r*g1;
					nG = m_cPos[col].g*offs + m_cPos[ncol].g*g1;
					nB = m_cPos[col].b*offs + m_cPos[ncol].b*g1;
				}
				else{
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
			if (m_nPhaseColorStrength && m_nPhase)
				iter += m_nPhaseColorStrength / 100 * 1024 * m_nPhase[x][y];
			nIter = (int64_t)floor(iter);
			offs = 1 - (iter - (double)nIter);
			if (m_bITrans)
				offs = 1 - offs;
			int col = nIter % 1024;
			if (m_bTrans && offs){
				double g1 = (1 - offs);
				int ncol = (col + 1) % 1024;
				s.r = (m_cPos[col].r*offs + m_cPos[ncol].r*g1) / 255.0f;
				s.g = (m_cPos[col].g*offs + m_cPos[ncol].g*g1) / 255.0f;
				s.b = (m_cPos[col].b*offs + m_cPos[ncol].b*g1) / 255.0f;
			}
			else{
				s.r = m_cPos[col].r / 255.0f;
				s.g = m_cPos[col].g / 255.0f;
				s.b = m_cPos[col].b / 255.0f;
			}
		}
	}
	if(m_bTexture)
		SetTexture(x,y,s);
	if (m_bSlopes){
		double diffx, diffy;
		if (m_nDifferences == Differences_Analytic)
		{
			complex<float> de(m_nDEx[x][y], m_nDEy[x][y]);
			complex<float> diff = complex<float>(1) / de;
			diffx = diff.m_r;
			diffy = diff.m_i;
		}
		else
		{
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
			if (w <= x && m_nPixels[x - w][y] != PIXEL_UNEVALUATED){
				p1 = (double)m_nPixels[x - w][y] + (double)1 - m_nTrans[x - w][y];
				p2 = (double)m_nPixels[x][y] + (double)1 - m_nTrans[x][y];
			}
			else if (x+w<m_nX && m_nPixels[x + w][y] != PIXEL_UNEVALUATED){
				p1 = (double)m_nPixels[x][y] + (double)1 - m_nTrans[x][y];
				p2 = (double)m_nPixels[x + w][y] + (double)1 - m_nTrans[x + w][y];
			}
			else
				p1 = p2 = (double)m_nPixels[x][y] + (double)1 - m_nTrans[x][y];
			diffx = (p1 - p2) / w;
			if (h <= y && m_nPixels[x][y - h] != PIXEL_UNEVALUATED){
				p1 = (double)m_nPixels[x][y - h] + (double)1 - m_nTrans[x][y - h];
				p2 = (double)m_nPixels[x][y] + (double)1 - m_nTrans[x][y];
			}
			else if (y+h<m_nY && m_nPixels[x][y + h] != PIXEL_UNEVALUATED){
				p1 = (double)m_nPixels[x][y] + (double)1 - m_nTrans[x][y];
				p2 = (double)m_nPixels[x][y + h] + (double)1 - m_nTrans[x][y + h];
			}
			else
				p1 = p2 = (double)m_nPixels[x][y] + (double)1 - m_nTrans[x][y];
			diffy = (p1 - p2) / h;
		}

		double diff = diffx*m_nSlopeX + diffy*m_nSlopeY;
		diff *= m_nSlopePower * double(m_nX) / 640;
		if (diff >= 0){
			diff = atan(diff) / (pi / 2);
			diff = diff*(double)m_nSlopeRatio / 100;;
			s.r = (1 - diff)*s.r;
			s.g = (1 - diff)*s.g;
			s.b = (1 - diff)*s.b;
		}
		else{
			diff = -diff;
			diff = atan(diff) / (pi / 2);
			diff = diff*(double)m_nSlopeRatio / 100;;
			s.r = (1 - diff)*s.r + diff;
			s.g = (1 - diff)*s.g + diff;
			s.b = (1 - diff)*s.b + diff;
		}

	}
	lrgb l;
	if (m_imageHalf)
		l = srgb2lrgb(s);
	srgb8 s8 = dither(s, x, y);
	if (nIter0 == m_nMaxIter && ! m_bTexture)
	{
		s8.r = m_cInterior.r;
		s8.g = m_cInterior.g;
		s8.b = m_cInterior.b;
		if (m_imageHalf)
		{
			s.r = s8.r / 255.0f;
			s.g = s8.g / 255.0f;
			s.b = s8.b / 255.0f;
			l = srgb2lrgb(s);
		}
	}
	m_lpBits[nIndex    ] = s8.r;
	m_lpBits[nIndex + 1] = s8.g;
	m_lpBits[nIndex + 2] = s8.b;
	if (m_imageHalf)
	{
		// flip vertically and convert BGR to RGB
		size_t i3 = (nIndex % m_row);
		size_t j = nIndex / m_row;
		size_t nIndex2 = (m_nY - 1 - j) * m_row + i3;
		float c; // clamp to finite non-negative
		c = l.b; if (! (c > 0)) c = 0; if (! (c < 65504)) c = 65504; m_imageHalf[nIndex2    ] = c;
		c = l.g; if (! (c > 0)) c = 0; if (! (c < 65504)) c = 65504; m_imageHalf[nIndex2 + 1] = c;
		c = l.r; if (! (c > 0)) c = 0; if (! (c < 65504)) c = 65504; m_imageHalf[nIndex2 + 2] = c;
	}
}

// based on cache-oblivious matrix transpose by divide and conquer
void CFraktalSFT::ApplyColors(int x0, int x1, int y0, int y1)
{
	int w = x1 - x0;
	int h = y1 - y0;
	//assert(w > 0);
	//assert(h > 0);
	if (w <= 16 && h <= 16)
	{
		for (int x = x0; x < x1; ++x)
		for (int y = y0; y < y1; ++y)
		{
			SetColor(x, y, 1, 1);
		}
	}
	else if (w <= h)
	{
		int y = (y0 + y1) >> 1;
		ApplyColors(x0, x1, y0, y);
		ApplyColors(x0, x1, y, y1);
	}
	else
	{
		int x = (x0 + x1) >> 1;
		ApplyColors(x0, x, y0, y1);
		ApplyColors(x, x1, y0, y1);
	}
}

static int ThApplyColors(TH_PARAMS *pMan)
{
	pMan->p->ApplyColors(pMan->nXStart, pMan->nXStop, 0, pMan->p->GetImageHeight());
	return 0;
}

void CFraktalSFT::ApplyColors()
{
#ifdef WINVER
	LoadTexture();
#endif
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
	if (m_nPixels && m_lpBits && ! m_bInhibitColouring){
#ifdef KF_OPENGL
		bool opengl_rendered = false;
		if (UseOpenGL())
		{
			if (m_bGLSLChanged || ! m_bGLSLCompiled)
			{
				request_compile_t req;
				response_compile_t resp;

				req.fragment_src = GetGLSL();

				m_OpenGL->compile(req, resp);

				std::string nl = "\n";
				SetGLSLLog
					( (resp.success ? "compiled" : "NOT COMPILED") + nl
					+ resp.vertex_log + nl
					+ resp.fragment_log + nl
					+ resp.link_log + nl
					);
				m_bGLSLCompiled = resp.success;
				m_bGLSLChanged = false;
			}
			if (m_bGLSLCompiled)
			{
				request_configure_t req;

				req.iterations = GetIterations();
				GetIterations(req.iterations_min, req.iterations_max, nullptr, nullptr, true);
				req.jitter_seed = GetJitterSeed();
				req.jitter_shape = GetJitterShape();
				req.jitter_scale = GetJitterScale();
				req.show_glitches = GetShowGlitches();
				req.iter_div = GetIterDiv();
				req.color_offset = GetColorOffset();
				req.color_method = GetColorMethod();
				req.differences = GetDifferences();
				req.color_phase_strength = GetPhaseColorStrength();
				req.colors.clear();
				for (int i = 0; i < m_nParts; ++i)
				{
					req.colors.push_back(m_cKeys[i].r);
					req.colors.push_back(m_cKeys[i].g);
					req.colors.push_back(m_cKeys[i].b);
				}
				COLOR14 interior = GetInteriorColor();
				req.interior_color[0] = interior.r;
				req.interior_color[1] = interior.g;
				req.interior_color[2] = interior.b;
				req.smooth = m_bTrans;
				req.flat = GetFlat();
				req.multiwaves_enabled = m_bMW;
				req.multiwaves_blend = m_bBlend;
				req.multiwaves.clear();
				for (int i = 0; i < m_nMW; ++i)
				{
					req.multiwaves.push_back(m_MW[i].nPeriod);
					req.multiwaves.push_back(m_MW[i].nStart);
					req.multiwaves.push_back(m_MW[i].nType);
				}
				req.inverse_transition = GetITransition();
				{
					int p = 0, r = 0, a = 0;
					req.slopes = GetSlopes(p, r, a);
					req.slope_power = p;
					req.slope_ratio = r;
					req.slope_angle = a;
				}
				{
					double m = 0, p = 0;
					int r = 0;
					std::string f;
					req.texture_enabled = GetTexture(m, p, r, f);
					req.texture_merge = m;
					req.texture_power = p;
					req.texture_ratio = r;
					req.texture_width = m_bmiBkg.biWidth;
					req.texture_height = m_bmiBkg.biHeight;
					req.texture = m_lpTextureBits; // FIXME row alignment?
				}
				req.use_srgb = GetUseSRGB();
				{
					CFixedFloat zoom = CFixedFloat(2) / m_ZoomRadius;
					floatexp zoomFE = floatexp(zoom);
					req.zoom_log2 = double(log2(zoomFE));
				}
				m_OpenGL->configure(req);
			}
			if (m_bGLSLCompiled)
			{
				request_render_t req;

				req.width = m_nX;
				req.height = m_nY;
				req.n_msb = m_nPixels_MSB;
				req.n_lsb = m_nPixels_LSB;
				req.n_f = m_nTrans ? &m_nTrans[0][0] : nullptr;
				req.t = m_nPhase ? &m_nPhase[0][0] : nullptr;
				req.dex = m_nDEx ? &m_nDEx[0][0] : nullptr;
				req.dey = m_nDEy ? &m_nDEy[0][0] : nullptr;
				req.rgb16 = m_imageHalf;
				req.rgb8 = m_lpBits; // FIXME row alignment?

				m_OpenGL->render(req);
				opengl_rendered = true;
			}
		}
		if (opengl_rendered)
		{
		}
		else
#endif // KF_OPENGL
		{
			SYSTEM_INFO sysinfo;
			GetSystemInfo(&sysinfo);
			int nParallel = GetThreadsPerCore() * sysinfo.dwNumberOfProcessors - GetThreadsReserveCore();
			if (nParallel < 1) nParallel = 1;
			CParallell P(
#ifdef _DEBUG
					1
#else
					nParallel
#endif
				);
			TH_PARAMS *pMan = new TH_PARAMS[nParallel];
			int nXStart = 0;
			int nXStep = (m_nX + nParallel - 1) / nParallel;
			for (i = 0; i < nParallel; i++)
			{
				pMan[i].p = this;
				pMan[i].nXStart = nXStart;
				nXStart += nXStep;
				if (nXStart > m_nX) nXStart = m_nX;
				pMan[i].nXStop = nXStart;
				P.AddFunction((LPEXECUTE)ThApplyColors, &pMan[i]);
			}
			P.Execute();
			P.Reset();
			delete[] pMan;
		}
	}
}
CFraktalSFT::~CFraktalSFT()
{
	DeleteArrays();

	delete[] m_APr;
	delete[] m_APi;
	delete m_APs;
}

std::string CFraktalSFT::ToZoom()
{
	return ToZoom(CDecNumber(2) / CDecNumber(m_ZoomRadius.m_f));
}
std::string CFraktalSFT::ToZoom(const CDecNumber &z)
{
	// Returns a human-readable version of the zoom factor
	static char szRet[40];
	std::string sszZoom = z.ToText();
	const char *szZoom = sszZoom.c_str();

	int len;
	for (len = 0; szZoom[len] && szZoom[len] != '.'; len++);
	len--;
	if (len <= 0){
		strncpy(szRet, szZoom, 3);
		szRet[3] = 0;
		return szRet;
	}
	szRet[0] = szZoom[0];
	if (szZoom[1]){
		szRet[1] = '.';
		szRet[2] = szZoom[1];
		if (szZoom[2] && szZoom[2] != '.'){
			szRet[3] = szZoom[2];
			wsprintf(szRet + 4, "e%03d", len);
		}
		else
			wsprintf(szRet + 3, "e%03d", len);
	}
	else
		szRet[1] = 0;
	char szTmp[40];
	strcpy(szTmp, szRet);
	if (strlen(szTmp)>4)
		szTmp[4] = 0;
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
	if (!GetMirror())
		return;
	int ty = m_nY - y - 1;
	if (ty<y)
		return;
	int tx = m_nX - x - 1;

	m_nPixels[tx][ty] = m_nPixels[x][y];
	m_nTrans[tx][ty] = m_nTrans[x][y];
	if (m_nPhase)
		m_nPhase[tx][ty] = m_nPhase[x][y];
	if (m_nDEx)
		m_nDEx[tx][ty] = -m_nDEx[x][y];
	if (m_nDEy)
		m_nDEy[tx][ty] = -m_nDEy[x][y];
	SetColor(tx, ty, 1, 1);
}

#define GET_EXP(val) ((*((__int64*)&val) & 0x7FF0000000000000)>>52)

//#define HARD_GUESS_EXP
//60 2.5

void CFraktalSFT::SetupArrays()
{
	if (m_nX == 0 || m_nY == 0)
		return;
	
	bool two = GetIterations() >= UINT32_MAX;
	m_nPixels_LSB = new_aligned<uint32_t>(m_nX * m_nY);
	m_nPixels_MSB = two ? new_aligned<uint32_t>(m_nX * m_nY) : nullptr;
	m_nTrans = new float*[m_nX];
	m_nPhase = new float*[m_nX];
	m_nDEx = new float*[m_nX];
	m_nDEy = new float*[m_nX];
	m_nTrans[0] = new_aligned<float>(m_nX * m_nY);
	m_nPhase[0] = new_aligned<float>(m_nX * m_nY);
	m_nDEx[0] = new_aligned<float>(m_nX * m_nY);
	m_nDEy[0] = new_aligned<float>(m_nX * m_nY);
	for (int x = 1; x<m_nX; x++){
		m_nTrans[x] = m_nTrans[0] + x * m_nY;
		m_nPhase[x] = m_nPhase[0] + x * m_nY;
		m_nDEx[x] = m_nDEx[0] + x * m_nY;
		m_nDEy[x] = m_nDEy[0] + x * m_nY;
	}
	m_nPixels = itercount_array(m_nY, 1, m_nPixels_LSB, m_nPixels_MSB);

	AllocateBitmap();
}

void CFraktalSFT::DeleteArrays()
{
		if (m_nPixels_LSB)
		{
			delete_aligned(m_nPixels_LSB);
			m_nPixels_LSB = nullptr;
		}
		if (m_nPixels_MSB)
		{
			delete_aligned(m_nPixels_MSB);
			m_nPixels_MSB = nullptr;
		}
		m_nPixels = itercount_array(0, 0, nullptr, nullptr); // invalid
		if (m_nTrans)
		{
			if (m_nTrans[0])
				delete_aligned(m_nTrans[0]);
			delete[] m_nTrans;
			m_nTrans = NULL;
		}
		if (m_nPhase)
		{
			if (m_nPhase[0])
				delete_aligned(m_nPhase[0]);
			delete[] m_nPhase;
			m_nPhase = nullptr;
		}
		if (m_nDEx)
		{
			if (m_nDEx[0])
				delete_aligned(m_nDEx[0]);
			delete[] m_nDEx;
			m_nDEx = nullptr;
		}
		if (m_nDEy)
		{
			if (m_nDEy[0])
				delete_aligned(m_nDEy[0]);
			delete[] m_nDEy;
			m_nDEy = nullptr;
		}
		FreeBitmap();
}

void CFraktalSFT::SetPosition(const CDecNumber &re, const CDecNumber &im, const CDecNumber &zoom, unsigned digits10)
{

	long e = 0;
	mpfr_get_d_2exp(&e, zoom.m_dec.backend().data(), MPFR_RNDN);

	m_nZoom = std::max(long(double(e)*0.30103), 1L);
	if(digits10 == 0)
		digits10 = 20 + std::max(20, m_nZoom);
	Precision pHi(digits10);

	m_rref.m_f.precision(digits10);
	m_iref.m_f.precision(digits10);
	m_CenterRe.m_f.precision(digits10);
	m_CenterIm.m_f.precision(digits10);
	m_ZoomRadius.m_f.precision(20u);

	m_CenterRe = re.m_dec;
	m_CenterIm = im.m_dec;
	m_ZoomRadius = (2/zoom).m_dec;

#ifndef WINVER
	// XXX code also in SetImageSize; if WINVER, in Render.
	CFixedFloat pixel_spacing = (m_ZoomRadius * 2) / m_nY;
	m_fPixelSpacing = floatexp(pixel_spacing);
#endif
}

void CFraktalSFT::SetPosition(const std::string &szR, const std::string &szI, const std::string &szZ)
{
	/*
		one must get the required precision from the zoom level
		before using that precision when converting re,im from string
		otherwise the precision is likely to be too low (e.g. unzoomed)
		leading to incorrect images (precision loss from rounding)
	*/
	try
	{
		Precision pLo(20u);
		CDecNumber z(szZ); // throws on bad string

		long e = 0;
		mpfr_get_d_2exp(&e, z.m_dec.backend().data(), MPFR_RNDN);
		unsigned digits10 = std::max(20L, long(20 + 0.30103 * e));
		Precision pHi(digits10);

		CDecNumber re(szR); // throws on bad string
		CDecNumber im(szI); // throws on bad string

		SetPosition(re,im,z, digits10);
	}
	catch (...)
	{
		std::cerr << "ERROR: SetPosition(): couldn't parse float (ignored)" << std::endl;
		/*
			if a float could not be parsed, the previous value will be used
			because all the parsing is done before the state is modified
		*/
	}
}

void CFraktalSFT::SetPosition(const char *szR, const char *szI, const char *szZ)
{
	return SetPosition(std::string(szR), std::string(szI), std::string(szZ));
}

#ifdef KF_OPENCL
void CFraktalSFT::RenderFractalOpenCL(const Reference_Type reftype)
{
	if (m_bStop)
	{
		return;
	}
	m_OpenCL_Glitched = false;
	m_OpenCL_Glitched_X = -1;
	m_OpenCL_Glitched_Y = -1;
	m_OpenCL_Glitched_Count = 0;
	m_bIterChanged = TRUE;
	size_t stride_y = &m_nTrans[0][1] - &m_nTrans[0][0];
	size_t stride_x = &m_nTrans[1][0] - &m_nTrans[0][0];
	size_t stride_offset = 0;
	const double nBailout = GetBailoutRadius();
	const double norm_p = GetBailoutNorm();
	const double nBailout2 = norm_p < 1.0/0.0 ? pow(nBailout, norm_p) : nBailout;
	mat2 transform = GetTransformMatrix();
	if (reftype == Reference_Float || reftype == Reference_ScaledFloat)
	{
		tfloatexp<float, int32_t> *APr = nullptr;
		tfloatexp<float, int32_t> *APi = nullptr;
		SeriesR2<float, int32_t> *APs = nullptr;
		int terms = GetApproxTerms();
		{
			APr = new tfloatexp<float, int32_t>[MAX_APPROX_TERMS+1];
			for (int i = 0; i < terms; ++i)
			{
				APr[i] = tfloatexp<float, int32_t>(m_APr[i]);
			}
		}
		{
			APi = new tfloatexp<float, int32_t>[MAX_APPROX_TERMS+1];
			for (int i = 0; i < terms; ++i)
			{
				APi[i] = tfloatexp<float, int32_t>(m_APi[i]);
			}
		}
		{
			APs = new SeriesR2<float, int32_t>;
			for (int i = 0; i < MAX_APPROX_TERMS+1; ++i)
			{
				for (int j = 0; j < MAX_APPROX_TERMS+1; ++j)
				{
					APs->s[i][j] = tfloatexp<float, int32_t>(m_APs->s[i][j]);
					APs->t[i][j] = tfloatexp<float, int32_t>(m_APs->t[i][j]);
				}
			}
		}
		cl->run<float, int32_t, float>
		(
		  reftype,
		  // for pixel -> parameter mapping
		  m_nX,
		  m_nY,
		  GetJitterSeed(),
		  GetJitterShape(),
		  GetJitterScale(),
		  tfloatexp<float, int32_t>(m_pixel_center_x),
		  tfloatexp<float, int32_t>(m_pixel_center_y),
		  tfloatexp<float, int32_t>(m_pixel_scale),
		  transform[0][0],
		  transform[0][1],
		  transform[1][0],
		  transform[1][1],
		  GetExponentialMap(),
		  // for result -> output mapping
		  stride_y,
		  stride_x,
		  stride_offset,
		  // for iteration control
		  nBailout,
		  nBailout2,
		  log(nBailout),
		  log(m_nPower),
		  m_nMaxIter,
		  m_nMaxIter,
		  reference_size_x(m_Reference),
		  m_bNoGlitchDetection,
		  m_bAddReference,
		  m_nSmoothMethod,
		  m_real,
		  m_imag,
		  norm_p,
		  m_FactorAR,
		  m_FactorAI,
		  m_epsilon,
		  // for series approximation
		  m_nMaxApproximation,
		  GetApproxTerms(),
		  GetApproximationType(),
		  APr,
		  APi,
		  APs,

		  // reference orbit
		  reference_ptr_x<float>(m_Reference),
		  reference_ptr_y<float>(m_Reference),
		  reference_ptr_z<float>(m_Reference),
		  0,
		  reference_size_x(m_Reference),
		  reference_size_N(m_Reference),
		  reference_ptr_N(m_Reference),
		  reference_ptr_X<float, int32_t>(m_Reference),
		  reference_ptr_Y<float, int32_t>(m_Reference),
		  reference_ptr_Z<float, int32_t>(m_Reference),

		  // formula selection
		  m_nFractalType,
		  m_nPower,
		  GetDerivatives(),
		  GetTriangleInequalityAverage(),

		  m_UseHybridFormula,
		  m_HybridFormula,

		  GetGuessing(),
		  m_nAddRefX,
		  m_nAddRefY,

		  GetGlitchCenterMethod(),
		  GetIsolatedGlitchNeighbourhood(),

		  // output arrays
		  m_nPixels_MSB,
		  m_nPixels_LSB,
		  &m_nTrans[0][0],
		  m_nPhase ? &m_nPhase[0][0] : nullptr,
		  m_nDEx ? &m_nDEx[0][0] : nullptr,
		  m_nDEy ? &m_nDEy[0][0] : nullptr,

		  m_bInteractive,
		  m_bAutoGlitch,
		  GetMaxReferences(),

		  m_OpenCL_Glitched,
		  m_OpenCL_Glitched_X,
		  m_OpenCL_Glitched_Y,
		  m_OpenCL_Glitched_Count
		);
		if (APr) delete[] APr;
		if (APi) delete[] APi;
		if (APs) delete   APs;
	}
	else if (reftype == Reference_FloatExpFloat)
	{
		tfloatexp<float, int32_t> *APr = nullptr;
		tfloatexp<float, int32_t> *APi = nullptr;
		SeriesR2<float, int32_t> *APs = nullptr;
		int terms = GetApproxTerms();
		{
			APr = new tfloatexp<float, int32_t>[MAX_APPROX_TERMS+1];
			for (int i = 0; i < terms; ++i)
			{
				APr[i] = tfloatexp<float, int32_t>(m_APr[i]);
			}
		}
		{
			APi = new tfloatexp<float, int32_t>[MAX_APPROX_TERMS+1];
			for (int i = 0; i < terms; ++i)
			{
				APi[i] = tfloatexp<float, int32_t>(m_APi[i]);
			}
		}
		{
			APs = new SeriesR2<float, int32_t>;
			for (int i = 0; i < MAX_APPROX_TERMS+1; ++i)
			{
				for (int j = 0; j < MAX_APPROX_TERMS+1; ++j)
				{
				APs->s[i][j] = tfloatexp<float, int32_t>(m_APs->s[i][j]);
				APs->t[i][j] = tfloatexp<float, int32_t>(m_APs->t[i][j]);
				}
			}
		}
		cl->run<float, int32_t, tfloatexp<float, int32_t>>
		(
		  reftype,
		  // for pixel -> parameter mapping
		  m_nX,
		  m_nY,
		  GetJitterSeed(),
		  GetJitterShape(),
		  GetJitterScale(),
		  tfloatexp<float, int32_t>(m_pixel_center_x),
		  tfloatexp<float, int32_t>(m_pixel_center_y),
		  tfloatexp<float, int32_t>(m_pixel_scale),
		  transform[0][0],
		  transform[0][1],
		  transform[1][0],
		  transform[1][1],
		  GetExponentialMap(),
		  // for result -> output mapping
		  stride_y,
		  stride_x,
		  stride_offset,
		  // for iteration control
		  nBailout,
		  nBailout2,
		  log(nBailout),
		  log(m_nPower),
		  m_nMaxIter,
		  m_nMaxIter,
		  reference_size_x(m_Reference),
		  m_bNoGlitchDetection,
		  m_bAddReference,
		  m_nSmoothMethod,
		  m_real,
		  m_imag,
		  norm_p,
		  m_FactorAR,
		  m_FactorAI,
		  m_epsilon,
		  // for series approximation
		  m_nMaxApproximation,
		  GetApproxTerms(),
		  GetApproximationType(),
		  APr,
		  APi,
		  APs,

		  // reference orbit
		  reference_ptr_x<tfloatexp<float, int32_t>>(m_Reference),
		  reference_ptr_y<tfloatexp<float, int32_t>>(m_Reference),
		  reference_ptr_z<tfloatexp<float, int32_t>>(m_Reference),
		  0,
		  reference_size_x(m_Reference),
		  reference_size_N(m_Reference),
		  reference_ptr_N(m_Reference),
		  reference_ptr_X<float, int32_t>(m_Reference),
		  reference_ptr_Y<float, int32_t>(m_Reference),
		  reference_ptr_Z<float, int32_t>(m_Reference),

		  // formula selection
		  m_nFractalType,
		  m_nPower,
		  GetDerivatives(),
		  GetTriangleInequalityAverage(),

		  m_UseHybridFormula,
		  m_HybridFormula,

		  GetGuessing(),
		  m_nAddRefX,
		  m_nAddRefY,

		  GetGlitchCenterMethod(),
		  GetIsolatedGlitchNeighbourhood(),

		  // output arrays
		  m_nPixels_MSB,
		  m_nPixels_LSB,
		  &m_nTrans[0][0],
		  m_nPhase ? &m_nPhase[0][0] : nullptr,
		  m_nDEx ? &m_nDEx[0][0] : nullptr,
		  m_nDEy ? &m_nDEy[0][0] : nullptr,

		  m_bInteractive,
		  m_bAutoGlitch,
		  GetMaxReferences(),

		  m_OpenCL_Glitched,
		  m_OpenCL_Glitched_X,
		  m_OpenCL_Glitched_Y,
		  m_OpenCL_Glitched_Count
		);
		if (APr) delete[] APr;
		if (APi) delete[] APi;
		if (APs) delete   APs;
	}
	else if (reftype == Reference_Double || reftype == Reference_ScaledDouble)
	{
		cl->run<double, int64_t, double>
		(
		  reftype,
		  // for pixel -> parameter mapping
		  m_nX,
		  m_nY,
		  GetJitterSeed(),
		  GetJitterShape(),
		  GetJitterScale(),
		  m_pixel_center_x,
		  m_pixel_center_y,
		  m_pixel_scale,
		  transform[0][0],
		  transform[0][1],
		  transform[1][0],
		  transform[1][1],
		  GetExponentialMap(),
		  // for result -> output mapping
		  stride_y,
		  stride_x,
		  stride_offset,
		  // for iteration control
		  nBailout,
		  nBailout2,
		  log(nBailout),
		  log(m_nPower),
		  m_nMaxIter,
		  m_nMaxIter,
		  reference_size_x(m_Reference),
		  m_bNoGlitchDetection,
		  m_bAddReference,
		  m_nSmoothMethod,
		  m_real,
		  m_imag,
		  norm_p,
		  m_FactorAR,
		  m_FactorAI,
		  m_epsilon,
		  // for series approximation
		  m_nMaxApproximation,
		  GetApproxTerms(),
		  GetApproximationType(),
		  m_APr,
		  m_APi,
		  m_APs,

		  // reference orbit
		  reference_ptr_x<double>(m_Reference),
		  reference_ptr_y<double>(m_Reference),
		  reference_ptr_z<double>(m_Reference),
		  0,
		  reference_size_x(m_Reference),
		  reference_size_N(m_Reference),
		  reference_ptr_N(m_Reference),
		  reference_ptr_X<double, int64_t>(m_Reference),
		  reference_ptr_Y<double, int64_t>(m_Reference),
		  reference_ptr_Z<double, int64_t>(m_Reference),

		  // formula selection
		  m_nFractalType,
		  m_nPower,
		  GetDerivatives(),
		  GetTriangleInequalityAverage(),

		  m_UseHybridFormula,
		  m_HybridFormula,

		  GetGuessing(),
		  m_nAddRefX,
		  m_nAddRefY,

		  GetGlitchCenterMethod(),
		  GetIsolatedGlitchNeighbourhood(),

		  // output arrays
		  m_nPixels_MSB,
		  m_nPixels_LSB,
		  &m_nTrans[0][0],
		  m_nPhase ? &m_nPhase[0][0] : nullptr,
		  m_nDEx ? &m_nDEx[0][0] : nullptr,
		  m_nDEy ? &m_nDEy[0][0] : nullptr,

		  m_bInteractive,
		  m_bAutoGlitch,
		  GetMaxReferences(),

		  m_OpenCL_Glitched,
		  m_OpenCL_Glitched_X,
		  m_OpenCL_Glitched_Y,
		  m_OpenCL_Glitched_Count
		);
	}
	else if (reftype == Reference_FloatExpDouble)
	{
		cl->run<double, int64_t, floatexp>
		(
		  reftype,
		  // for pixel -> parameter mapping
		  m_nX,
		  m_nY,
		  GetJitterSeed(),
		  GetJitterShape(),
		  GetJitterScale(),
		  m_pixel_center_x,
		  m_pixel_center_y,
		  m_pixel_scale,
		  transform[0][0],
		  transform[0][1],
		  transform[1][0],
		  transform[1][1],
		  GetExponentialMap(),
		  // for result -> output mapping
		  stride_y,
		  stride_x,
		  stride_offset,
		  // for iteration control
		  nBailout,
		  nBailout2,
		  log(nBailout),
		  log(m_nPower),
		  m_nMaxIter,
		  m_nMaxIter,
		  reference_size_x(m_Reference),
		  m_bNoGlitchDetection,
		  m_bAddReference,
		  m_nSmoothMethod,
		  m_real,
		  m_imag,
		  norm_p,
		  m_FactorAR,
		  m_FactorAI,
		  m_epsilon,
		  // for series approximation
		  m_nMaxApproximation,
		  GetApproxTerms(),
		  GetApproximationType(),
		  m_APr,
		  m_APi,
		  m_APs,

		  // reference orbit
		  reference_ptr_x<floatexp>(m_Reference),
		  reference_ptr_y<floatexp>(m_Reference),
		  reference_ptr_z<floatexp>(m_Reference),
		  0,
		  reference_size_x(m_Reference),
		  reference_size_N(m_Reference),
		  reference_ptr_N(m_Reference),
		  reference_ptr_X<double, int64_t>(m_Reference),
		  reference_ptr_Y<double, int64_t>(m_Reference),
		  reference_ptr_Z<double, int64_t>(m_Reference),

		  // formula selection
		  m_nFractalType,
		  m_nPower,
		  GetDerivatives(),
		  GetTriangleInequalityAverage(),

		  m_UseHybridFormula,
		  m_HybridFormula,

		  GetGuessing(),
		  m_nAddRefX,
		  m_nAddRefY,

		  GetGlitchCenterMethod(),
		  GetIsolatedGlitchNeighbourhood(),

		  // output arrays
		  m_nPixels_MSB,
		  m_nPixels_LSB,
		  &m_nTrans[0][0],
		  m_nPhase ? &m_nPhase[0][0] : nullptr,
		  m_nDEx ? &m_nDEx[0][0] : nullptr,
		  m_nDEy ? &m_nDEy[0][0] : nullptr,

		  m_bInteractive,
		  m_bAutoGlitch,
		  GetMaxReferences(),

		  m_OpenCL_Glitched,
		  m_OpenCL_Glitched_X,
		  m_OpenCL_Glitched_Y,
		  m_OpenCL_Glitched_Count
		);
	}
}
#endif

#ifdef WINVER
HBITMAP CFraktalSFT::GetBitmap()
{
	WaitForMutex(m_hMutex);
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
	WaitForMutex(m_hMutex);
	if (m_bmi && m_lpBits){
		HDC hDC = GetDC(NULL);
		if (!GetDIBits(hDC, m_bmBmp, 0, m_bmi->biHeight, m_lpBits,
			(LPBITMAPINFO)m_bmi, DIB_RGB_COLORS))
			{ /*Beep(1000,10)*/ }
		ReleaseDC(NULL, hDC);
	}
	ReleaseMutex(m_hMutex);
}

void CFraktalSFT::Stop()
{
	m_bNoPostWhenDone = TRUE; // inhibits colouring after stop completes
	m_bStop = true;
	m_bAddReference = FALSE;
	if (m_nMaxOldGlitches && m_pOldGlitch[m_nMaxOldGlitches - 1].x == -1)
		m_bNoGlitchDetection = FALSE;
	else
		m_bNoGlitchDetection = TRUE;
	double counter = 0;
	while (m_bIsRendering)
	{
		Sleep(1);
		counter += 1;
	}
#ifdef KF_DEBUG_SLEEP
	if (counter > 0)
		std::cerr << "Stop() slept for " << counter << "ms" << std::endl;
#endif
	m_bStop = false;
	m_bNoPostWhenDone=0;
}
#endif

void CFraktalSFT::Zoom(double nZoomSize)
{
#ifdef WINVER
	Stop();
#endif
	m_bAddReference = FALSE;
	if (m_nMaxOldGlitches && m_pOldGlitch[m_nMaxOldGlitches-1].x == -1)
		m_bNoGlitchDetection = FALSE;
	else
		m_bNoGlitchDetection = TRUE;

	m_ZoomRadius /= nZoomSize;
#ifdef WINVER
	Render();
#endif
}

void CFraktalSFT::Zoom(int nXPos, int nYPos, double nZoomSize, BOOL bReuseCenter, bool center_view)
{
#ifdef WINVER
	Stop();
#endif
	floatexp a, b;
	GetPixelCoordinates(nXPos, nYPos, a, b);

	m_bAddReference = FALSE;
	if (m_nMaxOldGlitches && m_pOldGlitch[m_nMaxOldGlitches-1].x == -1)
		m_bNoGlitchDetection = FALSE;
	else
		m_bNoGlitchDetection = TRUE;

	// zooming out. XXX do the same, inverted, for zooming in.
	if (bReuseCenter && !GetNoReuseCenter() && nZoomSize<=1){
		m_bAddReference = 2;
		int nOX = m_nX*nZoomSize;
		int nOY = m_nY*nZoomSize;
		int i;
		int64_t **Org = new int64_t*[nOX];
		for (i = 0; i<nOX; i++)
			Org[i] = new int64_t[nOY];
		float **OrgT = new float*[nOX];
		for (i = 0; i<nOX; i++)
			OrgT[i] = new float[nOY];
		float **OrgP = new float*[nOX];
		for (i = 0; i<nOX; i++)
			OrgP[i] = new float[nOY];
		float **OrgDEx = new float*[nOX];
		for (i = 0; i<nOX; i++)
			OrgDEx[i] = new float[nOY];
		float **OrgDEy = new float*[nOX];
		for (i = 0; i<nOX; i++)
			OrgDEy[i] = new float[nOY];
		int x, y, a, b;
		int nX2 = m_nX/2;
		int nY2 = m_nY/2;
		if(0 && nZoomSize<1 && nZoomSize>.8){
			for (x = 0; x<nOX; x++){
				for (y = 0; y<nOY; y++){
					Org[x][y]=PIXEL_UNEVALUATED;
				}
			}
		}
		else{
			for (x = 0; x<nOX; x++){
				for (y = 0; y<nOY; y++){
					a = (x + nXPos-nX2) / nZoomSize;
					b = (y + nYPos-nY2) / nZoomSize;
					if (a >= 0 && a < m_nX && b >= 0 && b < m_nY){
						Org[x][y] = m_nPixels[a][b];
						OrgT[x][y] = m_nTrans[a][b];
						OrgP[x][y] = m_nPhase[a][b];
						OrgDEx[x][y] = m_nDEx[a][b];
						OrgDEy[x][y] = m_nDEy[a][b];
						if (Org[x][y]>m_nMaxIter)
							Org[x][y] = m_nMaxIter;
					}
					else
					{
						Org[x][y] = PIXEL_UNEVALUATED;
						OrgT[x][y] = SET_TRANS_GLITCH(0);
						OrgP[x][y] = 0;
						OrgDEx[x][y] = 0;
						OrgDEy[x][y] = 0;
					}
				}
			}
		}
		a = (m_nX - nOX) / 2;
		b = (m_nY - nOY) / 2;
		for (x = 0; x<m_nX; x++){
			for (y = 0; y<m_nY; y++){
				if (x-a >= 0 && x-a < nOX && y-b >= 0 && y-b < nOY){
					m_nPixels[x][y] = Org[x - a][y - b];
					m_nTrans[x][y] = OrgT[x - a][y - b];
					m_nPhase[x][y] = OrgP[x - a][y - b];
					m_nDEx[x][y] = OrgDEx[x - a][y - b];
					m_nDEy[x][y] = OrgDEy[x - a][y - b];
				}
				else
				{
					m_nPixels[x][y] = PIXEL_UNEVALUATED;
					m_nTrans[x][y] = SET_TRANS_GLITCH(0);
					m_nPhase[x][y] = 0;
					m_nDEx[x][y] = 0;
					m_nDEy[x][y] = 0;
				}
			}
		}
		for (i = 0; i<nOX; i++){
			delete[] Org[i];
			delete[] OrgT[i];
			delete[] OrgP[i];
			delete[] OrgDEx[i];
			delete[] OrgDEy[i];
		}
		delete[] Org;
		delete[] OrgT;
		delete[] OrgP;
		delete[] OrgDEx;
		delete[] OrgDEy;
	}

	unsigned digits10 = 20u;
	{
		Precision pLo(20u);
		CFixedFloat pixelSpacing(m_ZoomRadius * 2 / m_nY); // FIXME handle skew
		long e = 0;
		mpfr_get_d_2exp(&e, pixelSpacing.m_f.backend().data(), MPFR_RNDN);
		digits10 = std::max(20.0, 20 + 0.30102999566398114 * (log2(nZoomSize) - e));
		CFixedFloat radius = m_ZoomRadius / nZoomSize;
		Precision p(digits10);
		double g = nZoomSize;
		if (g == 1 || center_view)
		{
		  g = 1.0 / 0.0;
		}
		m_CenterRe.m_f.precision(digits10);
		m_CenterIm.m_f.precision(digits10);
		m_rref.m_f.precision(digits10);
		m_iref.m_f.precision(digits10);
		CFixedFloat re0 = m_CenterRe;
		CFixedFloat im0 = m_CenterIm;
		CFixedFloat re1 = m_rref + CFixedFloat(a);
		CFixedFloat im1 = m_iref + CFixedFloat(b);
		CFixedFloat re = re1 + (re0 - re1) / g;
		CFixedFloat im = im1 + (im0 - im1) / g;


		m_CenterRe = re;
		m_CenterIm = im;
		m_ZoomRadius = radius;
	}
#ifdef WINVER
	Render();
#endif
}

double CFraktalSFT::GetProgress(double *reference, double *approximation, double *good_guessed, double *good, double *queued, double *bad, double *bad_guessed)
{
	int64_t iters = m_nMaxIter;
	if ((GetUseNanoMB1() || GetUseNanoMB2()) && m_bAutoGlitch == 1) iters = N.g_period;
	if (iters <= 0) iters = 1;
	if (reference) *reference = m_nRDone * 100.0 / iters;
	if (approximation) *approximation = m_nApprox * 100.0 / iters;
	int32_t good_guessed_0 = m_count_good_guessed;
	int32_t good_0 = m_count_good;
	int32_t queued_0 = m_count_queued;
	int32_t bad_0 = m_count_bad;
	int32_t bad_guessed_0 = m_count_bad_guessed;
	int32_t total = good_guessed_0 + good_0 + queued_0 + bad_0 + bad_guessed_0;
	if (total == 0) total = 1;
	if (good_guessed) *good_guessed = good_guessed_0 * 100.0 / total;
	if (good) *good = good_0 * 100.0 / total;
	if (queued) *queued = queued_0 * 100.0 / total;
	if (bad) *bad = bad_0 * 100.0 / total;
	if (bad_guessed) *bad_guessed = bad_guessed_0 * 100.0 / total;
	return (good_guessed_0 + good_0) * 100.0 / total;
}

void CFraktalSFT::GetIterations(int64_t &nMin, int64_t &nMax, int *pnCalculated, int *pnType, BOOL bSkipMaxIter)
{
	if (m_bIterChanged || pnCalculated){
		int64_t nMA = (m_nMaxApproximation == m_nMaxIter ? 0 : m_nMaxApproximation);
		int64_t nCalc1 = 0;
		int64_t nCalc2 = 0;
		if (pnType)
			*pnType = 0;
		if (m_nPixels){
			int x, y;
			nMax = PIXEL_UNEVALUATED;
			nMin = PIXEL_UNEVALUATED;
			for (x = 0; x<m_nX; x++){
				for (y = 0; y<m_nY; y++){
					if (bSkipMaxIter && m_nPixels[x][y] >= m_nMaxIter - 1) // FIXME what about glitches?
						continue;
					if (m_nPixels[x][y] != PIXEL_UNEVALUATED && (nMin == PIXEL_UNEVALUATED || nMin>m_nPixels[x][y]))
						nMin = m_nPixels[x][y];
					if (m_nPixels[x][y] != PIXEL_UNEVALUATED && (nMax == PIXEL_UNEVALUATED || nMax<m_nPixels[x][y]))
						nMax = m_nPixels[x][y];
					if (pnCalculated && m_nPixels[x][y] != PIXEL_UNEVALUATED){
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
int64_t CFraktalSFT::GetIterations()
{
	return m_nMaxIter;
}
void CFraktalSFT::SetIterations(int64_t nIterations)
{
	m_nMaxIter = nIterations;
}
std::string CFraktalSFT::GetRe()
{
	return m_CenterRe.ToText();
}
std::string CFraktalSFT::GetRe(int nXPos, int nYPos)
{
	floatexp a, b;
	GetPixelCoordinates(nXPos, nYPos, a, b);
	CFixedFloat re = m_rref + CFixedFloat(a);
	return re.ToText();
}
std::string CFraktalSFT::GetIm()
{
	return m_CenterIm.ToText();
}
std::string CFraktalSFT::GetIm(int nXPos, int nYPos)
{
	floatexp a, b;
	GetPixelCoordinates(nXPos, nYPos, a, b);
	CFixedFloat im = m_iref + CFixedFloat(b);
	return im.ToText();
}
std::string CFraktalSFT::GetZoom()
{
	CFixedFloat zoom = CFixedFloat(2) / m_ZoomRadius;
	floatexp zoomFE = floatexp(zoom);
	return zoomFE.toString();
}
BOOL CFraktalSFT::HighestIteration(int &rx, int &ry)
{
	if (!m_nPixels)
		return FALSE;
	int x, y;
	int nMax = PIXEL_UNEVALUATED;
	for (x = 0; x<m_nX; x++)
	for (y = 0; y<m_nY; y++)
	if (m_nPixels[x][y] != PIXEL_UNEVALUATED && m_nPixels[x][y]>nMax){
		nMax = m_nPixels[x][y];
		rx = x;
		ry = y;
	}
	return nMax != PIXEL_UNEVALUATED;
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
	// GetBitmap(); // required here why?
	int64_t nMin, nMax;
	GetIterations(nMin, nMax, NULL, NULL, TRUE);
	int64_t nMinIter = nMin + (nMax - nMin) / 4;

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
					int nIndex1 = x1 * BM_WIDTH + (m_bmi->biHeight - 1 - y1)*m_row;
					int nIndex2 = x2 * BM_WIDTH + (m_bmi->biHeight - 1 - y2)*m_row;
					if(((unsigned int)(nIndex2))>m_bmi->biSizeImage-3)
						continue;
					int	t = (m_lpBits[nIndex1]+m_lpBits[nIndex1+1]+m_lpBits[nIndex1+2])-(m_lpBits[nIndex2]+m_lpBits[nIndex2+1]+m_lpBits[nIndex2+2]);
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
	bool resized = m_nX != nx || m_nY != ny;
	if (resized){
		DeleteArrays();
		m_nX = nx;
		m_nY = ny;
		SetupArrays();
		m_bResized |= resized;
#ifndef WINVER
		// XXX code also in SetPosition; if WINVER, in Render.
		CFixedFloat pixel_spacing = (m_ZoomRadius * 2) / m_nY;
		m_fPixelSpacing = floatexp(pixel_spacing);
#endif
	}

	memset(m_nPixels_LSB, 0, sizeof(*m_nPixels_LSB) * m_nX * m_nY);
	if (m_nPixels_MSB)
		memset(m_nPixels_MSB, 0, sizeof(*m_nPixels_MSB) * m_nX * m_nY);
	memset(m_nTrans[0], 0, sizeof(float) * m_nX * m_nY);
	memset(m_nPhase[0], 0, sizeof(float) * m_nX * m_nY);
	memset(m_nDEx[0], 0, sizeof(float) * m_nX * m_nY);
	memset(m_nDEy[0], 0, sizeof(float) * m_nX * m_nY);
}

bool CFraktalSFT::OpenMapEXR(const std::string &szfile)
{
	SYSTEM_INFO sysinfo;
	GetSystemInfo(&sysinfo);
	int nParallel = GetThreadsPerCore() * sysinfo.dwNumberOfProcessors - GetThreadsReserveCore();
	if (nParallel < 1 || ! GetEXRParallel()) nParallel = 1;
	bool ret = ReadEXRMapFile(szfile, nParallel);
	m_bIterChanged = true;
	return ret;
}

BOOL CFraktalSFT::OpenMapB(const std::string &szFile, BOOL bReuseCenter, double nZoomSize)
{
	int **Org = 0;
	float **OrgT = 0;
	float **OrgP = 0;
	float **OrgDEx = 0;
	float **OrgDEy = 0;
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
		OrgP = new float*[nOX];
		for (i = 0; i<nOX; i++)
			OrgP[i] = new float[nOY];
		OrgDEx = new float*[nOX];
		for (i = 0; i<nOX; i++)
			OrgDEx[i] = new float[nOY];
		OrgDEy = new float*[nOX];
		for (i = 0; i<nOX; i++)
			OrgDEy[i] = new float[nOY];
		for (x = 0; x<nOX; x++){
			for (y = 0; y<nOY; y++){
				a = x / nZoomSize;
				b = y / nZoomSize;
				if (a >= 0 && a<m_nX && b >= 0 && b<m_nY)
				{
					Org[x][y] = m_nPixels[a][b];
					OrgT[x][y] = m_nTrans[a][b];
					OrgP[x][y] = m_nPhase[a][b];
					OrgDEx[x][y] = m_nDEx[a][b];
					OrgDEy[x][y] = m_nDEy[a][b];
				}
			}
		}
		a = (m_nX - nOX) / 2;
		b = (m_nY - nOY) / 2;
	}
	std::ifstream hFile(szFile, std::ios::in | std::ios::binary);
	if (!hFile)
		return FALSE; // FIXME leaks Org arrays
	char szId[3];
	hFile.read(szId, 3);
	char bNewFormat;  // yes that's misnamed
	if (!strncmp(szId, "KFB", 3))
		bNewFormat = 0;
	else if (!strncmp(szId, "KFC", 3))
		bNewFormat = 1;
	else if (!strncmp(szId, "KFD", 3))
		bNewFormat = 2;
	else{
		hFile.close();
		return FALSE;
	}
	int nx = -1, ny = -1;
	hFile.read(reinterpret_cast<char*>(&nx), sizeof(nx));
	hFile.read(reinterpret_cast<char*>(&ny), sizeof(ny));
	SetImageSize(nx, ny);
	float *pLine = NULL;
	if (bNewFormat == 1)
		pLine = new float[m_nY];
	else if (bNewFormat == 2)
		pLine = new float[m_nX];
	if (bNewFormat == 2){
		for (y = 0; y<m_nY; y++){
			hFile.read(reinterpret_cast<char*>(pLine), sizeof(float)*m_nX);
			for (x = 0; x<m_nX; x++){
				m_nPixels[x][y] = (int)pLine[x];
				m_nTrans[x][y] = pLine[x] - (int)m_nPixels[x][y];
			}
		}
	}
	else{
		for (x = 0; x<m_nX; x++){
			if (bNewFormat > 0){
				hFile.read(reinterpret_cast<char*>(pLine), sizeof(float)*m_nY);
				for (y = 0; y<m_nY; y++){
					m_nPixels[x][y] = (int)pLine[y];
					m_nTrans[x][y] = pLine[y] - (int)m_nPixels[x][y];
				}
			}
			else
				hFile.read(reinterpret_cast<char*>(m_nPixels_LSB + x * m_nY), sizeof(*m_nPixels_LSB)*m_nY);
		}
	}
	if (pLine)
		delete[] pLine;
	int div = 1;
	hFile.read(reinterpret_cast<char*>(&div), sizeof(div));
#if 0 // don't read IterDiv, it is stored as int (not float) due to historical reasons
	m_nIterDiv = div;
	if (m_nIterDiv == 0)
		m_nIterDiv = 1;
#endif
	hFile.read(reinterpret_cast<char*>(&m_nParts), sizeof(m_nParts));
	hFile.read(reinterpret_cast<char*>(m_cKeys), sizeof(COLOR14)*m_nParts);
	int nTest;
	hFile.read(reinterpret_cast<char*>(&nTest), sizeof(nTest));
	if (hFile.good())
		m_nMaxIter = nTest;

	if (bNewFormat == 0){
		for (x = 0; hFile.good() && x<m_nX; x++){
			hFile.read(reinterpret_cast<char*>(m_nTrans[x]), sizeof(float)*m_nY);
		}
		off_t pos1 = hFile.tellg();
		hFile.seekg(0, std::ios::end);
		off_t pos2 = hFile.tellg();
		if (pos1 != pos2)
		{
			hFile.seekg(pos1);
			for (x = 0; hFile.good() && x<m_nX; x++){
				hFile.read(reinterpret_cast<char*>(m_nDEx[x]), sizeof(float)*m_nY);
			}
		}
	}
	bool ok = hFile.good();
	hFile.close();

	if (bReuseCenter && !GetNoReuseCenter() && nZoomSize<1){
		for (x = 0; x<m_nX; x++){
			for (y = 0; y<m_nY; y++){
				if (x - a>0 && x - a<nOX - 1 && y - b>0 && y - b<nOY - 1){
					m_nPixels[x][y] = Org[x - a][y - b];
					m_nTrans[x][y] = OrgT[x - a][y - b];
					m_nPhase[x][y] = OrgP[x - a][y - b];
					m_nDEx[x][y] = OrgDEx[x - a][y - b];
					m_nDEy[x][y] = OrgDEy[x - a][y - b];
				}
			}
		}
		for (i = 0; i<nOX; i++){
			delete[] Org[i];
			delete[] OrgT[i];
			delete[] OrgP[i];
			delete[] OrgDEx[i];
			delete[] OrgDEy[i];
		}
		delete[] Org;
		delete[] OrgT;
		delete[] OrgP;
		delete[] OrgDEx;
		delete[] OrgDEy;
	}
	ReinitializeBitmap();
	m_bIterChanged = true;
	return ok;
}

void CFraktalSFT::ReinitializeBitmap()
{
	FreeBitmap();
	AllocateBitmap();
}

void CFraktalSFT::AllocateBitmap()
{
	if (m_bmi) {
		if(!m_lpBits)
			abort();
		return;
	}
	if(m_lpBits)
		abort();

	m_bmi = (BITMAPINFOHEADER *)malloc(sizeof(BITMAPINFOHEADER)+sizeof(RGBQUAD)* 256);
	memset(m_bmi, 0, sizeof(BITMAPINFOHEADER)+sizeof(RGBQUAD)* 256);
#ifdef WINVER
	m_bmi->biSize = sizeof(BITMAPINFOHEADER);
	m_bmi->biCompression = m_bmi->biClrUsed = m_bmi->biClrImportant = 0;
	HDC hDC = GetDC(NULL);
	m_bmBmp = create_bitmap(hDC, m_nX, m_nY);
	if (!GetDIBits(hDC, m_bmBmp, 0, 0, NULL, (LPBITMAPINFO)m_bmi, DIB_RGB_COLORS))
		{ /*Beep(1000,10)*/ }
#else
	m_bmi->biWidth = m_nX;
	m_bmi->biHeight = m_nY;
#endif
	m_bmi->biBitCount = 8*BM_WIDTH;
	m_row = ((((m_bmi->biWidth*(DWORD)m_bmi->biBitCount) + 31)&~31) >> 3);
	m_bmi->biSizeImage = m_row*m_bmi->biHeight;

	m_nSizeImage = m_bmi->biSizeImage;
	m_lpBits = new BYTE[m_bmi->biSizeImage];
#ifdef WINVER
	if (!GetDIBits(hDC, m_bmBmp, 0, m_bmi->biHeight, m_lpBits,
		(LPBITMAPINFO)m_bmi, DIB_RGB_COLORS))
			abort();
	ReleaseDC(NULL, hDC);
#endif
}

void CFraktalSFT::FreeBitmap()
{
	if(m_bAddReference)
		return;

#ifdef WINVER
	if (m_bmBmp) {
		DeleteObject(m_bmBmp);
		m_bmBmp = nullptr;
	}
#endif

	if (m_bmi) {
		free(m_bmi);
		m_bmi = nullptr;
	}
	if (m_lpBits) {
		delete[] m_lpBits;
		m_lpBits = nullptr;
	}
}

bool CFraktalSFT::OpenSettings(const std::string &filename) {
	bool ok = m_Settings.OpenFile(filename);
	if(ok) {
		int64_t w,h,s;
		GetTargetDimensions(&w, &h, &s);
		SetImageSize(w * s, h * s);
	}
	return ok;
}

#ifdef WINVER
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
		return SaveImage(szFile, m_lpBits, m_nX, m_nY, nQuality, comment);
	else{
		HBITMAP bmSave = ShrinkBitmap(GetBitmap(), nWidth, nHeight, 3); // always use high quality sRGB shrinking when saving
		int nRet = SaveImage(szFile, bmSave, nQuality, comment);
		DeleteObject(bmSave);
		return nRet;
	}
}
#endif

int64_t CFraktalSFT::GetMaxApproximation()
{
	return m_nApprox;
}
int64_t CFraktalSFT::GetIterationOnPoint(int x, int y)
{
#ifdef WINVER
	WaitForMutex(m_hMutex);
#else
	m_mutex.lock();
#endif
	if (!m_nPixels || x<0 || x >= m_nX || y<0 || y >= m_nY){
#ifdef WINVER
		ReleaseMutex(m_hMutex);
#else
		m_mutex.unlock();
#endif
		return PIXEL_UNEVALUATED;
	}
	int64_t nRet = m_nPixels[x][y];
#ifdef WINVER
	ReleaseMutex(m_hMutex);
#else
	m_mutex.unlock();
#endif
	return nRet;
}
double CFraktalSFT::GetTransOnPoint(int x, int y)
{
	if (x<0 || x >= m_nX || y<0 || y >= m_nY || !m_nTrans){
		return 0;
	}
	return 1 - m_nTrans[x][y];
}
static BOOL IsEqual(int a, int b, int nSpan = 2, BOOL bGreaterThan = FALSE)
{
	//	return a==b;
	if (a == PIXEL_UNEVALUATED || b == PIXEL_UNEVALUATED)
		return 0;
	if (bGreaterThan)
		return a>nSpan;
	int diff = a - b;
	if (diff<0)
		diff = -diff;
	return diff<nSpan;
}

BOOL CFraktalSFT::AddReference(int nXPos, int nYPos, BOOL bEraseAll, BOOL bResuming)
{
	if (!m_nPixels)
		return FALSE;

	// is there free space in the m_pOldGlitch array?
	if (m_nMaxOldGlitches && m_pOldGlitch[m_nMaxOldGlitches-1].x == -1)
		m_bNoGlitchDetection = FALSE;
	else
		m_bNoGlitchDetection = TRUE;
	{
		floatexp dbD0r, dbD0i;
		GetPixelCoordinates(nXPos, nYPos, dbD0r, dbD0i);
		m_rref = m_rref + CFixedFloat(dbD0r);
		m_iref = m_iref + CFixedFloat(dbD0i);
		m_nAddRefX = nXPos;
		m_nAddRefY = nYPos;
	}
#ifdef KF_OPENCL
	if (cl && GetGlitchCenterMethod() != 0)
	{
		m_count_queued = m_OpenCL_Glitched_Count;
	}
	else
#endif
	{
		int x, y;
		if(nXPos>=0 && nXPos<m_nX && nYPos>=0 && nYPos<m_nY)
			;
		else
			bResuming=TRUE;

		int nCount = 0;
		if (bEraseAll){
			for (x = 0; x<m_nX; x++)
				for (y = 0; y<m_nY; y++)
					m_nPixels[x][y] = PIXEL_UNEVALUATED;
			nCount = m_nX * m_nY;
		}
  		else if (!bResuming){
			for (x = 0; x<m_nX; x++){
				for (y = 0; y<m_nY; y++){
					// re-render all and only glitched pixels
					if (GET_TRANS_GLITCH(m_nTrans[x][y]))
					{
						m_nPixels[x][y] = PIXEL_UNEVALUATED;
						nCount++;
					}
				}
			}
		}
		m_count_queued = nCount;
	}
	m_count_bad = 0;
	m_count_bad_guessed = 0;
	m_bAddReference = TRUE;
#ifdef WINVER
	Render(FALSE, FALSE);
#endif
	return TRUE;
}

#define SMOOTH_TO 7
int CFraktalSFT::GetArea(itercount_array &Node, int nXStart,int nYStart,int nEqSpan, itercount_array &Pixels, int nDone, POINT *pQ, int nQSize)
{
	int x, y;
	int nAreaC=0;
	int nTarget = m_nPixels[nXStart][nYStart];

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
				Pixels[x][y]=PIXEL_UNEVALUATED;
			int w=x, e=x;
			while(nValidate && w && IsEqual(Node[w-1][y],nTarget,nEqSpan,Pixels?TRUE:FALSE) && GET_TRANS_GLITCH(m_nTrans[w-1][y])){
				nAreaC++;
				nValidate--;
				w--;
				Node[w][y]=nDone;
				if(Pixels && GET_TRANS_GLITCH(m_nTrans[w][y]))
					Pixels[w][y]=PIXEL_UNEVALUATED;
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
					Pixels[e][y]=PIXEL_UNEVALUATED;
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
					Pixels[x][w]=PIXEL_UNEVALUATED;
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
					Pixels[x][e]=PIXEL_UNEVALUATED;
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
	return nAreaC;
}

void CFraktalSFT::IgnoreIsolatedGlitches()
{
	int neighbourhood = GetIsolatedGlitchNeighbourhood();
	if (neighbourhood)
	{
		for (int x = 0; x < m_nX; ++x)
		{
			for (int y = 0; y < m_nY; ++y)
			{
				if (GET_TRANS_GLITCH(m_nTrans[x][y]))
				{
					bool neighbour_glitched = false;
					double sum = 0;
					double sum_phase_x = 0;
					double sum_phase_y = 0;
					double sum_de_x = 0;
					double sum_de_y = 0;
					for (int dx = -1; dx <= 1; ++dx)
					{
						for (int dy = -1; dy <= 1; ++dy)
						{
							if (dx == 0 && dy == 0)
							{
								continue;
							}
							if (neighbourhood == 4 && dx && dy)
							{
								continue;
							}
							int x2 = x + dx;
							int y2 = y + dy;
							if (x2 < 0 || m_nX <= x2) x2 = x - dx;
							if (y2 < 0 || m_nY <= y2) y2 = y - dy;
							float t = m_nTrans[x2][y2];
							neighbour_glitched |= GET_TRANS_GLITCH(t);
							if (neighbour_glitched)
							{
								break;
							}
							int64_t p = m_nPixels[x2][y2];
							double i = double(p) + double(1 - t);
							sum += i;
							if (m_nPhase)
							{
								sum_phase_x += cos(m_nPhase[x2][y2] * M_PI * 2);
								sum_phase_y += sin(m_nPhase[x2][y2] * M_PI * 2);
							}
							if (m_nDEx) sum_de_x += m_nDEx[x2][y2];
							if (m_nDEy) sum_de_y += m_nDEy[x2][y2];

						}
						if (neighbour_glitched)
						{
							break;
						}
					}
					if (! neighbour_glitched)
					{
						// average the neighbourhood
					  	sum /= neighbourhood;
						int64_t p = floor(sum);
						m_nPixels[x][y] = p;
						m_nTrans[x][y] = 1 - (sum - p);
						if (m_nPhase)
						{
							m_nPhase[x][y] = atan2(sum_phase_y, sum_phase_x) / M_PI / 2;
							m_nPhase[x][y] -= floor(m_nPhase[x][y]);
						}
						if (m_nDEx) m_nDEx[x][y] = sum_de_x / neighbourhood;
						if (m_nDEy) m_nDEy[x][y] = sum_de_y / neighbourhood;
						if(GetMirror())
						{
							Mirror(x,y);
						}
					}
				}
			}
		}
	}
}

struct glitch_id
{
  double glitch;
  int x;
  int y;
};

static inline glitch_id min_glitch(const glitch_id &a, const glitch_id &b)
{
  if (a.glitch < b.glitch) return a; else return b;
}

struct TH_FIND_CENTER
{
  CFraktalSFT *p;
  int nXStart;
  int nXStop;
  glitch_id glitch;
  int64_t count;
};

static int ThFindCenterOfGlitch(TH_FIND_CENTER *pMan)
{
	pMan->p->FindCenterOfGlitch(pMan->nXStart, pMan->nXStop, 0, pMan->p->GetImageHeight(), pMan);
	return 0;
}

void CFraktalSFT::FindCenterOfGlitch(int x0, int x1, int y0, int y1, TH_FIND_CENTER *p)
{
	int mode = GetGlitchCenterMethod();
	glitch_id us = { 1.0 / 0.0, -1, -1 };
	int64_t count = 0;
	for (int x = x0; x < x1; ++x)
	{
		if (m_bStop) {
			p->count = 0;
			return;
		}
			
		for (int y = y0; y < y1; ++y)
		{
			float t = m_nTrans[x][y];
			if (GET_TRANS_GLITCH(t))
			{
				count++;
				if (mode == 1) // argmin|z|
				{
					glitch_id me = { t, x, y };
					us = min_glitch(us, me);
				}
				else // mode = 2 // random
				{
					double random = dither(uint32_t(x), uint32_t(y), uint32_t(m_bAddReference));
					glitch_id me = { random, x, y };
					us = min_glitch(us, me);
				}
			}
		}
	}
	p->glitch = us;
	p->count = count;
}

// old method works better, so don't use the gradient descent for now...
#undef KF_CENTER_VIA_TRANS
int CFraktalSFT::FindCenterOfGlitch(int &ret_x, int &ret_y)
{
#ifdef KF_OPENCL
	if (cl && GetUseOpenCL())
	{
		// OpenCL has already ignored isolated glitches
		if (0 != GetGlitchCenterMethod())
		{
			// OpenCL has already selected a new reference
			if (m_OpenCL_Glitched)
			{
				ret_x = m_OpenCL_Glitched_X;
				ret_y = m_OpenCL_Glitched_Y;
				return m_OpenCL_Glitched_Count;
			}
			else
			{
				ret_x = -1;
				ret_y = -1;
				return 0;
			}
		}
		else
		{
			// arrays have already been downloaded from OpenCL
			// for processing below
		}
	}
	else
#endif
	{
		// regular CPU path
		IgnoreIsolatedGlitches();
	}
	if (1 == GetGlitchCenterMethod() || 2 == GetGlitchCenterMethod())
	{
		SYSTEM_INFO sysinfo;
		GetSystemInfo(&sysinfo);
		int nParallel = GetThreadsPerCore() * sysinfo.dwNumberOfProcessors - GetThreadsReserveCore();
		if (nParallel < 1) nParallel = 1;
		TH_FIND_CENTER *pMan = new TH_FIND_CENTER[nParallel];
		CParallell P(nParallel);
		int nXStart = 0;
		int nXStep = (m_nX + nParallel - 1) / nParallel;
		for (int i = 0; i < nParallel; i++)
		{
			pMan[i].p = this;
			pMan[i].nXStart = nXStart;
			nXStart += nXStep;
			if (nXStart > m_nX) nXStart = m_nX;
			pMan[i].nXStop = nXStart;
			P.AddFunction((LPEXECUTE) ThFindCenterOfGlitch, &pMan[i]);
		}
		P.Execute();
		P.Reset();
		glitch_id us = { 1.0 / 0.0, -1, -1 };
		int64_t count = 0;
		for (int i = 0; i < nParallel; ++i)
		{
			count += pMan[i].count;
			us = min_glitch(us, pMan[i].glitch);
		}

		delete[] pMan;
		if (count > 0)
		{
			ret_x = us.x;
			ret_y = us.y;
			return count + 1;
		}
		else
		{
			ret_x = -1;
			ret_y = -1;
			return 0;
		}
	}
	else
	{
		int x, y, i=0, io;
		int rx = -1, ry = -1;

		itercount_array &Pixels = m_nPixels;

		uint32_t *lsb = new uint32_t[m_nX * m_nY];
		uint32_t *msb = m_nPixels_MSB ? new uint32_t[m_nX * m_nY] : nullptr;
		memcpy(lsb, m_nPixels_LSB, sizeof(*lsb) * m_nX * m_nY);
		if (msb)
			memcpy(msb, m_nPixels_MSB, sizeof(*msb) * m_nX * m_nY);
		itercount_array Node(m_nY, 1, lsb, msb);

		int nDistance=-1;

		int nHeight = m_nY;
		if(GetMirror())
			nHeight=(nHeight+1)/2;

		int nQSize = m_nX*m_nY;
		if(nQSize>230400)
			nQSize=230400;
		POINT *pQ = new POINT[nQSize];
		for(x=0;x<m_nX;x++){
			if (m_bStop)
				return 0;

			for(y=0;y<nHeight;y++){
				int nDone = - (x*m_nY+y);
				if(Node[x][y]>0 && GET_TRANS_GLITCH(m_nTrans[x][y]) && Pixels[x][y]!=m_nMaxIter){
					itercount_array invalid(0, 0, nullptr, nullptr);
					int nDist = GetArea(Node,x,y,1,invalid,nDone, pQ, nQSize);
					if(nDistance<nDist){
						nDistance=nDist;
						rx=x;
						ry=y;
					}
				}
			}
		}
		delete[] pQ;
		pQ = nullptr;

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
					if (m_bStop)
						return 0;
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
					if (m_bStop)
						return 0;
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
				m_P.Init(m_nX, m_nY, m_bInteractive);
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
		delete[] lsb;
		delete[] msb;
		return nDistance + 1; // -1 becomes 0
	}
}
#undef KF_CENTER_VIA_TRANS

int CFraktalSFT::GetColorIndex(int x, int y)
{
	if (x<0 || x >= m_nX || y<0 || y >= m_nY || !m_nPixels)
		return -1;
	return (int64_t)floor(m_nPixels[x][y] / m_nIterDiv) % 1024;
}

void CFraktalSFT::SaveMap(const std::string &szFile)
{
	if (!m_nPixels)
		return;
	std::ofstream hFile(szFile);
	int x, y;
	for (y = 0; y<m_nY; y++){
		for (x = 0; x<m_nX; x++) {
			hFile << m_nPixels[x][y] << std::endl;
		}
	}
	hFile << "Colors: ";
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
	hFile << szC << std::endl;
	stColors.DeleteToText(szC);
	hFile.close();
}
void CFraktalSFT::SaveMapB(const std::string &szFile)
{
	// WONTFIX doesn't save m_nPixels_MSB
	if (!m_nPixels_LSB)
		return;
	std::ofstream hFile(szFile, std::ios::out | std::ios::binary | std::ios::trunc);
	if(!hFile) return;

	int x;
	hFile.write("KFB", 3);
	hFile.write(reinterpret_cast<char*>(&m_nX), sizeof(m_nX));
	hFile.write(reinterpret_cast<char*>(&m_nY), sizeof(m_nY));
	hFile.write(reinterpret_cast<char*>(m_nPixels_LSB), sizeof(*m_nPixels_LSB) * m_nX * m_nY);
	int div = m_nIterDiv;
	hFile.write(reinterpret_cast<char*>(&div), sizeof(div));
	hFile.write(reinterpret_cast<char*>(&m_nParts), sizeof(m_nParts));
	hFile.write(reinterpret_cast<char*>(m_cKeys), sizeof(COLOR14)*m_nParts);
	hFile.write(reinterpret_cast<char*>(&m_nMaxIter), sizeof(int));
	for (x = 0; x<m_nX; x++)
		hFile.write(reinterpret_cast<char*>(m_nTrans[x]), m_nY*sizeof(float));
	if (GetDerivatives() && m_nDEx)
	{
		float *column = new float[m_nY];
		for (x = 0; x<m_nX; x++)
		{
			for (int y = 0; y < m_nY; ++y)
				column[y] = std::hypot(m_nDEx[x][y], m_nDEy[x][y]);
			hFile.write(reinterpret_cast<char*>(column), m_nY*sizeof(float));
		}
		delete[] column;
	}
	hFile.close();
}

BailoutRadiusPreset CFraktalSFT::GetBailoutRadiusPreset()
{
	return m_nBailoutRadiusPreset;
}
void CFraktalSFT::SetBailoutRadiusPreset(int nBailoutRadiusPreset)
{
	switch (nBailoutRadiusPreset)
	{
		default:
		case 0:
			m_nBailoutRadiusPreset = BailoutRadius_High;
			break;
		case 1:
			m_nBailoutRadiusPreset = BailoutRadius_2;
			break;
		case 2:
			m_nBailoutRadiusPreset = BailoutRadius_Low;
			break;
		case 3:
			m_nBailoutRadiusPreset = BailoutRadius_Custom;
			break;
	}
}
double CFraktalSFT::GetBailoutRadiusCustom()
{
	return m_nBailoutRadiusCustom;
}
void CFraktalSFT::SetBailoutRadiusCustom(double nBailoutRadiusCustom)
{
	m_nBailoutRadiusCustom = nBailoutRadiusCustom;
}
double CFraktalSFT::GetBailoutRadius()
{
	switch (GetBailoutRadiusPreset())
	{
		default:
		case BailoutRadius_High: return SMOOTH_BAILOUT;
		case BailoutRadius_2: return 2;
		case BailoutRadius_Low: return pow(2.0, 1.0 / (GetPower() - 1));
		case BailoutRadius_Custom: return GetBailoutRadiusCustom();
	}
}
void CFraktalSFT::SetBailoutNormPreset(int nBailoutNormPreset)
{
	switch (nBailoutNormPreset)
	{
		case 0:
			m_nBailoutNormPreset = BailoutNorm_1;
			break;
		default:
		case 1:
			m_nBailoutNormPreset = BailoutNorm_2;
			break;
		case 2:
			m_nBailoutNormPreset = BailoutNorm_Infinity;
			break;
		case 3:
			m_nBailoutNormPreset = BailoutNorm_Custom;
			break;
	}
}
double CFraktalSFT::GetBailoutNorm()
{
	switch (GetBailoutNormPreset())
	{
		case BailoutNorm_1: return 1;
		default:
		case BailoutNorm_2: return 2;
		case BailoutNorm_Infinity: return 1.0/0.0;
		case BailoutNorm_Custom: return GetBailoutNormCustom();
	}
}

floatexp CFraktalSFT::GetBailoutSmall()
{
	return 1e-12;
}

void CFraktalSFT::SetPower(int nPower)
{
	if (nPower<2)
		nPower = 2;
	if (nPower>70)
		nPower = 70;
	SetSmoothMethod(m_nSmoothMethod); // update bailout if necessary

	if (m_nPower != nPower) {
        if (m_pnExpConsts){
            delete[] m_pnExpConsts;
            m_pnExpConsts = NULL;
        }
        if (nPower > 10) {
            // compute Pascal triangle numbers.
			// For n<=10 these are hardcoded into the requisite formula(s).
            m_pnExpConsts = new int[nPower + 1];
            m_pnExpConsts[0] = 1;
            int i,k;
            for(i=1;i<=nPower;i++) {
                m_pnExpConsts[i] = 1;
                for (k=i-1;k>0;k--)
                    m_pnExpConsts[k] += m_pnExpConsts[k-1];
            }
        }

	}

	m_nPower = nPower;
}

void CFraktalSFT::SetDifferences(int nDifferences)
{
	if (nDifferences < 0) nDifferences = 0;
	if (nDifferences > 7) nDifferences = 0;
	m_nDifferences = Differences(nDifferences);
}
Differences CFraktalSFT::GetDifferences()
{
	return m_nDifferences;
}

void CFraktalSFT::SetColorMethod(int nColorMethod)
{
	if (nColorMethod < 0) nColorMethod = 0;
	if (nColorMethod > 11) nColorMethod = 0;
	m_nColorMethod = ColorMethod(nColorMethod);
}
ColorMethod CFraktalSFT::GetColorMethod()
{
	return m_nColorMethod;
}

void CFraktalSFT::ErasePixel(int x, int y)
{
	if (x >= 0 && y >= 0 && x<m_nX && y<m_nY){
		m_nPixels[x][y] = 1;
		m_nTrans[x][y] = 0;
		m_nPhase[x][y] = 0;
		m_nDEx[x][y] = 0;
		m_nDEy[x][y] = 0;
		SetColor(x, y, 1, 1);
		m_nPixels[x][y] = PIXEL_UNEVALUATED;
	}
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
int64_t CFraktalSFT::GetMaxExceptCenter()
{
	int64_t nMax = 0;
	int x, y;
	int nXO = m_nX / 2 - 2;
	int nYO = m_nY / 2 - 2;
	for (x = 0; x<m_nX; x++) {
		if (x >= nXO && x <= m_nX - nXO)
			continue;
		for (y = 0; y<m_nY; y++) {
			if (y >= nYO && y <= m_nY - nYO)
				continue;
			if (nMax < m_nPixels[x][y])
				nMax = m_nPixels[x][y];
		}
	}
	return nMax;
}
void CFraktalSFT::SetFractalType(int nFractalType)
{
	if (nFractalType < 0 || nFractalType > 102)
		nFractalType = 0;
	m_nFractalType = nFractalType;
	if ((1 <= m_nFractalType && m_nFractalType <= 4) && !(2 <= m_nPower && m_nPower <= 5))
		m_nPower = 2;
	if (m_nFractalType>2 && m_nPower>2)
		m_nPower = 2;

	SetReferenceStrictZero(nFractalType != 0);
}

void CFraktalSFT::SetApproxTerms(int64_t nTerms)
{
	m_Settings.SetApproxTerms(nTerms);
	int m_nTerms = GetApproxTerms();
	delete[] m_APr;
	delete[] m_APi;
	m_APr = new floatexp[m_nTerms];
	m_APi = new floatexp[m_nTerms];
}

void CFraktalSFT::SetHalfColour(bool b)
{
	m_Settings.SetHalfColour(b);
	b = m_Settings.GetHalfColour();
	if (b)
	{
		if (! m_imageHalf)
		{
			m_imageHalf = new half[m_nSizeImage];
		}
	}
	else
	{
		if (m_imageHalf)
		{
			delete[] m_imageHalf;
			m_imageHalf = nullptr;
		}
	}
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
	m_bTexture = bTexture;
	m_nImgMerge = nImgMerge;
	m_nImgPower = nImgPower;
	m_nImgRatio = nImgRatio;
	m_szTexture = szTexture;
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
			SetUseOpenCL(false);
		}
		if (0 <= i && i < (int) m_cldevices.size())
		{
			clid = i;
			cl = new OpenCL(&cl_error, m_cldevices[i].pid, m_cldevices[i].did, m_cldevices[i].supports_double);
			SetUseOpenCL(true);
			SetOpenCLPlatform(i);
		}
	}
}
#endif


void CFraktalSFT::OutputIterationData(int x, int y, int w, int h, bool bGlitch, int64_t antal, double test1, double smooth, double phase, const complex<double> &de)
{
	int64_t antal0 = antal;
	if (std::isnan(smooth) || std::isinf(smooth)) smooth = 0;
	double i = antal + smooth;
	antal = std::floor(i);
	double t = i - antal;
	if (antal >= m_nMaxIter || antal0 >= m_nMaxIter){
		m_nPixels[x][y] = m_nMaxIter;
		m_nTrans[x][y] = 0;
		if (m_nPhase)
			m_nPhase[x][y] = 0;
		if (m_nDEx)
			m_nDEx[x][y] = 0;
		if (m_nDEy)
			m_nDEy[x][y] = 0;
	}
	else{
		if (x == m_nAddRefX && y == m_nAddRefY)
		{
			// never consider the pixel of the reference to be glitched
			bGlitch = false;
		}
		double de_multiplier = 1;
		if (GetExponentialMap())
		{
			double dx, dy;
			GetPixelOffset(x, y, dx, dy);
			double v = (y + dy) / m_nY;
			de_multiplier = std::exp2(v);
		}
		m_nPixels[x][y] = antal;
		m_nTrans[x][y] = 1 - t;
		if (m_nPhase)
			m_nPhase[x][y] = phase;
		if (m_nDEx)
			m_nDEx[x][y] = de.m_r * de_multiplier;
		if (m_nDEy)
			m_nDEy[x][y] = de.m_i * de_multiplier;
		if (bGlitch && !m_bNoGlitchDetection){
			m_nTrans[x][y] = SET_TRANS_GLITCH(test1);
		}
	}
	SetColor(x, y, w, h);
	if (GetMirror())
		Mirror(x, y);
}

void CFraktalSFT::OutputIterationData(int x, int y, int w, int h, bool bGlitch, int64_t antal, double test1, double test2, double phase, double nBailout, const complex<double> &de, int power)
{
	double smooth = 0;
	if (!bGlitch && (m_nSmoothMethod == SmoothMethod_Sqrt)){
		double p = GetBailoutNorm();
		if (! (p < 1.0 / 0.0)) p = 1;
		double div = pow(test1, 1 / p) - pow(test2, 1 / p);
		if (div != 0)
			smooth = 1 - (pow(test1, 1 / p) - nBailout) / div;
	}
	else if (!bGlitch && m_nSmoothMethod == SmoothMethod_Log){
		smooth = 1 - log(log(sqrt(test1)) / log(GetBailoutRadius())) / log((double) power);
	}
	if (!ISFLOATOK(smooth))
		smooth = 0;
	OutputIterationData(x, y, w, h, bGlitch, antal, test1, smooth, phase, de);
}

void CFraktalSFT::OutputPixelData(int x, int y, int w, int h, bool bGlitch)
{
		if ((!bGlitch || GetShowGlitches()) && ! m_bInhibitColouring && ! GetUseOpenGL())
    {
      int nIndex = x * BM_WIDTH + (m_bmi->biHeight - 1 - y) * m_row;
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
              if (m_nPixels[x2][y2] == PIXEL_UNEVALUATED)
              {
                int index2 = x2 * BM_WIDTH + (m_bmi->biHeight - 1 - y2) * m_row;
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

Guess CFraktalSFT::GuessPixel(int x, int y, int x0, int y0, int x1, int y1)
{
	if ( 0 <= x && x < m_nX && 0 <= x0 && x0 < m_nX && 0 <= x1 && x1 < m_nX &&
	     0 <= y && y < m_nY && 0 <= y0 && y0 < m_nY && 0 <= y1 && y1 < m_nY &&
	     m_nPixels[x0][y0] != PIXEL_UNEVALUATED &&
	     m_nPixels[x0][y0] == m_nPixels[x1][y1] &&
	     GET_TRANS_GLITCH(m_nTrans[x0][y0]) == GET_TRANS_GLITCH(m_nTrans[x1][y1]) &&
	     // only guess glitches or interior, not escaped exterior
	     ( GET_TRANS_GLITCH(m_nTrans[x0][y0]) || (m_nPixels[x0][y0] >= m_nMaxIter) ) &&
	      // never guess reference, in case we guess it to be glitched
	      // (prevent possible infinite loop in next reference selection)
	     x != m_nAddRefX && y != m_nAddRefY
	   )
	{
		// NOTE cast to int64_t is required to avoid copying just the ref!
		m_nPixels[x][y] = int64_t(m_nPixels[x0][y0]);
		m_nTrans[x][y] = (m_nTrans[x0][y0] + m_nTrans[x1][y1])*.5;
		if (m_nPhase)
		{
			m_nPhase[x][y] = atan2
			  ( sin(m_nPhase[x0][y0] * M_PI * 2) + sin(m_nPhase[x1][y1] * M_PI * 2)
			  , cos(m_nPhase[x0][y0] * M_PI * 2) + cos(m_nPhase[x1][y1] * M_PI * 2)
			  ) / M_PI / 2;
			m_nPhase[x][y] -= floor(m_nPhase[x][y]);
		}
		// use geometric mean for directional DE guessing
#ifdef KF_GUESS_DE_GEOMETRIC
		complex<float> de0(m_nDEx[x0][y0], m_nDEy[x0][y0]);
		complex<float> de1(m_nDEx[x1][y1], m_nDEy[x1][y1]);
		complex<float> deA = 0.5*(de0 + de1);
		complex<float> deG = sqrt(de0 * de1);
		complex<float> de = (norm(deA - deG) < norm(deA + deG)) ? deG : -1.0*deG;
#else
		m_nDEx[x][y] = 0.5 * (m_nDEx[x0][y0] + m_nDEx[x1][y1]);
		m_nDEy[x][y] = 0.5 * (m_nDEy[x0][y0] + m_nDEy[x1][y1]);
#endif
		int nIndex  = x  * BM_WIDTH + (m_bmi->biHeight - 1 - y )*m_row;
		int nIndex0 = x0 * BM_WIDTH + (m_bmi->biHeight - 1 - y0)*m_row;
		int nIndex1 = x1 * BM_WIDTH + (m_bmi->biHeight - 1 - y1)*m_row;
		m_lpBits[nIndex    ] = (m_lpBits[nIndex0    ] + m_lpBits[nIndex1    ]) / 2;
		m_lpBits[nIndex + 1] = (m_lpBits[nIndex0 + 1] + m_lpBits[nIndex1 + 1]) / 2;
		m_lpBits[nIndex + 2] = (m_lpBits[nIndex0 + 2] + m_lpBits[nIndex1 + 2]) / 2;
		if (GetMirror())
			Mirror(x, y);
		return GET_TRANS_GLITCH(m_nTrans[x0][y0]) ? Guess_Glitch : Guess_Interior;
	}
	return Guess_No;
}

Guess CFraktalSFT::GuessPixel(int x, int y, int w, int h)
{
	Guess g = Guess_No;
	if (GetGuessing())
	{
		if (w == 1 && h <= 2)
			if ((g = GuessPixel(x, y, x - 1, y    , x + 1, y    ))) return g;
		if (w == 1 && h == 1) {
			if ((g = GuessPixel(x, y, x    , y - 1, x    , y + 1))) return g;
			if ((g = GuessPixel(x, y, x - 1, y - 1, x + 1, y + 1))) return g;
			if ((g = GuessPixel(x, y, x - 1, y + 1, x + 1, y - 1))) return g;
	  }
	}
	return Guess_No;
}

void CFraktalSFT::GetPixelOffset(const int i, const int j, double &x, double &y) const
{
	int c = GetJitterSeed();
	if (c)
	{
		double s = GetJitterScale();
		double u = dither(i, j, 2 * c + 0);
		double v = dither(i, j, 2 * c + 1);
		switch (GetJitterShape())
		{
			default:
			case 0: // uniform
				{
					x = s * (u - 0.5);
					y = s * (v - 0.5);
				}
				break;
			case 1: // Gaussian
				{
					// FIXME cache slow trig functions for every pixel for every image?
					// https://en.wikipedia.org/wiki/Box%E2%80%93Muller_transform
					double r = 0 < u && u < 1 ? sqrt(-2 * log(u)) : 0;
					double t = 2 * 3.141592653589793 * v;
					s *= 0.5;
					x = s * r * cos(t);
					y = s * r * sin(t);
				}
				break;
		}
	}
	else
	{
		x = 0.0;
		y = 0.0;
	}
}

void CFraktalSFT::GetPixelCoordinates(const int i, const int j, floatexp &x, floatexp &y) const
{
	double di = 0;
	double dj = 0;
	GetPixelOffset(i, j, di, dj);
	double u0 = i + di;
	double v0 = j + dj;
	if (GetExponentialMap())
	{
		double re = -0.6931471805599453 * v0 / m_nY; // log 2
		double im = 6.283185307179586 * u0 / m_nX; // 2 pi
		double R = 0.5 * std::hypot(m_nX, m_nY);
		double r = std::exp(re);
		double c = std::cos(im);
		double s = std::sin(im);
		u0 = R * r * c;
		v0 = R * r * s;
	}
	else
	{
		u0 -= m_nX / 2;
		v0 -= m_nY / 2;
	}
	mat2 m = GetTransformMatrix();
	x = m_pixel_center_x + m_pixel_scale * (m[0][0] * u0 + m[0][1] * v0);
	y = m_pixel_center_y + m_pixel_scale * (m[1][0] * u0 + m[1][1] * v0);
}

void CFraktalSFT::GetPixelCoordinates(const int i, const int j, floatexp &x, floatexp &y, floatexp &daa, floatexp &dab, floatexp &dba, floatexp &dbb) const
{
	double di = 0;
	double dj = 0;
	GetPixelOffset(i, j, di, dj);
	dual<2, double> u0(i + di); u0.dx[0] = 1;
	dual<2, double> v0(j + dj); v0.dx[1] = 1;
	if (GetExponentialMap())
	{
		double re = -0.6931471805599453 * v0.x / double(m_nY); // log 2
		double im = 6.283185307179586 * u0.x / double(m_nX); // 2 pi
		double R = 0.5 * std::hypot(m_nX, m_nY);
		double r = exp(re);
		double c = cos(im);
		double s = sin(im);
		u0.x = R * r * c;
		v0.x = R * r * s;
		double d = std::exp2((j + dj) / m_nY);
		u0.dx[0] *= d;
		v0.dx[1] *= d;
	}
	else
	{
		u0 -= m_nX / 2;
		v0 -= m_nY / 2;
	}
	mat2 m = GetTransformMatrix();
	dual<2, floatexp> x0 = m_pixel_center_x + m_pixel_scale * dual<2, floatexp>(m[0][0] * u0 + m[0][1] * v0);
	dual<2, floatexp> y0 = m_pixel_center_y + m_pixel_scale * dual<2, floatexp>(m[1][0] * u0 + m[1][1] * v0);
	x = x0.x;
	y = y0.x;
	daa = x0.dx[0];
	dab = x0.dx[1];
	dba = y0.dx[0];
	dbb = y0.dx[1];
}

void CFraktalSFT::SetTransformPolar(const polar2 &P)
{
	m_TransformPolar = P;
	m_TransformMatrix = polar_composition(P);
}

void CFraktalSFT::SetTransformMatrix(const mat2 &M)
{
	m_TransformMatrix = M;
	m_TransformPolar = polar_decomposition(M);
}

Reference_Type CFraktalSFT::GetReferenceType(int64_t e) const
{
	NumberType n = GetNumberTypes();
	bool scalable = GetUseHybridFormula() ? true : scaling_supported(GetFractalType(), GetPower());
#ifdef KF_OPENCL
	bool supports_long_double = ! cl;
	bool supports_double = cl ? cl->supports_double : true;
#else
	bool supports_long_double = true;
	bool supports_double = true;
#endif
	if (! (e > DOUBLE_THRESHOLD_DEFAULT) && n.Single) { return Reference_Float; }
	if (scalable && n.RescaledSingle) { return Reference_ScaledFloat; }
	if (supports_double && ! (e > LONG_DOUBLE_THRESHOLD_DEFAULT) && n.Double) { return Reference_Double; }
	if (supports_double && scalable && n.RescaledDouble) { return Reference_ScaledDouble; }
	if (supports_long_double && ! (e > FLOATEXP_THRESHOLD_DEFAULT) && n.LongDouble) { return Reference_LongDouble; }
	if (n.FloatExpSingle) { return Reference_FloatExpFloat; }
	if (supports_double && n.FloatExpDouble) { return Reference_FloatExpDouble; }
	return Reference_FloatExpFloat; // FIXME fallback
}

void CFraktalSFT::ResetTimers()
{
	m_timer_total_wall_start = get_wall_time();
	m_timer_total_cpu_start = get_cpu_time();
	m_timer_reference_wall = 0;
	m_timer_reference_cpu = 0;
	m_timer_approximation_wall = 0;
	m_timer_approximation_cpu = 0;
	m_timer_perturbation_wall = 0;
	m_timer_perturbation_cpu = 0;
}

void CFraktalSFT::GetTimers(double *total_wall, double *total_cpu, double *reference_wall, double *reference_cpu, double *approximation_wall, double *approximation_cpu, double *perturbation_wall, double *perturbation_cpu)
{
	if (total_wall) *total_wall = get_wall_time() - m_timer_total_wall_start;
	if (total_cpu) *total_cpu = get_cpu_time() - m_timer_total_cpu_start;
	if (reference_wall) *reference_wall = m_timer_reference_wall;
	if (reference_cpu) *reference_cpu = m_timer_reference_cpu;
	if (approximation_wall) *approximation_wall = m_timer_approximation_wall;
	if (approximation_cpu) *approximation_cpu = m_timer_approximation_cpu;
	if (perturbation_wall) *perturbation_wall = m_timer_perturbation_wall;
	if (perturbation_cpu) *perturbation_cpu = m_timer_perturbation_cpu;
}
