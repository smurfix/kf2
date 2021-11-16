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

#ifndef KF_FRAKTAL_SFT_H
#define KF_FRAKTAL_SFT_H 1

#include <windows.h>
#include <half.h>

#include <atomic>

#include "Settings.h"
#include "CFixedFloat.h"
#include "CDecNumber.h"
#include "complex.h"
#include "floatexp.h"
#include "../common/matrix.h"
#include "itercount_array.h"
#include "colour.h"
#include "hybrid.h"
#include "main_numbertype.h"

struct Reference;
struct NanoMB1_Reference;
struct NanoMB2_Reference;

struct CPixel;
class CPixels
{
	int m_nX;
	int m_nY;
	int m_nY2;
	CPixel *m_pPixels;
	int m_nPixels;
	LONG m_nNextPixel;
	HANDLE m_hMutex;
public:
	CPixels();
	void Init(int nX, int nY, bool interactive);
	BOOL GetPixel(int &x, int &y, int &w, int &h, BOOL bMirrored = 0);
#if 0
	BOOL GetPixels(int *px, int *py, int &nCount);
#endif
};

// magic value stored in m_nPixels[][] when pixel needs (re)computation
#define PIXEL_UNEVALUATED INT_MIN
// magic value stored in m_nTrans[][] when a glitch is detected
#if 1
#define SET_TRANS_GLITCH(x) (((x > 0) ? log2(x) : -1024.0) - 2048.0)
#else
#define SET_TRANS_GLITCH(x) (-1.0)
#endif
#define GET_TRANS_GLITCH(x) ((x) < 0.0f)

// these are smaller than expected due to risk of derivative overflow
// thresholds for switching to floatexp iterations
#define FLOATEXP_THRESHOLD_DEFAULT 4900
// thresholds for switching to long double iterations
#define LONG_DOUBLE_THRESHOLD_DEFAULT 290
// threshold for switching to double iterations
#define DOUBLE_THRESHOLD_DEFAULT 20

#define SMOOTH_BAILOUT 10000
struct MC
{
	CFixedFloat *xr, *xi, *sr, *si, *xrxid;
	HANDLE hDone;
	HANDLE hWait;
	HANDLE hExit;
	int nType;
};
struct MC2
{
	CFixedFloat *xrn, *xin, *xrxid, *sr, *si, *m_iref, *m_rref;
	HANDLE hDone;
	HANDLE hWait;
	HANDLE hExit;
	int nType;
};


struct COLOR14 { unsigned char r, g, b; };

#if 0
typedef long double ldbl;

struct ldblexp {
	ldbl val;
	__int64 exp;
};
#endif

#define MULTIWAVE_MAX 30
struct MULTIWAVE
{
	int nPeriod;
	int nStart;
	int nType;
};

#if 0
template <class T> class CFileFloat
{
	HANDLE m_hFile;
	int m_nSize;
	int m_nStartPos;
	T *m_dbBuf;
	int m_nViewSize;
	char m_szFileName[256];
public:
	CFileFloat(int nSize)
	{
		m_nViewSize = 1000;
		m_dbBuf = new T[m_nViewSize];
		memset(m_dbBuf,0,m_nViewSize*sizeof(T));
		m_nSize = nSize;
		m_nStartPos=0;
		GetTempPath(sizeof(m_szFileName),m_szFileName);
		if(m_szFileName[strlen(m_szFileName)-1]!='\\')
			strcat(m_szFileName,"\\");
		do{
			wsprintf(strrchr(m_szFileName,'\\')+1,"%d",GetTickCount());
			m_hFile = CreateFile(m_szFileName,GENERIC_READ|GENERIC_WRITE,0,NULL,CREATE_NEW,0,NULL);
		}while(m_hFile==INVALID_HANDLE_VALUE);
		int nData=0;
		DWORD dw;
		while(nData<nSize){
			WriteFile(m_hFile,m_dbBuf,m_nViewSize*sizeof(T),&dw,NULL);
			nData+=m_nViewSize;
		}
		SetFilePointer(m_hFile,0,0,FILE_BEGIN);
	}
	~CFileFloat()
	{
		CloseHandle(m_hFile);
		DeleteFile(m_szFileName);
		delete m_dbBuf;
	}
	T &operator[] (int nIndex)
	{
		int nMapIndex = nIndex-m_nStartPos;
		int nPrevStart = m_nStartPos;
		BOOL bRemap=FALSE;
		while(nMapIndex<0){
			m_nStartPos-=m_nViewSize;
			nMapIndex = nIndex-m_nStartPos;
			bRemap=TRUE;
		}
		while(nMapIndex>=m_nViewSize){
			m_nStartPos+=m_nViewSize;
			nMapIndex = nIndex-m_nStartPos;
			bRemap=TRUE;
		}
		if(bRemap){
			__int64 nStartPos = (__int64)nPrevStart*sizeof(T);
			long *pPos = (long *)&nStartPos;
			SetFilePointer(m_hFile,pPos[0],&pPos[1],FILE_BEGIN);
			DWORD dw;
			WriteFile(m_hFile,m_dbBuf,m_nViewSize*sizeof(T),&dw,NULL);
			nStartPos = (__int64)m_nStartPos*sizeof(T);
			pPos = (long *)&nStartPos;
			SetFilePointer(m_hFile,pPos[0],&pPos[1],FILE_BEGIN);
			ReadFile(m_hFile,m_dbBuf,m_nViewSize*sizeof(T),&dw,NULL);
		}
		return m_dbBuf[nMapIndex];
	}
};
#endif

enum SmoothMethod
{
	SmoothMethod_Log = 0,
	SmoothMethod_Sqrt = 1
};

enum BailoutRadiusPreset
{
	BailoutRadius_High = 0,
	BailoutRadius_2 = 1,
	BailoutRadius_Low = 2,
	BailoutRadius_Custom = 3
};

enum BailoutNormPreset
{
	BailoutNorm_1 = 0,
	BailoutNorm_2 = 1,
	BailoutNorm_Infinity = 2,
	BailoutNorm_Custom = 3
};

enum ColorMethod
{
	ColorMethod_Standard = 0,
	ColorMethod_SquareRoot = 1,
	ColorMethod_CubicRoot = 2,
	ColorMethod_Logarithm = 3,
	ColorMethod_Stretched = 4,
	ColorMethod_DistanceLinear = 5,
	ColorMethod_DEPlusStandard = 6,
	ColorMethod_DistanceLog = 7,
	ColorMethod_DistanceSqrt = 8,
	ColorMethod_LogLog = 9,
	ColorMethod_ATan = 10,
	ColorMethod_FourthRoot = 11
};

enum Differences
{
	Differences_Traditional = 0,
	Differences_Forward3x3 = 1,
	Differences_Central3x3 = 2,
	Differences_Diagonal2x2 = 3,
	Differences_LeastSquares2x2 = 4,
	Differences_LeastSquares3x3 = 5,
	Differences_Laplacian3x3 = 6,
	Differences_Analytic = 7
};

enum Guess
{
	Guess_No = 0,
	Guess_Interior = 1,
	Guess_Glitch = 2
};

enum SeriesType
{
	SeriesType_None = 0,
	SeriesType_Complex = 1,
	SeriesType_Real = 2
};

template<typename mantissa, typename exponent>
struct SeriesR2
{
	tfloatexp<mantissa, exponent> s[MAX_APPROX_TERMS+1][MAX_APPROX_TERMS+1];
	tfloatexp<mantissa, exponent> t[MAX_APPROX_TERMS+1][MAX_APPROX_TERMS+1];
};


struct TextureParams
{
	BOOL m_bTexture;
	std::string m_szTexture;
	double m_nImgPower;
	int m_nX;
	int m_nY;
	bool m_bTextureResize;
};

struct TH_FIND_CENTER;

#ifdef KF_OPENCL
#include "../cl/opencl.h"
extern std::vector<cldevice> cldevices;
#endif

class CFraktalSFT
{
	Settings m_Settings;

	MULTIWAVE m_MW[MULTIWAVE_MAX];
	int m_nMW;
	BOOL m_bMW;
	BOOL m_bBlend;
	HANDLE m_hMutex;
	CPixels m_P;
	CFixedFloat m_CenterRe, m_CenterIm, m_ZoomRadius, m_rref, m_iref;
	double m_dPixelSpacing;
	long double m_lPixelSpacing;
	floatexp m_fPixelSpacing;
	double m_epsilon;
	CFixedFloat m_storedr, m_storedi;
	POINT m_pOldGlitch[OLD_GLITCH];
	int m_nSizeImage;
	int m_nZoom;
	int64_t m_nMaxIter;
	int m_nTotal;
	HBITMAP m_bmBmp;
	COLOR14 m_cPos[1025], m_cKeys[1025], m_cInterior;
	int m_nParts;
	int m_nSeed;
	uint32_t *m_nPixels_LSB;
	uint32_t *m_nPixels_MSB; // may be nullptr
	itercount_array m_nPixels; // FIXME
	BOOL m_bFlat;
	BOOL m_bTrans;
	BOOL m_bITrans;
	float **m_nTrans;
	float **m_nPhase;
	float **m_nDEx;
	float **m_nDEy;
	BOOL m_bNoGlitchDetection;
	int m_nPower;
	int m_nPrevPower;
	int *m_pnExpConsts;
	int m_nMaxOldGlitches;
	polar2 m_TransformPolar;
	mat2 m_TransformMatrix;
	BOOL m_bNoPostWhenDone;
	BOOL m_bSlopes;
	int m_nSlopePower;
	int m_nSlopeRatio;
	int m_nSlopeAngle;
	double m_nSlopeX, m_nSlopeY;
	double m_nZooms;
	bool m_bTriangleInequalityAverage;

	SmoothMethod m_nSmoothMethod;
	BailoutRadiusPreset m_nBailoutRadiusPreset;
	double m_nBailoutRadiusCustom;
	BailoutNormPreset m_nBailoutNormPreset;
	double m_nBailoutNormCustom;
	ColorMethod m_nColorMethod;
	Differences m_nDifferences;
	int m_nColorOffset;
	double m_nPhaseColorStrength;
	BOOL m_bIterChanged;
	int64_t m_nMinI, m_nMaxI;

	double m_xrd, m_xid;

	floatexp *m_APr;
	floatexp *m_APi;
	SeriesR2<double,int64_t> *m_APs;

	BOOL m_bMirrored;
	int m_nFractalType;

	double m_nIterDiv;
	int64_t m_nMaxApproximation;
	int64_t m_nApprox;
	RECT m_rApprox;

	floatexp m_pixel_center_x, m_pixel_center_y, m_pixel_scale;

	half *m_imageHalf; // for EXR export
	BYTE *m_lpBits;
	int m_row;
	BITMAPINFOHEADER *m_bmi;
	bool m_bResized;
	int m_nX, m_nXPrev;
	int m_nY, m_nYPrev;
	std::atomic<uint32_t> m_count_good_guessed, m_count_good, m_count_queued, m_count_bad, m_count_bad_guessed;
	double
	  m_timer_total_wall_start,
	  m_timer_total_cpu_start,
	  m_timer_reference_wall,
	  m_timer_reference_cpu,
	  m_timer_approximation_wall,
	  m_timer_approximation_cpu,
	  m_timer_perturbation_wall,
	  m_timer_perturbation_cpu;

	int64_t m_nRDone;
	bool m_bStop;
	char *m_szPosition;
	BOOL m_bReuseRef;
	int m_nStatus;
	int m_nFrameDone;
	BOOL m_bAddReference;
	BOOL m_bNoApproximation;

	BOOL m_bTexture;
	double m_nImgMerge;
	double m_nImgPower;
	int m_nImgRatio;
	std::string m_szTexture;
	bool m_bTextureResize;
	BYTE *m_lpTextureBits;
	BITMAPINFOHEADER m_bmiBkg;
	int m_rowBkg;
	TextureParams m_ActiveTextureParams;

	std::vector< complex<CFixedFloat> > m_Inflections;

	Reference *m_Reference;
	NanoMB1_Reference *m_NanoMB1Ref;
	NanoMB2_Reference *m_NanoMB2Ref;

	Reference *m_ReferenceReuse;
	CFixedFloat m_rrefReuse, m_irefReuse;

	SeriesType GetApproximationType()
	{
		if (GetUseHybridFormula())
			return SeriesType_None;
		if (m_nFractalType == 0)
			return SeriesType_Complex;
		if (m_nFractalType == 1 && m_nPower == 2 && ! GetUseOpenCL())
			return SeriesType_Real;
		return SeriesType_None;
	}
	void CalculateApproximation(int nType);
	void DoApproximation(int64_t &antal, const floatexp &D0r, const floatexp &D0i, floatexp &TDnr, floatexp &TDni, floatexp &TDDnr, floatexp &TDDni);
	void DoApproximation(int64_t &antal, const floatexp &a, const floatexp &b, floatexp &x, floatexp &y, floatexp &dxa, floatexp &dxb, floatexp &dya, floatexp &dyb);
	void DoApproximation(const floatexp &a, const floatexp &b, floatexp &x, floatexp &y); // for internal usage only, assumes isR
	void CalculateReference(enum Reference_Type reftype);
	bool CalculateReferenceThreaded();
	void CalculateReferenceNANOMB1();
	void CalculateReferenceNANOMB2();
	void CalcStart();
	void CreateLists();
	std::string ToZoom(const CDecNumber &z, int &zoom);
	void RenderFractalNANOMB1();
	void RenderFractalNANOMB2();
#ifdef KF_OPENCL
	void RenderFractalOpenCL(Reference_Type reftype);
#endif
	int GetArea(itercount_array &Node, int nXStart, int nXStop, int nEqSpan, itercount_array &Pixels, int nDone, POINT *pQ, int nQSize);

	void LoadTexture();
	void SetTexture(int nIndex, int x, int y, srgb &s);
	void SetColor(int nIndex, int64_t nIter, double offs = 0, int x = -1, int y = -1, int w = 1, int h = 1);
	void DeleteArrays();

#ifdef KF_OPENCL
	int clid;
  OpenCL *cl;
  bool m_OpenCL_Glitched;
  int m_OpenCL_Glitched_X;
  int m_OpenCL_Glitched_Y;
  int64_t m_OpenCL_Glitched_Count;
#endif

	std::vector<std::string> m_undo;
	std::vector<std::string> m_redo;

	bool m_bIsRendering;
	bool m_UseHybridFormula;
	hybrid_formula m_HybridFormula;

	bool m_bUseOpenGL;
	std::string m_sGLSL;
	std::string m_sGLSLLog;
	bool m_bGLSLChanged;
	bool m_bUseSRGB;

public:
	int m_opengl_major;
	int m_opengl_minor;
	BOOL m_bRunning;
	BOOL m_bInhibitColouring;
	bool m_bInteractive;
	int nPos;
	void MandelCalc(const Reference_Type reftype);
	void MandelCalcSIMD();
	template <typename mantissa> void MandelCalc1();
	template <typename mantissa, typename exponent> void MandelCalcScaled();
	void MandelCalcNANOMB1();
	void MandelCalcNANOMB2();
	void Done()
	{
#ifdef KF_OPENCL
		if (cl)
		{
			m_count_good_guessed = 0;
			m_count_good = m_nX * m_nY;
			m_count_queued = 0;
			m_count_bad = 0;
			m_count_bad_guessed = 0;
		}
#endif
	}
	HWND m_hWnd;

	CFraktalSFT();
	~CFraktalSFT();

	inline void SetWindow(HWND hWnd) { m_hWnd = hWnd; };

	void SetPosition(const CFixedFloat &re, const CFixedFloat &im, const CFixedFloat &radius, int nX, int nY);
	void SetPosition(const std::string &szR, const std::string &szI, const std::string &szZ);
	std::string ToZoom();
	void SetImageSize(int nx, int ny);
	void RenderFractal(int nX, int nY, int64_t nMaxIter, HWND hWnd, BOOL bNoThread = FALSE, BOOL bResetOldGlitch = TRUE);
	void RenderFractal();
	void CalcStart(int x0, int x1, int y0, int y1);
	HBITMAP GetBitmap();
	HBITMAP ShrinkBitmap(HBITMAP bmSrc,int nNewWidth,int nNewHeight,int mode = 1);
	void UpdateBitmap();
	int GetWidth();
	int GetHeight();
	void Stop();
	int CountFrames(int nProcent);
	void Zoom(double nZoomSize);
	void Zoom(int nXPos, int nYPos, double nZoomSize, int nWidth, int nHeight, BOOL bReuseCenter = FALSE, bool autoRender = true, bool center_view = false);
	BOOL Center(int &rx, int &ry, BOOL bSkipM = FALSE, BOOL bQuick = FALSE);
	double GetProgress(double *reference = nullptr, double *approximation = nullptr, double *good_guessed = nullptr, double *good = nullptr, double *queued = nullptr, double *bad = nullptr, double *bad_guessed = nullptr);
	void ResetTimers();
	void GetTimers(double *total_wall, double *total_cpu = nullptr, double *reference_wall = nullptr, double *reference_cpu = nullptr, double *approximation_wall = nullptr, double *approximation_cpu = nullptr, double *perturbation_wall = nullptr, double *perturbation_cpu = nullptr);
	std::string GetPosition();
	void GetIterations(int64_t &nMin, int64_t &nMax, int *pnCalculated = NULL, int *pnType = NULL, BOOL bSkipMaxIter = FALSE);
	int64_t GetIterations();
	void SetIterations(int64_t nIterations);
	std::string GetRe();
	std::string GetRe(int nXPos, int nYPos, int width, int height);
	std::string GetIm();
	std::string GetIm(int nXPos, int nYPos, int width, int height);
	std::string GetZoom();
	void GenerateColors(int nParts, int nSeed = -1);
	void GenerateColors2(int nParts, int nSeed = -1, int nWaves = 9);
	void AddWave(int nCol, int nPeriod = -1, int nStart = -1);
	void ChangeNumOfColors(int nParts);
	int GetNumOfColors();
	void ApplyColors(int x0, int x1, int y0, int y1);
	void ApplyColors();
	void ApplyIterationColors();
	void ApplyPhaseColors();
	void ApplySmoothColors();
	int GetSeed();
	COLOR14 GetKeyColor(int i);
	void SetKeyColor(COLOR14 col, int i);
	COLOR14 GetColor(int i);
	COLOR14 GetInteriorColor() { return m_cInterior; };
	void SetInteriorColor(const COLOR14 &c) { m_cInterior = c; };
	void ResetParameters();
	BOOL OpenFile(const std::string &szFile, BOOL bNoLocation = FALSE);
	BOOL OpenString(const std::string &szText, BOOL bNoLocation = FALSE);
	BOOL OpenMapB(const std::string &szFile, BOOL bReuseCenter = FALSE, double nZoomSize = 1);
	bool OpenMapEXR(const std::string &szFile);
	std::string ToText();
	BOOL SaveFile(const std::string &szFile, bool overwrite);
	double GetIterDiv();
	void SetIterDiv(double nIterDiv);
	int SaveJpg(const std::string &szFile, int nQuality, int nWidth = 0, int nHeight = 0);
	int64_t GetMaxApproximation();
	int64_t GetIterationOnPoint(int x, int y);
	double GetTransOnPoint(int x, int y);
	BOOL AddReference(int x, int y, BOOL bEraseAll = FALSE, BOOL bNoGlitchDetection = FALSE, BOOL bResuming = FALSE);
	BOOL HighestIteration(int &rx, int &ry);
	void IgnoreIsolatedGlitches();
	int FindCenterOfGlitch(int &rx, int &ry);
	void FindCenterOfGlitch(int x0, int x1, int y0, int y1, TH_FIND_CENTER *p);

	int GetColorIndex(int x, int y);
	BOOL GetFlat();
	void SetFlat(BOOL bFlat);
	BOOL GetTransition();
	void SetTransition(BOOL bTransition);
	BOOL GetITransition();
	void SetITransition(BOOL bITransition);

	bool GetTriangleInequalityAverage() { return m_bTriangleInequalityAverage; };
	void SetTriangleInequalityAverage(bool b) { m_bTriangleInequalityAverage = b; if (b) SetNoApprox(true); };

	void SaveMap(const std::string &szFile);
	void SaveMapB(const std::string &szFile);

	SmoothMethod GetSmoothMethod();
	void SetSmoothMethod(int nSmoothMethod);
	BailoutRadiusPreset GetBailoutRadiusPreset();
	void SetBailoutRadiusPreset(int nBailoutRadiusPreset);
	double GetBailoutRadiusCustom();
	void SetBailoutRadiusCustom(double nBailoutRadiusCustom);
	double GetBailoutRadius();
	floatexp GetBailoutSmall();
	BailoutNormPreset GetBailoutNormPreset();
	void SetBailoutNormPreset(int nBailoutNormPreset);
	double GetBailoutNormCustom();
	void SetBailoutNormCustom(double nBailoutNormCustom);
	double GetBailoutNorm();
	int GetPower() const;
	void SetPower(int nPower);
	void SetColorMethod(int nColorMethod);
	ColorMethod GetColorMethod();
	void SetDifferences(int nDifferences);
	Differences GetDifferences();
	void SetColorOffset(int nColorOffset);
	int GetColorOffset();
	void SetPhaseColorStrength(double nPhaseColorStrength);
	double GetPhaseColorStrength();
	void ErasePixel(int x, int y);

	void StoreLocation();
	void Mirror(int x, int y);

	int GetMWCount();
	void SetMW(BOOL bMW, BOOL bBlend);
	int GetMW(BOOL *pbBlend = NULL);
	BOOL GetMW(int nIndex, int &nPeriod, int &nStart, int &nType);
	BOOL AddMW(int nPeriod, int nStart, int nType);
	BOOL UpdateMW(int nIndex, int nPeriod, int nStart, int nType);
	BOOL DeleteMW(int nIndex);

	int64_t GetMaxExceptCenter();
	void SetFractalType(int nFractalType);
	int GetFractalType() const;

	int GetExponent();

	BOOL GetSlopes(int &nSlopePower, int &nSlopeRatio, int &nSlopeAngle);
	void SetSlopes(BOOL bSlope, int nSlopePower, int nSlopeRatio, int nSlopeAngle);

	BOOL GetTexture(double &nImgMerge,double &nImgPower,int &nImgRatio,std::string &szTexture);
	void SetTexture(BOOL bTexture,double nImgMerge,double nImgPower,int nImgRatio,const std::string &szTexture);

	bool GetTextureResize() const { return m_bTextureResize; }
	void SetTextureResize(bool resize) { m_bTextureResize = resize; }

	void AddInflectionPont(int x, int y);
	void RemoveInflectionPoint();

#ifdef KF_OPENCL
  int GetOpenCLDeviceIndex();
  void SetOpenCLDeviceIndex(int i);
#endif

	void OutputIterationData(int x, int y, int w, int h, bool bGlitch, int64_t antal, double test1, double test2, double phase, double nBailout, const complex<double> &de, int power);
	void OutputIterationData(int x, int y, int w, int h, bool bGlitch, int64_t antal, double test1, double smooth, double phase, double nBailout, const complex<double> &de);
	void OutputPixelData(int x, int y, int w, int h, bool bGlitch);
	Guess GuessPixel(int x, int y, int x0, int y0, int x1, int y1);
	Guess GuessPixel(int x, int y, int w, int h);

	inline bool OpenSettings(const std::string &filename) { return m_Settings.OpenFile(filename); }
	inline bool SaveSettings(const std::string &filename, bool overwrite) const { return m_Settings.SaveFile(filename, overwrite); }

	void SetTransformPolar(const polar2 &P);
	polar2 GetTransformPolar() const;
	void SetTransformMatrix(const mat2 &M);
	mat2 GetTransformMatrix() const;

#define DOUBLE(KEY) \
	inline double Get##KEY() const { return m_Settings.Get##KEY(); }; \
	inline void   Set##KEY(double x) { return m_Settings.Set##KEY(x); };
#define INT(KEY) \
	inline int64_t    Get##KEY() const { return m_Settings.Get##KEY(); }; \
	inline void   Set##KEY(int64_t x) { return m_Settings.Set##KEY(x); };
#define BOOL(KEY) \
	inline bool   Get##KEY() const { return m_Settings.Get##KEY(); }; \
	inline void   Set##KEY(bool x) { return m_Settings.Set##KEY(x); };
  DOUBLE(ZoomSize)
  INT(MaxReferences)
  DOUBLE(GlitchLowTolerance)
  DOUBLE(ApproxLowTolerance)
  BOOL(AutoApproxTerms)
	inline int64_t    GetApproxTerms() const { return m_Settings.GetApproxTerms(); };
	       void   SetApproxTerms(int64_t t);
  INT(WindowWidth)
  INT(WindowHeight)
  INT(WindowTop)
  INT(WindowLeft)
  INT(WindowBottom)
  INT(WindowRight)
  INT(ImageWidth)
  INT(ImageHeight)
  DOUBLE(ThreadsPerCore)
  INT(ThreadsReserveCore)
  BOOL(AnimateZoom)
  BOOL(ArbitrarySize)
  BOOL(ReuseReference)
  BOOL(AutoSolveGlitches)
  BOOL(Guessing)
  BOOL(SolveGlitchNear)
  BOOL(NoApprox)
  BOOL(Mirror)
  BOOL(AutoIterations)
  BOOL(ShowGlitches)
  BOOL(NoReuseCenter)
  INT(IsolatedGlitchNeighbourhood)
  INT(JitterSeed)
  INT(JitterShape)
  DOUBLE(JitterScale)
  BOOL(Derivatives)
  BOOL(ShowCrossHair)
  BOOL(UseNanoMB1)
  BOOL(UseNanoMB2)
  INT(OrderM)
  INT(OrderN)
  BOOL(InteriorChecking)
  DOUBLE(RadiusScale)
  INT(Shrink)
	inline bool   GetHalfColour() const { return m_Settings.GetHalfColour(); };
	       void   SetHalfColour(bool b);
	BOOL(SaveOverwrites)
	BOOL(ThreadedReference)
  INT(SIMDVectorSize)
  INT(SIMDChunkSize)
  INT(GlitchCenterMethod)
  inline bool GetUseArgMinAbsZAsGlitchCenter() const { return m_Settings.GetUseArgMinAbsZAsGlitchCenter(); };
  BOOL(UseOpenCL)
  BOOL(OpenCLThreaded)
  INT(OpenCLPlatform)
  inline EXRChannels GetEXRChannels() const { return m_Settings.GetEXRChannels(); };
  inline void SetEXRChannels(const EXRChannels x) { return m_Settings.SetEXRChannels(x); };
  BOOL(EXRParallel)
  BOOL(SaveNewtonProgress)
  BOOL(ExponentialMap)
  inline bool GetDerivativeGlitch() const
  {
		switch (GetReferenceType(m_nZoom))
		{
			// disable for single precision (does not work properly)
			case Reference_Float:
			case Reference_ScaledFloat:
			case Reference_FloatExpFloat:
				return false;
			default:
				return m_Settings.GetDerivativeGlitch();
		}
	};
	inline void SetDerivativeGlitch(bool x)
	{
		return m_Settings.SetDerivativeGlitch(x);
	};
  BOOL(ReferenceStrictZero)
  inline NumberType GetNumberTypes() const { return m_Settings.GetNumberTypes(); };
  inline void SetNumberTypes(const NumberType x) { return m_Settings.SetNumberTypes(x); };
  BOOL(UseRescaledSeries)
  BOOL(OpenResetsParameters)
  inline void GetTargetDimensions(int64_t *w, int64_t *h, int64_t *s) const { return m_Settings.GetTargetDimensions(w, h, s); }
  inline void SetTargetDimensions(int64_t w, int64_t h, int64_t s) { m_Settings.SetTargetDimensions(w, h, s); SetImageSize(w * s, h * s); }
#undef DOUBLE
#undef INT
#undef BOOL
  Reference_Type GetReferenceType(int64_t exponent10) const;

	void GetPixelOffset(const int i, const int j, double &x, double &y) const;
	void GetPixelCoordinates(const int i, const int j, floatexp &x, floatexp &y) const;
	void GetPixelCoordinates(const int i, const int j, floatexp &x, floatexp &y, floatexp &daa, floatexp &dab, floatexp &dba, floatexp &dbb) const;

  void UndoStore() { m_undo.push_back(ToText()); m_redo.clear(); };
  void Undo() { if (! m_undo.empty()) { auto s = m_undo.back(); m_undo.pop_back(); m_redo.push_back(s); OpenString(s); } };
  void Redo() { if (! m_redo.empty()) { auto s = m_redo.back(); m_redo.pop_back(); m_undo.push_back(s); OpenString(s); } };

  // for EXR IO
  itercount_array GetArrayCount() { return m_nPixels; };
  float *GetArrayTrans() { return m_nTrans[0]; };
  float *GetArrayPhase() { return m_nPhase ? m_nPhase[0] : nullptr; };
  float *GetArrayDEx() { return GetDerivatives() ? m_nDEx[0] : nullptr; };
  float *GetArrayDEy() { return GetDerivatives() ? m_nDEy[0] : nullptr; };
  half *GetArrayHalfColour() { return m_imageHalf; };
  size_t GetArrayHalfColourStride() { return m_row; };
	void ReinitializeBitmap();
	inline bool GetIsRendering() { return m_bIsRendering; };
	inline bool GetUseHybridFormula() const { return m_UseHybridFormula; };
	inline void SetUseHybridFormula(bool b)
	{
		SetReferenceStrictZero(true);
		m_UseHybridFormula = b;
	}
	const hybrid_formula &GetHybridFormula() const
	{
		return m_HybridFormula;
	};
	void SetHybridFormula(const hybrid_formula &h)
	{
		m_HybridFormula = h;
	};
	inline bool GetUseOpenGL() { return m_bUseOpenGL; }
	inline void SetUseOpenGL(bool gl) { m_bUseOpenGL = gl; }
	inline std::string GetGLSL() { return m_sGLSL; }
	inline void SetGLSL(const std::string &gl) { m_bGLSLChanged |= (m_sGLSL != gl); m_sGLSL = gl; }
	inline std::string GetGLSLLog() { return m_sGLSLLog; }
	inline void SetGLSLLog(const std::string &gl) { m_sGLSLLog = gl; }
	inline bool GetUseSRGB() { return m_bUseSRGB; }
	inline void SetUseSRGB(bool b) { m_bUseSRGB = b; }
};

struct TH_PARAMS
{
	int nXStart;
	int nXStop;
	CFraktalSFT *p;
	Reference_Type reftype;
};

extern int g_nAddRefX;
extern int g_nAddRefY;

extern double g_real;
extern double g_imag;
extern double g_SeedR;
extern double g_SeedI;
extern double g_FactorAR;
extern double g_FactorAI;

const double pi = 3.141592653589793;

extern void HSVToRGB(double hue, double sat, double bri, COLOR14 &cPos);
extern int MakePrime(int n);

// singleton instance
extern CFraktalSFT g_SFT;

template<typename T>
inline complex<double> compute_de(T Dr, T Di, T Jxa, T Jxb, T Jya, T Jyb, T s, const mat2 &TK)
{
  vec2 u = { double(Dr), double(Di) };
  mat2 J = { double(Jxa * s), double(Jxb * s), double(Jya * s), double(Jyb * s) };
  complex<double> v(u[0], u[1]);
  complex<double> num = abs(v) * log(abs(v));
  vec2 den = normalize(u) * (transpose(J) * TK);
  return num / complex<double>(den[0], den[1]);
}

#define KF_DEFAULT_GLSL "vec3 colour() { return KF_Colour(); }\n"

#endif
