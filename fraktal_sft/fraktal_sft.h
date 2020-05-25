/*
Kalles Fraktaler 2
Copyright (C) 2013-2017 Karl Runmo
Copyright (C) 2017-2020 Claude Heiland-Allen

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

#include "Settings.h"
#include "CFixedFloat.h"
#include "CDecNumber.h"
#include "complex.h"
#include "floatexp.h"
#include "../common/matrix.h"
#include "itercount_array.h"
#include "colour.h"

#ifdef KF_OPENCL
#include "../cl/opencl.h"
extern std::vector<cldevice> cldevices;
#endif

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
#define SET_TRANS_GLITCH(x) (fmin(log2((x) + 2.2250738585072014e-308) - 1024.0, -1.0))
#else
#define SET_TRANS_GLITCH(x) (-1.0)
#endif
#define GET_TRANS_GLITCH(x) ((x) < 0.0f)

// thresholds for switching to floatexp iterations
#define FLOATEXP_THRESHOLD_POWER_2 9800
#define FLOATEXP_THRESHOLD_POWER_3 6533
#define FLOATEXP_THRESHOLD_DEFAULT 4900
// thresholds for switching to long double iterations
#define LONG_DOUBLE_THRESHOLD_POWER_2 590
#define LONG_DOUBLE_THRESHOLD_POWER_3 390
#define LONG_DOUBLE_THRESHOLD_DEFAULT 290
// threshold for switching to scaled double iterations
// this is lower than the theoretical maximum to avoid derivative overflow
#define SCALED_DOUBLE_THRESHOLD 290

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
	ColorMethod_ATan = 10
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

extern double g_Degree;

enum SeriesType
{
	SeriesType_None = 0,
	SeriesType_Complex = 1,
	SeriesType_Real = 2
};

struct SeriesR2
{
	floatexp s[MAX_APPROX_TERMS+1][MAX_APPROX_TERMS+1];
	floatexp t[MAX_APPROX_TERMS+1][MAX_APPROX_TERMS+1];
};


struct TextureParams
{
	BOOL m_bTexture;
	std::string m_szTexture;
	double m_nImgPower;
	int m_nX;
	int m_nY;
};

struct TH_FIND_CENTER;

class CFraktalSFT
{
	Settings m_Settings;

	MULTIWAVE m_MW[MULTIWAVE_MAX];
	int m_nMW;
	BOOL m_bMW;
	BOOL m_bBlend;
	HANDLE m_hMutex;
	CPixels m_P;
	CFixedFloat m_rstart, m_istart, m_rstop, m_istop, m_rref, m_iref;
	double m_dPixelSpacing;
	long double m_lPixelSpacing;
	floatexp m_fPixelSpacing;
	double m_epsilon;
	CFixedFloat m_storedr, m_storedi;
	POINT m_pOldGlitch[OLD_GLITCH];
	int m_nSizeImage;
	int m_nZoom;
	int64_t m_nMaxIter;
	int64_t m_nGlitchIter;
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
	double m_C, m_S;
	struct SIZE_F { double cx; double cy; };
	SIZE_F m_scRatio;
	BOOL m_bNoPostWhenDone;
	BOOL m_bSlopes;
	int m_nSlopePower;
	int m_nSlopeRatio;
	int m_nSlopeAngle;
	double m_nSlopeX, m_nSlopeY;
	double m_nZooms;

	SmoothMethod m_nSmoothMethod;
	BailoutRadiusPreset m_nBailoutRadiusPreset;
	double m_nBailoutRadiusCustom;
	BailoutNormPreset m_nBailoutNormPreset;
	double m_nBailoutNormCustom;
	ColorMethod m_nColorMethod;
	Differences m_nDifferences;
	int m_nColorOffset;
	BOOL m_bIterChanged;
	int64_t m_nMinI, m_nMaxI;

	double m_xrd, m_xid;

	floatexp *m_dxr, *m_dxi;
	floatexp *m_APr;
	floatexp *m_APi;
	SeriesR2 *m_APs;

	BOOL m_bMirrored;
	int m_nFractalType;

	long double *m_ldxr, *m_ldxi;

	double m_nIterDiv;
	int64_t m_nMaxApproximation;
	int64_t m_nApprox;
	RECT m_rApprox;

	double *m_db_dxr;
	double *m_db_dxi;
	double *m_db_z;

	floatexp m_pixel_step_x, m_pixel_step_y;
	floatexp m_pixel_center_x, m_pixel_center_y;

	half *m_imageHalf; // for EXR export
	BYTE *m_lpBits;
	int m_row;
	BITMAPINFOHEADER *m_bmi;
	bool m_bResized;
	int m_nX, m_nXPrev;
	int m_nY, m_nYPrev;
	int m_nDone;
	int m_nGuessed;
	int64_t m_nRDone;
	bool m_bStop;
	HWND m_hWnd;
	char *m_szPosition;
	BOOL m_bReuseRef;
	double m_nScaling;
	int m_nScalingOffset;
	long double m_nScalingL;
	int m_nScalingOffsetL;
	int m_nStatus;
	int m_nFrameDone;
	BOOL m_bAddReference;
	BOOL m_bNoApproximation;

	BOOL m_bTexture;
	double m_nImgMerge;
	double m_nImgPower;
	int m_nImgRatio;
	std::string m_szTexture;
	BYTE *m_lpTextureBits;
	BITMAPINFOHEADER m_bmiBkg;
	int m_rowBkg;
	TextureParams m_ActiveTextureParams;

	std::vector< complex<CFixedFloat> > m_Inflections;

	NanoMB1_Reference *m_NanoMB1Ref;
	NanoMB2_Reference *m_NanoMB2Ref;

	SeriesType GetApproximationType()
	{
		if (m_nFractalType == 0)
			return SeriesType_Complex;
		if (m_nFractalType == 1 && m_nPower == 2)
			return SeriesType_Real;
		return SeriesType_None;
	}
	void CalculateApproximation(int nType);
	void DoApproximation(int64_t &antal, const floatexp &D0r, const floatexp &D0i, floatexp &TDnr, floatexp &TDni, floatexp &TDDnr, floatexp &TDDni);
	void DoApproximation(int64_t &antal, const floatexp &a, const floatexp &b, floatexp &x, floatexp &y, floatexp &dxa, floatexp &dxb, floatexp &dya, floatexp &dyb);
	void DoApproximation(const floatexp &a, const floatexp &b, floatexp &x, floatexp &y); // for internal usage only, assumes isR
	void CalculateReference();
	void CalculateReferenceEXP();
	void CalculateReferenceLDBL();
	void CalculateReferenceNANOMB1();
	void CalculateReferenceNANOMB2();
	void CalcStart();
	void CreateLists();
	std::string ToZoom(const CDecNumber &z, int &zoom);
	void RenderFractalEXP();
	void RenderFractalLDBL();
	void RenderFractalNANOMB1();
	void RenderFractalNANOMB2();
#ifdef KF_OPENCL
	void RenderFractalOpenCL();
	void RenderFractalOpenCLEXP();
#endif
	int GetArea(itercount_array &Node, int nXStart, int nXStop, int nEqSpan, itercount_array &Pixels, int nDone, POINT *pQ, int nQSize);

	void LoadTexture();
	void SetTexture(int nIndex, int x, int y, srgb &s);
	void SetColor(int nIndex, int64_t nIter, double offs = 0, int x = -1, int y = -1, int w = 1, int h = 1);
	void DeleteArrays();

#ifdef KF_OPENCL
	int clid;
  OpenCL *cl;
#endif

	std::vector<std::string> m_undo;
	std::vector<std::string> m_redo;

	bool m_bIsRendering;
public:
	BOOL m_bRunning;
	BOOL m_bInhibitColouring;
	bool m_bInteractive;
	int nPos;
	void MandelCalc();
	void MandelCalcEXP();
	void MandelCalcLDBL();
	void MandelCalcNANOMB1();
	void MandelCalcNANOMB2();

	CFraktalSFT();
	~CFraktalSFT();

	inline void SetWindow(HWND hWnd) { m_hWnd = hWnd; };

	void SetPosition(const CFixedFloat &rstart, const CFixedFloat &rstop, const CFixedFloat &istart, const CFixedFloat &istop, int nX, int nY);
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
	void Stop(BOOL bNoPostWhenDone = FALSE);
	int CountFrames(int nProcent);
	void Zoom(int nXPos, int nYPos, double nZoomSize, int nWidth, int nHeight, BOOL bReuseCenter = FALSE, bool autoRender = true);
	BOOL Center(int &rx, int &ry, BOOL bSkipM = FALSE, BOOL bQuick = FALSE);
	double GetProgress(int *pnGuessed = NULL, int *pnRDone = NULL, int *pnAP = NULL, int *pnT = NULL);
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

	void SaveMap(const std::string &szFile);
	void SaveMapB(const std::string &szFile);

	SmoothMethod GetSmoothMethod();
	void SetSmoothMethod(int nSmoothMethod);
	BailoutRadiusPreset GetBailoutRadiusPreset();
	void SetBailoutRadiusPreset(int nBailoutRadiusPreset);
	double GetBailoutRadiusCustom();
	void SetBailoutRadiusCustom(double nBailoutRadiusCustom);
	double GetBailoutRadius();
	BailoutNormPreset GetBailoutNormPreset();
	void SetBailoutNormPreset(int nBailoutNormPreset);
	double GetBailoutNormCustom();
	void SetBailoutNormCustom(double nBailoutNormCustom);
	double GetBailoutNorm();
	int GetPower();
	void SetPower(int nPower);
	void SetColorMethod(int nColorMethod);
	ColorMethod GetColorMethod();
	void SetDifferences(int nDifferences);
	Differences GetDifferences();
	void SetColorOffset(int nColorOffset);
	int GetColorOffset();
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
	int GetFractalType();

	int GetExponent();

	double GetRatioX();
	double GetRatioY();
	void SetRatio(double x, double y);

	BOOL GetSlopes(int &nSlopePower, int &nSlopeRatio, int &nSlopeAngle);
	void SetSlopes(BOOL bSlope, int nSlopePower, int nSlopeRatio, int nSlopeAngle);

	BOOL GetTexture(double &nImgMerge,double &nImgPower,int &nImgRatio,std::string &szTexture);
	void SetTexture(BOOL bTexture,double nImgMerge,double nImgPower,int nImgRatio,const std::string &szTexture);

	void AddInflectionPont(int x, int y);
	void RemoveInflectionPoint();

#ifdef KF_OPENCL
  int GetOpenCLDeviceIndex();
  void SetOpenCLDeviceIndex(int i);
#endif

	void OutputIterationData(int x, int y, int w, int h, bool bGlitch, int64_t antal, double test1, double test2, double phase, double nBailout, const complex<double> &de);
	void OutputPixelData(int x, int y, int w, int h, bool bGlitch);
	bool GuessPixel(int x, int y, int x0, int y0, int x1, int y1);
	bool GuessPixel(int x, int y, int w, int h);

	inline bool OpenSettings(const std::string &filename) { return m_Settings.OpenFile(filename); }
	inline bool SaveSettings(const std::string &filename, bool overwrite) const { return m_Settings.SaveFile(filename, overwrite); }

	inline void SetTransformPolar(const polar2 &P)
	{
		using std::sqrt;
		g_Degree = -P.stretch_angle;
		SetRatio(m_nX, m_nY / (P.stretch_factor * P.stretch_factor));
		// FIXME need to scale image by P.scale * P.stretch_factor
		// FIXME kf does not support rotation after skew...
	}
	inline void SetTransformMatrix(const mat2 &M) { SetTransformPolar(polar_decomposition(M)); }
	inline mat2 GetTransformMatrix() const
	{
		double ratio = (((double)m_nY/(double)m_nX)/(360.0/640.0)) * ((double)360 / (double)m_scRatio.cy);
		double k = sqrt(std::abs(ratio));
		return polar_composition(polar2(1.0, -g_Degree, k, g_Degree));
	}

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
  BOOL(GlitchLowTolerance)
  BOOL(ApproxLowTolerance)
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
  BOOL(LongDoubleAlways)
  BOOL(FloatExpAlways)
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
  BOOL(OpenCLPlatform)
  inline EXRChannels GetEXRChannels() const { return m_Settings.GetEXRChannels(); };
  inline void SetEXRChannels(const EXRChannels x) { return m_Settings.SetEXRChannels(x); };
  BOOL(EXRParallel)
#undef DOUBLE
#undef INT
#undef BOOL

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
};

struct TH_PARAMS
{
	int nXStart;
	int nXStop;
	CFraktalSFT *p;
};

extern int g_nAddRefX;
extern int g_nAddRefY;

extern double g_real;
extern double g_imag;
extern double g_SeedR;
extern double g_SeedI;
extern double g_FactorAR;
extern double g_FactorAI;

extern int g_nLDBL;
extern int g_nEXP;
extern int g_nRefZero;

extern BOOL g_LDBL;

const double pi = 3.141592653589793;

extern void HSVToRGB(double hue, double sat, double bri, COLOR14 &cPos);
extern int MakePrime(int n);

// singleton instance
extern CFraktalSFT g_SFT;

#endif
