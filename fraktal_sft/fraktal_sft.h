#ifndef KF_FRAKTAL_SFT_H
#define KF_FRAKTAL_SFT_H 1

#include <windows.h>
#include "Settings.h"
#include "CFixedFloat.h"
#include "CDecNumber.h"
#include "complex.h"

#include "floatexp.h"

#ifdef KF_OPENCL
#include "../cl/opencl.h"
extern std::vector<cldevice> cldevices;
#endif

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
	void Init(int nX, int nY);
	BOOL GetPixel(int &x, int &y, int &w, int &h, BOOL bMirrored = 0);
#if 0
	BOOL GetPixels(int *px, int *py, int &nCount);
#endif
};

// magic value stored in m_nTrans[][] when a glitch is detected
#define SET_TRANS_GLITCH(x) (fmin(log2((x) + 2.2250738585072014e-308) - 1024.0, -1.0))
#define GET_TRANS_GLITCH(x) ((x) < 0.0f)

// thresholds for switching to long double iterations
#define LONG_DOUBLE_THRESHOLD_POWER_2_MANDELBROT 590
#define LONG_DOUBLE_THRESHOLD_POWER_3_MANDELBROT 390
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
	SmoothMethod_Sqrt = 1,
	SmoothMethod_DE =2
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
	ColorMethod_DistanceSqrt = 8
};

enum Differences
{
	Differences_Traditional = 0,
	Differences_Forward3x3 = 1,
	Differences_Central3x3 = 2,
	Differences_Diagonal2x2 = 3
};

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
	int m_nMaxIter;
	int m_nGlitchIter;
	int m_nTotal;
	HBITMAP m_bmBmp;
	COLOR14 m_cPos[1025], m_cKeys[1025];
	int m_nParts;
	int m_nSeed;
	int **m_nPixels;
	BOOL m_bTrans;
	BOOL m_bITrans;
	float **m_nTrans;
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

	double m_nBailout;
	double m_nBailout2;
	SmoothMethod m_nSmoothMethod;
	ColorMethod m_nColorMethod;
	Differences m_nDifferences;
	int m_nColorOffset;
	BOOL m_bIterChanged;
	int m_nMinI, m_nMaxI;

	double m_xrd, m_xid;

	floatexp *m_dxr, *m_dxi;
	floatexp *m_APr;
	floatexp *m_APi;

	BOOL m_bMirrored;
	int m_nFractalType;

	long double *m_ldxr, *m_ldxi;

	double m_nIterDiv;
	int m_nMaxApproximation;
	int m_nApprox;
	RECT m_rApprox;

	double *m_db_dxr;
	double *m_db_dxi;
	double *m_db_z;

	floatexp m_pixel_step_x, m_pixel_step_y;
	floatexp m_pixel_center_x, m_pixel_center_y;

	BYTE *m_lpBits;
	int m_row;
	BITMAPINFOHEADER *m_bmi;
	bool m_bResized;
	int m_nX, m_nXPrev;
	int m_nY, m_nYPrev;
	int m_nDone;
	int m_nGuessed;
	int m_nRDone;
	BOOL m_bStop;
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

	int m_nInflections;
	complex<CFixedFloat> *m_pInflections;

	void CalculateApproximation(int nType);
	void DoApproximation(int &antal, const floatexp &D0r, const floatexp &D0i, floatexp &TDnr, floatexp &TDni, floatexp &TDDnr, floatexp &TDDni);
	void CalculateReference();
	void CalculateReferenceEXP();
	void CalculateReferenceLDBL();
	void CreateLists();
	std::string ToZoom(const CDecNumber &z, int &zoom);
	void RenderFractalEXP();
	void RenderFractalLDBL();
#ifdef KF_OPENCL
	void RenderFractalOpenCL();
	void RenderFractalOpenCLEXP();
#endif
	int GetArea(int **Node, int nXStart, int nXStop, int nEqSpan = 2, int **Pixels = NULL, int nDone = -1);

	HBITMAP ShrinkBitmap(HBITMAP bmSrc,int nNewWidth,int nNewHeight,BOOL bHalfTone=TRUE);
	void SetTexture(int nIndex, int x, int y);
	void SetColor(int nIndex, int nIter, double offs = 0, int x = -1, int y = -1);

#ifdef KF_OPENCL
	int clid;
  OpenCL *cl;
#endif

public:
	BOOL m_bRunning;
	int nPos;
	void MandelCalc();
	void MandelCalcEXP();
	void MandelCalcLDBL();

	CFraktalSFT();
	~CFraktalSFT();

	inline void SetWindow(HWND hWnd) { m_hWnd = hWnd; };

	void SetPosition(const CFixedFloat &rstart, const CFixedFloat &rstop, const CFixedFloat &istart, const CFixedFloat &istop, int nX, int nY);
	void SetPosition(const std::string &szR, const std::string &szI, const std::string &szZ);
	std::string ToZoom();
	void SetImageSize(int nx, int ny);
	void RenderFractal(int nX, int nY, int nMaxIter, HWND hWnd, BOOL bNoThread = FALSE, BOOL bResetOldGlitch = TRUE);
	void RenderFractal();
	HBITMAP GetBitmap();
	void UpdateBitmap();
	int GetWidth();
	int GetHeight();
	void Stop(BOOL bNoPostWhenDone = FALSE);
	int CountFrames(int nProcent);
	void Zoom(int nXPos, int nYPos, double nZoomSize, int nWidth, int nHeight, BOOL bReuseCenter = FALSE);
	BOOL Center(int &rx, int &ry, BOOL bSkipM = FALSE, BOOL bQuick = FALSE);
	int GetProgress(int *pnGuessed = NULL, int *pnRDone = NULL, int *pnAP = NULL);
	std::string GetPosition();
	void GetIterations(int &nMin, int &nMax, int *pnCalculated = NULL, int *pnType = NULL, BOOL bSkipMaxIter = FALSE);
	int GetIterations();
	void SetIterations(int nIterations);
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
	void ApplyColors();
	void ApplyIterationColors();
	void ApplySmoothColors();
	int GetSeed();
	COLOR14 GetKeyColor(int i);
	void SetKeyColor(COLOR14 col, int i);
	COLOR14 GetColor(int i);
	BOOL OpenFile(const std::string &szFile, BOOL bNoLocation = FALSE);
	BOOL OpenString(const std::string &szText, BOOL bNoLocation = FALSE);
	BOOL OpenMapB(const std::string &szFile, BOOL bReuseCenter = FALSE, double nZoomSize = 1);
	std::string ToText();
	BOOL SaveFile(const std::string &szFile);
	double GetIterDiv();
	void SetIterDiv(double nIterDiv);
	int SaveJpg(const std::string &szFile, int nQuality, int nWidth = 0, int nHeight = 0);
	int GetMaxApproximation();
	int GetIterationOnPoint(int x, int y);
	int GetTransOnPoint(int x, int y);
	BOOL AddReference(int x, int y, BOOL bEraseAll = FALSE, BOOL bNoGlitchDetection = FALSE, BOOL bResuming = FALSE);
	BOOL HighestIteration(int &rx, int &ry);
	int FindCenterOfGlitch(int &rx, int &ry);
	int GetColorIndex(int x, int y);
	BOOL GetTransition();
	void SetTransition(BOOL bTransition);
	BOOL GetITransition();
	void SetITransition(BOOL bITransition);

	void SaveMap(const std::string &szFile);
	void SaveMapB(const std::string &szFile);

	SmoothMethod GetSmoothMethod();
	void SetSmoothMethod(int nSmoothMethod);
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

	int GetMaxExceptCenter();
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

	void OutputIterationData(int x, int y, int bGlitch, int antal, double test1, double test2, double de);
	void OutputPixelData(int x, int y, int w, int h, int bGlitch);
	bool GuessPixel(int x, int y, int w, int h);

	inline bool OpenSettings(const std::string &filename) { return m_Settings.OpenFile(filename); }
	inline bool SaveSettings(const std::string &filename) const { return m_Settings.SaveFile(filename); }

#define DOUBLE(KEY) \
	inline double Get##KEY() const { return m_Settings.Get##KEY(); }; \
	inline void   Set##KEY(double x) { return m_Settings.Set##KEY(x); };
#define INT(KEY) \
	inline int    Get##KEY() const { return m_Settings.Get##KEY(); }; \
	inline void   Set##KEY(int x) { return m_Settings.Set##KEY(x); };
#define BOOL(KEY) \
	inline bool   Get##KEY() const { return m_Settings.Get##KEY(); }; \
	inline void   Set##KEY(bool x) { return m_Settings.Set##KEY(x); };
  DOUBLE(ZoomSize)
  INT(MaxReferences)
  BOOL(GlitchLowTolerance)
  BOOL(ApproxLowTolerance)
  BOOL(AutoApproxTerms)
	inline int    GetApproxTerms() const { return m_Settings.GetApproxTerms(); };
	       void   SetApproxTerms(int t);
  INT(WindowWidth)
  INT(WindowHeight)
  INT(WindowTop)
  INT(WindowLeft)
  INT(WindowBottom)
  INT(WindowRight)
  INT(ImageWidth)
  INT(ImageHeight)
  INT(ThreadsPerCore)
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
#undef DOUBLE
#undef INT
#undef BOOL

	void GetPixelOffset(const int i, const int j, double &x, double &y) const
	{
		int c = GetJitterSeed();
		if (c)
		{
			// https://en.wikipedia.org/wiki/Box%E2%80%93Muller_transform
			double u = dither(i, j, 2 * c + 0);
			double v = dither(i, j, 2 * c + 1);
			double r = u ? sqrt(-2 * log(u)) : 0;
			double t = 2 * 3.141592653589793 * v;
			x = r * cos(t);
			y = r * sin(t);

		}
		else
		{
			x = 0.0;
			y = 0.0;
		}
	}
	void GetPixelCoordinates(const int i, const int j, floatexp &x, floatexp &y) const
	{
		double di = 0;
		double dj = 0;
		GetPixelOffset(i, j, di, dj);
		floatexp u = (i - m_nX/2 + di) * m_pixel_step_x;
		floatexp v = (j - m_nY/2 + dj) * m_pixel_step_y;
		x = m_pixel_center_x + m_C * u + m_S * v;
		y = m_pixel_center_y - m_S * u + m_C * v;
	};
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

extern double g_Degree;
extern BOOL g_LDBL;

const double pi = 3.141592653589793;

extern void HSVToRGB(double hue, double sat, double bri, COLOR14 &cPos);
extern int MakePrime(int n);

// singleton instance
extern CFraktalSFT g_SFT;

#endif
