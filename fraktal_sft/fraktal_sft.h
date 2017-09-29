#include <windows.h>
#include "CFixedFloat.h"
#include "CDecNumber.h"
#include "complex.h"

#include "floatexp.h"

#ifdef KF_OPENCL
#include "../cl/opencl.h"
extern std::vector<cldevice> cldevices;
#endif

class CPixels
{
	RECT m_rRect;
	int m_nX;
	int m_nX2;
	int m_nY;
	int m_nY2;
	int m_nStep;
	int m_nStepPos;
	int m_nStepPos8;
	int m_nRectPos;
	int m_nRectPart;
	POINT *m_pPixels;
	int m_nPixels;
	int m_nNextPixel;
	HANDLE m_hMutex;
public:
	CPixels();
	void Init(int nStep, int nX, int nY);
	int GetStep();
	BOOL GetPixel(int &x, int &y, BOOL bMirrored = 0);
	BOOL GetPixels(int *px, int *py, int &nCount);
};

#define SMOOTH_BAILOUT 100
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

// this sets the maximum number of references per image
#define OLD_GLITCH 10000

#ifdef KF_LONG_DOUBLE_DLL
struct ldbl {
	unsigned char val[
#ifdef _WIN64
		16
#else
		12
#endif
	];
};
#else
typedef long double ldbl;
#endif


struct ldblexp {
	ldbl val;
	__int64 exp;
};
#define MULTIWAVE_MAX 30
struct MULTIWAVE
{
	int nPeriod;
	int nStart;
	int nType;
};
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

class CFraktalSFT
{
	MULTIWAVE m_MW[MULTIWAVE_MAX];
	int m_nMW;
	BOOL m_bMW;
	BOOL m_bBlend;
	HANDLE m_hMutex;
	CPixels m_P;
	CFixedFloat m_rstart, m_istart, m_rstop, m_istop, m_rref, m_iref;
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
	BOOL m_bLowTolerance;
	BOOL m_bGlitchLowTolerance;
	int m_nPrevPower;
	int *m_pnExpConsts;
	int m_nMaxOldGlitches;
	BOOL g_bShowGlitches;
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
	int m_nSmoothMethod;
	ColorMethod m_nColorMethod;
	Differences m_nDifferences;
	int m_nColorOffset;
	BOOL m_bIterChanged;
	int m_nMinI, m_nMaxI;

	double m_xrd, m_xid;

	int m_nTerms;
	BOOL m_bAutoTerms;
	floatexp *m_dxr, *m_dxi;
	floatexp *m_APr;
	floatexp *m_APi;
	floatexp *m_DX;
	floatexp *m_DY;

	BOOL m_bMirrored;
	int m_nFractalType;

	ldbl *m_ldxr, *m_ldxi;
	ldbl *m_lDX;
	ldbl *m_lDY;

	double m_nIterDiv;
	int m_nMaxApproximation;
	int m_nApprox;
	RECT m_rApprox;

	double *m_db_dxr;
	double *m_db_dxi;
	double *m_db_z;
	double *m_pDX;
	double *m_pDY;

	BYTE *m_lpBits;
	int m_row;
	BITMAPINFOHEADER *m_bmi;
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
	int m_nStatus;
	int m_nFrameDone;
	BOOL m_bAddReference;
	BOOL m_bNoApproximation;

	BOOL m_bTexture;
	double m_nImgMerge;
	double m_nImgPower;
	int m_nImgRatio;
	char m_szTexture[256];
	BYTE *m_lpTextureBits;
	BITMAPINFOHEADER m_bmiBkg;
	int m_rowBkg;

	int m_nInflections;
	complex<CFixedFloat> *m_pInflections;

	void CalculateApproximation(int nType);
	void CalculateReference();
	void CalculateReferenceEXP();
	void CalculateReferenceLDBL();
	void CreateLists();
	char *ToZoom(const CDecNumber &z, int &zoom);
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
	void MandelCalc(int nX, int nY);
	void MandelCalcEXP(int nX, int nY);
	void MandelCalcLDBL(int nX, int nY);

	CFraktalSFT();
	~CFraktalSFT();

	void SetPosition(const CFixedFloat &rstart, const CFixedFloat &rstop, const CFixedFloat &istart, const CFixedFloat &istop, int nX, int nY);
	void SetPosition(const char *szR, const char *szI, const char *szZ);
	void SetPosition(const char *szR, const char *szI, const std::string &szZ);
	char *ToZoom();
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
	char *GetPosition();
	void GetIterations(int &nMin, int &nMax, int *pnCalculated = NULL, int *pnType = NULL, BOOL bSkipMaxIter = FALSE);
	int GetIterations();
	void SetIterations(int nIterations);
	char *GetRe();
	char *GetRe(int nXPos, int nYPos, int width, int height);
	char *GetIm();
	char *GetIm(int nXPos, int nYPos, int width, int height);
	char *GetZoom();
	void GenerateColors(int nParts, int nSeed = -1);
	void GenerateColors2(int nParts, int nSeed = -1, int nWaves = 9);
	void AddWave(int nCol, int nPeriod = -1, int nStart = -1);
	void ChangeNumOfColors(int nParts);
	int GetNumOfColors();
	void ApplyColors();
	void ApplyIterationColors();
	void ApplySmoothColors();
	int GetSeed();
	void ReuseReference(BOOL bReuse);
	COLOR14 GetKeyColor(int i);
	void SetKeyColor(COLOR14 col, int i);
	COLOR14 GetColor(int i);
	BOOL OpenFile(char *szFile, BOOL bNoLocation = FALSE);
	BOOL OpenMapB(char *szFile, BOOL bReuseCenter = FALSE, double nZoomSize = 1);
	BOOL SaveFile(char *szFile);
	double GetIterDiv();
	void SetIterDiv(double nIterDiv);
	int SaveJpg(char *szFile, int nQuality, int nWidth = 0, int nHeight = 0);
	int GetMaxApproximation();
	int GetIterationOnPoint(int x, int y);
	int GetTransOnPoint(int x, int y);
	BOOL AddReference(int x, int y, BOOL bEraseAll = FALSE, BOOL bNP = FALSE, BOOL bNoGlitchDetection = FALSE, BOOL bResuming = FALSE);
	BOOL HighestIteration(int &rx, int &ry);
	BOOL FindCenterOfGlitch(int &rx, int &ry, BOOL bNP = FALSE);
	BOOL GetNoApproximation();
	void SetNoApproximation(BOOL bNoApproximation);
	int GetColorIndex(int x, int y);
	BOOL GetTransition();
	void SetTransition(BOOL bTransition);
	BOOL GetITransition();
	void SetITransition(BOOL bITransition);

	void SaveMap(char *szFile);
	void SaveMapB(char *szFile);

	int GetSmoothMethod();
	void SetSmoothMethod(int nSmoothMethod);
	int GetPower();
	void SetPower(int nPower);
	BOOL GetGlitchLowTolerance();
	void SetGlitchLowTolerance(BOOL bGlitchLowTolerance);
	BOOL GetLowTolerance();
	void SetLowTolerance(BOOL bLowTolerance);
	void SetColorMethod(int nColorMethod);
	ColorMethod GetColorMethod();
	void SetDifferences(int nDifferences);
	Differences GetDifferences();
	void SetColorOffset(int nColorOffset);
	int GetColorOffset();
	void ErasePixel(int x, int y);

	void StoreLocation();
	void Mirror(int x, int y);
	int GetMirror();
	void SetMirror(BOOL bMirror);

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

	int GetMaxOldGlitches();
	void SetMaxOldGlitches(int nMaxOldGlitches);
	int GetExponent();

	void SetTerms(int nTerms);
	int GetTerms();
	void SetAutoTerms(BOOL bAuto);
	BOOL GetAutoTerms();
	void SetShowGlitches(BOOL bShowGlitches);
	BOOL GetShowGlitches();

	double GetRatioX();
	double GetRatioY();
	void SetRatio(double x, double y);

	BOOL GetSlopes(int &nSlopePower, int &nSlopeRatio, int &nSlopeAngle);
	void SetSlopes(BOOL bSlope, int nSlopePower, int nSlopeRatio, int nSlopeAngle);

	BOOL GetTexture(double &nImgMerge,double &nImgPower,int &nImgRatio,char *szTexture);
	void SetTexture(BOOL bTexture,double nImgMerge,double nImgPower,int nImgRatio,char *szTexture);

	void AddInflectionPont(int x, int y);
	void RemoveInflectionPoint();

#ifdef KF_OPENCL
  int GetOpenCLDeviceIndex();
  void SetOpenCLDeviceIndex(int i);
#endif

};

struct TH_PARAMS
{
	int nXStart;
	int nXStop;
	CFraktalSFT *p;
};
