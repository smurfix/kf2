// Kalles Fraktaler 2
//
// � 2014-2015 Karl Runmo ,runmo@hotmail.com
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

#include <windows.h>
#include <math.h>
#include <commctrl.h>
#include <math.h>
#include "../common/parallell.h"
#include "../common/getimage.h"
#include "../common/StringVector.h"
#include "../common/FolderBrowser.h"
#include "listbox.h"
#include "../common/tooltip.h"
#include "resource.h"
#include "fraktal_sft.h"
#include "newton.h"
#include <malloc.h>
#include "../formula/formula.h"
#ifdef KF_OPENCL
#include "../cl/opencl.h"
#endif
#include "../common/bitmap.h"
#include <png.h>
#include <zlib.h>
#include "jpeg.h"
#include "png.h"
#include "main.h"
#include "main_color.h"
#include "main_examine.h"
#include "main_iterations.h"
#include "main_position.h"
#include "cmdline.h"
#include <iostream>

#ifdef KF_OPENCL
std::vector<cldevice> cldevices;
#endif

POINT g_pInflections[10];
int g_nInflection=0;
extern double g_SeedR;
extern double g_SeedI;
extern double g_FactorAR;
extern double g_FactorAI;

//#define PARAM_ANIMATION

BOOL g_bNewton=FALSE;
HWND g_hwNewton=NULL;
BOOL g_bResizing=FALSE;
BOOL g_bTrackSelect=FALSE;
POINT g_pTrackStart;
HICON g_hIcon;

bool g_bAddReference=false;
bool g_bEraser=false;

#ifdef KF_OPENCL
HWND g_hwOpenCL = NULL;
#endif


HBITMAP g_bmMarilyn=NULL;
double g_nMinDiff=0;

extern BOOL g_LDBL;
extern int g_nLDBL;
extern int g_nEXP;
#ifdef _WIN64
#define GCL_WNDPROC -24
#define GWL_WNDPROC -4
#define GWL_USERDATA -21
#endif

extern double g_Degree;
extern double g_real;
extern double g_imag;

BOOL ISFLOATOK(double a);

CFraktalSFT g_SFT;

bool g_bAddMainReference=false;
int g_bAutoGlitch = 1;
BOOL g_bRotate=FALSE;
BOOL g_bMove=FALSE;
BOOL g_bShowInflection=FALSE;
BOOL g_bShowSkew=FALSE;
double g_nSkewStretch;
double g_nSkewRotate;
double g_StartDegree=0;
BOOL bXSign=FALSE;
double g_MoveDegree=0;
SIZE g_scSize = {640,360};
BOOL g_bAnimateEachFrame=FALSE;

int g_nPrevGlitchX=-1;
int g_nPrevGlitchY=-1;
BOOL g_bStoreZoom=FALSE;
BOOL g_bStoreZoomMap=FALSE;
BOOL g_bStoreZoomJpg=FALSE;
BOOL g_bStoreZoomPng=FALSE;
int g_nStoreZoomCount = 0;
int g_nStoreZoomLimit = 0;

bool g_bWaitRead=false;
int g_nStopAtExponent=0;

const char *g_pszStatus[] = {
			"Generate reference",
			"Render first image",
			"Check done images",
			"Center",
			"Top",
			"Bottom",
			"Top left",
			"Bottom left",
			"Top right",
			"Bottom right",
			"Left",
			"Right"};
int g_nStatus=0;
BOOL g_bFirstDone=TRUE;
BOOL g_bSaveJpeg=FALSE;
BOOL g_bSavePng=FALSE;
BOOL g_bSaveMap=FALSE;
BOOL g_bInteractive=TRUE;
const CommandLineArguments *g_args = 0;

char g_szRecovery[256];

BOOL g_bResetReference=FALSE;
BOOL g_bFindMinibrot=FALSE;
BOOL g_bFindMinibrotCount=0;
int g_bFindMinibrotPos=0;

BOOL g_bZoomRunning=FALSE;
BOOL g_bZoomStop=FALSE;

static void bmp2rgb(BYTE *rgb, const BYTE *bmp, int height, int width, int stride, int bytes)
{
	// TODO add support for strict aliasing optimisations, "restrict" etc
	for (int y = height; y >= 0; --y)
	{
		int k = y * stride;
		for (int x = 0; x < width; ++x)
			if (k + 2 < bytes)
			{
				k += 3;
				*rgb++ = bmp[--k];
				*rgb++ = bmp[--k];
				*rgb++ = bmp[--k];
				k += 3;
			}
	}
}
extern int SaveImage(const std::string &szFileName,HBITMAP bmBmp,int nQuality, const std::string &comment)
{
	int row;
	BYTE *lpBits, *lpJeg;
	BITMAPINFOHEADER bmi={sizeof(BITMAPINFOHEADER)};
	HDC hDC = GetDC(NULL);
	if(!GetDIBits(hDC,bmBmp,0,0,NULL,(LPBITMAPINFO)&bmi,DIB_RGB_COLORS))
		Beep(1000,10);
	bmi.biCompression=bmi.biClrUsed=bmi.biClrImportant=0;
	bmi.biBitCount = 24;
	row = ((((bmi.biWidth*(DWORD)bmi.biBitCount)+31)&~31) >> 3);
	bmi.biSizeImage=row*bmi.biHeight;
	lpBits = new BYTE[bmi.biSizeImage];
	lpJeg = new BYTE[bmi.biSizeImage];
	if(!GetDIBits(hDC,bmBmp,0,bmi.biHeight,lpBits,
			(LPBITMAPINFO)&bmi,DIB_RGB_COLORS))
		Beep(1000,10);
	bmp2rgb(lpJeg, lpBits, bmi.biHeight, bmi.biWidth, row, bmi.biSizeImage);
	int nRet;
	if (nQuality < 0)
		nRet = SavePNG(szFileName,(char*)lpJeg,bmi.biHeight,bmi.biWidth,3,comment);
	else
		nRet = SaveJPG(szFileName,(char*)lpJeg,bmi.biHeight,bmi.biWidth,3,nQuality,comment);
	delete [] lpJeg;
	delete [] lpBits;
	ReleaseDC(NULL,hDC);
	return nRet;
}
POINT g_pSelect, g_pStart;
int g_bSelect=0;
HWND g_hwStatus=NULL;
__int64 g_nTStart;
BOOL g_bRunning=FALSE;
HWND g_hwHair;
HWND g_hwColors=NULL;
std::string g_szFile;

extern double GetDlgItemFloat(HWND hWnd,int nID)
{
	char szText[256];
	GetDlgItemText(hWnd,nID,szText,sizeof(szText));
	return atof(szText);
}

extern void SetDlgItemFloat(HWND hWnd,int nID,double val)
{
	char szText[256];
	snprintf(szText,256,"%.4f",val);
	SetDlgItemText(hWnd,nID,szText);
}

extern int FileExists(const std::string &szFind)
{
	WIN32_FIND_DATA wf;
	HANDLE hFind = FindFirstFile(szFind.c_str(),&wf);
	if(hFind==INVALID_HANDLE_VALUE)
		return 0;
	FindClose(hFind);
	return 1;
}

const char *GetToolText_const(int nID,LPARAM lParam)
{
	if(lParam==0){
		return const_cast<char *>(ColorToolTip(nID));
	}
	else if(lParam==1){
		return const_cast<char *>(IterationToolTip(nID));
	}
	else if(lParam==2){
		return const_cast<char *>(PositionToolTip(nID));
	}
	else if(lParam==3){
		switch(nID){
		case IDC_EDIT1:
		case IDC_SPIN1:
			return "Stretch in percent";
		case IDC_EDIT2:
		case IDC_SPIN2:
			return "Rotation in degrees";
		case IDC_BUTTON1:
			return "Reset to default values";
		case IDC_CHECK1:
			return "When this button is clicked, it will stay down\nWhile down, add 4 points in the Fractal view\nthese points will be connected 2 and 2 by lines\nThe program will automatically attempt to\nmake the angle perpendicular";
		case IDOK:
			return "Apply and close";
		case IDCANCEL:
			return "Close and undo";
		}
	}
	else if(lParam==4){
		return const_cast<char *>(ExamineToolTip(nID));
	}
	else if(lParam==5){
		switch (nID)
		{
			case IDC_STOREZOOM_KFB: return "Save KFB map files for each frame";
			case IDC_STOREZOOM_PNG: return "Save PNG image files for each frame";
			case IDC_STOREZOOM_JPG: return "Save JPEG image files for each frame";
			case IDC_STOREZOOM_COUNTAUTO: return "Automatically stop when completely zoomed out";
			case IDC_STOREZOOM_COUNT: return "Render this many frames (if auto is unchecked)";
			case IDOK: return "Apply and close";
			case IDCANCEL: return "Close and undo";
		}
	}
	static char szTmp[128];
	wsprintf(szTmp,"nID=%d, lParam=%d",nID,lParam);
	return szTmp;
#ifdef KF_OPENCL
		case IDC_COMBO_OPENCL_DEVICE:
			return "Select the OpenCL device to use for per-pixel iteration calculations";
#endif
}
extern char *GetToolText(int nID, LPARAM lParam)
{
	return const_cast<char *>(GetToolText_const(nID, lParam));
}


// settings update

static void UpdateZoomSize(HWND hWnd)
{
	double z = g_SFT.GetZoomSize();
	CheckMenuItem(GetMenu(hWnd),ID_ACTIONS_ZOOMSIZE_2,MF_BYCOMMAND|(z==2?MF_CHECKED:MF_UNCHECKED));
	CheckMenuItem(GetMenu(hWnd),ID_ACTIONS_ZOOMSIZE_4,MF_BYCOMMAND|(z==4?MF_CHECKED:MF_UNCHECKED));
	CheckMenuItem(GetMenu(hWnd),ID_ACTIONS_ZOOMSIZE_8,MF_BYCOMMAND|(z==8?MF_CHECKED:MF_UNCHECKED));
	CheckMenuItem(GetMenu(hWnd),ID_ACTIONS_ZOOMSIZE_16,MF_BYCOMMAND|(z==16?MF_CHECKED:MF_UNCHECKED));
	CheckMenuItem(GetMenu(hWnd),ID_ACTIONS_ZOOMSIZE_32,MF_BYCOMMAND|(z==32?MF_CHECKED:MF_UNCHECKED));
	CheckMenuItem(GetMenu(hWnd),ID_ACTIONS_ZOOMSIZE_64,MF_BYCOMMAND|(z==64?MF_CHECKED:MF_UNCHECKED));
	CheckMenuItem(GetMenu(hWnd),ID_ACTIONS_ZOOMSIZE_128,MF_BYCOMMAND|(z==128?MF_CHECKED:MF_UNCHECKED));
}

static void UpdateAnimateZoom(HWND hWnd)
{
	bool b = g_SFT.GetAnimateZoom();
	CheckMenuItem(GetMenu(hWnd),ID_ACTIONS_ANIMATEZOOM,MF_BYCOMMAND|(b?MF_CHECKED:MF_UNCHECKED));
}

static void UpdateArbitrarySize(HWND hWnd)
{
	bool b = g_SFT.GetArbitrarySize();
	CheckMenuItem(GetMenu(hWnd),ID_SPECIAL_ARBITRARYSIZE,MF_BYCOMMAND|(b?MF_CHECKED:MF_UNCHECKED));
}

static void UpdateReuseReference(HWND hWnd)
{
	bool b = g_SFT.GetReuseReference();
	CheckMenuItem(GetMenu(hWnd),ID_ACTIONS_REUSEREFERENCE,MF_BYCOMMAND|(b?MF_CHECKED:MF_UNCHECKED));
}

static void UpdateAutoSolveGlitches(HWND hWnd)
{
	bool b = g_SFT.GetAutoSolveGlitches();
	CheckMenuItem(GetMenu(hWnd),ID_ACTIONS_AUTOSOLVEGLITCHES,MF_BYCOMMAND|(b?MF_CHECKED:MF_UNCHECKED));
}

static void UpdateSolveGlitchNear(HWND hWnd)
{
	bool b = g_SFT.GetSolveGlitchNear();
	CheckMenuItem(GetMenu(hWnd),ID_ACTIONS_SPECIAL_SOLVEGLITCHWITHNEARPIXELSMETHOD,MF_BYCOMMAND|(b?MF_CHECKED:MF_UNCHECKED));
}

static void UpdateNoApprox(HWND hWnd)
{
	bool b = g_SFT.GetNoApprox();
	CheckMenuItem(GetMenu(hWnd),ID_ACTIONS_SPECIAL_NOAPPROXIMATION,MF_BYCOMMAND|(b?MF_CHECKED:MF_UNCHECKED));
}

static void UpdateMirror(HWND hWnd)
{
	bool b = g_SFT.GetMirror();
	CheckMenuItem(GetMenu(hWnd),ID_ACTIONS_SPECIAL_SPECIAL_MIRROR1,MF_BYCOMMAND|(b?MF_CHECKED:MF_UNCHECKED));
}

static void UpdateLongDoubleAlways(HWND hWnd)
{
	bool b = g_SFT.GetLongDoubleAlways();
	CheckMenuItem(GetMenu(hWnd),ID_ACTIONS_SPECIAL_USELONGDOUBLEFROMSTART,MF_BYCOMMAND|(b?MF_CHECKED:MF_UNCHECKED));
}

static void UpdateFloatExpAlways(HWND hWnd)
{
	bool b = g_SFT.GetFloatExpAlways();
	CheckMenuItem(GetMenu(hWnd),ID_ACTIONS_SPECIAL_USEFLOATEXPALWAYS,MF_BYCOMMAND|(b?MF_CHECKED:MF_UNCHECKED));
}

static void UpdateAutoIterations(HWND hWnd)
{
	bool b = g_SFT.GetAutoIterations();
	CheckMenuItem(GetMenu(hWnd),ID_ACTIONS_SPECIAL_AUTOITERATION,MF_BYCOMMAND|(b?MF_CHECKED:MF_UNCHECKED));
}

static void UpdateGuessing(HWND hWnd)
{
	bool b = g_SFT.GetGuessing();
	CheckMenuItem(GetMenu(hWnd),ID_ACTIONS_SPECIAL_GUESSING,MF_BYCOMMAND|(b?MF_CHECKED:MF_UNCHECKED));
}

static void UpdateShowGlitches(HWND hWnd)
{
	bool b = g_SFT.GetShowGlitches();
	CheckMenuItem(GetMenu(hWnd),ID_SPECIAL_SHOWGLITCHES,MF_BYCOMMAND|(b?MF_CHECKED:MF_UNCHECKED));
}

static void UpdateNoReuseCenter(HWND hWnd)
{
	bool b = g_SFT.GetNoReuseCenter();
	CheckMenuItem(GetMenu(hWnd),ID_ACTIONS_SPECIAL_NOREUSECENTER,MF_BYCOMMAND|(b?MF_CHECKED:MF_UNCHECKED));
}

static void UpdateMenusFromSettings(HWND hWnd)
{
	UpdateZoomSize(hWnd);
	UpdateAnimateZoom(hWnd);
	UpdateArbitrarySize(hWnd);
	UpdateReuseReference(hWnd);
	UpdateAutoSolveGlitches(hWnd);
	UpdateSolveGlitchNear(hWnd);
	UpdateNoApprox(hWnd);
	UpdateMirror(hWnd);
	UpdateLongDoubleAlways(hWnd);
	UpdateFloatExpAlways(hWnd);
	UpdateAutoIterations(hWnd);
	UpdateGuessing(hWnd);
	UpdateShowGlitches(hWnd);
	UpdateNoReuseCenter(hWnd);
}

static void UpdateWindowSize(HWND hWnd)
{
	int l = g_SFT.GetWindowLeft();
	int t = g_SFT.GetWindowTop();
	int r = g_SFT.GetWindowRight();
	int b = g_SFT.GetWindowBottom();
	MoveWindow(hWnd,l,t,r,b,TRUE);
}

#if 0
static long WINAPI ShowProc(HWND hWnd,UINT uMsg,WPARAM wParam,LPARAM lParam)
{
	if(uMsg==WM_PAINT){
		PAINTSTRUCT ps;
		BeginPaint(hWnd,&ps);
		int i;
		RECT cr;
		GetClientRect(hWnd,&cr);
		for(i=0;i<1024;i++){
			RECT r={i*cr.right/1024,0,(i+1)*cr.right/1024,cr.bottom};
			COLOR14 c = g_SFT.GetColor(i);
			HBRUSH br = CreateSolidBrush(RGB(c.b,c.g,c.r));
			FillRect(ps.hdc,&r,br);
			DeleteObject(br);
		}
		EndPaint(hWnd,&ps);
		return 0;
	}
	return CallWindowProc((WNDPROC)GetClassLongPtr(hWnd,GCLP_WNDPROC),hWnd,uMsg,lParam,wParam);
}
#endif


static char *Trim(char *sz,int nLen=-1)
{
	if(nLen==-1)
		nLen = strlen(sz);
	if(nLen>(int)strlen(sz))
		nLen = strlen(sz);
	int i;
	for(i=nLen;i>0 && (sz[i-1]==(char)-96 || sz[i-1]==' ' || sz[i-1]=='\r' || sz[i-1]=='\n' || sz[i-1]=='\t');i--);
	sz[i]=0;
	while(*sz==' ')
		sz++;
	return sz;
}
static int WINAPI CrossHairProc(HWND hWnd,UINT uMsg,WPARAM wParam,LPARAM lParam)
{
	(void) wParam;
	(void) lParam;
	if(uMsg==WM_INITDIALOG){
		SendMessage(hWnd, WM_SETICON, ICON_SMALL, LPARAM(g_hIcon));
		SendMessage(hWnd, WM_SETICON, ICON_BIG, LPARAM(g_hIcon));
		SetTimer(hWnd,1,50,NULL);

		HWND hwPrev = GetDlgItem(hWnd,IDC_PREV);
		RECT r, r2={0,0,16,16};
		GetWindowRect(hwPrev,&r);
		ScreenToClient(hWnd,(LPPOINT)&r);
		r.right=r2.right*8;
		r.bottom=r2.bottom*8;
		MoveWindow(hwPrev,r.left,r.top,r.right,r.bottom,TRUE);
	}
	else if(uMsg==WM_USER+112){
		HWND hwPrev = GetDlgItem(hWnd,IDC_PREV);
		RECT rc;
		GetClientRect(hWnd,&rc);
		MoveWindow(hwPrev,0,0,rc.right,rc.bottom,TRUE);
	}
	else if(uMsg==WM_TIMER){
		POINT p;
		GetCursorPos(&p);
		if(hWnd==WindowFromPoint(p) || hWnd==GetParent(WindowFromPoint(p)))
			return 0;
		HDC dcScreen = GetDC(NULL);

		HWND hwPrev = GetDlgItem(hWnd,IDC_PREV);
		RECT r, r2={0,0,16,16};
		GetWindowRect(hwPrev,&r);
		ScreenToClient(hWnd,(LPPOINT)&r);
		r.right=r2.right*8;
		r.bottom=r2.bottom*8;

		HDC dcPrev = GetDC(hwPrev);
		r2.left = p.x-r2.right/2+1;
		r2.top = p.y-r2.bottom/2+1;
		StretchBlt(dcPrev,0,0,r.right,r.bottom,dcScreen,r2.left,r2.top,r2.right,r2.bottom,SRCCOPY);

		SetROP2(dcPrev,R2_NOT);
		MoveToEx(dcPrev,r.right/2-4,0,NULL);
		LineTo(dcPrev,r.right/2-4,r.bottom);
		MoveToEx(dcPrev,0,r.bottom/2-4,NULL);
		LineTo(dcPrev,r.right,r.bottom/2-4);

		ReleaseDC(hwPrev,dcPrev);
		ReleaseDC(NULL,dcScreen);
	}
	return 0;
}
/*
int g_nFrames=0;
int WINAPI ZoomProc(HWND hWnd,UINT uMsg,WPARAM wParam,LPARAM lParam)
{
	if(uMsg==WM_INITDIALOG){
		RECT r;
		GetClientRect(GetParent(hWnd),&r);
		SetDlgItemInt(hWnd,IDC_EDIT3,r.right,0);
		return 1;
	}
	if(uMsg==WM_COMMAND){
		if(wParam==IDC_BUTTON1){
			char szFolder[256];
			GetDlgItemText(hWnd,IDC_EDIT1,szFolder,sizeof(szFolder));
			if(Browse(hWnd,szFolder,sizeof(szFolder)))
				SetDlgItemText(hWnd,IDC_EDIT1,szFolder);
		}
		else if(wParam==IDC_BUTTON3){
			int nProcent = GetDlgItemInt(hWnd,IDC_EDIT2,NULL,FALSE);
			if(!nProcent)
				return MessageBox(hWnd,"No percent given","Error",MB_OK|MB_ICONSTOP);
			SetDlgItemInt(hWnd,IDC_EDIT5,g_SFT.CountFrames(SendDlgItemMessage(hWnd,IDC_CHECK1,BM_GETCHECK,0,0)?25:nProcent),FALSE);
		}
		else if(LOWORD(wParam)==IDC_EDIT3 && HIWORD(wParam)==EN_CHANGE){
			int nHeight = GetDlgItemInt(hWnd,IDC_EDIT3,NULL,0)*360/640;
			SetDlgItemInt(hWnd,IDC_EDIT4,nHeight,0);
		}
		else if(wParam==IDOK){
			if(g_bZoomRunning){
				g_bZoomStop=TRUE;
				while(g_bZoomRunning)
					Sleep(10);
				SetDlgItemText(hWnd,IDOK,"Start");
				return 0;
			}
			char szFolder[256];
			GetDlgItemText(hWnd,IDC_EDIT1,szFolder,sizeof(szFolder));
			if(!*szFolder){
				SetFocus(GetDlgItem(hWnd,IDC_EDIT1));
				return MessageBox(hWnd,"No folder given","Error",MB_OK|MB_ICONSTOP);
			}
			int nWidth = GetDlgItemInt(hWnd,IDC_EDIT3,NULL,0);
			if(!nWidth){
				SetFocus(GetDlgItem(hWnd,IDC_EDIT3));
				return MessageBox(hWnd,"No width given","Error",MB_OK|MB_ICONSTOP);
			}
			int nHeight = GetDlgItemInt(hWnd,IDC_EDIT4,NULL,0);
			int nPercent = GetDlgItemInt(hWnd,IDC_EDIT2,NULL,0);
			if(!nPercent){
				SetFocus(GetDlgItem(hWnd,IDC_EDIT2));
				return MessageBox(hWnd,"No percent given","Error",MB_OK|MB_ICONSTOP);
			}
			g_nFrames = GetDlgItemInt(hWnd,IDC_EDIT5,NULL,0);
			if(!g_nFrames){
				SetFocus(GetDlgItem(hWnd,IDC_EDIT5));
				return MessageBox(hWnd,"No frames given","Error",MB_OK|MB_ICONSTOP);
			}
			BOOL bKfr = SendDlgItemMessage(hWnd,IDC_CHECK1,BM_GETCHECK,0,0);
			char szParams[256];
			strcpy(szParams,szFolder);
			if(szParams[strlen(szParams)-1]!='\\')
				strcat(szParams,"\\");
			strcat(szParams,"Parameters.kfz");
			DWORD dw;
			if(g_SFT.SaveFile(szParams)){
				CStringTable stParameters;
				stParameters.AddRow();
				stParameters.AddString(stParameters.GetCount()-1,"Folder");
				stParameters.AddString(stParameters.GetCount()-1,szFolder);
				stParameters.AddRow();
				stParameters.AddString(stParameters.GetCount()-1,"Width");
				stParameters.AddInt(stParameters.GetCount()-1,nWidth);
				stParameters.AddRow();
				stParameters.AddString(stParameters.GetCount()-1,"Height");
				stParameters.AddInt(stParameters.GetCount()-1,nHeight);
				stParameters.AddRow();
				stParameters.AddString(stParameters.GetCount()-1,"Frames");
				stParameters.AddInt(stParameters.GetCount()-1,g_nFrames);
				stParameters.AddRow();
				stParameters.AddString(stParameters.GetCount()-1,"Percent");
				stParameters.AddInt(stParameters.GetCount()-1,nPercent);
				stParameters.AddRow();
				stParameters.AddString(stParameters.GetCount()-1,"Kfr");
				stParameters.AddInt(stParameters.GetCount()-1,bKfr);
				HANDLE hFile = CreateFile(szParams,GENERIC_READ|GENERIC_WRITE,0,NULL,OPEN_EXISTING,0,NULL);
				if(hFile==INVALID_HANDLE_VALUE)
					return MessageBox(hWnd,"Could not write in given folder","Error",MB_OK|MB_ICONSTOP);
				SetFilePointer(hFile,0,NULL,FILE_END);
				char *szParameters = stParameters.ToText(": ","\r\n");
				WriteFile(hFile,szParameters,strlen(szParameters),&dw,NULL);
				CloseHandle(hFile);
				stParameters.DeleteToText(szParameters);
			}
			SetDlgItemText(hWnd,IDOK,"Stop");
			SetTimer(hWnd,0,500,NULL);
			RENDER_FRAKTAL *prf = new RENDER_FRAKTAL;
			char *sz = g_SFT.GetPosition();
			CStringTable stP(sz,":","\n");
			prf->rstart = stP[stP.FindString(0,"R-Start")][1];
			prf->rstop = stP[stP.FindString(0,"R-Stop")][1];
			prf->istart = stP[stP.FindString(0,"I-Start")][1];
			prf->istop = stP[stP.FindString(0,"I-Stop")][1];
			prf->nFrames = g_nFrames;
			prf->nProcent = (bKfr?25:nPercent);
			prf->nMaxIter = g_SFT.GetIterations();
			prf->r.left=prf->r.top=0;
			prf->r.right = nWidth;
			prf->r.bottom = nHeight;
			prf->hWnd = hWnd;
			prf->kfrParts = (int)(bKfr?(0.5 + log10(1.5)/log10(1 + 2*(double)nPercent/100)):0);
			if(prf->kfrParts<1 && bKfr)
				prf->kfrParts=2;
			strcpy(prf->szFileName,szFolder);
			if(prf->szFileName[strlen(prf->szFileName)-1]!='\\')
				strcat(prf->szFileName,"\\");
			strcat(prf->szFileName,"000000.jpg");
			prf->bCheckZoom=TRUE;
			g_bZoomRunning=TRUE;
			g_bZoomStop=FALSE;
			HANDLE hThread = CreateThread(NULL,150000000,(LPTHREAD_START_ROUTINE)RenderFraktal,(LPVOID)prf,0,&dw);
			CloseHandle(hThread);
//			g_SFT.CreateZoomSequence(szFolder,nWidth,nHeight,g_nFrames,nPercent);
		}
		else if(wParam==IDC_BUTTON4){
			static char szFile[256]={0};
			if(BrowseFile(hWnd,TRUE,"Open Incomplete Zoom Sequence","Zoom parameters\0*.kfz\0",szFile,sizeof(szFile))){
				DWORD dw;
				HANDLE hFile = CreateFile(szFile,GENERIC_READ,0,NULL,OPEN_EXISTING,0,NULL);
				if(hFile==INVALID_HANDLE_VALUE)
					return MessageBox(hWnd,"Could not open file","Error",MB_OK|MB_ICONSTOP);
				int nLen = GetFileSize(hFile,NULL);
				char *szData = new char[nLen+1];
				ReadFile(hFile,szData,nLen,&dw,NULL);
				CloseHandle(hFile);
				szData[nLen]=0;
				CStringTable stParameters(szData,": ","\r\n");
				delete [] szData;
				SetDlgItemText(hWnd,IDC_EDIT1,stParameters[stParameters.FindString(0,"Folder")][1]);
				SetDlgItemText(hWnd,IDC_EDIT2,stParameters[stParameters.FindString(0,"Percent")][1]);
				SetDlgItemText(hWnd,IDC_EDIT5,stParameters[stParameters.FindString(0,"Frames")][1]);
				SetDlgItemText(hWnd,IDC_EDIT3,stParameters[stParameters.FindString(0,"Width")][1]);
				SetDlgItemText(hWnd,IDC_EDIT4,stParameters[stParameters.FindString(0,"Height")][1]);
				SendDlgItemMessage(hWnd,IDC_CHECK1,BM_SETCHECK,atoi(stParameters[stParameters.FindString(0,"Kfr")][1]),0);
				if(g_hwColors)
					SendMessage(g_hwColors,WM_USER+99,0,0);
				g_SFT.OpenFile(szFile);
			}
		}
		else if(wParam==IDCANCEL){
			if(g_bZoomRunning){
				g_bZoomStop=TRUE;
				while(g_bZoomRunning)
					Sleep(10);
			}
			EndDialog(hWnd,0);
		}
	}
	else if(uMsg==WM_TIMER){
		char szDisplay[256];
		static char szPrev[256]="";
		wsprintf(szDisplay,"%d%% G:%d%% (%d of %d) %s",g_nDone*100/(g_nMax?g_nMax:1),g_nGuessed*100/(!g_nDone?1:g_nDone),g_nFrameDone,g_nFrameMax,g_pszStatus[g_nStatus]);
		if(strcmp(szDisplay,szPrev)){
			strcpy(szPrev,szDisplay);
			SetDlgItemText(hWnd,IDC_EDIT6,szDisplay);
		}
	}
/ *	else if(uMsg==WM_TIMER){
		static char szPev[256]={0};
		char szStatus[256];
		if(g_nFrames==g_SFT.GetFramesDone()){
			strcpy(szStatus,"Done");
			g_bZoomRunning=FALSE;
			SetDlgItemText(hWnd,IDOK,"Start");
		}
		else
			wsprintf(szStatus,"Frame %d (%s)",g_SFT.GetFramesDone(),g_SFT.GetZoomStatus());
		if(strcmp(szPev,szStatus)){
			strcpy(szPev,szStatus);
			SetDlgItemText(hWnd,IDC_EDIT6,szStatus);
		}
	}
	return 0;
}
*/
struct JPEG_PARAMS
{
	int nWidth;
	int nHeight;
	int nQuality;
}g_JpegParams;
static int WINAPI JpegProc(HWND hWnd,UINT uMsg,WPARAM wParam,LPARAM lParam)
{
	if(uMsg==WM_INITDIALOG){
		SendMessage(hWnd, WM_SETICON, ICON_SMALL, LPARAM(g_hIcon));
		SendMessage(hWnd, WM_SETICON, ICON_BIG, LPARAM(g_hIcon));
		SetDlgItemInt(hWnd,IDC_EDIT1,g_JpegParams.nWidth,0);
		SetDlgItemInt(hWnd,IDC_EDIT3,g_JpegParams.nHeight,0);
		if(lParam){
			ShowWindow(GetDlgItem(hWnd,IDC_EDIT4),FALSE);
			ShowWindow(GetDlgItem(hWnd,IDC_QUALITY_LABEL),FALSE);
			SetWindowText(hWnd,"Size");
		}
		else
			SetDlgItemInt(hWnd,IDC_EDIT4,g_JpegParams.nQuality,0);
		if(lParam==2){
			SetWindowText(hWnd,"Set Ratio");
			SendDlgItemMessage(hWnd,IDC_EDIT1,EM_SETREADONLY,TRUE,0);
			SendDlgItemMessage(hWnd,IDC_EDIT3,EM_SETREADONLY,FALSE,0);
			SetFocus(GetDlgItem(hWnd,IDC_EDIT3));
		}
		else if(lParam==3 && g_SFT.GetArbitrarySize())
			SendDlgItemMessage(hWnd,IDC_EDIT3,EM_SETREADONLY,FALSE,0);
		return 1;
	}
	else if(uMsg==WM_COMMAND){
		if(wParam==IDCANCEL)
			EndDialog(hWnd,0);
		else if(wParam==IDOK){
			g_JpegParams.nWidth = GetDlgItemInt(hWnd,IDC_EDIT1,NULL,0);
			g_JpegParams.nHeight = GetDlgItemInt(hWnd,IDC_EDIT3,NULL,0);
			g_JpegParams.nQuality = GetDlgItemInt(hWnd,IDC_EDIT4,NULL,0);
			EndDialog(hWnd,1);
		}
		else if(LOWORD(wParam)==IDC_EDIT1 && HIWORD(wParam)==EN_CHANGE){
			//int nHeight = GetDlgItemInt(hWnd,IDC_EDIT1,NULL,0)*(double)g_SFT.GetRatioY()/(double)g_SFT.GetRatioX();
			int nHeight = GetDlgItemInt(hWnd,IDC_EDIT1,NULL,0)*(double)g_SFT.GetHeight()/(double)g_SFT.GetWidth();
			SetDlgItemInt(hWnd,IDC_EDIT3,nHeight,0);
		}
	}
	return 0;
}
struct ANIM
{
	HBITMAP bmBmp;
	HWND hWnd;
	POINT pOffs;
	BOOL bZoomOut;
	BOOL bZoomOne;
	int nZoomSize;
	int nPos;
};
int g_nAnim=0;
bool g_bAnim=false;
static void UpdateBkpImage(ANIM *pAnim)
{
	double zoomDiff = pAnim->nZoomSize;
	if(pAnim->bZoomOut)
		zoomDiff = 1/zoomDiff;
	if(pAnim->bZoomOne)
		zoomDiff = 1;
	RECT r;
	GetClientRect(pAnim->hWnd,&r);
	RECT sr;
	GetWindowRect(g_hwStatus,&sr);
	sr.bottom-=sr.top;
	r.bottom-=sr.bottom;
	int nToXStart = pAnim->pOffs.x-r.right/(zoomDiff*2);
	int nToYStart = pAnim->pOffs.y-r.bottom/(zoomDiff*2);
	int nToXStop = r.right - (pAnim->pOffs.x+r.right/(zoomDiff*2));
	int nToYStop = r.bottom - (pAnim->pOffs.y+r.bottom/(zoomDiff*2));

	HDC hDC = GetDC(NULL);
	HDC dcBmp = CreateCompatibleDC(hDC);
	HBITMAP bmOld = (HBITMAP)SelectObject(dcBmp,pAnim->bmBmp);
	HDC dcBkg = CreateCompatibleDC(hDC);
	HBITMAP bmBkgOld = (HBITMAP)SelectObject(dcBkg,g_SFT.GetBitmap());
	if(pAnim->bZoomOut)
		SetStretchBltMode(dcBkg,HALFTONE);
	StretchBlt(dcBkg,0,0,g_SFT.GetWidth(),g_SFT.GetHeight(),dcBmp,nToXStart,nToYStart,r.right-nToXStop-nToXStart,r.bottom-nToYStop-nToYStart,SRCCOPY);
	SelectObject(dcBkg,bmBkgOld);
	g_SFT.UpdateBitmap();
	DeleteDC(dcBkg);
	SelectObject(dcBmp,bmOld);
	DeleteDC(dcBmp);
	ReleaseDC(NULL,hDC);
}
static int WINAPI ThAnim_(ANIM *pAnim)
{
	int nParts = 10;// * log((double)g_SFT.GetZoomSize())/log((double)2);
	g_bAnim=true;
	int pMyID = InterlockedIncrement((LPLONG)&g_nAnim);
	HDC hDC = GetDC(pAnim->hWnd);
	SetStretchBltMode(hDC,HALFTONE);
	HDC dcBmp = CreateCompatibleDC(hDC);
	HBITMAP bmOld = (HBITMAP)SelectObject(dcBmp,pAnim->bmBmp);
	double zoomDiff = pAnim->nZoomSize;
	if(pAnim->bZoomOut)
		zoomDiff = 1/zoomDiff;
	if(pAnim->bZoomOne)
		zoomDiff = 1;
	RECT r;
	GetClientRect(pAnim->hWnd,&r);
	RECT sr;
	GetWindowRect(g_hwStatus,&sr);
	sr.bottom-=sr.top;
	r.bottom-=sr.bottom;
	int nToXStart = pAnim->pOffs.x-r.right/(zoomDiff*2);
	double nToXStartOffs = (double)nToXStart/nParts;//pow(10,log10((double)nToXStart)/(nParts));
	int nToYStart = pAnim->pOffs.y-r.bottom/(zoomDiff*2);
	double nToYStartOffs = (double)nToYStart/nParts;//pow(10,log10((double)nToYStart)/(nParts));
	int nToXStop = r.right - (pAnim->pOffs.x+r.right/(zoomDiff*2));
	double nToXStopOffs = (double)nToXStop/nParts;//pow(10,log10((double)nToXStop)/(nParts));
	int nToYStop = r.bottom - (pAnim->pOffs.y+r.bottom/(zoomDiff*2));
	double nToYStopOffs = (double)nToYStop/nParts;//pow(10,log10((double)nToYStop)/(nParts));
	double nToXStartPos=0;
	double nToYStartPos=0;
	double nToXStopPos=0;
	double nToYStopPos=0;

	int k;
	for(k=0;pMyID==g_nAnim && k<=nParts;k++){
		StretchBlt(hDC,0,0,r.right,r.bottom,dcBmp,nToXStartPos,nToYStartPos,r.right-nToXStopPos-nToXStartPos,r.bottom-nToYStopPos-nToYStartPos,SRCCOPY);
		if(k==nParts)
			break;
		nToXStartPos+=nToXStartOffs;
		nToYStartPos+=nToYStartOffs;
		nToXStopPos+=nToXStopOffs;
		nToYStopPos+=nToYStopOffs;
		Sleep(7);
	}
	if(pMyID==g_nAnim && !g_SFT.m_bRunning){ // Render is done during animation
//		g_SFT.ApplyColors();
		InvalidateRect(pAnim->hWnd,NULL,FALSE);
	}
	if(pMyID==g_nAnim)
		g_bAnim=false;
	SelectObject(dcBmp,bmOld);
	DeleteDC(dcBmp);
	ReleaseDC(pAnim->hWnd,hDC);
	DeleteObject(pAnim->bmBmp);
	delete pAnim;
	return 0;
}
static int WINAPI ThAnim(ANIM *pAnim)
{
//#ifndef _DEBUG
	try{
//#endif
		return ThAnim_(pAnim);
//#ifndef _DEBUG
	}catch(...){
		char szPos[100];
		wsprintf(szPos,"Krash: %d",pAnim->nPos);
		MessageBox(pAnim->hWnd,szPos,"Krash",MB_OK);
	}
//#endif
	return 0;
}

extern std::string replace_path_filename(const std::string &path, const std::string &file)
{
	size_t slash = path.rfind('\\');
	if (slash == std::string::npos)
	{
		return file;
	}
	else
	{
		return path.substr(0, slash + 1) + file;
	}
}

extern std::string replace_path_extension(const std::string &path, const std::string &ext)
{
	size_t dot = path.rfind('.');
	if (dot == std::string::npos)
	{
		return path + "." + ext;
	}
	else
	{
		return path.substr(0, dot + 1) + ext;
	}
}

extern std::string get_filename_path(const std::string &path)
{
	size_t slash = path.rfind('\\');
	if (slash == std::string::npos)
	{
		return "";
	}
	else
	{
		return path.substr(0, slash + 1);
	}
}

extern std::string get_filename_extension(const std::string &path)
{
	size_t dot = path.rfind('.');
	if (dot == std::string::npos)
	{
		return "";
	}
	else
	{
		return path.substr(dot + 1);
	}
}

extern std::string get_filename_zoom_string(const std::string &file)
{
	size_t begin = file.rfind('_');
	size_t end = file.rfind('.');
	assert(begin != std::string::npos);
	assert(end != std::string::npos);
	assert(begin < end);
	return file.substr(begin + 1, end - (begin + 1));
}

extern std::string store_zoom_filename(int n, const std::string &z, const std::string &ext)
{
	std::ostringstream os;
	os << std::setfill('0') << std::setw(5) << n << "_" << z << "." << ext;
	return os.str();
}

static int ResumeZoomSequence(HWND hWnd)
{
	g_szFile = "";
	if(!BrowseFile(hWnd,TRUE,"Open location","Kalle's fraktaler\0*.kfr\0\0",g_szFile))
		return 0;
	if(!g_SFT.OpenFile(g_szFile))
		return MessageBox(hWnd,"Could not open file","Error",MB_OK|MB_ICONSTOP);
	g_SFT.StoreLocation();
	g_szFile = replace_path_filename(g_szFile, "*_*.jpg");
	WIN32_FIND_DATA fd;
	HANDLE hFind = FindFirstFile(g_szFile.c_str(),&fd);
	int countJpg = 0;
	if(hFind!=INVALID_HANDLE_VALUE){
		g_bStoreZoomJpg=1;
		do{
			countJpg++;
		}while(FindNextFile(hFind,&fd));
		FindClose(hFind);
	}
	else
		g_bStoreZoomJpg=0;
	g_szFile = replace_path_filename(g_szFile, "*_*.png");
	hFind = FindFirstFile(g_szFile.c_str(),&fd);
	int countPng = 0;
	if(hFind!=INVALID_HANDLE_VALUE){
		g_bStoreZoomPng=1;
		do{
			countPng++;
		}while(FindNextFile(hFind,&fd));
		FindClose(hFind);
	}
	else
		g_bStoreZoomPng=0;
	g_szFile = replace_path_filename(g_szFile, "*_*.kfb");
	hFind = FindFirstFile(g_szFile.c_str(),&fd);
	if(hFind!=INVALID_HANDLE_VALUE){
		g_bStoreZoomMap=1;
	}
	else
		g_bStoreZoomMap=0;

	std::vector<std::string> stExamine;
	int countMap = 0;
	if(hFind!=INVALID_HANDLE_VALUE){
		do{
			countMap++;
			g_szFile = replace_path_filename(g_szFile, fd.cFileName);
			stExamine.push_back(g_szFile);
		}while(FindNextFile(hFind,&fd));
		FindClose(hFind);
	}
	std::sort(stExamine.begin(), stExamine.end());
	std::reverse(stExamine.begin(), stExamine.end());
	BOOL bRecoveryFile=FALSE;

	g_szFile = replace_path_filename(g_szFile, "recovery.kfb");
	hFind = FindFirstFile(g_szFile.c_str(),&fd);
	if(hFind!=INVALID_HANDLE_VALUE){
		FindClose(hFind);
		g_SFT.OpenMapB(g_szFile);
		bRecoveryFile=TRUE;
	}
	else{
		g_szFile = replace_path_filename(g_szFile, "last.kfb");
		hFind = FindFirstFile(g_szFile.c_str(),&fd);
		if(hFind!=INVALID_HANDLE_VALUE){
			FindClose(hFind);
			g_SFT.OpenMapB(g_szFile);
			bRecoveryFile=FALSE;
		}
		else{
			if(stExamine.size())
				g_SFT.OpenMapB(stExamine[0]);
			else
				return MessageBox(hWnd,"Could not browse kfb files","Error",MB_OK|MB_ICONSTOP);
		}
	}

	if(stExamine.size()<2)
		g_SFT.SetZoomSize(2);
	else{
		CDecNumber A(get_filename_zoom_string(stExamine[0]));
		CDecNumber B(get_filename_zoom_string(stExamine[1]));
		g_SFT.SetZoomSize((B/A+CDecNumber(0.5)).ToInt());
	}
	UpdateZoomSize(hWnd);
	int zoomCount = countMap ? countMap
	              : countPng ? countPng
	              : countJpg ? countJpg
	              : stExamine.size();
	if(zoomCount){
		CDecNumber A = CDecNumber(g_SFT.GetZoom())/(CDecNumber(g_SFT.GetZoomSize())^(zoomCount-(bRecoveryFile?0:1)));
		char *szR = g_SFT.GetRe();
		char *szRe = new char[strlen(szR)+1];
		strcpy(szRe,szR);
		char *szI = g_SFT.GetIm();
		char *szIm = new char[strlen(szI)+1];
		strcpy(szIm,szI);
		g_SFT.SetPosition(szRe,szIm,A.ToText());
		delete[] szRe;
		delete[] szIm;
	}

	g_nStoreZoomCount = 0;
	// g_nStoreZoomLimit = 0; // FIXME should there be another dialog?
	g_bStoreZoom=zoomCount+1;
	g_JpegParams.nWidth = g_SFT.GetWidth();
	g_JpegParams.nHeight = g_SFT.GetHeight();
	g_JpegParams.nQuality = 100;
	//g_SFT.RenderFractal(g_SFT.GetWidth(),g_SFT.GetHeight(),g_SFT.GetIterations(),hWnd);
	if(g_SFT.GetAutoIterations()){
		int nMin, nMax, nIter;
		g_SFT.GetIterations(nMin,nMax);
		nIter = g_SFT.GetIterations();
		if(nIter<nMax)
			g_SFT.SetIterations(nMax); // increase iterations
		nMax = g_SFT.GetMaxExceptCenter();//GetIterationOnPoint(g_SFT.GetWidth()/2-1,g_SFT.GetHeight()/2-1);
		if(nMax<g_SFT.GetIterations()/3)
			g_SFT.SetIterations(nMax*3>1000?nMax*3:1000); // decrease iterations
	}
	if(bRecoveryFile){
		g_SFT.ToZoom();
		g_SFT.AddReference(g_JpegParams.nWidth/2,g_JpegParams.nHeight/2,FALSE,FALSE,TRUE);
	}
	else
	{
		bool bReuseCenter = (g_SFT.GetZoomSize() == round(g_SFT.GetZoomSize()));
		g_SFT.Zoom(g_JpegParams.nWidth/2,g_JpegParams.nHeight/2,1/(double)g_SFT.GetZoomSize(),g_JpegParams.nWidth,g_JpegParams.nHeight,bReuseCenter/* TRUE */ /* !g_bAutoGlitch */);
	}
	SetTimer(hWnd,0,500,NULL);
	return 0;
}
int g_nHandleDone=0;
int g_bSkewAnimation=0;
double g_dbSkewToRatio=0;
double g_dbSkewToAnim=0;
double g_dbSkewRatio=0;
double g_dbSkewAnim=0;
double g_dbRotateRatio=0;
double g_dbRotateStart=0;
double g_dbRotateAnim=0;
int g_nSkewFrames=300;
BOOL g_bMarilyn=FALSE;
int g_nMarilynDir=0;
int g_nMarilynX=0;
int g_nMarilynY=0;

static double CompareBitmaps(HBITMAP bm1, HBITMAP bm2)
{
	BITMAPINFOHEADER bmi={sizeof(BITMAPINFOHEADER)};
	BITMAPINFOHEADER bmi2={sizeof(BITMAPINFOHEADER)};
	HDC hDC = GetDC(NULL);
	if(!GetDIBits(hDC,bm1,0,0,NULL,(LPBITMAPINFO)&bmi,DIB_RGB_COLORS))
		return 0;
	if(!GetDIBits(hDC,bm2,0,0,NULL,(LPBITMAPINFO)&bmi2,DIB_RGB_COLORS))
		return 0;
	if(bmi.biWidth!=bmi2.biWidth || bmi.biHeight!=bmi2.biHeight)
		return 0;
	bmi.biCompression=bmi.biClrUsed=bmi.biClrImportant=0;
	bmi.biBitCount = 24;
	int row = ((((bmi.biWidth*(DWORD)bmi.biBitCount)+31)&~31) >> 3);
	bmi.biSizeImage=row*bmi.biHeight;
	BYTE *lpBits1 = new BYTE[bmi.biSizeImage];
	BYTE *lpBits2 = new BYTE[bmi.biSizeImage];
	if(!GetDIBits(hDC,bm1,0,bmi.biHeight,lpBits1,
			(LPBITMAPINFO)&bmi,DIB_RGB_COLORS)){
		delete[] lpBits1;
		delete[] lpBits2;
		return 0;
	}
	if(!GetDIBits(hDC,bm2,0,bmi.biHeight,lpBits2,
			(LPBITMAPINFO)&bmi,DIB_RGB_COLORS)){
		delete[] lpBits1;
		delete[] lpBits2;
		return 0;
	}
	int x, y;
	int nLM=0;
	double ret=0;
	for(y=0;y<bmi.biHeight;y++){
		for(x=0;x<bmi.biWidth;x++){
			int nIndex = x*3 + (bmi.biHeight-1-y)*row;
			int C1 = (lpBits1[nIndex] + lpBits1[nIndex+1] + lpBits1[nIndex+2])/3;
			int C2 = (lpBits2[nIndex] + lpBits2[nIndex+1] + lpBits2[nIndex+2])/3;
			nLM+=(C1-C2)*(C1-C2);
			if(nLM>1000000){
				int n = (nLM/1000000)*1000000;
				ret+=n;
				nLM-=n;
			}
		}
	}
	ret+=nLM;
	delete[] lpBits1;
	delete[] lpBits2;
	return ret;
}

#if 0
static int KRYield(HWND hWnd)
{
	UpdateWindow(hWnd);
	MSG msg;
	while(PeekMessage(&msg,hWnd,0,0,PM_REMOVE)){
		if(msg.message==WM_KEYDOWN && msg.wParam==VK_ESCAPE)
			return 0;
		if(IsDialogMessage(hWnd,&msg))
			continue;
		TranslateMessage(&msg);
		DispatchMessage(&msg);
	}
	return 1;
}
#endif

static int Marilyn(HWND hWnd)
{
	g_SFT.ApplyColors();
	InvalidateRect(hWnd,NULL,FALSE);
	UpdateWindow(hWnd);
	g_bmMarilyn = GetImage("C:\\Users\\krunmo\\Documents\\bkp\\img\\Marilyn.jpg");
	g_SFT.ApplyColors();
	InvalidateRect(hWnd,NULL,FALSE);
	UpdateWindow(hWnd);
	HBITMAP bmBmp = g_SFT.GetBitmap();
	double t = CompareBitmaps(bmBmp,g_bmMarilyn);
	if(g_nMinDiff==0 || g_nMinDiff>t){
		g_nMinDiff=t;
		g_nMarilynDir=0;
		g_nMarilynX=g_nMarilynY=0;
		g_SFT.SaveFile("C:\\Users\\krunmo\\Documents\\bkp\\img\\Marilyn.kfr");
	}
	char szTitle[1256];
	sprintf(szTitle,"Diff=%.0f, Dir=%d, t=%.0f <%d,%d>",g_nMinDiff,g_nMarilynDir,t,g_nMarilynX,g_nMarilynY);
	SetWindowText(hWnd,szTitle);
	RECT r;
	GetClientRect(hWnd,&r);
	int x = 800/2;
	int y = 450/2;
	int nOffs = 1 + g_nMarilynDir/30;
	int nDir = g_nMarilynDir%30;
	if(nDir==0){
		x+=nOffs;
		g_nMarilynX+=nOffs;
	}
	else if(nDir==1){
		x-=nOffs;
		g_nMarilynX-=nOffs;
		y-=nOffs;
		g_nMarilynY-=nOffs;
	}
	else if(nDir==2){
		x-=nOffs;
		g_nMarilynX-=nOffs;
		y+=nOffs;
		g_nMarilynY+=nOffs;
	}
	else if(nDir==3){
		x+=nOffs;
		g_nMarilynX+=nOffs;
		y+=nOffs;
		g_nMarilynY+=nOffs;
	}
	else if(nDir==4){
		x-=nOffs;
		g_nMarilynX-=nOffs;
		y-=2*nOffs;
		g_nMarilynY-=2*nOffs;
	}
	else if(nDir==5){
		x+=2*nOffs;
		g_nMarilynX+=2*nOffs;
	}
	else if(nDir==6){
		y+=2*nOffs;
		g_nMarilynY+=2*nOffs;
	}
	else if(nDir==7){
		x-=2*nOffs;
		g_nMarilynX-=2*nOffs;
	}
	else if(nDir==8){
		x+=nOffs;
		g_nMarilynX+=nOffs;
		y-=nOffs;
		g_nMarilynY-=nOffs;
	}
	else if(nDir==9){
		g_Degree+=(double)nOffs/20;
	}
	else if(nDir==10){
		x+=nOffs;
		g_nMarilynX+=nOffs;
	}
	else if(nDir==11){
		x-=nOffs;
		g_nMarilynX-=nOffs;
		y-=nOffs;
		g_nMarilynY-=nOffs;
	}
	else if(nDir==12){
		x-=nOffs;
		g_nMarilynX-=nOffs;
		y+=nOffs;
		g_nMarilynY+=nOffs;
	}
	else if(nDir==13){
		x+=nOffs;
		g_nMarilynX+=nOffs;
		y+=nOffs;
		g_nMarilynY+=nOffs;
	}
	else if(nDir==14){
		x-=nOffs;
		g_nMarilynX-=nOffs;
		y-=2*nOffs;
		g_nMarilynY-=2*nOffs;
	}
	else if(nDir==15){
		x+=2*nOffs;
		g_nMarilynX+=2*nOffs;
	}
	else if(nDir==16){
		y+=2*nOffs;
		g_nMarilynY+=2*nOffs;
	}
	else if(nDir==17){
		x-=2*nOffs;
		g_nMarilynX-=2*nOffs;
	}
	else if(nDir==18){
		x+=nOffs;
		g_nMarilynX+=nOffs;
		y-=nOffs;
		g_nMarilynY-=nOffs;
	}
	else if(nDir==19){
		g_Degree-=(double)nOffs/20;
		g_Degree-=(double)nOffs/20;
	}
	if(nDir==20){
		x+=nOffs;
		g_nMarilynX+=nOffs;
	}
	else if(nDir==21){
		x-=nOffs;
		g_nMarilynX-=nOffs;
		y-=nOffs;
		g_nMarilynY-=nOffs;
	}
	else if(nDir==22){
		x-=nOffs;
		g_nMarilynX-=nOffs;
		y+=nOffs;
		g_nMarilynY+=nOffs;
	}
	else if(nDir==23){
		x+=nOffs;
		g_nMarilynX+=nOffs;
		y+=nOffs;
		g_nMarilynY+=nOffs;
	}
	else if(nDir==24){
		x-=nOffs;
		g_nMarilynX-=nOffs;
		y-=2*nOffs;
		g_nMarilynY-=2*nOffs;
	}
	else if(nDir==25){
		x+=2*nOffs;
		g_nMarilynX+=2*nOffs;
	}
	else if(nDir==26){
		y+=2*nOffs;
		g_nMarilynY+=2*nOffs;
	}
	else if(nDir==27){
		x-=2*nOffs;
		g_nMarilynX-=2*nOffs;
	}
	else if(nDir==28){
		x+=nOffs;
		g_nMarilynX+=nOffs;
		y-=nOffs;
		g_nMarilynY-=nOffs;
	}
	else if(nDir==29){
		g_Degree+=(double)nOffs/20;
	}
	g_nMarilynDir++;

	DeleteObject(g_bmMarilyn);
	g_SFT.Zoom(x,y,1,g_SFT.GetWidth(),g_SFT.GetHeight());
	SetTimer(hWnd,0,500,NULL);
	return 0;
}
static HBITMAP ShrinkBitmap2(HBITMAP bmBmp,int nX, int nY)
{
	HDC hDC = GetDC(NULL);
	HDC dcBmp = CreateCompatibleDC(hDC);
	HDC dcResult = CreateCompatibleDC(hDC);
	BITMAP bm;
	GetObject(bmBmp,sizeof(BITMAP),&bm);
	HBITMAP bmOld = (HBITMAP)SelectObject(dcBmp,bmBmp);
	HBITMAP bmResult = create_bitmap(hDC,nX,nY);
	HBITMAP bmOldResult = (HBITMAP)SelectObject(dcResult,bmResult);
	SetStretchBltMode(dcResult,HALFTONE);
	StretchBlt(dcResult,0,0,nX,nY,dcBmp,0,0,bm.bmWidth,bm.bmHeight,SRCCOPY);
	SelectObject(dcBmp,bmOld);
	SelectObject(dcResult,bmOldResult);
	DeleteDC(dcBmp);
	DeleteDC(dcResult);
	ReleaseDC(NULL,hDC);
	return bmResult;
}
double g_length=0;
double g_degree=0;
HBITMAP g_bmSaveZoomBuff=NULL;
SIZE g_scSaveZoomBuff;
static void SaveZoomImg(const std::string &szFile, const std::string &comment)
{
	HBITMAP bmSave;
	HDC hDC = GetDC(NULL);
	HDC dcBmp = CreateCompatibleDC(hDC);
	HBITMAP bmBmp = g_SFT.GetBitmap();
	HBITMAP bmOldBmp = (HBITMAP)SelectObject(dcBmp,bmBmp);
	BITMAP bm;
	GetObject(bmBmp,sizeof(BITMAP),&bm);

	SIZE scNextZoom = {LONG(g_scSaveZoomBuff.cx*g_SFT.GetZoomSize()),LONG(g_scSaveZoomBuff.cy*g_SFT.GetZoomSize())};
	if(!g_bmSaveZoomBuff){
		scNextZoom.cx = g_scSaveZoomBuff.cx = bm.bmWidth;
		scNextZoom.cy = g_scSaveZoomBuff.cy = bm.bmHeight;
	}
	BOOL bScaled=FALSE;
	if(scNextZoom.cx/bm.bmWidth>2){
		scNextZoom.cx = bm.bmWidth;
		scNextZoom.cy = bm.bmHeight;
		bScaled=TRUE;
	}

	HBITMAP bmTmp = create_bitmap(hDC,scNextZoom.cx,scNextZoom.cy);
	HDC dcSaveZoom = CreateCompatibleDC(hDC);
	HBITMAP dcOldSaveZoom = (HBITMAP)SelectObject(dcSaveZoom,g_bmSaveZoomBuff);
	HDC dcTmp = CreateCompatibleDC(hDC);
	HBITMAP bmOldTmp = (HBITMAP)SelectObject(dcTmp,bmTmp);
	SetStretchBltMode(dcTmp,HALFTONE);
	StretchBlt(dcTmp,0,0,scNextZoom.cx,scNextZoom.cy,dcBmp,0,0,bm.bmWidth,bm.bmHeight,SRCCOPY);

	if(g_bmSaveZoomBuff){
		SIZE offs={LONG((scNextZoom.cx - scNextZoom.cx/g_SFT.GetZoomSize())/2),LONG((scNextZoom.cy - scNextZoom.cy/g_SFT.GetZoomSize())/2)};
		if(bScaled)
			StretchBlt(dcTmp,offs.cx,offs.cy,scNextZoom.cx-2*offs.cx,scNextZoom.cy-2*offs.cy,
				dcSaveZoom,0,0,g_scSaveZoomBuff.cx,g_scSaveZoomBuff.cy,SRCCOPY);
		else
			BitBlt(dcTmp,offs.cx,offs.cy,scNextZoom.cx-2*offs.cx,scNextZoom.cy-2*offs.cy,
				dcSaveZoom,0,0,SRCCOPY);
	}
	SelectObject(dcSaveZoom,dcOldSaveZoom);
	DeleteDC(dcSaveZoom);
	SelectObject(dcTmp,bmOldTmp);
	DeleteDC(dcTmp);
	SelectObject(dcBmp,bmOldBmp);
	DeleteDC(dcBmp);
	DeleteObject(g_bmSaveZoomBuff);
	ReleaseDC(NULL,hDC);
	g_bmSaveZoomBuff = bmTmp;
	bmSave = ShrinkBitmap2(bmTmp,bm.bmWidth,bm.bmHeight);
	SaveImage(szFile,bmSave,100,comment);
	//SaveJpg(szFile,bmTmp,99);
	DeleteObject(bmSave);
	g_scSaveZoomBuff.cx = scNextZoom.cx;
	g_scSaveZoomBuff.cy = scNextZoom.cy;
}
static int HandleDone(HWND hWnd,UINT uMsg,WPARAM wParam,LPARAM lParam,int &nPos)
{
	if(g_bStoreZoom){
		g_szFile = replace_path_filename(g_szFile, "recovery.kfb");
		if(uMsg==WM_USER+199)
			DeleteFile(g_szFile.c_str());
		else{
			g_nHandleDone++;
			if(g_nHandleDone>60){
				g_nHandleDone=0;
				g_SFT.SaveMapB(g_szFile);
			}
		}
	}
nPos=0;
	int nG, nR, nA;
	int nP = g_SFT.GetProgress(&nG,&nR,&nA);
	if(!wParam && uMsg==WM_USER+199 && (!g_bAnim || !g_SFT.GetAnimateZoom())){
		g_SFT.ApplyColors();
		InvalidateRect(hWnd,NULL,FALSE);
	}

nPos=1;
	char szTmp[154];
	wsprintf(szTmp,"%d%% R:%d%% G:%d%% A:%d%%",nP,nR,nG,nA);
	SendMessage(g_hwStatus,SB_SETTEXT,0,(LPARAM)szTmp);
	SYSTEMTIME st;
	__int64 nTStop;
	GetLocalTime(&st);
	SystemTimeToFileTime(&st,(LPFILETIME)&nTStop);
	nTStop-=g_nTStart;
	FileTimeToSystemTime((LPFILETIME)&nTStop,&st);
	if(st.wDay>1)
		st.wHour+=(st.wDay-1)*24;
	wsprintf(szTmp,"Zoom:%s T:%02d:%02d:%02d.%03d",g_SFT.ToZoom(),st.wHour,st.wMinute,st.wSecond,st.wMilliseconds);
nPos=9;
	if(g_bAutoGlitch){
		wsprintf(szTmp+strlen(szTmp)," Ref: %d",g_bAutoGlitch);
	}
nPos=2;
	if(!g_hwExamine && uMsg==WM_USER+199 && !wParam){
nPos=3;
		if(g_bAutoGlitch && g_bAutoGlitch-1<g_SFT.GetMaxReferences()){
			g_bAutoGlitch++;
nPos=4;
			int x, y, d;

			if((d = g_SFT.FindCenterOfGlitch(x, y))){
nPos=5;
				if(g_nPrevGlitchX!=x || g_nPrevGlitchY!=y){
nPos=6;
					if (! g_bInteractive)
					{
						std::cerr << "add reference " << g_bAutoGlitch << " at (" << x << "," << y << ") area " << (d - 1) << std::endl;
						SendMessage(g_hwStatus,SB_SETTEXT,1,(LPARAM)szTmp);
					}
					if(g_SFT.AddReference(x, y,FALSE,g_SFT.GetSolveGlitchNear(),g_bAutoGlitch==g_SFT.GetMaxReferences())){
nPos=7;
						return 0;
					}
				}
			}
			else
				g_bAutoGlitch--;
nPos=8;
		}
	}
	if(g_bAutoGlitch){
		wsprintf(szTmp+strlen(szTmp)," %s", uMsg==WM_USER+199?"Done":"");
nPos=10;
		SendMessage(g_hwStatus,SB_SETTEXT,1,(LPARAM)szTmp);
		if (uMsg==WM_USER+199)
			g_bAutoGlitch=1;
	}
	if(g_hwExamine && uMsg==WM_USER+199)
		PostMessage(g_hwExamine,uMsg,wParam,lParam);
	if(nP && (!g_bAnim || !g_SFT.GetAnimateZoom()))
		InvalidateRect(hWnd,NULL,FALSE);
nPos=11;
	if(uMsg==WM_USER+199){
		KillTimer(hWnd,0);
//			g_nAnim++;
//			g_bAnim=FALSE;
		g_SFT.ApplyColors();
		if(!g_bAnim || !g_SFT.GetAnimateZoom()){
			InvalidateRect(hWnd,NULL,FALSE);
			UpdateWindow(hWnd);
		}
nPos=12;
		if(g_bFindMinibrot){
			PostMessage(hWnd,WM_COMMAND,ID_ACTIONS_FINDMINIBROT,1);
		}
		if(g_bMarilyn)
			return Marilyn(hWnd);
		if(g_bStoreZoom){
nPos=13;
			std::string szZ = g_SFT.ToZoom();
			if(g_bStoreZoomJpg){
				g_szFile = replace_path_filename(g_szFile, store_zoom_filename(g_bStoreZoom, szZ, "jpg"));
				if(g_SFT.GetZoomSize()<2 && !g_bAnimateEachFrame)
					SaveZoomImg(g_szFile, "KF2");
				else
					g_SFT.SaveJpg(g_szFile,100);
			}
			if(g_bStoreZoomPng){
				g_szFile = replace_path_filename(g_szFile, store_zoom_filename(g_bStoreZoom, szZ, "png"));
				if(g_SFT.GetZoomSize()<2 && !g_bAnimateEachFrame)
					SaveZoomImg(g_szFile, "KF2");
				else
					g_SFT.SaveJpg(g_szFile,-1);
			}
				g_szFile = replace_path_filename(g_szFile, store_zoom_filename(g_bStoreZoom, szZ, "kfb"));
			if(!g_bAnimateEachFrame && g_bStoreZoomMap)
				g_SFT.SaveMapB(g_szFile);

			g_szFile = replace_path_filename(g_szFile, "last.kfb");
			if(!g_bAnimateEachFrame && !g_bStoreZoomMap)
				g_SFT.SaveMapB(g_szFile);
nPos=14;
			g_bStoreZoom++;
			g_nStoreZoomCount++;
			if(!std::stod(szZ) || (g_nStoreZoomLimit && g_nStoreZoomCount >= g_nStoreZoomLimit)){
				g_bStoreZoom=FALSE;
				DeleteObject(g_bmSaveZoomBuff);
				g_bmSaveZoomBuff=NULL;
			}
			else{
				if(g_SFT.GetAutoIterations()){
					int nMax = g_SFT.GetMaxExceptCenter();//GetIterationOnPoint(g_SFT.GetWidth()/2-1,g_SFT.GetHeight()/2-1);
					if(nMax<g_SFT.GetIterations()/3)
						g_SFT.SetIterations(nMax*3>1000?nMax*3:1000);
				}

				if(g_bSkewAnimation){
					if(g_bSkewAnimation==1){
						g_bSkewAnimation=2;
						g_dbSkewAnim = g_SFT.GetRatioY();

						g_dbSkewAnim = 100*g_SFT.GetRatioY()/360;
						SIZE size;
						size.cx = g_SFT.GetWidth();
						size.cy = g_SFT.GetHeight();
						double xRatio = 640.0/size.cx;
						size.cx = 640;
						size.cy = size.cy*xRatio;
						xRatio = (double)360/(double)size.cy;
						g_dbSkewAnim*=xRatio;

						g_dbSkewRatio = pow(10,log10((double)100/g_dbSkewAnim)/(double)g_nSkewFrames);
						g_dbSkewAnim = g_SFT.GetRatioY();
						//g_dbSkewRatio = ((double)360*g_dbSkewToRatio/100-(double)g_dbSkewAnim)/(double)(g_nSkewFrames-1);
						g_dbRotateRatio = (pi*g_dbSkewToAnim/180-g_Degree)/g_nSkewFrames;
						/*g_dbRotateStart = g_Degree;
						g_dbRotateAnim = (g_Degree<0?-g_Degree:g_Degree)+1;
						g_dbRotateRatio = pow(10,log10(g_dbRotateAnim)/(double)g_nSkewFrames);
						g_dbRotateAnim=g_dbRotateRatio;*/
					}
					g_dbSkewAnim*=g_dbSkewRatio;
					//g_dbSkewAnim+=g_dbSkewRatio;
					g_Degree+=g_dbRotateRatio;
					/*if(g_Degree>0)
						g_Degree = g_dbRotateStart-(g_dbRotateAnim-1);
					else
						g_Degree = g_dbRotateStart+(g_dbRotateAnim-1);
						*/
					g_dbRotateAnim *= g_dbRotateRatio;
					g_nSkewFrames--;
					if(g_nSkewFrames==0)
						return 0;
					g_SFT.SetRatio(640,g_dbSkewAnim);
					g_SFT.RenderFractal(g_SFT.GetWidth(),g_SFT.GetHeight(),g_SFT.GetIterations(),hWnd);
					SetTimer(hWnd,0,500,NULL);
				}
				else{
//					if(g_bAnimateEachFrame)
//						g_Degree+=0.01;
					bool bReuseCenter = (g_SFT.GetZoomSize() == round(g_SFT.GetZoomSize()));
					g_SFT.Zoom(g_SFT.GetZoomSize()==1?-g_JpegParams.nWidth/2:g_JpegParams.nWidth/2,g_JpegParams.nHeight/2,1/(double)g_SFT.GetZoomSize(),g_JpegParams.nWidth,g_JpegParams.nHeight,!g_bAnimateEachFrame && bReuseCenter);
				}
				SetTimer(hWnd,0,500,NULL);
				return 0;
			}
nPos=15;
		}
		if(g_bFirstDone){
nPos=16;
			if(!g_LDBL)
				MessageBox(hWnd,"The library "
				"kf.dll"
				" could not be loaded. You may continue to use this application, but the speed of rendering images on depths between 1e600 and 1e4900 will be significantly slower without this library","Missing file",MB_OK|MB_ICONWARNING);

nPos=17;
			if(g_LDBL==2){
				MessageBox(hWnd,"The library "
				"kf.dll"
				" is old. Please update this file.","Old file",MB_OK|MB_ICONWARNING);
			}
			g_bFirstDone=FALSE;
nPos=18;
			HANDLE hFile = CreateFile(g_szRecovery,GENERIC_READ,FILE_SHARE_READ|FILE_SHARE_WRITE,0,OPEN_EXISTING,0,NULL);
			if(hFile!=INVALID_HANDLE_VALUE && MessageBox(hWnd,"Kalle's Fraktaler was not closed properly last session. Do you want to recover your last location?","Kalle's Fraktaler",MB_YESNO)==IDYES){
nPos=19;
				CloseHandle(hFile);
				g_SFT.Stop();
				g_bAnim=false;
				g_SFT.OpenFile(g_szRecovery);
				PostMessage(hWnd,WM_KEYDOWN,VK_F5,0);
			}
			else if(hFile!=INVALID_HANDLE_VALUE)
				CloseHandle(hFile);
nPos=20;
		}
		else{
nPos=21;
			g_SFT.SaveFile(g_szRecovery);
		}

nPos=22;
		g_SFT.ApplyColors();
		if(g_bSaveJpeg){
nPos=23;
			if (g_bInteractive)
			{
				g_bSaveJpeg=FALSE;
				std::string szFile;
				if(BrowseFile(hWnd,FALSE,"Save as Jpeg","Jpeg\0*.jpg\0\0",szFile)){
					if(!g_SFT.SaveJpg(szFile,g_JpegParams.nQuality))
						MessageBox(hWnd,"File could not be saved","Error",MB_OK|MB_ICONSTOP);
					PostMessage(hWnd,WM_KEYDOWN,VK_F5,0);
				}
			}
			else if (uMsg==WM_USER+199 && wParam==0)
			{
				g_bSaveJpeg=FALSE;
				char szFile[1024]={0};
				strncpy(szFile, g_args->sSaveJPG.c_str(), sizeof(szFile));
				std::cerr << "saving JPG " << szFile << std::endl;
				if(!g_SFT.SaveJpg(szFile,100))
					std::cerr << "ERROR in save jpg: " << szFile << std::endl;
			}
		}
		if(g_bSavePng){
nPos=24;
			if (g_bInteractive)
			{
				g_bSavePng=FALSE;
				std::string szFile;
				if(BrowseFile(hWnd,FALSE,"Save as PNG","PNG\0*.png\0\0",szFile)){
					if(!g_SFT.SaveJpg(szFile,-1))
						MessageBox(hWnd,"File could not be saved","Error",MB_OK|MB_ICONSTOP);
					PostMessage(hWnd,WM_KEYDOWN,VK_F5,0);
				}
			}
			else if (uMsg==WM_USER+199 && wParam==0)
			{
				g_bSavePng=FALSE;
				char szFile[1024]={0};
				strncpy(szFile, g_args->sSavePNG.c_str(), sizeof(szFile));
				std::cerr << "saving PNG " << szFile << std::endl;
				if(!g_SFT.SaveJpg(szFile,-1))
					std::cerr << "ERROR in save png: " << szFile << std::endl;
			}
		}
		if(g_bSaveMap){
			if (g_bInteractive)
			{
				g_bSaveMap = FALSE;
			}
			else if (uMsg==WM_USER+199 && wParam==0)
			{
				g_bSaveMap=FALSE;
				char szFile[1024]={0};
				strncpy(szFile, g_args->sSaveMap.c_str(), sizeof(szFile));
				std::cerr << "saving KFB " << szFile << std::endl;
				g_SFT.SaveMapB(szFile);
			}
		}
		if (uMsg==WM_USER+199 && wParam==0 && !g_bInteractive)
		{
			PostQuitMessage(0);
		}
/*			int nMin, nMax;
		g_SFT.GetIterations(nMin,nMax);
		if(nMax<g_SFT.GetIterations())
			g_SFT.SetIterations(nMax+nMax/3);
*/
	}
nPos=25;
	return 0;
}
static int HandleDoneSEH(HWND hWnd,UINT uMsg,WPARAM wParam,LPARAM lParam)
{
	int nPos=0;
#ifndef _DEBUG
	try{
#endif
		return HandleDone(hWnd,uMsg,wParam,lParam,nPos);
#ifndef _DEBUG
	}catch(...){
	}
#endif
	return 0;
}

static int WINAPI CustomZoomSize(HWND hWnd,UINT uMsg,WPARAM wParam,LPARAM lParam){
	if(uMsg==WM_INITDIALOG){
		SendMessage(hWnd, WM_SETICON, ICON_SMALL, LPARAM(g_hIcon));
		SendMessage(hWnd, WM_SETICON, ICON_BIG, LPARAM(g_hIcon));
		SetDlgItemText(hWnd,IDC_LABEL,"Zoom size(float value)");
		SetWindowLongPtr(hWnd,GWLP_USERDATA,lParam);
		return 1;
	}
	if(uMsg==WM_COMMAND){
		if(wParam==IDCANCEL)
			EndDialog(hWnd,0);
		else if(wParam==IDOK){
			char *szTmp = (char*)GetWindowLongPtr(hWnd,GWLP_USERDATA);
			GetDlgItemText(hWnd,IDC_EDIT1,szTmp,25);
			EndDialog(hWnd,1);
		}
	}
	return 0;
}
static int WINAPI StopAtProc(HWND hWnd,UINT uMsg,WPARAM wParam,LPARAM lParam)
{
	if(uMsg==WM_INITDIALOG){
		SendMessage(hWnd, WM_SETICON, ICON_SMALL, LPARAM(g_hIcon));
		SendMessage(hWnd, WM_SETICON, ICON_BIG, LPARAM(g_hIcon));
		if(lParam){
			char *szTitle = (char*)lParam;
			SetWindowText(hWnd,szTitle);
			SetDlgItemText(hWnd,IDC_LABEL,szTitle);
		}
		return 1;
	}
	if(uMsg==WM_COMMAND){
		if(wParam==IDCANCEL)
			EndDialog(hWnd,0);
		else if(wParam==IDOK)
			EndDialog(hWnd,GetDlgItemInt(hWnd,IDC_EDIT1,NULL,0));
	}
	return 0;
}
static void RotateImageAroundPoint(HBITMAP bmBkg,POINT pm)
{
	HDC hDC = GetDC(NULL);
	BYTE *lpBits=NULL;
	BITMAPINFOHEADER bmi={sizeof(BITMAPINFOHEADER)};
	int row;
	GetDIBits(hDC,bmBkg,0,0,NULL,(LPBITMAPINFO)&bmi,DIB_RGB_COLORS);
	bmi.biCompression=bmi.biClrUsed=bmi.biClrImportant=0;
	bmi.biBitCount = 24;
	row = ((((bmi.biWidth*(DWORD)bmi.biBitCount)+31)&~31) >> 3);
	bmi.biSizeImage=row*bmi.biHeight;
	BYTE *lpOrgBits = new BYTE[bmi.biSizeImage];
	GetDIBits(hDC,bmBkg,0,bmi.biHeight,lpOrgBits,
			(LPBITMAPINFO)&bmi,DIB_RGB_COLORS);
	lpBits = new BYTE[bmi.biSizeImage];
	memset(lpBits,0,bmi.biSizeImage);
	int x, y;
	double diagonal = sqrt((double)(bmi.biWidth*bmi.biWidth/4+bmi.biHeight*bmi.biHeight/4));
	for(x=0;x<bmi.biWidth;x++){
		for(y=0;y<bmi.biHeight;y++){
			double ratio = (double)(y-pm.y)/(double)(x-pm.x);
			double degree = -atan(ratio);
			if(x==pm.x){
				if(y<pm.y)
					degree=-pi/2;
				else
					degree=pi/2;
			}
			if(x>pm.x)
				degree-=pi;
			double dist = sqrt((double)((x-pm.x)*(x-pm.x)+(y-pm.y)*(y-pm.y)))/diagonal;
			int dx = (double)pm.x + (double)(x-pm.x)*cos(degree)*dist
				+ (double)(y-pm.y)*sin(degree)*dist;
			int dy = (double)pm.y + (double)(y-pm.y)*cos(degree)*dist
				- (double)(x-pm.x)*sin(degree)*dist;
			if(dx<=-1 || dy<=-1 || dx>=bmi.biWidth || dy>=bmi.biHeight)
				continue;
			int nIndex = x*3 + (bmi.biHeight-1-y)*row;
			int nDIndex = dx*3 + (bmi.biHeight-1-dy)*row;
			lpBits[nIndex]=lpOrgBits[nDIndex];
			lpBits[nIndex+1]=lpOrgBits[nDIndex+1];
			lpBits[nIndex+2]=lpOrgBits[nDIndex+2];

		}
	}
	SetDIBits(hDC,bmBkg,0,bmi.biHeight,lpBits,
			(LPBITMAPINFO)&bmi,DIB_RGB_COLORS);
	ReleaseDC(NULL,hDC);
	delete lpOrgBits;
	delete lpBits;
}
static void RotateImage(HBITMAP bmBkg,HBITMAP bmBkgDraw,POINT pm,double nDegree)
{

	double s = sin(nDegree);
	double c = cos(nDegree);
	HDC hDC = GetDC(NULL);
	BYTE *lpBits=NULL;
	BITMAPINFOHEADER bmi={sizeof(BITMAPINFOHEADER)};
	int row;
	GetDIBits(hDC,bmBkg,0,0,NULL,(LPBITMAPINFO)&bmi,DIB_RGB_COLORS);
	bmi.biCompression=bmi.biClrUsed=bmi.biClrImportant=0;
	bmi.biBitCount = 24;
	row = ((((bmi.biWidth*(DWORD)bmi.biBitCount)+31)&~31) >> 3);
	bmi.biSizeImage=row*bmi.biHeight;
	BYTE *lpOrgBits = new BYTE[bmi.biSizeImage];
	GetDIBits(hDC,bmBkg,0,bmi.biHeight,lpOrgBits,
			(LPBITMAPINFO)&bmi,DIB_RGB_COLORS);

	BITMAPINFOHEADER bmiDraw={sizeof(BITMAPINFOHEADER)};
	GetDIBits(hDC,bmBkgDraw,0,0,NULL,(LPBITMAPINFO)&bmiDraw,DIB_RGB_COLORS);
	bmiDraw.biCompression=bmiDraw.biClrUsed=bmiDraw.biClrImportant=0;
	bmiDraw.biBitCount = 24;
	int rowDraw = ((((bmiDraw.biWidth*(DWORD)bmiDraw.biBitCount)+31)&~31) >> 3);
	bmiDraw.biSizeImage=rowDraw*bmiDraw.biHeight;
	lpBits = new BYTE[bmiDraw.biSizeImage];
	memset(lpBits,50,bmiDraw.biSizeImage);

	POINT p1, p2;
	for(p1.y=0;p1.y<bmiDraw.biHeight;p1.y++){
		for(p1.x=0;p1.x<bmiDraw.biWidth;p1.x++){
			lpBits[p1.x*3 + (bmiDraw.biHeight-p1.y-1)*rowDraw] = lpOrgBits[0];
			lpBits[p1.x*3 + (bmiDraw.biHeight-p1.y-1)*rowDraw+1] = lpOrgBits[1];
			lpBits[p1.x*3 + (bmiDraw.biHeight-p1.y-1)*rowDraw+2] = lpOrgBits[2];

			p2 = p1;

			double dx = (double)pm.x + (double)(p2.x-pm.x)*c
				+ (double)(p2.y-pm.y)*s;
			double dy = (double)pm.y + (double)(p2.y-pm.y)*c
				- (double)(p2.x-pm.x)*s;
			if(dx<=-1 || dy<=-1)
				continue;
			if(dx<0)
				dx=0;
			if(dy<0)
				dy=0;
			p2.x = (int)dx;
			p2.y = (int)dy;
			if(p2.x<0 || p2.x>=bmi.biWidth || p2.y<0 || p2.y>=bmi.biHeight)
				continue;
			int R, Rx, Ry, Rz, G, Gx, Gy, Gz, B, Bx, By, Bz;
			double X, Xn, Y, Yn, Z, Zn;
			Xn = dx - (int)dx;
			X = 1 - Xn;
			Yn = dy - (int)dy;
			Y = 1 - Yn;
			Zn = (Xn+Yn)/2;
			Z = 1 - Zn;
			R = lpOrgBits[p2.x*3 + (bmi.biHeight-p2.y-1)*row];
			G = lpOrgBits[p2.x*3 + (bmi.biHeight-p2.y-1)*row + 1];
			B = lpOrgBits[p2.x*3 + (bmi.biHeight-p2.y-1)*row + 2];
			p2.x++;
			if(p2.x<0 || p2.x>=bmi.biWidth || p2.y<0 || p2.y>=bmi.biHeight){
				Rx = R;
				Gx = G;
				Bx = B;
			}
			else{
				Rx = lpOrgBits[p2.x*3 + (bmi.biHeight-p2.y-1)*row];
				Gx = lpOrgBits[p2.x*3 + (bmi.biHeight-p2.y-1)*row + 1];
				Bx = lpOrgBits[p2.x*3 + (bmi.biHeight-p2.y-1)*row + 2];
			}
			p2.x--;
			p2.y++;
			if(p2.x<0 || p2.x>=bmi.biWidth || p2.y<0 || p2.y>=bmi.biHeight){
				Ry = R;
				Gy = G;
				By = B;
			}
			else{
				Ry = lpOrgBits[p2.x*3 + (bmi.biHeight-p2.y-1)*row];
				Gy = lpOrgBits[p2.x*3 + (bmi.biHeight-p2.y-1)*row + 1];
				By = lpOrgBits[p2.x*3 + (bmi.biHeight-p2.y-1)*row + 2];
			}
			p2.x++;
			if(p2.x<0 || p2.x>=bmi.biWidth || p2.y<0 || p2.y>=bmi.biHeight){
				Rz = R;
				Gz = G;
				Bz = B;
			}
			else{
				Rz = lpOrgBits[p2.x*3 + (bmi.biHeight-p2.y-1)*row];
				Gz = lpOrgBits[p2.x*3 + (bmi.biHeight-p2.y-1)*row + 1];
				Bz = lpOrgBits[p2.x*3 + (bmi.biHeight-p2.y-1)*row + 2];
			}

			R = (int)(((double)R*X + (double)Rx*Xn + (double)R*Y + (double)Ry*Yn + (double)R*Z + (double)Rz*Zn)/3);
			G = (int)(((double)G*X + (double)Gx*Xn + (double)G*Y + (double)Gy*Yn + (double)G*Z + (double)Gz*Zn)/3);
			B = (int)(((double)B*X + (double)Bx*Xn + (double)B*Y + (double)By*Yn + (double)B*Z + (double)Bz*Zn)/3);
			lpBits[p1.x*3 + (bmiDraw.biHeight-p1.y-1)*rowDraw] = R;
			lpBits[p1.x*3 + (bmiDraw.biHeight-p1.y-1)*rowDraw+1] = G;
			lpBits[p1.x*3 + (bmiDraw.biHeight-p1.y-1)*rowDraw+2] = B;

//			memcpy(&lpBits[p1.x*3 + (bmiDraw.biHeight-p1.y-1)*rowDraw],&lpOrgBits[p2.x*3 + (bmi.biHeight-p2.y-1)*row],3);
		}
	}
	SetDIBits(hDC,bmBkgDraw,0,bmiDraw.biHeight,lpBits,
			(LPBITMAPINFO)&bmiDraw,DIB_RGB_COLORS);
	delete lpBits;
	delete lpOrgBits;
	ReleaseDC(NULL,hDC);
}
static void SkewImage(HBITMAP bmBmp)
{
	HDC hDC = GetDC(NULL);
	BITMAP bm;
	GetObject(bmBmp,sizeof(BITMAP),&bm);
	HBITMAP bmNew = create_bitmap(hDC,bm.bmWidth,bm.bmHeight);
	POINT pm = {bm.bmWidth/2,bm.bmHeight/2};
	double r=pi*(double)g_nSkewRotate/180;
	RotateImage(bmBmp,bmNew,pm,r);

	int nWidth = g_nSkewStretch*bm.bmWidth/100;
	HDC dcNew = CreateCompatibleDC(hDC);
	HBITMAP bmOldNew = (HBITMAP)SelectObject(dcNew,bmNew);
	HDC dcBmp = CreateCompatibleDC(hDC);
	HBITMAP bmOldBmp = (HBITMAP)SelectObject(dcBmp,bmBmp);
	SetStretchBltMode(dcBmp,HALFTONE);

	RECT rc = {0,0,bm.bmWidth,bm.bmHeight};
	FillRect(dcBmp,&rc,(HBRUSH)GetStockObject(BLACK_BRUSH));
	StretchBlt(dcBmp,(bm.bmWidth-nWidth)/2,0,nWidth,bm.bmHeight,dcNew,0,0,bm.bmWidth,bm.bmHeight,SRCCOPY);
	SelectObject(dcNew,bmOldNew);
	SelectObject(dcBmp,bmOldBmp);
	DeleteDC(dcNew);
	DeleteDC(dcBmp);
	DeleteObject(bmNew);
	ReleaseDC(NULL,hDC);
}
static void UnSkewImage(HBITMAP bmBmp)
{
	HDC hDC = GetDC(NULL);
	BITMAP bm;
	GetObject(bmBmp,sizeof(BITMAP),&bm);
	HBITMAP bmNew = create_bitmap(hDC,bm.bmWidth,bm.bmHeight);

	int nWidth = (10000/g_nSkewStretch)*bm.bmWidth/100;
	HDC dcNew = CreateCompatibleDC(hDC);
	HBITMAP bmOldNew = (HBITMAP)SelectObject(dcNew,bmNew);
	HDC dcBmp = CreateCompatibleDC(hDC);
	HBITMAP bmOldBmp = (HBITMAP)SelectObject(dcBmp,bmBmp);
	SetStretchBltMode(dcNew,HALFTONE);

	RECT rc = {0,0,bm.bmWidth,bm.bmHeight};
	FillRect(dcNew,&rc,(HBRUSH)GetStockObject(BLACK_BRUSH));
	StretchBlt(dcNew,(bm.bmWidth-nWidth)/2,0,nWidth,bm.bmHeight,dcBmp,0,0,bm.bmWidth,bm.bmHeight,SRCCOPY);
	SelectObject(dcNew,bmOldNew);
	SelectObject(dcBmp,bmOldBmp);
	DeleteDC(dcNew);
	DeleteDC(dcBmp);
	POINT pm = {bm.bmWidth/2,bm.bmHeight/2};
	double r=-pi*(double)g_nSkewRotate/180;
	RotateImage(bmNew,bmBmp,pm,r);

	DeleteObject(bmNew);
	ReleaseDC(NULL,hDC);
}
BOOL g_DialogInit=0;
POINT g_Cross[4];
int g_nCrossPos=0;
double g_nTestDegree;
double g_nTestRatio;
static int WINAPI SkewProc(HWND hWnd,UINT uMsg,WPARAM wParam,LPARAM lParam)
{
	if(uMsg==WM_INITDIALOG){
		SendMessage(hWnd, WM_SETICON, ICON_SMALL, LPARAM(g_hIcon));
		SendMessage(hWnd, WM_SETICON, ICON_BIG, LPARAM(g_hIcon));
		InitToolTip(hWnd,GetModuleHandle(NULL),GetToolText,3);
		SetWindowLongPtr(hWnd,GWLP_USERDATA,lParam);
		SendDlgItemMessage(hWnd,IDC_SPIN1,UDM_SETRANGE,0,MAKELONG(10000,1));
		SetDlgItemFloat(hWnd,IDC_EDIT1,g_nSkewStretch);
		SendDlgItemMessage(hWnd,IDC_SPIN2,UDM_SETRANGE,0,MAKELONG(360,-360));
		SetDlgItemFloat(hWnd,IDC_EDIT2,g_nSkewRotate);
		g_DialogInit=1;
		g_nCrossPos=0;
		return 1;
	}
	if(uMsg==WM_NOTIFY){
		LPNMUPDOWN lpnmud = (LPNMUPDOWN) lParam;
		if(lpnmud->hdr.code!=UDN_DELTAPOS)
			return 0;
		if(lpnmud->hdr.idFrom==IDC_SPIN1)
			SetDlgItemFloat(hWnd,IDC_EDIT1,GetDlgItemFloat(hWnd,IDC_EDIT1)+lpnmud->iDelta);
		else if(lpnmud->hdr.idFrom==IDC_SPIN2)
			SetDlgItemFloat(hWnd,IDC_EDIT2,GetDlgItemFloat(hWnd,IDC_EDIT2)+lpnmud->iDelta);
		return 1;
	}
	if(uMsg==WM_COMMAND){
		if(wParam==IDOK)
			EndDialog(hWnd,1);
		else if(wParam==IDCANCEL)
			EndDialog(hWnd,0);
		else if(wParam==IDC_BUTTON1){
			SetDlgItemInt(hWnd,IDC_EDIT1,100,FALSE);
			SetDlgItemInt(hWnd,IDC_EDIT2,0,TRUE);
		}
		else if(wParam==IDC_CHECK1){
			if(SendDlgItemMessage(hWnd,IDC_CHECK1,BM_GETCHECK,0,0)){
				g_nCrossPos=0;
				SetCapture(hWnd);
			}
			else
				ReleaseCapture();
		}
		else if(g_DialogInit){
			g_nSkewStretch = GetDlgItemFloat(hWnd,IDC_EDIT1);
			g_nSkewRotate = GetDlgItemFloat(hWnd,IDC_EDIT2);
			HWND hwParent = (HWND)GetWindowLongPtr(hWnd,GWLP_USERDATA);
			if(hwParent){
				InvalidateRect(hwParent,NULL,FALSE);
				UpdateWindow(hwParent);
				RECT rC;
				GetClientRect(hwParent,&rC);
				POINT pm;
				pm.x = rC.right/2;
				pm.y = rC.bottom/2;

				HDC hDC = GetDC(hwParent);
				HPEN pn = CreatePen(0,2,RGB(0,0,255));
				HPEN pnOld = (HPEN)SelectObject(hDC,pn);
				int i;
				double nDegree = -pi*(double)g_nSkewRotate/(double)180;
				double s = sin(nDegree);
				double c = cos(nDegree);
				double nStrecth = (double)g_nSkewStretch/(double)100;
				POINT p[4];
				for(i=0;i<g_nCrossPos;i++){
					POINT pt = g_Cross[i];
					p[i].x = (double)pm.x + (double)(pt.x-pm.x)*c
						+ (double)(pt.y-pm.y)*s;
					p[i].y = (double)pm.y + (double)(pt.y-pm.y)*c
						- (double)(pt.x-pm.x)*s;
					p[i].x = (p[i].x-pm.x)*nStrecth + pm.x;

					MoveToEx(hDC,p[i].x-2,p[i].y-2,NULL);
					LineTo(hDC,p[i].x+2,p[i].y+2);
					MoveToEx(hDC,p[i].x+2,p[i].y-2,NULL);
					LineTo(hDC,p[i].x-2,p[i].y+2);
					if(i%2==1){
						POINT pt = g_Cross[i-1];
						p[i-1].x = (double)pm.x + (double)(pt.x-pm.x)*c
							+ (double)(pt.y-pm.y)*s;
						p[i-1].y = (double)pm.y + (double)(pt.y-pm.y)*c
							- (double)(pt.x-pm.x)*s;
						p[i-1].x = (p[i-1].x-pm.x)*nStrecth + pm.x;
						MoveToEx(hDC,p[i-1].x,p[i-1].y,NULL);
						LineTo(hDC,p[i].x,p[i].y);
					}
				}
				if(g_nCrossPos==4){
					double b1 =  (double)(p[0].y - p[1].y) / (double)(p[0].x - p[1].x==0?1:p[0].x - p[1].x);
					double b2 =  (double)(p[2].y - p[3].y) / (double)(p[2].x - p[3].x==0?1:p[2].x - p[3].x);
					double a1 = p[0].y - b1*p[0].x;
					double a2 = p[2].y - b2*p[2].x;
					POINT pM;
					pM.x = - (a1 - a2) / (b1 - b2) + .5;
					pM.y = a1 + b1*pM.x + .5;

					double distance1 = sqrt((double)(p[0].x-p[1].x)*(p[0].x-p[1].x) + (double)(p[0].y-p[1].y)*(p[0].y-p[1].y));
					double distance2 = sqrt((double)(p[2].x-p[3].x)*(p[2].x-p[3].x) + (double)(p[2].y-p[3].y)*(p[2].y-p[3].y));

					double ratio = (double)(p[0].y-pM.y)/(double)(p[0].x-pM.x);
					double nDegree1 = atan(ratio);
					if((p[0].x-pM.x>=0 && p[0].y-pM.y<0) || (p[0].x-pM.x<0 && p[0].y-pM.y>0))
						nDegree1+=pi;
					ratio = (double)(p[2].y-pM.y)/(double)(p[2].x-pM.x);
					double nDegree2 = atan(ratio);
					if((p[2].x-pM.x>=0 && p[2].y-pM.y<0) || (p[2].x-pM.x<0 && p[2].y-pM.y>0))
						nDegree2+=pi;
					double nDegree = 180*(nDegree1-nDegree2)/pi;
					if(nDegree<0)
						nDegree=-nDegree;
					char szDegree[130];
					sprintf(szDegree,"Degree: %.2f, Ratio: %.2f",nDegree,distance1/distance2);
					TextOut(hDC,0,0,szDegree,strlen(szDegree));
					g_nTestDegree = nDegree;
					g_nTestRatio = distance1/distance2;
				}

				SelectObject(hDC,pnOld);
				DeleteObject(pn);
				ReleaseDC(hwParent,hDC);
			}
		}
	}
	else if(uMsg==WM_TIMER){
		SkewProc(hWnd,WM_COMMAND,0,0);
		double nTestDegree = g_nTestDegree;
#ifdef _TEST_RATIO
		double nTestRatio = g_nTestRatio;
#endif
		double nSkewStretch = GetDlgItemFloat(hWnd,IDC_EDIT1);
		double nSkewRotate = GetDlgItemFloat(hWnd,IDC_EDIT2);
		double nTestStretch = nSkewStretch;
		double nTestRotate = nSkewRotate;
		double nBestStretch = nSkewStretch;
		double nBestRotate = nSkewRotate;
		double nStretchOffs = nTestStretch/40;
		double nRotateOffs = .2;
		int bSet=0;
		if(nStretchOffs==0)
			nStretchOffs=1;
		BOOL bSetValue=0;

		if(g_nTestDegree>85 && g_nTestDegree<95){
			nStretchOffs/=2;
			nRotateOffs/=2;
		}
		if(g_nTestDegree>89 && g_nTestDegree<91){
			nStretchOffs/=2;
			nRotateOffs/=2;
		}

		nTestStretch-=nStretchOffs;
		SetDlgItemFloat(hWnd,IDC_EDIT1,nTestStretch);
		SkewProc(hWnd,WM_COMMAND,0,0);

		bSet=0;
		if(nTestDegree<90 && g_nTestDegree<90 && g_nTestDegree>nTestDegree)
			bSet=1;
		if(nTestDegree>90 && g_nTestDegree>90 && g_nTestDegree<nTestDegree)
			bSet=1;
#ifdef _TEST_RATIO
		if(nTestRatio<1 && g_nTestRatio>nTestRatio)
			bSet++;
		if(nTestRatio>1 && g_nTestRatio<nTestRatio)
			bSet++;
#endif
		if(bSet){
			bSetValue=TRUE;
			nTestDegree = g_nTestDegree;
			nBestStretch = nTestStretch;
			nBestRotate = nTestRotate;
		}

		nTestRotate-=nRotateOffs;
		SetDlgItemFloat(hWnd,IDC_EDIT2,nTestRotate);
		SkewProc(hWnd,WM_COMMAND,0,0);
		bSet=0;
		if(nTestDegree<90 && g_nTestDegree<90 && g_nTestDegree>nTestDegree)
			bSet=1;
		if(nTestDegree>90 && g_nTestDegree>90 && g_nTestDegree<nTestDegree)
			bSet=1;
#ifdef _TEST_RATIO
		if(nTestRatio<1 && g_nTestRatio>nTestRatio)
			bSet++;
		if(nTestRatio>1 && g_nTestRatio<nTestRatio)
			bSet++;
#endif
		if(bSet){
			bSetValue=TRUE;
			nTestDegree = g_nTestDegree;
			nBestStretch = nTestStretch;
			nBestRotate = nTestRotate;
		}

		nTestStretch+=2*nStretchOffs;
		SetDlgItemFloat(hWnd,IDC_EDIT1,nTestStretch);
		SkewProc(hWnd,WM_COMMAND,0,0);
		bSet=0;
		if(nTestDegree<90 && g_nTestDegree<90 && g_nTestDegree>nTestDegree)
			bSet=1;
		if(nTestDegree>90 && g_nTestDegree>90 && g_nTestDegree<nTestDegree)
			bSet=1;
#ifdef _TEST_RATIO
		if(nTestRatio<1 && g_nTestRatio>nTestRatio)
			bSet++;
		if(nTestRatio>1 && g_nTestRatio<nTestRatio)
			bSet++;
#endif
		if(bSet){
			bSetValue=TRUE;
			nTestDegree = g_nTestDegree;
			nBestStretch = nTestStretch;
			nBestRotate = nTestRotate;
		}

		nTestRotate+=2*nRotateOffs;
		SetDlgItemFloat(hWnd,IDC_EDIT2,nTestRotate);
		SkewProc(hWnd,WM_COMMAND,0,0);
		bSet=0;
		if(nTestDegree<90 && g_nTestDegree<90 && g_nTestDegree>nTestDegree)
			bSet=1;
		if(nTestDegree>90 && g_nTestDegree>90 && g_nTestDegree<nTestDegree)
			bSet=1;
#ifdef _TEST_RATIO
		if(nTestRatio<1 && g_nTestRatio>nTestRatio)
			bSet++;
		if(nTestRatio>1 && g_nTestRatio<nTestRatio)
			bSet++;
#endif
		if(bSet){
			bSetValue=TRUE;
			nTestDegree = g_nTestDegree;
			nBestStretch = nTestStretch;
			nBestRotate = nTestRotate;
		}

		if(bSetValue){
			SetDlgItemFloat(hWnd,IDC_EDIT1,nBestStretch);
			SetDlgItemFloat(hWnd,IDC_EDIT2,nBestRotate);
			SkewProc(hWnd,WM_COMMAND,0,0);
		}
		else{
			SetDlgItemFloat(hWnd,IDC_EDIT1,nSkewStretch);
			SetDlgItemFloat(hWnd,IDC_EDIT2,nSkewRotate);
			KillTimer(hWnd,0);
			SetDlgItemText(hWnd,IDC_CHECK1,"Help lines");
		}
		if(g_nTestDegree>89.5 && g_nTestDegree<91.5){
			KillTimer(hWnd,0);
			SetDlgItemText(hWnd,IDC_CHECK1,"Help lines");
		}
	}
	else if(uMsg==WM_CAPTURECHANGED && SendDlgItemMessage(hWnd,IDC_CHECK1,BM_GETCHECK,0,0)){
		SendDlgItemMessage(hWnd,IDC_CHECK1,BM_SETCHECK,0,0);
	}
	else if(uMsg==WM_LBUTTONDOWN && SendDlgItemMessage(hWnd,IDC_CHECK1,BM_GETCHECK,0,0)){
		HWND hwParent = (HWND)GetWindowLongPtr(hWnd,GWLP_USERDATA);
		g_Cross[g_nCrossPos].x = (short)LOWORD(lParam);
		g_Cross[g_nCrossPos].y = (short)HIWORD(lParam);
		ClientToScreen(hWnd,&g_Cross[g_nCrossPos]);
		ScreenToClient(hwParent,&g_Cross[g_nCrossPos]);

		HDC hDC = GetDC(hwParent);
		HPEN pn = CreatePen(0,2,RGB(0,0,255));
		HPEN pnOld = (HPEN)SelectObject(hDC,pn);
		MoveToEx(hDC,g_Cross[g_nCrossPos].x-2,g_Cross[g_nCrossPos].y-2,NULL);
		LineTo(hDC,g_Cross[g_nCrossPos].x+2,g_Cross[g_nCrossPos].y+2);
		MoveToEx(hDC,g_Cross[g_nCrossPos].x+2,g_Cross[g_nCrossPos].y-2,NULL);
		LineTo(hDC,g_Cross[g_nCrossPos].x-2,g_Cross[g_nCrossPos].y+2);
		if(g_nCrossPos%2==1){
			MoveToEx(hDC,g_Cross[g_nCrossPos-1].x,g_Cross[g_nCrossPos-1].y,NULL);
			LineTo(hDC,g_Cross[g_nCrossPos].x,g_Cross[g_nCrossPos].y);
		}

		g_nCrossPos++;
		if(g_nCrossPos==4){
			SendDlgItemMessage(hWnd,IDC_CHECK1,BM_SETCHECK,0,0);
			ReleaseCapture();
			POINT pM;

			double b1 =  (double)(g_Cross[0].y - g_Cross[1].y) / (double)(g_Cross[0].x - g_Cross[1].x);
			double b2 =  (double)(g_Cross[2].y - g_Cross[3].y) / (double)(g_Cross[2].x - g_Cross[3].x);
			double a1 = g_Cross[0].y - b1*g_Cross[0].x;
			double a2 = g_Cross[2].y - b2*g_Cross[2].x;

			pM.x = - (a1 - a2) / (b1 - b2) + .5;
			pM.y = a1 + b1*pM.x + .5;

			SelectObject(hDC,pnOld);
			DeleteObject(pn);
			pn = CreatePen(0,2,RGB(255,0,0));
			pnOld = (HPEN)SelectObject(hDC,pn);
			MoveToEx(hDC,pM.x-2,pM.y-2,NULL);
			LineTo(hDC,pM.x+2,pM.y+2);
			MoveToEx(hDC,pM.x+2,pM.y-2,NULL);
			LineTo(hDC,pM.x-2,pM.y+2);

			double nStrecth = (double)g_nSkewStretch/(double)100;
			double nDegree = pi*(double)g_nSkewRotate/(double)180;
			double s = sin(nDegree);
			double c = cos(nDegree);
			RECT rC;
			GetClientRect(hwParent,&rC);
			POINT pm;
			pm.x = rC.right/2;
			pm.y = rC.bottom/2;
			int i;
			for(i=0;i<g_nCrossPos;i++){
				POINT pt = g_Cross[i];
				pt.x = (pt.x-pm.x)/nStrecth + pm.x;
				g_Cross[i].x = (double)pm.x + (double)(pt.x-pm.x)*c
					+ (double)(pt.y-pm.y)*s;
				g_Cross[i].y = (double)pm.y + (double)(pt.y-pm.y)*c
					- (double)(pt.x-pm.x)*s;
			}
			SetTimer(hWnd,0,100,NULL);
			SetDlgItemText(hWnd,IDC_CHECK1,"Working");
		}
		SelectObject(hDC,pnOld);
		DeleteObject(pn);
		ReleaseDC(hwParent,hDC);
	}
	return 0;
}
static int WINAPI SkewAnimateProc(HWND hWnd,UINT uMsg,WPARAM wParam,LPARAM lParam)
{
	(void) lParam;
	if(uMsg==WM_INITDIALOG){
		SendMessage(hWnd, WM_SETICON, ICON_SMALL, LPARAM(g_hIcon));
		SendMessage(hWnd, WM_SETICON, ICON_BIG, LPARAM(g_hIcon));
		SetDlgItemText(hWnd,IDC_EDIT1,"400");
		SetDlgItemText(hWnd,IDC_EDIT3,"100");
		SetDlgItemText(hWnd,IDC_EDIT4,"0");
		return 1;
	}
	if(uMsg==WM_COMMAND){
		if(wParam==IDCANCEL)
			EndDialog(hWnd,0);
		else if(wParam==IDOK){
			g_nSkewFrames = GetDlgItemInt(hWnd,IDC_EDIT1,NULL,FALSE);
			if(g_nSkewFrames<1)
				g_nSkewFrames=1;
			g_dbSkewToRatio = GetDlgItemFloat(hWnd,IDC_EDIT3);
			g_dbSkewToAnim = GetDlgItemFloat(hWnd,IDC_EDIT4);
			EndDialog(hWnd,1);
		}
	}
	return 0;
}

#ifdef KF_OPENCL
LRESULT CALLBACK OpenCLProc(HWND hWnd, UINT msg, WPARAM wParam, LPARAM lParam)
{
  switch(msg)
  {
    case WM_INITDIALOG:
		{
			SendMessage(hWnd, WM_SETICON, ICON_SMALL, LPARAM(g_hIcon));
			SendMessage(hWnd, WM_SETICON, ICON_BIG, LPARAM(g_hIcon));
			InitToolTip(hWnd,GetModuleHandle(NULL),GetToolText,1);

			SendDlgItemMessage(hWnd, IDC_COMBO_OPENCL_DEVICE, CB_ADDSTRING, 0, (LPARAM) "(none)");
			for (int i = 0; i < cldevices.size(); ++i)
			{
				cldevice d = cldevices[i];
				SendDlgItemMessage(hWnd, IDC_COMBO_OPENCL_DEVICE, CB_ADDSTRING, 0, (LPARAM) d.name.c_str());
			}

			SIZE sc;
			HDC hDC = GetDC(NULL);
			HFONT hfOld = (HFONT)SelectObject(hDC,(HFONT)GetStockObject(ANSI_VAR_FONT));
			int i, nMaxWidth=0;
			for(i=0;i<SendDlgItemMessage(hWnd,IDC_COMBO_OPENCL_DEVICE,CB_GETCOUNT,0,0);i++){
				int n = SendDlgItemMessage(hWnd,IDC_COMBO_OPENCL_DEVICE,CB_GETLBTEXTLEN,i,0);
				char *szT = new char[n+1];
				SendDlgItemMessage(hWnd,IDC_COMBO_OPENCL_DEVICE,CB_GETLBTEXT,i,(LPARAM)szT);
				GetTextExtentPoint32A(hDC,szT,strlen(szT),&sc);
				if(sc.cx>nMaxWidth)
					nMaxWidth = sc.cx;
				delete szT;
			}
			SendDlgItemMessage(hWnd,IDC_COMBO_OPENCL_DEVICE,CB_SETDROPPEDWIDTH,nMaxWidth+8+GetSystemMetrics(SM_CXHTHUMB),0);

			SendDlgItemMessage(hWnd, IDC_COMBO_OPENCL_DEVICE, CB_SETCURSEL, g_SFT.GetOpenCLDeviceIndex() + 1, 0);
		  break;
		}
		case WM_COMMAND:
		{
			if (wParam == IDOK)
			{
				g_SFT.SetOpenCLDeviceIndex(SendDlgItemMessage(hWnd, IDC_COMBO_OPENCL_DEVICE, CB_GETCURSEL, 0, 0) - 1);
				ExitToolTip(hWnd);
				EndDialog(hWnd, 0);
			}
			else if(wParam == IDCANCEL)
			{
				ExitToolTip(hWnd);
				EndDialog(hWnd, 0);
			}
			break;
		}
	}
	return 0;
}
#endif

static long OpenFile(HWND hWnd, bool &ret)
{
				g_SFT.Stop();
				g_bAnim=false;
				if(!g_SFT.OpenFile(g_szFile))
				{
					ret = true;
					return MessageBox(hWnd,"Invalid parameter file","Error",MB_OK|MB_ICONSTOP);
				}
				else{
					std::string extension = get_filename_extension(g_szFile);
					if (extension != ".kfr")
					{
						// prevent ctrl-s save overwriting a file with the wrong extension
						g_szFile += ".kfr";
					}
					if(g_hwColors)
						SendMessage(g_hwColors,WM_USER+99,0,0);
					char szTitle[1000];
					wsprintf(szTitle,"Kalle's Fraktaler 2 - %s",g_szFile.c_str());
					SetWindowText(hWnd,szTitle);
					PostMessage(hWnd,WM_KEYDOWN,VK_F5,0);
				}
				ret = false;
				return 0;
}

static long OpenSettings(HWND hWnd, bool &ret)
{
				g_SFT.Stop();
				g_bAnim=false;
				if(!g_SFT.OpenSettings(g_szFile))
				{
					ret = true;
					return MessageBox(hWnd,"Invalid settings file","Error",MB_OK|MB_ICONSTOP);
				}
				else{
					UpdateMenusFromSettings(hWnd);
					UpdateWindowSize(hWnd);
					g_SFT.SetImageSize(g_SFT.GetImageWidth(), g_SFT.GetImageHeight());
					PostMessage(hWnd,WM_KEYDOWN,VK_F5,0);
				}
				ret = false;
				return 0;
}

static long WINAPI StoreZoomProc(HWND hWnd,UINT uMsg,WPARAM wParam,LPARAM lParam)
{
	(void) lParam;
	switch (uMsg)
	{
		case WM_INITDIALOG:
		{
			SendMessage(hWnd, WM_SETICON, ICON_SMALL, LPARAM(g_hIcon));
			SendMessage(hWnd, WM_SETICON, ICON_BIG, LPARAM(g_hIcon));
			InitToolTip(hWnd,GetModuleHandle(NULL),GetToolText,5);
		  break;
		}
		case WM_COMMAND:
		{
			if (wParam == IDOK)
			{
				g_bStoreZoomMap = SendDlgItemMessage(hWnd, IDC_STOREZOOM_KFB, BM_GETCHECK, 0, 0) != 0;
				g_bStoreZoomPng = SendDlgItemMessage(hWnd, IDC_STOREZOOM_PNG, BM_GETCHECK, 0, 0) != 0;
				g_bStoreZoomJpg = SendDlgItemMessage(hWnd, IDC_STOREZOOM_JPG, BM_GETCHECK, 0, 0) != 0;
				g_nStoreZoomCount = 0;
				g_nStoreZoomLimit = GetDlgItemInt(hWnd, IDC_STOREZOOM_COUNT, NULL, FALSE);
				if (g_nStoreZoomLimit <= 0 || SendDlgItemMessage(hWnd, IDC_STOREZOOM_COUNTAUTO, BM_GETCHECK, 0, 0))
				{
					g_nStoreZoomLimit = 0;
				}
				ExitToolTip(hWnd);
				EndDialog(hWnd, g_bStoreZoomMap || g_bStoreZoomPng || g_bStoreZoomJpg);
			}
			else if(wParam == IDCANCEL)
			{
				ExitToolTip(hWnd);
				EndDialog(hWnd, 0);
			}
			break;
		}
	}
	return 0;
}

static long WINAPI MainProc(HWND hWnd,UINT uMsg,WPARAM wParam,LPARAM lParam)
{
	if(uMsg==WM_CREATE){
		g_hwStatus = CreateStatusWindow(WS_CHILD|WS_VISIBLE,"",hWnd,0);
		RECT sr;
		GetWindowRect(g_hwStatus,&sr);
		sr.bottom-=sr.top;
		int widths[3]={180,430,-1};
		SendMessage(g_hwStatus,SB_SETPARTS,3,(LPARAM)&widths);
		SendMessage(g_hwStatus,SB_SETTEXT,0,(LPARAM)"");
		SendMessage(g_hwStatus,SB_SETTEXT,1,(LPARAM)"1");
		SendMessage(g_hwStatus,SB_SETTEXT,2,(LPARAM)"");
		RECT wr, cr;
		GetWindowRect(hWnd,&wr);
		wr.right-=wr.left;
		wr.bottom-=wr.top;
		GetClientRect(hWnd,&cr);
		int nXOffs = 640-cr.right;
		wr.right+=nXOffs;
		int nYOffs = 360-cr.bottom;
		wr.bottom+=nYOffs+sr.bottom;
		CheckMenuItem(GetMenu(hWnd),ID_ACTIONS_ZOOMSIZE_4,MF_BYCOMMAND|MF_CHECKED);
		g_SFT.SetShowGlitches(GetPrivateProfileInt("SETTINGS","ShowGlitches",0,"fraktal_sft.ini"));
		UpdateShowGlitches(hWnd);

		RECT r;
		GetClientRect(GetDesktopWindow(),&r);
		g_SFT.SetWindowLeft(r.right/2-wr.right/2);
		g_SFT.SetWindowTop(r.bottom/2-wr.bottom/2);
		g_SFT.SetWindowRight(wr.right);
		g_SFT.SetWindowBottom(wr.bottom);
		UpdateWindowSize(hWnd);

		g_SFT.SetPosition((CFixedFloat)-2,(CFixedFloat)2,(CFixedFloat)-2,(CFixedFloat)2,640,360);
		SetTimer(hWnd,0,500,NULL);
		SYSTEMTIME st;
		GetLocalTime(&st);
		SystemTimeToFileTime(&st,(LPFILETIME)&g_nTStart);

		g_hwHair = CreateDialog(GetModuleHandle(NULL),MAKEINTRESOURCE(IDD_DIALOG4),hWnd,(DLGPROC)CrossHairProc);
		GetWindowRect(hWnd,&wr);
		GetWindowRect(g_hwHair,&r);
		r.right-=r.left;
		r.bottom-=r.top;
		MoveWindow(g_hwHair,wr.left-r.right-3,wr.top,128,144,TRUE);
		PostMessage(g_hwHair,WM_USER+112,0,0);
		ShowWindow(g_hwHair,SW_SHOWNA);

		g_SFT.GenerateColors(g_SFT.GetNumOfColors(),1);
		g_SFT.ApplyColors();
		UpdateAutoSolveGlitches(hWnd);
		UpdateAutoIterations(hWnd);

		g_SFT.SetAnimateZoom(GetPrivateProfileInt("SETTINGS","AnimateZoom",1,"fraktal_sft.ini"));
		UpdateAnimateZoom(hWnd);

		g_SFT.SetArbitrarySize(GetPrivateProfileInt("SETTINGS","ArbitrarySize",0,"fraktal_sft.ini"));
		UpdateArbitrarySize(hWnd);

		if (g_args->bLoadSettings)
		{
			bool ret;
			g_szFile = g_args->sLoadSettings;
			std::cerr << "loading settings: " << g_szFile << std::endl;
			OpenSettings(hWnd, ret);
		}
		if (g_args->bLoadLocation)
		{
			bool ret;
			g_szFile = g_args->sLoadLocation;
			std::cerr << "loading location: " << g_szFile << std::endl;
			OpenFile(hWnd, ret);
		}
		g_bSaveJpeg = g_args->bSaveJPG;
		g_bSavePng = g_args->bSavePNG;
		g_bSaveMap = g_args->bSaveMap;
		g_bInteractive = !(g_args->bSaveJPG || g_args->bSavePNG || g_args->bSaveMap);
		if (! g_bInteractive)
		{
			std::cerr << "rendering at " << g_SFT.GetImageWidth() << "x" << g_SFT.GetImageHeight() << std::endl;
		}
		g_SFT.RenderFractal(g_SFT.GetImageWidth(),g_SFT.GetImageHeight(),g_SFT.GetIterations(),hWnd);
	}
	else if(uMsg==WM_CLOSE)
		PostQuitMessage(0);
	else if(uMsg==WM_PAINT){
		RECT sr;
		GetWindowRect(g_hwStatus,&sr);
		sr.bottom-=sr.top;
		PAINTSTRUCT ps;
		BeginPaint(hWnd,&ps);
		SetStretchBltMode(ps.hdc,HALFTONE);
		HDC dcBmp = CreateCompatibleDC(ps.hdc);
		HBITMAP bmBmp = g_SFT.GetBitmap();
		HBITMAP bmOld = (HBITMAP)SelectObject(dcBmp,bmBmp);
		RECT rc;
		GetClientRect(hWnd,&rc);
		rc.bottom-=sr.bottom;
		if(g_bShowInflection){
			POINT p;
			int i;
			for(i=0;i<g_nInflection;i++){
				p = g_pInflections[i];
				p.x = p.x*g_SFT.GetWidth()/rc.right;
				p.y = p.y*g_SFT.GetHeight()/rc.bottom;
				RotateImageAroundPoint(bmBmp,p);
			}
			GetCursorPos(&p);
			ScreenToClient(hWnd,&p);
			p.x = p.x*g_SFT.GetWidth()/rc.right;
			p.y = p.y*g_SFT.GetHeight()/rc.bottom;
			RotateImageAroundPoint(bmBmp,p);
		}
		else if(g_bShowSkew){
			SelectObject(dcBmp,bmOld);
			SkewImage(bmBmp);
			bmOld = (HBITMAP)SelectObject(dcBmp,bmBmp);
		}
		StretchBlt(ps.hdc,0,0,rc.right,rc.bottom,dcBmp,0,0,g_SFT.GetWidth(),g_SFT.GetHeight(),SRCCOPY);
		SelectObject(dcBmp,bmOld);
		DeleteDC(dcBmp);
		EndPaint(hWnd,&ps);
		return 0;
	}
	else if(uMsg==WM_RBUTTONDOWN && HIWORD(GetKeyState(VK_CONTROL))){
		g_bTrackSelect=TRUE;
		g_pSelect.x = (short)LOWORD(lParam);
		g_pSelect.y = (short)HIWORD(lParam);
		g_pTrackStart.x = (short)LOWORD(lParam);
		g_pTrackStart.y = (short)HIWORD(lParam);
		SetCapture(hWnd);
		return 0;
	}
	else if(uMsg==WM_LBUTTONDOWN){
		if(g_bNewton){
			static RECT r;
			GetClientRect(hWnd,&r);
			RECT sr;
			GetWindowRect(g_hwStatus,&sr);
			sr.bottom-=sr.top;
			r.bottom-=sr.bottom;
			r.left = (short)LOWORD(lParam);
			r.top = (short)HIWORD(lParam);
			PostMessage(g_hwNewton,WM_USER+1,0,(LPARAM)&r);
			return 0;
		}
		if(!HIWORD(GetKeyState(VK_CONTROL)) && !g_bRotate && !g_bFindMinibrot && g_SFT.GetAnimateZoom() && !g_bAddReference && !g_bEraser && !g_hwExamine && !g_bAddMainReference){
			while(g_bAnim){
				Sleep(3);
			}
			g_SFT.Stop();
			g_bMove=TRUE;
			SetCapture(hWnd);
			g_pSelect.x = (short)LOWORD(lParam);
			g_pSelect.y = (short)HIWORD(lParam);
			return 0;
		}
		if(g_bRotate){
			g_SFT.Stop();
			g_bAnim=false;
			RECT r;
			GetClientRect(hWnd,&r);
				RECT sr;
				GetWindowRect(g_hwStatus,&sr);
				sr.bottom-=sr.top;
				r.bottom-=sr.bottom;
			POINT pm={r.right/2,r.bottom/2};
			POINT p;
			GetCursorPos(&p);
			ScreenToClient(hWnd,&p);
			double ratio = (double)(p.y-pm.y)/(double)(p.x-pm.x);
			g_StartDegree = atan(ratio);
			if(p.x-pm.x>0)
				bXSign=TRUE;
			else
				bXSign=FALSE;
			g_MoveDegree = 0;
			g_bRotate=2;
			SetCapture(hWnd);
			return 0;
		}
		if(g_bWaitRead)
			return 0;
		MSG msg;
		int nButtons=0;
		g_SFT.Stop();
		g_bAnim=false;
		while(PeekMessage(&msg,hWnd,WM_LBUTTONDOWN,WM_LBUTTONDOWN,PM_REMOVE)){
			nButtons++;
			Sleep(10);
		}
		g_bFindMinibrot=FALSE;
		g_bStoreZoom=FALSE;
		DeleteObject(g_bmSaveZoomBuff);
		g_bmSaveZoomBuff=NULL;

		InvalidateRect(hWnd,NULL,FALSE);
		UpdateWindow(hWnd);
		RECT rc;
		GetClientRect(hWnd,&rc);
		RECT sr;
		GetWindowRect(g_hwStatus,&sr);
		sr.bottom-=sr.top;
		rc.bottom-=sr.bottom;
		HDC hDC = GetDC(hWnd);
		SetROP2(hDC,R2_NOT);
		g_pSelect.x = (short)LOWORD(lParam);
		g_pSelect.y = (short)HIWORD(lParam);
		MoveToEx(hDC,g_pSelect.x-rc.right/(g_SFT.GetZoomSize()*2),g_pSelect.y-rc.bottom/(g_SFT.GetZoomSize()*2),NULL);
		LineTo(hDC,g_pSelect.x+rc.right/(g_SFT.GetZoomSize()*2),g_pSelect.y-rc.bottom/(g_SFT.GetZoomSize()*2));
		LineTo(hDC,g_pSelect.x+rc.right/(g_SFT.GetZoomSize()*2),g_pSelect.y+rc.bottom/(g_SFT.GetZoomSize()*2));
		LineTo(hDC,g_pSelect.x-rc.right/(g_SFT.GetZoomSize()*2),g_pSelect.y+rc.bottom/(g_SFT.GetZoomSize()*2));
		LineTo(hDC,g_pSelect.x-rc.right/(g_SFT.GetZoomSize()*2),g_pSelect.y-rc.bottom/(g_SFT.GetZoomSize()*2));
		ReleaseDC(hWnd,hDC);
		g_bSelect=1;
		SetCapture(hWnd);
	}
	else if(uMsg==WM_MOUSEMOVE){
		if(g_bTrackSelect==1){
			HDC hDC = GetDC(hWnd);
			SetROP2(hDC,R2_NOT);
			MoveToEx(hDC,g_pTrackStart.x,g_pTrackStart.y,NULL);

			LineTo(hDC,g_pSelect.x,g_pTrackStart.y);
			LineTo(hDC,g_pSelect.x,g_pSelect.y);
			LineTo(hDC,g_pTrackStart.x,g_pSelect.y);
			LineTo(hDC,g_pTrackStart.x,g_pTrackStart.y);

			g_pSelect.x = (short)LOWORD(lParam);
			g_pSelect.y = (short)HIWORD(lParam);
			double yoffs = (double)(g_pSelect.y - g_pTrackStart.y)/(double)g_SFT.GetHeight();
			double xoffs = (double)(g_pSelect.x - g_pTrackStart.x)/(double)g_SFT.GetWidth();
			if((yoffs<0?-yoffs:yoffs)>(xoffs<0?-xoffs:xoffs)){
				double offs = yoffs;
				if((yoffs<0 && xoffs>0) || (yoffs>0 && xoffs<0))
					offs=-offs;
				g_pSelect.x = g_pTrackStart.x + g_SFT.GetWidth()*offs;
			}
			else{
				double offs = xoffs;
				if((yoffs<0 && xoffs>0) || (yoffs>0 && xoffs<0))
					offs=-offs;
				g_pSelect.y = g_pTrackStart.y + g_SFT.GetHeight()*offs;
			}

			MoveToEx(hDC,g_pTrackStart.x,g_pTrackStart.y,NULL);
			LineTo(hDC,g_pSelect.x,g_pTrackStart.y);
			LineTo(hDC,g_pSelect.x,g_pSelect.y);
			LineTo(hDC,g_pTrackStart.x,g_pSelect.y);
			LineTo(hDC,g_pTrackStart.x,g_pTrackStart.y);
			ReleaseDC(hWnd,hDC);
		}
		if(g_bShowInflection)
			InvalidateRect(hWnd,NULL,FALSE);
		if(g_bMove){
			SetCursor(LoadCursor(NULL,IDC_SIZEALL));
			RECT rc;
			GetClientRect(hWnd,&rc);
			RECT sr;
			GetWindowRect(g_hwStatus,&sr);
			sr.bottom-=sr.top;
			rc.bottom-=sr.bottom;
			POINT p;
			p.x = (short)LOWORD(lParam) - g_pSelect.x;
			p.y = (short)HIWORD(lParam) - g_pSelect.y;
			HDC hDC = GetDC(hWnd);
			HDC dcBmp = CreateCompatibleDC(hDC);
			HDC dcBuff = CreateCompatibleDC(hDC);
			HBITMAP bmOld = (HBITMAP)SelectObject(dcBmp,g_SFT.GetBitmap());
			HBITMAP bmBuff = create_bitmap(hDC,rc.right,rc.bottom);
			HBITMAP bmOldBuff = (HBITMAP)SelectObject(dcBuff,bmBuff);
			SetStretchBltMode(dcBuff,HALFTONE);
			StretchBlt(dcBuff,p.x,p.y,rc.right,rc.bottom,dcBmp,0,0,g_SFT.GetWidth(),g_SFT.GetHeight(),SRCCOPY);
			SetStretchBltMode(hDC,HALFTONE);
			BitBlt(hDC,0,0,rc.right,rc.bottom,dcBuff,0,0,SRCCOPY);

		/*
		HDC dcBkg = CreateCompatibleDC(hDC);
		SetStretchBltMode(dcBkg,HALFTONE);
		HBITMAP bmBkgOld = (HBITMAP)SelectObject(dcBkg,g_SFT.GetBitmap());
		StretchBlt(dcBkg,0,0,g_SFT.GetWidth(),g_SFT.GetHeight(),dcBuff,0,0,rc.right,rc.bottom,SRCCOPY);
		SelectObject(dcBkg,bmBkgOld);
		DeleteDC(dcBkg);
		g_SFT.UpdateBitmap();
		*/

			SelectObject(dcBmp,bmOld);
			DeleteDC(dcBmp);
			SelectObject(dcBuff,bmOldBuff);
			DeleteDC(dcBuff);
			DeleteObject(bmBuff);
			ReleaseDC(hWnd,hDC);
			return 0;
		}
		if(g_bRotate){
			SetCursor(LoadCursor(GetModuleHandle(NULL),MAKEINTRESOURCE(IDC_CURSOR1)));
			if(g_bRotate==2){
				HDC hDC = GetDC(hWnd);
				RECT r;
				GetClientRect(hWnd,&r);
				RECT sr;
				GetWindowRect(g_hwStatus,&sr);
				sr.bottom-=sr.top;
				r.bottom-=sr.bottom;
				POINT pm={r.right/2,r.bottom/2};
				HBITMAP bmBmp = create_bitmap(hDC,g_SFT.GetWidth(),g_SFT.GetHeight());
				POINT p;
				GetCursorPos(&p);
				ScreenToClient(hWnd,&p);
				double ratio = (double)(p.y-pm.y)/(double)(p.x-pm.x);
				g_MoveDegree = atan(ratio);
				if((p.x-pm.x>0 && !bXSign) || (p.x-pm.x<0 && bXSign))
					g_MoveDegree+=pi;
				g_MoveDegree -= g_StartDegree;
				pm.x = g_SFT.GetWidth()/2;
				pm.y = g_SFT.GetHeight()/2;
				RotateImage(g_SFT.GetBitmap(),bmBmp,pm,g_MoveDegree);
				HDC dcBmp = CreateCompatibleDC(hDC);
				HBITMAP bmOld = (HBITMAP)SelectObject(dcBmp,bmBmp);
				SetStretchBltMode(hDC,HALFTONE);
				StretchBlt(hDC,0,0,r.right,r.bottom,dcBmp,0,0,g_SFT.GetWidth(),g_SFT.GetHeight(),SRCCOPY);
				SelectObject(dcBmp,bmOld);
				DeleteObject(dcBmp);
				DeleteObject(bmBmp);
				ReleaseDC(hWnd,hDC);
			}
			return 0;
		}
		if(g_bSelect){
			RECT rc;
			GetClientRect(hWnd,&rc);
			RECT sr;
			GetWindowRect(g_hwStatus,&sr);
			sr.bottom-=sr.top;
			rc.bottom-=sr.bottom;
			HDC hDC = GetDC(hWnd);
			SetROP2(hDC,R2_NOT);
			MoveToEx(hDC,g_pSelect.x-rc.right/(g_SFT.GetZoomSize()*2),g_pSelect.y-rc.bottom/(g_SFT.GetZoomSize()*2),NULL);
			LineTo(hDC,g_pSelect.x+rc.right/(g_SFT.GetZoomSize()*2),g_pSelect.y-rc.bottom/(g_SFT.GetZoomSize()*2));
			LineTo(hDC,g_pSelect.x+rc.right/(g_SFT.GetZoomSize()*2),g_pSelect.y+rc.bottom/(g_SFT.GetZoomSize()*2));
			LineTo(hDC,g_pSelect.x-rc.right/(g_SFT.GetZoomSize()*2),g_pSelect.y+rc.bottom/(g_SFT.GetZoomSize()*2));
			LineTo(hDC,g_pSelect.x-rc.right/(g_SFT.GetZoomSize()*2),g_pSelect.y-rc.bottom/(g_SFT.GetZoomSize()*2));

			g_pSelect.x = (short)LOWORD(lParam);
			g_pSelect.y = (short)HIWORD(lParam);
			MoveToEx(hDC,g_pSelect.x-rc.right/(g_SFT.GetZoomSize()*2),g_pSelect.y-rc.bottom/(g_SFT.GetZoomSize()*2),NULL);
			LineTo(hDC,g_pSelect.x+rc.right/(g_SFT.GetZoomSize()*2),g_pSelect.y-rc.bottom/(g_SFT.GetZoomSize()*2));
			LineTo(hDC,g_pSelect.x+rc.right/(g_SFT.GetZoomSize()*2),g_pSelect.y+rc.bottom/(g_SFT.GetZoomSize()*2));
			LineTo(hDC,g_pSelect.x-rc.right/(g_SFT.GetZoomSize()*2),g_pSelect.y+rc.bottom/(g_SFT.GetZoomSize()*2));
			LineTo(hDC,g_pSelect.x-rc.right/(g_SFT.GetZoomSize()*2),g_pSelect.y-rc.bottom/(g_SFT.GetZoomSize()*2));
			ReleaseDC(hWnd,hDC);
		}
		char szI[128];
		strcpy(szI,"I:");

		RECT rc;
		GetClientRect(hWnd,&rc);
		RECT sr;
		GetWindowRect(g_hwStatus,&sr);
		sr.bottom-=sr.top;
		rc.bottom-=sr.bottom;
		int x = (short)LOWORD(lParam)*g_SFT.GetWidth()/rc.right;
		int y = (short)HIWORD(lParam)*g_SFT.GetHeight()/rc.bottom;
		int i = g_SFT.GetIterationOnPoint(x,y);
		if(i>=0){
			wsprintf(szI+strlen(szI),"%d",i);
			wsprintf(szI+strlen(szI)," <%d,%d> S:%d",(short)LOWORD(lParam),(short)HIWORD(lParam),g_SFT.GetTransOnPoint(x,y));
			SendMessage(g_hwStatus,SB_SETTEXT,2,(LPARAM)szI);
		}
	}
	else if(uMsg==WM_LBUTTONUP && g_bMove){
		g_bMove=FALSE;
		ReleaseCapture();
		POINT p;
		p.x = (short)LOWORD(lParam) - g_pSelect.x;
		p.y = (short)HIWORD(lParam) - g_pSelect.y;
		RECT rc;
		GetClientRect(hWnd,&rc);
		RECT sr;
		GetWindowRect(g_hwStatus,&sr);
		sr.bottom-=sr.top;
		rc.bottom-=sr.bottom;

		if(1){
			HDC hDC = GetDC(hWnd);
			HDC dcBmp = CreateCompatibleDC(hDC);
			HDC dcBuff = CreateCompatibleDC(hDC);
			HBITMAP bmBuff = ShrinkBitmap2(g_SFT.GetBitmap(),g_SFT.GetWidth(),g_SFT.GetHeight());
			HBITMAP bmOld = (HBITMAP)SelectObject(dcBmp,g_SFT.GetBitmap());
			HBITMAP bmOldBuff = (HBITMAP)SelectObject(dcBuff,bmBuff);
			int x = p.x*g_SFT.GetWidth()/rc.right;
			int y = p.y*g_SFT.GetHeight()/rc.bottom;
			BitBlt(dcBmp,x,y,g_SFT.GetWidth(),g_SFT.GetHeight(),dcBuff,0,0,SRCCOPY);
			SelectObject(dcBmp,bmOld);
			SelectObject(dcBuff,bmOldBuff);
			DeleteObject(bmBuff);
			DeleteDC(dcBmp);
			DeleteDC(dcBuff);
			ReleaseDC(NULL,hDC);
			g_SFT.UpdateBitmap();
		}

		p.x = (short)(rc.right/2-p.x)*g_SFT.GetWidth()/rc.right;
		p.y = (short)(rc.bottom/2-p.y)*g_SFT.GetHeight()/rc.bottom;

		SYSTEMTIME st;
		GetLocalTime(&st);
		SystemTimeToFileTime(&st,(LPFILETIME)&g_nTStart);
		if(g_bAutoGlitch)
			g_bAutoGlitch=1;

		g_SFT.Zoom(p.x,p.y,1,g_SFT.GetWidth(),g_SFT.GetHeight(),TRUE);
		SetTimer(hWnd,0,500,NULL);
		return 0;
	}
	else if(uMsg==WM_LBUTTONUP && g_bRotate){
		ReleaseCapture();
		g_bRotate=FALSE;
		CheckMenuItem(GetMenu(hWnd),ID_ACTIONS_ROTATE,MF_BYCOMMAND|(g_bRotate?MF_CHECKED:MF_UNCHECKED));

				HDC hDC = GetDC(hWnd);
				RECT r;
				GetClientRect(hWnd,&r);
				RECT sr;
				GetWindowRect(g_hwStatus,&sr);
				sr.bottom-=sr.top;
				r.bottom-=sr.bottom;
				POINT pm={r.right/2,r.bottom/2};
				HBITMAP bmBmp = create_bitmap(hDC,g_SFT.GetWidth(),g_SFT.GetHeight());
				POINT p;
				GetCursorPos(&p);
				ScreenToClient(hWnd,&p);
				double ratio = (double)(p.y-pm.y)/(double)(p.x-pm.x);
				g_MoveDegree = atan(ratio);
				if((p.x-pm.x>0 && !bXSign) || (p.x-pm.x<0 && bXSign))
					g_MoveDegree+=pi;
				g_MoveDegree -= g_StartDegree;
				pm.x = g_SFT.GetWidth()/2;
				pm.y = g_SFT.GetHeight()/2;
				RotateImage(g_SFT.GetBitmap(),bmBmp,pm,g_MoveDegree);
				HDC dcBmp = CreateCompatibleDC(hDC);
				HDC dcSft = CreateCompatibleDC(hDC);
				HBITMAP bmOld = (HBITMAP)SelectObject(dcBmp,bmBmp);
				HBITMAP bmOldSft = (HBITMAP)SelectObject(dcSft,g_SFT.GetBitmap());
				SetStretchBltMode(hDC,HALFTONE);
				SetStretchBltMode(dcSft,HALFTONE);
				StretchBlt(hDC,0,0,r.right,r.bottom,dcBmp,0,0,g_SFT.GetWidth(),g_SFT.GetHeight(),SRCCOPY);
				BitBlt(dcSft,0,0,g_SFT.GetWidth(),g_SFT.GetHeight(),dcBmp,0,0,SRCCOPY);
				SelectObject(dcBmp,bmOld);
				DeleteObject(dcBmp);
				DeleteObject(bmBmp);
				SelectObject(dcSft,bmOldSft);
				g_SFT.UpdateBitmap();
				DeleteObject(dcSft);
				ReleaseDC(hWnd,hDC);

		g_Degree += g_MoveDegree;
		SYSTEMTIME st;
		GetLocalTime(&st);
		SystemTimeToFileTime(&st,(LPFILETIME)&g_nTStart);
		SetTimer(hWnd,0,500,NULL);
//		std::cerr << "WM_LBUTTONUP && g_bRotate" << std::endl;
		g_SFT.RenderFractal(g_SFT.GetWidth(),g_SFT.GetHeight(),g_SFT.GetIterations(),hWnd);
	}
	else if(g_bTrackSelect==1 && (uMsg==WM_CAPTURECHANGED || uMsg==WM_RBUTTONUP || (uMsg==WM_KEYDOWN && wParam==VK_ESCAPE))){
		g_bTrackSelect=FALSE;
		ReleaseCapture();
		HDC hDC = GetDC(hWnd);
		SetROP2(hDC,R2_NOT);
		MoveToEx(hDC,g_pTrackStart.x,g_pTrackStart.y,NULL);
		LineTo(hDC,g_pSelect.x,g_pTrackStart.y);
		LineTo(hDC,g_pSelect.x,g_pSelect.y);
		LineTo(hDC,g_pTrackStart.x,g_pSelect.y);
		LineTo(hDC,g_pTrackStart.x,g_pTrackStart.y);
		ReleaseDC(hWnd,hDC);
		if(uMsg==WM_KEYDOWN && wParam==VK_ESCAPE){
			g_bTrackSelect=2;
			return 0;
		}
		if(g_pSelect.y==g_pTrackStart.y || g_pSelect.x==g_pTrackStart.x)
			return 0;

		RECT rc;
		GetClientRect(hWnd,&rc);
		RECT sr;
		GetWindowRect(g_hwStatus,&sr);
		sr.bottom-=sr.top;
		rc.bottom-=sr.bottom;
		double nZoom = (double)rc.bottom/(double)(g_pSelect.y-g_pTrackStart.y);
		if(nZoom<0)
			nZoom=-nZoom;
		POINT p;
		p.x = (g_pSelect.x+g_pTrackStart.x)/2;
		p.y = (g_pSelect.y+g_pTrackStart.y)/2;

		if(g_SFT.GetAnimateZoom()){
			g_pSelect.x = -g_SFT.GetWidth()/2;
			g_pSelect.y = g_SFT.GetHeight()/2;
			ANIM* pAnim = new ANIM;
			if(lParam==9)
				pAnim->nZoomSize = 2;
			else
				pAnim->nZoomSize = nZoom;
			pAnim->bmBmp = ShrinkBitmap2(g_SFT.GetBitmap(),rc.right,rc.bottom);
			pAnim->hWnd = hWnd;
			pAnim->pOffs = p;
			pAnim->bZoomOut = FALSE;
			pAnim->bZoomOne = FALSE;
			DWORD dw;
			HANDLE hThread = CreateThread(NULL,0,(LPTHREAD_START_ROUTINE)ThAnim,(LPVOID)pAnim,0,&dw);
			CloseHandle(hThread);
		}
		if (g_SFT.GetAutoIterations())
		{
			int nMin, nMax, nIter;
			g_SFT.GetIterations(nMin,nMax);
			nIter = g_SFT.GetIterations();
			if(nIter<nMin+nMin+2000)
				g_SFT.SetIterations(nMin+nMin+3000);
		}
		SYSTEMTIME st;
		GetLocalTime(&st);
		SystemTimeToFileTime(&st,(LPFILETIME)&g_nTStart);
		p.x = (short)(p.x)*g_SFT.GetWidth()/rc.right;
		p.y = (short)(p.y)*g_SFT.GetHeight()/rc.bottom;
		g_SFT.Zoom(p.x,p.y,nZoom,g_SFT.GetWidth(),g_SFT.GetHeight(),FALSE);
		SetTimer(hWnd,0,500,NULL);
	}
	else if(!g_bWaitRead && g_bSelect && ((uMsg==WM_KEYDOWN && wParam==VK_ESCAPE) || uMsg==WM_LBUTTONUP || uMsg==WM_CAPTURECHANGED)){
		g_bSelect=FALSE;
		ReleaseCapture();
		if(g_bRotate){
			g_bRotate=FALSE;
			return 0;
		}
		RECT rc;
		GetClientRect(hWnd,&rc);
		RECT sr;
		GetWindowRect(g_hwStatus,&sr);
		sr.bottom-=sr.top;
		rc.bottom-=sr.bottom;
		HDC hDC = GetDC(hWnd);
		SetROP2(hDC,R2_NOT);
		MoveToEx(hDC,g_pSelect.x-rc.right/(g_SFT.GetZoomSize()*2),g_pSelect.y-rc.bottom/(g_SFT.GetZoomSize()*2),NULL);
		LineTo(hDC,g_pSelect.x+rc.right/(g_SFT.GetZoomSize()*2),g_pSelect.y-rc.bottom/(g_SFT.GetZoomSize()*2));
		LineTo(hDC,g_pSelect.x+rc.right/(g_SFT.GetZoomSize()*2),g_pSelect.y+rc.bottom/(g_SFT.GetZoomSize()*2));
		LineTo(hDC,g_pSelect.x-rc.right/(g_SFT.GetZoomSize()*2),g_pSelect.y+rc.bottom/(g_SFT.GetZoomSize()*2));
		LineTo(hDC,g_pSelect.x-rc.right/(g_SFT.GetZoomSize()*2),g_pSelect.y-rc.bottom/(g_SFT.GetZoomSize()*2));
		ReleaseDC(hWnd,hDC);
		if(uMsg==WM_LBUTTONUP || uMsg==WM_CAPTURECHANGED){
			if(g_SFT.GetAnimateZoom() && !g_bAddMainReference && !g_bAddReference && !g_bEraser){
				ANIM* pAnim = new ANIM;
				pAnim->nZoomSize = g_SFT.GetZoomSize();
				pAnim->bmBmp = ShrinkBitmap2(g_SFT.GetBitmap(),rc.right,rc.bottom);
				pAnim->hWnd = hWnd;
				pAnim->pOffs = g_pSelect;
				pAnim->bZoomOut = FALSE;
				pAnim->bZoomOne = FALSE;
				DWORD dw;
				HANDLE hThread = CreateThread(NULL,0,(LPTHREAD_START_ROUTINE)ThAnim,(LPVOID)pAnim,0,&dw);
				CloseHandle(hThread);
			}

			if(!g_bAddMainReference && !g_bAddReference && !g_bEraser){
				HDC hDC = GetDC(NULL);
				HDC dcBmp = CreateCompatibleDC(hDC);
				HBITMAP bmBmp = g_SFT.GetBitmap();

				HBITMAP bmOld = (HBITMAP)SelectObject(dcBmp,bmBmp);
				HDC dcSBmp = CreateCompatibleDC(hDC);
				SIZE sc, sb;
				POINT pSelect;
				sb.cx = g_SFT.GetWidth();
				sb.cy = g_SFT.GetHeight();
				pSelect.x = g_pSelect.x*sb.cx/rc.right;
				pSelect.y = g_pSelect.y*sb.cy/rc.bottom;
				sc.cx = sb.cx/(g_SFT.GetZoomSize());
				sc.cy = sb.cy/(g_SFT.GetZoomSize());
				HBITMAP bmSBmp = create_bitmap(hDC,sc.cx,sc.cy);
				HBITMAP bmSOld = (HBITMAP)SelectObject(dcSBmp,bmSBmp);
				SetStretchBltMode(dcSBmp,HALFTONE);
				SetStretchBltMode(dcBmp,HALFTONE);
				BitBlt(dcSBmp,0,0,sc.cx,sc.cy,dcBmp,pSelect.x-sb.cx/(g_SFT.GetZoomSize()*2),pSelect.y-sb.cy/(g_SFT.GetZoomSize()*2),SRCCOPY);
				StretchBlt(dcBmp,0,0,sb.cx,sb.cy,dcSBmp,0,0,sc.cx,sc.cy,SRCCOPY);
				SelectObject(dcBmp,bmOld);
				SelectObject(dcSBmp,bmSOld);
				DeleteObject(bmSBmp);
				DeleteDC(dcBmp);
				DeleteDC(dcSBmp);
				ReleaseDC(NULL,hDC);
				g_SFT.UpdateBitmap();
				if(!g_SFT.GetAnimateZoom()){
					InvalidateRect(hWnd,NULL,FALSE);
					UpdateWindow(hWnd);
				}

				if(!g_hwExamine && g_SFT.GetAutoIterations()){
					int nMin, nMax, nIter;
					g_SFT.GetIterations(nMin,nMax);
					nIter = g_SFT.GetIterations();
					if(nIter<nMin+nMin+2000)
						g_SFT.SetIterations(nMin+nMin+3000);
				}
			}

			SYSTEMTIME st;
			GetLocalTime(&st);
			SystemTimeToFileTime(&st,(LPFILETIME)&g_nTStart);

			int x = (short)LOWORD(lParam)*g_SFT.GetWidth()/rc.right;
			int y = (short)HIWORD(lParam)*g_SFT.GetHeight()/rc.bottom;


			if(g_bAddMainReference){
				if(!g_hwExamine)
					g_bAddMainReference=false;
				if(g_hwExamine){
					g_bExamineDirty=TRUE;
					SetFocus(g_hwExamine);
				}
				CheckMenuItem(GetMenu(hWnd),ID_ACTIONS_SPECIAL_SETMAINREFERENCE,MF_BYCOMMAND|MF_UNCHECKED);
				int x = (short)LOWORD(lParam)*g_SFT.GetWidth()/rc.right;
				int y = (short)HIWORD(lParam)*g_SFT.GetHeight()/rc.bottom;
				g_SFT.AddReference(x,y,TRUE);
				SetTimer(hWnd,0,500,NULL);
				return 0;
			}
			else if(g_bAddReference){
				if(!g_hwExamine)
					g_bAddReference=FALSE;
				if(g_hwExamine){
					g_bExamineDirty=TRUE;
					SetFocus(g_hwExamine);
				}
				CheckMenuItem(GetMenu(hWnd),ID_ACTIONS_ADDREFERENCE,MF_BYCOMMAND|MF_UNCHECKED);
				int x = (short)LOWORD(lParam)*g_SFT.GetWidth()/rc.right;
				int y = (short)HIWORD(lParam)*g_SFT.GetHeight()/rc.bottom;
				if(g_SFT.AddReference(x,y,FALSE,g_SFT.GetSolveGlitchNear()))
					SetTimer(hWnd,0,500,NULL);
				return 0;
			}
			else if(g_bEraser){
				RECT rE = {LONG(g_pSelect.x-rc.right/(g_SFT.GetZoomSize()*2)),LONG(g_pSelect.y-rc.bottom/(g_SFT.GetZoomSize()*2)),LONG(g_pSelect.x+rc.right/(g_SFT.GetZoomSize()*2)),LONG(g_pSelect.y+rc.bottom/(g_SFT.GetZoomSize()*2))};
				rE.left = g_SFT.GetWidth()*rE.left/rc.right;
				rE.top = g_SFT.GetHeight()*rE.top/rc.bottom;
				rE.right = g_SFT.GetWidth()*rE.right/rc.right;
				rE.bottom = g_SFT.GetHeight()*rE.bottom/rc.bottom;
				int x, y;
				for(x=rE.left;x<rE.right;x++)
					for(y=rE.top;y<rE.bottom;y++)
						g_SFT.ErasePixel(x,y);
				g_bExamineDirty=TRUE;
				g_SFT.ApplyColors();
				InvalidateRect(hWnd,NULL,FALSE);
				UpdateWindow(hWnd);
				return 0;
			}
			else
				g_SFT.Zoom(x,y,g_SFT.GetZoomSize(),g_SFT.GetWidth(),g_SFT.GetHeight(),g_SFT.GetZoomSize()==1);
			SetTimer(hWnd,0,500,NULL);
		}
	}
	else if(uMsg==WM_KEYDOWN && wParam==VK_F5){
		if(g_hwExamine)
			g_bExamineDirty=TRUE;
		g_nPrevGlitchX=g_nPrevGlitchY=-1;
		g_SFT.Stop();
		g_bAnim=false;
		g_bFindMinibrot=FALSE;
		g_bStoreZoom=FALSE;
		DeleteObject(g_bmSaveZoomBuff);
		g_bmSaveZoomBuff=NULL;
		SetTimer(hWnd,0,500,NULL);
		RECT r;
		GetClientRect(hWnd,&r);
		RECT sr;
		GetWindowRect(g_hwStatus,&sr);
		sr.bottom-=sr.top;
		r.bottom-=sr.bottom;
		SYSTEMTIME st;
		GetLocalTime(&st);
		SystemTimeToFileTime(&st,(LPFILETIME)&g_nTStart);
#if 0
		if(g_SFT.GetWidth()<r.right || g_SFT.GetHeight()<r.bottom)
		{
//			std::cerr << "WM_KEYDOWN && wParam==VK_F5 && small" << std::endl;
			g_SFT.RenderFractal(r.right,r.bottom,g_SFT.GetIterations(),hWnd);
		}
		else
#endif
		if(g_SFT.GetArbitrarySize()){
			SIZE sc;
			sc.cy = g_SFT.GetHeight();
			sc.cx = (double)r.right*((double)sc.cy/(double)r.bottom);
//			std::cerr << "WM_KEYDOWN && wParam==VK_F5 && arbitrary" << std::endl;
			g_SFT.RenderFractal(sc.cx,sc.cy,g_SFT.GetIterations(),hWnd);
		}
		else
		{
//			std::cerr << "WM_KEYDOWN && wParam==VK_F5 && otherwise" << std::endl;
			g_SFT.RenderFractal(g_SFT.GetWidth(),g_SFT.GetHeight(),g_SFT.GetIterations(),hWnd);
		}
	}
	else if(uMsg==WM_KEYDOWN && wParam==VK_ESCAPE){
		if(g_bAddReference){
			g_bAddReference=FALSE;
			CheckMenuItem(GetMenu(hWnd),ID_ACTIONS_ADDREFERENCE,MF_BYCOMMAND|MF_UNCHECKED);
		}
		if(g_bAddMainReference){
			g_bAddMainReference=false;
			CheckMenuItem(GetMenu(hWnd),ID_ACTIONS_SPECIAL_SETMAINREFERENCE,MF_BYCOMMAND|MF_UNCHECKED);
		}
		g_SFT.Stop();
		g_bAnim=false;
		g_bFindMinibrot=FALSE;
		g_bStoreZoom=FALSE;
		DeleteObject(g_bmSaveZoomBuff);
		g_bmSaveZoomBuff=NULL;
	}
	else if(uMsg==WM_KEYDOWN && wParam=='V'){
		if(wParam=='V'){
			if(HIWORD(GetKeyState(VK_CONTROL))){
				MessageBox(NULL,g_SFT.GetPosition(),g_SFT.ToZoom(),MB_OK);
			}
			else{
				if(!OpenClipboard(hWnd))
					return 0;
				HANDLE hTemp;
				char *szTemp=0;
				if((hTemp = GetClipboardData(CF_TEXT)))
					szTemp = (char*)GlobalLock(hTemp);
				if(!szTemp || !*szTemp){
					GlobalUnlock(hTemp);
					CloseClipboard();
					return 0;
				}
				CStringTable stP(szTemp,":","\r\n");
				GlobalUnlock(hTemp);
				CloseClipboard();

				if(stP.FindString(0,"MANDELBROT")!=-1){
					g_SFT.Stop();
					g_bAnim=false;
					g_bFindMinibrot=FALSE;
					g_bStoreZoom=FALSE;
					DeleteObject(g_bmSaveZoomBuff);
					g_bmSaveZoomBuff=NULL;
					RECT r;
					GetClientRect(hWnd,&r);
					RECT sr;
					GetWindowRect(g_hwStatus,&sr);
					sr.bottom-=sr.top;
					r.bottom-=sr.bottom;
					g_SFT.SetPosition(
						(CFixedFloat)Trim(stP[stP.FindString(0,"R-Start")][1]),
						(CFixedFloat)Trim(stP[stP.FindString(0,"R-Stop")][1]),
						(CFixedFloat)Trim(stP[stP.FindString(0,"I-Start")][1]),
						(CFixedFloat)Trim(stP[stP.FindString(0,"I-Stop")][1]),
						r.right,r.bottom);
					PostMessage(hWnd,WM_KEYDOWN,VK_F5,0);
				}
			}
		}
	}
	else if(uMsg==WM_KEYDOWN && wParam==187){
		lParam=9;
		g_bAddReference=FALSE;
		g_bAddMainReference=false;
		g_bFindMinibrot=FALSE;
		g_bStoreZoom=FALSE;
		DeleteObject(g_bmSaveZoomBuff);
		g_bmSaveZoomBuff=NULL;
		while(g_bAnim){
			Sleep(3);
		}
		POINT p;
		GetCursorPos(&p);
		ScreenToClient(hWnd,&p);
		RECT rc;
		GetClientRect(hWnd,&rc);
		RECT sr;
		GetWindowRect(g_hwStatus,&sr);
		sr.bottom-=sr.top;
		rc.bottom-=sr.bottom;
		if(!g_SFT.GetAnimateZoom()){
			SendMessage(hWnd,WM_KEYDOWN,VK_ESCAPE,0);
			SendMessage(hWnd,WM_LBUTTONDOWN,0,MAKELONG(p.x,p.y));
			PostMessage(hWnd,WM_LBUTTONUP,0,MAKELONG(p.x,p.y));
			return 0;
		}
		if(lParam==9){
			p.x=rc.right/2 + (p.x-rc.right/2)/2;
			p.y=rc.bottom/2 + (p.y-rc.bottom/2)/2;
		}
		if (g_SFT.GetAutoIterations())
		{
			int nMin, nMax, nIter;
			g_SFT.GetIterations(nMin,nMax);
			nIter = g_SFT.GetIterations();
			if(nIter<nMin+nMin/2+2000)
				g_SFT.SetIterations(nMin+nMin/2+3000);
		}
		g_pSelect.x = -g_SFT.GetWidth()/2;
		g_pSelect.y = g_SFT.GetHeight()/2;
		ANIM* pAnim = new ANIM;
		if(lParam==9)
			pAnim->nZoomSize = 2;
		else
			pAnim->nZoomSize = g_SFT.GetZoomSize();
		pAnim->bmBmp = ShrinkBitmap2(g_SFT.GetBitmap(),rc.right,rc.bottom);
		pAnim->hWnd = hWnd;
		pAnim->pOffs = p;
		pAnim->bZoomOut = FALSE;
		pAnim->bZoomOne = FALSE;
		UpdateBkpImage(pAnim);
		if(g_SFT.GetAnimateZoom()){
			DWORD dw;
			HANDLE hThread = CreateThread(NULL,0,(LPTHREAD_START_ROUTINE)ThAnim,(LPVOID)pAnim,0,&dw);
			CloseHandle(hThread);
		}
		else
			delete pAnim;

		SYSTEMTIME st;
		GetLocalTime(&st);
		SystemTimeToFileTime(&st,(LPFILETIME)&g_nTStart);
		p.x = (short)(p.x)*g_SFT.GetWidth()/rc.right;
		p.y = (short)(p.y)*g_SFT.GetHeight()/rc.bottom;
		g_SFT.Zoom(p.x,p.y,(lParam==9?2:g_SFT.GetZoomSize()),g_SFT.GetWidth(),g_SFT.GetHeight(),FALSE);
		SetTimer(hWnd,0,500,NULL);
		MSG msg;
		while(PeekMessage(&msg,hWnd,WM_KEYDOWN,WM_KEYDOWN,PM_REMOVE));
	}
	else if(uMsg==WM_KEYDOWN && wParam==189){
		lParam=9;
		g_bAddReference=FALSE;
		g_bAddMainReference=false;
		g_bFindMinibrot=FALSE;
		g_bStoreZoom=FALSE;
		DeleteObject(g_bmSaveZoomBuff);
		g_bmSaveZoomBuff=NULL;
		while(g_bAnim){
			Sleep(3);
		}
		POINT p;
		GetCursorPos(&p);
		ScreenToClient(hWnd,&p);
		RECT rc;
		GetClientRect(hWnd,&rc);
		RECT sr;
		GetWindowRect(g_hwStatus,&sr);
		sr.bottom-=sr.top;
		rc.bottom-=sr.bottom;
		if(!g_SFT.GetAnimateZoom()){
			SendMessage(hWnd,WM_KEYDOWN,VK_ESCAPE,0);
			SendMessage(hWnd,WM_RBUTTONDOWN,0,MAKELONG(p.x,p.y));
			PostMessage(hWnd,WM_RBUTTONUP,0,MAKELONG(p.x,p.y));
			return 0;
		}
		if(lParam==9){
			p.x=rc.right/2 - (p.x-rc.right/2);
			p.y=rc.bottom/2 - (p.y-rc.bottom/2);
		}

		g_pSelect.x = -g_SFT.GetWidth()/2;
		g_pSelect.y = g_SFT.GetHeight()/2;
		ANIM* pAnim = new ANIM;
		if(lParam==9)
			pAnim->nZoomSize = 2;
		else
			pAnim->nZoomSize = g_SFT.GetZoomSize();
		pAnim->bmBmp = ShrinkBitmap2(g_SFT.GetBitmap(),rc.right,rc.bottom);
		pAnim->hWnd = hWnd;
		pAnim->pOffs = p;
		pAnim->bZoomOut = TRUE;
		pAnim->bZoomOne = FALSE;
		UpdateBkpImage(pAnim);
		if(g_SFT.GetAnimateZoom()){
			DWORD dw;
			HANDLE hThread = CreateThread(NULL,0,(LPTHREAD_START_ROUTINE)ThAnim,(LPVOID)pAnim,0,&dw);
			CloseHandle(hThread);
		}
		else
			delete pAnim;

		p.x = (short)(p.x)*g_SFT.GetWidth()/rc.right;
		p.y = (short)(p.y)*g_SFT.GetHeight()/rc.bottom;
		g_SFT.Zoom(p.x,p.y,(lParam==9?.5:(double)1/(double)g_SFT.GetZoomSize()),g_SFT.GetWidth(),g_SFT.GetHeight(),FALSE);
		SetTimer(hWnd,0,500,NULL);
		MSG msg;
		while(PeekMessage(&msg,hWnd,WM_KEYDOWN,WM_KEYDOWN,PM_REMOVE));
	}
	else if(uMsg==0x020A){//WM_MOUSEWHEEL
		if((short)HIWORD(wParam)>0)
			SendMessage(hWnd,WM_KEYDOWN,187,9);
		else
			SendMessage(hWnd,WM_KEYDOWN,189,9);
		MSG msg;
		while(PeekMessage(&msg,hWnd,0x020A,0x020A,PM_REMOVE));
	}
	else if(uMsg==WM_COMMAND && wParam==ID_ACTIONSSHOWITERATIONS){
		g_SFT.ApplyIterationColors();
		InvalidateRect(hWnd,NULL,FALSE);
	}
	else if(uMsg==WM_COMMAND && wParam==ID_ACTIONS_SPECIAL_SHOWSMOOTHTRANSITIONCOLORS){
		g_SFT.ApplySmoothColors();
		InvalidateRect(hWnd,NULL,FALSE);
	}
	else if(uMsg==WM_COMMAND && wParam==ID_ACTIONS_ANIMATEZOOM){
		g_SFT.SetAnimateZoom(! g_SFT.GetAnimateZoom());
		UpdateAnimateZoom(hWnd);
		WritePrivateProfileString("SETTINGS","AnimateZoom",g_SFT.GetAnimateZoom()?"1":"0","fraktal_sft.ini");
	}
	else if(uMsg==WM_COMMAND && wParam==ID_SPECIAL_ARBITRARYSIZE){
		if(g_SFT.GetArbitrarySize()){
			g_SFT.SetRatio(640,360);
			g_SFT.SetArbitrarySize(false);
			SendMessage(hWnd,WM_SIZE,0,0);
			g_SFT.SetArbitrarySize(true);
			SendMessage(hWnd,WM_KEYDOWN,VK_F5,0);
		}
		g_SFT.SetArbitrarySize(! g_SFT.GetArbitrarySize());
		CheckMenuItem(GetMenu(hWnd),ID_SPECIAL_ARBITRARYSIZE,MF_BYCOMMAND|(g_SFT.GetArbitrarySize()?MF_CHECKED:MF_UNCHECKED));
		WritePrivateProfileString("SETTINGS","ArbitrarySize",g_SFT.GetArbitrarySize()?"1":"0","fraktal_sft.ini");
	}
	else if(uMsg==WM_COMMAND && wParam==ID_SPECIAL_NEWTON){
		g_bNewton=!g_bNewton;
		CheckMenuItem(GetMenu(hWnd),ID_SPECIAL_NEWTON,MF_BYCOMMAND|(g_bNewton?MF_CHECKED:MF_UNCHECKED));
		if(!g_bNewton && g_hwNewton){
			if(g_bNewtonRunning){
				g_bNewtonStop=TRUE;
				while(g_bNewtonRunning)
					Sleep(10);
			}
			DestroyWindow(g_hwNewton);
			g_hwNewton=NULL;
		}
		else{
			g_hwNewton = CreateDialog(GetModuleHandle(NULL),MAKEINTRESOURCE(IDD_DIALOG12),hWnd,(DLGPROC)NewtonProc);
			ShowWindow(g_hwNewton,SW_SHOW);
		}
	}

#ifdef KF_OPENCL
	else if(uMsg==WM_COMMAND && wParam==ID_SPECIAL_OPENCL){
		if(g_hwOpenCL){
			DestroyWindow(g_hwOpenCL);
			g_hwOpenCL=NULL;
		}
		g_hwOpenCL = CreateDialog(GetModuleHandle(NULL),MAKEINTRESOURCE(IDD_DIALOG_OPENCL),hWnd,(DLGPROC)OpenCLProc);
		ShowWindow(g_hwOpenCL,SW_SHOW);
	}
#endif

	else if(uMsg==WM_COMMAND && wParam==ID_ACTIONS_SKEW){
		g_bShowSkew=TRUE;
		g_DialogInit=0;
		SYSTEMTIME st;

		g_SFT.Stop();
		g_nSkewStretch = 100*g_SFT.GetRatioY()/360;
		SIZE size;
		size.cx = g_SFT.GetWidth();
		size.cy = g_SFT.GetHeight();
		double xRatio = 640.0/size.cx;
		size.cx = 640;
		size.cy = size.cy*xRatio;
		xRatio = (double)360/(double)size.cy;
		g_nSkewStretch*=xRatio;

		g_nSkewRotate = 180*g_Degree/pi;
		HBITMAP bmBmp = g_SFT.GetBitmap();
		UnSkewImage(bmBmp);
		g_SFT.UpdateBitmap();

		if(DialogBoxParam(GetModuleHandle(NULL),MAKEINTRESOURCE(IDD_DIALOG10),hWnd,(DLGPROC)SkewProc,(LPARAM)hWnd)){
			g_bShowSkew=FALSE;

			SIZE size;
			size.cx = g_SFT.GetWidth();
			size.cy = g_SFT.GetHeight();
			double xRatio = 640.0/size.cx;
			size.cx = 640;
			size.cy = size.cy*xRatio;
			xRatio = (double)size.cy/(double)360;
			g_nSkewStretch*=xRatio;

			g_SFT.SetRatio(640,360*g_nSkewStretch/100);
			g_Degree = pi*(double)(g_nSkewRotate)/180;
			GetLocalTime(&st);
			SystemTimeToFileTime(&st,(LPFILETIME)&g_nTStart);
			g_SFT.UpdateBitmap();
			g_SFT.RenderFractal(g_SFT.GetWidth(),g_SFT.GetHeight(),g_SFT.GetIterations(),hWnd);
			SetTimer(hWnd,0,500,NULL);
		}
		else{
			g_bShowSkew=FALSE;
			g_SFT.ApplyColors();
			InvalidateRect(hWnd,NULL,FALSE);
		}

	}
	else if(uMsg==WM_COMMAND && wParam==ID_ACTIONS_SHOWINFLECTION){
		g_bShowInflection=!g_bShowInflection;
		g_nInflection=0;
		CheckMenuItem(GetMenu(hWnd),ID_ACTIONS_SHOWINFLECTION,MF_BYCOMMAND|(g_bShowInflection?MF_CHECKED:MF_UNCHECKED));
		InvalidateRect(hWnd,NULL,FALSE);
	}
	else if(uMsg==WM_COMMAND && wParam==ID_SPECIAL_SHOWGLITCHES){
		g_SFT.SetShowGlitches(!g_SFT.GetShowGlitches());
		WritePrivateProfileString("SETTINGS","ShowGlitches",g_SFT.GetShowGlitches()?"1":"0","fraktal_sft.ini");
		CheckMenuItem(GetMenu(hWnd),ID_SPECIAL_SHOWGLITCHES,MF_BYCOMMAND|(g_SFT.GetShowGlitches()?MF_CHECKED:MF_UNCHECKED));
	}
	else if(uMsg==WM_COMMAND && wParam==ID_ACTIONS_SPECIAL_NOREUSECENTER){
		g_SFT.SetNoReuseCenter(!g_SFT.GetNoReuseCenter());
		CheckMenuItem(GetMenu(hWnd),ID_ACTIONS_SPECIAL_NOREUSECENTER,MF_BYCOMMAND|(g_SFT.GetNoReuseCenter()?MF_CHECKED:MF_UNCHECKED));
	}
	else if(uMsg==WM_COMMAND && wParam==ID_ACTIONS_ROTATE){
		g_bRotate=!g_bRotate;
		CheckMenuItem(GetMenu(hWnd),ID_ACTIONS_ROTATE,MF_BYCOMMAND|(g_bRotate?MF_CHECKED:MF_UNCHECKED));
	}
	else if(uMsg==WM_COMMAND && wParam==ID_ACTIONS_RESETROTATION){
		g_SFT.Stop();
		g_bAnim=false;
		g_Degree=0;
		SetTimer(hWnd,0,500,NULL);
		g_SFT.RenderFractal(g_SFT.GetWidth(),g_SFT.GetHeight(),g_SFT.GetIterations(),hWnd);
	}
	else if(uMsg==WM_COMMAND && wParam==ID_ACTIONS_SPECIAL_SPECIAL_MIRROR1){
		if(g_SFT.GetMirror()==1)
			g_SFT.SetMirror(0);
		else
			g_SFT.SetMirror(1);
		UpdateMirror(hWnd);
	}
	else if(uMsg==WM_COMMAND && wParam==ID_ACTIONS_SPECIAL_SPECIAL_SETRATIO){
		g_JpegParams.nWidth = g_SFT.GetRatioX();
		g_JpegParams.nHeight = g_SFT.GetRatioY();
		if(!DialogBoxParam(GetModuleHandle(NULL),MAKEINTRESOURCE(IDD_DIALOG7),hWnd,(DLGPROC)JpegProc,2))
			return 0;
		g_SFT.SetRatio(g_JpegParams.nWidth,g_JpegParams.nHeight);
		SetTimer(hWnd,0,500,NULL);
		g_SFT.RenderFractal(g_SFT.GetWidth(),g_SFT.GetHeight(),g_SFT.GetIterations(),hWnd);
	}
	else if(uMsg==WM_COMMAND && wParam==ID_ACTIONS_SPECIAL_SPECIAL_RESETRATIO){
		g_SFT.SetRatio(640,360);
		SetTimer(hWnd,0,500,NULL);
		g_SFT.RenderFractal(g_SFT.GetWidth(),g_SFT.GetHeight(),g_SFT.GetIterations(),hWnd);
	}
	else if(uMsg==WM_COMMAND && wParam==ID_FILE_SAVEMAP){
		if(BrowseFile(hWnd,FALSE,"Save Map","Kalle's fraktaler\0*.kfb\0\0",g_szFile))
			g_SFT.SaveMapB(g_szFile);
	}
	else if(uMsg==WM_COMMAND && (wParam==ID_FILE_STOREZOOMOUTIMAGES)){
		if(g_SFT.GetZoomSize()!=2 && MessageBox(hWnd,"The Zoom size is not 2, do you want to proceed?\n\nTo preserve quality the lowest Zoom size is recommended.","Kalle's Fraktaler",MB_OKCANCEL)==IDCANCEL)
			return 0;
		MainProc(hWnd,WM_COMMAND,ID_FILE_SAVEAS_,0);
		g_JpegParams.nWidth = g_SFT.GetWidth();
		g_JpegParams.nHeight = g_SFT.GetHeight();
		g_JpegParams.nQuality = 100;
		while(1){
			if(!DialogBoxParam(GetModuleHandle(NULL),MAKEINTRESOURCE(IDD_DIALOG7),hWnd,(DLGPROC)JpegProc,0))
				return 0;
			if(g_JpegParams.nWidth>3840){
				int nR;
				if((nR=MessageBox(hWnd,"Width can not be bigger than 3840, do you want to proceed anyway?","Error",MB_YESNOCANCEL|MB_ICONSTOP))==IDCANCEL)
					return 0;
				if(nR==IDYES)
					break;
			}
			else
				break;
		}
		if(!DialogBoxParam(GetModuleHandle(NULL),MAKEINTRESOURCE(IDD_STOREZOOM),hWnd,(DLGPROC)StoreZoomProc,0))
			return 0;
		g_szFile = get_filename_path(g_szFile);
		if(!Browse(hWnd,g_szFile))
			return 0;
		if (g_szFile[g_szFile.length() - 1] != '\\')
			g_szFile += "\\";
		SetTimer(hWnd,0,500,NULL);
		g_bStoreZoom=1;
		std::string szFile = g_szFile;
		szFile = replace_path_filename(szFile, store_zoom_filename(g_bStoreZoom, "*", "kfb"));
		while(FileExists(szFile)){
			g_bStoreZoom++;
			szFile = replace_path_filename(szFile, store_zoom_filename(g_bStoreZoom, "*", "kfb"));
		}
		g_SFT.StoreLocation();
		g_SFT.RenderFractal(g_JpegParams.nWidth,g_JpegParams.nHeight,g_SFT.GetIterations(),hWnd);
	}
	else if((uMsg==WM_COMMAND && wParam==ID_ACTIONS_CENTERCURSOR) || (uMsg==WM_KEYDOWN && wParam=='U' && HIWORD(GetKeyState(VK_CONTROL)))){
		POINT p;
		int p_x, p_y;
		if(g_SFT.Center(p_x, p_y)){
			p.x = p_x;
			p.y = p_y;
			RECT rc;
			GetClientRect(hWnd,&rc);
			RECT sr;
			GetWindowRect(g_hwStatus,&sr);
			sr.bottom-=sr.top;
			rc.bottom-=sr.bottom;
			p.x = p.x*rc.right/g_SFT.GetWidth();
			p.y = p.y*rc.bottom/g_SFT.GetHeight();

			ClientToScreen(hWnd,&p);
			SetCursorPos(p.x,p.y);
		}
		else
			MessageBox(hWnd,"Could not find center","Kalle's Fraktaler",MB_OK|MB_ICONINFORMATION);
	}
	else if(uMsg==WM_COMMAND && wParam==ID_ACTIONS_SPECIAL_NOAPPROXIMATION){
		g_SFT.SetNoApprox(!g_SFT.GetNoApprox());
		UpdateNoApprox(hWnd);
	}

	else if((uMsg==WM_COMMAND && wParam==ID_ACTIONS_SETIMAGESIZE) || (uMsg==WM_KEYDOWN && wParam=='Z' && HIWORD(GetKeyState(VK_CONTROL)))){
		g_JpegParams.nWidth = g_SFT.GetWidth();
		g_JpegParams.nHeight = g_SFT.GetHeight();
		if(!DialogBoxParam(GetModuleHandle(NULL),MAKEINTRESOURCE(IDD_DIALOG7),hWnd,(DLGPROC)JpegProc,1))
			return 0;
		g_SFT.Stop();
		g_bAnim=false;
		g_nPrevGlitchX=g_nPrevGlitchY=-1;
		g_bFindMinibrot=FALSE;
		g_bStoreZoom=FALSE;
		DeleteObject(g_bmSaveZoomBuff);
		g_bmSaveZoomBuff=NULL;
		SYSTEMTIME st;
		GetLocalTime(&st);
		SystemTimeToFileTime(&st,(LPFILETIME)&g_nTStart);
		g_nPrevGlitchX=g_nPrevGlitchY=-1;
		SetTimer(hWnd,0,500,NULL);
		g_SFT.RenderFractal(g_JpegParams.nWidth,g_JpegParams.nHeight,g_SFT.GetIterations(),hWnd);
	}
	else if((uMsg==WM_COMMAND && wParam==ID_ACTIONS_SETWINDOWSIZE) || (uMsg==WM_KEYDOWN && wParam=='W' && HIWORD(GetKeyState(VK_CONTROL)))){
		RECT wr, cr;
		GetClientRect(hWnd,&cr);
		g_scSize.cx = cr.right;
		g_scSize.cy = cr.bottom;
		RECT sr;
		GetWindowRect(g_hwStatus,&sr);
		sr.bottom-=sr.top;
		g_JpegParams.nWidth = cr.right;
		g_JpegParams.nHeight = cr.bottom-sr.bottom;
		if(!DialogBoxParam(GetModuleHandle(NULL),MAKEINTRESOURCE(IDD_DIALOG7),hWnd,(DLGPROC)JpegProc,3))
			return 0;
		GetWindowRect(hWnd,&wr);
		wr.right-=wr.left;
		wr.bottom-=wr.top;
		int nXOffs = g_JpegParams.nWidth-cr.right;
		wr.right+=nXOffs;
		wr.left-=nXOffs/2;
		int nYOffs = g_JpegParams.nHeight-cr.bottom;
		wr.bottom+=nYOffs+sr.bottom;
		wr.top-=nYOffs/2+sr.bottom/2;
		g_SFT.SetWindowWidth(g_JpegParams.nWidth);
		g_SFT.SetWindowHeight(g_JpegParams.nHeight);
		g_SFT.SetWindowLeft(wr.left);
		g_SFT.SetWindowTop(wr.top);
		g_SFT.SetWindowRight(wr.right);
		g_SFT.SetWindowBottom(wr.bottom);
		UpdateWindowSize(hWnd);
	}
	else if(uMsg==WM_COMMAND && wParam==ID_ACTIONS_FINDHIGHESTITERATION){
		POINT p;
		int p_x, p_y;
		if(g_SFT.HighestIteration(p_x, p_y)){
			p.x = p_x;
			p.y = p_y;
			RECT rc;
			GetClientRect(hWnd,&rc);
			RECT sr;
			GetWindowRect(g_hwStatus,&sr);
			sr.bottom-=sr.top;
			rc.bottom-=sr.bottom;
			p.x = p.x*rc.right/g_SFT.GetWidth();
			p.y = p.y*rc.bottom/g_SFT.GetHeight();
			ClientToScreen(hWnd,&p);
			SetCursorPos(p.x,p.y);
		}
	}
	else if(uMsg==WM_COMMAND && wParam==ID_ACTIONS_ADDREFERENCE){
		g_bAddReference=TRUE;
		CheckMenuItem(GetMenu(hWnd),ID_ACTIONS_ADDREFERENCE,MF_BYCOMMAND|MF_CHECKED);

		g_bAddMainReference=false;
		CheckMenuItem(GetMenu(hWnd),ID_ACTIONS_SPECIAL_SETMAINREFERENCE,MF_BYCOMMAND|MF_UNCHECKED);
	}
	else if(uMsg==WM_COMMAND && wParam==ID_ACTIONS_SPECIAL_ADDREFERENCEERRORS){
		g_bAddReference=FALSE;
		CheckMenuItem(GetMenu(hWnd),ID_ACTIONS_ADDREFERENCE,MF_BYCOMMAND|MF_UNCHECKED);
		g_bAddMainReference=false;
		CheckMenuItem(GetMenu(hWnd),ID_ACTIONS_SPECIAL_SETMAINREFERENCE,MF_BYCOMMAND|MF_UNCHECKED);
	}
	else if(uMsg==WM_COMMAND && wParam==ID_ACTIONS_SPECIAL_SETMAINREFERENCE){
		g_bAddMainReference=true;
		CheckMenuItem(GetMenu(hWnd),ID_ACTIONS_SPECIAL_SETMAINREFERENCE,MF_BYCOMMAND|MF_CHECKED);

		g_bAddReference=FALSE;
		CheckMenuItem(GetMenu(hWnd),ID_ACTIONS_ADDREFERENCE,MF_BYCOMMAND|MF_UNCHECKED);
	}
	else if(uMsg==WM_COMMAND && wParam==ID_ACTIONS_FINDCENTEROFGLITCH){
		POINT p;
		int p_x, p_y;
		if(g_SFT.FindCenterOfGlitch(p_x, p_y)){
			RECT rc;
			GetClientRect(hWnd,&rc);
			RECT sr;
			GetWindowRect(g_hwStatus,&sr);
			sr.bottom-=sr.top;
			rc.bottom-=sr.bottom;
			p.x = p.x*rc.right/g_SFT.GetWidth();
			p.y = p.y*rc.bottom/g_SFT.GetHeight();
			ClientToScreen(hWnd,&p);
			SetCursorPos(p.x,p.y);
		}
		else
			MessageBox(hWnd,"Could not find any glitches","Kalle's Fraktaler",MB_OK|MB_ICONINFORMATION);
	}
	else if(uMsg==WM_COMMAND && wParam==ID_FILE_EXAMINEZOOMSEQUENCE){
		bool ok = Examine(hWnd);
		if (! ok)
			return 0;
	}
	else if(uMsg==WM_COMMAND && wParam==ID_FILE_RESUMEZOOMSEQUENCE){
		return ResumeZoomSequence(hWnd);
	}
	else if(uMsg==WM_COMMAND && wParam==ID_ACTIONS_SPECIAL_SOLVEGLITCHWITHNEARPIXELSMETHOD){
		g_SFT.SetSolveGlitchNear(! g_SFT.GetSolveGlitchNear());
		UpdateSolveGlitchNear(hWnd);
	}
	else if(uMsg==WM_COMMAND && wParam==ID_ACTIONS_AUTOSOLVEGLITCHES){
		g_bAutoGlitch=!g_bAutoGlitch;
		g_nPrevGlitchX=g_nPrevGlitchY=-1;
		g_SFT.SetAutoSolveGlitches(g_bAutoGlitch);
		UpdateAutoSolveGlitches(hWnd);
		if(g_bAutoGlitch){
			g_SFT.SetReuseReference(!g_bAutoGlitch);
			UpdateReuseReference(hWnd);
		}
		else{
			g_SFT.SetSolveGlitchNear(false);
			CheckMenuItem(GetMenu(hWnd),ID_ACTIONS_SPECIAL_SOLVEGLITCHWITHNEARPIXELSMETHOD,MF_BYCOMMAND|(g_SFT.GetSolveGlitchNear()?MF_CHECKED:MF_UNCHECKED));
		}
	}
	else if(uMsg==WM_COMMAND && wParam==ID_ACTIONS_SPECIAL_AUTOITERATION){
		g_SFT.SetAutoIterations(! g_SFT.GetAutoIterations());
		CheckMenuItem(GetMenu(hWnd),ID_ACTIONS_SPECIAL_AUTOITERATION,MF_BYCOMMAND|(g_SFT.GetAutoIterations()?MF_CHECKED:MF_UNCHECKED));
	}
	else if(uMsg==WM_COMMAND && wParam==ID_ACTIONS_SPECIAL_GUESSING){
		g_SFT.SetGuessing(!g_SFT.GetGuessing());
		CheckMenuItem(GetMenu(hWnd),ID_ACTIONS_SPECIAL_GUESSING,MF_BYCOMMAND|(g_SFT.GetGuessing()?MF_CHECKED:MF_UNCHECKED));
	}
	else if(uMsg==WM_COMMAND && wParam==ID_ACTIONS_SPECIAL_USELONGDOUBLEFROMSTART){
		if(g_nLDBL>100)
			g_nLDBL=0;
		else{
			if(g_SFT.GetPower()==2)
				g_nLDBL=600;
			if(g_SFT.GetPower()==3)
				g_nLDBL=400;
			else
				g_nLDBL=300;
		}
		g_nEXP=4900;
		g_SFT.SetLongDoubleAlways(g_nLDBL < 100);
		g_SFT.SetFloatExpAlways(g_nEXP == 3);
		UpdateLongDoubleAlways(hWnd);
		UpdateFloatExpAlways(hWnd);
	}
	else if(uMsg==WM_COMMAND && wParam==ID_ACTIONS_SPECIAL_USEFLOATEXPALWAYS){
		if(g_nEXP==4900){
			g_nLDBL=2;
			g_nEXP=3;
		}
		else{
			if(g_SFT.GetPower()==2)
				g_nLDBL=600;
			else if(g_SFT.GetPower()==3)
				g_nLDBL=400;
			else
				g_nLDBL=300;
			g_nEXP=4900;
		}
		g_SFT.SetLongDoubleAlways(false);
		g_SFT.SetFloatExpAlways(g_nEXP == 3);
		UpdateLongDoubleAlways(hWnd);
		UpdateFloatExpAlways(hWnd);
	}
	else if(uMsg==WM_KEYDOWN && wParam==VK_LEFT && HIWORD(GetKeyState(VK_CONTROL))){
		RECT r;
		GetClientRect(hWnd,&r);
		RECT sr;
		GetWindowRect(g_hwStatus,&sr);
		r.bottom-=(sr.bottom-sr.top);
		if(g_SFT.GetAnimateZoom()){
			g_pSelect.x = -g_SFT.GetWidth()/2;
			g_pSelect.y = g_SFT.GetHeight()/2;
			ANIM* pAnim = new ANIM;
			pAnim->nZoomSize = g_SFT.GetZoomSize();
			pAnim->bmBmp = ShrinkBitmap2(g_SFT.GetBitmap(),r.right,r.bottom);
			pAnim->hWnd = hWnd;
			pAnim->pOffs.x = -r.right/2;
			pAnim->pOffs.y = r.bottom/2;
			pAnim->bZoomOut = FALSE;
			pAnim->bZoomOne = TRUE;
			DWORD dw;
			HANDLE hThread = CreateThread(NULL,0,(LPTHREAD_START_ROUTINE)ThAnim,(LPVOID)pAnim,0,&dw);
			CloseHandle(hThread);
		}
		g_SFT.Zoom(g_pSelect.x,g_pSelect.y,1,g_SFT.GetWidth(),g_SFT.GetHeight(),FALSE);
		SetTimer(hWnd,0,500,NULL);
	}
	else if(uMsg==WM_KEYDOWN && wParam==VK_RIGHT && HIWORD(GetKeyState(VK_CONTROL))){
		RECT r;
		GetClientRect(hWnd,&r);
		RECT sr;
		GetWindowRect(g_hwStatus,&sr);
		r.bottom-=(sr.bottom-sr.top);
		if(g_SFT.GetAnimateZoom()){
			g_pSelect.x = g_SFT.GetWidth()+g_SFT.GetWidth()/2;
			g_pSelect.y = g_SFT.GetHeight()/2;
			ANIM* pAnim = new ANIM;
			pAnim->nZoomSize = g_SFT.GetZoomSize();
			pAnim->bmBmp = ShrinkBitmap2(g_SFT.GetBitmap(),r.right,r.bottom);
			pAnim->hWnd = hWnd;
			pAnim->pOffs.x = r.right+r.right/2;
			pAnim->pOffs.y = r.bottom/2;
			pAnim->bZoomOut = FALSE;
			pAnim->bZoomOne = TRUE;
			DWORD dw;
			HANDLE hThread = CreateThread(NULL,0,(LPTHREAD_START_ROUTINE)ThAnim,(LPVOID)pAnim,0,&dw);
			CloseHandle(hThread);
		}
		g_SFT.Zoom(g_pSelect.x,g_pSelect.y,1,g_SFT.GetWidth(),g_SFT.GetHeight(),FALSE);
		SetTimer(hWnd,0,500,NULL);
	}
	else if(uMsg==WM_KEYDOWN && wParam==VK_UP && HIWORD(GetKeyState(VK_CONTROL))){
		RECT r;
		GetClientRect(hWnd,&r);
		RECT sr;
		GetWindowRect(g_hwStatus,&sr);
		r.bottom-=(sr.bottom-sr.top);
		if(g_SFT.GetAnimateZoom()){
			g_pSelect.x = g_SFT.GetWidth()/2;
			g_pSelect.y = -g_SFT.GetHeight()/2;
			ANIM* pAnim = new ANIM;
			pAnim->nZoomSize = g_SFT.GetZoomSize();
			pAnim->bmBmp = ShrinkBitmap2(g_SFT.GetBitmap(),r.right,r.bottom);
			pAnim->hWnd = hWnd;
			pAnim->pOffs.x = r.right/2;
			pAnim->pOffs.y = -r.bottom/2;
			pAnim->bZoomOut = FALSE;
			pAnim->bZoomOne = TRUE;
			DWORD dw;
			HANDLE hThread = CreateThread(NULL,0,(LPTHREAD_START_ROUTINE)ThAnim,(LPVOID)pAnim,0,&dw);
			CloseHandle(hThread);
		}
		g_SFT.Zoom(g_pSelect.x,g_pSelect.y,1,g_SFT.GetWidth(),g_SFT.GetHeight(),FALSE);
		SetTimer(hWnd,0,500,NULL);
	}
	else if(uMsg==WM_KEYDOWN && wParam==VK_DOWN && HIWORD(GetKeyState(VK_CONTROL))){
		RECT r;
		GetClientRect(hWnd,&r);
		RECT sr;
		GetWindowRect(g_hwStatus,&sr);
		r.bottom-=(sr.bottom-sr.top);
		if(g_SFT.GetAnimateZoom()){
			g_pSelect.x = g_SFT.GetWidth()/2;
			g_pSelect.y = g_SFT.GetHeight()+g_SFT.GetHeight()/2;
			ANIM* pAnim = new ANIM;
			pAnim->nZoomSize = g_SFT.GetZoomSize();
			pAnim->bmBmp = ShrinkBitmap2(g_SFT.GetBitmap(),r.right,r.bottom);
			pAnim->hWnd = hWnd;
			pAnim->pOffs.x = r.right/2;
			pAnim->pOffs.y = r.bottom+r.bottom/2;
			pAnim->bZoomOut = FALSE;
			pAnim->bZoomOne = TRUE;
			DWORD dw;
			HANDLE hThread = CreateThread(NULL,0,(LPTHREAD_START_ROUTINE)ThAnim,(LPVOID)pAnim,0,&dw);
			CloseHandle(hThread);
		}
		g_SFT.Zoom(g_pSelect.x,g_pSelect.y,1,g_SFT.GetWidth(),g_SFT.GetHeight(),FALSE);
		SetTimer(hWnd,0,500,NULL);
	}
	else if(uMsg==WM_KEYDOWN && wParam=='O' && HIWORD(GetKeyState(VK_CONTROL)))
		PostMessage(hWnd,WM_COMMAND,ID_FILE_OPEN_,0);
	else if(uMsg==WM_KEYDOWN && wParam=='S' && HIWORD(GetKeyState(VK_CONTROL)))
		PostMessage(hWnd,WM_COMMAND,ID_FILE_SAVE_,0);
	else if(uMsg==WM_KEYDOWN && wParam=='N' && HIWORD(GetKeyState(VK_CONTROL)))
		PostMessage(hWnd,WM_COMMAND,ID_ACTIONS_RESET,0);
	else if(uMsg==WM_KEYDOWN && wParam=='N' && HIWORD(GetKeyState(VK_CONTROL)))
		PostMessage(hWnd,WM_COMMAND,ID_FILE_SAVEAS_,0);
	else if(uMsg==WM_KEYDOWN && wParam=='2' && HIWORD(GetKeyState(VK_CONTROL)))
		PostMessage(hWnd,WM_COMMAND,ID_ACTIONS_ZOOMSIZE_2,0);
	else if(uMsg==WM_KEYDOWN && wParam=='4' && HIWORD(GetKeyState(VK_CONTROL)))
		PostMessage(hWnd,WM_COMMAND,ID_ACTIONS_ZOOMSIZE_4,0);
	else if(uMsg==WM_KEYDOWN && wParam=='8' && HIWORD(GetKeyState(VK_CONTROL)))
		PostMessage(hWnd,WM_COMMAND,ID_ACTIONS_ZOOMSIZE_8,0);
	else if(uMsg==WM_KEYDOWN && wParam=='1' && HIWORD(GetKeyState(VK_CONTROL)))
		PostMessage(hWnd,WM_COMMAND,ID_ACTIONS_ZOOMSIZE_16,0);
	else if(uMsg==WM_KEYDOWN && wParam=='3' && HIWORD(GetKeyState(VK_CONTROL)))
		PostMessage(hWnd,WM_COMMAND,ID_ACTIONS_ZOOMSIZE_32,0);
	else if(uMsg==WM_KEYDOWN && wParam=='6' && HIWORD(GetKeyState(VK_CONTROL)))
		PostMessage(hWnd,WM_COMMAND,ID_ACTIONS_ZOOMSIZE_64,0);
	else if(uMsg==WM_KEYDOWN && wParam=='L' && HIWORD(GetKeyState(VK_CONTROL)))
		PostMessage(hWnd,WM_COMMAND,ID_ACTIONS_POSITION,0);
	else if(uMsg==WM_KEYDOWN && wParam=='C' && HIWORD(GetKeyState(VK_CONTROL)))
		PostMessage(hWnd,WM_COMMAND,ID_ACTIONS_SETCOLORS,0);
	else if(uMsg==WM_KEYDOWN && wParam=='D' && HIWORD(GetKeyState(VK_CONTROL)))
		PostMessage(hWnd,WM_COMMAND,ID_SPECIAL_NEWTON,0);
	else if(uMsg==WM_KEYDOWN && wParam=='I' && HIWORD(GetKeyState(VK_CONTROL)))
		PostMessage(hWnd,WM_COMMAND,ID_ACTIONS_ITERATIONS,0);
	else if(uMsg==WM_KEYDOWN && wParam=='E' && HIWORD(GetKeyState(VK_CONTROL)))
		PostMessage(hWnd,WM_COMMAND,ID_ACTIONS_REUSEREFERENCE,0);
	else if(uMsg==WM_KEYDOWN && wParam=='Q' && HIWORD(GetKeyState(VK_CONTROL)))
		PostMessage(hWnd,WM_COMMAND,ID_FILE_STOREZOOMOUTIMAGES,0);
	else if(uMsg==WM_KEYDOWN && wParam=='M' && HIWORD(GetKeyState(VK_CONTROL)))
		PostMessage(hWnd,WM_COMMAND,ID_ACTIONS_FINDMINIBROT,0);
	else if(uMsg==WM_KEYDOWN && wParam=='K' && HIWORD(GetKeyState(VK_CONTROL)))
		PostMessage(hWnd,WM_COMMAND,ID_ACTIONS_SKEW,0);
	else if(uMsg==WM_KEYDOWN && wParam=='H' && HIWORD(GetKeyState(VK_CONTROL)))
		PostMessage(hWnd,WM_COMMAND,ID_ACTIONS_SHOWINFLECTION,0);
	else if(uMsg==WM_KEYDOWN && wParam=='H' && HIWORD(GetKeyState(VK_SHIFT))){
		POINT p;
		GetCursorPos(&p);
		ScreenToClient(hWnd,&p);
		g_SFT.AddInflectionPont(p.x,p.y);
		PostMessage(hWnd,WM_KEYDOWN,VK_F5,0);
	}
	else if(uMsg==WM_KEYDOWN && wParam=='G' && HIWORD(GetKeyState(VK_SHIFT))){
		g_SFT.RemoveInflectionPoint();
		PostMessage(hWnd,WM_KEYDOWN,VK_F5,0);
	}
	else if(uMsg==WM_KEYDOWN && wParam=='X' && HIWORD(GetKeyState(VK_CONTROL))){
		if(size_t(g_nInflection)<sizeof(g_pInflections)/sizeof(POINT)){
			GetCursorPos(&g_pInflections[g_nInflection]);
			ScreenToClient(hWnd,&g_pInflections[g_nInflection]);
			g_nInflection++;
		}
	}
	else if((uMsg==WM_KEYDOWN && wParam=='B' && HIWORD(GetKeyState(VK_CONTROL))) || (uMsg==WM_COMMAND && wParam==ID_SPECIAL_SKEWANIMATION)){
		g_bSkewAnimation=!g_bSkewAnimation;
		if(g_bSkewAnimation)
			g_bSkewAnimation=DialogBoxParam(GetModuleHandle(NULL),MAKEINTRESOURCE(IDD_DIALOG11),hWnd,(DLGPROC)SkewAnimateProc,(LPARAM)0);
		CheckMenuItem(GetMenu(hWnd),ID_SPECIAL_SKEWANIMATION,MF_BYCOMMAND|(g_bSkewAnimation?MF_CHECKED:MF_UNCHECKED));
		return 0;
	}
	else if(uMsg==WM_KEYDOWN && wParam=='G' && HIWORD(GetKeyState(VK_CONTROL))){
		g_bMarilyn=!g_bMarilyn;
	}
	else if(uMsg==WM_KEYDOWN && wParam=='J' && HIWORD(GetKeyState(VK_CONTROL)))
		PostMessage(hWnd,WM_COMMAND,ID_FILE_SAVEASJPEG,0);
	else if(uMsg==WM_KEYDOWN && wParam=='P' && HIWORD(GetKeyState(VK_CONTROL)))
		PostMessage(hWnd,WM_COMMAND,ID_FILE_SAVEASPNG,0);
	else if(uMsg==WM_KEYDOWN && wParam=='R' && HIWORD(GetKeyState(VK_CONTROL)))
		PostMessage(hWnd,WM_COMMAND,ID_ACTIONS_ADDREFERENCE,0);
	else if(uMsg==WM_KEYDOWN && wParam=='T' && HIWORD(GetKeyState(VK_CONTROL)))
		PostMessage(hWnd,WM_COMMAND,ID_ACTIONS_ROTATE,0);
	else if(uMsg==WM_KEYDOWN && wParam=='F' && HIWORD(GetKeyState(VK_CONTROL)))
		PostMessage(hWnd,WM_COMMAND,ID_ACTIONS_FINDCENTEROFGLITCH,0);
	else if(uMsg==WM_KEYDOWN && wParam=='A' && HIWORD(GetKeyState(VK_CONTROL)))
		PostMessage(hWnd,WM_COMMAND,ID_FILE_SAVEAS_,0);
	else if(uMsg==WM_RBUTTONUP && !g_bWaitRead){
		if(g_bTrackSelect==2)
			g_bTrackSelect=0;
		else{
			g_SFT.Stop();
			g_bAnim=false;
			SYSTEMTIME st;
			GetLocalTime(&st);
			SystemTimeToFileTime(&st,(LPFILETIME)&g_nTStart);
			g_bFindMinibrot=FALSE;
			g_bStoreZoom=FALSE;
			DeleteObject(g_bmSaveZoomBuff);
			g_bmSaveZoomBuff=NULL;
			PostMessage(hWnd,WM_USER+299,wParam,lParam);
		}
	}
	else if(uMsg==WM_USER+299){
		RECT rc;
		GetClientRect(hWnd,&rc);
		RECT sr;
		GetWindowRect(g_hwStatus,&sr);
		sr.bottom-=sr.top;
		rc.bottom-=sr.bottom;
		g_pSelect.x = (short)LOWORD(lParam)*g_SFT.GetWidth()/rc.right;
		g_pSelect.y = (short)HIWORD(lParam)*g_SFT.GetHeight()/rc.bottom;
		if(g_SFT.GetAnimateZoom()){
			ANIM* pAnim = new ANIM;
			pAnim->nZoomSize = g_SFT.GetZoomSize();
			pAnim->bmBmp = ShrinkBitmap2(g_SFT.GetBitmap(),rc.right,rc.bottom);
			pAnim->hWnd = hWnd;
			pAnim->pOffs.x = (short)LOWORD(lParam);
			pAnim->pOffs.y = (short)HIWORD(lParam);
			pAnim->bZoomOut = TRUE;
			pAnim->bZoomOne = FALSE;
			DWORD dw;
			HANDLE hThread = CreateThread(NULL,0,(LPTHREAD_START_ROUTINE)ThAnim,(LPVOID)pAnim,0,&dw);
			CloseHandle(hThread);

			RECT r = {0,0,g_SFT.GetWidth(),g_SFT.GetHeight()};
			double zoomDiff = (double)1/(double)g_SFT.GetZoomSize();
			int nToXStart = g_pSelect.x-r.right/(zoomDiff*2);
			int nToYStart = g_pSelect.y-r.bottom/(zoomDiff*2);
			int nToXStop = r.right - (g_pSelect.x+r.right/(zoomDiff*2));
			int nToYStop = r.bottom - (g_pSelect.y+r.bottom/(zoomDiff*2));
			HDC hDC = GetDC(NULL);
			HDC dcBmp = CreateCompatibleDC(hDC);
			HBITMAP bmBmp = g_SFT.GetBitmap();
			HBITMAP bmBmp2 = ShrinkBitmap2(bmBmp,r.right,r.bottom);
			HBITMAP bmOld = (HBITMAP)SelectObject(dcBmp,bmBmp);
			SetStretchBltMode(dcBmp,HALFTONE);
			HDC dcBmp2 = CreateCompatibleDC(hDC);
			HBITMAP bmOld2 = (HBITMAP)SelectObject(dcBmp2,bmBmp2);
			FillRect(dcBmp,&r,(HBRUSH)GetStockObject(BLACK_BRUSH));
			StretchBlt(dcBmp,0,0,r.right,r.bottom,dcBmp2,nToXStart,nToYStart,r.right-nToXStop-nToXStart,r.bottom-nToYStop-nToYStart,SRCCOPY);
			SelectObject(dcBmp,bmOld);
			DeleteDC(dcBmp);
			SelectObject(dcBmp2,bmOld2);
			DeleteDC(dcBmp2);
			DeleteObject(bmBmp2);
			ReleaseDC(NULL,hDC);
			g_SFT.UpdateBitmap();
		}
		bool bReuseCenter = (g_SFT.GetZoomSize() == round(g_SFT.GetZoomSize()));
		if(!g_bAutoGlitch && g_SFT.GetReuseReference() && g_pSelect.x==g_SFT.GetWidth()/2 && g_pSelect.y==g_SFT.GetHeight()/2)
			g_SFT.Zoom(g_pSelect.x,g_pSelect.y,1/(double)g_SFT.GetZoomSize(),g_SFT.GetWidth(),g_SFT.GetHeight(),bReuseCenter);
		else
			g_SFT.Zoom(g_pSelect.x,g_pSelect.y,1/(double)g_SFT.GetZoomSize(),g_SFT.GetWidth(),g_SFT.GetHeight());
		SetTimer(hWnd,0,500,NULL);
	}
	else if(!g_SFT.GetArbitrarySize() && uMsg==WM_SIZING){
		RECT sr, cr;
		LPRECT pwr = (LPRECT)lParam;
		pwr->right-=pwr->left;
		pwr->bottom-=pwr->top;
		GetClientRect(hWnd,&cr);
		GetWindowRect(g_hwStatus,&sr);
		sr.bottom-=sr.top;
		cr.bottom-=sr.bottom;
		int nXOffs = 16*cr.bottom/9 - cr.right;
		pwr->right+=nXOffs;

/*		MoveWindow(hWnd,pwr->left,pwr->top,pwr->right,pwr->bottom,TRUE);
		SendMessage(g_hwStatus,uMsg,wParam,lParam);
		InvalidateRect(hWnd,NULL,FALSE);
*/
		pwr->right+=pwr->left;
		pwr->bottom+=pwr->top;
		return TRUE;
	}
	else if(uMsg==WM_SIZE){
		if(!g_SFT.GetArbitrarySize()){
			RECT wr, cr;
			GetWindowRect(hWnd,&wr);
			wr.right-=wr.left;
			wr.bottom-=wr.top;
			GetClientRect(hWnd,&cr);
			RECT sr;
			GetWindowRect(g_hwStatus,&sr);
			sr.bottom-=sr.top;
			cr.bottom-=sr.bottom;
			int nXOffs = 16*cr.bottom/9 - cr.right;
			wr.right+=nXOffs;
			MoveWindow(hWnd,wr.left,wr.top,wr.right,wr.bottom,TRUE);
		}
		InvalidateRect(hWnd,NULL,TRUE);
		SendMessage(g_hwStatus,uMsg,wParam,lParam);
		if(g_SFT.GetArbitrarySize() && !g_bResizing && (wParam==SIZE_MAXIMIZED || wParam==SIZE_RESTORED))
			SendMessage(hWnd,WM_EXITSIZEMOVE,0,0);
	}
	else if(uMsg==WM_ENTERSIZEMOVE && g_SFT.GetArbitrarySize()){
		g_bResizing=TRUE;
		RECT cr;
		GetClientRect(hWnd,&cr);
		g_scSize.cx = cr.right;
		g_scSize.cy = cr.bottom;
	}
	else if(uMsg==WM_EXITSIZEMOVE && g_SFT.GetArbitrarySize() && !g_bFirstDone){
		g_bResizing=FALSE;
		RECT cr;
		GetClientRect(hWnd,&cr);
		if(cr.right!=g_scSize.cx || cr.bottom!=g_scSize.cy){
			RECT sr;
			GetWindowRect(g_hwStatus,&sr);
			sr.bottom-=sr.top;
			cr.bottom-=sr.bottom;
			g_scSize.cy-=sr.bottom;
			double xRatio = 640.0/cr.right;
			cr.right = 640;
			cr.bottom = cr.bottom*xRatio;
			xRatio = 640.0/g_scSize.cx;
			g_scSize.cx = 640;
			g_scSize.cy = g_scSize.cy*xRatio;
			xRatio = (double)g_SFT.GetRatioY()/(double)g_scSize.cy;
			g_SFT.SetRatio(cr.right,cr.bottom*xRatio);
			PostMessage(hWnd,WM_KEYDOWN,VK_F5,0);
		}
		GetClientRect(hWnd,&cr);
		g_scSize.cx = cr.right;
		g_scSize.cy = cr.bottom;
	}
	else if((uMsg==WM_USER+199 || uMsg==WM_TIMER) && !g_bMove){
		return HandleDoneSEH(hWnd,uMsg,wParam,lParam);
	}
	else if(uMsg==WM_COMMAND){
		if(wParam==ID_ACTIONS_POSITION){
//			MessageBox(NULL,g_SFT.GetPosition(),g_SFT.ToZoom(),MB_OK);
			if(DialogBox(GetModuleHandle(NULL),MAKEINTRESOURCE(IDD_DIALOG5),hWnd,(DLGPROC)PositionProc))
				PostMessage(hWnd,WM_KEYDOWN,VK_F5,0);
		}
		else if(wParam==ID_ACTIONS_EXIT)
			PostQuitMessage(0);
		else if(wParam==ID_ACTIONS_REFRESH)
			PostMessage(hWnd,WM_KEYDOWN,VK_F5,0);
		else if(wParam==ID_ACTIONS_CANCELRENDERING)
			PostMessage(hWnd,WM_KEYDOWN,VK_ESCAPE,0);
		else if(wParam==ID_ACTIONS_ITERATIONS){
			int n = DialogBoxParam(GetModuleHandle(NULL),MAKEINTRESOURCE(IDD_DIALOG2),hWnd,(DLGPROC)IterationProc,0);
			if(n){
				SetTimer(hWnd,0,500,NULL);
				RECT r;
				GetClientRect(hWnd,&r);
				RECT sr;
				GetWindowRect(g_hwStatus,&sr);
				sr.bottom-=sr.top;
				r.bottom-=sr.bottom;
				g_SFT.Stop();
				g_bAnim=false;
				g_SFT.SetIterations(n);
				PostMessage(hWnd,WM_KEYDOWN,VK_F5,0);
			}
		}
		else if(wParam==ID_ACTIONS_SETCOLORS){
			if(!g_hwColors)
				g_hwColors = CreateDialog(GetModuleHandle(NULL),MAKEINTRESOURCE(IDD_DIALOG3),hWnd,(DLGPROC)ColorProc);
			ShowWindow(g_hwColors,SW_SHOW);
		}
		else if(wParam==ID_ACTIONS_ZOOMSIZE_1 || wParam==ID_ACTIONS_ZOOMSIZE_2 ||
			wParam==ID_ACTIONS_ZOOMSIZE_4 ||
			wParam==ID_ACTIONS_ZOOMSIZE_8 ||
			wParam==ID_ACTIONS_ZOOMSIZE_16 ||
			wParam==ID_ACTIONS_ZOOMSIZE_32 ||
			wParam==ID_ACTIONS_ZOOMSIZE_64 ||
			wParam==ID_ACTIONS_ZOOMSIZE_128 ||
			wParam==ID_ZOOMSIZE_CUSTOM){
			if(wParam==ID_ACTIONS_ZOOMSIZE_1)
				g_SFT.SetZoomSize(1);
			if(wParam==ID_ACTIONS_ZOOMSIZE_2)
				g_SFT.SetZoomSize(2);
			else if(wParam==ID_ACTIONS_ZOOMSIZE_4)
				g_SFT.SetZoomSize(4);
			else if(wParam==ID_ACTIONS_ZOOMSIZE_8)
				g_SFT.SetZoomSize(8);
			else if(wParam==ID_ACTIONS_ZOOMSIZE_16)
				g_SFT.SetZoomSize(16);
			else if(wParam==ID_ACTIONS_ZOOMSIZE_32)
				g_SFT.SetZoomSize(32);
			else if(wParam==ID_ACTIONS_ZOOMSIZE_64)
				g_SFT.SetZoomSize(64);
			else if(wParam==ID_ACTIONS_ZOOMSIZE_128)
				g_SFT.SetZoomSize(128);
			else if(wParam==ID_ZOOMSIZE_CUSTOM){
				char szTmp[25];
				if(DialogBoxParam(GetModuleHandle(NULL),MAKEINTRESOURCE(IDD_DIALOG9),hWnd,(DLGPROC)CustomZoomSize,(LPARAM)szTmp)){
					g_SFT.SetZoomSize(atof(szTmp));
					if(!g_SFT.GetZoomSize())
						g_SFT.SetZoomSize(2);
				}
			}
			UpdateZoomSize(hWnd);
		}
		else if(wParam==ID_ACTIONS_REUSEREFERENCE){
			g_SFT.SetReuseReference(! g_SFT.GetReuseReference());
			CheckMenuItem(GetMenu(hWnd),ID_ACTIONS_REUSEREFERENCE,MF_BYCOMMAND|(g_SFT.GetReuseReference()?MF_CHECKED:MF_UNCHECKED));
			if(g_SFT.GetReuseReference()){
				g_SFT.SetAutoSolveGlitches(!g_SFT.GetReuseReference());
				CheckMenuItem(GetMenu(hWnd),ID_ACTIONS_AUTOSOLVEGLITCHES,MF_BYCOMMAND|(g_SFT.GetAutoSolveGlitches()?MF_CHECKED:MF_UNCHECKED));
			}
		}
		else if(wParam==ID_FILE_OPENSETTINGS){
			if(BrowseFile(hWnd,TRUE,"Open Settings","Kalle's fraktaler\0*.kfs\0Image files\0*.png;*.jpg;*.jpeg\0\0",g_szFile)){
				bool ret;
				long r = OpenSettings(hWnd, ret);
				if (ret) return r;
			}
		}
		else if(wParam==ID_FILE_SAVESETTINGS){
			if(BrowseFile(hWnd,FALSE,"Save Settings","Kalle's fraktaler\0*.kfs\0\0",g_szFile)){
				if(!g_SFT.SaveSettings(g_szFile))
					return MessageBox(hWnd,"Could not save settings","Error",MB_OK|MB_ICONSTOP);
			}
		}
		else if(wParam==ID_FILE_OPEN_){
			if(BrowseFile(hWnd,TRUE,"Open Location Parameters","Kalle's fraktaler\0*.kfr\0Image files\0*.png;*.jpg;*.jpeg\0\0",g_szFile)){
				bool ret;
				long r = OpenFile(hWnd, ret);
				if (ret) return r;
			}
		}
		else if(wParam==ID_FILE_SAVE_){
			if(g_szFile == "")
				PostMessage(hWnd,WM_COMMAND,ID_FILE_SAVEAS_,0);
			else if(!g_SFT.SaveFile(g_szFile))
				return MessageBox(hWnd,"Could not save parameters","Error",MB_OK|MB_ICONSTOP);
		}
		else if(wParam==ID_FILE_SAVEASJPEG){
			g_JpegParams.nWidth = g_SFT.GetWidth();
			g_JpegParams.nHeight = g_SFT.GetHeight();
			g_JpegParams.nQuality = 100;
			if(DialogBoxParam(GetModuleHandle(NULL),MAKEINTRESOURCE(IDD_DIALOG7),hWnd,(DLGPROC)JpegProc,0)){
				std::string szFile;
				if(g_JpegParams.nWidth>g_SFT.GetWidth()){
					g_bSaveJpeg=TRUE;
					SetTimer(hWnd,0,500,NULL);
					g_SFT.RenderFractal(g_JpegParams.nWidth,g_JpegParams.nHeight,g_SFT.GetIterations(),hWnd);
					return 0;
				}
				if(BrowseFile(hWnd,FALSE,"Save as Jpeg","Jpeg\0*.jpg\0\0",szFile)){
					if(!g_SFT.SaveJpg(szFile,g_JpegParams.nQuality,g_JpegParams.nWidth,g_JpegParams.nHeight))
						MessageBox(hWnd,"File could not be saved","Error",MB_OK|MB_ICONSTOP);
					szFile = replace_path_extension(szFile, "kfb");
					if(FileExists(szFile) && MessageBox(hWnd,"Found a map file (.kfb) with the same name, do you want to replace it?","Kalle's Fraktaler",MB_YESNO)==IDYES)
						g_SFT.SaveMapB(szFile);
				}
			}
		}
		else if(wParam==ID_FILE_SAVEASPNG){
			g_JpegParams.nWidth = g_SFT.GetWidth();
			g_JpegParams.nHeight = g_SFT.GetHeight();
			g_JpegParams.nQuality = 100;
			if(DialogBoxParam(GetModuleHandle(NULL),MAKEINTRESOURCE(IDD_DIALOG7),hWnd,(DLGPROC)JpegProc,1)){
				std::string szFile;
				if(g_JpegParams.nWidth>g_SFT.GetWidth()){
					g_bSavePng=TRUE;
					SetTimer(hWnd,0,500,NULL);
					g_SFT.RenderFractal(g_JpegParams.nWidth,g_JpegParams.nHeight,g_SFT.GetIterations(),hWnd);
					return 0;
				}
				if(BrowseFile(hWnd,FALSE,"Save as PNG","PNG\0*.png\0\0",szFile)){
					if(!g_SFT.SaveJpg(szFile,-1,g_JpegParams.nWidth,g_JpegParams.nHeight))
						MessageBox(hWnd,"File could not be saved","Error",MB_OK|MB_ICONSTOP);
					szFile = replace_path_extension(szFile, "kfb");
					if(FileExists(szFile) && MessageBox(hWnd,"Found a map file (.kfb) with the same name, do you want to replace it?","Kalle's Fraktaler",MB_YESNO)==IDYES)
						g_SFT.SaveMapB(szFile);
				}
			}
		}
		else if(wParam==ID_FILE_SAVEAS_){
			if(BrowseFile(hWnd,FALSE,"Save Location Parameters","Kalle's fraktaler\0*.kfr\0\0",g_szFile)){
				if(!g_SFT.SaveFile(g_szFile))
					return MessageBox(hWnd,"Could not save parameters","Error",MB_OK|MB_ICONSTOP);
				char szTitle[369];
				wsprintf(szTitle,"Kalle's Fraktaler 2 - %s",g_szFile);
				SetWindowText(hWnd,szTitle);
			}
		}
/*		else if(wParam==ID_ACTIONS_CREATEZOOMSEQUENCE){
			DialogBoxParam(GetModuleHandle(NULL),MAKEINTRESOURCE(IDD_DIALOG6),hWnd,(DLGPROC)ZoomProc,0);
		}
*/		else if(wParam==ID_ACTIONS_RESET){
			g_szFile="";
			SetWindowText(hWnd,"Kalle's Fraktaler 2");
			g_SFT.SetPosition("0","0","1");
			PostMessage(hWnd,WM_KEYDOWN,VK_F5,0);
		}
		else if(wParam==ID_SPECIAL_NON){
			if(!g_bFindMinibrotCount){
#if 0
				char *e = g_SFT.GetZoom();
				e = stristr(e,"e");
				if(e)
					e++;
				else e = "1";
#endif
				g_bFindMinibrotPos=1;// + 2*log(atof(e))/log((double)2)/3;
				g_bFindMinibrotCount=1;
			}
			else
				g_bFindMinibrotCount=0;
			CheckMenuItem(GetMenu(hWnd),ID_SPECIAL_NON,MF_BYCOMMAND|(g_bFindMinibrotCount?MF_CHECKED:MF_UNCHECKED));
		}
		else if(wParam==ID_ACTIONS_FINDMINIBROT){
			if(!lParam && HIWORD(GetKeyState(VK_CONTROL)))
				g_nStopAtExponent = DialogBox(GetModuleHandle(NULL),MAKEINTRESOURCE(IDD_DIALOG9),hWnd,(DLGPROC)StopAtProc);
			else if(!lParam)
				g_nStopAtExponent=0;
			if(g_nStopAtExponent && g_nStopAtExponent<=g_SFT.GetExponent())
				return MessageBox(hWnd,"Done","Error",MB_OK|MB_ICONINFORMATION);
			g_SFT.Stop();
			g_bAnim=false;
			g_bStoreZoom=FALSE;
			DeleteObject(g_bmSaveZoomBuff);
			g_bmSaveZoomBuff=NULL;
			int x, y;
			if(g_bFindMinibrotCount && g_bFindMinibrotCount==g_bFindMinibrotPos){
				int nMin,nMax;
				g_SFT.GetIterations(nMin,nMax);
				if(nMax<nMin+3){
					SYSTEMTIME st;
					GetLocalTime(&st);
					SystemTimeToFileTime(&st,(LPFILETIME)&g_nTStart);
					g_SFT.Zoom(0,0,1.0/g_SFT.GetZoomSize(),g_SFT.GetWidth(),g_SFT.GetHeight());
					SetTimer(hWnd,0,500,NULL);
					return 0;
				}
				int nIter;
				g_SFT.GetIterations(nMin,nMax,NULL,NULL,TRUE);
				int diff = (nMax-nMin)/6;
				int nTries=0;
				do{
					x = rand()%g_SFT.GetWidth();
					y = rand()%g_SFT.GetHeight();
					nIter = g_SFT.GetIterationOnPoint(x,y);
					if(nTries++>20 && diff>10){
						nTries=0;
						diff--;
					}
				}while(nIter<nMin + diff || nIter==g_SFT.GetIterations());
			}
			else if(!g_SFT.Center(x,y,g_bFindMinibrotCount && g_bFindMinibrotCount==g_bFindMinibrotPos)){
				g_bFindMinibrot=FALSE;
				return MessageBox(hWnd,"Cannot find center","Error",MB_OK|MB_ICONSTOP);
			}
			if(g_bFindMinibrotCount){
				int nMin,nMax;
				g_SFT.GetIterations(nMin,nMax);
				int nIter=g_SFT.GetIterationOnPoint(x,y);
				if(nIter<nMin+8)
					g_SFT.HighestIteration(x,y);
			}
			if(g_bFindMinibrotCount){
				if(g_bFindMinibrotPos==g_bFindMinibrotCount){
					g_bFindMinibrotCount=0;
					g_bFindMinibrotPos=1;//+=(g_bFindMinibrotPos==1?1:g_bFindMinibrotPos/2);
				}
				g_bFindMinibrotCount++;
				char szTitle[256];
				wsprintf(szTitle,"Kalle's Fraktaler 2 - %d",g_bFindMinibrotPos-g_bFindMinibrotCount);
				SetWindowText(hWnd,szTitle);
			}
			else
				SetWindowText(hWnd,"Kalle's Fraktaler 2");
			int nMin, nMax, nIter;
			g_SFT.GetIterations(nMin,nMax);
			nIter = g_SFT.GetIterations();
			if(nIter<nMin+nMin/2+2000)
			{
				// ignore auto iterations setting, otherwise it could stop early
				g_SFT.SetIterations(nMin+nMin/2+3000);
			}
			if(nIter==nMax && !g_bFindMinibrotCount){
				g_bFindMinibrot=FALSE;
				MessageBox(hWnd,"Done","Kalle's Fraktaler",MB_OK);
			}
			RECT rc;
			GetClientRect(hWnd,&rc);
			RECT sr;
			GetWindowRect(g_hwStatus,&sr);
			sr.bottom-=sr.top;
			rc.bottom-=sr.bottom;
			x = x*rc.right/g_SFT.GetWidth();
			y = y*rc.bottom/g_SFT.GetHeight();
			while(g_bAnim){
				Sleep(10);
			}
			SendMessage(hWnd,WM_LBUTTONDOWN,0,MAKELONG(x,y));
			PostMessage(hWnd,WM_LBUTTONUP,0,MAKELONG(x,y));
			g_bFindMinibrot=TRUE;
		}
		else if(wParam==ID_MENUITEM40025){
			char szMsg[1024]; // this is the size limit for wsprintf, longer is truncated
			SYSTEM_INFO sysinfo;
			GetSystemInfo( &sysinfo );  //�
			wsprintf(szMsg,
				"version %s\n"
				"�2013-2017 Karl Runmo\n"
				"�2017 Claude Heiland-Allen\n\n"
				"Processors: %d\n"
				"Compiled for %s\n"
				"Precision: %d bits (%d decimal digits)\n"
				"\nLibraries:\n"
				"- JPEG 6b <http://ijg.org>\n"
				"- PNG %s <http://libpng.org>\n"
				"- ZLIB %s <http://zlib.net>\n"
				"- GMP %d.%d.%d <http://gmplib.org>\n"
				"- MPFR %s <http://mpfr.org>\n"
				"- Boost %d.%d.%d <http://boost.org>\n"
#ifdef KF_OPENCL
				"- CLEW git.50751dd <https://github.com/martijnberger/clew>\n"
#endif
				"\nThanks to:\n"
				" - K.I.Martin for applying Perturbation and Series Approximation on the Mandelbrot set and sharing theory and source code!\n"
				" - Pauldelbrot for reliable glitch detection method\n"
				" - Botond K�sa and knighty for extensions of Series Approximation\n"
				" - laser blaster for Burning ship formula\n"
				" - stardust4ever for other fractal types\n"
				" - claude for Newton-Raphson method\n"
				" - gerrit for differencing variations\n"
				" - Dinkydau, Fractal universe, CFJH, Foxxie and others for bug reports\n"
				" - Chillheimer for hosting my program\n\n"
				"http://www.chillheimer.de/kallesfraktaler/\n\n"
				"Claude also thanks Karl for releasing the source code.\n\n"
				"https://mathr.co.uk/kf/kf.html",
				version.c_str(),
				sysinfo.dwNumberOfProcessors,
				sizeof(void*)==4?"32-bit":"64-bit",
				int(MPFR_PREC_MAX),
				int(MPFR_PREC_MAX * log10(2.0)),
				png_libpng_ver,
				zlib_version,
				__GNU_MP_VERSION, __GNU_MP_VERSION_MINOR, __GNU_MP_VERSION_PATCHLEVEL,
				MPFR_VERSION_STRING,
				BOOST_VERSION / 100000, BOOST_VERSION / 100 % 1000, BOOST_VERSION % 100
				);
			szMsg[1024-1] = 0;
			return MessageBox(hWnd,szMsg,"Kalle's Fraktaler 2",MB_OK);
		}
	}
	return DefWindowProc(hWnd,uMsg,wParam,lParam);
}

#if 0
static int Test()
{
	CFileFloat <double>tmp(3000);
	int i;
	for(i=0;i<3000;i++)
		tmp[i] = i;
	for(i=0;i<3000;i++)
		if(tmp[i]!=i)
			break;
	if(i<3000)
		return MessageBox(NULL,"Error","Error",MB_OK|MB_ICONSTOP);
	else
		return MessageBox(NULL,"Success","Success",MB_OK);
	return 0;
}

static int Test2()
{
	return 0;
}

static int Test1()
{
	CFixedFloat xr = 0, xi = 0, xin, xrn, sr = 0, si = 0, xrxid = 0;
	CFixedFloat m_rref = 0.25, m_iref=0;
	int antal;
	SYSTEMTIME st;
	__int64 t1, t2;
	MessageBox(NULL,"Start","Debug",MB_OK);
	GetLocalTime(&st);
	SystemTimeToFileTime(&st,(LPFILETIME)&t1);
	for(antal=0;antal<10000;antal++){
		xrn = sr - si + m_rref;
		xrxid = 2 * xr*xi;
//		xrxid = CFixedFloat_Multiply((CFixedFloat)2,xr);
//		xrxid = CFixedFloat_Multiply(xrxid,xi);
		xin = xrxid.Abs() + m_iref;
		xr = xrn;
		xi = xin;
		sr = xr.Square();
		si = xi.Square();

	}
	GetLocalTime(&st);
	SystemTimeToFileTime(&st,(LPFILETIME)&t2);
	t2-=t1;
	FileTimeToSystemTime((LPFILETIME)&t2,&st);
	__int64 td = 10000000;
	char szRes[256];
	wsprintf(szRes,"%02d:%02d:%02d.%07d",t2/(td*3600),(t2/(td*60))%60,(t2/td)%60,t2%td);
//	wsprintf(szRes+strlen(szRes),"\n%02d:%02d:%02d.%03d",st.wHour,st.wMinute,st.wSecond,st.wMilliseconds);
	return MessageBox(NULL,szRes,"Res",MB_OK);
}
#endif

extern int WINAPI WinMain(HINSTANCE hInstance,HINSTANCE,LPSTR commandline,int)
{
//	return Test();

	CommandLineArguments args(commandline);
	if (args.bError)
	{
		std::cerr << "ERROR: bad command line arguments" << std::endl;
	}
	if (args.bVersion)
	{
		std::cout << version << std::endl;
	}
	if (args.bHelp)
	{
		std::cout << usage;
	}
	if (args.bVersion || args.bHelp || args.bError)
	{
		return 0;
	}
	g_args = &args;

#ifdef KF_OPENCL
	cldevices = initialize_opencl();
#endif

	GetModuleFileName(GetModuleHandle(NULL),g_szRecovery,sizeof(g_szRecovery));
	strcpy(strrchr(g_szRecovery,'.'),".rec");

	WNDCLASS wc={0};
	wc.hInstance = hInstance;
	wc.lpszClassName = "FRAKTAL_SFT";
	wc.lpfnWndProc = (WNDPROC)MainProc;
	wc.hCursor = LoadCursor(NULL,IDC_CROSS);
	wc.hIcon = LoadIcon(hInstance,MAKEINTRESOURCE(IDI_ICON1));
	g_hIcon = wc.hIcon;
	RegisterClass(&wc);
	HWND hWnd = CreateWindowEx(WS_EX_CLIENTEDGE,wc.lpszClassName,"Kalle's Fraktaler 2",WS_OVERLAPPEDWINDOW|WS_VISIBLE,0,0,200,200,NULL,LoadMenu(hInstance,MAKEINTRESOURCE(IDR_MENU1)),hInstance,0);
	g_SFT.SetWindow(hWnd);
	ShowWindow(hWnd,SW_SHOW);

	MSG msg;
	while(GetMessage(&msg,NULL,0,0)){
		if(GetDlgCtrlID(msg.hwnd)==IDC_LIST1 && msg.message==WM_RBUTTONDOWN)
			SendMessage(GetParent(msg.hwnd),WM_USER+88,GetDlgCtrlID(msg.hwnd),0);
		if(g_hwColors && IsDialogMessage(g_hwColors,&msg))
			continue;
		if(g_hwExamine && IsDialogMessage(g_hwExamine,&msg))
			continue;
		if(g_hwNewton && IsDialogMessage(g_hwNewton,&msg))
			continue;
		TranslateMessage(&msg);
		DispatchMessage(&msg);
	}
	DeleteFile(g_szRecovery);
	return 0;
}
