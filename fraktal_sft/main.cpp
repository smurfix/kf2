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

BOOL g_nAnimateZoom=TRUE;
BOOL g_bNewton=FALSE;
HWND g_hwNewton=NULL;
BOOL g_bResizing=FALSE;
BOOL g_bTrackSelect=FALSE;
POINT g_pTrackStart;
HICON g_hIcon;

void HSVToRGB(double hue, double sat, double bri, COLOR14 &cPos);

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

double CHECK_FLOAT(double a);
BOOL ISFLOATOK(double a);
CFraktalSFT g_SFT;
BOOL g_bAddReference=FALSE;
BOOL g_bEraser=FALSE;
BOOL g_bAddMainReference=FALSE;
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
BOOL g_bStoreZoomJpg=FALSE;
BOOL g_bStoreZoomPng=FALSE;
BOOL g_bWaitRead=FALSE;
int MakePrime(int n);
int g_nStopAtExponent=0;

char *g_pszStatus[] = {
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
char g_szRecovery[256];

BOOL g_bResetReference=FALSE;
BOOL g_bFindMinibrot=FALSE;
BOOL g_bFindMinibrotCount=0;
int g_bFindMinibrotPos=0;

BOOL g_bZoomRunning=FALSE;
BOOL g_bZoomStop=FALSE;
HWND g_hwExamine=NULL;
char g_szExamine[256];
CStringTable g_stExamine;
int g_nExamine=-1;
int g_nExamineZoom=-1;
BOOL g_bExamineDirty=FALSE;
void bmp2rgb(BYTE *rgb, const BYTE *bmp, int height, int width, int stride, int bytes)
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
int SaveImage(char *szFileName,HBITMAP bmBmp,int nQuality, const char *comment)
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
char g_szFile[256]={0};

double GetDlgItemFloat(HWND hWnd,int nID)
{
	char szText[256];
	GetDlgItemText(hWnd,nID,szText,sizeof(szText));
	return atof(szText);
}
void SetDlgItemFloat(HWND hWnd,int nID,double val)
{
	char szText[256];
	sprintf(szText,"%.4f",val);
	SetDlgItemText(hWnd,nID,szText);
}
int kGetDlgItemInt(HWND hWnd,int nID)
{
	char szText[256];
	GetDlgItemText(hWnd,nID,szText,sizeof(szText));
	return atoi(szText);
}
void kSetDlgItemInt(HWND hWnd,int nID,int val)
{
	char szText[256];
	sprintf(szText,"%d",val);
	SetDlgItemText(hWnd,nID,szText);
}
int g_nPrevCalc=-1;

char * GetToolText(int nID,LPARAM lParam)
{
	if(lParam==0){
		switch(nID){
		case IDC_EDIT1:
		case IDC_SPIN1:
			return "Number of Key Colors\nThese colors will be spread out on the 1024 color palette\nInteger value";
		case IDC_EDIT3:
		case IDC_SPIN2:
			return "Division of iteration mapping\nThis value can be a float value but not negative";
		case IDC_EDIT12:
		case IDC_SPIN5:
			return "Color offset\nOffset the colors in the palette,\nvalid values are 0-1024";
		case IDC_EDIT2:
			return "Seed value for making a random palette";
		case IDC_BUTTON2:
			return "Generate the given number of key colors\nfrom the seed value";
		case IDC_BUTTON13:
			return "Generate the given number of key colors\nfrom a random seed value";
		case IDC_BUTTON12:
			return "Generate RGB key colors\nfrom the given values";
		case IDC_BUTTON14:
			return "Generate RGB key colors\nfrom random values";
		case IDC_BUTTON1:
			return "Save the current palette";
		case IDC_BUTTON5:
			return "Open a previosly saved palette";
		case IDC_CHECK4:
			return "Apply 3D-like shadows based on changes in iteration values";
		case IDC_EDIT20:
		case IDC_SPIN6:
			return "Slope shadow depth";
		case IDC_EDIT21:
		case IDC_SPIN7:
			return "Slope shadow strength";
		case IDC_EDIT22:
		case IDC_SPIN8:
			return "Slope shadow angle (0-360)";
		case IDC_BUTTON22:
			return "Select an image from which colors will be fetched";
		case IDC_BUTTON6:
			return "Double the number of Key Colors\nby spreading out the current colors";
		case IDC_BUTTON7:
			return "Expand the number of Key Colors to 1024\nby spreading out the current colors";
		case IDC_BUTTON10:
			return "Double by repeating the Key Colors";
		case IDC_CHECK1:
			return "Move the cursor over the fractal to select the Key Color in the list.\nWill only work if color offset is zero";
		case IDC_CHECK2:
			return "Make colors smooth";
		case IDC_CHECK3:
			return "Inverse color transition";
		case IDC_COMBO1:
			return "Color method. Available methods are\nStandard: Standard iteration band coloring\nSquare root: Iterations are squared before colors are appplied\nCubic root: Cube root is applied before colors\nLogarithm: Logarithm is applied before colors\nStretched: The palette is stretched over min-max iteration values\nDistance (Linear): Distance Estimation with linear transfer (2.11.1 compatible)\nDE+Standard: hybrid mode\nDistance (Logarithm) DE with log transfer\nDistance (Square Root) DE with sqrt transfer (2.11.1+gmp.DATE compatible)";
		case IDC_DIFFERENCES:
			return "Derivative differencing calculation method for distance colouring";
		case IDC_RADIO4:
			return "Colors are merged on distinct steps";
		case IDC_RADIO5:
			return "Colors are merged by sine function with given period length";
		case IDC_EDIT9:
		case IDC_SPIN3:
			return "Length of period";
		case IDC_EDIT10:
		case IDC_SPIN4:
			return "Rate in percent of merged color";
		case IDC_BUTTON8:
			return "Select the color to be merged, which will be applied when OK is clicked in the color dialog";
		case IDC_LIST1:
			return "List of Key Colors.\nEach Key Color can be edited by double click.\nAdditional functions are available by right click on a Key Color";
		case IDC_CHECK6:
			return "Activate sine waves on HSB coloring";
		case IDC_CHECK7:
			return "Blend Infinite Colors and the Color Palette";
		case IDC_COMBO4:
			return "Select type of wave:\nHue, Saturation or Brightness";
		case IDC_EDIT23:
			return "Period length of the wave";
		case IDC_BUTTON29:
			return "Change the period wave to the nearest higher prime value";
		case IDC_BUTTON30:
			return "Fill the palette with the colors from infinte waves";
		case IDC_BUTTON26:
			return "Add a new wave with the given values";
		case IDC_BUTTON27:
			return "Update the selected wave";
		case IDC_BUTTON28:
			return "Remove the selected wave";
		case IDC_EDIT11:
			return "Period lenght of Red color";
		case IDC_EDIT14:
			return "Period lenght of Green color";
		case IDC_EDIT16:
			return "Period lenght of Blue color";
		case IDC_EDIT18:
			return "Period lenght of Black and White color";
		case IDC_BUTTON16:
			return "Change the period wave to the nearest higher prime value for Red color";
		case IDC_BUTTON19:
			return "Change the period wave to the nearest higher prime value for Green color";
		case IDC_BUTTON20:
			return "Change the period wave to the nearest higher prime value for Blue color";
		case IDC_BUTTON21:
			return "Change the period wave to the nearest higher prime value for Black and White color";
		case IDC_EDIT13:
			return "Change the start position of the wave of Red color";
		case IDC_EDIT15:
			return "Change the start position of the wave of Green color";
		case IDC_EDIT17:
			return "Change the start position of the wave of Blue color";
		case IDC_EDIT19:
			return "Change the start position of the wave of Black and White color";
		case IDC_LIST6:
			return "List of Infinite Waves";
		case IDC_BUTTON17:
			return "Apply more contrast on the palette";
		case IDC_BUTTON18:
			return "Apply less contrast on the palette";
		case IDOK:
			return "Apply current palette";
		case IDCLOSE:
			return "Close the dialog and undo all changes";
		case IDCANCEL:
			return "Close the dialog";
		case 1051:
			return "Enable texture";
		case 1052:
			return "Texture depth";
		case 1053:
			return "Texture strength/ratio";
		case 1054:
			return "Browse for image";
		case 1055:
			return "Texture image";
		}
	}
	else if(lParam==1){
		switch(nID){
		case IDC_EDIT1:
			return "Maximum number of iterations";
		case IDC_EDIT2:
			return "Minimum number of iteration in current view";
		case IDC_EDIT5:
			return "Maximum number of iteration in current view";
		case IDC_EDIT7:
			return "Iterations skipped by Series Approximation";
		case IDC_COMBO2:
			return "Bailout value for iterations";
		case IDC_COMBO3:
			return "Power of Mandelbrot function";
		case IDC_EDIT3:
			return "Maximum of extra references for glitch correction";
		case IDC_GLITCHLOWTOLERANCE:
			return "Checked for low tolerance of Glitch Detection\nComplex images may need this to be checked to be rendered correctly\nThe render time may be faster if this checkbox is not checked";
		case IDC_CHECK1:
			return "Checked for low tolerance of Series Approximation\nComplex images may need this to be checked to be rendered correctly\nThe render time may be faster if this checkbox is not checked";
		case IDC_CHECK2:
			return "Terms for Series approximation is adjusted\nbased on the number of pixels to be rendered.";
		case IDC_COMBO5:
			return "List of type of Mandelbrot based Fractals\nSome of them have additional Power options";
		case IDC_COMBO6:
			return "Terms for Series approximation.\nMore terms usually yield more skipped iterations and faster rendering,\nhowever is more time consuming to be processed";
		case IDC_EDIT8:
			return "Display number of calculations performed";
		case IDC_CHECK3:
			return "Include real part when checking bailout.\nUncheck for variation";
		case IDC_CHECK5:
			return "Include imaginary part when checking bailout.\nUncheck for variation";
		case 1002:
			return "Real seed value (0 is standard)";
		case 1003:
			return "Imaginary seed value (0 is standard)";
		case IDOK:
			return "Apply and close";
		case IDCANCEL:
			return "Close and undo";
#ifdef KF_OPENCL
		case IDC_COMBO_OPENCL_DEVICE:
			return "Select the OpenCL device to use for per-pixel iteration calculations";
#endif
		}
	}
	else if(lParam==2){
		switch(nID){
		case IDC_EDIT1:
			return "Real value";
		case IDC_EDIT3:
			return "Imaginary value";
		case IDC_EDIT4:
			return "Zoom value";
		case IDC_EDIT2:
			return "Display minimum iteration value of current location";
		case IDC_EDIT5:
			return "Display maximum iteration value of current location";
		case IDOK:
			return "Apply and close";
		case IDCANCEL:
			return "Close and undo";
		}
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
		switch(nID){
		case IDC_RADIO1:
			return "Add Reference\nclick in the view to add additional references";
		case IDC_RADIO2:
			return "Set main reference.\nThe whole image will be rendered";
		case IDC_RADIO3:
			return "Erase specific parts of the view by clicking\nAfter erased add references in the erased areas";
		case IDC_BUTTON1:
			return "Save eventual changes and go to previous Key Frame";
		case IDOK:
			return "Save eventual changes and go to next Key Frame";
		case IDC_BUTTON5:
			return "Save eventual changes and go to previous Key Frame\nThe current frame will be applied on the center of the previous frame";
		case IDC_BUTTON2:
			return "Undo all changes on current Key Frame";
		case IDC_EDIT1:
			return "Current Key Frame index";
		case IDC_BUTTON3:
			return "Jump to specified Key Frame index";
		case IDCANCEL:
			return "Close this dialog";
		case IDC_EDIT2:
			return "Show if current Key Frame is changed";
		case IDC_EDIT3:
			return "Status of reading/saving current Key Frame";
		case IDC_BUTTON4:
			return "Automatically solve glitches in Key Frames\nThe Key Frames will be browsed backwards\nand stop on the first frame\nStart this function preferable from the last frame";
		case IDC_EDIT4:
			return "Shows status of automatically glitch solving";
		case IDC_EDITMAXREFS:
			return "Choose maximum number of references to add per frame when solving glitches";

		}
	}
	static char szTmp[128];
	wsprintf(szTmp,"nID=%d, lParam=%d",nID,lParam);
	return szTmp;
}


int WINAPI IterationProc(HWND hWnd,UINT uMsg,WPARAM wParam,LPARAM lParam)
{
	(void) lParam;
	if(uMsg==WM_INITDIALOG || uMsg==WM_TIMER){
		if(uMsg==WM_INITDIALOG){
			SendMessage(hWnd, WM_SETICON, ICON_SMALL, LPARAM(g_hIcon));
			SendMessage(hWnd, WM_SETICON, ICON_BIG, LPARAM(g_hIcon));
			InitToolTip(hWnd,GetModuleHandle(NULL),GetToolText,1);
			SetDlgItemInt(hWnd,IDC_EDIT1,g_SFT.GetIterations(),FALSE);
			SetTimer(hWnd,0,1000,NULL);

			SendDlgItemMessage(hWnd,IDC_COMBO2,CB_ADDSTRING,0,(LPARAM)"High bailout");
			SendDlgItemMessage(hWnd,IDC_COMBO2,CB_ADDSTRING,0,(LPARAM)"Bailout=2");
			SendDlgItemMessage(hWnd,IDC_COMBO2,CB_SETCURSEL,g_SFT.GetSmoothMethod(),0);

			combo5_addstrings(hWnd, IDC_COMBO5);

			SIZE sc;
			HDC hDC = GetDC(NULL);
			SelectObject(hDC,(HFONT)GetStockObject(ANSI_VAR_FONT));
			int i, nMaxWidth=0;
			for(i=0;i<SendDlgItemMessage(hWnd,IDC_COMBO5,CB_GETCOUNT,0,0);i++){
				int n = SendDlgItemMessage(hWnd,IDC_COMBO5,CB_GETLBTEXTLEN,i,0);
				char *szT = new char[n+1];
				SendDlgItemMessage(hWnd,IDC_COMBO5,CB_GETLBTEXT,i,(LPARAM)szT);
				GetTextExtentPoint32A(hDC,szT,strlen(szT),&sc);
				if(sc.cx>nMaxWidth)
					nMaxWidth = sc.cx;
				delete szT;
			}
			SendDlgItemMessage(hWnd,IDC_COMBO5,CB_SETDROPPEDWIDTH,nMaxWidth+8+GetSystemMetrics(SM_CXHTHUMB),0);

			SendDlgItemMessage(hWnd,IDC_COMBO5,CB_SETCURSEL,g_SFT.GetFractalType(),0);
			int nType = SendDlgItemMessage(hWnd,IDC_COMBO5,CB_GETCURSEL,0,0);
			SendDlgItemMessage(hWnd,IDC_COMBO3,CB_ADDSTRING,0,(LPARAM)"2");
			if(nType<=4 || nType>=10)
				SendDlgItemMessage(hWnd,IDC_COMBO3,CB_ADDSTRING,0,(LPARAM)"3");
			if(nType==0){
				int p;
				for(p=4;p<=10;p++){
					char szNum[4];
					wsprintf(szNum,"%d",p);
					SendDlgItemMessage(hWnd,IDC_COMBO3,CB_ADDSTRING,0,(LPARAM)szNum);
				}
			}
			else{
				SendDlgItemMessage(hWnd,IDC_COMBO3,CB_ADDSTRING,0,(LPARAM)"4");
				SendDlgItemMessage(hWnd,IDC_COMBO3,CB_ADDSTRING,0,(LPARAM)"5");
				SendDlgItemMessage(hWnd,IDC_COMBO3,CB_ADDSTRING,0,(LPARAM)"6");
			}
			SendDlgItemMessage(hWnd,IDC_COMBO3,CB_SETCURSEL,g_SFT.GetPower()-2,0);
			EnableWindow(GetDlgItem(hWnd,IDC_COMBO3),nType<=4);
			if(nType>=10)
				SendDlgItemMessage(hWnd,IDC_COMBO3,CB_SETCURSEL,1,0);
			if(nType>=15)
				SendDlgItemMessage(hWnd,IDC_COMBO3,CB_SETCURSEL,2,0);
			if(nType>=27)
				SendDlgItemMessage(hWnd,IDC_COMBO3,CB_SETCURSEL,3,0);
			if(nType>=33)
				SendDlgItemMessage(hWnd,IDC_COMBO3,CB_SETCURSEL,2,0);
			if(nType==40)
				SendDlgItemMessage(hWnd,IDC_COMBO3,CB_SETCURSEL,4,0);
			if(nType==41)
				SendDlgItemMessage(hWnd,IDC_COMBO3,CB_SETCURSEL,0,0);
			if(nType>=42)
				SendDlgItemMessage(hWnd,IDC_COMBO3,CB_SETCURSEL,1,0);
			if(nType==45 || nType==46)
				SendDlgItemMessage(hWnd,IDC_COMBO3,CB_SETCURSEL,2,0);
			if(nType==47 || nType==48)
				SendDlgItemMessage(hWnd,IDC_COMBO3,CB_SETCURSEL,3,0);
			if(nType==49 || nType==50)
				SendDlgItemMessage(hWnd,IDC_COMBO3,CB_SETCURSEL,4,0);
			if(nType>=51)
				SendDlgItemMessage(hWnd,IDC_COMBO3,CB_SETCURSEL,2,0);

			SendDlgItemMessage(hWnd,IDC_GLITCHLOWTOLERANCE,BM_SETCHECK,g_SFT.GetGlitchLowTolerance(),0);
			SendDlgItemMessage(hWnd,IDC_CHECK1,BM_SETCHECK,g_SFT.GetApproxLowTolerance(),0);
			SendDlgItemMessage(hWnd,IDC_CHECK2,BM_SETCHECK,g_SFT.GetAutoApproxTerms(),0);
			SetDlgItemInt(hWnd,IDC_COMBO3,g_SFT.GetPower(),FALSE);
			SetDlgItemInt(hWnd,IDC_EDIT3,g_SFT.GetMaxReferences(),FALSE);

			SendDlgItemMessage(hWnd,IDC_COMBO6,CB_ADDSTRING,0,(LPARAM)"5");
			SendDlgItemMessage(hWnd,IDC_COMBO6,CB_ADDSTRING,0,(LPARAM)"10");
			SendDlgItemMessage(hWnd,IDC_COMBO6,CB_ADDSTRING,0,(LPARAM)"15");
			SendDlgItemMessage(hWnd,IDC_COMBO6,CB_ADDSTRING,0,(LPARAM)"20");
			SendDlgItemMessage(hWnd,IDC_COMBO6,CB_ADDSTRING,0,(LPARAM)"30");
			SetDlgItemInt(hWnd,IDC_COMBO6,g_SFT.GetApproxTerms(),FALSE);

			//if(nType)
			//	SendDlgItemMessage(hWnd,IDC_COMBO3,CB_SETCURSEL,0,0);
			//EnableWindow(GetDlgItem(hWnd,IDC_COMBO3),nType==0);
			EnableWindow(GetDlgItem(hWnd,IDC_CHECK2),nType==0 && g_SFT.GetPower()==2);
			EnableWindow(GetDlgItem(hWnd,IDC_COMBO6),nType==0 && g_SFT.GetPower()==2);

			SendDlgItemMessage(hWnd,IDC_CHECK3,BM_SETCHECK,g_real==0?0:1,0);
			SendDlgItemMessage(hWnd,IDC_CHECK5,BM_SETCHECK,g_imag==0?0:1,0);

			char szTmp[40];
			sprintf(szTmp,"%g",g_SeedR);
			SetDlgItemText(hWnd,IDC_EDIT4,szTmp);
			sprintf(szTmp,"%g",g_SeedI);
			SetDlgItemText(hWnd,IDC_EDIT9,szTmp);

			sprintf(szTmp,"%g",g_FactorAR);
			SetDlgItemText(hWnd,IDC_EDIT28,szTmp);
			sprintf(szTmp,"%g",g_FactorAI);
			SetDlgItemText(hWnd,IDC_EDIT10,szTmp);
		}
		int nMin, nMax, nCalc=0,nType=0;
		g_SFT.GetIterations(nMin,nMax,&nCalc,&nType);
		SetDlgItemInt(hWnd,IDC_EDIT2,nMin,FALSE);
		SetDlgItemInt(hWnd,IDC_EDIT5,nMax,FALSE);
		SetDlgItemInt(hWnd,IDC_EDIT7,g_SFT.GetMaxApproximation(),FALSE);
		if(uMsg==WM_TIMER){
			char szCalc[128];
			wsprintf(szCalc,"%d",nCalc);
			int k=strlen(szCalc);
			while(k>3){
				int n=strlen(szCalc);
				while(n>k-4){
					szCalc[n+1]=szCalc[n];
					n--;
				}
				szCalc[k-3]=' ';
				k-=3;
			}
			if(nType)
				strcat(szCalc," 000 000");
			if(g_nPrevCalc!=-1){
				int nC = nCalc-g_nPrevCalc;
				if(!nType)
					nC/=1000000;
				wsprintf(szCalc+strlen(szCalc),", %d M/s",nC);
			}
			g_nPrevCalc=nCalc;
			SetDlgItemText(hWnd,IDC_EDIT8,szCalc);
			if(SendDlgItemMessage(hWnd,IDC_CHECK2,BM_GETCHECK,0,0))
				SetDlgItemInt(hWnd,IDC_COMBO6,g_SFT.GetApproxTerms(),FALSE);
		}
		else
			g_nPrevCalc=-1;
		return 1;
	}
	else if(uMsg==WM_COMMAND){
		if(wParam==IDOK){
			g_real = SendDlgItemMessage(hWnd,IDC_CHECK3,BM_GETCHECK,0,0);
			g_imag = SendDlgItemMessage(hWnd,IDC_CHECK5,BM_GETCHECK,0,0);
			char szTmp[40];
			GetDlgItemText(hWnd,IDC_EDIT4,szTmp,sizeof(szTmp));
			g_SeedR = atof(szTmp);
			GetDlgItemText(hWnd,IDC_EDIT9,szTmp,sizeof(szTmp));
			g_SeedI = atof(szTmp);
			GetDlgItemText(hWnd,IDC_EDIT28,szTmp,sizeof(szTmp));
			g_FactorAR = atof(szTmp);
			GetDlgItemText(hWnd,IDC_EDIT10,szTmp,sizeof(szTmp));
			g_FactorAI = atof(szTmp);

			g_bExamineDirty=TRUE;
			g_SFT.SetSmoothMethod(SendDlgItemMessage(hWnd,IDC_COMBO2,CB_GETCURSEL,0,0));
			char szPower[256];
			GetDlgItemText(hWnd,IDC_COMBO6,szPower,sizeof(szPower));
			g_SFT.SetApproxTerms(atoi(szPower));
			g_SFT.SetGlitchLowTolerance(SendDlgItemMessage(hWnd,IDC_GLITCHLOWTOLERANCE,BM_GETCHECK,0,0));
			g_SFT.SetApproxLowTolerance(SendDlgItemMessage(hWnd,IDC_CHECK1,BM_GETCHECK,0,0));
			g_SFT.SetAutoApproxTerms(SendDlgItemMessage(hWnd,IDC_CHECK2,BM_GETCHECK,0,0));
			ExitToolTip(hWnd);
			EndDialog(hWnd,GetDlgItemInt(hWnd,IDC_EDIT1,NULL,0));
			g_SFT.SetFractalType(SendDlgItemMessage(hWnd,IDC_COMBO5,CB_GETCURSEL,0,0));
			g_SFT.SetMaxReferences(GetDlgItemInt(hWnd,IDC_EDIT3,NULL,FALSE));
			GetDlgItemText(hWnd,IDC_COMBO3,szPower,sizeof(szPower));
			g_SFT.SetPower(atoi(szPower));
		}
		else if(wParam==IDC_CHECK3){
			if(!SendDlgItemMessage(hWnd,IDC_CHECK3,BM_GETCHECK,0,0) && !SendDlgItemMessage(hWnd,IDC_CHECK5,BM_GETCHECK,0,0))
				SendDlgItemMessage(hWnd,IDC_CHECK5,BM_SETCHECK,1,0);
		}
		else if(wParam==IDC_CHECK5){
			if(!SendDlgItemMessage(hWnd,IDC_CHECK3,BM_GETCHECK,0,0) && !SendDlgItemMessage(hWnd,IDC_CHECK5,BM_GETCHECK,0,0))
				SendDlgItemMessage(hWnd,IDC_CHECK3,BM_SETCHECK,1,0);
		}
		else if(wParam==IDCANCEL){
			ExitToolTip(hWnd);
			EndDialog(hWnd,0);
		}
		else if(HIWORD(wParam)==CBN_SELCHANGE && LOWORD(wParam)==IDC_COMBO5){
			int nType = SendDlgItemMessage(hWnd,IDC_COMBO5,CB_GETCURSEL,0,0);
			int nPow = SendDlgItemMessage(hWnd,IDC_COMBO3,CB_GETCURSEL,0,0);
			SendDlgItemMessage(hWnd,IDC_COMBO3,CB_RESETCONTENT,0,0);
			SendDlgItemMessage(hWnd,IDC_COMBO3,CB_ADDSTRING,0,(LPARAM)"2");
			if(nType<=4 || nType>=10)
				SendDlgItemMessage(hWnd,IDC_COMBO3,CB_ADDSTRING,0,(LPARAM)"3");
			if(nType==0){
				int p;
				for(p=4;p<=10;p++){
					char szNum[4];
					wsprintf(szNum,"%d",p);
					SendDlgItemMessage(hWnd,IDC_COMBO3,CB_ADDSTRING,0,(LPARAM)szNum);
				}
			}
			else{
				SendDlgItemMessage(hWnd,IDC_COMBO3,CB_ADDSTRING,0,(LPARAM)"4");
				SendDlgItemMessage(hWnd,IDC_COMBO3,CB_ADDSTRING,0,(LPARAM)"5");
				SendDlgItemMessage(hWnd,IDC_COMBO3,CB_ADDSTRING,0,(LPARAM)"6");
			}
			SendDlgItemMessage(hWnd,IDC_COMBO3,CB_SETCURSEL,nPow,0);
			if(SendDlgItemMessage(hWnd,IDC_COMBO3,CB_GETCURSEL,0,0)==-1)
				SendDlgItemMessage(hWnd,IDC_COMBO3,CB_SETCURSEL,0,0);
#ifndef _DEBUG
			EnableWindow(GetDlgItem(hWnd,IDC_COMBO3),nType<=4);
#endif
			if(nType>=10)
				SendDlgItemMessage(hWnd,IDC_COMBO3,CB_SETCURSEL,1,0);
			if(nType>=15)
				SendDlgItemMessage(hWnd,IDC_COMBO3,CB_SETCURSEL,2,0);
			if(nType>=27)
				SendDlgItemMessage(hWnd,IDC_COMBO3,CB_SETCURSEL,3,0);
			if(nType>=33)
				SendDlgItemMessage(hWnd,IDC_COMBO3,CB_SETCURSEL,2,0);
			if(nType==40)
				SendDlgItemMessage(hWnd,IDC_COMBO3,CB_SETCURSEL,4,0);
			if(nType==41)
				SendDlgItemMessage(hWnd,IDC_COMBO3,CB_SETCURSEL,0,0);
			if(nType>=42)
				SendDlgItemMessage(hWnd,IDC_COMBO3,CB_SETCURSEL,1,0);
			if(nType==45 || nType==46)
				SendDlgItemMessage(hWnd,IDC_COMBO3,CB_SETCURSEL,2,0);
			if(nType==47 || nType==48)
				SendDlgItemMessage(hWnd,IDC_COMBO3,CB_SETCURSEL,3,0);
			if(nType==49 || nType==50)
				SendDlgItemMessage(hWnd,IDC_COMBO3,CB_SETCURSEL,4,0);
			if(nType>=51)
				SendDlgItemMessage(hWnd,IDC_COMBO3,CB_SETCURSEL,2,0);
		}
		int nType = SendDlgItemMessage(hWnd,IDC_COMBO5,CB_GETCURSEL,0,0);
//		if(nType)
//			SendDlgItemMessage(hWnd,IDC_COMBO3,CB_SETCURSEL,0,0);
//		EnableWindow(GetDlgItem(hWnd,IDC_COMBO3),nType==0);
		EnableWindow(GetDlgItem(hWnd,IDC_CHECK2),nType==0 && GetDlgItemInt(hWnd,IDC_COMBO3,NULL,FALSE)==2);
		EnableWindow(GetDlgItem(hWnd,IDC_COMBO6),nType==0 && GetDlgItemInt(hWnd,IDC_COMBO3,NULL,FALSE)==2);
		EnableWindow(GetDlgItem(hWnd,IDC_COMBO6),!SendDlgItemMessage(hWnd,IDC_CHECK2,BM_GETCHECK,0,0));
	}
	return 0;
}
long WINAPI ShowProc(HWND hWnd,UINT uMsg,WPARAM wParam,LPARAM lParam)
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
RECT g_rShow;
COLOR14 g_colCopy={0};
BOOL g_bCaptureMouse=FALSE;
BOOL g_bInitColorDialog=FALSE;
CListBoxEdit *g_pWaves=NULL;
char g_szTmpFile[MAX_PATH];
int WINAPI ColorProc(HWND hWnd,UINT uMsg,WPARAM wParam,LPARAM lParam)
{
	static COLORREF colCust[16]={0};
	if(uMsg==WM_SHOWWINDOW && wParam){
		GetTempPath(sizeof(g_szTmpFile),g_szTmpFile);
		GetTempFileName(g_szTmpFile,"KFR",TRUE,g_szTmpFile);
		g_SFT.SaveFile(g_szTmpFile);
	}
	if(uMsg==WM_INITDIALOG)
	{
		SendMessage(hWnd, WM_SETICON, ICON_SMALL, LPARAM(g_hIcon));
		SendMessage(hWnd, WM_SETICON, ICON_BIG, LPARAM(g_hIcon));
		InitToolTip(hWnd,GetModuleHandle(NULL),GetToolText,0);
	}
	if(uMsg==WM_INITDIALOG || uMsg==WM_USER+99 || (uMsg==WM_SHOWWINDOW && wParam) || (uMsg==WM_COMMAND && (wParam==IDC_CHECK6 || wParam==IDC_CHECK7))){
		char szTexture[256];
		double nRatio, nPower;
		int nMerge;
		BOOL bTexture = g_SFT.GetTexture(nRatio,nPower,nMerge,szTexture);
		SetDlgItemText(hWnd,IDC_EDIT29,szTexture);
		SendDlgItemMessage(hWnd,IDC_CHECK8,BM_SETCHECK,bTexture,0);
		SetDlgItemFloat(hWnd,IDC_EDIT26,nPower);
		SetDlgItemInt(hWnd,IDC_EDIT27,nRatio*100,FALSE);
		if(!g_pWaves){
			HWND hwnds[2]={GetDlgItem(hWnd,IDC_EDIT23),GetDlgItem(hWnd,IDC_EDIT25)};
			g_pWaves = new CListBoxEdit(GetDlgItem(hWnd,IDC_BUTTON26), GetDlgItem(hWnd,IDC_BUTTON27), GetDlgItem(hWnd,IDC_BUTTON28), GetDlgItem(hWnd,IDC_EDIT24), GetDlgItem(hWnd,IDC_LIST6),hwnds, 2);

			SendDlgItemMessage(hWnd,IDC_COMBO4,CB_ADDSTRING,0,(LPARAM)"Hue");
			SendDlgItemMessage(hWnd,IDC_COMBO4,CB_ADDSTRING,0,(LPARAM)"Saturation");
			SendDlgItemMessage(hWnd,IDC_COMBO4,CB_ADDSTRING,0,(LPARAM)"Brightness");
		}
		if(uMsg==WM_COMMAND && (wParam==IDC_CHECK6 || wParam==IDC_CHECK7)){
			g_SFT.SetMW(SendDlgItemMessage(hWnd,IDC_CHECK6,BM_GETCHECK,0,0),SendDlgItemMessage(hWnd,IDC_CHECK7,BM_GETCHECK,0,0));
			PostMessage(hWnd,WM_COMMAND,IDOK,0);
		}
		else{
			BOOL bBlend=FALSE;
			SendDlgItemMessage(hWnd,IDC_CHECK6,BM_SETCHECK,g_SFT.GetMW(&bBlend),0);
			SendDlgItemMessage(hWnd,IDC_CHECK7,BM_SETCHECK,bBlend,0);
		}
		EnableWindow(GetDlgItem(hWnd,IDC_COMBO4),g_SFT.GetMW());
		EnableWindow(GetDlgItem(hWnd,IDC_EDIT23),g_SFT.GetMW());
		EnableWindow(GetDlgItem(hWnd,IDC_CHECK7),g_SFT.GetMW());
		EnableWindow(GetDlgItem(hWnd,IDC_BUTTON29),g_SFT.GetMW());
		EnableWindow(GetDlgItem(hWnd,IDC_BUTTON30),g_SFT.GetMW());
		EnableWindow(GetDlgItem(hWnd,IDC_EDIT25),g_SFT.GetMW());
		EnableWindow(GetDlgItem(hWnd,IDC_LIST6),g_SFT.GetMW());
		EnableWindow(GetDlgItem(hWnd,IDC_BUTTON26),g_SFT.GetMW());
		EnableWindow(GetDlgItem(hWnd,IDC_BUTTON27),g_SFT.GetMW());
		EnableWindow(GetDlgItem(hWnd,IDC_BUTTON28),g_SFT.GetMW());
	}
	else if(HIWORD(wParam)==LBN_SELCHANGE && LOWORD(wParam)==IDC_LIST6){
		char szTmp[256];
		int i = SendDlgItemMessage(hWnd,IDC_LIST6,LB_GETCURSEL,0,0);
		if(i!=-1){
			SendDlgItemMessage(hWnd,IDC_LIST6,LB_GETTEXT,i,(LPARAM)szTmp);
			if(*szTmp=='H')
				SendDlgItemMessage(hWnd,IDC_COMBO4,CB_SETCURSEL,0,0);
			else if(*szTmp=='S')
				SendDlgItemMessage(hWnd,IDC_COMBO4,CB_SETCURSEL,1,0);
			else if(*szTmp=='B')
				SendDlgItemMessage(hWnd,IDC_COMBO4,CB_SETCURSEL,2,0);
		}
	}
	else if(HIWORD(wParam)==CBN_SELCHANGE && LOWORD(wParam)==IDC_COMBO4){
		int i = SendDlgItemMessage(hWnd,IDC_COMBO4,CB_GETCURSEL,0,0);
		if(i==0)
			SetDlgItemText(hWnd,IDC_EDIT24,"H");
		else if(i==1)
			SetDlgItemText(hWnd,IDC_EDIT24,"S");
		else if(i==2)
			SetDlgItemText(hWnd,IDC_EDIT24,"B");
	}
	if(uMsg==WM_INITDIALOG || uMsg==WM_USER+99 || (uMsg==WM_SHOWWINDOW && wParam)){
		g_bInitColorDialog=FALSE;

		SendDlgItemMessage(hWnd,IDC_LIST6,LB_RESETCONTENT,0,0);
		int i;
		char szTmp[256];
		for(i=0;i<g_SFT.GetMWCount();i++){
			int nPeriod, nStart, nType;
			g_SFT.GetMW(i,nPeriod, nStart, nType);
			if(nType==0)
				strcpy(szTmp,"H\t");
			else if(nType==1)
				strcpy(szTmp,"S\t");
			else if(nType==2)
				strcpy(szTmp,"B\t");
			itoa(nPeriod,szTmp+strlen(szTmp),10);
			strcat(szTmp,"\t");
			itoa(nStart,szTmp+strlen(szTmp),10);
			SendDlgItemMessage(hWnd,IDC_LIST6,LB_ADDSTRING,0,(LPARAM)szTmp);
		}

		SetWindowText(hWnd,"Number of Colors");
		SetDlgItemInt(hWnd,IDC_EDIT1,g_SFT.GetNumOfColors(),FALSE);
		SetDlgItemInt(hWnd,IDC_EDIT2,g_SFT.GetSeed(),FALSE);
		sprintf(szTmp,"%f",g_SFT.GetIterDiv());
		int e = strlen(szTmp);
		while(e && szTmp[e-1]=='0')
			e--;
		szTmp[e]=0;
		SetDlgItemText(hWnd,IDC_EDIT3,szTmp);
		SetDlgItemInt(hWnd,IDC_EDIT12,g_SFT.GetColorOffset(),FALSE);
		SendDlgItemMessage(hWnd,IDC_SPIN1,UDM_SETRANGE,0,MAKELONG(1024,2));
		SendDlgItemMessage(hWnd,IDC_SPIN2,UDM_SETRANGE,0,MAKELONG(1000000,1));
		SendDlgItemMessage(hWnd,IDC_SPIN5,UDM_SETRANGE,0,MAKELONG(1023,0));
		SendDlgItemMessage(hWnd,IDC_SPIN6,UDM_SETRANGE,0,MAKELONG(100,1));
		SendDlgItemMessage(hWnd,IDC_SPIN7,UDM_SETRANGE,0,MAKELONG(100,1));
		SendDlgItemMessage(hWnd,IDC_SPIN8,UDM_SETRANGE,0,MAKELONG(360,-360));
		int nSP, nSC, nSA;
		SendDlgItemMessage(hWnd,IDC_CHECK4,BM_SETCHECK,g_SFT.GetSlopes(nSP,nSC,nSA),0);
		SetDlgItemInt(hWnd,IDC_EDIT20,nSP,FALSE);
		SetDlgItemInt(hWnd,IDC_EDIT21,nSC,FALSE);
		SetDlgItemInt(hWnd,IDC_EDIT22,nSA,FALSE);
		EnableWindow(GetDlgItem(hWnd,IDC_EDIT20),SendDlgItemMessage(hWnd,IDC_CHECK4,BM_GETCHECK,0,0));
		EnableWindow(GetDlgItem(hWnd,IDC_EDIT21),SendDlgItemMessage(hWnd,IDC_CHECK4,BM_GETCHECK,0,0));
		EnableWindow(GetDlgItem(hWnd,IDC_EDIT22),SendDlgItemMessage(hWnd,IDC_CHECK4,BM_GETCHECK,0,0));

		SendDlgItemMessage(hWnd,IDC_LIST1,WM_SETREDRAW,0,0);
		SendDlgItemMessage(hWnd,IDC_LIST1,LB_RESETCONTENT,0,0);
		for(i=0;i<g_SFT.GetNumOfColors();i++)
			SendDlgItemMessage(hWnd,IDC_LIST1,LB_ADDSTRING,0,(LPARAM)"");
		SendDlgItemMessage(hWnd,IDC_LIST1,WM_SETREDRAW,1,0);
		HWND hw = GetDlgItem(hWnd,IDC_SHOW);
		if(hw){
//		BOOL b = SetWindowLong(hw,GWL_WNDPROC,(LONG)ShowProc);
			GetWindowRect(hw,&g_rShow);
			g_rShow.right-=g_rShow.left;
			g_rShow.bottom-=g_rShow.top;
			ScreenToClient(hWnd,(LPPOINT)&g_rShow);
			DestroyWindow(hw);
		}
		SendDlgItemMessage(hWnd,IDC_CHECK2,BM_SETCHECK,g_SFT.GetTransition(),0);
		SendDlgItemMessage(hWnd,IDC_CHECK3,BM_SETCHECK,g_SFT.GetITransition(),0);
		if(uMsg==WM_INITDIALOG || (uMsg==WM_SHOWWINDOW && wParam)){
			SetDlgItemInt(hWnd,IDC_EDIT9,2,0);
			SetDlgItemInt(hWnd,IDC_EDIT11,4,0);
			SendDlgItemMessage(hWnd,IDC_SPIN3,UDM_SETRANGE,0,MAKELONG(1,1024));
			SetDlgItemInt(hWnd,IDC_EDIT10,50,0);
			SendDlgItemMessage(hWnd,IDC_SPIN4,UDM_SETRANGE,0,MAKELONG(1,100));

			if(SendDlgItemMessage(hWnd,IDC_COMBO1,CB_GETCOUNT,0,0)==0){
				SendDlgItemMessage(hWnd,IDC_COMBO1,CB_ADDSTRING,0,(LPARAM)"Standard");
				SendDlgItemMessage(hWnd,IDC_COMBO1,CB_ADDSTRING,0,(LPARAM)"Square root");
				SendDlgItemMessage(hWnd,IDC_COMBO1,CB_ADDSTRING,0,(LPARAM)"Cubic root");
				SendDlgItemMessage(hWnd,IDC_COMBO1,CB_ADDSTRING,0,(LPARAM)"Logarithm");
				SendDlgItemMessage(hWnd,IDC_COMBO1,CB_ADDSTRING,0,(LPARAM)"Stretched");
				SendDlgItemMessage(hWnd,IDC_COMBO1,CB_ADDSTRING,0,(LPARAM)"Distance (Linear)");
				SendDlgItemMessage(hWnd,IDC_COMBO1,CB_ADDSTRING,0,(LPARAM)"DE+Standard");
				SendDlgItemMessage(hWnd,IDC_COMBO1,CB_ADDSTRING,0,(LPARAM)"Distance (Logarithm)");
				SendDlgItemMessage(hWnd,IDC_COMBO1,CB_ADDSTRING,0,(LPARAM)"Distance (Square Root)");
			}

			if(SendDlgItemMessage(hWnd,IDC_DIFFERENCES,CB_GETCOUNT,0,0)==0){
				SendDlgItemMessage(hWnd,IDC_DIFFERENCES,CB_ADDSTRING,0,(LPARAM)"Traditional");
				SendDlgItemMessage(hWnd,IDC_DIFFERENCES,CB_ADDSTRING,0,(LPARAM)"Forward 3x3");
				SendDlgItemMessage(hWnd,IDC_DIFFERENCES,CB_ADDSTRING,0,(LPARAM)"Central 3x3");
				SendDlgItemMessage(hWnd,IDC_DIFFERENCES,CB_ADDSTRING,0,(LPARAM)"Diagonal 2x2");
			}

			if(uMsg==WM_INITDIALOG){
				SendDlgItemMessage(hWnd,IDC_RADIO4,BM_SETCHECK,1,0);
				SendDlgItemMessage(hWnd,IDC_RADIO5,BM_SETCHECK,0,0);
			}
		}
		else
			InvalidateRect(hWnd,NULL,FALSE);
		SendDlgItemMessage(hWnd,IDC_COMBO1,CB_SETCURSEL,g_SFT.GetColorMethod(),0);
		SendDlgItemMessage(hWnd,IDC_DIFFERENCES,CB_SETCURSEL,g_SFT.GetDifferences(),0);
		g_bInitColorDialog=TRUE;
		return 1;
	}
	else if(uMsg==WM_PAINT){
		PAINTSTRUCT ps;
		BeginPaint(hWnd,&ps);
		int i;
		RECT rb={g_rShow.left,g_rShow.top,g_rShow.right+g_rShow.left,g_rShow.top+g_rShow.bottom+1};
		FillRect(ps.hdc,&rb,(HBRUSH)GetStockObject(WHITE_BRUSH));
		for(i=0;i<1024;i++){
			RECT r={i*g_rShow.right/1024 + g_rShow.left,g_rShow.top,(i+1)*g_rShow.right/1024+g_rShow.left,g_rShow.top+g_rShow.bottom/4};
			COLOR14 c = g_SFT.GetColor(i);
			HBRUSH br = CreateSolidBrush(RGB(c.b,c.g,c.r));
			FillRect(ps.hdc,&r,br);
			DeleteObject(br);
		}
		HPEN pn = CreatePen(0,1,RGB(255,0,0));
		HPEN pnOld = (HPEN)SelectObject(ps.hdc,pn);
		MoveToEx(ps.hdc,g_rShow.left,g_rShow.top+g_rShow.bottom/2 - g_SFT.GetColor(0).b*(g_rShow.bottom/4)/255,NULL);
		for(i=1;i<1024;i++)
			LineTo(ps.hdc,i*g_rShow.right/1024 + g_rShow.left,g_rShow.top+g_rShow.bottom/2 - g_SFT.GetColor(i).b*(g_rShow.bottom/4)/255);
		SelectObject(ps.hdc,pnOld);
		DeleteObject(pn);

		pn = CreatePen(0,1,RGB(0,255,0));
		pnOld = (HPEN)SelectObject(ps.hdc,pn);
		MoveToEx(ps.hdc,g_rShow.left,g_rShow.top+3*g_rShow.bottom/4 - g_SFT.GetColor(0).g*(g_rShow.bottom/4)/255,NULL);
		for(i=1;i<1024;i++)
			LineTo(ps.hdc,i*g_rShow.right/1024 + g_rShow.left,g_rShow.top+3*g_rShow.bottom/4 - g_SFT.GetColor(i).g*(g_rShow.bottom/4)/255);
		SelectObject(ps.hdc,pnOld);
		DeleteObject(pn);

		pn = CreatePen(0,1,RGB(0,0,255));
		pnOld = (HPEN)SelectObject(ps.hdc,pn);
		MoveToEx(ps.hdc,g_rShow.left,g_rShow.top+g_rShow.bottom - g_SFT.GetColor(0).r*(g_rShow.bottom/4)/255,NULL);
		for(i=1;i<1024;i++)
			LineTo(ps.hdc,i*g_rShow.right/1024 + g_rShow.left,g_rShow.top+g_rShow.bottom - g_SFT.GetColor(i).r*(g_rShow.bottom/4)/255);
		SelectObject(ps.hdc,pnOld);
		DeleteObject(pn);

		EndPaint(hWnd,&ps);
		return 0;
	}
/*	else if(uMsg==WM_NOTIFY){
		LPNMUPDOWN lpnmud = (LPNMUPDOWN)lParam;
		if(lpnmud->hdr.code==UDN_DELTAPOS && (lpnmud->hdr.idFrom==IDC_SPIN5 || lpnmud->hdr.idFrom==IDC_SPIN1 || lpnmud->hdr.idFrom==IDC_SPIN2))
			SendMessage(hWnd,WM_COMMAND,IDOK,0);
	}
*/	else if(uMsg==WM_COMMAND){
		EnableWindow(GetDlgItem(hWnd,IDC_EDIT20),SendDlgItemMessage(hWnd,IDC_CHECK4,BM_GETCHECK,0,0));
		EnableWindow(GetDlgItem(hWnd,IDC_EDIT21),SendDlgItemMessage(hWnd,IDC_CHECK4,BM_GETCHECK,0,0));
		EnableWindow(GetDlgItem(hWnd,IDC_EDIT22),SendDlgItemMessage(hWnd,IDC_CHECK4,BM_GETCHECK,0,0));
		if((g_bInitColorDialog && (HIWORD(wParam)==EN_UPDATE && (LOWORD(wParam)==IDC_EDIT1 || LOWORD(wParam)==IDC_EDIT3 || LOWORD(wParam)==IDC_EDIT12 || LOWORD(wParam)==IDC_EDIT20 || LOWORD(wParam)==IDC_EDIT21 || LOWORD(wParam)==IDC_EDIT22))) || (HIWORD(wParam)==CBN_SELCHANGE && (LOWORD(wParam)==IDC_COMBO1 || LOWORD(wParam)==IDC_DIFFERENCES))){
			g_bInitColorDialog=FALSE;
			SendMessage(hWnd,WM_COMMAND,IDOK,0);
			return 0;
		}
		if(wParam==IDCANCEL){
			ShowWindow(hWnd,SW_HIDE);
		}
		else if(wParam==IDCLOSE){
			g_SFT.OpenFile(g_szTmpFile,TRUE);
			ShowWindow(hWnd,SW_HIDE);
		}
		else if(wParam==IDC_BUTTON23){
			char szFile[256]={0};
			GetDlgItemText(hWnd,IDC_EDIT29,szFile,sizeof(szFile));
			if(BrowseFile(hWnd,TRUE,"Select texture","Jpg\0*.jpg\0\0",szFile,sizeof(szFile))){
				SetDlgItemText(hWnd,IDC_EDIT29,szFile);
				SendMessage(hWnd,WM_COMMAND,IDOK,0);
			}
		}
		else if(wParam==IDOK){
			char szTexture[256];
			double nPower;
			int nRatio;
			GetDlgItemText(hWnd,IDC_EDIT29,szTexture,sizeof(szTexture));
			BOOL bTexture = SendDlgItemMessage(hWnd,IDC_CHECK8,BM_GETCHECK,0,0);
			nPower = GetDlgItemFloat(hWnd,IDC_EDIT26);
			nRatio = GetDlgItemInt(hWnd,IDC_EDIT27,NULL,0);
			g_SFT.SetTexture(bTexture,(double)nRatio/100,nPower,100,szTexture);

			int nColors = GetDlgItemInt(hWnd,IDC_EDIT1,NULL,0);
			if(nColors<=0 || nColors>1024){
				nColors=1024;
				SetDlgItemInt(hWnd,IDC_EDIT1,nColors,FALSE);
			}
			g_SFT.ChangeNumOfColors(nColors);
			char szDiv[256];
			GetDlgItemText(hWnd,IDC_EDIT3,szDiv,sizeof(szDiv));
			double nDiv = atof(szDiv);
			if(nDiv==0)
				nDiv=1;
			g_SFT.SetIterDiv(nDiv);
			int nO = GetDlgItemInt(hWnd,IDC_EDIT12,NULL,0);
			g_SFT.SetColorOffset(nO);
			if(nO!=g_SFT.GetColorOffset())
				SetDlgItemInt(hWnd,IDC_EDIT12,g_SFT.GetColorOffset(),FALSE);
			g_SFT.SetColorMethod(SendDlgItemMessage(hWnd,IDC_COMBO1,CB_GETCURSEL,0,0));
			g_SFT.SetDifferences(SendDlgItemMessage(hWnd,IDC_DIFFERENCES,CB_GETCURSEL,0,0));
			g_SFT.SetTransition(SendDlgItemMessage(hWnd,IDC_CHECK2,BM_GETCHECK,0,0));
			g_SFT.SetITransition(SendDlgItemMessage(hWnd,IDC_CHECK3,BM_GETCHECK,0,0));
			if(nColors!=SendDlgItemMessage(hWnd,IDC_LIST1,LB_GETCOUNT,0,0)){
				SendDlgItemMessage(hWnd,IDC_LIST1,WM_SETREDRAW,0,0);
				SendDlgItemMessage(hWnd,IDC_LIST1,LB_RESETCONTENT,0,0);
				int i;
				for(i=0;i<nColors;i++)
					SendDlgItemMessage(hWnd,IDC_LIST1,LB_ADDSTRING,0,(LPARAM)"");
				SendDlgItemMessage(hWnd,IDC_LIST1,WM_SETREDRAW,1,0);
			}
			CStringVektor sv;
			g_pWaves->GetStrings(&sv);
			char *szSV = sv.ToText("\n");
			CStringTable stMW(szSV,"\t","\n");
			sv.DeleteToText(szSV);
			int i;
			for(i=0;i<stMW.GetCount();i++){
				int nType = 0;
				if(*stMW[i][0]=='H')
					nType=0;
				else if(*stMW[i][0]=='S')
					nType=1;
				else if(*stMW[i][0]=='B')
					nType=2;
				int nPeriod = atoi(stMW[i][1]);
				int nStart = atoi(stMW[i][2]);
				if(i==g_SFT.GetMWCount())
					g_SFT.AddMW(nPeriod,nStart,nType);
				else
					g_SFT.UpdateMW(i,nPeriod,nStart,nType);
			}
			while(i<g_SFT.GetMWCount())
				g_SFT.DeleteMW(i);

			g_SFT.SetSlopes(SendDlgItemMessage(hWnd,IDC_CHECK4,BM_GETCHECK,0,0),GetDlgItemInt(hWnd,IDC_EDIT20,NULL,FALSE),GetDlgItemInt(hWnd,IDC_EDIT21,NULL,FALSE),GetDlgItemInt(hWnd,IDC_EDIT22,NULL,TRUE));

			g_SFT.ApplyColors();
			InvalidateRect(GetParent(hWnd),NULL,FALSE);
			UpdateWindow(GetParent(hWnd));
			InvalidateRect(hWnd,NULL,FALSE);
			g_bInitColorDialog=TRUE;
		}
		else if(wParam==IDC_CHECK2 || wParam==IDC_CHECK3 || wParam==IDC_CHECK4)
			PostMessage(hWnd,WM_COMMAND,IDOK,0);
		else if(wParam==IDC_BUTTON1){
			char szFile[256]={0};
			if(BrowseFile(hWnd,FALSE,"Save palette","Palette\0*.kfp\0\0",szFile,sizeof(szFile)))
				g_SFT.SaveFile(szFile);
		}
		else if(wParam==IDC_BUTTON29){
			int val = GetDlgItemInt(hWnd,IDC_EDIT23,NULL,FALSE);
			val = MakePrime(val);
			SetDlgItemInt(hWnd,IDC_EDIT23,val,FALSE);
		}
		else if(wParam==IDC_BUTTON30){
			srand(GetTickCount());
			int nColors = GetDlgItemInt(hWnd,IDC_EDIT1,NULL,0);
			if(nColors>1024)
				nColors=1024;
			int c, w;
			for(c=0;c<nColors;c++){
				double nH = 0, nS = 0, nB = 0;
				int nDR = 0, nDG = 0, nDB = 0;
				for(w=0;w<100;w++){
					int nPeriod,nStart,nType;
					if(!g_SFT.GetMW(w,nPeriod,nStart,nType))
						break;

					double col = (double)c*(double)1024/(double)nColors;
					double g = sin((pi*((int)col)) / nPeriod) / 2 + .5;
					if (nPeriod<0)
						g = -(double)nPeriod / (double)100;
					if (nType == 0){
						nH += g;
						nDR++;
					}
					if (nType == 1){
						nS += g;
						nDG++;
					}
					if (nType == 2){
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
				COLOR14 cPos;
				HSVToRGB(nH, nS, nB, cPos);
				g_SFT.SetKeyColor(cPos,c);
			}
			g_SFT.ApplyColors();
			InvalidateRect(GetDlgItem(hWnd,IDC_LIST1),NULL,FALSE);
			InvalidateRect(GetParent(hWnd),NULL,FALSE);
			InvalidateRect(hWnd,NULL,FALSE);
		}
		else if(wParam==IDC_BUTTON6)
		{
			int nParts = g_SFT.GetNumOfColors();
			int nPParts = nParts;
			int j=nParts-1;
			nParts*=2;
			if(nParts>1024)
				return ColorProc(hWnd,uMsg,IDC_BUTTON7,lParam);
			g_SFT.ChangeNumOfColors(nParts);
			int i;
			for(i=nParts-1;i>=0;i-=2){
				COLOR14 n;
				if(j>=nPParts-1)
					n=g_SFT.GetKeyColor(0);
				else
					n = g_SFT.GetKeyColor(j+1);
				COLOR14 c = g_SFT.GetKeyColor(j--);
				n.r = (n.r+c.r)/2;
				n.g = (n.g+c.g)/2;
				n.b = (n.b+c.b)/2;
				g_SFT.SetKeyColor(n,i);
				g_SFT.SetKeyColor(c,i-1);
			}
//			g_SFT.ChangeNumOfColors(nParts-1);
			SendMessage(hWnd,WM_USER+99,0,0);
			g_SFT.ApplyColors();
		}
		else if(wParam==IDC_BUTTON7){
			int i;
			g_SFT.ApplyColors();
			COLOR14 c[1024];
			for(i=0;i<1024;i++)
				c[i] = g_SFT.GetColor(i);
			g_SFT.ChangeNumOfColors(1024);
			for(i=0;i<1024;i++)
				g_SFT.SetKeyColor(c[i],i);
			SendMessage(hWnd,WM_USER+99,0,0);
			g_SFT.ApplyColors();
		}
		else if(wParam==IDC_BUTTON17){
			int i, nParts = g_SFT.GetNumOfColors();
			COLOR14 n;
			for(i=0;i<nParts;i++){
				n=g_SFT.GetKeyColor(i);
				int col;
				for(col=0;col<3;col++){
					unsigned char &c = (col==0?n.r:col==1?n.g:n.b);
					int diff = c - 128;
					if(diff>0){
						diff = 255-c;
						diff = 10*diff/100;
						c+=diff;
					}
					else{
						diff=c;
						diff = 10*diff/100;
						c-=diff;
					}
				}
				g_SFT.SetKeyColor(n,i);
			}
			SendMessage(hWnd,WM_USER+99,0,0);
			PostMessage(hWnd,WM_COMMAND,IDOK,0);
		}
		else if(wParam==IDC_BUTTON18){
			int i, nParts = g_SFT.GetNumOfColors();
			COLOR14 n;
			for(i=0;i<nParts;i++){
				n=g_SFT.GetKeyColor(i);
				int col;
				for(col=0;col<3;col++){
					unsigned char &c = (col==0?n.r:col==1?n.g:n.b);
					int diff = c - 128;
					if(diff>0){
						diff = c-128;
						diff = 10*diff/100;
						c-=diff;
					}
					else{
						diff=128-c;
						diff = 10*diff/100;
						c+=diff;
					}
				}
				g_SFT.SetKeyColor(n,i);
			}
			SendMessage(hWnd,WM_USER+99,0,0);
			PostMessage(hWnd,WM_COMMAND,IDOK,0);
		}
		else if(wParam==IDC_BUTTON22){
			char szFile[256]={0};
			if(!BrowseFile(hWnd,TRUE,"Open image","Image\0*.bmp;*.jp*g;*.gif\0\0",szFile,sizeof(szFile)))
				return 0;
			HBITMAP bmBmp = GetImage(szFile);
			HDC hDC = GetDC(NULL);
			BITMAPINFOHEADER bmi={sizeof(BITMAPINFOHEADER)};
			int row;
			GetDIBits(hDC,bmBmp,0,0,NULL,(LPBITMAPINFO)&bmi,DIB_RGB_COLORS);
			bmi.biCompression=bmi.biClrUsed=bmi.biClrImportant=0;
			bmi.biBitCount = 24;
			row = ((((bmi.biWidth*(DWORD)bmi.biBitCount)+31)&~31) >> 3);
			bmi.biSizeImage=row*bmi.biHeight;
			BYTE *lpBits = new BYTE[bmi.biSizeImage];
			GetDIBits(hDC,bmBmp,0,bmi.biHeight,lpBits,
					(LPBITMAPINFO)&bmi,DIB_RGB_COLORS);

			double woffs = (double)bmi.biWidth/(double)1024;
			int nParts = bmi.biWidth;
			if(nParts>1024)
				nParts=1024;
			else
				woffs=1;
			double hoffs = (double)bmi.biHeight/(double)nParts;
			g_SFT.ChangeNumOfColors(nParts);
			int i;
			for(i=0;i<nParts;i++){
				int x = i*woffs;
				int y = i*hoffs;
				if(x>=bmi.biWidth)
					x=bmi.biWidth;
				if(y>=bmi.biHeight)
					y=bmi.biHeight;
				int nIndex = x*3 + (bmi.biHeight-1-y)*row;
				COLOR14 c = g_SFT.GetKeyColor(i);
				c.r = lpBits[nIndex];
				c.g = lpBits[nIndex+1];
				c.b = lpBits[nIndex+2];
				g_SFT.SetKeyColor(c,i);
			}
			ReleaseDC(NULL,hDC);
			DeleteObject(bmBmp);
			delete[] lpBits;
			if(nParts!=SendDlgItemMessage(hWnd,IDC_LIST1,LB_GETCOUNT,0,0)){
				SendDlgItemMessage(hWnd,IDC_LIST1,LB_RESETCONTENT,0,0);
				int i;
				for(i=0;i<nParts;i++)
					SendDlgItemMessage(hWnd,IDC_LIST1,LB_ADDSTRING,0,(LPARAM)"");
			}
			InvalidateRect(GetDlgItem(hWnd,IDC_LIST1),NULL,FALSE);
		}
		else if(wParam==IDC_CHECK1){
			if(SendDlgItemMessage(hWnd,IDC_CHECK1,BM_GETCHECK,0,0)){
				SetCapture(hWnd);
				SetTimer(hWnd,0,200,NULL);
			}
			else{
				ReleaseCapture();
				KillTimer(hWnd,0);
			}
		}
		else if(wParam==IDC_BUTTON5){
			char szFile[256]={0};
			if(BrowseFile(hWnd,TRUE,"Open palette","Palette\0*.kfp\0\0",szFile,sizeof(szFile))){
				DWORD dw;
				HANDLE hFile = CreateFile(szFile,GENERIC_READ,0,NULL,OPEN_EXISTING,0,NULL);
				if(hFile==INVALID_HANDLE_VALUE)
					return FALSE;
				int nData = GetFileSize(hFile,NULL);
				char *szData = new char[nData+1];
				ReadFile(hFile,szData,nData,&dw,NULL);
				CloseHandle(hFile);
				szData[nData]=0;
				CStringTable stParams(szData,": ","\r\n");
				delete [] szData;
				int nC = stParams.FindString(0,"Colors");
				if(nC==-1)
					return MessageBox(hWnd,"Invalid file","Error",MB_OK|MB_ICONSTOP);
				CStringTable stColors(stParams[nC][1],"",",");
				int nParts = stColors.GetCount()/3;
				g_SFT.ChangeNumOfColors(nParts);
				int i;
				COLOR14 c;
				for(i=0;i<nParts;i++){
					c.r = atoi(stColors[i*3][0]);
					c.g = atoi(stColors[i*3+1][0]);
					c.b = atoi(stColors[i*3+2][0]);
					g_SFT.SetKeyColor(c,i);
				}
				int nID = stParams.FindString(0,"IterDiv");
				double nDiv=1;
				if(nID!=-1)
					nDiv = atof(stParams[nID][1]);
				if(nDiv==0)
					nDiv=1;
				g_SFT.SetIterDiv(nDiv);
				nID = stParams.FindString(0,"ColorMethod");
				if(nID!=-1){
					nID = atoi(stParams[nID][1]);
					g_SFT.SetColorMethod(nID);
					SendDlgItemMessage(hWnd,IDC_COMBO1,CB_SETCURSEL,nID,0);
				}
				nID = stParams.FindString(0,"Differences");
				if(nID!=-1){
					nID = atoi(stParams[nID][1]);
					g_SFT.SetDifferences(nID);
					SendDlgItemMessage(hWnd,IDC_DIFFERENCES,CB_SETCURSEL,nID,0);
				}
				SendMessage(hWnd,WM_USER+99,0,0);
				g_SFT.ApplyColors();
				PostMessage(hWnd,WM_COMMAND,IDOK,0);
			}
		}
		else if(wParam==IDC_BUTTON2){
			int nColors = GetDlgItemInt(hWnd,IDC_EDIT1,NULL,0);
			if(nColors>1024){
				nColors=1024;
				SetDlgItemInt(hWnd,IDC_EDIT1,nColors,FALSE);
			}
			int nSeed = GetDlgItemInt(hWnd,IDC_EDIT2,NULL,0);
			g_SFT.GenerateColors(nColors,nSeed);
			g_SFT.ApplyColors();
			InvalidateRect(GetDlgItem(hWnd,IDC_LIST1),NULL,FALSE);
			InvalidateRect(GetParent(hWnd),NULL,FALSE);
			if(nColors!=SendDlgItemMessage(hWnd,IDC_LIST1,LB_GETCOUNT,0,0)){
				SendDlgItemMessage(hWnd,IDC_LIST1,LB_RESETCONTENT,0,0);
				int i;
				for(i=0;i<nColors;i++)
					SendDlgItemMessage(hWnd,IDC_LIST1,LB_ADDSTRING,0,(LPARAM)"");
			}
			InvalidateRect(hWnd,NULL,FALSE);
		}
		else if(wParam==IDC_BUTTON12 || wParam==IDC_BUTTON14){
			if(wParam==IDC_BUTTON14){
				int nCols = g_SFT.GetNumOfColors()/2;
				srand(GetTickCount());
				int nVal = rand()%nCols;
				if(nVal==0)
					nVal=1;
				SetDlgItemInt(hWnd,IDC_EDIT11,nVal = MakePrime(nVal),FALSE);
				SetDlgItemInt(hWnd,IDC_EDIT13,rand()%nVal,FALSE);
				nVal = rand()%nCols;
				if(nVal==0)
					nVal=1;
				SetDlgItemInt(hWnd,IDC_EDIT14,nVal = MakePrime(nVal),FALSE);
				SetDlgItemInt(hWnd,IDC_EDIT15,rand()%nVal,FALSE);
				nVal = rand()%nCols;
				if(nVal==0)
					nVal=1;
				SetDlgItemInt(hWnd,IDC_EDIT16,nVal = MakePrime(nVal),FALSE);
				SetDlgItemInt(hWnd,IDC_EDIT17,rand()%nVal,FALSE);
				nVal = rand()%nCols;
				if(nVal==0)
					nVal=1;
				SetDlgItemInt(hWnd,IDC_EDIT18,nVal = MakePrime(nVal),FALSE);
				SetDlgItemInt(hWnd,IDC_EDIT19,rand()%nVal,FALSE);
			}
			int nPeriod = GetDlgItemInt(hWnd,IDC_EDIT11,NULL,FALSE);
			int nStart = GetDlgItemInt(hWnd,IDC_EDIT13,NULL,FALSE);
			g_SFT.AddWave(2,nPeriod,nStart);
			nPeriod = GetDlgItemInt(hWnd,IDC_EDIT14,NULL,FALSE);
			nStart = GetDlgItemInt(hWnd,IDC_EDIT15,NULL,FALSE);
			g_SFT.AddWave(1,nPeriod,nStart);
			nPeriod = GetDlgItemInt(hWnd,IDC_EDIT16,NULL,FALSE);
			nStart = GetDlgItemInt(hWnd,IDC_EDIT17,NULL,FALSE);
			g_SFT.AddWave(0,nPeriod,nStart);
			nPeriod = GetDlgItemInt(hWnd,IDC_EDIT18,NULL,FALSE);
			nStart = GetDlgItemInt(hWnd,IDC_EDIT19,NULL,FALSE);
			g_SFT.AddWave(3,nPeriod,nStart);

			g_SFT.ApplyColors();
			InvalidateRect(GetDlgItem(hWnd,IDC_LIST1),NULL,FALSE);
			InvalidateRect(GetParent(hWnd),NULL,FALSE);
			InvalidateRect(hWnd,NULL,FALSE);
		}
		else if(wParam==IDC_BUTTON16){
			int nPeriod = MakePrime(GetDlgItemInt(hWnd,IDC_EDIT11,NULL,FALSE));
			SetDlgItemInt(hWnd,IDC_EDIT11,nPeriod,FALSE);
		}
		else if(wParam==IDC_BUTTON19){
			int nPeriod = MakePrime(GetDlgItemInt(hWnd,IDC_EDIT14,NULL,FALSE));
			SetDlgItemInt(hWnd,IDC_EDIT14,nPeriod,FALSE);
		}
		else if(wParam==IDC_BUTTON20){
			int nPeriod = MakePrime(GetDlgItemInt(hWnd,IDC_EDIT16,NULL,FALSE));
			SetDlgItemInt(hWnd,IDC_EDIT16,nPeriod,FALSE);
		}
		else if(wParam==IDC_BUTTON21){
			int nPeriod = MakePrime(GetDlgItemInt(hWnd,IDC_EDIT18,NULL,FALSE));
			SetDlgItemInt(hWnd,IDC_EDIT18,nPeriod,FALSE);
		}
		else if(HIWORD(wParam)==LBN_DBLCLK && LOWORD(wParam)==IDC_LIST1){
			int i = SendDlgItemMessage(hWnd,IDC_LIST1,LB_GETCURSEL,0,0);
			if(i==-1)
				return 0;
			CHOOSECOLOR col={sizeof(CHOOSECOLOR)};
			col.hwndOwner = hWnd;
			col.lpCustColors = colCust;
			col.Flags = CC_RGBINIT;
			COLOR14 c = g_SFT.GetKeyColor(i);
			col.rgbResult = RGB(c.b,c.g,c.r);
			if(ChooseColor(&col)){
				char *cc = (char*)&col.rgbResult;
				c.b = cc[0];
				c.g = cc[1];
				c.r = cc[2];
				g_SFT.SetKeyColor(c,i);
				InvalidateRect(GetDlgItem(hWnd,IDC_LIST1),NULL,FALSE);
			}
		}
		else if(wParam==IDC_BUTTON8){
			COLOR14 c;
			CHOOSECOLOR col={sizeof(CHOOSECOLOR)};
			col.hwndOwner = hWnd;
			col.lpCustColors = colCust;
			col.Flags = CC_RGBINIT;
			int nStep = GetDlgItemInt(hWnd,IDC_EDIT9,NULL,FALSE);
			if(nStep==0)
				nStep =1;
			int nPercent = GetDlgItemInt(hWnd,IDC_EDIT10,NULL,FALSE);
			if(nPercent>100)
				nPercent=100;
			if(ChooseColor(&col)){
				char *cc = (char*)&col.rgbResult;
				c.b = cc[0];
				c.g = cc[1];
				c.r = cc[2];
				int i = SendDlgItemMessage(hWnd,IDC_LIST1,LB_GETCURSEL,0,0);
				if(i==-1)
					i=0;
				if(SendDlgItemMessage(hWnd,IDC_RADIO4,BM_GETCHECK,0,0)){
					for(;i<g_SFT.GetNumOfColors();i+=nStep){
						COLOR14 n = g_SFT.GetKeyColor(i);
						n.r = ((100-nPercent)*n.r+nPercent*c.r)/100;
						n.g = ((100-nPercent)*n.g+nPercent*c.g)/100;
						n.b = ((100-nPercent)*n.b+nPercent*c.b)/100;
						g_SFT.SetKeyColor(n,i);
					}
				}
				else{
					for(;i<g_SFT.GetNumOfColors();i++){
						double g = sin((pi*i*2)/nStep)/2+.5;
						g = (double)nPercent*g/(double)100;
						COLOR14 n = g_SFT.GetKeyColor(i);
						n.r = ((1-g)*n.r+g*c.r);
						n.g = ((1-g)*n.g+g*c.g);
						n.b = ((1-g)*n.b+g*c.b);
						g_SFT.SetKeyColor(n,i);
					}
				}
			}
			SendMessage(hWnd,WM_USER+99,0,0);
			g_SFT.ApplyColors();
		}
		else if(wParam==IDC_BUTTON9){
			COLOR14 c;
			CHOOSECOLOR col={sizeof(CHOOSECOLOR)};
			col.hwndOwner = hWnd;
			col.lpCustColors = colCust;
			col.Flags = CC_RGBINIT;
			if(ChooseColor(&col)){
				char *cc = (char*)&col.rgbResult;
				c.b = cc[0];
				c.g = cc[1];
				c.r = cc[2];
				int i = SendDlgItemMessage(hWnd,IDC_LIST1,LB_GETCURSEL,0,0);
				if(i==-1)
					i=0;
				for(;i<g_SFT.GetNumOfColors();i+=3){
					COLOR14 n = g_SFT.GetKeyColor(i);
					n.r = (n.r+c.r)/2;
					n.g = (n.g+c.g)/2;
					n.b = (n.b+c.b)/2;
					g_SFT.SetKeyColor(n,i);
				}
			}
			SendMessage(hWnd,WM_USER+99,0,0);
			g_SFT.ApplyColors();
		}
		else if(wParam==IDC_BUTTON11){
			COLOR14 c;
			CHOOSECOLOR col={sizeof(CHOOSECOLOR)};
			col.hwndOwner = hWnd;
			col.lpCustColors = colCust;
			col.Flags = CC_RGBINIT;
			if(ChooseColor(&col)){
				char *cc = (char*)&col.rgbResult;
				c.b = cc[0];
				c.g = cc[1];
				c.r = cc[2];
				int i = SendDlgItemMessage(hWnd,IDC_LIST1,LB_GETCURSEL,0,0);
				if(i==-1)
					i=0;
				for(;i<g_SFT.GetNumOfColors();i+=4){
					COLOR14 n = g_SFT.GetKeyColor(i);
					n.r = (n.r+c.r)/2;
					n.g = (n.g+c.g)/2;
					n.b = (n.b+c.b)/2;
					g_SFT.SetKeyColor(n,i);
				}
			}
			SendMessage(hWnd,WM_USER+99,0,0);
			g_SFT.ApplyColors();
		}
		else if(wParam==IDC_BUTTON10){
			int nParts = g_SFT.GetNumOfColors();
			int nPParts = nParts;
			nParts*=2;
			if(nParts>1024)
				nParts=1024;
			g_SFT.ChangeNumOfColors(nParts);
			int i, j=0;
			for(i=nPParts;i<nParts;i++){
				COLOR14 c = g_SFT.GetKeyColor(j++);
				g_SFT.SetKeyColor(c,i);
			}
			SendMessage(hWnd,WM_USER+99,0,0);
			g_SFT.ApplyColors();
		}
		else if(wParam==IDC_BUTTON13){
			srand(GetTickCount());
			int nColors = GetDlgItemInt(hWnd,IDC_EDIT1,NULL,0);
			if(nColors>1024){
				nColors=1024;
				SetDlgItemInt(hWnd,IDC_EDIT1,nColors,FALSE);
			}
			int nSeed = rand();
			SetDlgItemInt(hWnd,IDC_EDIT2,nSeed,0);
			g_SFT.GenerateColors(nColors,nSeed);
			g_SFT.ApplyColors();
			InvalidateRect(GetDlgItem(hWnd,IDC_LIST1),NULL,FALSE);
			InvalidateRect(GetParent(hWnd),NULL,FALSE);
			if(nColors!=SendDlgItemMessage(hWnd,IDC_LIST1,LB_GETCOUNT,0,0)){
				SendDlgItemMessage(hWnd,IDC_LIST1,LB_RESETCONTENT,0,0);
				int i;
				for(i=0;i<nColors;i++)
					SendDlgItemMessage(hWnd,IDC_LIST1,LB_ADDSTRING,0,(LPARAM)"");
			}
			InvalidateRect(hWnd,NULL,FALSE);
		}
	}
	else if(uMsg==WM_USER+88){
		POINT p;
		GetCursorPos(&p);
		ScreenToClient(GetDlgItem(hWnd,IDC_LIST1),&p);
		int i = SendDlgItemMessage(hWnd,IDC_LIST1,LB_ITEMFROMPOINT,0,MAKELONG(p.x,p.y));
		SendDlgItemMessage(hWnd,IDC_LIST1,LB_SETCURSEL,i,0);
		HMENU hMen = CreatePopupMenu();
		AppendMenu(hMen,MF_STRING,1,"Copy color");
		AppendMenu(hMen,MF_STRING,2,"Paste color");
		AppendMenu(hMen,MF_SEPARATOR,0,"");
		AppendMenu(hMen,MF_STRING,3,"Paste every second");
		AppendMenu(hMen,MF_STRING,4,"Paste every third");
		AppendMenu(hMen,MF_STRING,5,"Paste every forth");
		AppendMenu(hMen,MF_STRING,6,"Paste every fifth");
		AppendMenu(hMen,MF_STRING,7,"Paste every sixth");
		AppendMenu(hMen,MF_STRING,8,"Paste every seventh");
		AppendMenu(hMen,MF_STRING,9,"Paste every eighth");
		AppendMenu(hMen,MF_SEPARATOR,0,"");
		AppendMenu(hMen,MF_STRING,10,"Capture from mouse");
		GetCursorPos(&p);
		int rc = TrackPopupMenu(hMen,TPM_LEFTALIGN|TPM_RETURNCMD,p.x,p.y,0,hWnd,NULL);
		DestroyMenu(hMen);
		if(rc==0)
			return 0;
		if(rc==1)
			g_colCopy = g_SFT.GetKeyColor(i);
		else if(rc==2){
			g_SFT.SetKeyColor(g_colCopy,i);
			InvalidateRect(GetDlgItem(hWnd,IDC_LIST1),NULL,FALSE);
		}
		else if(rc==3){
			do{
				g_SFT.SetKeyColor(g_colCopy,i);
				i+=2;
			}while(i<g_SFT.GetNumOfColors());
			InvalidateRect(GetDlgItem(hWnd,IDC_LIST1),NULL,FALSE);
		}
		else if(rc==4){
			do{
				g_SFT.SetKeyColor(g_colCopy,i);
				i+=3;
			}while(i<g_SFT.GetNumOfColors());
			InvalidateRect(GetDlgItem(hWnd,IDC_LIST1),NULL,FALSE);
		}
		else if(rc==5){
			do{
				g_SFT.SetKeyColor(g_colCopy,i);
				i+=4;
			}while(i<g_SFT.GetNumOfColors());
			InvalidateRect(GetDlgItem(hWnd,IDC_LIST1),NULL,FALSE);
		}
		else if(rc==6){
			do{
				g_SFT.SetKeyColor(g_colCopy,i);
				i+=5;
			}while(i<g_SFT.GetNumOfColors());
			InvalidateRect(GetDlgItem(hWnd,IDC_LIST1),NULL,FALSE);
		}
		else if(rc==7){
			do{
				g_SFT.SetKeyColor(g_colCopy,i);
				i+=6;
			}while(i<g_SFT.GetNumOfColors());
			InvalidateRect(GetDlgItem(hWnd,IDC_LIST1),NULL,FALSE);
		}
		else if(rc==8){
			do{
				g_SFT.SetKeyColor(g_colCopy,i);
				i+=7;
			}while(i<g_SFT.GetNumOfColors());
			InvalidateRect(GetDlgItem(hWnd,IDC_LIST1),NULL,FALSE);
		}
		else if(rc==9){
			do{
				g_SFT.SetKeyColor(g_colCopy,i);
				i+=8;
			}while(i<g_SFT.GetNumOfColors());
			InvalidateRect(GetDlgItem(hWnd,IDC_LIST1),NULL,FALSE);
		}
		else if(rc==10){
			SetCapture(hWnd);
			g_bCaptureMouse=TRUE;
		}
	}
	else if(uMsg==WM_DRAWITEM){
		LPDRAWITEMSTRUCT lpdis = (LPDRAWITEMSTRUCT)lParam;
		COLOR14 c = g_SFT.GetKeyColor(lpdis->itemID);
		HBRUSH br = CreateSolidBrush(RGB(c.b,c.g,c.r));
		FillRect(lpdis->hDC,&lpdis->rcItem,br);
		DeleteObject(br);
		if(lpdis->itemState & ODS_SELECTED){
			SkuggadRect(lpdis->hDC,lpdis->rcItem,FALSE,0,0);
			lpdis->rcItem.left++;
			lpdis->rcItem.top++;
			lpdis->rcItem.right--;
			lpdis->rcItem.bottom--;
			SkuggadRect(lpdis->hDC,lpdis->rcItem,FALSE,TRUE,0);
		}
	}
	else if(uMsg==WM_TIMER){
		POINT p;
		GetCursorPos(&p);
		ScreenToClient(GetParent(hWnd),&p);
		RECT rc;
		GetClientRect(GetParent(hWnd),&rc);
		RECT sr;
		GetWindowRect(g_hwStatus,&sr);
		sr.bottom-=sr.top;
		rc.bottom-=sr.bottom;
		int x = p.x*g_SFT.GetWidth()/rc.right;
		int y = p.y*g_SFT.GetHeight()/rc.bottom;
		int i = g_SFT.GetColorIndex(x,y);
		i/=(1024/g_SFT.GetNumOfColors());
		SendDlgItemMessage(hWnd,IDC_LIST1,LB_SETCURSEL,i,0);
	}
	else if(uMsg==WM_CAPTURECHANGED){
		g_bCaptureMouse=FALSE;
		SendDlgItemMessage(hWnd,IDC_CHECK1,BM_SETCHECK,0,0);
		KillTimer(hWnd,0);
	}
	else if(uMsg==WM_LBUTTONDOWN && SendDlgItemMessage(hWnd,IDC_CHECK1,BM_GETCHECK,0,0)){
		POINT p;
		GetCursorPos(&p);
		RECT rC;
		GetWindowRect(hWnd,&rC);
		if(p.x>=rC.left && p.x<=rC.right && p.y>=rC.top && p.y<=rC.bottom){
			KillTimer(hWnd,0);
			ReleaseCapture();
			return 0;
		}
		ScreenToClient(GetParent(hWnd),&p);

		RECT rc;
		GetClientRect(GetParent(hWnd),&rc);
		RECT sr;
		GetWindowRect(g_hwStatus,&sr);
		sr.bottom-=sr.top;
		rc.bottom-=sr.bottom;
		int x = p.x*g_SFT.GetWidth()/rc.right;
		int y = p.y*g_SFT.GetHeight()/rc.bottom;

		int i = g_SFT.GetColorIndex(x,y);
		i/=(1024/g_SFT.GetNumOfColors());
		SendDlgItemMessage(hWnd,IDC_LIST1,LB_SETCURSEL,i,0);
		ReleaseCapture();
		SendDlgItemMessage(hWnd,IDC_CHECK1,BM_SETCHECK,0,0);
		KillTimer(hWnd,0);
		if(i!=-1)
			SendMessage(hWnd,WM_COMMAND,MAKELONG(IDC_LIST1,LBN_DBLCLK),(LPARAM)GetDlgItem(hWnd,IDC_LIST1));
	}
	else if(uMsg==WM_LBUTTONDOWN && g_bCaptureMouse){
		g_bCaptureMouse=FALSE;
		ReleaseCapture();
		POINT p;
		GetCursorPos(&p);
		HDC hDC = GetDC(NULL);
		COLORREF col = GetPixel(hDC,p.x,p.y);
		ReleaseDC(NULL,hDC);

		int i = SendDlgItemMessage(hWnd,IDC_LIST1,LB_GETCURSEL,0,0);
		if(i==-1)
			return 0;
		COLOR14 c = g_SFT.GetKeyColor(i);
		char *cc = (char*)&col;
		c.b = cc[0];
		c.g = cc[1];
		c.r = cc[2];
		g_SFT.SetKeyColor(c,i);
		InvalidateRect(GetDlgItem(hWnd,IDC_LIST1),NULL,FALSE);
	}
	if(g_pWaves)
		g_pWaves->ProcessMessage(hWnd,uMsg,wParam,lParam);
	if(uMsg==WM_COMMAND && (wParam==IDC_BUTTON26 || wParam==IDC_BUTTON27 || wParam==IDC_BUTTON28))
		PostMessage(hWnd,WM_COMMAND,IDOK,0);
	return 0;
}
char *Trim(char *sz,int nLen=-1)
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
int WINAPI CrossHairProc(HWND hWnd,UINT uMsg,WPARAM wParam,LPARAM lParam)
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
void FixNumber(char *sz)
{
	int a, b, p, e, m;
	for(a=0;sz[a]==' ' || sz[a]=='\t' || sz[a]=='\r' || sz[a]=='\n';a++);
	for(b=p=e=m=0;sz[a];a++){
		if(isdigit(sz[a]) || (!e && (sz[a]=='e' || sz[a]=='E')) || (!p && sz[a]=='.') || (!m && sz[a]=='-'))
			sz[b++]=sz[a];
		if(!e && (sz[a]=='e' || sz[a]=='E'))
			e=1;
		if(!p && sz[a]=='.')
			p=1;
		if(!m && sz[a]=='-')
			m=1;
	}
	sz[b]=0;
}
int WINAPI PositionProc(HWND hWnd,UINT uMsg,WPARAM wParam,LPARAM lParam)
{
	(void) lParam;
	if(uMsg==WM_INITDIALOG){
		SendMessage(hWnd, WM_SETICON, ICON_SMALL, LPARAM(g_hIcon));
		SendMessage(hWnd, WM_SETICON, ICON_BIG, LPARAM(g_hIcon));
		InitToolTip(hWnd,GetModuleHandle(NULL),GetToolText,2);
		SendDlgItemMessage(hWnd,IDC_EDIT1,EM_SETLIMITTEXT,0,0);
		SendDlgItemMessage(hWnd,IDC_EDIT3,EM_SETLIMITTEXT,0,0);
		SetDlgItemText(hWnd,IDC_EDIT1,g_SFT.GetRe());
		SetDlgItemText(hWnd,IDC_EDIT3,g_SFT.GetIm());
		SetDlgItemText(hWnd,IDC_EDIT4,g_SFT.GetZoom());
		int nMin, nMax;
		g_SFT.GetIterations(nMin,nMax);
		SetDlgItemInt(hWnd,IDC_EDIT2,nMin,FALSE);
		SetDlgItemInt(hWnd,IDC_EDIT5,nMax,FALSE);
		return 1;
	}
	else if(uMsg==WM_COMMAND){
		if(wParam==IDOK){
			int n = GetWindowTextLength(GetDlgItem(hWnd,IDC_EDIT1));
			char *szR = new char[n+1];
			GetDlgItemText(hWnd,IDC_EDIT1,szR,n+1);
			FixNumber(szR);
			n = GetWindowTextLength(GetDlgItem(hWnd,IDC_EDIT3));
			char *szI = new char[n+1];
			GetDlgItemText(hWnd,IDC_EDIT3,szI,n+1);
			FixNumber(szI);
			n = GetWindowTextLength(GetDlgItem(hWnd,IDC_EDIT4));
			char *szZ = new char[n+1];
			GetDlgItemText(hWnd,IDC_EDIT4,szZ,n+1);
			FixNumber(szZ);
			g_SFT.SetPosition(szR,szI,szZ);
			delete [] szR;
			delete [] szI;
			delete [] szZ;
			ExitToolTip(hWnd);
			EndDialog(hWnd,1);
		}
		else if(wParam==IDCANCEL){
			ExitToolTip(hWnd);
			EndDialog(hWnd,0);
		}
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
int WINAPI JpegProc(HWND hWnd,UINT uMsg,WPARAM wParam,LPARAM lParam)
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
int FileExists(char *szFind)
{
	WIN32_FIND_DATA wf;
	HANDLE hFind = FindFirstFile(szFind,&wf);
	if(hFind==INVALID_HANDLE_VALUE)
		return 0;
	FindClose(hFind);
	return 1;
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
BOOL g_bAnim=FALSE;
void UpdateBkpImage(ANIM *pAnim)
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
int WINAPI ThAnim_(ANIM *pAnim)
{
	int nParts = 10;// * log((double)g_SFT.GetZoomSize())/log((double)2);
	g_bAnim=TRUE;
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
		g_bAnim=FALSE;
	SelectObject(dcBmp,bmOld);
	DeleteDC(dcBmp);
	ReleaseDC(pAnim->hWnd,hDC);
	DeleteObject(pAnim->bmBmp);
	delete pAnim;
	return 0;
}
int WINAPI ThAnim(ANIM *pAnim)
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
int g_nPrevAutoGlitchNP;
int g_bAutoSolveGlitch=0;
int g_nAutoSolveGlitchLimit=10;
int WINAPI ExamineProc(HWND hWnd,UINT uMsg,WPARAM wParam,LPARAM lParam)
{
	(void) lParam;
	if(uMsg==WM_INITDIALOG){
		SendMessage(hWnd, WM_SETICON, ICON_SMALL, LPARAM(g_hIcon));
		SendMessage(hWnd, WM_SETICON, ICON_BIG, LPARAM(g_hIcon));
		InitToolTip(hWnd,GetModuleHandle(NULL),GetToolText,4);
		g_nPrevAutoGlitchNP=g_SFT.GetSolveGlitchNear();
		g_SFT.SetSolveGlitchNear(false);
		CheckMenuItem(GetMenu(GetParent(hWnd)),ID_ACTIONS_SPECIAL_SOLVEGLITCHWITHNEARPIXELSMETHOD,MF_BYCOMMAND|(g_SFT.GetSolveGlitchNear()?MF_CHECKED:MF_UNCHECKED));
		g_bAddReference=TRUE;
		g_bEraser=FALSE;
		g_stExamine.Reset();
		SendDlgItemMessage(hWnd,IDC_RADIO1,BM_SETCHECK,1,0);
		WIN32_FIND_DATA fd;
		char szExamine[256];
		strcpy(szExamine,g_szExamine);
		char *sz = strrchr(szExamine,'\\');
		if(sz)
			strcpy(sz+1,"*_*.kfb");
		HANDLE hFind = FindFirstFile(szExamine,&fd);
		if(!sz || hFind==INVALID_HANDLE_VALUE){
			if(hFind)
				FindClose(hFind);
			MessageBox(hWnd,"Could not browse kfb files","Error",MB_OK|MB_ICONSTOP);
			DestroyWindow(hWnd);
			g_hwExamine=NULL;
			return 0;
		}
		do{
			strcpy(strrchr(szExamine,'\\')+1,fd.cFileName);
			g_stExamine.AddRow();
			g_stExamine.AddString(g_stExamine.GetCount()-1,szExamine);
		}while(FindNextFile(hFind,&fd));
		FindClose(hFind);
		g_stExamine.M3QSort(0,1);
		g_SFT.OpenFile(g_szExamine);
		g_SFT.OpenMapB(g_stExamine[0][0]);
		g_SFT.ApplyColors();
		g_nExamine=0;
		SetDlgItemInt(hWnd,IDC_EDIT1,g_nExamine,FALSE);
		g_bExamineDirty=FALSE;
		int last = g_stExamine.GetCount() - 1;
		char *szA = strrchr(g_stExamine[last][0],'_');
		szA++;
		strcpy(szExamine,szA);
		*strrchr(szExamine,'.')=0;
		CDecNumber A(szExamine);
		szA = strrchr(g_stExamine[last-1][0],'_');
		if (szA)
		{
			szA++;
			strcpy(szExamine,szA);
			*strrchr(szExamine,'.')=0;
		}
		CDecNumber B(szExamine);
		g_nExamineZoom = (A/B+CDecNumber(0.5)).ToInt();
		A = CDecNumber(g_SFT.GetZoom())/(CDecNumber(g_nExamineZoom)^(g_stExamine.GetCount()-g_nExamine-1));
		char *szR = g_SFT.GetRe();
		char *szRe = new char[strlen(szR)+1];
		strcpy(szRe,szR);
		char *szI = g_SFT.GetIm();
		char *szIm = new char[strlen(szI)+1];
		strcpy(szIm,szI);
		g_SFT.SetPosition(szRe,szIm,A.ToText());
		delete[] szRe;
		delete[] szIm;

		PostMessage(GetParent(hWnd),WM_USER+199,0,0);
		SetTimer(hWnd,0,100,NULL);
	}
	else if(uMsg==WM_COMMAND){
		if(wParam==IDCANCEL){
			DestroyWindow(hWnd);
			g_hwExamine=NULL;
			g_SFT.SetSolveGlitchNear(g_nPrevAutoGlitchNP);
			CheckMenuItem(GetMenu(GetParent(hWnd)),ID_ACTIONS_SPECIAL_SOLVEGLITCHWITHNEARPIXELSMETHOD,MF_BYCOMMAND|(g_SFT.GetSolveGlitchNear()?MF_CHECKED:MF_UNCHECKED));
		}
		else if(wParam==IDOK || wParam==IDC_BUTTON1 || wParam==IDC_BUTTON2 || wParam==IDC_BUTTON5){
			SetDlgItemText(hWnd,IDC_EDIT3,"Reading...");
			g_SFT.Stop();
			g_bAnim=FALSE;
			KillTimer(hWnd,1);
			if(wParam!=IDC_BUTTON2 && g_bExamineDirty){
				g_SFT.SaveMapB(g_stExamine[g_nExamine][0]);
				char szFile[256];
				strcpy(szFile,g_stExamine[g_nExamine][0]);
				strcpy(strrchr(szFile,'.'),".jpg");
				if(FileExists(szFile))
					g_SFT.SaveJpg(szFile,100);
			}
			if(wParam==IDC_BUTTON5)
				g_bExamineDirty=TRUE;
			else
				g_bExamineDirty=FALSE;
			if(wParam==IDOK){
				g_nExamine++;
				if(g_nExamine>g_stExamine.GetCount()-1)
					g_nExamine=0;
			}
			else if(wParam==IDC_BUTTON1 || wParam==IDC_BUTTON5){
				g_nExamine--;
				if(g_nExamine<0){
					wParam=IDC_BUTTON1;
					g_nExamine=g_stExamine.GetCount()-1;
					g_bExamineDirty=FALSE;
				}
			}
			SetDlgItemInt(hWnd,IDC_EDIT1,g_nExamine,FALSE);
			UpdateWindow(GetDlgItem(hWnd,IDC_EDIT1));
			g_SFT.OpenMapB(g_stExamine[g_nExamine][0],wParam==IDC_BUTTON5,(double)1/g_nExamineZoom);
			g_SFT.ApplyColors();
			if(wParam!=IDC_BUTTON2)
				SetTimer(hWnd,1,500,NULL);
			InvalidateRect(GetParent(hWnd),NULL,FALSE);
			UpdateWindow(GetParent(hWnd));
//			SetCapture(hWnd);
			g_bWaitRead=TRUE;
		}
		else if(wParam==IDC_BUTTON3){
			SetDlgItemText(hWnd,IDC_EDIT3,"Reading...");
			g_SFT.Stop();
			g_bAnim=FALSE;
			KillTimer(hWnd,1);
			g_nExamine = GetDlgItemInt(hWnd,IDC_EDIT1,NULL,FALSE);
			if(g_nExamine<0)
				g_nExamine=0;
			else if(g_nExamine>=g_stExamine.GetCount())
				g_nExamine = g_stExamine.GetCount()-1;
			g_SFT.OpenMapB(g_stExamine[g_nExamine][0],wParam==IDC_BUTTON5,(double)1/g_nExamineZoom);
			g_SFT.ApplyColors();
			SetTimer(hWnd,1,500,NULL);
			InvalidateRect(GetParent(hWnd),NULL,FALSE);
			UpdateWindow(GetParent(hWnd));
			g_bWaitRead=TRUE;
		}
		else if(wParam==IDC_BUTTON4){
			if(!g_SFT.GetAutoSolveGlitches())
				SetTimer(hWnd,2,10,NULL);
			g_SFT.SetAutoSolveGlitches(! g_SFT.GetAutoSolveGlitches());
			if(g_SFT.GetAutoSolveGlitches())
			{
				// update additional references limit from GUI
				g_nAutoSolveGlitchLimit = kGetDlgItemInt(hWnd, IDC_EDITMAXREFS);
				if (g_nAutoSolveGlitchLimit < 1) g_nAutoSolveGlitchLimit = 10;
				kSetDlgItemInt(hWnd, IDC_EDITMAXREFS, g_nAutoSolveGlitchLimit);
				SetDlgItemText(hWnd,IDC_BUTTON4,"Stop Auto solve glitch");
			}
			else{
				SetDlgItemText(hWnd,IDC_EDIT4,"");
				SetDlgItemText(hWnd,IDC_BUTTON4,"Auto solve glitch");
			}
		}
		else if(wParam==IDC_RADIO1){
			g_bAddReference=TRUE;
			g_bAddMainReference=FALSE;
			g_bEraser=FALSE;
			EnableWindow(GetDlgItem(hWnd,IDC_CHECK1),TRUE);
		}
		else if(wParam==IDC_RADIO2){
			g_bAddReference=FALSE;
			g_bAddMainReference=TRUE;
			g_bEraser=FALSE;
			EnableWindow(GetDlgItem(hWnd,IDC_CHECK1),FALSE);
		}
		else if(wParam==IDC_RADIO3){
			g_bEraser=TRUE;
			g_bAddReference=FALSE;
			g_bAddMainReference=FALSE;
			EnableWindow(GetDlgItem(hWnd,IDC_CHECK1),FALSE);
		}
		else if(wParam==IDC_CHECK1){
			g_SFT.SetSolveGlitchNear(! g_SFT.GetSolveGlitchNear());
			CheckMenuItem(GetMenu(GetParent(hWnd)),ID_ACTIONS_SPECIAL_SOLVEGLITCHWITHNEARPIXELSMETHOD,MF_BYCOMMAND|(g_SFT.GetSolveGlitchNear()?MF_CHECKED:MF_UNCHECKED));
		}
	}
	else if(uMsg==WM_TIMER && wParam==0)
		SetDlgItemText(hWnd,IDC_EDIT2,g_bExamineDirty?"Changed":"");
	else if(uMsg==WM_TIMER && wParam==1){
		g_bWaitRead=FALSE;
		SetDlgItemText(hWnd,IDC_EDIT3,"");
		KillTimer(hWnd,1);
		ReleaseCapture();
		int nMI = g_SFT.GetIterations();
		g_SFT.OpenFile(g_szExamine);
		g_SFT.SetIterations(nMI);
		CDecNumber A = CDecNumber(g_SFT.GetZoom())/(CDecNumber(g_nExamineZoom)^(g_stExamine.GetCount()-g_nExamine-1));
		char *szR = g_SFT.GetRe();
		char *szRe = new char[strlen(szR)+1];
		strcpy(szRe,szR);
		char *szI = g_SFT.GetIm();
		char *szIm = new char[strlen(szI)+1];
		strcpy(szIm,szI);
		g_SFT.SetPosition(szRe,szIm,A.ToText());
		delete[] szRe;
		delete[] szIm;

		PostMessage(GetParent(hWnd),WM_USER+199,0,0);
	}
	else if(uMsg==WM_TIMER && wParam==2){
		KillTimer(hWnd,2);
		if(!g_bAutoSolveGlitch)
			return 0;
SetDlgItemText(hWnd,IDC_EDIT4,"AutoSolveGlitch");
UpdateWindow(GetDlgItem(hWnd,IDC_EDIT4));
		int rx, ry;
SetDlgItemText(hWnd,IDC_EDIT4,"Search for glitch");
UpdateWindow(GetDlgItem(hWnd,IDC_EDIT4));
		while(!g_SFT.FindCenterOfGlitch(rx,ry) || g_bAutoSolveGlitch>=g_nAutoSolveGlitchLimit){
			if(g_nExamine==0){
				g_bAutoSolveGlitch=0;
				SetDlgItemText(hWnd,IDC_BUTTON4,"Auto solve glitch");
SetDlgItemText(hWnd,IDC_EDIT4,"Done");
UpdateWindow(GetDlgItem(hWnd,IDC_EDIT4));
				return 0;
			}
			if(g_bAutoSolveGlitch>1){
SetDlgItemText(hWnd,IDC_EDIT4,"No more glitch found - save and previous");
UpdateWindow(GetDlgItem(hWnd,IDC_EDIT4));
				SendMessage(hWnd,WM_COMMAND,IDC_BUTTON5,0);
			}
			else{
SetDlgItemText(hWnd,IDC_EDIT4,"No glitch found - previous");
UpdateWindow(GetDlgItem(hWnd,IDC_EDIT4));
				SendMessage(hWnd,WM_COMMAND,IDC_BUTTON1,0);
			}
			KillTimer(hWnd,1);
			g_bAutoSolveGlitch=1;
		}
		if(g_bAutoSolveGlitch==1){
SetDlgItemText(hWnd,IDC_EDIT4,"First glitch found - read location");
UpdateWindow(GetDlgItem(hWnd,IDC_EDIT4));
			int nMI = g_SFT.GetIterations();
			g_SFT.OpenFile(g_szExamine);
			g_SFT.SetIterations(nMI);
			CDecNumber A = CDecNumber(g_SFT.GetZoom())/(CDecNumber(g_nExamineZoom)^(g_stExamine.GetCount()-g_nExamine-1));
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

char szAdd[128];
wsprintf(szAdd,"Add reference %d",g_bAutoSolveGlitch);
SetDlgItemText(hWnd,IDC_EDIT4,szAdd);
UpdateWindow(GetDlgItem(hWnd,IDC_EDIT4));
		g_SFT.AddReference(rx,ry);
		g_bAutoSolveGlitch++;
		SetTimer(GetParent(hWnd),0,500,NULL);
		g_bExamineDirty=TRUE;
	}
	else if(uMsg==WM_USER+199 && g_bAutoSolveGlitch){
		SendMessage(hWnd,WM_TIMER,2,0);
	}
	return 0;
}
int ResumeZoomSequence(HWND hWnd)
{
	memset(g_szFile,0,sizeof(g_szFile));
	if(!BrowseFile(hWnd,TRUE,"Open location","Kalle's fraktaler\0*.kfr\0\0",g_szFile,sizeof(g_szFile)))
		return 0;
	if(!g_SFT.OpenFile(g_szFile))
		return MessageBox(hWnd,"Could not open file","Error",MB_OK|MB_ICONSTOP);
	g_SFT.StoreLocation();
	char *sz = strrchr(g_szFile,'\\');
	if(sz)
		strcpy(sz+1,"*_*.jpg");
	WIN32_FIND_DATA fd;
	HANDLE hFind = FindFirstFile(g_szFile,&fd);
	if(hFind!=INVALID_HANDLE_VALUE){
		g_bStoreZoomJpg=1;
		FindClose(hFind);
	}
	else
		g_bStoreZoomJpg=0;
	sz = strrchr(g_szFile,'\\');
	if(sz)
		strcpy(sz+1,"*_*.png");
	hFind = FindFirstFile(g_szFile,&fd);
	if(hFind!=INVALID_HANDLE_VALUE){
		g_bStoreZoomPng=1;
		FindClose(hFind);
	}
	else
		g_bStoreZoomPng=0;
	if(sz)
		strcpy(sz+1,"*_*.kfb");
	hFind = FindFirstFile(g_szFile,&fd);
/*	if(!sz || hFind==INVALID_HANDLE_VALUE){
		if(hFind)
			FindClose(hFind);
		return MessageBox(hWnd,"Could not browse kfb files","Error",MB_OK|MB_ICONSTOP);
	}*/
	CStringTable stExamine;
	if(hFind!=INVALID_HANDLE_VALUE){
		do{
			strcpy(strrchr(g_szFile,'\\')+1,fd.cFileName);
			stExamine.AddRow();
			stExamine.AddString(stExamine.GetCount()-1,g_szFile);
			stExamine.AddString(stExamine.GetCount()-1,fd.cFileName);
		}while(FindNextFile(hFind,&fd));
		FindClose(hFind);
	}
	stExamine.M3QSort(0,1);
	BOOL bRecoveryFile=FALSE;

	strcpy(strrchr(g_szFile,'\\')+1,"recovery.kfb");
	hFind = FindFirstFile(g_szFile,&fd);
	if(hFind!=INVALID_HANDLE_VALUE){
		FindClose(hFind);
		g_SFT.OpenMapB(g_szFile);
		bRecoveryFile=TRUE;
	}
	else{
		if(stExamine.GetCount())
			g_SFT.OpenMapB(stExamine[0][0]);
		else
			return MessageBox(hWnd,"Could not browse kfb files","Error",MB_OK|MB_ICONSTOP);
	}

	if(stExamine.GetCount()<2)
		g_SFT.SetZoomSize(2);
	else{
		char *szA = strrchr(stExamine[0][0],'_');
		szA++;
		strcpy(g_szExamine,szA);
		*strrchr(g_szExamine,'.')=0;
		CDecNumber A(g_szExamine);
		szA = strrchr(stExamine[1][0],'_');
		szA++;
		strcpy(g_szExamine,szA);
		*strrchr(g_szExamine,'.')=0;
		CDecNumber B(g_szExamine);
		g_SFT.SetZoomSize((B/A+CDecNumber(0.5)).ToInt());
	}
	CheckMenuItem(GetMenu(hWnd),ID_ACTIONS_ZOOMSIZE_2,MF_BYCOMMAND|(g_SFT.GetZoomSize()==2?MF_CHECKED:MF_UNCHECKED));
	CheckMenuItem(GetMenu(hWnd),ID_ACTIONS_ZOOMSIZE_4,MF_BYCOMMAND|(g_SFT.GetZoomSize()==4?MF_CHECKED:MF_UNCHECKED));
	CheckMenuItem(GetMenu(hWnd),ID_ACTIONS_ZOOMSIZE_8,MF_BYCOMMAND|(g_SFT.GetZoomSize()==8?MF_CHECKED:MF_UNCHECKED));
	CheckMenuItem(GetMenu(hWnd),ID_ACTIONS_ZOOMSIZE_16,MF_BYCOMMAND|(g_SFT.GetZoomSize()==16?MF_CHECKED:MF_UNCHECKED));
	CheckMenuItem(GetMenu(hWnd),ID_ACTIONS_ZOOMSIZE_32,MF_BYCOMMAND|(g_SFT.GetZoomSize()==32?MF_CHECKED:MF_UNCHECKED));
	CheckMenuItem(GetMenu(hWnd),ID_ACTIONS_ZOOMSIZE_64,MF_BYCOMMAND|(g_SFT.GetZoomSize()==64?MF_CHECKED:MF_UNCHECKED));
	CheckMenuItem(GetMenu(hWnd),ID_ACTIONS_ZOOMSIZE_128,MF_BYCOMMAND|(g_SFT.GetZoomSize()==128?MF_CHECKED:MF_UNCHECKED));
	if(stExamine.GetCount()){
		CDecNumber A = CDecNumber(g_SFT.GetZoom())/(CDecNumber(g_SFT.GetZoomSize())^(stExamine.GetCount()-(bRecoveryFile?0:1)));
		char *szR = g_SFT.GetRe();
		char *szRe = new char[strlen(szR)+1];
		strcpy(szRe,szR);
		char *szI = g_SFT.GetIm();
		char *szIm = new char[strlen(szI)+1];
		strcpy(szIm,szI);
		g_SFT.SetPosition(szRe,szIm,A.ToText());
		delete szRe;
		delete szIm;
	}

	g_bStoreZoom=atoi(stExamine[0][1])+1;
	g_JpegParams.nWidth = g_SFT.GetWidth();
	g_JpegParams.nHeight = g_SFT.GetHeight();
	g_JpegParams.nQuality = 100;
	//g_SFT.RenderFractal(g_SFT.GetWidth(),g_SFT.GetHeight(),g_SFT.GetIterations(),hWnd);
	if(g_SFT.GetAutoIterations()){
		int nMax = g_SFT.GetMaxExceptCenter();//GetIterationOnPoint(g_SFT.GetWidth()/2-1,g_SFT.GetHeight()/2-1);
		if(nMax<g_SFT.GetIterations()/3)
			g_SFT.SetIterations(nMax*3>1000?nMax*3:1000);
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

double CompareBitmaps(HBITMAP bm1, HBITMAP bm2)
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
		delete lpBits1;
		delete lpBits2;
		return 0;
	}
	if(!GetDIBits(hDC,bm2,0,bmi.biHeight,lpBits2,
			(LPBITMAPINFO)&bmi,DIB_RGB_COLORS)){
		delete lpBits1;
		delete lpBits2;
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
	delete lpBits1;
	delete lpBits2;
	return ret;
}
int KRYield(HWND hWnd)
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
int Marilyn(HWND hWnd)
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
HBITMAP ShrinkBitmap2(HBITMAP bmBmp,int nX, int nY)
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
void SaveZoomImg(char *szFile, char *comment)
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
int HandleDone(HWND hWnd,UINT uMsg,WPARAM wParam,LPARAM lParam,int &nPos)
{
	if(g_bStoreZoom){
		strcpy(strrchr(g_szFile,'\\')+1,"recovery.kfb");
		if(uMsg==WM_USER+199)
			DeleteFile(g_szFile);
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
	if(!wParam && uMsg==WM_USER+199 && (!g_bAnim || !g_nAnimateZoom)){
		g_SFT.ApplyColors();
		InvalidateRect(hWnd,NULL,FALSE);
	}

nPos=1;
	char szTmp[154];
	wsprintf(szTmp,"%d%% R:%d%% G:%d%% A:%d%%",nP,nR,nG,nA);
	SendMessage(g_hwStatus,SB_SETTEXT,0,(LPARAM)szTmp);
nPos=2;
	if(!g_hwExamine && uMsg==WM_USER+199 && !wParam){
nPos=3;
		if(g_bAutoGlitch && g_bAutoGlitch-1<g_SFT.GetMaxReferences()){
			g_bAutoGlitch++;
nPos=4;
			int x, y;

			if(g_SFT.FindCenterOfGlitch(x, y)){
nPos=5;
				if(g_nPrevGlitchX!=x || g_nPrevGlitchY!=y){
nPos=6;
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
	if(g_hwExamine && uMsg==WM_USER+199)
		PostMessage(g_hwExamine,uMsg,wParam,lParam);
	SYSTEMTIME st;
	__int64 nTStop;
	GetLocalTime(&st);
	SystemTimeToFileTime(&st,(LPFILETIME)&nTStop);
	nTStop-=g_nTStart;
	FileTimeToSystemTime((LPFILETIME)&nTStop,&st);
	if(st.wDay>1)
		st.wHour+=(st.wDay-1)*24;
	wsprintf(szTmp,"Zoom:%s T:%02d:%02d:%02d.%03d %s",g_SFT.ToZoom(),st.wHour,st.wMinute,st.wSecond,st.wMilliseconds,uMsg==WM_USER+199?"Done":"");
nPos=9;
	if(g_bAutoGlitch){
		wsprintf(szTmp+strlen(szTmp)," Ref: %d",g_bAutoGlitch);
		if(uMsg==WM_USER+199){
			g_bAutoGlitch=1;
		}
	}
nPos=10;
	SendMessage(g_hwStatus,SB_SETTEXT,1,(LPARAM)szTmp);
	if(nP && (!g_bAnim || !g_nAnimateZoom))
		InvalidateRect(hWnd,NULL,FALSE);
nPos=11;
	if(uMsg==WM_USER+199){
		KillTimer(hWnd,0);
//			g_nAnim++;
//			g_bAnim=FALSE;
		g_SFT.ApplyColors();
		if(!g_bAnim || !g_nAnimateZoom){
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
			char *szZ = g_SFT.ToZoom();
			if(g_bStoreZoomJpg){
				wsprintf(strrchr(g_szFile,'\\')+1,"%05d_%s.jpg",g_bStoreZoom,szZ);
#ifdef PARAM_ANIMATION
				sprintf(strrchr(g_szFile,'\\')+1,"%05d_(%.3f,%.3f).jpg",g_bStoreZoom,g_SeedR,g_SeedI);
#endif
				if(g_SFT.GetZoomSize()<2 && !g_bAnimateEachFrame)
					SaveZoomImg(g_szFile, "KF2");
				else
					g_SFT.SaveJpg(g_szFile,100);
			}
			if(g_bStoreZoomPng){
				wsprintf(strrchr(g_szFile,'\\')+1,"%05d_%s.png",g_bStoreZoom,szZ);
#ifdef PARAM_ANIMATION
				sprintf(strrchr(g_szFile,'\\')+1,"%05d_(%.3f,%.3f).png",g_bStoreZoom,g_SeedR,g_SeedI);
#endif
				if(g_SFT.GetZoomSize()<2 && !g_bAnimateEachFrame)
					SaveZoomImg(g_szFile, "KF2");
				else
					g_SFT.SaveJpg(g_szFile,-1);
			}
#ifndef PARAM_ANIMATION
			wsprintf(strrchr(g_szFile,'\\')+1,"%05d_%s.kfb",g_bStoreZoom,szZ);
			if(!g_bAnimateEachFrame)
				g_SFT.SaveMapB(g_szFile);
#endif
nPos=14;
			g_bStoreZoom++;
			if(!atof(szZ)){
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

#ifdef PARAM_ANIMATION
				if(g_bSkewAnimation){
					g_length+=0.001;
					g_degree+=0.04;
					g_SeedR = cos(g_degree)*g_length + sin(g_degree)*g_length;
					g_SeedI = cos(g_degree)*g_length - sin(g_degree)*g_length;
					g_SFT.RenderFractal(g_SFT.GetWidth(),g_SFT.GetHeight(),g_SFT.GetIterations(),hWnd);
				}else
#endif
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
				g_bAnim=FALSE;
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
			g_bSaveJpeg=FALSE;
nPos=23;
			char szFile[256]={0};
			if(BrowseFile(hWnd,FALSE,"Save as Jpeg","Jpeg\0*.jpg\0\0",szFile,sizeof(szFile))){
				if(!g_SFT.SaveJpg(szFile,g_JpegParams.nQuality))
					MessageBox(hWnd,"File could not be saved","Error",MB_OK|MB_ICONSTOP);
				PostMessage(hWnd,WM_KEYDOWN,VK_F5,0);
			}
		}
		if(g_bSavePng){
			g_bSavePng=FALSE;
nPos=24;
			char szFile[256]={0};
			if(BrowseFile(hWnd,FALSE,"Save as PNG","PNG\0*.png\0\0",szFile,sizeof(szFile))){
				if(!g_SFT.SaveJpg(szFile,-1))
					MessageBox(hWnd,"File could not be saved","Error",MB_OK|MB_ICONSTOP);
				PostMessage(hWnd,WM_KEYDOWN,VK_F5,0);
			}
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
int HandleDoneSEH(HWND hWnd,UINT uMsg,WPARAM wParam,LPARAM lParam)
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

int WINAPI CustomZoomSize(HWND hWnd,UINT uMsg,WPARAM wParam,LPARAM lParam){
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
int WINAPI StopAtProc(HWND hWnd,UINT uMsg,WPARAM wParam,LPARAM lParam)
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
void RotateImageAroundPoint(HBITMAP bmBkg,POINT pm)
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
void RotateImage(HBITMAP bmBkg,HBITMAP bmBkgDraw,POINT pm,double nDegree)
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
void SkewImage(HBITMAP bmBmp)
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
void UnSkewImage(HBITMAP bmBmp)
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
int WINAPI SkewProc(HWND hWnd,UINT uMsg,WPARAM wParam,LPARAM lParam)
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
int WINAPI SkewAnimateProc(HWND hWnd,UINT uMsg,WPARAM wParam,LPARAM lParam)
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

long WINAPI MainProc(HWND hWnd,UINT uMsg,WPARAM wParam,LPARAM lParam)
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
		CheckMenuItem(GetMenu(hWnd),ID_SPECIAL_SHOWGLITCHES,MF_BYCOMMAND|(g_SFT.GetShowGlitches()?MF_CHECKED:MF_UNCHECKED));

		RECT r;
		GetClientRect(GetDesktopWindow(),&r);
		MoveWindow(hWnd,r.right/2-wr.right/2,r.bottom/2-wr.bottom/2,wr.right,wr.bottom,TRUE);
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
		g_SFT.RenderFractal(640,360,g_SFT.GetIterations(),hWnd);
		CheckMenuItem(GetMenu(hWnd),ID_ACTIONS_AUTOSOLVEGLITCHES,MF_BYCOMMAND|(g_SFT.GetAutoSolveGlitches()?MF_CHECKED:MF_UNCHECKED));
		CheckMenuItem(GetMenu(hWnd),ID_ACTIONS_SPECIAL_AUTOITERATION,MF_BYCOMMAND|(g_SFT.GetAutoIterations()?MF_CHECKED:MF_UNCHECKED));

		g_nAnimateZoom = GetPrivateProfileInt("SETTINGS","AnimateZoom",1,"fraktal_sft.ini");
		g_SFT.SetArbitrarySize(GetPrivateProfileInt("SETTINGS","ArbitrarySize",0,"fraktal_sft.ini"));
		CheckMenuItem(GetMenu(hWnd),ID_ACTIONS_ANIMATEZOOM,MF_BYCOMMAND|(g_nAnimateZoom?MF_CHECKED:MF_UNCHECKED));
		CheckMenuItem(GetMenu(hWnd),ID_SPECIAL_ARBITRARYSIZE,MF_BYCOMMAND|(g_SFT.GetArbitrarySize()?MF_CHECKED:MF_UNCHECKED));
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
		if(!HIWORD(GetKeyState(VK_CONTROL)) && !g_bRotate && !g_bFindMinibrot && g_nAnimateZoom && !g_bAddReference && !g_bEraser && !g_hwExamine && !g_bAddMainReference){
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
			g_bAnim=FALSE;
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
		g_bAnim=FALSE;
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

		if(g_nAnimateZoom){
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
		int nMin, nMax, nIter;
		g_SFT.GetIterations(nMin,nMax);
		nIter = g_SFT.GetIterations();
		if(nIter<nMin+nMin+2000)
			g_SFT.SetIterations(nMin+nMin+3000);
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
			if(g_nAnimateZoom && !g_bAddMainReference && !g_bAddReference && !g_bEraser){
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
				if(!g_nAnimateZoom){
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
					g_bAddMainReference=FALSE;
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
		g_bAnim=FALSE;
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
		if(g_SFT.GetWidth()<r.right || g_SFT.GetHeight()<r.bottom)
			g_SFT.RenderFractal(r.right,r.bottom,g_SFT.GetIterations(),hWnd);
		else if(g_SFT.GetArbitrarySize()){
			SIZE sc;
			sc.cy = g_SFT.GetHeight();
			sc.cx = (double)r.right*((double)sc.cy/(double)r.bottom);
			g_SFT.RenderFractal(sc.cx,sc.cy,g_SFT.GetIterations(),hWnd);
		}
		else
			g_SFT.RenderFractal(g_SFT.GetWidth(),g_SFT.GetHeight(),g_SFT.GetIterations(),hWnd);
	}
	else if(uMsg==WM_KEYDOWN && wParam==VK_ESCAPE){
		if(g_bAddReference){
			g_bAddReference=FALSE;
			CheckMenuItem(GetMenu(hWnd),ID_ACTIONS_ADDREFERENCE,MF_BYCOMMAND|MF_UNCHECKED);
		}
		if(g_bAddMainReference){
			g_bAddMainReference=FALSE;
			CheckMenuItem(GetMenu(hWnd),ID_ACTIONS_SPECIAL_SETMAINREFERENCE,MF_BYCOMMAND|MF_UNCHECKED);
		}
		g_SFT.Stop();
		g_bAnim=FALSE;
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
				char *szTemp="";
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
					g_bAnim=FALSE;
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
		g_bAddMainReference=FALSE;
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
		if(!g_nAnimateZoom){
			SendMessage(hWnd,WM_KEYDOWN,VK_ESCAPE,0);
			SendMessage(hWnd,WM_LBUTTONDOWN,0,MAKELONG(p.x,p.y));
			PostMessage(hWnd,WM_LBUTTONUP,0,MAKELONG(p.x,p.y));
			return 0;
		}
		if(lParam==9){
			p.x=rc.right/2 + (p.x-rc.right/2)/2;
			p.y=rc.bottom/2 + (p.y-rc.bottom/2)/2;
		}
		int nMin, nMax, nIter;
		g_SFT.GetIterations(nMin,nMax);
		nIter = g_SFT.GetIterations();
		if(nIter<nMin+nMin/2+2000)
			g_SFT.SetIterations(nMin+nMin/2+3000);

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
		if(g_nAnimateZoom){
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
		g_bAddMainReference=FALSE;
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
		if(!g_nAnimateZoom){
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
		if(g_nAnimateZoom){
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
		g_nAnimateZoom=!g_nAnimateZoom;
		CheckMenuItem(GetMenu(hWnd),ID_ACTIONS_ANIMATEZOOM,MF_BYCOMMAND|(g_nAnimateZoom?MF_CHECKED:MF_UNCHECKED));
		WritePrivateProfileString("SETTINGS","AnimateZoom",g_nAnimateZoom?"1":"0","fraktal_sft.ini");
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
	else if(uMsg==WM_COMMAND && wParam==ID_ACTIONS_ROTATE){
		g_bRotate=!g_bRotate;
		CheckMenuItem(GetMenu(hWnd),ID_ACTIONS_ROTATE,MF_BYCOMMAND|(g_bRotate?MF_CHECKED:MF_UNCHECKED));
	}
	else if(uMsg==WM_COMMAND && wParam==ID_ACTIONS_RESETROTATION){
		g_SFT.Stop();
		g_bAnim=FALSE;
		g_Degree=0;
		SetTimer(hWnd,0,500,NULL);
		g_SFT.RenderFractal(g_SFT.GetWidth(),g_SFT.GetHeight(),g_SFT.GetIterations(),hWnd);
	}
	else if(uMsg==WM_COMMAND && wParam==ID_ACTIONS_SPECIAL_SPECIAL_MIRROR1){
		if(g_SFT.GetMirror()==1)
			g_SFT.SetMirror(0);
		else
			g_SFT.SetMirror(1);
		CheckMenuItem(GetMenu(hWnd),ID_ACTIONS_SPECIAL_SPECIAL_MIRROR1,MF_BYCOMMAND|(g_SFT.GetMirror()?MF_CHECKED:MF_UNCHECKED));
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
		if(BrowseFile(hWnd,FALSE,"Save Map","Kalle's fraktaler\0*.kfb\0\0",g_szFile,sizeof(g_szFile)))
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
		if(strrchr(g_szFile,'\\'))
			*strrchr(g_szFile,'\\')=0;
		if(!Browse(hWnd,g_szFile,sizeof(g_szFile)))
			return 0;
		if(g_szFile[strlen(g_szFile)-1]!='\\')
			strcat(g_szFile,"\\");
		SetTimer(hWnd,0,500,NULL);
		g_bStoreZoom=1;
		if(MessageBox(hWnd,"Do you want to store PNG images?","Kalle's Fraktaler",MB_YESNO|MB_ICONQUESTION)==IDYES)
			g_bStoreZoomPng=1;
		else
			g_bStoreZoomPng=0;
		if(MessageBox(hWnd,"Do you want to store JPEG images?","Kalle's Fraktaler",MB_YESNO|MB_ICONQUESTION)==IDYES)
			g_bStoreZoomJpg=1;
		else
			g_bStoreZoomJpg=0;
		char szFile[256];
		strcpy(szFile,g_szFile);
		wsprintf(strrchr(szFile,'\\')+1,"%05d_*.kfb",g_bStoreZoom);
		while(FileExists(szFile)){
			g_bStoreZoom++;
			wsprintf(strrchr(szFile,'\\')+1,"%05d_*.kfb",g_bStoreZoom);
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
		CheckMenuItem(GetMenu(hWnd),ID_ACTIONS_SPECIAL_NOAPPROXIMATION,MF_BYCOMMAND|(g_SFT.GetNoApprox()?MF_CHECKED:MF_UNCHECKED));
	}

	else if((uMsg==WM_COMMAND && wParam==ID_ACTIONS_SETIMAGESIZE) || (uMsg==WM_KEYDOWN && wParam=='Z' && HIWORD(GetKeyState(VK_CONTROL)))){
		g_JpegParams.nWidth = g_SFT.GetWidth();
		g_JpegParams.nHeight = g_SFT.GetHeight();
		if(!DialogBoxParam(GetModuleHandle(NULL),MAKEINTRESOURCE(IDD_DIALOG7),hWnd,(DLGPROC)JpegProc,1))
			return 0;
		g_SFT.Stop();
		g_bAnim=FALSE;
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
		MoveWindow(hWnd,wr.left,wr.top,wr.right,wr.bottom,TRUE);
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

		g_bAddMainReference=FALSE;
		CheckMenuItem(GetMenu(hWnd),ID_ACTIONS_SPECIAL_SETMAINREFERENCE,MF_BYCOMMAND|MF_UNCHECKED);
	}
	else if(uMsg==WM_COMMAND && wParam==ID_ACTIONS_SPECIAL_ADDREFERENCEERRORS){
		g_bAddReference=FALSE;
		CheckMenuItem(GetMenu(hWnd),ID_ACTIONS_ADDREFERENCE,MF_BYCOMMAND|MF_UNCHECKED);
		g_bAddMainReference=FALSE;
		CheckMenuItem(GetMenu(hWnd),ID_ACTIONS_SPECIAL_SETMAINREFERENCE,MF_BYCOMMAND|MF_UNCHECKED);
	}
	else if(uMsg==WM_COMMAND && wParam==ID_ACTIONS_SPECIAL_SETMAINREFERENCE){
		g_bAddMainReference=TRUE;
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
		memset(g_szExamine,0,sizeof(g_szExamine));
		if(!BrowseFile(hWnd,TRUE,"Open location","Kalle's fraktaler\0*.kfr\0\0",g_szExamine,sizeof(g_szExamine)))
			return 0;
		if(g_hwExamine)
			SetFocus(g_hwExamine);
		else{
			g_hwExamine = CreateDialog(GetModuleHandle(NULL),MAKEINTRESOURCE(IDD_DIALOG8),hWnd,(DLGPROC)ExamineProc);
			ShowWindow(g_hwExamine,SW_SHOW);
		}
	}
	else if(uMsg==WM_COMMAND && wParam==ID_FILE_RESUMEZOOMSEQUENCE){
		return ResumeZoomSequence(hWnd);
	}
	else if(uMsg==WM_COMMAND && wParam==ID_ACTIONS_SPECIAL_SOLVEGLITCHWITHNEARPIXELSMETHOD){
		g_SFT.SetSolveGlitchNear(! g_SFT.GetSolveGlitchNear());
		CheckMenuItem(GetMenu(hWnd),ID_ACTIONS_SPECIAL_SOLVEGLITCHWITHNEARPIXELSMETHOD,MF_BYCOMMAND|(g_SFT.GetSolveGlitchNear()?MF_CHECKED:MF_UNCHECKED));
	}
	else if(uMsg==WM_COMMAND && wParam==ID_ACTIONS_AUTOSOLVEGLITCHES){
		g_bAutoGlitch=!g_bAutoGlitch;
		g_nPrevGlitchX=g_nPrevGlitchY=-1;
		g_SFT.SetAutoSolveGlitches(g_bAutoGlitch);
		CheckMenuItem(GetMenu(hWnd),ID_ACTIONS_AUTOSOLVEGLITCHES,MF_BYCOMMAND|(g_SFT.GetAutoSolveGlitches()?MF_CHECKED:MF_UNCHECKED));
		if(g_bAutoGlitch){
			g_SFT.SetReuseReference(!g_bAutoGlitch);
			CheckMenuItem(GetMenu(hWnd),ID_ACTIONS_REUSEREFERENCE,MF_BYCOMMAND|(g_SFT.GetReuseReference()?MF_CHECKED:MF_UNCHECKED));
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
		CheckMenuItem(GetMenu(hWnd),ID_ACTIONS_SPECIAL_USELONGDOUBLEFROMSTART,MF_BYCOMMAND|(g_nLDBL<100?MF_CHECKED:MF_UNCHECKED));
		CheckMenuItem(GetMenu(hWnd),ID_ACTIONS_SPECIAL_USEFLOATEXPALWAYS,MF_BYCOMMAND|(g_nEXP==3?MF_CHECKED:MF_UNCHECKED));
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
		CheckMenuItem(GetMenu(hWnd),ID_ACTIONS_SPECIAL_USELONGDOUBLEFROMSTART,MF_BYCOMMAND|MF_UNCHECKED);
		CheckMenuItem(GetMenu(hWnd),ID_ACTIONS_SPECIAL_USEFLOATEXPALWAYS,MF_BYCOMMAND|(g_nEXP==3?MF_CHECKED:MF_UNCHECKED));
	}
	else if(uMsg==WM_KEYDOWN && wParam==VK_LEFT && HIWORD(GetKeyState(VK_CONTROL))){
		RECT r;
		GetClientRect(hWnd,&r);
		RECT sr;
		GetWindowRect(g_hwStatus,&sr);
		r.bottom-=(sr.bottom-sr.top);
		if(g_nAnimateZoom){
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
		if(g_nAnimateZoom){
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
		if(g_nAnimateZoom){
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
		if(g_nAnimateZoom){
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
			g_bAnim=FALSE;
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
		if(g_nAnimateZoom){
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
				g_bAnim=FALSE;
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
			CheckMenuItem(GetMenu(hWnd),ID_ACTIONS_ZOOMSIZE_1,MF_BYCOMMAND|(g_SFT.GetZoomSize()==1?MF_CHECKED:MF_UNCHECKED));
			CheckMenuItem(GetMenu(hWnd),ID_ACTIONS_ZOOMSIZE_2,MF_BYCOMMAND|(g_SFT.GetZoomSize()==2?MF_CHECKED:MF_UNCHECKED));
			CheckMenuItem(GetMenu(hWnd),ID_ACTIONS_ZOOMSIZE_4,MF_BYCOMMAND|(g_SFT.GetZoomSize()==4?MF_CHECKED:MF_UNCHECKED));
			CheckMenuItem(GetMenu(hWnd),ID_ACTIONS_ZOOMSIZE_8,MF_BYCOMMAND|(g_SFT.GetZoomSize()==8?MF_CHECKED:MF_UNCHECKED));
			CheckMenuItem(GetMenu(hWnd),ID_ACTIONS_ZOOMSIZE_16,MF_BYCOMMAND|(g_SFT.GetZoomSize()==16?MF_CHECKED:MF_UNCHECKED));
			CheckMenuItem(GetMenu(hWnd),ID_ACTIONS_ZOOMSIZE_32,MF_BYCOMMAND|(g_SFT.GetZoomSize()==32?MF_CHECKED:MF_UNCHECKED));
			CheckMenuItem(GetMenu(hWnd),ID_ACTIONS_ZOOMSIZE_64,MF_BYCOMMAND|(g_SFT.GetZoomSize()==64?MF_CHECKED:MF_UNCHECKED));
			CheckMenuItem(GetMenu(hWnd),ID_ACTIONS_ZOOMSIZE_128,MF_BYCOMMAND|(g_SFT.GetZoomSize()==128?MF_CHECKED:MF_UNCHECKED));
		}
		else if(wParam==ID_ACTIONS_REUSEREFERENCE){
			g_SFT.SetReuseReference(! g_SFT.GetReuseReference());
			CheckMenuItem(GetMenu(hWnd),ID_ACTIONS_REUSEREFERENCE,MF_BYCOMMAND|(g_SFT.GetReuseReference()?MF_CHECKED:MF_UNCHECKED));
			if(g_SFT.GetReuseReference()){
				g_SFT.SetAutoSolveGlitches(!g_SFT.GetReuseReference());
				CheckMenuItem(GetMenu(hWnd),ID_ACTIONS_AUTOSOLVEGLITCHES,MF_BYCOMMAND|(g_SFT.GetAutoSolveGlitches()?MF_CHECKED:MF_UNCHECKED));
			}
		}
		else if(wParam==ID_FILE_OPEN_){
			if(BrowseFile(hWnd,TRUE,"Open Location Parameters","Kalle's fraktaler\0*.kfr\0Image files\0*.png;*.jpg;*.jpeg\0\0",g_szFile,sizeof(g_szFile))){
				g_SFT.Stop();
				g_bAnim=FALSE;
				if(!g_SFT.OpenFile(g_szFile))
					return MessageBox(hWnd,"Invalid parameter file","Error",MB_OK|MB_ICONSTOP);
				else{
					char *extension = strrchr(g_szFile, '.');
					if (extension && 0 != strcmp(".kfr", extension))
					{
						// prevent ctrl-s save overwriting a file with the wrong extension
						strcat(extension, ".kfr");
					}
					if(g_hwColors)
						SendMessage(g_hwColors,WM_USER+99,0,0);
					PostMessage(hWnd,WM_KEYDOWN,VK_F5,0);
					char szTitle[369];
					wsprintf(szTitle,"Kalle's Fraktaler 2 - %s",g_szFile);
					SetWindowText(hWnd,szTitle);
				}
			}
		}
		else if(wParam==ID_FILE_SAVE_){
			if(!*g_szFile)
				PostMessage(hWnd,WM_COMMAND,ID_FILE_SAVEAS_,0);
			else if(!g_SFT.SaveFile(g_szFile))
				return MessageBox(hWnd,"Could not save parameters","Error",MB_OK|MB_ICONSTOP);
		}
		else if(wParam==ID_FILE_SAVEASJPEG){
			g_JpegParams.nWidth = g_SFT.GetWidth();
			g_JpegParams.nHeight = g_SFT.GetHeight();
			g_JpegParams.nQuality = 100;
			if(DialogBoxParam(GetModuleHandle(NULL),MAKEINTRESOURCE(IDD_DIALOG7),hWnd,(DLGPROC)JpegProc,0)){
				char szFile[256]={0};
				if(g_JpegParams.nWidth>g_SFT.GetWidth()){
					g_bSaveJpeg=TRUE;
					SetTimer(hWnd,0,500,NULL);
					g_SFT.RenderFractal(g_JpegParams.nWidth,g_JpegParams.nHeight,g_SFT.GetIterations(),hWnd);
					return 0;
				}
				if(BrowseFile(hWnd,FALSE,"Save as Jpeg","Jpeg\0*.jpg\0\0",szFile,sizeof(szFile))){
					if(!g_SFT.SaveJpg(szFile,g_JpegParams.nQuality,g_JpegParams.nWidth,g_JpegParams.nHeight))
						MessageBox(hWnd,"File could not be saved","Error",MB_OK|MB_ICONSTOP);
					char *e = strrchr(szFile,'.');
					if(e)
						e++;
					else
						e = szFile+strlen(szFile);
					strcpy(e,"kfb");
					if(FileExists(szFile) && MessageBox(hWnd,"Found a map file (.kfb) with the same name, do you want to replace it?","Kalle's Fraktaler",MB_YESNO)==IDYES)
						g_SFT.SaveMapB(szFile);
				}
			}
		}
		else if(wParam==ID_FILE_SAVEASPNG){
			g_JpegParams.nWidth = g_SFT.GetWidth();
			g_JpegParams.nHeight = g_SFT.GetHeight();
			g_JpegParams.nQuality = 100;
			if(DialogBoxParam(GetModuleHandle(NULL),MAKEINTRESOURCE(IDD_DIALOG7),hWnd,(DLGPROC)JpegProc,0)){
				char szFile[256]={0};
				if(g_JpegParams.nWidth>g_SFT.GetWidth()){
					g_bSavePng=TRUE;
					SetTimer(hWnd,0,500,NULL);
					g_SFT.RenderFractal(g_JpegParams.nWidth,g_JpegParams.nHeight,g_SFT.GetIterations(),hWnd);
					return 0;
				}
				if(BrowseFile(hWnd,FALSE,"Save as PNG","PNG\0*.png\0\0",szFile,sizeof(szFile))){
					if(!g_SFT.SaveJpg(szFile,-1,g_JpegParams.nWidth,g_JpegParams.nHeight))
						MessageBox(hWnd,"File could not be saved","Error",MB_OK|MB_ICONSTOP);
					char *e = strrchr(szFile,'.');
					if(e)
						e++;
					else
						e = szFile+strlen(szFile);
					strcpy(e,"kfb");
					if(FileExists(szFile) && MessageBox(hWnd,"Found a map file (.kfb) with the same name, do you want to replace it?","Kalle's Fraktaler",MB_YESNO)==IDYES)
						g_SFT.SaveMapB(szFile);
				}
			}
		}
		else if(wParam==ID_FILE_SAVEAS_){
			if(BrowseFile(hWnd,FALSE,"Save Location Parameters","Kalle's fraktaler\0*.kfr\0\0",g_szFile,sizeof(g_szFile))){
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
			*g_szFile=0;
			SetWindowText(hWnd,"Kalle's Fraktaler 2");
			g_SFT.SetPosition("0","0","1");
			PostMessage(hWnd,WM_KEYDOWN,VK_F5,0);
		}
		else if(wParam==ID_SPECIAL_NON){
			if(!g_bFindMinibrotCount){
				char *e = g_SFT.GetZoom();
				e = stristr(e,"e");
				if(e)
					e++;
				else e = "1";
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
			g_bAnim=FALSE;
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
				g_SFT.SetIterations(nMin+nMin/2+3000);
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
			char szMsg[1024]; // this is the size limit for MessageBox, longer is truncated
			SYSTEM_INFO sysinfo;
			GetSystemInfo( &sysinfo );  //©
			wsprintf(szMsg,
				"version 2.12.4\n"
				"©2013-2017 Karl Runmo\n"
				"©2017 Claude Heiland-Allen\n\n"
				"Processors: %d\n"
				// mpf_t decimal digits = floor(64.0 * ((1<<31)-1) * log2(10))
				"Precision: 456562320657\n"
				"%s\n"
				"\nLibraries:\n"
				"- JPEG 6b <http://ijg.org>\n"
				"- PNG %s <http://libpng.org>\n"
				"- ZLIB %s <http://zlib.net>\n"
				"- GMP %d.%d.%d <http://gmplib.org>\n"
				"- Boost %d.%d.%d <http://boost.org>\n"
#ifdef KF_OPENCL
				"- CLEW git.50751dd <https://github.com/martijnberger/clew>\n"
#endif
				"\nThanks to:\n"
				" - K.I.Martin for applying Perturbation and Series Approximation on the Mandelbrot set and sharing theory and source code!\n"
				" - Pauldelbrot for reliable glitch detection method\n"
				" - Botond Kósa and knighty for extensions of Series Approximation\n"
				" - laser blaster for Burning ship formula\n"
				" - stardust4ever for other fractal types\n"
				" - claude for Newton-Raphson method\n"
				" - gerrit for differencing variations\n"
				" - Dinkydau, Fractal universe, CFJH and others for bug reports\n"
				" - Chillheimer for hosting my program\n\n"
				"http://www.chillheimer.de/kallesfraktaler/\n\n"
				"Claude also thanks Karl for releasing source code so we all could learn from it and make modifications.\n\n"
				"https://mathr.co.uk/kf/kf.html",
				sysinfo.dwNumberOfProcessors,sizeof(void*)==4?"32-bit":"64-bit",
				png_libpng_ver,
				zlib_version,
				__GNU_MP_VERSION, __GNU_MP_VERSION_MINOR, __GNU_MP_VERSION_PATCHLEVEL,
				BOOST_VERSION / 100000, BOOST_VERSION / 100 % 1000, BOOST_VERSION % 100
				);
			return MessageBox(hWnd,szMsg,"Kalle's Fraktaler 2",MB_OK);
		}
	}
	return DefWindowProc(hWnd,uMsg,wParam,lParam);
}
int Test()
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

int Test2()
{
	return 0;
}

int Test1()
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

int WINAPI WinMain(HINSTANCE hInstance,HINSTANCE,LPSTR,int)
{
//	return Test();

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
