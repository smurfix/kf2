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

#include "main.h"
#include "main_color.h"
#include "fraktal_sft.h"
#include "resource.h"
#include "tooltip.h"
#include "../common/FolderBrowser.h"
#include "../common/StringVector.h"
#include "../common/getimage.h"
#include "listbox.h"

#include <commctrl.h>

#include <fstream>

static char g_szTmpFile[MAX_PATH];

static RECT g_rShow;
static COLOR14 g_colCopy={0};
static BOOL g_bCaptureMouse=FALSE;
static BOOL g_bInitColorDialog=FALSE;
static CListBoxEdit *g_pWaves=NULL;
static BOOL g_AutoColour = TRUE;
static int g_AutoUpdate = 0;
static HWND g_hwColorsOpenGL = nullptr;

static std::string read_file(const std::string &filename)
{
  std::ifstream in(filename);
  if (in.is_open())
  {
    std::ostringstream sstr;
    sstr << in.rdbuf();
    return sstr.str();
  }
  return "";
}

static bool write_file(const std::string &filename, const std::string &data)
{
	std::ofstream out(filename);
	out << data;
	out.close();
	return !!out;
}

static std::string GetDlgItemString(HWND hWnd, int idc)
{
	int length = GetWindowTextLength(GetDlgItem(hWnd, idc));
	char *buffer = new char[length + 1];
	GetDlgItemText(hWnd, idc, buffer, length + 1);
  std::string ret(buffer);
  delete[] buffer;
  return ret;
}

extern int WINAPI ColorOpenGLProc(HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	if (uMsg == WM_INITDIALOG)
	{
		SendMessage(hWnd, WM_SETICON, ICON_SMALL, LPARAM(g_hIcon));
		SendMessage(hWnd, WM_SETICON, ICON_BIG, LPARAM(g_hIcon));

// this dialog is never destoyed, but hidden and reshown
// this means we don't need to store the tooltip windows
// because they will be deleted at program exit (I hope)
// also hopefully WM_INITDIALOG will be called only once
#define T(idc,str) CreateToolTip(idc, hWnd, str);
		T(IDC_OPENGL_GLSL, "OpenGL Shader Language source code fragment")
		T(IDC_OPENGL_LOG, "OpenGL shader compilation log")
		T(IDC_OPENGL_IMPORT, "Import OpenGL shader fragment")
		T(IDC_OPENGL_EXPORT, "Export OpenGL shader fragment")
		T(IDC_OPENGL_ENABLED, "Enable colouring using OpenGL shader")
		T(IDC_OPENGL_SRGB, "Convert input/output from/to sRGB colour space\nWhen activated, shader operates in linear light")
		T(IDC_OPENGL_VERSION, "Reports the version of OpenGL currently in use\nThis is typically the highest supported on this machine\nShader GLSL should test __VERSION__ to adapt to the environment")
		T(IDOK, "Apply changes and compile shader")
		T(IDCANCEL, "Close the dialog")
#undef T

		SendDlgItemMessage(hWnd, IDC_OPENGL_GLSL, EM_SETLIMITTEXT, 0, 0);
		SendDlgItemMessage(hWnd, IDC_OPENGL_LOG, EM_SETLIMITTEXT, 0, 0);
		DragAcceptFiles(hWnd, TRUE);
  }
  if (uMsg == WM_SHOWWINDOW || uMsg == WM_USER + 99)
  {
		SendDlgItemMessage(hWnd, IDC_OPENGL_ENABLED, BM_SETCHECK, g_SFT.GetUseOpenGL() ? 1 : 0, 0);
		SendDlgItemMessage(hWnd, IDC_OPENGL_SRGB, BM_SETCHECK, g_SFT.GetUseSRGB() ? 1 : 0, 0);
		SetDlgItemText(hWnd, IDC_OPENGL_GLSL, g_SFT.GetGLSL().c_str());
		g_AutoUpdate++;
		SendMessage(hWnd, WM_COMMAND, IDOK, 0);
		g_AutoUpdate--;
	}
	else if (uMsg == WM_COMMAND)
	{
		if (wParam == IDCANCEL || wParam == IDCLOSE)
		{
			ShowWindow(hWnd, SW_HIDE);
		}
		else if (wParam == IDC_OPENGL_EXPORT)
		{
			std::string file;
			if (BrowseFile(hWnd, false, "Select shader file", "GLSL\0*.glsl\0\0", file))
			{
				write_file(file, GetDlgItemString(hWnd, IDC_OPENGL_GLSL));
			}
		}
		else if (wParam == IDC_OPENGL_IMPORT)
		{
			g_SFT.UndoStore();
			std::string file;
			if (BrowseFile(hWnd, true, "Select shader file", "GLSL\0*.glsl\0\0", file))
			{
				SetDlgItemText(hWnd, IDC_OPENGL_GLSL, read_file(file).c_str());
				g_AutoUpdate++;
				SendMessage(hWnd, WM_COMMAND, IDOK, 0);
				g_AutoUpdate--;
			}
		}
		else if (wParam == IDC_OPENGL_DEFAULT)
		{
			g_SFT.UndoStore();
			SetDlgItemText(hWnd, IDC_OPENGL_GLSL, KF_DEFAULT_GLSL);
			g_AutoUpdate++;
			SendMessage(hWnd, WM_COMMAND, IDOK, 0);
			g_AutoUpdate--;
		}
		else if (wParam == IDC_OPENGL_SRGB)
		{
			g_SFT.UndoStore();
			g_SFT.SetUseSRGB(SendDlgItemMessage(hWnd, IDC_OPENGL_SRGB, BM_GETCHECK, 0, 0));
			g_AutoUpdate++;
			SendMessage(hWnd, WM_COMMAND, IDOK, 0);
			g_AutoUpdate--;
		}
		else if (wParam == IDC_OPENGL_ENABLED)
		{
			g_SFT.UndoStore();
			g_SFT.SetUseOpenGL(SendDlgItemMessage(hWnd, IDC_OPENGL_ENABLED, BM_GETCHECK, 0, 0));
			g_AutoUpdate++;
			SendMessage(hWnd, WM_COMMAND, IDOK, 0);
			g_AutoUpdate--;
		}
		else if (wParam == IDOK)
		{
			if (! g_AutoUpdate)
			{
				g_SFT.UndoStore();
			}
			g_SFT.SetGLSL(GetDlgItemString(hWnd, IDC_OPENGL_GLSL));
			SendMessage(g_hwColors, WM_COMMAND, IDOK, 0);
			SendDlgItemMessage(hWnd, IDC_OPENGL_ENABLED, BM_SETCHECK, g_SFT.GetUseOpenGL() ? 1 : 0, 0);
			SetDlgItemText(hWnd, IDC_OPENGL_LOG, g_SFT.GetGLSLLog().c_str());
			char version[] = { char('0' + g_SFT.m_opengl_major), '.', char('0' + g_SFT.m_opengl_minor), 0 };
			SetDlgItemText(hWnd, IDC_OPENGL_VERSION, version);
		}
	}
	else if (uMsg == WM_DROPFILES)
	{
		HDROP hDrop = (HDROP) wParam;
		if (hDrop)
		{
			UINT len = DragQueryFile(hDrop, 0, 0, 0);
			if (len > 0)
			{
				char *buffer = (char *) calloc(1, 2 * len + 1);
				if (buffer)
				{
					UINT ok = DragQueryFile(hDrop, 0, buffer, 2 * len);
					if (ok)
					{
						std::string file(buffer);
						std::string glsl = read_file(file);
						SetDlgItemText(hWnd, IDC_OPENGL_GLSL, glsl.c_str());
						SendMessage(hWnd, WM_COMMAND, IDOK, 0);
					}
					free(buffer);
				}
			}
			DragFinish(hDrop);
		}
	}
	return 0;
}

extern int WINAPI ColorProc(HWND hWnd,UINT uMsg,WPARAM wParam,LPARAM lParam)
{
	static COLORREF colCust[16]={0};
	if(uMsg==WM_SHOWWINDOW && wParam){
		GetTempPath(sizeof(g_szTmpFile),g_szTmpFile);
		GetTempFileName(g_szTmpFile,"KFR",TRUE,g_szTmpFile);
		g_SFT.SaveFile(g_szTmpFile, true);
	}
	if(uMsg==WM_INITDIALOG)
	{
		SendMessage(hWnd, WM_SETICON, ICON_SMALL, LPARAM(g_hIcon));
		SendMessage(hWnd, WM_SETICON, ICON_BIG, LPARAM(g_hIcon));

// this dialog is never destoyed, but hidden and reshown
// this means we don't need to store the tooltip windows
// because they will be deleted at program exit (I hope)
// also hopefully WM_INITDIALOG will be called only once
#define T(idc,str) CreateToolTip(idc, hWnd, str);
#define T2(idc1,idc2,str) T(idc1, str) T(idc2, str)
		T2(IDC_EDIT1, IDC_SPIN1, "Number of Key Colors\nThese colors will be spread out on the 1024 color palette\nInteger value")
	  T2(IDC_EDIT3, IDC_SPIN2, "Division of iteration mapping\nThis value can be a float value but not negative")
	  T2(IDC_EDIT12, IDC_SPIN5, "Color offset\nOffset the colors in the palette,\nvalid values are 0-1024")
	  T(IDC_EDIT2, "Seed value for making a random palette")
		T(IDC_BUTTON2, "Generate the given number of key colors\nfrom the seed value")
		T(IDC_BUTTON13, "Generate the given number of key colors\nfrom a random seed value")
		T(IDC_BUTTON12, "Generate RGB key colors\nfrom the given values")
		T(IDC_BUTTON14, "Generate RGB key colors\nfrom random values")
		T(IDC_BUTTON1, "Save the current palette")
		T(IDC_BUTTON5, "Open a previously saved palette")
		T(IDC_CHECK4, "Apply 3D-like shadows based on changes in iteration values")
		T2(IDC_EDIT20, IDC_SPIN6, "Slope shadow depth")
		T2(IDC_EDIT21, IDC_SPIN7, "Slope shadow strength")
		T2(IDC_EDIT22, IDC_SPIN8, "Slope shadow angle (0-360)")
		T(IDC_BUTTON22, "Select an image from which colors will be fetched")
		T(IDC_BUTTON6, "Double the number of Key Colors\nby spreading out the current colors")
		T(IDC_BUTTON7, "Expand the number of Key Colors to 1024\nby spreading out the current colors")
		T(IDC_BUTTON10, "Double by repeating the Key Colors")
		T(IDC_CHECK1, "Move the cursor over the fractal to select the Key Color in the list.\nWill only work if color offset is zero")
		T(IDC_COLOR_PHASE_STRENGTH, "Adjust colors based on final iterate phase angle")
		T(IDC_COLOR_TRANSITION_FLAT, "Make colors flat")
		T(IDC_CHECK2, "Make colors smooth")
		T(IDC_CHECK3, "Inverse color transition")
		T(IDC_COMBO1, "Color method. Available methods are\nStandard: Standard iteration band coloring\nSquare root: Iterations are squared before colors are appplied\nCubic root: Cube root is applied before colors\nLogarithm: Logarithm is applied before colors\nStretched: The palette is stretched over min-max iteration values\nDistance (Linear): Distance Estimation with linear transfer (2.11.1 compatible)\nDE+Standard: hybrid mode\nDistance (Logarithm) DE with log transfer\nDistance (Square Root) DE with sqrt transfer (2.11.1+gmp.DATE compatible)")
		T(IDC_DIFFERENCES, "Derivative differencing calculation method for distance colouring")
		T(IDC_RADIO4, "Colors are merged on distinct steps")
		T(IDC_RADIO5, "Colors are merged by sine function with given period length")
		T2(IDC_EDIT9, IDC_SPIN3, "Length of period")
		T2(IDC_EDIT10, IDC_SPIN4, "Rate in percent of merged color")
		T(IDC_BUTTON8, "Select the color to be merged, which will be applied when OK is clicked in the color dialog")
		T(IDC_LIST1, "List of Key Colors.\nEach Key Color can be edited by double click.\nAdditional functions are available by right click on a Key Color")
		T(IDC_CHECK6, "Activate sine waves on HSB coloring")
		T(IDC_CHECK7, "Blend Infinite Colors and the Color Palette")
		T(IDC_COMBO4, "Select type of wave:\nHue, Saturation or Brightness")
		T(IDC_EDIT23, "Period length of the wave")
		T(IDC_BUTTON29, "Change the period wave to the nearest higher prime value")
		T(IDC_BUTTON30, "Fill the palette with the colors from infinte waves")
		T(IDC_BUTTON26, "Add a new wave with the given values")
		T(IDC_BUTTON27, "Update the selected wave")
		T(IDC_BUTTON28, "Remove the selected wave")
		T(IDC_EDIT11, "Period lenght of Red color")
		T(IDC_EDIT14, "Period lenght of Green color")
		T(IDC_EDIT16, "Period lenght of Blue color")
		T(IDC_EDIT18, "Period lenght of Black and White color")
		T(IDC_BUTTON16, "Change the period wave to the nearest higher prime value for Red color")
		T(IDC_BUTTON19, "Change the period wave to the nearest higher prime value for Green color")
		T(IDC_BUTTON20, "Change the period wave to the nearest higher prime value for Blue color")
		T(IDC_BUTTON21, "Change the period wave to the nearest higher prime value for Black and White color")
		T(IDC_EDIT13, "Change the start position of the wave of Red color")
		T(IDC_EDIT15, "Change the start position of the wave of Green color")
		T(IDC_EDIT17, "Change the start position of the wave of Blue color")
		T(IDC_EDIT19, "Change the start position of the wave of Black and White color")
		T(IDC_LIST6, "List of Infinite Waves")
		T(IDC_BUTTON17, "Apply more contrast on the palette")
		T(IDC_BUTTON18, "Apply less contrast on the palette")
		T(IDC_AUTOCOLOUR, "Automatically apply palette on change")
		T(IDC_COLOR_OPENGL, "Show OpenGL colouring dialog (Ctrl+G)")
		T(IDOK, "Apply current palette")
		T(IDCLOSE, "Close the dialog and undo all changes")
		T(IDCANCEL, "Close the dialog")
		T(1051, "Enable texture")
		T(1052, "Texture depth")
		T(1053, "Texture strength/ratio")
		T(1054, "Browse for image")
		T(1055, "Texture image")
#undef T2
#undef T

		SendDlgItemMessage(hWnd, IDC_AUTOCOLOUR, BM_SETCHECK, g_AutoColour, 0);
		DragAcceptFiles(hWnd, TRUE);
	}
	if(uMsg==WM_INITDIALOG || uMsg==WM_USER+99 || (uMsg==WM_SHOWWINDOW && wParam) || (uMsg==WM_COMMAND && (wParam==IDC_CHECK6 || wParam==IDC_CHECK7))){
		std::string szTexture;
		double nRatio, nPower;
		int nMerge;
		BOOL bTexture = g_SFT.GetTexture(nRatio,nPower,nMerge,szTexture);
		SetDlgItemText(hWnd,IDC_EDIT29,szTexture.c_str());
		SendDlgItemMessage(hWnd,IDC_CHECK8,BM_SETCHECK,bTexture,0);
		SetDlgItemFloat(hWnd,IDC_EDIT26,nPower);
		SetDlgItemInt(hWnd,IDC_EDIT27,nRatio*100,FALSE);
		SetDlgItemFloat(hWnd, IDC_COLOR_PHASE_STRENGTH, g_SFT.GetPhaseColorStrength());
		if(!g_pWaves){
			HWND hwnds[2]={GetDlgItem(hWnd,IDC_EDIT23),GetDlgItem(hWnd,IDC_EDIT25)};
			g_pWaves = new CListBoxEdit(GetDlgItem(hWnd,IDC_BUTTON26), GetDlgItem(hWnd,IDC_BUTTON27), GetDlgItem(hWnd,IDC_BUTTON28), GetDlgItem(hWnd,IDC_EDIT24), GetDlgItem(hWnd,IDC_LIST6),hwnds, 2);

			SendDlgItemMessage(hWnd,IDC_COMBO4,CB_ADDSTRING,0,(LPARAM)"Hue");
			SendDlgItemMessage(hWnd,IDC_COMBO4,CB_ADDSTRING,0,(LPARAM)"Saturation");
			SendDlgItemMessage(hWnd,IDC_COMBO4,CB_ADDSTRING,0,(LPARAM)"Brightness");
		}
		if(uMsg==WM_COMMAND && (wParam==IDC_CHECK6 || wParam==IDC_CHECK7)){
			g_SFT.SetMW(SendDlgItemMessage(hWnd,IDC_CHECK6,BM_GETCHECK,0,0),SendDlgItemMessage(hWnd,IDC_CHECK7,BM_GETCHECK,0,0));
			g_AutoUpdate++;
			SendMessage(hWnd,WM_COMMAND,IDOK,0);
			g_AutoUpdate--;
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
		if (uMsg == WM_USER + 99)
		{
			SendMessage(g_hwColorsOpenGL, WM_USER + 99, 0, 0);
		}
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

		if (uMsg==WM_INITDIALOG)
		{
			SetWindowText(hWnd,"Colors");
		}
		SetDlgItemInt(hWnd,IDC_EDIT1,g_SFT.GetNumOfColors(),FALSE);
		SetDlgItemInt(hWnd,IDC_EDIT2,g_SFT.GetSeed(),FALSE);
		snprintf(szTmp,256,"%.12g",g_SFT.GetIterDiv());
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
		SetDlgItemFloat(hWnd, IDC_COLOR_PHASE_STRENGTH, g_SFT.GetPhaseColorStrength());
		SendDlgItemMessage(hWnd,IDC_COLOR_TRANSITION_FLAT,BM_SETCHECK,g_SFT.GetFlat(),0);
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
				SendDlgItemMessage(hWnd,IDC_COMBO1,CB_ADDSTRING,0,(LPARAM)"LogLog");
				SendDlgItemMessage(hWnd,IDC_COMBO1,CB_ADDSTRING,0,(LPARAM)"ATan");
				SendDlgItemMessage(hWnd,IDC_COMBO1,CB_ADDSTRING,0,(LPARAM)"Fourth root");
			}

			if(SendDlgItemMessage(hWnd,IDC_DIFFERENCES,CB_GETCOUNT,0,0)==0){
				SendDlgItemMessage(hWnd,IDC_DIFFERENCES,CB_ADDSTRING,0,(LPARAM)"Traditional");
				SendDlgItemMessage(hWnd,IDC_DIFFERENCES,CB_ADDSTRING,0,(LPARAM)"Forward 3x3");
				SendDlgItemMessage(hWnd,IDC_DIFFERENCES,CB_ADDSTRING,0,(LPARAM)"Central 3x3");
				SendDlgItemMessage(hWnd,IDC_DIFFERENCES,CB_ADDSTRING,0,(LPARAM)"Diagonal 2x2");
				SendDlgItemMessage(hWnd,IDC_DIFFERENCES,CB_ADDSTRING,0,(LPARAM)"Least Squares 2x2");
				SendDlgItemMessage(hWnd,IDC_DIFFERENCES,CB_ADDSTRING,0,(LPARAM)"Least Squares 3x3");
				SendDlgItemMessage(hWnd,IDC_DIFFERENCES,CB_ADDSTRING,0,(LPARAM)"Laplacian 3x3");
				SendDlgItemMessage(hWnd,IDC_DIFFERENCES,CB_ADDSTRING,0,(LPARAM)"Analytic");
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

		RECT r =
		  { g_rShow.right + g_rShow.left - 96
			, g_rShow.top - 32
			, g_rShow.right + g_rShow.left
			, g_rShow.top - 16
			};
    COLOR14 c = g_SFT.GetInteriorColor();
    HBRUSH br = CreateSolidBrush(RGB(c.b, c.g, c.r));
    FillRect(ps.hdc, &r, br);
    DeleteObject(br);

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
			g_AutoUpdate++;
			SendMessage(hWnd,WM_COMMAND,IDOK,0);
			g_AutoUpdate--;
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
			char buffer[1024] = {0};
			GetDlgItemText(hWnd,IDC_EDIT29,buffer,sizeof(buffer));
			std::string szFile = buffer;
			if(BrowseFile(hWnd,TRUE,"Select texture","Jpg\0*.jpg\0\0",szFile)){
				SetDlgItemText(hWnd,IDC_EDIT29,szFile.c_str());
				g_AutoUpdate++;
				SendMessage(hWnd,WM_COMMAND,IDOK,0);
				g_AutoUpdate--;
			}
		}
		else if (wParam == IDC_AUTOCOLOUR)
		{
			g_AutoColour = SendDlgItemMessage(hWnd, IDC_AUTOCOLOUR, BM_GETCHECK, 0, 0);
			if (g_AutoColour)
			{
				g_AutoUpdate++;
				SendMessage(hWnd, WM_COMMAND, IDOK, 0);
				g_AutoUpdate--;
			}
		}
		else if(wParam==IDOK){
			if (! g_AutoUpdate)
				g_SFT.UndoStore();
			char szTexture[1024];
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
			g_SFT.SetPhaseColorStrength(GetDlgItemFloat(hWnd, IDC_COLOR_PHASE_STRENGTH));
			g_SFT.SetFlat(SendDlgItemMessage(hWnd,IDC_COLOR_TRANSITION_FLAT,BM_GETCHECK,0,0));
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

			if (g_AutoColour || ((! g_AutoColour) && g_AutoUpdate == 0))
			{
				g_SFT.ApplyColors();
				InvalidateRect(GetParent(hWnd),NULL,FALSE);
				UpdateWindow(GetParent(hWnd));
			}
			InvalidateRect(hWnd,NULL,FALSE);
			g_bInitColorDialog=TRUE;
		}
		else if(wParam==IDC_COLOR_TRANSITION_FLAT || wParam==IDC_COLOR_PHASE_STRENGTH || wParam==IDC_CHECK2 || wParam==IDC_CHECK3 || wParam==IDC_CHECK4)
		{
			g_AutoUpdate++;
			SendMessage(hWnd,WM_COMMAND,IDOK,0);
			g_AutoUpdate--;
		}
		else if(wParam==IDC_BUTTON1){
			std::string szFile;
			if(BrowseFile(hWnd,FALSE,"Save palette","Palette\0*.kfp\0\0",szFile))
			{
				g_SFT.SaveFile(szFile, true);
				char szTitle[1024];
				snprintf(szTitle, sizeof(szTitle), "Colors - %s", get_filename_file(szFile).c_str());
				SetWindowText(hWnd, szTitle);
			}
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
			if (g_AutoColour) g_SFT.ApplyColors();
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
			if (g_AutoColour) g_SFT.ApplyColors();
		}
		else if(wParam==IDC_BUTTON7){
			int i;
			if (g_AutoColour) g_SFT.ApplyColors();
			COLOR14 c[1024];
			for(i=0;i<1024;i++)
				c[i] = g_SFT.GetColor(i);
			g_SFT.ChangeNumOfColors(1024);
			for(i=0;i<1024;i++)
				g_SFT.SetKeyColor(c[i],i);
			SendMessage(hWnd,WM_USER+99,0,0);
			if (g_AutoColour) g_SFT.ApplyColors();
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
			g_AutoUpdate++;
			SendMessage(hWnd,WM_COMMAND,IDOK,0);
			g_AutoUpdate--;
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
			g_AutoUpdate++;
			SendMessage(hWnd,WM_COMMAND,IDOK,0);
			g_AutoUpdate--;
		}
		else if(wParam==IDC_BUTTON22){
			std::string szFile;
			if(!BrowseFile(hWnd,TRUE,"Open image","Supported Images\0*.bmp;*.gif;*.jpeg;*.jpg;*.png\0\0",szFile))
				return 0;
			HBITMAP bmBmp = GetImage(szFile.c_str());
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

			int nParts = std::min(std::max(bmi.biWidth, bmi.biHeight), 1024L);
			double woffs = (double) bmi.biWidth  / (double) nParts;
			double hoffs = (double) bmi.biHeight / (double) nParts;
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
			SetDlgItemInt(hWnd,IDC_EDIT1,g_SFT.GetNumOfColors(),FALSE);
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
			std::string szFile;
			if(BrowseFile(hWnd,TRUE,"Open palette","Palette\0*.kfp\0\0",szFile)){
				g_SFT.OpenFile(szFile, TRUE);
				char szTitle[1024];
				snprintf(szTitle, sizeof(szTitle), "Colors - %s", get_filename_file(szFile).c_str());
				SetWindowText(hWnd, szTitle);
				SendMessage(hWnd,WM_USER+99,0,0);
				if (g_AutoColour) g_SFT.ApplyColors();
				g_AutoUpdate++;
				SendMessage(hWnd,WM_COMMAND,IDOK,0);
				g_AutoUpdate--;
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
			if (g_AutoColour) g_SFT.ApplyColors();
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

			if (g_AutoColour) g_SFT.ApplyColors();
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
			if (g_AutoColour) g_SFT.ApplyColors();
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
			if (g_AutoColour) g_SFT.ApplyColors();
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
			if (g_AutoColour) g_SFT.ApplyColors();
		}
		else if(wParam==IDC_INTERIORCOLOR){
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
				g_SFT.SetInteriorColor(c);
			}
			SendMessage(hWnd,WM_USER+99,0,0);
			if (g_AutoColour) g_SFT.ApplyColors();
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
			if (g_AutoColour) g_SFT.ApplyColors();
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
			if (g_AutoColour) g_SFT.ApplyColors();
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
		else if (wParam == IDC_COLOR_OPENGL)
		{
			if (! g_hwColorsOpenGL)
			{
				g_hwColorsOpenGL = CreateDialog(GetModuleHandle(NULL), MAKEINTRESOURCE(IDD_OPENGL), hWnd, (DLGPROC) ColorOpenGLProc);
			}
			ShowWindow(g_hwColorsOpenGL, SW_SHOW);
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
  else if(uMsg==WM_DROPFILES)
  {
		HDROP hDrop = (HDROP) wParam;
		if (hDrop)
		{
			UINT len = DragQueryFile(hDrop, 0, 0, 0);
			if (len > 0)
			{
				char *buffer = (char *) calloc(1, 2 * len + 1);
				if (buffer)
				{
					UINT ok = DragQueryFile(hDrop, 0, buffer, 2 * len);
					if (ok)
					{
						std::string file(buffer);
						g_SFT.OpenFile(file, TRUE);
						char szTitle[1024];
						snprintf(szTitle, sizeof(szTitle), "Colors - %s", get_filename_file(file).c_str());
						SetWindowText(hWnd, szTitle);
						SendMessage(hWnd,WM_USER+99,0,0);
						if (g_AutoColour) g_SFT.ApplyColors();
						g_AutoUpdate++;
						SendMessage(hWnd,WM_COMMAND,IDOK,0);
						g_AutoUpdate--;
					}
					free(buffer);
				}
			}
			DragFinish(hDrop);
		}
	}

	if(g_pWaves)
		g_pWaves->ProcessMessage(hWnd,uMsg,wParam,lParam);
	if(uMsg==WM_COMMAND && (wParam==IDC_BUTTON26 || wParam==IDC_BUTTON27 || wParam==IDC_BUTTON28))
	{
		g_AutoUpdate++;
		SendMessage(hWnd,WM_COMMAND,IDOK,0);
		g_AutoUpdate--;
	}
	return 0;
}
