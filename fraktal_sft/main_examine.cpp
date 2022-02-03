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

#include "main.h"
#include "main_examine.h"
#include "fraktal_sft.h"
#include "resource.h"
#include "tooltip.h"
#include "../common/FolderBrowser.h"

static int g_nPrevAutoGlitchNP;
static int g_bAutoSolveGlitch=0;
static int g_nAutoSolveGlitchLimit=10;

static std::string g_szExamine;
static std::vector<std::string> g_stExamine;
static int g_nExamine=-1;
static int g_nExamineZoom=-1;

HWND g_hwExamine=NULL;

bool g_bExamineDirty=false;

static int kGetDlgItemInt(HWND hWnd,int nID)
{
	char szText[256];
	GetDlgItemText(hWnd,nID,szText,sizeof(szText));
	return atoi(szText);
}

static void kSetDlgItemInt(HWND hWnd,int nID,int val)
{
	char szText[256];
	sprintf(szText,"%d",val);
	SetDlgItemText(hWnd,nID,szText);
}

extern bool Examine(HWND hWnd)
{
	g_szExamine = "";
	if(!BrowseFile(hWnd,TRUE,"Open location","Kalle's fraktaler\0*.kfr\0\0",g_szExamine))
		return false;
	if(g_hwExamine)
		SetFocus(g_hwExamine);
	else{
		g_hwExamine = CreateDialog(GetModuleHandle(NULL),MAKEINTRESOURCE(IDD_DIALOG8),hWnd,(DLGPROC)ExamineProc);
		ShowWindow(g_hwExamine,SW_SHOW);
	}
	return true;
}

static std::vector<HWND> tooltips;

extern int WINAPI ExamineProc(HWND hWnd,UINT uMsg,WPARAM wParam,LPARAM lParam)
{
	(void) lParam;
	if(uMsg==WM_INITDIALOG){
		SendMessage(hWnd, WM_SETICON, ICON_SMALL, LPARAM(g_hIcon));
		SendMessage(hWnd, WM_SETICON, ICON_BIG, LPARAM(g_hIcon));

#define T(idc,str) tooltips.push_back(CreateToolTip(idc, hWnd, str));
		T(IDC_RADIO1, "Add Reference\nclick in the view to add additional references")
		T(IDC_RADIO2, "Set main reference.\nThe whole image will be rendered")
		T(IDC_RADIO3, "Erase specific parts of the view by clicking\nAfter erased add references in the erased areas")
		T(IDC_BUTTON1, "Save eventual changes and go to previous Key Frame")
		T(IDOK, "Save eventual changes and go to next Key Frame")
		T(IDC_BUTTON5, "Save eventual changes and go to previous Key Frame\nThe current frame will be applied on the center of the previous frame")
		T(IDC_BUTTON2, "Undo all changes on current Key Frame")
		T(IDC_EDIT1, "Current Key Frame index")
		T(IDC_BUTTON3, "Jump to specified Key Frame index")
		T(IDCANCEL, "Close this dialog")
		T(IDC_EDIT2, "Show if current Key Frame is changed")
		T(IDC_EDIT3, "Status of reading/saving current Key Frame")
		T(IDC_BUTTON4, "Automatically solve glitches in Key Frames\nThe Key Frames will be browsed backwards\nand stop on the first frame\nStart this function preferable from the last frame")
		T(IDC_EDIT4, "Shows status of automatically glitch solving")
		T(IDC_EDITMAXREFS, "Choose maximum number of references to add per frame when solving glitches")
#undef T

		g_nPrevAutoGlitchNP=g_SFT.GetSolveGlitchNear();
		g_SFT.SetSolveGlitchNear(false);
		CheckMenuItem(GetMenu(GetParent(hWnd)),ID_ACTIONS_SPECIAL_SOLVEGLITCHWITHNEARPIXELSMETHOD,MF_BYCOMMAND|(g_SFT.GetSolveGlitchNear()?MF_CHECKED:MF_UNCHECKED));
		g_bAddReference=TRUE;
		g_bEraser=FALSE;
		g_stExamine.resize(0);
		SendDlgItemMessage(hWnd,IDC_RADIO1,BM_SETCHECK,1,0);
		WIN32_FIND_DATA fd;
		std::string szExamine = replace_path_filename(g_szExamine, "*_*.kfb");
		HANDLE hFind = FindFirstFile(szExamine.c_str(),&fd);
		if(hFind==INVALID_HANDLE_VALUE){
			if(hFind)
				FindClose(hFind);
			MessageBox(hWnd,"Could not browse kfb files","Error",MB_OK|MB_ICONSTOP);
			DestroyWindow(hWnd);
			g_hwExamine=NULL;
			return 0;
		}
		do{
			szExamine = replace_path_filename(szExamine, fd.cFileName);
			g_stExamine.push_back(szExamine);
		}while(FindNextFile(hFind,&fd));
		FindClose(hFind);
		std::sort(g_stExamine.begin(), g_stExamine.end());
		std::reverse(g_stExamine.begin(), g_stExamine.end());
		g_SFT.OpenFile(g_szExamine);
		g_SFT.OpenMapB(g_stExamine[0]);
		g_SFT.ApplyColors();
		g_nExamine=0;
		SetDlgItemInt(hWnd,IDC_EDIT1,g_nExamine,FALSE);
		g_bExamineDirty=FALSE;
		int last = g_stExamine.size() - 1;
		CDecNumber A(get_filename_zoom_string(g_stExamine[last]));
		CDecNumber B(get_filename_zoom_string(g_stExamine[last - 1]));
		g_nExamineZoom = (A/B+CDecNumber(0.5)).ToInt();
		A = CDecNumber(g_SFT.GetZoom())/(CDecNumber(g_nExamineZoom)^(g_stExamine.size()-g_nExamine-1));
		std::string szRe = g_SFT.GetRe();
		std::string szIm = g_SFT.GetIm();
		g_SFT.SetPosition(szRe,szIm,A.ToText());

		PostMessage(GetParent(hWnd),WM_USER+199,0,0);
		SetTimer(hWnd,0,100,NULL);
	}
	else if(uMsg==WM_COMMAND){
		if(wParam==IDCANCEL){
			for (auto tooltip : tooltips)
      {
        DestroyWindow(tooltip);
      }
      tooltips.clear();
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
				g_SFT.SaveMapB(g_stExamine[g_nExamine]);
				std::string szFile = replace_path_extension(g_stExamine[g_nExamine], "jpg");
				if(FileExists(szFile))
					g_SFT.SaveJpg(szFile,100);
			}
			if(wParam==IDC_BUTTON5)
				g_bExamineDirty=TRUE;
			else
				g_bExamineDirty=FALSE;
			if(wParam==IDOK){
				g_nExamine++;
				if(g_nExamine>=ssize_t(g_stExamine.size()))
					g_nExamine=0;
			}
			else if(wParam==IDC_BUTTON1 || wParam==IDC_BUTTON5){
				g_nExamine--;
				if(g_nExamine<0){
					wParam=IDC_BUTTON1;
					g_nExamine=g_stExamine.size()-1;
					g_bExamineDirty=FALSE;
				}
			}
			SetDlgItemInt(hWnd,IDC_EDIT1,g_nExamine,FALSE);
			UpdateWindow(GetDlgItem(hWnd,IDC_EDIT1));
			g_SFT.OpenMapB(g_stExamine[g_nExamine],wParam==IDC_BUTTON5,(double)1/g_nExamineZoom);
			g_SFT.ApplyColors();
			if(wParam!=IDC_BUTTON2)
				SetTimer(hWnd,1,500,NULL);
			InvalidateRect(GetParent(hWnd),NULL,FALSE);
			UpdateWindow(GetParent(hWnd));
//			SetCapture(hWnd);
			g_bWaitRead=true;
		}
		else if(wParam==IDC_BUTTON3){
			SetDlgItemText(hWnd,IDC_EDIT3,"Reading...");
			g_SFT.Stop();
			g_bAnim=FALSE;
			KillTimer(hWnd,1);
			g_nExamine = GetDlgItemInt(hWnd,IDC_EDIT1,NULL,FALSE);
			if(g_nExamine<0)
				g_nExamine=0;
			else if(g_nExamine>=ssize_t(g_stExamine.size()))
				g_nExamine = g_stExamine.size()-1;
			g_SFT.OpenMapB(g_stExamine[g_nExamine],wParam==IDC_BUTTON5,(double)1/g_nExamineZoom);
			g_SFT.ApplyColors();
			SetTimer(hWnd,1,500,NULL);
			InvalidateRect(GetParent(hWnd),NULL,FALSE);
			UpdateWindow(GetParent(hWnd));
			g_bWaitRead=true;
		}
		else if(wParam==IDC_BUTTON4){
			if(!g_SFT.GetAutoSolveGlitches())
			{
				g_bAutoSolveGlitch = 1;
				SetTimer(hWnd,2,10,NULL);
			}
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
			g_bAddMainReference=false;
			g_bEraser=FALSE;
			EnableWindow(GetDlgItem(hWnd,IDC_CHECK1),TRUE);
		}
		else if(wParam==IDC_RADIO2){
			g_bAddReference=FALSE;
			g_bAddMainReference=true;
			g_bEraser=FALSE;
			EnableWindow(GetDlgItem(hWnd,IDC_CHECK1),FALSE);
		}
		else if(wParam==IDC_RADIO3){
			g_bEraser=TRUE;
			g_bAddReference=FALSE;
			g_bAddMainReference=false;
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
		g_bWaitRead=false;
		SetDlgItemText(hWnd,IDC_EDIT3,"");
		KillTimer(hWnd,1);
		ReleaseCapture();
		int nMI = g_SFT.GetIterations();
		g_SFT.OpenFile(g_szExamine);
		g_SFT.SetIterations(nMI);
		CDecNumber A = CDecNumber(g_SFT.GetZoom())/(CDecNumber(g_nExamineZoom)^(g_stExamine.size()-g_nExamine-1));
		std::string szRe = g_SFT.GetRe();
		std::string szIm = g_SFT.GetIm();
		g_SFT.SetPosition(szRe,szIm,A.ToText());

		PostMessage(GetParent(hWnd),WM_USER+199,0,0);
	}
	else if(uMsg==WM_TIMER && wParam==2){
		KillTimer(hWnd,2);
		if(!g_bAutoSolveGlitch)
			return 0;
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
			CDecNumber A = CDecNumber(g_SFT.GetZoom())/(CDecNumber(g_nExamineZoom)^(g_stExamine.size()-g_nExamine-1));
			std::string szRe = g_SFT.GetRe();
			std::string szIm = g_SFT.GetIm();
			g_SFT.SetPosition(szRe,szIm,A.ToText());
		}

char szAdd[128];
wsprintf(szAdd,"Add reference %d",g_bAutoSolveGlitch);
SetDlgItemText(hWnd,IDC_EDIT4,szAdd);
UpdateWindow(GetDlgItem(hWnd,IDC_EDIT4));
		g_SFT.AddReference(rx,ry);
		g_SFT.Render(false, false);
		g_bAutoSolveGlitch++;
		g_bExamineDirty=TRUE;
	}
	else if(uMsg==WM_USER+199 && g_bAutoSolveGlitch){
		SendMessage(hWnd,WM_TIMER,2,0);
	}
	return 0;
}
