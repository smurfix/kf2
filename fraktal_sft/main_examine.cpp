#include "main.h"
#include "main_examine.h"
#include "fraktal_sft.h"
#include "resource.h"
#include "../common/tooltip.h"
#include "../common/StringVector.h"
#include "../common/FolderBrowser.h"

static int g_nPrevAutoGlitchNP;
static int g_bAutoSolveGlitch=0;
static int g_nAutoSolveGlitchLimit=10;

static char g_szExamine[256];
static CStringTable g_stExamine;
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
	memset(g_szExamine,0,sizeof(g_szExamine));
	if(!BrowseFile(hWnd,TRUE,"Open location","Kalle's fraktaler\0*.kfr\0\0",g_szExamine,sizeof(g_szExamine)))
		return false;
	if(g_hwExamine)
		SetFocus(g_hwExamine);
	else{
		g_hwExamine = CreateDialog(GetModuleHandle(NULL),MAKEINTRESOURCE(IDD_DIALOG8),hWnd,(DLGPROC)ExamineProc);
		ShowWindow(g_hwExamine,SW_SHOW);
	}
	return true;
}

extern int WINAPI ExamineProc(HWND hWnd,UINT uMsg,WPARAM wParam,LPARAM lParam)
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
			else if(g_nExamine>=g_stExamine.GetCount())
				g_nExamine = g_stExamine.GetCount()-1;
			g_SFT.OpenMapB(g_stExamine[g_nExamine][0],wParam==IDC_BUTTON5,(double)1/g_nExamineZoom);
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

extern const char *ExamineToolTip(int nID)
{
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
  default:
    static char tooltip[100];
    snprintf(tooltip, 100, "%d", nID);
    return tooltip;
  }
}
