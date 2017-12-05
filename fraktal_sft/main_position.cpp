#include "main.h"
#include "main_position.h"
#include "fraktal_sft.h"
#include "resource.h"
#include "../common/tooltip.h"

static void FixNumber(char *sz)
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

extern int WINAPI PositionProc(HWND hWnd,UINT uMsg,WPARAM wParam,LPARAM lParam)
{
	(void) lParam;
	if(uMsg==WM_INITDIALOG){
		SendMessage(hWnd, WM_SETICON, ICON_SMALL, LPARAM(g_hIcon));
		SendMessage(hWnd, WM_SETICON, ICON_BIG, LPARAM(g_hIcon));
		InitToolTip(hWnd,GetModuleHandle(NULL),GetToolText,2);
		SendDlgItemMessage(hWnd,IDC_EDIT1,EM_SETLIMITTEXT,0,0);
		SendDlgItemMessage(hWnd,IDC_EDIT3,EM_SETLIMITTEXT,0,0);
    std::string re = g_SFT.GetRe();
    std::string im = g_SFT.GetIm();
    std::string z = g_SFT.GetZoom();
		SetDlgItemText(hWnd,IDC_EDIT1,re.c_str());
		SetDlgItemText(hWnd,IDC_EDIT3,im.c_str());
		SetDlgItemText(hWnd,IDC_EDIT4,z.c_str());
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

extern const char *PositionToolTip(int nID)
{
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
  default:
    static char tooltip[100];
    snprintf(tooltip, 100, "%d", nID);
    return tooltip;
  }
}
