/*
Kalles Fraktaler 2
Copyright (C) 2013-2017 Karl Runmo
Copyright (C) 2017-2018 Claude Heiland-Allen

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
#include "main_position.h"
#include "fraktal_sft.h"
#include "resource.h"
#include "newton.h"
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
		int64_t nMin, nMax;
		g_SFT.GetIterations(nMin,nMax);
		char s[100];
		snprintf(s, 100, "%lld", nMin);
		SetDlgItemText(hWnd,IDC_EDIT2,s);
		snprintf(s, 100, "%lld", nMax);
		SetDlgItemText(hWnd,IDC_EDIT5,s);
		snprintf(s, 100, "%lld", g_period);
		SetDlgItemText(hWnd,IDC_LOCATION_PERIOD,s);
		return 1;
	}
	else if(uMsg==WM_COMMAND){
		if(wParam==IDOK){
			g_SFT.UndoStore();
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
			n = GetWindowTextLength(GetDlgItem(hWnd,IDC_EDIT4));
			char *szP = new char[n+1];
			GetDlgItemText(hWnd,IDC_LOCATION_PERIOD,szP,n+1);
			g_period = atoll(szP);
			g_SFT.SetPosition(szR,szI,szZ);
			delete [] szR;
			delete [] szI;
			delete [] szZ;
			delete [] szP;
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
  case IDC_LOCATION_PERIOD:
    return "Display period of last Newton zoom, sets limit for NanoMB2";
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
