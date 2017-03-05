#include <windows.h>
#include <stdio.h>
#include <string.h>
#include <memory.h>
#include "tooltip.h"
#include "StringVector.h"

HWND hToolTip;
HWND hSkugga;
HFONT hfFont;
BOOL bFirst=TRUE;
LPGETTEXT g_lpfnGetText;
LPARAM g_lParam;
BOOL g_bCenter=0;
CStringTable g_stWindows;

#ifdef _WIN64
#define GCL_WNDPROC -24
#define GWL_WNDPROC -4
#define GWL_USERDATA -21
#endif

typedef void  (__stdcall *lpfnSetLayeredWindowAttributes)(HWND,int,int,int);
lpfnSetLayeredWindowAttributes g_lpfnSetLayeredWindowAttributes=NULL;

BOOL IsActiveApp(HWND hWnd)
{
	BOOL bActive=FALSE;

	hWnd=GetParent(hWnd);
	while(hWnd && !bActive){
		if(GetActiveWindow()==hWnd)
			bActive=TRUE;
		else
			hWnd=GetParent(hWnd);
	}
	return bActive;
}

void SetRegions(HWND hWnd)
{
	HRGN hRgn1, hRgn2, hRgn3;
	RECT r;
	POINT p[3];
	GetWindowRect(hWnd,&r);
	hRgn1 = CreateRoundRectRgn(0,10,r.right-r.left+1,r.bottom-r.top+1,35,35);
	p[0].x=0;	p[0].y=0;
	p[1].x=3;	p[1].y=20;
	p[2].x=15;	p[2].y=15;
	hRgn2 = CreatePolygonRgn(p,3,WINDING);
	hRgn3 = CreateRectRgn(0,0,0,0);
	CombineRgn(hRgn3,hRgn1,hRgn2,RGN_OR);
	SetWindowRgn(hWnd,hRgn3,TRUE);
	DeleteObject(hRgn1);
	DeleteObject(hRgn2);
}

char szTemp[15120];
long WINAPI SkuggaProc(HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	PAINTSTRUCT ps;
	HDC hDC;
	RECT r;
//	int i;
	HBRUSH br;
	HBITMAP bmo, bm;

	if(uMsg==WM_CREATE){
		SetWindowLong( hWnd, GWL_EXSTYLE, GetWindowLong(hWnd, GWL_EXSTYLE) | 0x00080000/*WS_EX_LAYERED*/);
		g_lpfnSetLayeredWindowAttributes(hWnd, 0, 100, 2/*LWA_ALPHA*/);
	}
	else if(uMsg==WM_PAINT){
		if(g_lpfnSetLayeredWindowAttributes)
			br=CreateSolidBrush(RGB(0,0,0));
		else
			br=CreateSolidBrush(RGB(111,111,111));
		GetClientRect(hWnd,&r);
		BeginPaint(hWnd,&ps);
		hDC = CreateCompatibleDC(ps.hdc);
		bm=CreateCompatibleBitmap(ps.hdc,r.right,r.bottom);
		bmo=(HBITMAP)SelectObject(hDC,bm);
		if(g_lpfnSetLayeredWindowAttributes){
			int a = r.bottom;
			int nCol = 180;
			for(;r.bottom>a/2;r.bottom--){
				DeleteObject(br);
				br = CreateSolidBrush(RGB(nCol,nCol,nCol));
				nCol-=20;
				FillRect(ps.hdc,&r,br);
				r.right--;
				r.left++;
				r.top++;
				if(!nCol)
					break;
			}
		}
		else{
			FillRect(hDC,&r,br);
			BitBlt(ps.hdc,0,0,r.right,r.bottom,hDC,0,0,SRCAND);
		}
		SelectObject(hDC,bmo);
		DeleteObject(bm);
		DeleteObject(br);
		DeleteDC(hDC);
		EndPaint(hWnd,&ps);
/*		GetClientRect(hWnd,&r);
		hDC=BeginPaint(hWnd,&ps);
		for(i=-r.bottom;i<r.right;i+=2){
			MoveToEx(hDC,i,0,NULL);
			LineTo(hDC,i+r.bottom,r.bottom);
		}
		EndPaint(hWnd,&ps);
*/	}

	return DefWindowProc(hWnd,uMsg,wParam,lParam);
}
BOOL g_bShow=FALSE;
int g_nPrevIndex=-1;
long WINAPI ToolTipProc(HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	char szTemp[15120];
	PAINTSTRUCT ps;
	HDC hDC;
	HFONT hfOld;
	RECT r;
	HRGN rgn;

	if(uMsg==WM_PAINT){
		SendMessage(hWnd,WM_GETTEXT,sizeof(szTemp),(LPARAM)szTemp);
		hDC=BeginPaint(hWnd,&ps);
		hfOld=(HFONT)SelectObject(hDC,(HFONT)GetStockObject(ANSI_VAR_FONT));
		SetTextAlign(hDC,TA_LEFT|TA_TOP);
		SetBkMode(hDC,TRANSPARENT);
		GetClientRect(hWnd,&r);
		r.top=14;
		DrawText(hDC,szTemp,strlen(szTemp),&r,DT_CENTER|DT_TOP|DT_EXPANDTABS);
		SelectObject(hDC,hfOld);
		rgn = CreateRectRgn(0,0,0,0);
		GetWindowRgn(hWnd,rgn);
		OffsetRgn(rgn,-1,-1);
		FrameRgn(hDC,rgn,(HBRUSH)GetStockObject(BLACK_BRUSH),1,1);
		DeleteObject(rgn);
		GetClientRect(hWnd,&r);
		MoveToEx(ps.hdc,0,r.bottom-1,NULL);
		LineTo(ps.hdc,r.right,r.bottom-1);
		EndPaint(hWnd,&ps);
	}
	if(uMsg==WM_TIMER){
		POINT p;
		GetCursorPos(&p);
		char szTmp[256];
		HWND hw = WindowFromPoint(p);
		wsprintf(szTmp,"%d",(int)hw);
		int n=-1;
//		if(IsActiveApp(hWnd))
			n = g_stWindows.FindStringHash(0,szTmp);
		char *sz;
		if(n!=-1){
			sz = g_lpfnGetText(GetDlgCtrlID(hw),atoi(g_stWindows[n][2]));
			if(sz && *sz)
				strcpy(szTemp,sz);
			else
				n=-1;
		}
		if(g_nPrevIndex!=-1 && g_nPrevIndex!=n)
			n=-1;
		if(n==-1){
			if(g_bShow){
				g_nPrevIndex=-1;
				g_bShow=FALSE;
				ShowWindow(hToolTip,SW_HIDE);
				ShowWindow(hSkugga,SW_HIDE);
			}
			return DefWindowProc(hWnd,uMsg,wParam,lParam);
		}

		if(g_nPrevIndex==-1){
			g_bShow=TRUE;
			g_nPrevIndex=n;

			hDC=GetDC(hToolTip);
			hfOld=(HFONT)SelectObject(hDC,hfFont);
			memset(&r,0,sizeof(RECT));
			DrawText(hDC,szTemp,strlen(szTemp),&r,DT_CALCRECT);
			SelectObject(hDC,hfOld);
			ReleaseDC(hToolTip,hDC);

			RECT wr;
			GetClientRect(GetDesktopWindow(),&wr);
			if(p.x+15+r.right+12>wr.right)
				p.x=wr.right-15-r.right-20;
			if(p.y+15+r.bottom+12>wr.bottom)
				p.y=wr.bottom-15-r.bottom-30;
			SetWindowText(hToolTip,szTemp);
			SetWindowPos(hSkugga,HWND_TOPMOST,p.x+15+10,p.y+15+4,r.right+20-4,r.bottom+20,SWP_NOACTIVATE);
			SetRegions(hSkugga);
			ShowWindow(hSkugga,SW_SHOWNA);
			SetWindowPos(hToolTip,HWND_TOPMOST,p.x+15,p.y+15,r.right+20,r.bottom+20,SWP_NOACTIVATE);
			SetRegions(hToolTip);
			ShowWindow(hToolTip,SW_SHOWNA);
		}
	}

	return DefWindowProc(hWnd,uMsg,wParam,lParam);
}
void ErrorText(DWORD dwError)  
{     
	TCHAR szMsgBuf[500];      

	dwError = GetLastError();         
	FormatMessage(         
		FORMAT_MESSAGE_FROM_SYSTEM,         
		NULL,                               
		dwError,                     
		MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),   
		szMsgBuf,                   
		500,                                  
		NULL );                              
	MessageBox(NULL,szMsgBuf,"Error",MB_ICONSTOP|MB_OK);
}  
void CollectObjects(HWND hwNext,HWND hwParent,LPARAM lParam)
{
	hwNext=GetWindow(hwNext,GW_CHILD);
	while(hwNext){
		g_stWindows.AddRow();
		g_stWindows.AddInt(g_stWindows.GetCount()-1,(int)hwNext);
		g_stWindows.AddInt(g_stWindows.GetCount()-1,(int)hwParent);
		g_stWindows.AddInt(g_stWindows.GetCount()-1,(int)lParam);
		CollectObjects(hwNext,hwParent,lParam);
		hwNext=GetWindow(hwNext,GW_HWNDNEXT);
	}
}

BOOL InitToolTip(HWND hWnd,HINSTANCE hInst,LPGETTEXT lpfnGetText,LPARAM lParam,BOOL bCenter,char *szFont, int nFont)
{
	if(!g_lpfnSetLayeredWindowAttributes){
		HINSTANCE hMod = LoadLibrary("User32.dll");
		g_lpfnSetLayeredWindowAttributes = 
			(lpfnSetLayeredWindowAttributes)GetProcAddress(hMod,"SetLayeredWindowAttributes");
	}
	g_bCenter = bCenter;
	g_lpfnGetText = lpfnGetText;
	g_lParam = lParam;
	WNDCLASS wc;
	HBRUSH brGul;
	if(szFont){
		hfFont = CreateFont(nFont,0,0,0,0,0,0,0,0,0,0,0,0,szFont);
	}
	else
		hfFont = (HFONT)GetStockObject(ANSI_VAR_FONT);

	if(bFirst){
		bFirst=FALSE;
		brGul=CreateSolidBrush(RGB(255,255,200));
		memset(&wc,0,sizeof(WNDCLASS));
		wc.lpfnWndProc=(WNDPROC)ToolTipProc;
		wc.hInstance=(HINSTANCE)hInst;
		wc.hCursor= LoadCursor(NULL,IDC_ARROW);
		wc.hbrBackground=brGul;
		wc.lpszClassName="TOOLTIP";
		HWND hwParent = hWnd;
		while(GetParent(hwParent))
			hwParent = GetParent(hwParent);
		if(RegisterClass(&wc))
			hToolTip=CreateWindow("TOOLTIP","",WS_POPUP|WS_BORDER,0,0,120,120,hwParent,NULL,(HINSTANCE)hInst,0);
		//SetWindowPos(hToolTip,HWND_TOPMOST,0,0,0,0,SWP_NOMOVE|SWP_NOSIZE);
		wc.lpfnWndProc=(WNDPROC)SkuggaProc;
		wc.hbrBackground=NULL;
		wc.lpszClassName="SKUGGA";
		if(RegisterClass(&wc))
			hSkugga=CreateWindow("SKUGGA","",WS_POPUP,0,0,120,120,hwParent,NULL,hInst,0);
		//SetWindowPos(hSkugga,hToolTip,0,0,0,0,SWP_NOMOVE|SWP_NOSIZE);
	}
	CollectObjects(hWnd,hWnd,lParam);
	g_stWindows.BuildHash(0);
	SetTimer(hToolTip,0,100,NULL);
	return TRUE;
}
void ExitToolTip(HWND hWnd)
{
	int i;
	for(i=0;i<g_stWindows.GetCount();i++){
		if(atoi(g_stWindows[i][1])==(int)hWnd)
			g_stWindows.DeleteRow(i--);
	}
}