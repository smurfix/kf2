// Kalles Fraktaler 2
//
// © 2014 Karl Runmo ,runmo@hotmail.com
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
#include <commctrl.h>
#include <shlobj.h>
#include "FolderBrowser.h"

char g_szFolder[MAX_PATH]={0};
char g_szValue[MAX_PATH]={0};
char *g_szLabel;
int g_nInitialized;

#ifdef _WIN64
#define GCL_WNDPROC -24
#define GWL_WNDPROC -4
#endif

BOOL SkapaKatalog(char *szFile)
{
	char *s = strrchr(szFile,'\\') ,*ss = strrchr(szFile,'/');

	BOOL bRet=TRUE;
	if(!s || (ss && ss>s))
		s = ss;
	if(!s)
		return FALSE;
	*s=0;
	if(!CreateDirectory(szFile,NULL)){
		SkapaKatalog(szFile);
		bRet = CreateDirectory(szFile,NULL);
	}
	*s='\\';
	return bRet;
}
int ValidatePath(char *szPath)
{
	if(!szPath)
		return 0;
	char szFind[MAX_PATH];
	strcpy(szFind,szPath);
	if(szFind[strlen(szFind)-1]!='\\')
		strcat(szFind,"\\");
	strcat(szFind,"*.*");
	WIN32_FIND_DATA wf;
	HANDLE hFind = FindFirstFile(szFind,&wf);
	if(hFind==INVALID_HANDLE_VALUE)
		return 0;
	FindClose(hFind);
	return 1;
}
int WINAPI lpfnEdit(HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	if(uMsg==WM_DESTROY)
		GetWindowText(hWnd,g_szFolder,MAX_PATH);
	return CallWindowProc((WNDPROC)GetClassLongPtr(hWnd,GCLP_WNDPROC),hWnd,uMsg,wParam,lParam);
}
int WINAPI lpfnCallBack(HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	HWND hEdit;

	if(uMsg==BFFM_INITIALIZED){
		hEdit=CreateWindowEx(WS_EX_CLIENTEDGE,"edit",NULL,WS_CHILD|WS_VISIBLE|ES_AUTOHSCROLL|WS_TABSTOP,50,10,257,20,hWnd,(HMENU)1221,GetModuleHandle(NULL),0);
		SetWindowLongPtr(hEdit,GWLP_WNDPROC,(LONG_PTR)lpfnEdit);
		SendMessage(hEdit,WM_SETFONT,(WPARAM)GetStockObject(ANSI_VAR_FONT),MAKELPARAM(TRUE, 0));

		SendMessage(hWnd,BFFM_SETSELECTION ,1,(LPARAM) g_szFolder);
		SetWindowText(hEdit,g_szFolder);
		g_nInitialized=1;
		EnableWindow(GetDlgItem(hWnd,IDOK),TRUE);
	}
	if(uMsg==BFFM_SELCHANGED && g_nInitialized){
		SHGetPathFromIDList((LPITEMIDLIST)wParam,g_szFolder);
		SetWindowText(GetDlgItem(hWnd,1221),g_szFolder);
	}
	return 0;
}
intptr_t Browse(HWND hWnd,char *szFolder, int nFolder)
{
	if(*szFolder)
		strcpy(g_szFolder,szFolder);
	BROWSEINFO bi={0};
	char szDisplayName[MAX_PATH];
	char szPathName[MAX_PATH];
	bi.hwndOwner=hWnd;
	bi.pszDisplayName=szDisplayName;
	bi.lpszTitle="Folder:";
	bi.ulFlags=BIF_RETURNONLYFSDIRS;
	bi.lpfn=(BFFCALLBACK)lpfnCallBack;
	bi.lParam=(LPARAM)szPathName;
	g_nInitialized=0;
	intptr_t nRet = (intptr_t)SHBrowseForFolderA(&bi);
	if(!nRet)
		return 0;
	memset(szFolder, 0,nFolder);
	strncpy(szFolder,g_szFolder,nFolder-1);
	return nRet;
}
long WINAPI FilterProc(HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	if(uMsg==WM_DESTROY){
		GetWindowText(hWnd,g_szValue,sizeof(g_szValue));
	}
	return CallWindowProc((WNDPROC)GetClassLongPtr(hWnd,GCLP_WNDPROC),hWnd,uMsg,wParam,lParam);
}
int WINAPI lpfnCallBack2(HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	HWND hEdit;

	if(uMsg==BFFM_INITIALIZED){
		hEdit=CreateWindowEx(WS_EX_CLIENTEDGE,"edit",NULL,WS_CHILD|WS_VISIBLE|ES_AUTOHSCROLL|WS_TABSTOP,50,10,257,20,hWnd,(HMENU)1221,GetModuleHandle(NULL),0);
		SetWindowLongPtr(hEdit,GWLP_WNDPROC,(LONG_PTR)lpfnEdit);
		SendMessage(hEdit,WM_SETFONT,(WPARAM)GetStockObject(ANSI_VAR_FONT),MAKELPARAM(TRUE, 0));

		SendMessage(hWnd,BFFM_SETSELECTION ,1,(LPARAM) g_szFolder);
		SetWindowText(hEdit,g_szFolder);
		
		hEdit=CreateWindowEx(0,"static",g_szLabel,WS_CHILD|WS_VISIBLE,7,257,30,20,hWnd,NULL,GetModuleHandle(NULL),0);
		SendMessage(hEdit,WM_SETFONT,(WPARAM)GetStockObject(ANSI_VAR_FONT),MAKELPARAM(TRUE, 0));

		hEdit=CreateWindowEx(WS_EX_CLIENTEDGE,"edit",g_szValue,WS_CHILD|WS_VISIBLE,35,255,60,20,hWnd,NULL,GetModuleHandle(NULL),0);
		SetWindowLongPtr(hEdit,GWLP_WNDPROC,(LONG_PTR)FilterProc);
		SendMessage(hEdit,WM_SETFONT,(WPARAM)GetStockObject(ANSI_VAR_FONT),MAKELPARAM(TRUE, 0));
		
		g_nInitialized=1;
		EnableWindow(GetDlgItem(hWnd,IDOK),TRUE);
	}
	if(uMsg==BFFM_SELCHANGED && g_nInitialized){
		SHGetPathFromIDList((LPITEMIDLIST)wParam,g_szFolder);
		SetWindowText(GetDlgItem(hWnd,1221),g_szFolder);
	}
	return 0;
}
intptr_t Browse(HWND hWnd,char *szFolder, int nFolder,char *szLabel,char *szValue,int nValue)
{
	if(*szFolder)
		strcpy(g_szFolder,szFolder);
	BROWSEINFO bi={0};
	char szDisplayName[MAX_PATH];
	char szPathName[MAX_PATH];
	bi.hwndOwner=hWnd;
	bi.pszDisplayName=szDisplayName;
	bi.lpszTitle="Folder:";
	bi.ulFlags=BIF_RETURNONLYFSDIRS;
	bi.lpfn=(BFFCALLBACK)lpfnCallBack2;
	bi.lParam=(LPARAM)szPathName;
	g_nInitialized=0;
	g_szLabel = szLabel;
	strcpy(g_szValue,szValue);
	intptr_t nRet = (intptr_t)SHBrowseForFolder(&bi);
	if(!nRet)
		return 0;
	memset(szFolder, 0,nFolder);
	strncpy(szFolder,g_szFolder,nFolder-1);
	strncpy(szValue,g_szValue,nValue);
	return nRet;
}

int BrowseFile(HWND hwParent,BOOL bOpen, char *szTitle,char *szExt,char *szFile,int nFile)
{
	OPENFILENAME ofn={sizeof(OPENFILENAME)};
	ofn.hInstance = GetModuleHandle(NULL);
	ofn.lpstrFile = szFile;
	ofn.lpstrTitle = szTitle;
	ofn.nMaxFile = nFile;
	ofn.hwndOwner = hwParent;
	ofn.lpstrFilter = szExt;
	ofn.nFilterIndex = 1;
	if(bOpen){
		ofn.Flags = OFN_SHOWHELP | OFN_PATHMUSTEXIST | OFN_FILEMUSTEXIST | OFN_HIDEREADONLY;
		return GetOpenFileName(&ofn);
	}
	else{
		ofn.Flags = OFN_SHOWHELP | OFN_OVERWRITEPROMPT | OFN_HIDEREADONLY;
		if(GetSaveFileName(&ofn)){
			char *s = strrchr(szFile,'\\');
			if(!s)
				s = szFile;
			else
				s++;
			if(!strrchr(s,'.')){
				char *e = szExt;
				e+=strlen(e)+1;
				if(*e){
					for(int i=1;i<(int)ofn.nFilterIndex;i++){
						e+=strlen(e)+1;
						if(!*e)
							break;
						e+=strlen(e)+1;
						if(!*e)
							break;
					}
				}
				if(strstr(e,"."))
					e = strstr(e,".");
				strcat(szFile,e);
			}
			return 1;
		}
		else
			return 0;
	}
}

int g_nExtraMin=0;
int g_nExtraMax=0;
int *g_pnExtraValue;
char *g_szExtraName;
int *g_pnExtraValue1;
char *g_szExtraName1;
UINT WINAPI HookProc(HWND hWnd,UINT uMsg,WPARAM wParam,LPARAM lParam)
{
	RECT rWnd;
	HWND hwTmp;
	LPOFNOTIFY lpon;

	RECT r;

	switch(uMsg){
	case WM_INITDIALOG:
		GetWindowRect(hWnd,&rWnd);
		hwTmp = GetWindow(hWnd,GW_CHILD);
		while(hwTmp)
			hwTmp = GetWindow(hwTmp,GW_HWNDNEXT);
		GetWindowRect(GetDesktopWindow(),&r);
		rWnd.right-=rWnd.left;
		rWnd.bottom-=rWnd.top;
		MoveWindow(hWnd,rWnd.left,rWnd.top,rWnd.right,rWnd.bottom+40,TRUE);

		hwTmp = CreateWindow("static",g_szExtraName,WS_VISIBLE|WS_CHILD,10,250,200,20,
			GetParent(hWnd),NULL,GetModuleHandle(NULL),0);
		SendMessage(hwTmp,WM_SETFONT,(WPARAM)GetStockObject(ANSI_VAR_FONT),0);

		GetClientRect(GetParent(hWnd),&rWnd);
		hwTmp = CreateWindow("msctls_trackbar32","",WS_TABSTOP|WS_VISIBLE|WS_CHILD,10,250+20,rWnd.right-25,30,
			GetParent(hWnd),(HMENU)1122,GetModuleHandle(NULL),0);
		SendMessage(hwTmp,WM_SETFONT,(WPARAM)GetStockObject(ANSI_VAR_FONT),0);
		SendMessage(hwTmp,TBM_SETRANGE,1,MAKELONG(g_nExtraMin,g_nExtraMax));
		SendMessage(hwTmp,TBM_SETPOS,1,*g_pnExtraValue);
		int i;
		for(i=g_nExtraMin+g_nExtraMax/10;i<g_nExtraMax;i+=g_nExtraMax/10)
			SendMessage(hwTmp,TBM_SETTIC,0,i);
		break;
	case WM_NOTIFY:
		lpon = (LPOFNOTIFY)lParam;
		if(lpon->hdr.code==CDN_FILEOK){
			*g_pnExtraValue = SendDlgItemMessage(GetParent(hWnd),1122,TBM_GETPOS,0,0);
		}
		break;
	}
	return 0;
}
int BrowseFile(HWND hwParent,BOOL bOpen, char *szTitle,char *szExt,char *szFile,int nFile,char *szExtraName,int &nExtraValue,int nExtraMin, int nExtraMax)
{
	g_nExtraMin=nExtraMin;
	g_nExtraMax=nExtraMax;
	g_pnExtraValue = &nExtraValue;
	g_szExtraName = szExtraName;
	OPENFILENAME ofn={sizeof(OPENFILENAME)};
	ofn.hInstance = GetModuleHandle(NULL);
	ofn.lpstrFile = szFile;
	ofn.lpstrTitle = szTitle;
	ofn.nMaxFile = nFile;
	ofn.hwndOwner = hwParent;
	ofn.lpstrFilter = szExt;
	ofn.nFilterIndex = 1;
	ofn.lpfnHook = (LPOFNHOOKPROC)HookProc;
	if(bOpen){
		ofn.Flags = OFN_SHOWHELP | OFN_PATHMUSTEXIST | OFN_FILEMUSTEXIST | OFN_HIDEREADONLY | OFN_ENABLEHOOK | OFN_EXPLORER;
		return GetOpenFileName(&ofn);
	}
	else{
		ofn.Flags = OFN_SHOWHELP | OFN_OVERWRITEPROMPT | OFN_HIDEREADONLY | OFN_ENABLEHOOK | OFN_EXPLORER;
		if(GetSaveFileName(&ofn)){
			char *s = strrchr(szFile,'\\');
			if(!s)
				s = szFile;
			else
				s++;
			if(!strrchr(s,'.')){
				char *e = szExt;
				e+=strlen(e)+1;
				if(*e){
					for(int i=1;i<(int)ofn.nFilterIndex;i++){
						e+=strlen(e)+1;
						if(!*e)
							break;
						e+=strlen(e)+1;
						if(!*e)
							break;
					}
				}
				if(strstr(e,"."))
					e = strstr(e,".");
				strcat(szFile,e);
			}
			return 1;
		}
		else
			return 0;
	}
}

UINT WINAPI HookProc2(HWND hWnd,UINT uMsg,WPARAM wParam,LPARAM lParam)
{
	RECT rWnd;
	HWND hwTmp;
	LPOFNOTIFY lpon;

	RECT r;

	switch(uMsg){
	case WM_INITDIALOG:
		GetWindowRect(hWnd,&rWnd);
		hwTmp = GetWindow(hWnd,GW_CHILD);
		while(hwTmp)
			hwTmp = GetWindow(hwTmp,GW_HWNDNEXT);
		GetWindowRect(GetDesktopWindow(),&r);
		rWnd.right-=rWnd.left;
		rWnd.bottom-=rWnd.top;

		hwTmp = CreateWindow("button",g_szExtraName,BS_AUTOCHECKBOX|WS_VISIBLE|WS_CHILD|WS_TABSTOP,10,240,200,20,
			GetParent(hWnd),(HMENU)1122,GetModuleHandle(NULL),0);
		SendMessage(hwTmp,WM_SETFONT,(WPARAM)GetStockObject(ANSI_VAR_FONT),0);
		SendMessage(hwTmp,BM_SETCHECK,*g_pnExtraValue,0);
		break;
	case WM_NOTIFY:
		lpon = (LPOFNOTIFY)lParam;
		if(lpon->hdr.code==CDN_FILEOK){
			*g_pnExtraValue = SendDlgItemMessage(GetParent(hWnd),1122,BM_GETCHECK,0,0);
		}
		break;
	}
	return 0;
}
int BrowseFile(HWND hwParent,BOOL bOpen, char *szTitle,char *szExt,char *szFile,int nFile,char *szExtraCheck,int &nExtraCheck)
{
	g_pnExtraValue = &nExtraCheck;
	g_szExtraName = szExtraCheck;
	OPENFILENAME ofn={sizeof(OPENFILENAME)};
	ofn.hInstance = GetModuleHandle(NULL);
	ofn.lpstrFile = szFile;
	ofn.lpstrTitle = szTitle;
	ofn.nMaxFile = nFile;
	ofn.hwndOwner = hwParent;
	ofn.lpstrFilter = szExt;
	ofn.nFilterIndex = 1;
	ofn.lpfnHook = (LPOFNHOOKPROC)HookProc2;
	if(bOpen){
		ofn.Flags = OFN_SHOWHELP | OFN_PATHMUSTEXIST | OFN_FILEMUSTEXIST | OFN_HIDEREADONLY | OFN_ENABLEHOOK | OFN_EXPLORER;
		return GetOpenFileName(&ofn);
	}
	else{
		ofn.Flags = OFN_SHOWHELP | OFN_OVERWRITEPROMPT | OFN_HIDEREADONLY | OFN_ENABLEHOOK | OFN_EXPLORER;
		if(GetSaveFileName(&ofn)){
			char *s = strrchr(szFile,'\\');
			if(!s)
				s = szFile;
			else
				s++;
			if(!strrchr(s,'.')){
				char *e = szExt;
				e+=strlen(e)+1;
				if(*e){
					for(int i=1;i<(int)ofn.nFilterIndex;i++){
						e+=strlen(e)+1;
						if(!*e)
							break;
						e+=strlen(e)+1;
						if(!*e)
							break;
					}
				}
				if(strstr(e,"."))
					e = strstr(e,".");
				strcat(szFile,e);
			}
			return 1;
		}
		else
			return 0;
	}
}


UINT WINAPI HookProc4(HWND hWnd,UINT uMsg,WPARAM wParam,LPARAM lParam)
{
	RECT rWnd;
	HWND hwTmp;
	LPOFNOTIFY lpon;

	RECT r;

	switch(uMsg){
	case WM_INITDIALOG:
		GetWindowRect(hWnd,&rWnd);
		hwTmp = GetWindow(hWnd,GW_CHILD);
		while(hwTmp)
			hwTmp = GetWindow(hwTmp,GW_HWNDNEXT);
		GetWindowRect(GetDesktopWindow(),&r);
		rWnd.right-=rWnd.left;
		rWnd.bottom-=rWnd.top;

		hwTmp = CreateWindow("button",g_szExtraName,BS_AUTOCHECKBOX|WS_VISIBLE|WS_CHILD|WS_TABSTOP,10,240,130,20,
			GetParent(hWnd),(HMENU)1122,GetModuleHandle(NULL),0);
		SendMessage(hwTmp,WM_SETFONT,(WPARAM)GetStockObject(ANSI_VAR_FONT),0);
		SendMessage(hwTmp,BM_SETCHECK,*g_pnExtraValue,0);

		hwTmp = CreateWindow("button",g_szExtraName1,BS_AUTOCHECKBOX|WS_VISIBLE|WS_CHILD|WS_TABSTOP,140,240,130,20,
			GetParent(hWnd),(HMENU)1123,GetModuleHandle(NULL),0);
		SendMessage(hwTmp,WM_SETFONT,(WPARAM)GetStockObject(ANSI_VAR_FONT),0);
		SendMessage(hwTmp,BM_SETCHECK,*g_pnExtraValue1,0);
		break;
	case WM_NOTIFY:
		lpon = (LPOFNOTIFY)lParam;
		if(lpon->hdr.code==CDN_FILEOK){
			*g_pnExtraValue = SendDlgItemMessage(GetParent(hWnd),1122,BM_GETCHECK,0,0);
			*g_pnExtraValue1 = SendDlgItemMessage(GetParent(hWnd),1123,BM_GETCHECK,0,0);
		}
		break;
	}
	return 0;
}
int BrowseFile(HWND hwParent,BOOL bOpen, char *szTitle,char *szExt,char *szFile,int nFile,char *szExtraCheck,int &nExtraCheck,char *szExtraCheck1,int &nExtraCheck1)
{
	g_pnExtraValue = &nExtraCheck;
	g_szExtraName = szExtraCheck;
	g_pnExtraValue1 = &nExtraCheck1;
	g_szExtraName1 = szExtraCheck1;
	OPENFILENAME ofn={sizeof(OPENFILENAME)};
	ofn.hInstance = GetModuleHandle(NULL);
	ofn.lpstrFile = szFile;
	ofn.lpstrTitle = szTitle;
	ofn.nMaxFile = nFile;
	ofn.hwndOwner = hwParent;
	ofn.lpstrFilter = szExt;
	ofn.nFilterIndex = 1;
	ofn.lpfnHook = (LPOFNHOOKPROC)HookProc4;
	if(bOpen){
		ofn.Flags = OFN_SHOWHELP | OFN_PATHMUSTEXIST | OFN_FILEMUSTEXIST | OFN_HIDEREADONLY | OFN_ENABLEHOOK | OFN_EXPLORER;
		return GetOpenFileName(&ofn);
	}
	else{
		ofn.Flags = OFN_SHOWHELP | OFN_OVERWRITEPROMPT | OFN_HIDEREADONLY | OFN_ENABLEHOOK | OFN_EXPLORER;
		if(GetSaveFileName(&ofn)){
			char *s = strrchr(szFile,'\\');
			if(!s)
				s = szFile;
			else
				s++;
			if(!strrchr(s,'.')){
				char *e = szExt;
				e+=strlen(e)+1;
				if(*e){
					for(int i=1;i<(int)ofn.nFilterIndex;i++){
						e+=strlen(e)+1;
						if(!*e)
							break;
						e+=strlen(e)+1;
						if(!*e)
							break;
					}
				}
				if(strstr(e,"."))
					e = strstr(e,".");
				strcat(szFile,e);
			}
			return 1;
		}
		else
			return 0;
	}
}

CStringVektor *g_psvExtraList=NULL;
UINT WINAPI HookProc3(HWND hWnd,UINT uMsg,WPARAM wParam,LPARAM lParam)
{
	RECT rWnd;
	HWND hwTmp;
	LPOFNOTIFY lpon;
	HWND hwParent;

	RECT r;

	switch(uMsg){
	case WM_INITDIALOG:
		GetWindowRect(hWnd,&rWnd);
		hwTmp = GetWindow(hWnd,GW_CHILD);
		if(hwTmp==NULL)
			hwTmp = hWnd;
		while(hwTmp)
			hwTmp = GetWindow(hwTmp,GW_HWNDNEXT);
		GetWindowRect(GetDesktopWindow(),&r);
		rWnd.right-=rWnd.left;
		rWnd.bottom-=rWnd.top;
		hwParent = GetParent(hWnd);
		if(hwParent==NULL || hwParent==GetDesktopWindow())
			hwParent = hWnd;

		hwTmp = CreateWindow("button",g_szExtraName,BS_AUTOCHECKBOX|WS_VISIBLE|WS_CHILD|WS_TABSTOP,10,240,130,20,
			hwParent,(HMENU)1122,GetModuleHandle(NULL),0);
		SendMessage(hwTmp,WM_SETFONT,(WPARAM)GetStockObject(ANSI_VAR_FONT),0);
		SendMessage(hwTmp,BM_SETCHECK,*g_pnExtraValue,0);

		hwTmp = CreateWindow("combobox","",WS_CHILD|WS_VSCROLL|WS_VISIBLE|WS_BORDER|CBS_DROPDOWNLIST|WS_CLIPSIBLINGS,140,240,130,420,
			hwParent,(HMENU)1123,GetModuleHandle(NULL),0);
		SendMessage(hwTmp,WM_SETFONT,(WPARAM)GetStockObject(ANSI_VAR_FONT),0);
		int i;
		for(i=0;g_psvExtraList && i<g_psvExtraList->GetCount();i++)
			SendMessage(hwTmp,CB_ADDSTRING,0,(LPARAM)(*g_psvExtraList)[i]);
		SendMessage(hwTmp,CB_SETCURSEL,*g_pnExtraValue1,0);
		break;
	case WM_NOTIFY:
		lpon = (LPOFNOTIFY)lParam;
		if(lpon->hdr.code==CDN_FILEOK){
			hwParent = GetParent(hWnd);
			if(hwParent==NULL || hwParent==GetDesktopWindow())
				hwParent = hWnd;
			*g_pnExtraValue = SendDlgItemMessage(hwParent,1122,BM_GETCHECK,0,0);
			*g_pnExtraValue1 = SendDlgItemMessage(hwParent,1123,CB_GETCURSEL,0,0);
		}
		break;
	}
	return 0;
}
int BrowseFile(HWND hwParent,BOOL bOpen, char *szTitle,char *szExt,char *szFile,int nFile,char *szExtraCheck,int &nExtraCheck,CStringVektor &svExtraList,int &nExtraListSelectedIndex)
{
	g_pnExtraValue = &nExtraCheck;
	g_szExtraName = szExtraCheck;
	g_psvExtraList = &svExtraList;
	g_pnExtraValue1 = &nExtraListSelectedIndex;
	OPENFILENAME ofn={sizeof(OPENFILENAME)};
	ofn.hInstance = GetModuleHandle(NULL);
	ofn.lpstrFile = szFile;
	ofn.lpstrTitle = szTitle;
	ofn.nMaxFile = nFile;
	ofn.hwndOwner = hwParent;
	ofn.lpstrFilter = szExt;
	ofn.nFilterIndex = 1;
	ofn.lpfnHook = (LPOFNHOOKPROC)HookProc3;
	if(bOpen){
		ofn.Flags = OFN_SHOWHELP | OFN_PATHMUSTEXIST | OFN_FILEMUSTEXIST | OFN_HIDEREADONLY | OFN_ENABLEHOOK | OFN_EXPLORER;
		return GetOpenFileName(&ofn);
	}
	else{
		ofn.Flags = OFN_SHOWHELP | OFN_OVERWRITEPROMPT | OFN_HIDEREADONLY | OFN_ENABLEHOOK | OFN_EXPLORER;
		if(GetSaveFileName(&ofn)){
			char *s = strrchr(szFile,'\\');
			if(!s)
				s = szFile;
			else
				s++;
			if(!strrchr(s,'.')){
				char *e = szExt;
				e+=strlen(e)+1;
				if(*e){
					for(int i=1;i<(int)ofn.nFilterIndex;i++){
						e+=strlen(e)+1;
						if(!*e)
							break;
						e+=strlen(e)+1;
						if(!*e)
							break;
					}
				}
				if(strstr(e,"."))
					e = strstr(e,".");
				strcat(szFile,e);
			}
			return 1;
		}
		else
			return 0;
	}
}
