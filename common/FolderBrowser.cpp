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

#include <shlobj.h>
#include "FolderBrowser.h"

char g_szFolder[MAX_PATH]={0};
int g_nInitialized;

#ifdef _WIN64
#define GCL_WNDPROC -24
#define GWL_WNDPROC -4
#endif

int WINAPI lpfnEdit(HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	if(uMsg==WM_DESTROY)
		GetWindowText(hWnd,g_szFolder,MAX_PATH);
	return CallWindowProc((WNDPROC)GetClassLongPtr(hWnd,GCLP_WNDPROC),hWnd,uMsg,wParam,lParam);
}

int WINAPI lpfnCallBack(HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	(void) lParam;
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

intptr_t Browse(HWND hWnd, std::string &szFolder)
{
	if(szFolder[0])
		strcpy(g_szFolder,szFolder.c_str());
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
	szFolder = g_szFolder;
	return nRet;
}

int BrowseFile(HWND hwParent,BOOL bOpen,const char *szTitle,const char *szExt,std::string &szFile)
{
	char buffer[1024] = { 0 };
	strncpy(buffer, szFile.c_str(), sizeof(buffer));
	buffer[sizeof(buffer) - 1] = 0;
	OPENFILENAME ofn={sizeof(OPENFILENAME)};
	ofn.hInstance = GetModuleHandle(NULL);
	ofn.lpstrFile = buffer;
	ofn.lpstrTitle = szTitle;
	ofn.nMaxFile = sizeof(buffer);
	ofn.hwndOwner = hwParent;
	ofn.lpstrFilter = szExt;
	ofn.nFilterIndex = 1;
	if(bOpen){
		ofn.Flags = OFN_SHOWHELP | OFN_PATHMUSTEXIST | OFN_FILEMUSTEXIST | OFN_HIDEREADONLY;
		int ret = GetOpenFileName(&ofn);
		if (ret)
		{
			szFile = buffer;
		}
		return ret;
	}
	else{
		ofn.Flags = OFN_SHOWHELP | OFN_OVERWRITEPROMPT | OFN_HIDEREADONLY;
		if(GetSaveFileName(&ofn)){
			szFile = buffer;
			char *s = strrchr(buffer,'\\');
			if(!s)
				s = buffer;
			else
				s++;
			if(!strrchr(s,'.')){
				const char *e = szExt;
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
				szFile += e;
			}
			return 1;
		}
		else
			return 0;
	}
}
