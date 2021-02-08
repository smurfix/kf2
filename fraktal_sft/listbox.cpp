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

#include <windows.h>
#include "listbox.h"
#include "../common/getimage.h"

#ifdef _WIN64
#define GCL_WNDPROC -24
#define GWL_WNDPROC -4
#define GWL_USERDATA -21
#endif

int WINAPI SubclassListProc(HWND hWnd,UINT uMsg,WPARAM wParam,LPARAM lParam)
{
	if(uMsg==WM_COMMAND){
		CListBox*pList = (CListBox*)GetWindowLongPtr(hWnd,GWLP_USERDATA);
		if(pList)
			pList->OnCommand((HWND)lParam);
	}
	else if(uMsg==WM_SIZE || uMsg==WM_VSCROLL){
		CListBox*pList = (CListBox*)GetWindowLongPtr(hWnd,GWLP_USERDATA);
		if(pList)
			pList->ArrangeButtons(uMsg==WM_VSCROLL);
	}
	else if(uMsg==WM_DRAWITEM){
		CListBox*pList = (CListBox*)GetWindowLongPtr(hWnd,GWLP_USERDATA);
		if(pList)
			pList->DrawButton((LPDRAWITEMSTRUCT)lParam);
	}
	return CallWindowProc((WNDPROC)GetClassLongPtr(hWnd,GCLP_WNDPROC),hWnd,uMsg,wParam,lParam);
}
char *CListBox::CopyString(char *szString)
{
	int nLen = strlen(szString);
	char *szRet = new char[nLen+1];
	strcpy(szRet,szString);
	return szRet;
}
void CListBox::DrawItem(LPDRAWITEMSTRUCT lpdis)
{
	if(lpdis->itemID==(UINT)(-1))
		return;
	int i;
	COLORREF col = (lpdis->itemID%2?m_colRow2:m_colRow1);
	if(m_bShowSelection && (lpdis->itemState & ODS_SELECTED)){
		char *szCol = (char*)&col;
		col = RGB(~szCol[0],~szCol[1],~szCol[2]);
		SetTextColor(lpdis->hDC,RGB(255,255,255));
	}
	else
		SetTextColor(lpdis->hDC,RGB(0,0,0));
	HBRUSH br = CreateSolidBrush(col);
	FillRect(lpdis->hDC,&lpdis->rcItem,br);
	DeleteObject(br);

	SetBkMode(lpdis->hDC,TRANSPARENT);
	HFONT hfOld = (HFONT)SelectObject(lpdis->hDC,m_hfBold);
	TextOutA(lpdis->hDC,2,lpdis->rcItem.top+2,m_pRows[lpdis->itemID].szBoldText,strlen(m_pRows[lpdis->itemID].szBoldText));
	SelectObject(lpdis->hDC,m_hfNormal);
	TextOutA(lpdis->hDC,2,lpdis->rcItem.top+4+m_nHeight,m_pRows[lpdis->itemID].szNormalText,strlen(m_pRows[lpdis->itemID].szNormalText));
	SelectObject(lpdis->hDC,hfOld);
	for(i=0;i<m_pRows[lpdis->itemID].nButtons;i++){
		InvalidateRect(m_pRows[lpdis->itemID].phwButtons[i],NULL,FALSE);
		UpdateWindow(m_pRows[lpdis->itemID].phwButtons[i]);
	}
}
void CListBox::ArrangeButtons(BOOL bVScroll)
{
	int i, j;
	RECT r;
	int nLeft=0;
	BOOL bInvalidate=FALSE;
	for(i=0;i<m_nRows;i++){
		SendMessageA(m_hwList,LB_GETITEMRECT,i,(LPARAM)&r);
		for(j=0;j<m_pRows[i].nButtons;j++)
			r.right-=m_pButtons[j].nWidth+2;
		if(!nLeft)
			nLeft = r.right-2;
		for(j=0;j<m_pRows[i].nButtons;j++){
			RECT wr;
			GetWindowRect(m_pRows[i].phwButtons[j],&wr);
			ScreenToClient(m_hwList,(LPPOINT)&wr);
			if(wr.left!=r.right || wr.top!=r.top+2)
				bInvalidate=TRUE;
			MoveWindow(m_pRows[i].phwButtons[j],r.right,r.top+2,m_pButtons[j].nWidth,m_pButtons[j].nHeight,TRUE);
			r.right+=m_pButtons[j].nWidth+2;
		}
	}
	if(bVScroll || bInvalidate || 1){
		GetClientRect(m_hwList,&r);
		r.left=nLeft;
		InvalidateRect(m_hwList,&r,FALSE);
	}
}
void CListBox::OnCommand(HWND hWnd)
{
	int i, j;
	for(i=0;i<m_nRows;i++)
		for(j=0;j<m_pRows[i].nButtons;j++)
			if(m_pRows[i].phwButtons[j]==hWnd){
				m_pButtons[j].lpfnButtonProc(m_pButtons[j].nCommand,i,m_pButtons[j].lParam);
				return;
			}
}

CListBox::CListBox(HWND hwParent,HWND hwList,char *szFontFace, int nSize,COLORREF colRow1,COLORREF colRow2,BOOL bShowSelection)
{
	m_hwParent = hwParent;
	m_hwList = hwList;
	SetWindowLongPtr(m_hwList,GWLP_USERDATA,(LONG_PTR)this);
	SetWindowLongPtr(m_hwList,GWLP_WNDPROC,(LONG_PTR)SubclassListProc);
	m_hfBold = CreateFontA(nSize,0,0,0,FW_BOLD,0,0,0,0,0,0,0,0,szFontFace);
	m_hfNormal = CreateFontA(nSize,0,0,0,FW_NORMAL,0,0,0,0,0,0,0,0,szFontFace);
	m_nButtons = 0;
	m_pButtons = NULL;
	m_pRows = NULL;
	m_nRows = 0;

	HDC dcTest = GetDC(m_hwList);
	HFONT hfOld = (HFONT)SelectObject(dcTest,m_hfNormal);
	SIZE sc;
	GetTextExtentPointA(dcTest,"1",1,&sc);
	m_nHeight = sc.cy;
	SelectObject(dcTest,hfOld);
	ReleaseDC(m_hwList,dcTest);
	SendMessageA(m_hwList,LB_SETITEMHEIGHT,0,m_nHeight*2+6);
	m_colRow1 = colRow1;
	m_colRow2 = colRow2;
	m_bShowSelection = bShowSelection;
}
CListBox::~CListBox()
{
	int i, j;
	for(i=0;i<m_nButtons;i++){
		if(m_pButtons[i].szTitle)
			delete m_pButtons[i].szTitle;
	}
	if(m_pButtons)
		free(m_pButtons);
	for(i=0;i<m_nRows;i++){
		if(m_pRows[i].szBoldText)
			delete m_pRows[i].szBoldText;
		if(m_pRows[i].szNormalText)
			delete m_pRows[i].szNormalText;
		for(j=0;j<m_pRows[i].nButtons;j++)
			DestroyWindow(m_pRows[i].phwButtons[j]);
		if(m_pRows[i].phwButtons)
			delete m_pRows[i].phwButtons;
	}
	if(m_pRows)
		free(m_pRows);
	DeleteObject(m_hfBold);
	DeleteObject(m_hfNormal);
}

void CListBox::AddButton(int nType,char *szTitle,HBITMAP bmBmp,int nCommand,LPBUTTONPROC lpfnButtonProc,LPARAM lParam)
{
	int i = m_nButtons++;
	m_pButtons = (BUTTON*)realloc(m_pButtons,sizeof(BUTTON)*m_nButtons);
	memset(&m_pButtons[i],0,sizeof(BUTTON));
	if(nType==BUTTONSTRING){
		m_pButtons[i].szTitle = CopyString(szTitle);
		HDC dcTest = GetDC(m_hwList);
		HFONT hfOld = (HFONT)SelectObject(dcTest,(HFONT)GetStockObject(ANSI_VAR_FONT));
		SIZE sc;
		GetTextExtentPointA(dcTest,szTitle,strlen(szTitle),&sc);
		m_pButtons[i].nHeight = sc.cy+8;
		m_pButtons[i].nWidth = sc.cx+8;
		SelectObject(dcTest,hfOld);
		ReleaseDC(m_hwList,dcTest);
	}
	else{
		m_pButtons[i].bmBmp = (HBITMAP)bmBmp;
		BITMAP bm;
		GetObject(bmBmp,sizeof(BITMAP),&bm);
		m_pButtons[i].nHeight = bm.bmHeight+6;
		m_pButtons[i].nWidth = bm.bmWidth+6;
	}
	m_pButtons[i].lpfnButtonProc = lpfnButtonProc;
	m_pButtons[i].nCommand = nCommand;
	m_pButtons[i].lParam = lParam;
}
void CListBox::AddRow(char *szBoldText, char *szNormalText)
{
	RECT r;
	int j, i = m_nRows++;
	m_pRows = (ROWS*)realloc(m_pRows,sizeof(ROWS)*m_nRows);
	memset(&m_pRows[i],0,sizeof(ROWS));
	m_pRows[i].szBoldText = CopyString(szBoldText);
	m_pRows[i].szNormalText = CopyString(szNormalText);

	i = SendMessageA(m_hwList,LB_ADDSTRING,0,(LPARAM)"");

	SendMessageA(m_hwList,LB_GETITEMRECT,i,(LPARAM)&r);
	m_pRows[i].nButtons = m_nButtons;
	m_pRows[i].phwButtons = new HWND[m_nButtons];
	for(j=0;j<m_pRows[i].nButtons;j++)
		r.right-=m_pButtons[j].nWidth+2;
	for(j=0;j<m_pRows[i].nButtons;j++){
		m_pRows[i].phwButtons[j] = CreateWindowA("button",m_pButtons[j].szTitle,
			(m_pButtons[j].bmBmp?BS_OWNERDRAW :0) | WS_CHILD|WS_VISIBLE,r.right,r.top+2,m_pButtons[j].nWidth,
			m_pButtons[j].nHeight,m_hwList,NULL,GetModuleHandle(NULL),0);
		SendMessageA(m_pRows[i].phwButtons[j],WM_SETFONT,(WPARAM)GetStockObject(ANSI_VAR_FONT),0);
		r.right+=m_pButtons[j].nWidth+2;
	}
	ArrangeButtons();
}
void CListBox::DeleteRow(int nItem)
{
	int i, j;
	if(nItem<0 || nItem>m_nRows)
		return;
	if(m_pRows[nItem].szBoldText)
		delete m_pRows[nItem].szBoldText;
	if(m_pRows[nItem].szNormalText)
		delete m_pRows[nItem].szNormalText;
	for(j=0;j<m_pRows[nItem].nButtons;j++)
		DestroyWindow(m_pRows[nItem].phwButtons[j]);
	if(m_pRows[nItem].nButtons)
		delete m_pRows[nItem].phwButtons;
	m_nRows--;
	for(i=nItem;i<m_nRows;i++)
		m_pRows[i] = m_pRows[i+1];
	SendMessageA(m_hwList,LB_DELETESTRING,nItem,0);
	ArrangeButtons();
}
void CListBox::ClearList()
{
	while(m_nRows)
		DeleteRow(0);
}
int CListBox::ProcessMessage(HWND hWnd,UINT uMsg,WPARAM wParam,LPARAM lParam)
{
	(void) hWnd;
	if(uMsg==WM_DRAWITEM && wParam==(UINT)GetDlgCtrlID(m_hwList)){
		DrawItem((LPDRAWITEMSTRUCT)lParam);
		if(!m_bShowSelection)
			return 1;
	}
	return 0;
}


CListBoxEdit::CListBoxEdit(HWND hwAdd, HWND hwUpdate, HWND hwRemove, HWND hwEdit, HWND hwList,HWND *phwEdits, int nEdits)
{
	m_hwAdd = hwAdd;
	m_hwUpdate = hwUpdate;
	m_hwRemove = hwRemove;
	m_hwEdit = hwEdit;
	m_hwList = hwList;
	int i;
	for(i=0;i<nEdits;i++)
		m_stEdits.AddInt((intptr_t)phwEdits[i]);
}
int CListBoxEdit::ProcessMessage(HWND hWnd,UINT uMsg,WPARAM wParam,LPARAM lParam)
{
	(void) hWnd;
	if(uMsg==WM_COMMAND){
		if(lParam==(LPARAM)m_hwAdd){
			int nLen = GetWindowTextLength(m_hwEdit);
			int i;
			for(i=0;i<m_stEdits.GetCount();i++)
			{
				intptr_t h = 0;
				sscanf(m_stEdits[i], "%" SCNdPTR, &h);
				nLen+=GetWindowTextLength((HWND)h)+1;
			}
			char *szTmp = new char[nLen+1];
			GetWindowTextA(m_hwEdit,szTmp,nLen+1);
			char szClass[256];
			for(i=0;i<m_stEdits.GetCount();i++){
				strcat(szTmp,"\t");
				intptr_t h = 0;
				sscanf(m_stEdits[i], "%" SCNdPTR, &h);
				GetClassName((HWND)h,szClass,sizeof(szClass));
				if(!stricmp(szClass,"button"))
					itoa(SendMessage((HWND)h,BM_GETCHECK,0,0),szTmp+strlen(szTmp),10);
				else
					GetWindowTextA((HWND)h,szTmp+strlen(szTmp),nLen+1);
			}
			SendMessageA(m_hwList,LB_ADDSTRING,0,(LPARAM)szTmp);
			delete[] szTmp;
			SetFocus(m_hwEdit);
			SendMessageA(m_hwEdit,EM_SETSEL,0,-1);
		}
		else if(lParam==(LPARAM)m_hwUpdate){
			int nSel = SendMessageA(m_hwList,LB_GETCURSEL,0,0);
			if(nSel==-1)
				return 0;
			SendMessageA(m_hwList,LB_DELETESTRING,nSel,0);
			int nLen = GetWindowTextLength(m_hwEdit);
			int i;
			for(i=0;i<m_stEdits.GetCount();i++)
			{
				intptr_t h = 0;
				sscanf(m_stEdits[i], "%" SCNdPTR, &h);
				nLen+=GetWindowTextLength((HWND)h)+1;
			}
			char *szTmp = new char[nLen+1];
			GetWindowTextA(m_hwEdit,szTmp,nLen+1);
			char szClass[256];
			for(i=0;i<m_stEdits.GetCount();i++){
				strcat(szTmp,"\t");
				intptr_t h = 0;
				sscanf(m_stEdits[i], "%" SCNdPTR, &h);
				GetClassName((HWND)h,szClass,sizeof(szClass));
				if(!stricmp(szClass,"button"))
					itoa(SendMessage((HWND)h,BM_GETCHECK,0,0),szTmp+strlen(szTmp),10);
				else
					GetWindowTextA((HWND)h,szTmp+strlen(szTmp),nLen+1);
			}
			SendMessageA(m_hwList,LB_INSERTSTRING,nSel,(LPARAM)szTmp);
			delete[] szTmp;
			SetFocus(m_hwEdit);
			SendMessageA(m_hwEdit,EM_SETSEL,0,-1);
		}
		else if(lParam==(LPARAM)m_hwRemove){
			int i = SendMessageA(m_hwList,LB_GETCURSEL,0,0);
			if(i==-1)
				return 0;
			SendMessageA(m_hwList,LB_DELETESTRING,i,0);
			SetFocus(m_hwEdit);
			SendMessageA(m_hwEdit,EM_SETSEL,0,-1);
		}
		else if(lParam==(LPARAM)m_hwList && HIWORD(wParam)==LBN_SELCHANGE){
			int i = SendMessageA(m_hwList,LB_GETCURSEL,0,0);
			if(i==-1)
				return 0;
			int nLen = SendMessageA(m_hwList,LB_GETTEXTLEN,i,0);
			char *szTmp = new char[nLen+1];
			SendMessageA(m_hwList,LB_GETTEXT,i,(LPARAM)szTmp);
			char szClass[256];
			if(m_stEdits.GetCount()){
				CStringTable stT(szTmp,"\t","");
				GetClassName(m_hwEdit,szClass,sizeof(szClass));
				SetWindowTextA(m_hwEdit,stT[0][0]);
				for(i=0;i<m_stEdits.GetCount();i++){
					intptr_t h = 0;
					sscanf(m_stEdits[i], "%" SCNdPTR, &h);
					GetClassName((HWND)h,szClass,sizeof(szClass));
					if(!stricmp(szClass,"button"))
						SendMessage((HWND)h,BM_SETCHECK,atoi(stT[0][i+1]),0);
					else
						SetWindowTextA((HWND)h,stT[0][i+1]);
				}
			}
			else{
				GetClassName(m_hwEdit,szClass,sizeof(szClass));
				if(!stricmp(szClass,"combobox")){
					int li;
					for(li=0;li<SendMessage(m_hwEdit,CB_GETCOUNT,0,0);li++){
						int nL = SendMessage(m_hwEdit,CB_GETLBTEXTLEN,li,0);
						char *szL = new char[nL+1];
						SendMessage(m_hwEdit,CB_GETLBTEXT,li,(LPARAM)szL);
						if(!strcmp(szTmp,szL)){
							delete[] szL;
							break;
						}
						delete[] szL;
					}
					SendMessage(m_hwEdit,CB_SETCURSEL,li,0);
				}
				else
					SetWindowTextA(m_hwEdit,szTmp);
			}
			delete[] szTmp;
		}
	}
	return 0;
}
int CListBoxEdit::GetStrings(CStringVektor *psv)
{
	psv->Clean();
	int nLen = SendMessageA(m_hwList,LB_GETCOUNT,0,0);
	int i;
	for(i=0;i<nLen;i++){
		int nLen = SendMessageA(m_hwList,LB_GETTEXTLEN,i,0);
		char *szTmp = new char[nLen+1];
		SendMessageA(m_hwList,LB_GETTEXT,i,(LPARAM)szTmp);
		psv->AddString(szTmp);
		delete[] szTmp;
	}
	return nLen;
}
void CListBox::DrawButton(LPDRAWITEMSTRUCT lpdis)
{
	int i, j = 0;
	for(i=0;i<m_nRows;i++){
		for(j=0;j<m_pRows[i].nButtons;j++)
			if(m_pRows[i].phwButtons[j]==lpdis->hwndItem)
				break;
		if(j<m_pRows[i].nButtons)
			break;
	}
	HDC dcBmp = CreateCompatibleDC(lpdis->hDC);
	HBITMAP bmOld = (HBITMAP)SelectObject(dcBmp,m_pButtons[j].bmBmp);
	BITMAP bm;
	GetObject(m_pButtons[j].bmBmp,sizeof(BITMAP),&bm);
	COLORREF col = GetPixel(dcBmp,0,0);
	HBRUSH br = CreateSolidBrush(col);
	FillRect(lpdis->hDC,&lpdis->rcItem,br);
	DeleteObject(br);
#ifndef NO_GETIMAGE
	RECT r = lpdis->rcItem;
	if(lpdis->itemState & ODS_SELECTED){
		SkuggadRect(lpdis->hDC,r,FALSE,TRUE,0);
		r.left++;
		r.top++;
		r.right--;
		r.bottom--;
		SkuggadRect(lpdis->hDC,r,FALSE,FALSE,0);
		BitBlt(lpdis->hDC,4,4,bm.bmWidth,bm.bmHeight,dcBmp,0,0,SRCCOPY);
	}
	else{
		SkuggadRect(lpdis->hDC,r,TRUE,TRUE,0);
		r.left++;
		r.top++;
		r.right--;
		r.bottom--;
		SkuggadRect(lpdis->hDC,r,TRUE,FALSE,0);
		BitBlt(lpdis->hDC,2,2,bm.bmWidth,bm.bmHeight,dcBmp,0,0,SRCCOPY);
	}
#endif
	SelectObject(dcBmp,bmOld);
	DeleteDC(dcBmp);
}
