#ifndef __LIST_H__
#define __LIST_H__

#include "..\common\stringvector.h"

typedef int (*LPBUTTONPROC)(int nCommand, int nItem,LPARAM lParam);

enum BUTTONTYPES{
	BUTTONSTRING,
	BUTTONBITMAP
};
class CListBox
{
	HWND m_hwList;
	HWND m_hwParent;
	int m_nHeight;

	HFONT m_hfBold;
	HFONT m_hfNormal;

	COLORREF m_colRow1, m_colRow2;
	BOOL m_bShowSelection;

	struct BUTTON{
		char *szTitle;
		HBITMAP bmBmp;
		LPBUTTONPROC lpfnButtonProc;
		int nHeight;
		int nWidth;
		int nCommand;
		LPARAM lParam;
	}*m_pButtons;
	int m_nButtons;

	struct ROWS{
		char *szBoldText;
		char *szNormalText;
		HWND *phwButtons;
		int nButtons;
	}*m_pRows;
	int m_nRows;

	char *CopyString(char *szString);
	void DrawItem(LPDRAWITEMSTRUCT lpdis);
public:
	CListBox(HWND hwParent,HWND hwList,char *szFontFace, int nSize,COLORREF colRow1,COLORREF colRow2,BOOL bShowSelection);
	~CListBox();

	void ArrangeButtons(BOOL bVScroll=FALSE);
	void OnCommand(HWND hWnd);
	void DrawButton(LPDRAWITEMSTRUCT lpdis);
	void AddButton(int nType,char *szTitle,HBITMAP bmBmp,int nCommand,LPBUTTONPROC lpfnButtonProc,LPARAM lParam);
	void AddRow(char *szBoldText, char *szNormalText);
	void DeleteRow(int nItem);
	void ClearList();
	int GetCount() {return m_nRows;}
	int ProcessMessage(HWND hWnd,UINT uMsg,WPARAM wParam,LPARAM lParam);
};

class CListBoxEdit
{
	HWND m_hwAdd, m_hwUpdate, m_hwRemove;
	HWND m_hwEdit, m_hwList;
	CStringVektor m_stEdits;
public:
	CListBoxEdit(HWND hwAdd, HWND hwUpdate, HWND hwRemove, HWND hwEdit, HWND hwList,HWND *phwEdits=NULL, int nEdits=0);
	int ProcessMessage(HWND hWnd,UINT uMsg,WPARAM wParam,LPARAM lParam);
	int GetStrings(CStringVektor *psv);
};
#endif//__LIST_H__
