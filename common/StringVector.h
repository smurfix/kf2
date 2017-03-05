#ifndef __STRINGVECTOR_H__
#define __STRINGVECTOR_H__

class CStringVektor
{
	char **m_pszStrings;
	int *m_pnStrings;
	int *m_pnIndexValues;
	int m_nStrings;
public:
	CStringVektor();
	~CStringVektor();
	void Clean();
	int AddInt(int nVal);
	int AddString(char *szString,int nSize=-1);
	int InsertString(int nIndex, char *szString,int nSize=-1);
	int DeleteString(int nIndex);
	int SetString(int nIndex, char *szString, int nSize=-1);
	int AppendString(int nIndex, char *szString,int nSize=-1);
	char *GetString(int nIndex,int *pnSize=NULL);
	int GetCount();
	int SetCount(int nCount);
	char *operator [] (int nIndex);
	CStringVektor &operator =(CStringVektor &s);
	int MakeIndex(char *szString, int nLenght);
	int GetIndex(int nIndex);

	int MoveString(int nFrom, int nTo);
	int GetLength(int nIndex);
	char *ToText(char *szFieldSep);
	void DeleteToText(char *szToText);
	int FindString(char *szString, int nLen=-1);
};
class CStringTable
{
	CStringVektor **m_pVektors;
	CStringVektor m_err;
	int m_nVektors;
	int m_nRowSize;
	int *m_pnOrders;
	int *m_pnHash;
	int m_nHash;
	int m_nHashColumn;
	int m_nLastHash;
	BOOL m_bHashDirty;
public:
	CStringTable();
	CStringTable(CStringTable &stCopy);
	CStringTable(char *szData,char *szFieldSep, char *szRowSep,BOOL bApo=FALSE);
	~CStringTable();
	int AddRow(char **pszRow, int nRow);
	int AddRow();
	int InsertRow(int nRow);
	int AddString(int nRow, char *szString, int nString=-1);
	int AddInt(int nRow, int nVal);
	int DeleteRow(int nRow);
	int DeleteColumn(int nColumn);
	void Reset();
	int SetString(int nRow, int nIndex, char *szString, int nString=-1);
	int AppendString(int nRow, int nIndex, char *szString,int nString=-1);
	int SetInt(int nRow, int nIndex, int nVal);
	char* GetString(int nRow, int nIndex,int *pnSize=NULL);
	int GetCount();
	int GetRowSize(int nRow=-1);

	CStringVektor &operator [] (int nRow);
	CStringTable &operator = (CStringTable &st);
	int ReadCSV(char *szFileName);

	int Compare(int a, int b, int nColumn, int nType, int nOrder);
	void Swap(int a,  int b);
	void M3QSort(int nColumn,int nOrder=0,int nType=0,int left=0, int right=-1);

	int FindString(int nColumn, char *szString, int nLength=-1);
	int FindStringN(int nColumn, char *szString, int nLength=-1);
	int Save(char *szFile);
	int Load(char *szFile);

	int BuildHash(int nColumn,int nItems=-1);
	// Must be a hash build before this function is called!!
	int FindStringHash(int nColumn, char *szString,int nString=-1,int nPrevHit=-1);
	int GetHashColumn();

	// The table must be sorted with order 0 on this column!!
	int FindStringBinary(int nColumn, char *szString, int nLength=-1);

	int SplitString(char *szData,char *szFieldSep, char *szRowSep,BOOL bApo=FALSE);
	char *ToText(char *szFieldSep="\t", char *szRowSep="\r\n");
	void DeleteToText(char *szToText);

	int MoveCol(int nFrom, int nTo);
	int MoveRow(int nFrom, int nTo);
};

// stristr
//
// Why is this function not included in the standard library?
//
// 000821 KR Commented
//
char *stristr(char *src, char *find);

#endif//__STRINGVECTOR_H__

