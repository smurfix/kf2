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

#ifndef __STRINGVECTOR_H__
#define __STRINGVECTOR_H__

#include <string>
#include <cinttypes>

class CStringVektor
{
	char **m_pszStrings;
	int *m_pnStrings;
	int *m_pnIndexValues;
	int m_nStrings;
	char nullstr[1];
public:
	CStringVektor();
	~CStringVektor();
	void Clean();
	int AddInt(intptr_t nVal);
	int AddString(const char *szString,int nSize=-1);
	int InsertString(int nIndex, const char *szString,int nSize=-1);
	int DeleteString(int nIndex);
	int SetString(int nIndex, const char *szString, int nSize=-1);
	int AppendString(int nIndex, const char *szString,int nSize=-1);
	char *GetString(int nIndex,int *pnSize=NULL);
	int GetCount();
	int SetCount(int nCount);
	char *operator [] (int nIndex);
	CStringVektor &operator =(CStringVektor &s);
	int MakeIndex(const char *szString, int nLenght);
	int GetIndex(int nIndex);

	int MoveString(int nFrom, int nTo);
	int GetLength(int nIndex);
	char *ToText(const char *szFieldSep);
	void DeleteToText(char *szToText);
	int FindString(const char *szString, int nLen=-1);
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
	char nullstr[1];
public:
	CStringTable();
	CStringTable(CStringTable &stCopy);
	CStringTable(const char *szData,const char *szFieldSep, const char *szRowSep,BOOL bApo=FALSE);
	~CStringTable();
	int AddRow(char **pszRow, int nRow);
	int AddRow();
	int InsertRow(int nRow);
	int AddString(int nRow, const char *szString, int nString=-1);
	int AddString(int nRow, const std::string &szString, int nString=-1);
	int AddInt(int nRow, intptr_t nVal);
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

	int FindString(int nColumn, const char *szString, int nLength=-1);
	int FindStringN(int nColumn, const char *szString, int nLength=-1);
	int Save(char *szFile);
	int Load(char *szFile);

	int BuildHash(int nColumn,int nItems=-1);
	// Must be a hash build before this function is called!!
	int FindStringHash(int nColumn, char *szString,int nString=-1,int nPrevHit=-1);
	int GetHashColumn();

	// The table must be sorted with order 0 on this column!!
	int FindStringBinary(int nColumn, char *szString, int nLength=-1);

	int SplitString(const char *szData,const char *szFieldSep, const char *szRowSep,BOOL bApo=FALSE);
	char *ToText(const char *szFieldSep="\t", const char *szRowSep="\r\n");
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

