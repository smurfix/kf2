/*
Kalles Fraktaler 2
Copyright (C) 2013-2017 Karl Runmo
Copyright (C) 2017-2019 Claude Heiland-Allen

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
//#include "stdafx.h"
#include <windows.h>
#include "StringVector.h"

#define stricmp strcasecmp
#define strnicmp strncasecmp

int g_Allocated_StringTable=0;
int g_nAllocatedNum=0;
int g_nTestCount=0;
IMalloc *pAlloc=NULL;
#define HASHOFFS2 1

union Header{
	struct{
		Header *ptr;
		unsigned int size;
	}s;
	double x[1];
};
static Header base;
static Header *freep=NULL;
static Header trailer;
#define NALLOC 1048576

HANDLE g_hSTMutex=NULL;

BOOL g_StringTableMemCorrupted=FALSE;
class CSTMutex
{
public:
	CSTMutex()
	{
		g_hSTMutex = CreateMutex(NULL,0,NULL);
	}
	~CSTMutex()
	{
		CloseHandle(g_hSTMutex);
	}
}g_STMutex;
void kr_free(void *ap,BOOL bNoCheck=0)
{
	Header *bp, *p;
	BOOL bCheckFree=0;

	if(!ap)
		return;
	bp = (Header*)ap-1;
	if(!bNoCheck && memcmp(&trailer,bp+bp->s.size-1,sizeof(Header))){
		char szDmp[256];
		GetModuleFileName(GetModuleHandle(NULL),szDmp,sizeof(szDmp));
		strcat(szDmp,".dmp");
		DWORD dw;
		HANDLE hFile = CreateFile(szDmp,GENERIC_WRITE,0,NULL,CREATE_ALWAYS,0,NULL);
		if(hFile!=INVALID_HANDLE_VALUE){
			wsprintf(szDmp,"Trailer overwritten. Size: %d, Data:\r\n\r\n",bp->s.size*sizeof(Header));
			WriteFile(hFile,szDmp,strlen(szDmp),&dw,NULL);
			WriteFile(hFile,bp,bp->s.size*sizeof(Header),&dw,NULL);
			CloseHandle(hFile);
		}
	}

	WaitForSingleObject(g_hSTMutex,INFINITE);
	for(p=freep;!(bp>p && bp<p->s.ptr);p=p->s.ptr){
		if(p>=p->s.ptr && (bp>p || bp<p->s.ptr))
			break;
	}
	memset(ap,0xcc,(bp->s.size-1)*sizeof(Header));
	if(bp+bp->s.size==p->s.ptr){
		bp->s.size+=p->s.ptr->s.size;
		bp->s.ptr=p->s.ptr->s.ptr;
		if(bp->s.size==NALLOC)
			bCheckFree=1;
	}
	else
		bp->s.ptr=p->s.ptr;
	if(p+p->s.size==bp){
		p->s.size+=bp->s.size;
		p->s.ptr=bp->s.ptr;
		if(p->s.size==NALLOC)
			bCheckFree=1;
	}
	else
		p->s.ptr=bp;
	freep=p;
	if(bNoCheck || !bCheckFree){
		ReleaseMutex(g_hSTMutex);
		return;
	}
	p = &base;
	Header *prevp=NULL;
	do{
		if(p->s.size==NALLOC){
			if(prevp){
				prevp->s.ptr = p->s.ptr;
				if(freep==p)
					freep=prevp;
			}
			else
				freep=NULL;
			free(p);
			g_Allocated_StringTable--;
			p=prevp;
			break;
		}
		prevp = p;
		p=p->s.ptr;
	}while(p!=&base);
	ReleaseMutex(g_hSTMutex);
}
static Header *morecore(unsigned int nu)
{
	char *cp;
	Header *up;

	if(nu<NALLOC)
		nu = NALLOC;
	cp = (char*)malloc(nu*sizeof(Header));
	g_Allocated_StringTable++;
	if(cp==NULL)
		return NULL;
	up = (Header*)cp;
	up->s.size=nu;
	kr_free((void*)(up+1),1);
	return freep;
}
int g_nMaxTries=0;
void *kr_malloc(unsigned int nbytes)
{
	g_nTestCount++;
	Header *p, *prevp;
	unsigned int nunits;
	nunits = (nbytes+sizeof(Header)-1)/sizeof(Header) + 2;
	if(nunits>=NALLOC)
		Beep(1000,10);
	WaitForSingleObject(g_hSTMutex,INFINITE);
	if((prevp=freep)==NULL){
		memset(&trailer,1,sizeof(Header));
		base.s.ptr = freep = prevp = &base;
		base.s.size = 0;
	}
	int nTries=0;
	for(p=prevp->s.ptr;;prevp=p,p=p->s.ptr){
		if(p->s.size>=nunits){
			if(p->s.size-nunits<3)
				nunits = p->s.size;
			if(p->s.size==nunits)
				prevp->s.ptr = p->s.ptr;
			else{
				p->s.size-=nunits;
				p+=p->s.size;
				p->s.size = nunits;
			}
			freep = prevp;
			ReleaseMutex(g_hSTMutex);
			memset(p+p->s.size-1,1,sizeof(Header));
			if(nTries>g_nMaxTries)
				g_nMaxTries=nTries;
			return (void*)(p+1);
		}
		nTries++;
		if(p==freep){
			if((p=morecore(nunits))==NULL){
				ReleaseMutex(g_hSTMutex);
				return NULL;
			}
		}
	}
	ReleaseMutex(g_hSTMutex);
}

void *realloc_stringtable(void *p,size_t n)
{
#ifdef _KR_MALLOC_
	if(p){
		Header *ph = (Header *)p;
		ph--;
		int nPrevSize = (ph->s.size-2)*sizeof(Header);
		if(nPrevSize>=n)
			return p;
	}
	void *pr = kr_malloc(n+(p && n+n<NALLOC*sizeof(Header)?n:0));
	if(p){
		Header *ph = (Header *)p;
		ph--;
		int nPrevSize = ph->s.size*sizeof(Header);
		memcpy(pr,p,nPrevSize<n?nPrevSize:n);
		kr_free(p);
	}
	return pr;
#endif
	
	if(!pAlloc)
		CoGetMalloc(1,&pAlloc);
	void *pp=NULL;
	while(!pp){
		if(p){
			size_t a = *((int*)((char*)p-sizeof(int)));
			if(a>100000000)
				Beep(1000,10);

			char *TEST = (char*)p;
			if(TEST[a]!='A' || TEST[a+1]!='A'){
#ifdef _DEBUG
				MessageBox(NULL,"Memory corrupted","Error",MB_OK);
#endif
				g_StringTableMemCorrupted=TRUE;
			}

			if(a>=n)
				return p;
			if(n>1024)
				n<<=1;
			else
				n<<=2;
			pp = pAlloc->Alloc(n+sizeof(int)+8);
			if(pp){
				memcpy((char*)pp+sizeof(int),p,a);
				pAlloc->Free((char*)p-sizeof(int));
				g_Allocated_StringTable-=a;
			}
		}
		else{
			g_nAllocatedNum++;
	/*		if(n>1024)
				n<<=1;
			else
				n<<=2;
	*/		pp = pAlloc->Alloc(n+sizeof(int)+8);
		}
		if(!pp){
			pAlloc->HeapMinimize();
		}
	}
	char *TEST = (char*)pp;
	TEST+=sizeof(int);
	TEST[n]='A';
	TEST[n+1]='A';

	*((int*)((char*)pp)) = n;
	g_Allocated_StringTable+=n;

	if(g_nAllocatedNum%100000==0)
		pAlloc->HeapMinimize();

	return (char*)pp+sizeof(int);
}
void free_stringtable(void *p)
{
#ifdef _KR_MALLOC_
	kr_free(p);
	return;
#endif

	if(p){
		size_t a = *((int*)((char*)p-sizeof(int)));
//		if(a>1000000)
//			Beep(1000,10);

		char *TEST = (char*)p;
		if(TEST[a]!='A' || TEST[a+1]!='A'){
#ifdef _DEBUG
				MessageBox(NULL,"Memory corrupted","Error",MB_OK);
#endif
			g_StringTableMemCorrupted=TRUE;
		}

		g_Allocated_StringTable -= *((int*)((char*)p-sizeof(int)));
		pAlloc->Free((char*)p-sizeof(int));
	}
}
CStringVektor::CStringVektor()
	: m_pszStrings(NULL)
	, m_pnStrings(NULL)
	, m_pnIndexValues(NULL)
	, m_nStrings(0)
{
	nullstr[0] = 0;
}
CStringVektor::~CStringVektor()
{
	Clean();
}
void CStringVektor::Clean()
{
	int i;
	for(i=0;i<m_nStrings;i++)
		delete[] m_pszStrings[i];
	free_stringtable(m_pszStrings);
	m_nStrings=0;
	m_pszStrings=NULL;
	free_stringtable(m_pnStrings);
	m_pnStrings=NULL;
	free_stringtable(m_pnIndexValues);
	m_pnIndexValues=NULL;
}
int CStringVektor::MakeIndex(const char *szString, int nLenght)
{
	__int64 nIndex=0, i;
	for(i=0;i<nLenght;i++)
		nIndex = (217*nIndex+szString[i]);
	return (int)nIndex;
}

int CStringVektor::AddInt(intptr_t nVal)
{
	char szTmp[80];
	snprintf(szTmp, 80, "%" PRIdPTR, nVal);
	return AddString(szTmp);
}
int CStringVektor::AddString(const char *szString,int nSize)
{
	if(!szString)
		szString = "";
	int i=m_nStrings++;
	m_pnStrings = (int*)realloc_stringtable(m_pnStrings,sizeof(int)*m_nStrings);
	m_pnIndexValues = (int*)realloc_stringtable(m_pnIndexValues,sizeof(int)*m_nStrings);
	if(nSize==-1)
		m_pnStrings[i] = strlen(szString);
	else
		m_pnStrings[i] = nSize;
	m_pszStrings = (char**)realloc_stringtable(m_pszStrings,sizeof(char*)*m_nStrings);
	m_pszStrings[i] = new char[m_pnStrings[i]+1];
	if(m_pnStrings[i])
		memcpy(m_pszStrings[i],szString,m_pnStrings[i]);
	m_pszStrings[i][m_pnStrings[i]]=0;
	if(m_pnStrings[i])
		m_pnIndexValues[i] = MakeIndex(szString, m_pnStrings[i]);
	else
		m_pnIndexValues[i]=0;
	return 1;
}
int CStringVektor::InsertString(int nIndex, const char *szString,int nSize)
{
	if(nIndex>=m_nStrings)
		return AddString(szString,nSize);
	if(!szString || nIndex<0 || nIndex>=m_nStrings)
		return 0;
	int i;
	m_nStrings++;
	if(nSize==-1)
		nSize = strlen(szString);
	m_pnStrings = (int*)realloc_stringtable(m_pnStrings,sizeof(int)*m_nStrings);
	m_pnIndexValues = (int*)realloc_stringtable(m_pnIndexValues,sizeof(int)*m_nStrings);
	m_pszStrings = (char**)realloc_stringtable(m_pszStrings,sizeof(char*)*m_nStrings);
	for(i=m_nStrings-1;i>nIndex;i--){
		m_pnStrings[i] = m_pnStrings[i-1];
		m_pnIndexValues[i] = m_pnIndexValues[i-1];
		m_pszStrings[i] = m_pszStrings[i-1];
	}
	m_pnStrings[i] = nSize;
	m_pszStrings[i] = new char[m_pnStrings[i]+1];
	if(m_pnStrings[i])
		memcpy(m_pszStrings[i],szString,m_pnStrings[i]);
	m_pszStrings[i][m_pnStrings[i]]=0;
	if(m_pnStrings[i])
		m_pnIndexValues[i] = MakeIndex(szString, m_pnStrings[i]);
	else
		m_pnIndexValues[i]=0;
	return 1;
}

int CStringVektor::DeleteString(int nIndex)
{
	if(nIndex<0 || nIndex>=m_nStrings)
		return 0;
	delete[] m_pszStrings[nIndex];
	m_nStrings--;
	for(int i=nIndex;i<m_nStrings;i++){
		m_pnStrings[i] = m_pnStrings[i+1];
		m_pnIndexValues[i] = m_pnIndexValues[i+1];
		m_pszStrings[i] = m_pszStrings[i+1];
	}
	return 1;
}
int CStringVektor::SetString(int nIndex, const char *szString, int nSize)
{
	if(nIndex<0 || nIndex>=m_nStrings || !szString)
		return 0;
	char *szTmp = m_pszStrings[nIndex];
	if(nSize==-1)
		m_pnStrings[nIndex] = strlen(szString);
	else
		m_pnStrings[nIndex] = nSize;
	m_pszStrings[nIndex] = new char[m_pnStrings[nIndex]+1];
	memcpy(m_pszStrings[nIndex],szString,m_pnStrings[nIndex]+1);
	m_pszStrings[nIndex][m_pnStrings[nIndex]]=0;
	m_pnIndexValues[nIndex] = MakeIndex(szString, m_pnStrings[nIndex]);
	delete[] szTmp;

	return 1;
}
int CStringVektor::AppendString(int nIndex, const char *szString, int nSize)
{
	if(nIndex<0 || nIndex>=m_nStrings || !szString)
		return 0;
	char *szTmp = m_pszStrings[nIndex];
	int nPrevSize = m_pnStrings[nIndex];
	if(nSize==-1)
		nSize = strlen(szString);
	m_pnStrings[nIndex] += nSize;

	m_pszStrings[nIndex] = new char[m_pnStrings[nIndex]+1];
	memcpy(m_pszStrings[nIndex],szTmp,nPrevSize+1);
	m_pszStrings[nIndex][nPrevSize]=0;
	memcpy(m_pszStrings[nIndex]+nPrevSize,szString,nSize+1);
	m_pszStrings[nIndex][m_pnStrings[nIndex]]=0;
	m_pnIndexValues[nIndex] = MakeIndex(m_pszStrings[nIndex], m_pnStrings[nIndex]);
	delete[] szTmp;

	return 1;
}
char *CStringVektor::GetString(int nIndex,int *pnSize)
{
	if(nIndex<0 || nIndex>=m_nStrings){
		if(pnSize)
			*pnSize=0;
		return nullstr;
	}
	if(pnSize)
		*pnSize = m_pnStrings[nIndex];
	return m_pszStrings[nIndex]?m_pszStrings[nIndex]:nullstr;
}
int CStringVektor::GetIndex(int nIndex)
{
	if(nIndex<0 || nIndex>=m_nStrings)
		return -1;
	return m_pnIndexValues[nIndex];
}
int CStringVektor::GetCount()
{
	return m_nStrings;
}
int CStringVektor::GetLength(int nIndex)
{
	if(nIndex<0 || nIndex>=m_nStrings)
		return -1;
	return m_pnStrings[nIndex];
}
int CStringVektor::SetCount(int nCount)
{
	int i;
	if(nCount<m_nStrings){
		for(i=nCount;i<m_nStrings;i++)
			delete[] m_pszStrings[i];
	}
	else{
		m_pszStrings = (char**)realloc_stringtable(m_pszStrings,sizeof(char*)*nCount);
		m_pnStrings = (int*)realloc_stringtable(m_pnStrings,sizeof(int)*nCount);
		m_pnIndexValues = (int*)realloc_stringtable(m_pnIndexValues,sizeof(int)*nCount);
		for(i=m_nStrings;i<nCount;i++){
			m_pszStrings[i] = new char[1];
			*m_pszStrings[i]=0;
			m_pnStrings[i]=0;
			m_pnIndexValues[i]=-1;
		}
	}
	return (m_nStrings = nCount);
}
char *CStringVektor::operator [] (int nIndex)
{
	return GetString(nIndex);
}
CStringVektor &CStringVektor::operator =(CStringVektor &s)
{
	Clean();
	int i, n;
	for(i=0;i<s.GetCount();i++){
		const char *sz = s.GetString(i,&n);
		AddString(sz,n);
	}
	return *this;
}

int CStringVektor::MoveString(int nFrom, int nTo)
{
	if(nFrom<0 || nFrom>=m_nStrings)
		return 0;
	if(nTo<0 || nTo>=m_nStrings)
		return 0;
	char *szTmpStr = m_pszStrings[nFrom];
	m_pszStrings[nFrom] = m_pszStrings[nTo];
	m_pszStrings[nTo] = szTmpStr;
	int nTmp = m_pnStrings[nFrom];
	m_pnStrings[nFrom] = m_pnStrings[nTo];
	m_pnStrings[nTo] = nTmp;
	nTmp = m_pnIndexValues[nFrom];
	m_pnIndexValues[nFrom] = m_pnIndexValues[nTo];
	m_pnIndexValues[nTo] = nTmp;
	return 1;
}
char *CStringVektor::ToText(const char *szFieldSep)
{
	int nLen=0;
	char *szRet=NULL;
	int j, nSize;
	int nFieldSep=strlen(szFieldSep);

	for(j=0;j<m_nStrings;j++){
		GetString(j,&nSize);
		nLen+=nSize;
		if(j<m_nStrings-1)
			nLen+=nFieldSep;
	}
	nLen++;
	szRet = new char[nLen];
	nLen=0;
	for(j=0;j<m_nStrings;j++){
		strcpy(szRet+nLen,GetString(j,&nSize));
		nLen+=nSize;
		if(j<m_nStrings-1){
			strcpy(szRet+nLen,szFieldSep);
			nLen+=nFieldSep;
		}
	}
	szRet[nLen]=0;
	return szRet;
}
void CStringVektor::DeleteToText(char *szToText)
{
	delete[] szToText;
}
int CStringVektor::FindString(const char *szString,int nLen)
{
	if(nLen==-1)
		nLen = strlen(szString);
	int index = MakeIndex(szString,nLen);
	int i;
	for(i=0;i<m_nStrings;i++)
		if(m_pnIndexValues[i]==index && !strncmp(m_pszStrings[i],szString,nLen) && m_pnStrings[i]==nLen)
			return i;
	return -1;
}

CStringTable::CStringTable()
	: m_pVektors(NULL)
	, m_nVektors(0)
	, m_nRowSize(0)
	, m_pnOrders(NULL)
	, m_pnHash(NULL)
	, m_nHash(0)
	, m_nHashColumn(-1)
	, m_nLastHash(-1)
	, m_bHashDirty(FALSE)
{
}
CStringTable::CStringTable(const char *szData,const char *szFieldSep, const char *szRowSep,BOOL bApo)
	: m_pVektors(NULL)
	, m_nVektors(0)
	, m_nRowSize(0)
	, m_pnOrders(NULL)
	, m_pnHash(NULL)
	, m_nHash(0)
	, m_nHashColumn(-1)
	, m_nLastHash(-1)
	, m_bHashDirty(FALSE)
{
	nullstr[0] = 0;
	SplitString(szData,szFieldSep,szRowSep,bApo);
}

CStringTable::~CStringTable()
{
	int i;
	for(i=0;i<m_nVektors;i++)
		delete m_pVektors[i];
	free_stringtable(m_pVektors);
	m_nVektors=0;
	m_pVektors=NULL;
	delete[] m_pnHash;
	m_pnHash = NULL;
}
int CStringTable::AddRow(char **pszRow, int nRow)
{
	if(nRow>m_nRowSize)
		m_nRowSize = nRow;
	int j, i = AddRow()-1;
	for(j=0;j<nRow;j++)
		m_pVektors[i]->AddString(pszRow[j]);
	return m_nVektors;
}
int CStringTable::AddRow()
{
	int i = m_nVektors++;
	m_pVektors = (CStringVektor**)realloc_stringtable(m_pVektors,sizeof(CStringVektor*)*m_nVektors);
	m_pVektors[i] = new CStringVektor();
	return m_nVektors;
}
int CStringTable::InsertRow(int nRow)
{
	if(nRow<0 || nRow>m_nVektors)
		return 0;
	
	m_bHashDirty=1;
/*	if(m_pnHash)
		delete m_pnHash;
	m_pnHash=NULL;
	m_nHash = -1;
	m_nHashColumn = -1;
*/
	if(nRow==m_nVektors){
		AddRow();
		return 1;
	}
	int i;
	m_nVektors++;
	m_pVektors = (CStringVektor**)realloc_stringtable(m_pVektors,sizeof(CStringVektor*)*m_nVektors);
	for(i=m_nVektors-1;i>nRow;i--)
		m_pVektors[i] = m_pVektors[i-1];
	m_pVektors[nRow] = new CStringVektor();
	return 1;
}
int CStringTable::AddString(int nRow, const char *szString, int nString)
{
	if(nRow<0 || nRow>=m_nVektors)
		return 0;
	m_pVektors[nRow]->AddString(szString,nString);
	if(m_nRowSize<m_pVektors[nRow]->GetCount())
		m_nRowSize = m_pVektors[nRow]->GetCount();

	if(m_pVektors[nRow]->GetCount()==m_nHashColumn+1){
		if(GetCount()<m_nHash-10000){
			int j;
			j = (unsigned int)m_pVektors[nRow]->GetIndex(m_nHashColumn)%m_nHash;
			while(m_pnHash[j]!=-1){
				j = (j+HASHOFFS2)%m_nHash;
			}
			m_pnHash[j] = nRow;
		}
		else
			BuildHash(m_nHashColumn);
	}
	return 1;
}
int CStringTable::AddString(int nRow, const std::string &szString, int nString)
{
	char *s = strdup(szString.c_str());
	int ret = AddString(nRow, s, nString);
	free(s);
	return ret;
}

int CStringTable::AddInt(int nRow, intptr_t nVal)
{
	char szTmp[80];
	snprintf(szTmp, 80, "%" PRIdPTR, nVal);
	return AddString(nRow,szTmp);
}
int CStringTable::DeleteRow(int nRow)
{
	if(nRow<0 || nRow>=m_nVektors)
		return 0;
	m_bHashDirty=1;
	CStringVektor *p = m_pVektors[nRow];
	m_nVektors--;
	for(;nRow<m_nVektors;nRow++)
		m_pVektors[nRow] = m_pVektors[nRow+1];
	delete p;
	return 1;
}
int CStringTable::DeleteColumn(int nColumn)
{
	if(nColumn<=m_nHashColumn){
		m_bHashDirty=1;
/*		if(m_pnHash)
			delete m_pnHash;
		m_pnHash=NULL;
		m_nHash = -1;
		m_nHashColumn = -1;
*/	}
	int i;
	for(i=0;i<m_nVektors;i++)
		m_pVektors[i]->DeleteString(nColumn);
	return 1;
}
void CStringTable::Reset()
{
	while(m_nVektors)
		DeleteRow(m_nVektors-1);
	if(m_nHashColumn!=-1)
		BuildHash(m_nHashColumn);
}
int CStringTable::SetString(int nRow, int nIndex, char *szString, int nString)
{
	if(nRow<0 || nRow>=m_nVektors)
		return 0;

	if(m_nHashColumn==nIndex){
		m_bHashDirty=1;
/*		if(m_pnHash)
			delete m_pnHash;
		m_pnHash=NULL;
		m_nHash = -1;
		m_nHashColumn = -1;
*/	}	
	return m_pVektors[nRow]->SetString(nIndex,szString,nString);
}
int CStringTable::AppendString(int nRow, int nIndex, char *szString, int nString)
{
	if(nRow<0 || nRow>=m_nVektors)
		return 0;

	if(m_nHashColumn==nIndex){
		m_bHashDirty=1;
/*		if(m_pnHash)
			delete m_pnHash;
		m_pnHash=NULL;
		m_nHash = -1;
		m_nHashColumn = -1;
*/	}	
	return m_pVektors[nRow]->AppendString(nIndex,szString,nString);
}
int CStringTable::SetInt(int nRow, int nIndex, intptr_t nVal)
{
	char szTmp[80];
	snprintf(szTmp, 80, "%" PRIdPTR, nVal);
	return SetString(nRow,nIndex,szTmp);
}
char *CStringTable::GetString(int nRow, int nIndex,int *pnSize)
{
	if(nRow<0 || nRow>=m_nVektors)
		return nullstr;
	return m_pVektors[nRow]->GetString(nIndex,pnSize);
}
int CStringTable::GetCount()		
{ 
	return m_nVektors;
}
int CStringTable::GetRowSize(int nRow)
{ 
	if(nRow==-1)
		return m_nRowSize;
	if(nRow<0 || nRow>=m_nVektors)
		return 0;
	return m_pVektors[nRow]->GetCount();
}

CStringVektor &CStringTable::operator [] (int nRow)
{
	if(nRow<0 || nRow>=m_nVektors)
		return m_err;
	return *m_pVektors[nRow];
}
CStringTable &CStringTable::operator = (CStringTable &st)
{
	Reset();
	int i, j, n = 0;
	for(i=0;i<st.GetCount();i++){
		AddRow();
		for(j=0;j<st.GetRowSize(i);j++){
			char *sz = st.GetString(i,j,&n);
			AddString(i,sz,n);
		}
	}
	return *this;
}
CStringTable::CStringTable(CStringTable &st)
	: m_pVektors(NULL)
	, m_nVektors(0)
	, m_nRowSize(0)
	, m_pnOrders(NULL)
	, m_pnHash(NULL)
	, m_nHash(0)
	, m_nHashColumn(-1)
	, m_nLastHash(-1)
	, m_bHashDirty(FALSE)
{
	*this = st;
}

int CStringTable::ReadCSV(char *szFileName)
{
	DWORD dw;
	HANDLE hFile = CreateFileA(szFileName,GENERIC_READ,FILE_SHARE_READ|FILE_SHARE_WRITE,NULL,OPEN_EXISTING,0,NULL);
	if(hFile==INVALID_HANDLE_VALUE)
		return 0;
	int nLen = GetFileSize(hFile,NULL);
	char *szData = new char[nLen+1];
	ReadFile(hFile,szData,nLen,&dw,NULL);
	CloseHandle(hFile);
	szData[nLen]=0;
	
	AddRow();
	SplitString(szData,";","\r\n");
	delete[] szData;
	return 1;
}

char *stristr(char *src, char *find)
{
	int i;

	while(*src){
		if(toupper(*src)==toupper(*find)){
			for(i=1;find[i] && toupper(src[i])==toupper(find[i]);i++);
			if(find[i]==0)
				return src;
		}
		src++;
	}
	return NULL;
}

int CStringTable::Compare(int a, int b, int nColumn, int nType, int nOrder)
{
	int ret;
	
	int nCmp=0;
	if(nType==0)
		nCmp = stricmp(m_pVektors[a]->GetString(nColumn),m_pVektors[b]->GetString(nColumn));
	else{
		if(atof(m_pVektors[a]->GetString(nColumn))<atof(m_pVektors[b]->GetString(nColumn)))
			nCmp = -1;
		else if(atof(m_pVektors[a]->GetString(nColumn))>atof(m_pVektors[b]->GetString(nColumn)))
			nCmp = 1;
	}
	if(nOrder){
		if(nCmp==0 && m_pnOrders[a]>=m_pnOrders[b])
			nCmp=-1;
		if(nCmp<0)
			ret=1;
		else
			ret=0;
	}
	else{
		if(nCmp==0 && m_pnOrders[a]>=m_pnOrders[b])
			nCmp=1;
		if(nCmp>0)
			ret=1;
		else
			ret=0;
	}

	return ret;
}

void CStringTable::Swap(int a,  int b)
{
	CStringVektor *tmp = m_pVektors[a];
	m_pVektors[a] = m_pVektors[b];
	m_pVektors[b] = tmp;

	if(m_pnOrders){
		int nTmp = m_pnOrders[a];
		m_pnOrders[a] = m_pnOrders[b];
		m_pnOrders[b] = nTmp;
	}
}
void CStringTable::M3QSort(int nColumn,int nOrder,int nType,int left, int right)
{
	if(!m_nVektors || left>=m_nVektors)
		return;
	BOOL bInit=FALSE;
	if(right==-1)
		right = m_nVektors-1;
	int i, j, z, cut;
	if(left==right) 
		return;
	i=left;
	j=right;
	z=(right+left)/2;
	cut = 0;

	if(!m_pnOrders){
		bInit=TRUE;
		m_pnOrders = new int[m_nVektors];
		for(int i=0;i<m_nVektors;i++)
			m_pnOrders[i] = i;
	}

	if(Compare(left,z,nColumn,nType,nOrder))
		Swap(left, z);
	if(Compare(z,right,nColumn,nType,nOrder)){
		Swap(z, right);
		if(Compare(left,z,nColumn,nType,nOrder))
			Swap(left, z);
	}

	cut = z;
	do{
		while(Compare(cut,i,nColumn,nType,nOrder)){
			i=i+1;
			if(i==right) break;
		}
		while(Compare(j,cut,nColumn,nType,nOrder)){
			j=j-1;
			if(j==left) break;
		}
		if(i<=j){
			if(j!=i)
				Swap(i,j);
			i=i+1;
			j=j-1;
		}
	}while(i<=j);
	if(left < j)
		M3QSort(nColumn,nOrder,nType,left,j);
	if(i < right)
		M3QSort(nColumn,nOrder,nType,i,right);

	if(bInit){
		delete[] m_pnOrders;
		m_pnOrders=NULL;
	}
}
int CStringTable::FindString(int nColumn, const char *szString, int nLength)
{
	if(!m_nVektors || !szString)
		return -1;
	if(nLength==-1)
		nLength = strlen(szString);
	int nIndexValue = m_pVektors[0]->MakeIndex(szString,nLength);
	int i;
	for(i=0;i<m_nVektors;i++)
		if(nIndexValue==m_pVektors[i]->GetIndex(nColumn)){ 
			if(!strncmp(szString,m_pVektors[i]->GetString(nColumn),nLength) && (int)strlen(m_pVektors[i]->GetString(nColumn))==nLength)
				return i;
		}
	return -1;
}
int CStringTable::FindStringN(int nColumn, const char *szString, int nLength)
{
	if(!m_nVektors)
		return -1;
	if(nLength==-1)
		nLength = strlen(szString);
	int i;
	for(i=0;i<m_nVektors;i++)
		if(!strnicmp(m_pVektors[i]->GetString(nColumn),szString,nLength))
			return i;
	return -1;
}
int CStringTable::Save(char *szFile)
{
	int i, j;
	DWORD dw;
	HANDLE hFile = CreateFileA(szFile,GENERIC_WRITE,0,NULL,CREATE_ALWAYS,0,NULL);
	if(hFile==INVALID_HANDLE_VALUE)
		return 0;
	WriteFile(hFile,&m_nVektors,sizeof(m_nVektors),&dw,NULL);
	for(i=0;i<m_nVektors;i++){
		int nLen = m_pVektors[i]->GetCount();
		WriteFile(hFile,&nLen,sizeof(nLen),&dw,NULL);
		for(j=0;j<m_pVektors[i]->GetCount();j++){
			char *sz = m_pVektors[i]->GetString(j,&nLen);
			WriteFile(hFile,&nLen,sizeof(nLen),&dw,NULL);
			WriteFile(hFile,sz,nLen,&dw,NULL);
		}
	}
	CloseHandle(hFile);
	return 1;
}
int CStringTable::Load(char *szFile)
{
	int i, j, nLen;
	DWORD dw;
	HANDLE hFile = CreateFileA(szFile,GENERIC_READ,0,NULL,OPEN_EXISTING,0,NULL);
	if(hFile==INVALID_HANDLE_VALUE)
		return 0;
	while(m_nVektors)
		DeleteRow(0);
	int nRows;
	ReadFile(hFile,&nRows,sizeof(nRows),&dw,NULL);
	for(i=0;i<nRows;i++){
		ReadFile(hFile,&nLen,sizeof(nLen),&dw,NULL);
		AddRow();
		for(j=0;j<nLen;j++){
			int nStringSize;
			ReadFile(hFile,&nStringSize,sizeof(nStringSize),&dw,NULL);
			char *sz = new char[nStringSize+1];
			ReadFile(hFile,sz,nStringSize,&dw,NULL);
			sz[nStringSize]=0;
			AddString(i,sz,nStringSize);
			delete[] sz;
		}
	}
	CloseHandle(hFile);
	return 1;
}

int CStringTable::SplitString(const char *szData,const char *szFieldSep, const char *szRowSep,BOOL bApo)
{
	if(!szData)
		return 0;
	AddRow();
	int q, p;
	int nFieldSep=0;
	if(szFieldSep)
		nFieldSep = strlen(szFieldSep);
	int nRowSep=0;
	if(szRowSep)
		nRowSep = strlen(szRowSep);
	BOOL bInsideApo=0;
	BOOL bRecordStart=TRUE;
	for(p=q=0;szData[q];q++){
		if(bApo && bRecordStart && szData[q]=='"'){
			bInsideApo=TRUE;
			bRecordStart=FALSE;
		}
		else if(bInsideApo && szData[q]=='"' && szData[q+1]!='"')
			bInsideApo=FALSE;
		if(bInsideApo)
			continue;

		if(!szData[q] || (nFieldSep && !strncmp(&szData[q],szFieldSep,nFieldSep)) || 
				(nRowSep && !strncmp(&szData[q],szRowSep,nRowSep))){
			if(bApo && szData[p]=='"')
				AddString(GetCount()-1,&szData[p+1],q-p-2);
			else
				AddString(GetCount()-1,&szData[p],q-p);
			if(nRowSep && !strncmp(&szData[q],szRowSep,nRowSep)){
				q+=nRowSep-1;
				AddRow();
			}
			else
				q+=nFieldSep-1;
			p=q+1;
			bRecordStart=TRUE;
		}
	}
	if(szData[p])
		AddString(GetCount()-1,&szData[p]);
	else if(!GetRowSize(GetCount()-1))
		DeleteRow(GetCount()-1);
	return 1;
}

char *CStringTable::ToText(const char *szFieldSep, const char *szRowSep)
{
	int nLen=0;
	char *szRet=NULL;
	int i, j, k, nSize;
	int nFieldSep=strlen(szFieldSep);
	int nRowSep=strlen(szRowSep);

	for(i=0;i<GetCount();i++){
		for(j=0;j<GetRowSize(i);j++){
			m_pVektors[i]->GetString(j,&nSize);
			nLen+=nSize;
			if(j<GetRowSize(i)-1)
				nLen+=nFieldSep;
			else
				nLen+=nRowSep;
		}
	}
	nLen++;
	szRet = new char[nLen];
	*szRet=0;
	nLen=0;
	for(i=0;i<GetCount();i++){
		for(j=0;j<GetRowSize(i);j++){
			strcpy(&szRet[nLen],m_pVektors[i]->GetString(j,&nSize));
			for(k=0;k<nSize;k++)
				if(!szRet[nLen+k])
					szRet[nLen+k]=' ';
			nLen+=nSize;
			if(j<GetRowSize(i)-1){
				strcpy(&szRet[nLen],szFieldSep);
				nLen+=nFieldSep;
			}
			else{
				strcpy(&szRet[nLen],szRowSep);
				nLen+=nRowSep;
			}
		}
	}
	return szRet;
}
void CStringTable::DeleteToText(char *szToText)
{
	delete[] szToText;
}

int CStringTable::MoveCol(int nFrom, int nTo)
{
	int i;
	for(i=0;i<GetCount();i++)
		m_pVektors[i]->MoveString(nFrom,nTo);
	return 1;
}
int CStringTable::MoveRow(int nFrom, int nTo)
{
	if(nFrom<0 || nFrom>=m_nVektors || nTo<0 || nTo>=m_nVektors)
		return 0;
	CStringVektor *pTmp = m_pVektors[nFrom];
	m_pVektors[nFrom] = m_pVektors[nTo];
	m_pVektors[nTo] = pTmp;
	return 1;
}


int CStringTable::BuildHash(int nColumn,int nItems)
{
	if(m_pnHash)
		delete[] m_pnHash;
	m_nHash = nItems;
	m_nHashColumn = nColumn;
	m_nLastHash = -1;
	if(m_nHash<GetCount()+10)
		m_nHash = GetCount()*2;
	if(m_nHash<64919)
		m_nHash=64919;
	else{
		m_nHash+=64919;
//#ifdef REAL_PRIME_VALUE
		int q;
		while(1){
			int nHalf = m_nHash/2;
			for(q=2;q<=nHalf;q++)
				if(m_nHash%q==0)
					break;
			if(q>nHalf)
				break;
			m_nHash++;
		}
//#endif
	}
	m_pnHash = new int[m_nHash];
	memset(m_pnHash,-1,sizeof(int)*m_nHash);
	int i, j;
#ifdef _DEBUG
	int nFirst, nKrock=0, nKrock2=0;
#endif
	for(i=0;i<GetCount();i++){
#ifdef _DEBUG
		nFirst=1;
#endif
		j = (unsigned int)m_pVektors[i]->GetIndex(nColumn)%m_nHash;
		while(m_pnHash[j]!=-1){
#ifdef _DEBUG
			if(nFirst){
				nFirst=0;
				nKrock++;
			}
			nKrock2++;
#endif
			j = (j+HASHOFFS2)%m_nHash;
		}
		m_pnHash[j] = i;
	}
	m_bHashDirty=FALSE;
	return 0;
}
int CStringTable::FindStringHash(int nColumn, char *szString,int nString,int nPrevHit)
{
	if(!m_pnHash)
		return -1;
	if(!m_pVektors)
		return -1;
	if(nColumn!=m_nHashColumn)
		return -1;
	if(m_bHashDirty)
		BuildHash(m_nHashColumn);
	if(nString==-1)
		nString = strlen(szString);
	int nTest;
	if(nPrevHit!=-1){
		if(m_nLastHash==-1)
			return -1;
		int j = m_nLastHash;
		if(j<0 || j>=m_nHash || m_pnHash[j]!=nPrevHit)
			return -1;
		while(m_pnHash[j]!=-1){
			do{
				j = (j+HASHOFFS2)%m_nHash;
			}while(m_pnHash[j]!=-1 && (strncmp(szString,m_pVektors[m_pnHash[j]]->GetString(nColumn,&nTest),nString) || nString!=nTest));
			m_nLastHash = j;
			return m_pnHash[j];
		}
		m_nLastHash = j;
		return m_pnHash[j];
	}
	int nIndexValue = m_pVektors[0]->MakeIndex(szString,nString);
	int j = (unsigned int)nIndexValue%m_nHash;
	while(m_pnHash[j]!=-1 && nIndexValue!=m_pVektors[m_pnHash[j]]->GetIndex(nColumn)){
		j = (j+HASHOFFS2)%m_nHash;
	}
	while(m_pnHash[j]!=-1 && (strncmp(szString,m_pVektors[m_pnHash[j]]->GetString(nColumn,&nTest),nString) || nString!=nTest)){
		j = (j+HASHOFFS2)%m_nHash;
	}
	m_nLastHash = j;
	return m_pnHash[j];
}
int CStringTable::GetHashColumn()
{
	return m_nHashColumn;
}

int CStringTable::FindStringBinary(int nColumn, char *szString, int nLength)
{
	int m = 0;
	int n = GetCount();
	int k = n/2;
	int t;
	if(nLength==-1)
		nLength = strlen(szString);
	while((t = strncmp(GetString(k,nColumn),szString,nLength))){
		if(t<0)
			m = k;
		else
			n = k;
		if(m==n)
			return -1;
		k = (n-m)/2 + m;
	}
	return k;
}
