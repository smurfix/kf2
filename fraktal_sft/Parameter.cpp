#include "fraktal_sft.h"
#include "png.h"
#include "jpeg.h"
#include "../common/StringVector.h"

#include <iostream>

BOOL CFraktalSFT::OpenFile(char *szFile, BOOL bNoLocation)
{
	char *szData = 0;

	char *extension = strrchr(szFile, '.');
	if (extension && 0 == strcmp(".png", extension))
	{
		std::string filename = szFile;
		std::string comment = ReadPNGComment(filename);
		if (comment == "")
		  return FALSE;
		size_t n = comment.length() + 1;
		szData = new char[n];
		strncpy(szData, comment.c_str(), n);
		szData[n-1] = 0;
	}
	else if (extension && (0 == strcmp(".jpg", extension) || 0 == strcmp(".jpeg", extension)))
	{
		std::string filename = szFile;
		std::string comment = ReadJPEGComment(filename);
		if (comment == "")
		  return FALSE;
		size_t n = comment.length() + 1;
		szData = new char[n];
		strncpy(szData, comment.c_str(), n);
		szData[n-1] = 0;
	}
	else // anything else, probably .kfr
	{
		DWORD dw;
		HANDLE hFile = CreateFile(szFile, GENERIC_READ, 0, NULL, OPEN_EXISTING, 0, NULL);
		if (hFile == INVALID_HANDLE_VALUE)
			return FALSE;
		int nData = GetFileSize(hFile, NULL);
		szData = new char[nData + 1];
		ReadFile(hFile, szData, nData, &dw, NULL);
		CloseHandle(hFile);
		szData[nData] = 0;
	}

	CStringTable stParams(szData, ": ", "\r\n");
	delete[] szData;
	int nR = stParams.FindString(0, "Re");
	if (nR == -1)
		return FALSE;
	int nI = stParams.FindString(0, "Im");
	if (nI == -1)
		return FALSE;
	int nZ = stParams.FindString(0, "Zoom");
	if (nZ == -1)
		return FALSE;
	int nC = stParams.FindString(0, "Colors");
	if (nC == -1)
		return FALSE;
	int nIterations = stParams.FindString(0, "Iterations");
	if (nIterations == -1)
		return FALSE;
	int nID = stParams.FindString(0, "IterDiv");
	if (nID == -1)
		m_nIterDiv = 1;
	else
		m_nIterDiv = atof(stParams[nID][1]);

	nID = stParams.FindString(0, "Rotate");
	if (nID == -1)
		g_Degree = 0;
	else
		g_Degree = atof(stParams[nID][1]);

	nID = stParams.FindString(0, "Ratio");
	if (nID == -1)
		m_scRatio.cy = 360;
	else
		m_scRatio.cy = atof(stParams[nID][1]);
	SIZE size;
	size.cx = m_nX;
	size.cy = m_nY;
	double xRatio = 640.0/size.cx;
	size.cx = 640;
	size.cy = size.cy*xRatio;
	xRatio = (double)size.cy/(double)360;
	m_scRatio.cy*=xRatio;


	int nT = stParams.FindString(0, "Smooth");
	if (nT != -1)
		m_bTrans = atoi(stParams[nT][1]);
	nID = stParams.FindString(0, "SmoothMethod");
	if (nID != -1){
		m_nSmoothMethod = atoi(stParams[nID][1]);
		if (m_nSmoothMethod<0 || m_nSmoothMethod>1)
			m_nSmoothMethod = 0;
	}
	m_nBailout = m_nSmoothMethod == 1 ? 2 : SMOOTH_BAILOUT;
	m_nBailout2 = m_nBailout*m_nBailout;

	nID = stParams.FindString(0, "ColorMethod");
	if (nID != -1){
		int m = atoi(stParams[nID][1]);
		if (m<0 || m>8)
			m = 0;
		m_nColorMethod = ColorMethod(m);
	}
	else
		m_nColorMethod = ColorMethod_Standard;
	nID = stParams.FindString(0, "Differences");
	if (nID != -1)
	{
		int m = atoi(stParams[nID][1]);
		if (m < 0 || m > 3)
			m = 0;
		m_nDifferences = Differences(m);
	}
	else
		m_nDifferences = Differences_Traditional;
	nID = stParams.FindString(0, "ColorOffset");
	if (nID != -1){
		m_nColorOffset = atoi(stParams[nID][1]);
		if (m_nColorOffset<0 || m_nColorOffset>1023)
			m_nColorOffset = 0;
	}
	else
		m_nColorOffset = 0;
	nID = stParams.FindString(0, "Power");
	if (nID != -1){
		m_nPower = atoi(stParams[nID][1]);
		if (m_nPower<2)
			m_nPower = 2;
	}
	else
		m_nPower = 2;
	nID = stParams.FindString(0, "FractalType");
	if (nID != -1)
		m_nFractalType = atoi(stParams[nID][1]);
	else
		m_nFractalType = 0;

	nID = stParams.FindString(0, "Slopes");
	if (nID != -1)
		m_bSlopes = atoi(stParams[nID][1]);
	else
		m_bSlopes = FALSE;
	nID = stParams.FindString(0, "SlopePower");
	if (nID != -1){
		m_nSlopePower = atoi(stParams[nID][1]);
		if (m_nSlopePower<1)
			m_nSlopePower = 1;
	}
	else
		m_nSlopePower = 100;
	nID = stParams.FindString(0, "SlopeRatio");
	if (nID != -1){
		m_nSlopeRatio = atoi(stParams[nID][1]);
		if (m_nSlopeRatio<0)
			m_nSlopeRatio = 0;
	}
	else
		m_nSlopeRatio = 50;
	nID = stParams.FindString(0, "SlopeAngle");
	if (nID != -1)
		m_nSlopeAngle = atoi(stParams[nID][1]);
	else
		m_nSlopeAngle = 45;

	SetSlopes(m_bSlopes, m_nSlopePower, m_nSlopeRatio, m_nSlopeAngle);

	nID = stParams.FindString(0, "real");
	if (nID != -1)
		g_real = atoi(stParams[nID][1]);
	else
		g_real = 1;
	nID = stParams.FindString(0, "imag");
	if (nID != -1)
		g_imag = atoi(stParams[nID][1]);
	else
		g_imag = 1;

	nID = stParams.FindString(0, "SeedR");
	if (nID != -1)
		g_SeedR = atof(stParams[nID][1]);
	else
		g_SeedR = 0;
	nID = stParams.FindString(0, "SeedI");
	if (nID != -1)
		g_SeedI = atof(stParams[nID][1]);
	else
		g_SeedI = 0;
	nID = stParams.FindString(0, "FactorAR");
	if (nID != -1)
		g_FactorAR = atof(stParams[nID][1]);
	else
		g_FactorAR = 1;
	nID = stParams.FindString(0, "FactorAI");
	if (nID != -1)
		g_FactorAI = atof(stParams[nID][1]);
	else
		g_FactorAI = 0;

	if (g_nLDBL>100){
		if (m_nPower == 2 && !m_nFractalType)
			g_nLDBL = 600;
		else if (m_nPower == 3 && !m_nFractalType)
			g_nLDBL = 400;
		else
			g_nLDBL = 300;
	}

	if (!bNoLocation)
		SetPosition(stParams[nR][1], stParams[nI][1], stParams[nZ][1]);
	CStringTable stColors(stParams[nC][1], "", ",");
	m_nParts = stColors.GetCount() / 3;
	int i;
	for (i = 0; i<m_nParts; i++){
		m_cKeys[i].r = atoi(stColors[i * 3][0]);
		m_cKeys[i].g = atoi(stColors[i * 3 + 1][0]);
		m_cKeys[i].b = atoi(stColors[i * 3 + 2][0]);
	}
	i = stParams.FindString(0, "MultiColor");
	if (i != -1){
		m_bMW = atoi(stParams[i][1]);
		m_nMW = 0;
		i = stParams.FindString(0, "MultiColors");
		if (i != -1){
			stColors.Reset();
			stColors.SplitString(stParams[i][1], "\t", ",");
			for (i = 0; i<stColors.GetCount() && m_nMW<MULTIWAVE_MAX; i++){
				m_MW[m_nMW].nPeriod = atoi(stColors[i][0]);
				m_MW[m_nMW].nStart = atoi(stColors[i][1]);
				m_MW[m_nMW].nType = atoi(stColors[i][2]);
				m_nMW++;
			}
		}
	}
	else
		m_bMW = 0;
	i = stParams.FindString(0, "BlendMC");
	if (i != -1)
		m_bBlend = atoi(stParams[i][1]);
	ApplyColors();
	if (m_hWnd) InvalidateRect(m_hWnd, NULL, FALSE);
	m_nMaxIter = atoi(stParams[nIterations][1]);
	return TRUE;
}

std::string CFraktalSFT::ToText()
{
	CStringTable stSave;
	stSave.AddRow();
	stSave.AddString(stSave.GetCount() - 1, "Re");
	stSave.AddString(stSave.GetCount() - 1, GetRe());
	stSave.AddRow();
	stSave.AddString(stSave.GetCount() - 1, "Im");
	stSave.AddString(stSave.GetCount() - 1, GetIm());
	stSave.AddRow();
	stSave.AddString(stSave.GetCount() - 1, "Zoom");
	stSave.AddString(stSave.GetCount() - 1, GetZoom());
	stSave.AddRow();
	stSave.AddString(stSave.GetCount() - 1, "Iterations");
	stSave.AddInt(stSave.GetCount() - 1, m_nMaxIter);
	stSave.AddRow();
	stSave.AddString(stSave.GetCount() - 1, "IterDiv");
	char szDiv[256];
	sprintf(szDiv, "%f", m_nIterDiv);
	stSave.AddString(stSave.GetCount() - 1, szDiv);
	stSave.AddRow();
	stSave.AddString(stSave.GetCount() - 1, "SmoothMethod");
	stSave.AddInt(stSave.GetCount() - 1, m_nSmoothMethod);
	stSave.AddRow();
	stSave.AddString(stSave.GetCount() - 1, "ColorMethod");
	stSave.AddInt(stSave.GetCount() - 1, m_nColorMethod);
	stSave.AddRow();
	stSave.AddString(stSave.GetCount() - 1, "Differences");
	stSave.AddInt(stSave.GetCount() - 1, m_nDifferences);
	stSave.AddRow();
	stSave.AddString(stSave.GetCount() - 1, "ColorOffset");
	stSave.AddInt(stSave.GetCount() - 1, m_nColorOffset);

	stSave.AddRow();
	stSave.AddString(stSave.GetCount() - 1, "Rotate");
	sprintf(szDiv, "%f", g_Degree);
	stSave.AddString(stSave.GetCount() - 1, szDiv);

	stSave.AddRow();
	stSave.AddString(stSave.GetCount() - 1, "Ratio");
	double nRatio = m_scRatio.cy;
	SIZE size;
	size.cx = m_nX;
	size.cy = m_nY;
	double xRatio = 640.0/size.cx;
	size.cx = 640;
	size.cy = size.cy*xRatio;
	xRatio = (double)360/(double)size.cy;
	nRatio*=xRatio;
	sprintf(szDiv, "%f", nRatio);
	stSave.AddString(stSave.GetCount() - 1, szDiv);

	CStringTable stColors;
	int i;
	for (i = 0; i<m_nParts; i++){
		stColors.AddRow();
		stColors.AddInt(stColors.GetCount() - 1, m_cKeys[i].r);
		stColors.AddRow();
		stColors.AddInt(stColors.GetCount() - 1, m_cKeys[i].g);
		stColors.AddRow();
		stColors.AddInt(stColors.GetCount() - 1, m_cKeys[i].b);
	}
	char *szColors = stColors.ToText("", ",");
	stSave.AddRow();
	stSave.AddString(stSave.GetCount() - 1, "Colors");
	stSave.AddString(stSave.GetCount() - 1, szColors);
	stColors.DeleteToText(szColors);
	stSave.AddRow();
	stSave.AddString(stSave.GetCount() - 1, "Smooth");
	stSave.AddInt(stSave.GetCount() - 1, m_bTrans);

	stSave.AddRow();
	stSave.AddString(stSave.GetCount() - 1, "MultiColor");
	stSave.AddInt(stSave.GetCount() - 1, m_bMW);
	stSave.AddRow();
	stSave.AddString(stSave.GetCount() - 1, "BlendMC");
	stSave.AddInt(stSave.GetCount() - 1, m_bBlend);
	stColors.Reset();
	for (i = 0; i<m_nMW; i++){
		stColors.AddRow();
		stColors.AddInt(stColors.GetCount() - 1, m_MW[i].nPeriod);
		stColors.AddInt(stColors.GetCount() - 1, m_MW[i].nStart);
		stColors.AddInt(stColors.GetCount() - 1, m_MW[i].nType);
	}
	szColors = stColors.ToText("\t", ",");
	stSave.AddRow();
	stSave.AddString(stSave.GetCount() - 1, "MultiColors");
	stSave.AddString(stSave.GetCount() - 1, szColors);
	stColors.DeleteToText(szColors);

	stSave.AddRow();
	stSave.AddString(stSave.GetCount() - 1, "Power");
	stSave.AddInt(stSave.GetCount() - 1, m_nPower);

	stSave.AddRow();
	stSave.AddString(stSave.GetCount() - 1, "FractalType");
	stSave.AddInt(stSave.GetCount() - 1, m_nFractalType);

	stSave.AddRow();
	stSave.AddString(stSave.GetCount() - 1, "Slopes");
	stSave.AddInt(stSave.GetCount() - 1, m_bSlopes);
	stSave.AddRow();
	stSave.AddString(stSave.GetCount() - 1, "SlopePower");
	stSave.AddInt(stSave.GetCount() - 1, m_nSlopePower);
	stSave.AddRow();
	stSave.AddString(stSave.GetCount() - 1, "SlopeRatio");
	stSave.AddInt(stSave.GetCount() - 1, m_nSlopeRatio);
	stSave.AddRow();
	stSave.AddString(stSave.GetCount() - 1, "SlopeAngle");
	stSave.AddInt(stSave.GetCount() - 1, m_nSlopeAngle);
	stSave.AddRow();
	stSave.AddString(stSave.GetCount() - 1, "imag");
	stSave.AddInt(stSave.GetCount() - 1, g_imag);
	stSave.AddRow();
	stSave.AddString(stSave.GetCount() - 1, "real");
	stSave.AddInt(stSave.GetCount() - 1, g_real);
	stSave.AddRow();
	char szTmp[50];
	sprintf(szTmp,"%g",g_SeedR);
	stSave.AddRow();
	stSave.AddString(stSave.GetCount() - 1, "SeedR");
	stSave.AddString(stSave.GetCount() - 1, szTmp);
	sprintf(szTmp,"%g",g_SeedI);
	stSave.AddRow();
	stSave.AddString(stSave.GetCount() - 1, "SeedI");
	stSave.AddString(stSave.GetCount() - 1, szTmp);
	sprintf(szTmp,"%g",g_FactorAR);
	stSave.AddRow();
	stSave.AddString(stSave.GetCount() - 1, "FactorAR");
	stSave.AddString(stSave.GetCount() - 1, szTmp);
	sprintf(szTmp,"%g",g_FactorAI);
	stSave.AddRow();
	stSave.AddString(stSave.GetCount() - 1, "FactorAI");
	stSave.AddString(stSave.GetCount() - 1, szTmp);
	char *szData = stSave.ToText(": ", "\r\n");
	std::string ret(szData);
	stSave.DeleteToText(szData);
	return ret;
}

BOOL CFraktalSFT::SaveFile(char *szFile)
{
	std::string szText(ToText());
	const char *szData = szText.c_str();
	DWORD dw;
	HANDLE hFile = CreateFile(szFile, GENERIC_WRITE, 0, NULL, CREATE_ALWAYS, 0, NULL);
	if (hFile == INVALID_HANDLE_VALUE)
		return FALSE;
	WriteFile(hFile, szData, strlen(szData), &dw, NULL);
	CloseHandle(hFile);
	return TRUE;
}
