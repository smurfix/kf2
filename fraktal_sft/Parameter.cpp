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

#include "fraktal_sft.h"
#include "cmdline.h"
#include "tiff.h"
#include "png.h"
#include "jpeg.h"
#include "exr.h"
#include "main.h"
#include "newton.h"
#include "../common/StringVector.h"

#include <iostream>

std::string glsl_escape(const std::string &s)
{
	std::ostringstream o;
	for (auto p = s.begin(); p != s.end(); ++p)
	{
		switch (*p)
		{
			case '\\': o << '\\' << '\\'; break;
			case ' ': o << '\\' << ' '; break;
			case '\t': o << '\\' << 't'; break;
			case '\r': o << '\\' << 'r'; break;
			case '\n': o << '\\' << 'n'; break;
			default: o << *p; break;
		}
	}
	return o.str();
}

std::string glsl_unescape(const std::string &s)
{
	std::ostringstream o;
	for (auto p = s.begin(); p != s.end(); ++p)
	{
		switch (*p)
		{
			case '\\':
			{
				++p;
				if (p != s.end())
				{
					switch (*p)
					{
						case '\\': o << '\\'; break;
						case ' ': o << ' '; break;
						case 't': o << '\t'; break;
						case 'r': o << '\r'; break;
						case 'n': o << '\n'; break;
						default: o << *p; break; // FIXME
					}
				}
				else
				{
					// FIXME
				}
				break;
			}
			default: o << *p; break;
		}
	}
	return o.str();
}

static const double deg = 360 / 6.283185307179586;

BOOL CFraktalSFT::OpenFile(const std::string &szFile, BOOL bNoLocation)
{
	std::string data;
  std::string extension = get_filename_extension(szFile);
	if (extension == "tif" || extension == "tiff")
	{
		data = ReadTIFFComment(szFile);
		if (data == "")
		  return FALSE;
	}
	else if (extension == "png")
	{
		data = ReadPNGComment(szFile);
		if (data == "")
		  return FALSE;
	}
	else if (extension == "jpg" || extension == "jpeg")
	{
		data = ReadJPEGComment(szFile);
		if (data == "")
		  return FALSE;
	}
	else if (extension == "exr")
	{
		data = ReadEXRComment(szFile);
		if (data == "")
		  return FALSE;
	}
	else // anything else, probably .kfr
	{
		DWORD dw;
		HANDLE hFile = CreateFile(szFile.c_str(), GENERIC_READ, 0, NULL, OPEN_EXISTING, 0, NULL);
		if (hFile == INVALID_HANDLE_VALUE)
			return FALSE;
		int nData = GetFileSize(hFile, NULL);
		char *szData = new char[nData + 1];
		ReadFile(hFile, szData, nData, &dw, NULL);
		CloseHandle(hFile);
		szData[nData] = 0;
    data = szData;
    delete[] szData;
	}
  return OpenString(data, bNoLocation);
}

BOOL CFraktalSFT::OpenString(const std::string &data, BOOL bNoLocation)
{
	CStringTable stParams(data.c_str(), ": ", "\r\n");
	{
		int nv = stParams.FindString(0, "Version");
		if (nv != -1)
		{
			int str_version_number = atoi(stParams[nv][1]);
			if (str_version_number > kfr_version_number)
			{
				fprintf(stderr, "WARNING: file format is newer than this EXE version\n");
			}
		}
	}
	if (GetOpenResetsParameters())
	{
		ResetParameters();
	}
	int nR = -1, nI = -1, nZ = -1, nIterations = -1;
	if (! bNoLocation)
	{
		nR = stParams.FindString(0, "Re");
		nI = stParams.FindString(0, "Im");
		nZ = stParams.FindString(0, "Zoom");
		nIterations = stParams.FindString(0, "Iterations");
	}

	int nID = stParams.FindString(0, "IterDiv");
	if (nID != -1)
		m_nIterDiv = atof(stParams[nID][1]);

	nID = stParams.FindString(0, "ColorMethod");
	if (nID != -1){
		int m = atoi(stParams[nID][1]);
		if (m<0 || m>10)
			m = 0;
		m_nColorMethod = ColorMethod(m);
	}
	nID = stParams.FindString(0, "Differences");
	if (nID != -1)
	{
		int m = atoi(stParams[nID][1]);
		if (m < 0 || m > 7)
			m = 0;
		m_nDifferences = Differences(m);
	}
	nID = stParams.FindString(0, "ColorOffset");
	if (nID != -1){
		m_nColorOffset = atoi(stParams[nID][1]);
		if (m_nColorOffset<0 || m_nColorOffset>1023)
			m_nColorOffset = 0;
	}
	nID = stParams.FindString(0, "ColorPhaseStrength");
	if (nID != -1){
		m_nPhaseColorStrength = atof(stParams[nID][1]);
	}

	if (! bNoLocation)
	{
	nID = stParams.FindString(0, "FractalType");
	if (nID != -1)
		SetFractalType(atoi(stParams[nID][1]));
	nID = stParams.FindString(0, "Power");
	if (nID != -1)
		SetPower(atoi(stParams[nID][1]));
	// KFR version >= 2150500
	nID = stParams.FindString(0, "TriangleInequalityAverage");
	if (nID != -1)
		SetTriangleInequalityAverage(atoi(stParams[nID][1]));
	}

	int nT = stParams.FindString(0, "Smooth");
	if (nT != -1)
		m_bTrans = atoi(stParams[nT][1]);
	nT = stParams.FindString(0, "Flat");
	if (nT != -1)
		m_bFlat = atoi(stParams[nT][1]);
	if (! bNoLocation)
	{
		// KFR version < 2
		nID = stParams.FindString(0, "SmoothMethod"); // must come after "Power"
		if (nID != -1){
			int m = atoi(stParams[nID][1]);
			switch (m)
			{
				default:
				case 0:
					SetBailoutRadiusPreset(BailoutRadius_High);
					SetSmoothMethod(SmoothMethod_Log);
				  break;
				case 1:
					SetBailoutRadiusPreset(BailoutRadius_2);
					SetSmoothMethod(SmoothMethod_Sqrt);
				  break;
				case 2:
					SetBailoutRadiusPreset(BailoutRadius_Low);
					SetSmoothMethod(SmoothMethod_Sqrt);
				  break;
			}
		}
#if 0
	m_nBailout = m_nSmoothMethod == SmoothMethod_SqrtLow ? pow(2.0, 1.0 / (m_nPower - 1))
	           : m_nSmoothMethod == SmoothMethod_Sqrt ? 2 : SMOOTH_BAILOUT;
	m_nBailout2 = m_nBailout*m_nBailout;
#endif
		// KFR version >= 2
		nID = stParams.FindString(0, "SmoothingMethod"); // must come after SmoothMethod
		if (nID != -1){
			int m = atoi(stParams[nID][1]);
			if (m < 0 || m > 1)
			{
				m = 0;
			}
			SetSmoothMethod(SmoothMethod(m));
		}
		nID = stParams.FindString(0, "BailoutRadiusPreset"); // must come after SmoothMethod
		if (nID != -1){
			int m = atoi(stParams[nID][1]);
			if (m < 0 || m > 3)
			{
				m = 0;
			}
			SetBailoutRadiusPreset(BailoutRadiusPreset(m));
		}
		nID = stParams.FindString(0, "BailoutRadiusCustom"); // must come after SmoothMethod
		if (nID != -1){
			SetBailoutRadiusCustom(atof(stParams[nID][1]));
		}
	}

	nID = stParams.FindString(0, "Slopes");
	if (nID != -1)
		m_bSlopes = atoi(stParams[nID][1]);
	nID = stParams.FindString(0, "SlopePower");
	if (nID != -1){
		m_nSlopePower = atoi(stParams[nID][1]);
		if (m_nSlopePower<1)
			m_nSlopePower = 1;
	}
	nID = stParams.FindString(0, "SlopeRatio");
	if (nID != -1){
		m_nSlopeRatio = atoi(stParams[nID][1]);
		if (m_nSlopeRatio<0)
			m_nSlopeRatio = 0;
	}
	nID = stParams.FindString(0, "SlopeAngle");
	if (nID != -1)
		m_nSlopeAngle = atoi(stParams[nID][1]);

	SetSlopes(m_bSlopes, m_nSlopePower, m_nSlopeRatio, m_nSlopeAngle);

	if (! bNoLocation)
	{
	nID = stParams.FindString(0, "real");
	if (nID != -1)
	{
		g_real = atof(stParams[nID][1]);
	}
	nID = stParams.FindString(0, "imag");
	if (nID != -1)
	{
		g_imag = atof(stParams[nID][1]);
	}

	nID = stParams.FindString(0, "BailoutNormPreset");
	if (nID != -1)
	{
		int m = atoi(stParams[nID][1]);
		if (m < 0 || m > 3)
		{
			m = 1;
		}
		SetBailoutNormPreset(BailoutNormPreset(m));
	}

	nID = stParams.FindString(0, "BailoutNormCustom");
	if (nID != -1)
	{
		SetBailoutNormCustom(atof(stParams[nID][1]));
	}

	nID = stParams.FindString(0, "SeedR");
	if (nID != -1)
		g_SeedR = atof(stParams[nID][1]);
	nID = stParams.FindString(0, "SeedI");
	if (nID != -1)
		g_SeedI = atof(stParams[nID][1]);
	nID = stParams.FindString(0, "FactorAR");
	if (nID != -1)
		g_FactorAR = atof(stParams[nID][1]);
	nID = stParams.FindString(0, "FactorAI");
	if (nID != -1)
		g_FactorAI = atof(stParams[nID][1]);

	nID = stParams.FindString(0, "Period");
	if (nID != -1)
		g_period = atoll(stParams[nID][1]);
	else
		g_period = 0;
	}

	int nC = stParams.FindString(0, "Colors");
  if (nC != -1)
  {
		CStringTable stColors(stParams[nC][1], "", ",");
		m_nParts = stColors.GetCount() / 3;
		int i;
		for (i = 0; i<m_nParts; i++){
			m_cKeys[i].r = atoi(stColors[i * 3][0]);
			m_cKeys[i].g = atoi(stColors[i * 3 + 1][0]);
			m_cKeys[i].b = atoi(stColors[i * 3 + 2][0]);
		}
  }
	int i = stParams.FindString(0, "MultiColor");
	if (i != -1){
		m_bMW = atoi(stParams[i][1]);
		m_nMW = 0;
		i = stParams.FindString(0, "MultiColors");
		if (i != -1){
			CStringTable stColors(stParams[i][1], "\t", ",");
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

	i = stParams.FindString(0, "InteriorColor");
	if (i != -1)
	{
		CStringTable stColor(stParams[i][1], "", ",");
		if (stColor.GetCount() == 3)
		{
			m_cInterior.r = atoi(stColor[0][0]);
			m_cInterior.g = atoi(stColor[1][0]);
			m_cInterior.b = atoi(stColor[2][0]);
		}
	}

	i = stParams.FindString(0, "TextureEnabled");
	if (i != -1) m_bTexture = atoi(stParams[i][1]);
	i = stParams.FindString(0, "TextureMerge");
	if (i != -1) m_nImgMerge = atof(stParams[i][1]);
	i = stParams.FindString(0, "TexturePower");
	if (i != -1) m_nImgPower = atof(stParams[i][1]);
	i = stParams.FindString(0, "TextureRatio");
	if (i != -1) m_nImgRatio = atoi(stParams[i][1]);
	i = stParams.FindString(0, "TextureFile");
	if (i != -1) m_szTexture = stParams[i][1];
	i = stParams.FindString(0, "TextureResize");
	if (i != -1) m_bTextureResize = atoi(stParams[i][1]);

	i = stParams.FindString(0, "UseOpenGL");
	if (i != -1) SetUseOpenGL(atoi(stParams[i][1]));
	i = stParams.FindString(0, "GLSL");
	if (i != -1) SetGLSL(glsl_unescape(stParams[i][1]));
	i = stParams.FindString(0, "UseSRGB");
	if (i != -1) SetUseSRGB(atoi(stParams[i][1]));

	if (! bNoLocation)
	{
	i = stParams.FindString(0, "UseHybridFormula");
	if (i != -1 && atoi(stParams[i][1]))
	{
		i = stParams.FindString(0, "HybridFormula");
		if (i != -1)
		{
			hybrid_formula h = hybrid_formula_from_string(stParams[i][1]);
			if (valid(h))
			{
				SetHybridFormula(h);
				SetUseHybridFormula(true);
			}
			else
			{
				SetUseHybridFormula(false);
			}
		}
		else
		{
			SetUseHybridFormula(false);
		}
	}
	else
	{
		SetUseHybridFormula(false);
	}
	}

	if (! bNoLocation)
	{
	double RotateAngle = 0;
	double StretchAngle = 0;
	double StretchAmount = 0;
	bool ImagPointsUp = false;
	i = stParams.FindString(0, "RotateAngle");
	if (i != -1)
	{
		RotateAngle = atof(stParams[i][1]);
	}
	i = stParams.FindString(0, "StretchAngle");
	if (i != -1)
	{
		StretchAngle = atof(stParams[i][1]);
	}
	i = stParams.FindString(0, "StretchAmount");
	if (i != -1)
	{
		StretchAmount = atof(stParams[i][1]);
	}
	i = stParams.FindString(0, "ImagPointsUp");
	if (i != -1)
	{
		ImagPointsUp = atoi(stParams[i][1]);
	}
	SetTransformPolar(polar2(ImagPointsUp ? -1 : 1, 1, RotateAngle / deg, std::exp2(StretchAmount), StretchAngle / deg));
	}

	if (! bNoLocation)
	{
		if (nIterations != -1) m_nMaxIter = atoll(stParams[nIterations][1]);
		std::string re = GetRe();
		std::string im = GetIm();
		std::string zm = GetZoom();
		if (nR != -1) re = stParams[nR][1];
		if (nI != -1) im = stParams[nI][1];
		if (nZ != -1) zm = stParams[nZ][1];
		SetPosition(re, im, zm);
	}
	ApplyColors();
	if (m_hWnd) InvalidateRect(m_hWnd, NULL, FALSE);
	return TRUE;
}

std::string CFraktalSFT::ToText()
{
	CStringTable stSave;

#define STRING(k,v) \
	stSave.AddRow(); \
	stSave.AddString(stSave.GetCount() - 1, k); \
	stSave.AddString(stSave.GetCount() - 1, v);

#define INT(k,v) \
	stSave.AddRow(); \
	stSave.AddString(stSave.GetCount() - 1, k); \
	stSave.AddInt(stSave.GetCount() - 1, v);

#define DOUBLE(k,v) \
	{ \
		char s[100]; \
		snprintf(s, sizeof(s), "%.18g", v); \
		stSave.AddRow(); \
		stSave.AddString(stSave.GetCount() - 1, k); \
		stSave.AddString(stSave.GetCount() - 1, s); \
	}

	STRING("Re", GetRe())
	STRING("Im", GetIm())
	STRING("Zoom", GetZoom())
	INT("Iterations", GetIterations())
	DOUBLE("IterDiv", GetIterDiv())

	// KFR version < 2
	INT("SmoothMethod", GetBailoutRadiusPreset() == BailoutRadius_Low ? 2 : GetSmoothMethod())
	// KFR version >= 2
	INT("SmoothingMethod", GetSmoothMethod())
	INT("BailoutRadiusPreset", GetBailoutRadiusPreset())
	DOUBLE("BailoutRadiusCustom", GetBailoutRadiusCustom())
	INT("BailoutNormPreset", GetBailoutNormPreset())
	DOUBLE("BailoutNormCustom", GetBailoutNormCustom())

	INT("ColorMethod", GetColorMethod())
	INT("Differences", GetDifferences())
	INT("ColorOffset", GetColorOffset()) // FIXME DOUBLE
	DOUBLE("ColorPhaseStrength", GetPhaseColorStrength())

	{
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
		STRING("Colors", szColors)
		stColors.DeleteToText(szColors);
	}

	{
		CStringTable stColor;
		stColor.AddRow();
		stColor.AddInt(stColor.GetCount() - 1, m_cInterior.r);
		stColor.AddRow();
		stColor.AddInt(stColor.GetCount() - 1, m_cInterior.g);
		stColor.AddRow();
		stColor.AddInt(stColor.GetCount() - 1, m_cInterior.b);
		char *szColor = stColor.ToText("", ",");
		STRING("InteriorColor", szColor)
		stColor.DeleteToText(szColor);
	}

	INT("Smooth", GetTransition())
	INT("Flat", GetFlat()) // KFR version >= 2

	INT("MultiColor", m_bMW)
	INT("BlendMC", m_bBlend)

	{
		CStringTable stColors;
		int i;
		for (i = 0; i<m_nMW; i++){
			stColors.AddRow();
			stColors.AddInt(stColors.GetCount() - 1, m_MW[i].nPeriod);
			stColors.AddInt(stColors.GetCount() - 1, m_MW[i].nStart);
			stColors.AddInt(stColors.GetCount() - 1, m_MW[i].nType);
		}
		char *szColors = stColors.ToText("\t", ",");
		STRING("MultiColors", szColors)
		stColors.DeleteToText(szColors);
	}

	INT("Power", GetPower())
	INT("FractalType", GetFractalType())

	INT("Slopes", m_bSlopes)
	INT("SlopePower", m_nSlopePower);
	INT("SlopeRatio", m_nSlopeRatio);
	INT("SlopeAngle", m_nSlopeAngle);

	DOUBLE("real", g_real)
	DOUBLE("imag", g_imag)
	DOUBLE("SeedR", g_SeedR)
	DOUBLE("SeedI", g_SeedI)
	DOUBLE("FactorAR", g_FactorAR)
	DOUBLE("FactorAI", g_FactorAI)
	INT("Period", g_period)

	INT("TextureEnabled", m_bTexture)
	DOUBLE("TextureMerge", m_nImgMerge)
	DOUBLE("TexturePower", m_nImgPower)
	INT("TextureRatio", m_nImgRatio)
	STRING("TextureFile", m_szTexture)
	INT("TextureResize", m_bTextureResize)

	// KFR version >= 3
	if (GetUseHybridFormula())
	{
		INT("UseHybridFormula", 1)
		STRING("HybridFormula", to_string(GetHybridFormula()).c_str())
	}

	// KFR version >= 3
	polar2 P = GetTransformPolar();
	DOUBLE("RotateAngle", P.rotate * deg)
	DOUBLE("StretchAngle", P.stretch_angle * deg)
	DOUBLE("StretchAmount", std::log2(P.stretch_factor))
	// KFR version >= 2150400
	INT("ImagPointsUp", P.sign < 0);

  // KFR version >= 2150200
  if (GetUseOpenGL())
  {
		INT("UseOpenGL", 1)
		STRING("GLSL", glsl_escape(GetGLSL()).c_str())
		INT("UseSRGB", GetUseSRGB())
  }

  // KFR version >= 2150500
  INT("TriangleInequalityAverage", GetTriangleInequalityAverage())

	INT("Version", kfr_version_number)

	char *szData = stSave.ToText(": ", "\r\n");
	std::string ret(szData);
	stSave.DeleteToText(szData);
	return ret;
}

BOOL CFraktalSFT::SaveFile(const std::string &szFile, bool overwrite)
{
	std::string szText(ToText());
	const char *szData = szText.c_str();
	DWORD dw;
	HANDLE hFile = CreateFile(szFile.c_str(), GENERIC_WRITE, 0, NULL, overwrite ? CREATE_ALWAYS : CREATE_NEW, 0, NULL);
	if (hFile == INVALID_HANDLE_VALUE)
		return FALSE;
	WriteFile(hFile, szData, strlen(szData), &dw, NULL);
	CloseHandle(hFile);
	return TRUE;
}

void CFraktalSFT::ResetParameters()
{
	// required for backward compatibility with older parameter files
	SetIterDiv(1);
	SetColorMethod(ColorMethod_Standard);
	SetDifferences(Differences_Traditional);
	SetColorOffset(0);
	SetPhaseColorStrength(0);
	SetFractalType(0);
	SetPower(2);
	SetSlopes(false, 50, 20, 45);
	g_real = 1;
	g_imag = 1;
	SetBailoutNormPreset(BailoutNorm_2);
	SetBailoutNormCustom(2);
	g_SeedR = 0;
	g_SeedI = 0;
	g_FactorAR = 1;
	g_FactorAI = 0;
}
