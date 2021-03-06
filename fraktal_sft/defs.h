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

#ifndef KF_FRAKTAL_DEFS_H
#define KF_FRAKTAL_DEFS_H 1

#include <windows.h>

#include <ostream>
#include <string>
#include <string_view>
#include "StringHelper.h"

#include "CFixedFloat.h"
#include "floatexp.h"
#include "reference.h"

#include "kf-task.h"


// this sets the maximum number of references per image
#define OLD_GLITCH 10000

// this sets the range of approximation terms
// storage is O(terms^2) for R2 fractals, O(terms) for C fractals
#define MIN_APPROX_TERMS 3
#define MAX_APPROX_TERMS 63


#include <type_traits>
template <typename T>
constexpr auto operator+(T e) noexcept
    -> std::enable_if_t<std::is_enum<T>::value, std::underlying_type_t<T>>
{
    return static_cast<std::underlying_type_t<T>>(e);
}


struct Reference;
struct NanoMB1_Reference;
struct NanoMB2_Reference;

struct CPixel;

class CPixels
{
	int m_nX;
	int m_nY;
	int m_nY2;
	CPixel *m_pPixels;
	int m_nPixels;
	LONG m_nNextPixel;
	std::mutex mutex;

public:
	CPixels();
	void Init(int nX, int nY, bool interactive);
	BOOL GetPixel(int &x, int &y, int &w, int &h, BOOL bMirrored = 0);
#if 0
	BOOL GetPixels(int *px, int *py, int &nCount);
#endif
};

// magic value stored in m_nPixels[][] when pixel needs (re)computation
#define PIXEL_UNEVALUATED INT_MIN
// magic value stored in m_nTrans[][] when a glitch is detected
#if 1
#define SET_TRANS_GLITCH(x) (((x > 0) ? log2(x) : -1024.0) - 2048.0)
#else
#define SET_TRANS_GLITCH(x) (-1.0)
#endif
#define GET_TRANS_GLITCH(x) ((x) < 0.0f)

// these are smaller than expected due to risk of derivative overflow
// thresholds for switching to floatexp iterations
#define FLOATEXP_THRESHOLD_DEFAULT 4900
// thresholds for switching to long double iterations
#define LONG_DOUBLE_THRESHOLD_DEFAULT 290
// threshold for switching to double iterations
#define DOUBLE_THRESHOLD_DEFAULT 20

#define SMOOTH_BAILOUT 10000


// We use this to implement redirecting accesses to Settings
// without a metric ton of GetXXX()/SetXXX() methods
// or m_Settings-> stanzas everywhere.
//
// Example usage:
//
// class Test{
// public:
//   Property<int, Test> Number{this,&Test::setNumber,&Test::getNumber};
//
// private:
//   int itsNumber;
//
//   void setNumber(int theNumber)
//     { itsNumber = theNumber; }
//
//   int getNumber() const
//     { return itsNumber; }
// };
//
class CFraktal;

extern int MakePrime(int n);

struct MC
{
	CFixedFloat *xr, *xi, *sr, *si, *xrxid;
	HANDLE hDone;
	HANDLE hWait;
	HANDLE hExit;
	int nType;
};

struct MC2
{
	CFixedFloat *xrn, *xin, *xrxid, *sr, *si, *m_iref, *m_rref;
	HANDLE hDone;
	HANDLE hWait;
	HANDLE hExit;
	int nType;
};

struct COLOR14 {
	unsigned char r, g, b;

    inline COLOR14() : COLOR14(0,0,0) { }
	inline COLOR14(unsigned char rr, unsigned char gg, unsigned char bb) { r=rr; g=gg; b=bb; }
	inline COLOR14(uint32_t rgb) { r=rgb>>16; g=rgb>>8; b=rgb; }
	inline int32_t pack() const { return (r<<16) | (g<<8) | b; }
	COLOR14(std::string_view rgb);
	std::string to_string() const;
    inline bool operator==(const COLOR14 &other) const {
       return pack() == other.pack();
    }
    inline bool operator==(const COLOR14 &&other) const {
       return pack() == other.pack();
    }

};

// TODO should be a vector, but one step at a time
struct ColorArray : std::array<COLOR14,1025> {
	std::string to_string(int n) const;
	int from_string(std::string_view data);

	ColorArray() {}
	ColorArray(std::string_view data) : ColorArray() { from_string(data); }
};

static inline std::ostream& 
operator<<(std::ostream& output, COLOR14& col)
{
    output << col.to_string();
    return output;
}


#if 0
typedef long double ldbl;

struct ldblexp {
	ldbl val;
	__int64 exp;
};
#endif

#define MULTIWAVE_MAX 30
struct MULTIWAVE
{
	int nPeriod;
	int nStart;
	int nType;

    inline MULTIWAVE() : MULTIWAVE(0,0,0) { }
	inline MULTIWAVE(unsigned char p, unsigned char s, unsigned char t) {
		nPeriod = p;
		nStart = s;
		nType = t;
	}
    inline MULTIWAVE(std::string_view data) : MULTIWAVE() { from_string(data); }
	std::string to_string() const;
	void from_string(std::string_view data);
    inline bool operator==(const MULTIWAVE &other) const {
       return nPeriod == other.nPeriod && nStart == other.nStart && nType == other.nType;
    }
    inline bool operator==(const MULTIWAVE &&other) const {
       return nPeriod == other.nPeriod && nStart == other.nStart && nType == other.nType;
    }
};

struct MultiWaveArray : std::array<MULTIWAVE,MULTIWAVE_MAX> {
	std::string to_string(int n) const;
	int from_string(std::string_view data);

	MultiWaveArray() {}
	MultiWaveArray(std::string_view data) : MultiWaveArray() { from_string(data); }
};


enum SmoothMethod
{
	SmoothMethod_Log = 0,
	SmoothMethod_Sqrt = 1
};

enum BailoutRadiusPreset
{
	BailoutRadius_High = 0,
	BailoutRadius_2 = 1,
	BailoutRadius_Low = 2,
	BailoutRadius_Custom = 3
};

enum BailoutNormPreset
{
	BailoutNorm_1 = 0,
	BailoutNorm_2 = 1,
	BailoutNorm_Infinity = 2,
	BailoutNorm_Custom = 3
};

enum ColorMethod
{
	ColorMethod_Standard = 0,
	ColorMethod_SquareRoot = 1,
	ColorMethod_CubicRoot = 2,
	ColorMethod_Logarithm = 3,
	ColorMethod_Stretched = 4,
	ColorMethod_DistanceLinear = 5,
	ColorMethod_DEPlusStandard = 6,
	ColorMethod_DistanceLog = 7,
	ColorMethod_DistanceSqrt = 8,
	ColorMethod_LogLog = 9,
	ColorMethod_ATan = 10,
	ColorMethod_FourthRoot = 11
};

enum Differences
{
	Differences_Traditional = 0,
	Differences_Forward3x3 = 1,
	Differences_Central3x3 = 2,
	Differences_Diagonal2x2 = 3,
	Differences_LeastSquares2x2 = 4,
	Differences_LeastSquares3x3 = 5,
	Differences_Laplacian3x3 = 6,
	Differences_Analytic = 7
};

enum Guess
{
	Guess_No = 0,
	Guess_Interior = 1,
	Guess_Glitch = 2
};

enum SeriesType
{
	SeriesType_None = 0,
	SeriesType_Complex = 1,
	SeriesType_Real = 2
};

template<typename mantissa, typename exponent>
struct SeriesR2
{
	tfloatexp<mantissa, exponent> s[MAX_APPROX_TERMS+1][MAX_APPROX_TERMS+1];
	tfloatexp<mantissa, exponent> t[MAX_APPROX_TERMS+1][MAX_APPROX_TERMS+1];
};


struct TextureParams
{
	BOOL m_bTexture;
	std::string m_szTexture;
	double m_nImgPower;
	int m_nX;
	int m_nY;
	bool m_bTextureResize;
};

struct TH_FIND_CENTER;

struct CFraktalSFT;

struct TH_PARAMS
{
	int nXStart;
	int nXStop;
	CFraktalSFT *p;
	Reference_Type reftype;
};

#ifndef KF_EMBED
inline void ReportProgress(void *p, int d, const std::string &s) { SetDlgItemText((HWND)p,d,s.c_str()); }
#else
#define ReportProgress(p, d, s) do { } while(0)  // XXX use a callback
#endif

const double pi = 3.141592653589793;
extern const std::string version;
extern const int kfr_version_number;
extern const int kfs_version_number;

#endif
