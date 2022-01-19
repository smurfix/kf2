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

#ifndef KF_FRAKTAL_SFT_H
#define KF_FRAKTAL_SFT_H 1

#include <windows.h>
#include <half.h>

#include "kf-task.h"

#include "Settings.h"
#include "CFixedFloat.h"
#include "CDecNumber.h"
#include "complex.h"
#include "floatexp.h"
#include "../common/matrix.h"
#include "itercount_array.h"
#include "colour.h"
#include "hybrid.h"
#include "newton.h"
#include "main_numbertype.h"

#include "defs.h"
#include "opengl.h"

#ifdef KF_OPENCL
#include "../cl/opencl.h"
#endif

// terminology: the fractal is what we calculate. The image is the output
// that gets saved to disk, and the window is the on-screen view.

class CFraktalSFT
{
// TODO: prefix all of these declarations with either P_ or I_.
// #define P_ public:
// #define I_ private:

public:
	CFraktalSFT();
	CFraktalSFT(SP_Settings data);
	~CFraktalSFT();

	SP_Settings m_Settings;
	SP_Settings m_NewSettings;

	inline Settings& ModSettings() {
	  if(m_NewSettings == nullptr)
		m_NewSettings = NEW_SETTINGS(*m_Settings);
	  return *m_NewSettings;
	}

	std::string m_szFile;

private:
#include "Settings.scp.inc"
#include "Settings.pcp.inc"
#include "Settings.lcp.inc"

public:
#include "Settings.sca.inc"
#include "Settings.pca.inc"
#include "Settings.lca.inc"

#include "Settings.sgf.inc"
#include "Settings.pgf.inc"
#include "Settings.lgf.inc"

#include "Settings.ssf.inc"
#include "Settings.psf.inc"
#include "Settings.lsf.inc"

	bool ApplySettings(SP_Settings data);
	bool ApplyNewSettings();
	void UpdateHalfColour();
	void UpdateApproxTerms(int nT = -1);

	void PrepareSave();

  // settings and parameters
	bool OpenSettings(const std::string &filename) {
		return ModSettings().OpenFile(filename, true, false, false); }
	inline bool SaveSettings(const std::string &filename, bool overwrite) {
		PrepareSave();
		return m_Settings->SaveFile(filename, overwrite, true, false, false); }
	inline std::string GetSettings() {
		return m_Settings->ToText(true,false,false); }
	inline bool SetSettings(const std::string &data) {
		return ModSettings().FromText(data,true,false,false); }

private:

    void CloseOldSettings(SP_Settings data);
    bool OpenNewSettings(SP_Settings data);

public:

	void ResetParameters();
	// S BOOL(OpenResetsParameters)
	inline bool OpenFile(const std::string &filename, BOOL noLocation = FALSE) {
		return ModSettings().OpenFile(filename, false, true, !noLocation); }
	inline bool OpenString(const std::string &text, BOOL noLocation = FALSE) {
		return ModSettings().FromText(text, false, true, !noLocation); }

	BOOL OpenMapB(const std::string &filename, BOOL reuseCenter = FALSE, double zoomSize = 1);
	bool OpenMapEXR(const std::string &filename);

	std::string ToText() { return m_Settings->ToText(true,true,true); };
	BOOL SaveFile(const std::string &filename, bool overwrite, bool useSettings=false, bool useParams=true, bool useLocation=true) {
		PrepareSave();
		return m_Settings->SaveFile(filename, overwrite, useSettings,useParams,useLocation);
	}
#ifdef WINVER
	int SaveJpg(const std::string &filename, int quality, int width = 0, int height = 0);
#endif
	void SaveMap(const std::string &filename);
	void SaveMapB(const std::string &filename);


// Basics
	// S DOUBLE(ThreadsPerCore)
	// S INT(ThreadsReserveCore)
	// nParallel is calculated as GetThreadsPerCore() * sysinfo.dwNumberOfProcessors - GetThreadsReserveCore();
	// TODO refactor: export that as a method

// This part actually calculates a fractal
//
  // What and how do we calculate?
	// L int m_nFractalType;
	// P bool m_ReferenceStrictZero;
	// L bool m_UseHybridFormula;
	// L hybrid_formula m_HybridFormula;
	// P int m_nPower;
	int *m_pnExpConsts;     // Pascal Triangle, for m_nPower>10
	void UpdatePower();

	// S BOOL(Derivatives)       // also calculate slopes etc.
	//
	// S BOOL(UseNanoMB1)        // run experimental code?
	// S BOOL(UseNanoMB2)        // more experimental code?
	// S INT(OrderM)             // NanoMBx parameter
	// S INT(OrderN)             // NanoMBx parameter
	// S DOUBLE(RadiusScale)     // NanoMBx parameter 
	// S BOOL(InteriorChecking)  // NanoMBx: may speed up interior space calc
	//
	// S INT(SIMDVectorSize)     // CPU vector calc optimization
	// S INT(SIMDChunkSize)      // CPU vector calc optimization

  // Bail-out radius: deciding that a point is outside the set
	double GetBailoutRadius();
	// P BailoutRadiusPreset m_nBailoutRadiusPreset;
	// P double m_nBailoutRadiusCustom;
	floatexp GetBailoutSmall(); // XXX constant 1e-12

	// how to calculate the bail-out distance
	// L BailoutNormPreset m_nBailoutNormPreset;
	// L double m_nBailoutNormCustom;
	double GetBailoutNorm(); // depends on m_nBailoutNormPreset

	// Smoothing. Accepts Log=0 and Sqrt=1 only.
	// P SmoothMethod m_nSmoothMethod;
  //
  // Where do we calculate it? (Parameters etc)

	// Position of fractal view: where and how large
	// L CFixedFloat m_CenterRe, m_CenterIm, m_ZoomRadius;
	
	// radius is from center to top/bottom edge
	// set in SaveNew
	//
	// String versions of center and zoom
	// L std::string GetRe();
	// L std::string GetIm();
	// L std::string GetZoom();

	// String versions of address at specific positions.
	// TODO used only in Newton calc, most probably stupidly
	std::string GetRe(int nXPos, int nYPos);
	std::string GetIm(int nXPos, int nYPos);

	// GUI use: set position and zoom level.
	void SetPosition(const std::string &szR, const std::string &szI, const std::string &szZ)
	{
		m_Settings->SetPosition(szR,szI,szZ);
	}
	void SetPosition(const char *const szR, const char *const szI, const char *const szZ)
	{
		m_Settings->SetPosition(szR,szI,szZ);
	}

	// internal / embedded use: set position and zoom level.
	void SetPosition(const CDecNumber &re, const CDecNumber &im, const CDecNumber &zoom, unsigned digits10=0) {
		m_Settings->SetPosition(re,im,zoom,digits10);
	}

	// S BOOL(NoReuseCenter)     // when zooming out, re-use the center?

	// fractal size
	// S int m_nX, m_nY;
	void SetImageSize(int nx, int ny);

	void ClearImage();

	// forwards to Settings: stores the target image size and a supersampling factor
	inline void GetTargetDimensions(int64_t *w, int64_t *h, int64_t *s) { m_Settings->GetTargetDimensions(w, h, s); }
	inline void SetTargetDimensions(int64_t w, int64_t h, int64_t s) { ModSettings().SetTargetDimensions(w, h, s); }

	// Flag for resizing the main bitmap before rendering
	bool m_bResized;

	// distance between two pixels: 2*radius / m_nY
	floatexp m_fPixelSpacing;

	// Jitter, i.e. vary pixel position slightly
	// S INT(JitterSeed)
	// S INT(JitterShape)
	// S DOUBLE(JitterScale)

	// Retrieve jitter offset
	void GetPixelOffset(const int i, const int j, double &x, double &y) const;

	// Retrieve to-be-calculated locations (and slopes)
	void GetPixelCoordinates(const int i, const int j, floatexp &x, floatexp &y) const;
	void GetPixelCoordinates(const int i, const int j, floatexp &x, floatexp &y, floatexp &daa, floatexp &dab, floatexp &dba, floatexp &dbb) const;

	// These arrays contain the actual values.
	uint32_t *m_nPixels_LSB;
	uint32_t *m_nPixels_MSB; // if iterating beyond 2^32
	itercount_array m_nPixels; // A 64-bit, 2D view into m_nPixels_*
	itercount_array GetArrayCount() { return m_nPixels; };

	float **m_nTrans;  // fractional part of iteration count
	float *GetArrayTrans() { return m_nTrans[0]; };

	float **m_nPhase;  // TODO meaning=?
	float *GetArrayPhase() { return m_nPhase ? m_nPhase[0] : nullptr; };

	float **m_nDEx;    // derivatives (X)
	float *GetArrayDEx() { return GetDerivatives() ? m_nDEx[0] : nullptr; };

	float **m_nDEy;    // derivatives (Y)
	float *GetArrayDEy() { return GetDerivatives() ? m_nDEy[0] : nullptr; };

	// safely get the iteration value from m_nPixels (mutex; out-of-bounds-check)
	int64_t GetIterationOnPoint(int x, int y);
	double GetTransOnPoint(int x, int y);  // its fractional part

	// setting a pixel is SetColor(), below
	int GetColorIndex(int x, int y);  // into m_cPos (TODO verify)
	void ErasePixel(int x, int y);  // clear it

	void SetupArrays(); // Set up the above arrays
	void DeleteArrays(); // Clean up the above arrays

	// Pixel calculation sequence; when interactive, this encodes Adam7-style
	// incremental refinement.
	CPixels m_P;

	// L int64_t m_nMaxIter; // iterate this often before declaring a point to be within the set.
	// Get/SetIterations

	// Get the observed iteration min/max.
	// pnCalculated is the sum of all iterations, pnType is set to 1 if that should be *1mio.
	// XXX use an int64_t for pnCalc and remove pnType
	// if skipMaxIter is set, ignores max values; use this mode for display only!
	void GetIterations(int64_t &nMin, int64_t &nMax, int *pnCalculated = NULL, int *pnType = NULL, BOOL bSkipMaxIter = FALSE);
	int64_t GetMaxExceptCenter();  // skips 5x5 pixels
	// get the coordinates of max iteration
	BOOL HighestIteration(int &rx, int &ry);
	// Cached values of min+max iterations. TODO also cache nCalculated
	int64_t m_nMinI, m_nMaxI;
	BOOL m_bIterChanged;

	// S BOOL(AutoIterations)   // auto-calculate the iteration limit
	void FixIterLimit();   // calculate a new limit

  // approximation
	BOOL m_bNoApproximation;
	int64_t m_nMaxApproximation; // calculated XXX explain the math somewhere
	SeriesType GetApproximationType()
	{
		if (GetUseHybridFormula())
			return SeriesType_None;
		if (m_nFractalType == 0)
			return SeriesType_Complex;
		if (m_nFractalType == 1 && m_nPower == 2 && ! GetUseOpenCL())
			return SeriesType_Real;
		return SeriesType_None;
	}

	int64_t m_nApprox; // statistics only
	int64_t GetMaxApproximation();
	RECT m_rApprox; // currently useless, disabled in render.cpp

	floatexp m_pixel_center_x, m_pixel_center_y;  // XXX zero? (pixel_center_x = CenterRe-m_rref)
	floatexp m_pixel_scale; // distance between pixels: radius*2/nY

	CFixedFloat m_rref, m_iref;  // XXX starts off as center of image

  // transformation
	// matrix for calculation
	// L mat2 m_TransformMatrix;
	// L void SetTransformMatrix(const mat2 &M);
	// L inline mat2 GetTransformMatrix() const { return m_TransformMatrix; }

	// GUI's original (polar form) of transform matrix
	// L polar2 m_TransformPolar;
	// L void SetTransformPolar(const polar2 &P);
	// L inline polar2 GetTransformPolar() const { return m_TransformPolar; }

  // actually calculate a fractal
  //
	// main renderer. TODO: this waits for prev render to finish, changes
	// images size, might allocate the bitmap and whatnot, before doing the
	// actual work. All of which should be factored out.
#ifndef KF_EMBED
	void Render(BOOL bNoThread = FALSE, BOOL bResetOldGlitch = TRUE);
#endif

	void CalcStart();         // clear all pixels (possibly in parallel)
	void CalcStart(int x0, int x1, int y0, int y1);  // clear this area

	void RenderFractal();              // the actual rendering main code

	// dispatcher per number type, started in parallel by RenderFractal
	void MandelCalc(const Reference_Type reftype);
	void MandelCalcSIMD();
	template <typename mantissa> void MandelCalc1();
	template <typename mantissa, typename exponent> void MandelCalcScaled();
	void MandelCalcNANOMB1();
	void MandelCalcNANOMB2();

	// approximations
	void CalculateApproximation();
	void DoApproximation(int64_t &antal, const floatexp &D0r, const floatexp &D0i, floatexp &TDnr, floatexp &TDni, floatexp &TDDnr, floatexp &TDDni);
	void DoApproximation(int64_t &antal, const floatexp &a, const floatexp &b, floatexp &x, floatexp &y, floatexp &dxa, floatexp &dxb, floatexp &dya, floatexp &dyb);
	void DoApproximation(const floatexp &a, const floatexp &b, floatexp &x, floatexp &y); // for internal usage only, assumes isR

	void RenderFractal(int nX, int nY, int64_t nMaxIter, HWND hWnd, BOOL bNoThread = FALSE, BOOL bResetOldGlitch = TRUE);
	void Zoom(int nXPos, int nYPos, double nZoomSize, int nWidth, int nHeight, BOOL bReuseCenter = FALSE, bool autoRender = true, bool center_view = false);
	BOOL Center(int &rx, int &ry, BOOL bSkipM = FALSE, BOOL bQuick = FALSE);

  // … and stop doing so.
#ifndef KF_EMBED
	void Stop();              // user interrupted (Escape key, Zoom, …)
	BOOL m_bNoPostWhenDone;   // inhibits colouring after Stop() is called
#endif

	bool m_needRender;
	bool GetNeedRender() { return m_needRender; }
	void SetNeedRender();

	bool m_bIsRendering;

	inline bool GetIsRendering() { return m_bIsRendering; };
	bool m_bStop;             // flag to tell rendering threads to stop
	BOOL m_bInhibitColouring; // inhibits colouring during noninteractive usage

#ifdef KF_OPENCL
  // calculate faster with GPUs
	// S BOOL(UseOpenCL)        // use it?
	// S BOOL(OpenCLThreaded)   // run OpenCL in render thread?
	// S INT(OpenCLPlatform)    // select which OpenCL impl to use
	int clid;              // device index
	OpenCL *cl;            // device interface
	OpenCL_ErrorInfo cl_error; // opencl errors
	std::vector<cldevice> m_cldevices;

	int GetOpenCLDeviceIndex();
	void SetOpenCLDeviceIndex(int i);


	bool m_OpenCL_Glitched;
	int m_OpenCL_Glitched_X;
	int m_OpenCL_Glitched_Y;
	int64_t m_OpenCL_Glitched_Count;
#endif

  // more glitch handling
	BOOL m_bAddReference; // TODO explain what this does; XXX this is not a bool!
	int m_nAddRefX;
	int m_nAddRefY;
	// L double m_real;
	// L double m_imag;
	// L double m_SeedR;
	// L double m_SeedI;
	// L double m_FactorAR;
	// L double m_FactorAI;


	// S BOOL(ThreadedReference) // use multiple threads for ref calculation? MB2 only
	Reference_Type GetReferenceType(int64_t exponent10) const;
	BOOL AddReference(int x, int y, BOOL bEraseAll = FALSE, BOOL bResuming = FALSE);

	// S INT(GlitchCenterMethod)          // Menu: advanced > Reference Selection
	// S INT(IsolatedGlitchNeighbourhood) // Menu: adv > Ignore isolated
	//     either 0 (ignore), 4 (orthogonal) or 8 (+diagonal)
	//
	// set when too many glitches found
	BOOL m_bNoGlitchDetection;
	POINT m_pOldGlitch[OLD_GLITCH];  // TODO convert to a vector
	void ResetGlitches();

	// TODO add a var for current length instead of terminating with x==-1
	int m_nMaxOldGlitches; // XXX constant OLD_GLITCH

	int m_bAutoGlitch;          // #references  XXX it's not a bool!
	// S INT(MaxReferences)           // 0…OLD_GLITCH  XXX max# of secondary refs
	//
	// S BOOL(AutoSolveGlitches)      // auto-find glitches
	// S BOOL(SolveGlitchNear)        // only re-render connected pixels

	void IgnoreIsolatedGlitches();
	int FindCenterOfGlitch(int &rx, int &ry);
	void FindCenterOfGlitch(int x0, int x1, int y0, int y1, TH_FIND_CENTER *p);
	int GetArea(itercount_array &Node, int nXStart,int nYStart,int nEqSpan, itercount_array &Pixels, int nDone, POINT *pQ, int nQSize);

  // references, for faster calculation
	// S BOOL(ReuseReference)         // Do not re-calculate the reference for further zooming
	// S BOOL(ReferenceStrictZero)        // Use strict zero test for reference dynamic range

	void CalculateReference(enum Reference_Type reftype);
	bool CalculateReferenceThreaded();
	void CalculateReferenceNANOMB1();
	void CalculateReferenceNANOMB2();

	void RenderFractalNANOMB1();
	void RenderFractalNANOMB2();
#ifdef KF_OPENCL
	void RenderFractalOpenCL(Reference_Type reftype);
#endif

	Reference *m_Reference;
	NanoMB1_Reference *m_NanoMB1Ref;
	NanoMB2_Reference *m_NanoMB2Ref;

	Reference *m_ReferenceReuse;
	CFixedFloat m_rrefReuse, m_irefReuse;

	// BOOL(DerivativeGlitch)     // Use derivative-based glitch detection for power 2 Mandelbrot
	// S bool m_DerivativeGlitch;      // filtered: does not work for some reference types
	void UpdateDerivativeGlitch();

  // resizing
	// L int m_nZoom;                 // base10 exponent of zoom factor
	inline int GetExponent() { return m_nZoom; }
	// XXX rename this to GetZoomExponent

	std::string ToZoom();        // return a human-readable zoom scale. Also, set "m_nZoom".
	std::string ToZoom(const CDecNumber &z);

	// S BOOL(Mirror)                 // XXX never read
	void Mirror(int x, int y);   // set value corresponding to mirrored x/y

  // random parameters
	// S BOOL(Guessing)               // guess interior of same-itercount regions
	// S BOOL(NoApprox)               // disable series approximation

	double m_epsilon;            // used for glitch avoidance. Undocumented constant.
	// TODO this is a constant: it doesn't belong here.

  // perturbation tuning             // Menu: Advanced > Perturbation and …
	// S DOUBLE(GlitchLowTolerance)       // tolerance for glitch detection, 0…1
	// S DOUBLE(ApproxLowTolerance)       // tolerance for series approximation, 0…1
	// S BOOL(AutoApproxTerms)            // choose ApproxTerms based on remaining pixels
	//
	//INT(ApproxTerms)               // Number of terms for series approximation
	int m_nTerms;

	// approximation terms. Large.
	// Updated by UpdateApproxTerms.
	floatexp *m_APr; // settings.approxTerms large
	floatexp *m_APi;

	SeriesR2<double,int64_t> *m_APs; // always MAX_APPROX_TERMS sized
  
	// S BOOL(UseRescaledSeries)          // Use rescaled version of series approximation, power 2 MB only

  // Newton-Raphson zoom support
	struct CNewton N;                // XXX split that off
#ifdef KF_EMBED
	void ThNewton();
#endif
	// S BOOL(SaveNewtonProgress)         // status files. No read-back, so of limited use

// This part creates a nice image from the fractal

  // Colors
	COLOR14 m_cPos[1025];   // pre-calculated spread of m_cKeys, 1024 wide
	COLOR14 GetColor(int i);

	// P ColorArray m_cKeys;  // the color keys (set in the dialog)
	inline COLOR14 GetKeyColor(int i) { return m_Settings->GetKeyColor(i); }
	inline void SetKeyColor(COLOR14 col, int i) { ModSettings().SetKeyColor(col, i); }

	// P int m_nParts;           // how many color keys are filled

	// P COLOR14 m_cInterior;    // uniform

	// double m_nIterDiv;      // basic scaling of mapping the iter count to colors
	// P int m_nColorOffset;     // start pos in color list

	// P double m_nPhaseColorStrength;

	// these end up setting pixels
	void OutputIterationData(int x, int y, int w, int h, bool glitch, int64_t antal, double test1, double test2, double phase, double bailout, const complex<double> &de, int power);
	void OutputIterationData(int x, int y, int w, int h, bool glitch, int64_t antal, double test1, double smooth, double phase, const complex<double> &de);
	void OutputPixelData(int x, int y, int w, int h, bool glitch);
	Guess GuessPixel(int x, int y, int x0, int y0, int x1, int y1);
	Guess GuessPixel(int x, int y, int w, int h);


	// P int m_nSeed;            // Seed for random colorization. Inconsistently used

	// fill cKeys with random colors
	void GenerateColors(int nParts, int nSeed = -1) {
		ModSettings().GenerateColors(nParts, nSeed);
	}

	// set r/g/b/y (ncol=0,1,2,3) to a sine wave
	// if ncol&4, overlay a sine wave instead
	void AddWave(int col, int period = -1, int start = -1) {
		ModSettings().AddWave(col,period,start);
	}

	// Infinite waves?
	// P MultiWaveArray m_MW;  // see defs.h
	// P int m_nMW;    // how many?
	// P BOOL m_bMW;   // enabled?
	// P BOOL m_bBlend; // blend them?

	inline int GetMWCount() {
		return ModSettings().GetMWCount();
	}
	inline bool GetMW(int index, int &period, int &start, int &type) {
		return ModSettings().GetMW(index, period,start,type);
	}
	inline bool AddMW(int period, int start, int type) {
		return ModSettings().AddMW(period,start,type);
	}
	inline bool UpdateMW(int index, int period, int start, int type) {
		return ModSettings().UpdateMW(index, period,start,type);
	}
	inline bool DeleteMW(int index) {
		return ModSettings().DeleteMW(index);
	}

  // bitmap for the fractal colors
	BITMAPINFOHEADER *m_bmi;          // bitmap header of fractal image bitmap
	BYTE *m_lpBits;                   // fractal image bits (RGB / RGBA)
#ifdef WINVER
	HBITMAP m_bmBmp;                  // corresponding Windows device-specific bitmap
#endif
#ifndef KF_EMBED
	std::mutex m_mutex;                  // protect the stuff below
#endif
	void AllocateBitmap();
	void FreeBitmap();
	void ReinitializeBitmap();

	int m_row;                        // Y stride (32-bit aligned)
	int m_rowHalf;                    // Y stride (24-bit aligned)
	size_t GetArrayHalfColourStride() { return m_row; };

  // how to transform iterations to color indices
	// P ColorMethod m_nColorMethod;

  // color slopes
	// P BOOL m_bSlopes;                   // whether to do them at all
	// P int m_nSlopePower;
	// P int m_nSlopeRatio;
	// P int m_nSlopeAngle;
	double m_nSlopeX, m_nSlopeY;      // cos/sin of the slope angle
	void UpdateSlopes();              // called when the angle has changed
	// accessors
	BOOL GetSlopes(int &slopePower, int &slopeRatio, int &slopeAngle); // returns current m_bSlopes
	inline void SetSlopes(BOOL slope, int slopePower, int slopeRatio, int slopeAngle) {
		ModSettings().SetSlopes(slope);
		ModSettings().SetSlopePower(slopePower);
		ModSettings().SetSlopeRatio(slopeRatio);
		ModSettings().SetSlopeAngle(slopeAngle);
	}


  // More coloring parameters
	// P BOOL m_bFlat;      // flat colors
	// P BOOL GetFlat() { return m_bFlat; }
	// P void SetFlat(BOOL flat) { m_bFlat = flat; }

	// S BOOL(ShowGlitches) // show in uniform color?

  // Fast coloring? Use OpenGL!
	bool UseOpenGL();  // initializes OpenGL if enabled+necessary. Returns true if useable+locked.
	void StopUseOpenGL();  // close the OpenGL instance, if any is open

	OpenGL_processor *m_OpenGL; // our OpenGL instance
	int m_opengl_major; // info only
	int m_opengl_minor;

	// P bool m_bUseOpenGL;       // use it at all?
	bool m_bBadOpenGL;       // init failed: unuseable.
	inline bool GetBadOpenGL() { return m_bBadOpenGL; }

	// P std::string m_sGLSL;     // current shader fragment

	std::string m_sGLSLLog;  // compilation log
	inline std::string GetGLSLLog() { return m_sGLSLLog; } // compile log
	inline void SetGLSLLog(const std::string &gl) { m_sGLSLLog = gl; }

	bool m_bGLSLChanged;     // changed since last compile attempt
	bool m_bGLSLCompiled;    // false == not compileable

	// P bool m_bUseSRGB;         // use SRGB colors?

	// Triangle Inequity Average coloring, requires OpenGL
	// L bool m_bTriangleInequalityAverage;

	// Distance estimation
	// P Differences m_nDifferences;

  // Texture support
	// P BOOL m_bTexture;                    // apply at all?
	// P double m_nImgMerge;                 // 0…1 how much texture to apply
	// P double m_nImgPower;                 // warping power
	// P int m_nImgRatio;                    // Texture height/width ratio. Corrently fixed at 100

	// P bool m_bTextureResize;              // dies loading the texture resize it to 

	BYTE *m_lpTextureBits;
	// P std::string m_szTexture;            // file name
	//
	BITMAPINFOHEADER m_bmiBkg;
	int m_rowBkg;
	TextureParams m_ActiveTextureParams;  // loaded currently

	void LoadTexture();
	void SetTexture(int x, int y, srgb &s);

	// access texture params
	BOOL GetTexture(double &imgMerge,double &imgPower,int &imgRatio,std::string &filename);
	void SetTexture(BOOL doTexture,double imgMerge,double imgPower,int imgRatio,const std::string &filename);

  // Calculate m_cPos, run OpenGL / CPU coloring
	void ApplyColors();
  // CPU coloring of a single range, one 16x16 square at a time
	void ApplyColors(int x0, int x1, int y0, int y1);
  // CPU coloring of a single pixel
	void SetColor(int x, int y, int w = 1, int h = 1);

  // EXR support
	half *m_imageHalf;     // linear SRGB data, for EXR export
	half *GetArrayHalfColour() { return m_imageHalf; };

	//BOOL(HalfColour)     // pre-calculate m_imageHalf (if you know you'll export EXR eventually)
	// Setter needs to alloc/destroy data

	//INT(EXRChannels)          // bitmap which channels to save to EXR files
	// unpacked to a struct for faster access

	// S BOOL(EXRParallel)           // save w/ multiple tasks
	// S BOOL(ExponentialMap)         // coordinate transform, for reassembly with zoomasm

	//
  // strictly GUI
	bool m_bInteractive;     // do we have a GUI at all?

	std::vector<SP_Settings> m_undo;  // settings storage for undo/redo
	std::vector<SP_Settings> m_redo;

	void UndoStore(SP_Settings old) {
		m_undo.push_back(old);
		m_redo.clear();
	};
	void Undo() { if (! m_undo.empty()) { auto s = m_undo.back(); m_undo.pop_back(); m_redo.push_back(s); ApplySettings(s); } };
	void Redo() { if (! m_redo.empty()) { auto s = m_redo.back(); m_redo.pop_back(); m_undo.push_back(s); ApplySettings(s); } };

	// S DOUBLE(ZoomSize)             // zoom factor
	// S BOOL(AnimateZoom)        // animate zooming; currently only zoom factor 2 works correctly
	void Zoom(double nZoomSize);
	void Zoom(int xPos, int yPos, double zoomSize, BOOL reuseCenter = FALSE, bool centerView = false);
	// 
	// S INT(WindowWidth)         // window size, showing the (scaled) output image
	// S INT(WindowHeight)
	// S INT(Shrink)              // shrink quality (enum: fast default best sRGB)
	//
	// S BOOL(ArbitrarySize)      // flag: output to window ratio is not 1, XXX remove from config
	// S BOOL(ShowCrossHair)      // small magnifier window. Doesn't work on WINE?
	//
	// show special coloring
	void ApplyIterationColors();
	void ApplyPhaseColors();
	void ApplySmoothColors();

  // OS GUI interface
  //
	// S INT(WindowTop)
	// S INT(WindowLeft)
	// S INT(WindowBottom)
	// S INT(WindowRight)
	//
	// S BOOL(SaveOverwrites)    // flag for Save to append a timestamp to the filename
	//

  // somewhat-useful statistics
	std::atomic<uint32_t> m_count_good_guessed, m_count_good, m_count_queued, m_count_bad, m_count_bad_guessed;
	double
	  m_timer_total_wall_start,
	  m_timer_total_cpu_start,
	  m_timer_reference_wall,
	  m_timer_reference_cpu,
	  m_timer_approximation_wall,
	  m_timer_approximation_cpu,
	  m_timer_perturbation_wall,
	  m_timer_perturbation_cpu;
	int64_t m_nRDone; // count reference calculations

	double GetProgress(double *reference = nullptr, double *approximation = nullptr, double *good_guessed = nullptr, double *good = nullptr, double *queued = nullptr, double *bad = nullptr, double *bad_guessed = nullptr);
	void ResetTimers();
	void GetTimers(double *total_wall, double *total_cpu = nullptr, double *reference_wall = nullptr, double *reference_cpu = nullptr, double *approximation_wall = nullptr, double *approximation_cpu = nullptr, double *perturbation_wall = nullptr, double *perturbation_cpu = nullptr);

// Windows only stuff, TODO
#ifndef KF_EMBED
	HWND m_hWnd;  // XXX also set by RenderFractal(…hWnd…)
	inline void SetWindow(HWND hWnd) { m_hWnd = hWnd; };

	HBITMAP GetBitmap();
	HBITMAP ShrinkBitmap(HBITMAP bmSrc,int nNewWidth,int nNewHeight,int mode = 1);
	void UpdateBitmap();

// main window
	int SaveImage(const std::string &fileName,HBITMAP bmp,int quality, const std::string &comment);
	int SaveImage(const std::string &fileName, const BYTE *bits, int width, int height, int quality, const std::string &comment);
#endif

	int SaveEXR
		( const std::string &filename
		, const unsigned char *Data
		, int nWidth
		, int nHeight
		, const std::string &comment
		, unsigned int nParallel
		);

	bool ReadEXRMapFile(const std::string &filename, int threads);

	void Done()
	{
#ifdef KF_OPENCL
		if (cl)
		{
			m_count_good_guessed = 0;
			m_count_good = m_nX * m_nY;
			m_count_queued = 0;
			m_count_bad = 0;
			m_count_bad_guessed = 0;
		}
#endif
	}

	#undef DOUBLE
	#undef INT
	#undef BOOL


};

// singleton instance
#ifndef KF_EMBED
extern CFraktalSFT g_SFT;
#endif

// 1 is considered prime, 0 is not. TODO should be unsigned. XXX move?
extern int MakePrime(int n);

#define KF_DEFAULT_GLSL "vec3 colour() { return KF_Colour(); }\n"

#endif
