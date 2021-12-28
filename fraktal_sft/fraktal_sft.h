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

#include <atomic>

#include "Settings.h"
#include "CFixedFloat.h"
#include "CDecNumber.h"
#include "complex.h"
#include "floatexp.h"
#include "../common/matrix.h"
#include "itercount_array.h"
#include "colour.h"
#include "hybrid.h"
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
	~CFraktalSFT();

	Settings m_Settings;
#define DOUBLE(KEY) \
	inline double Get##KEY() const { return m_Settings.Get##KEY(); }; \
	inline void   Set##KEY(double x) { return m_Settings.Set##KEY(x); };
#define INT(KEY) \
	inline int64_t    Get##KEY() const { return m_Settings.Get##KEY(); }; \
	inline void   Set##KEY(int64_t x) { return m_Settings.Set##KEY(x); };
#define BOOL(KEY) \
	inline bool   Get##KEY() const { return m_Settings.Get##KEY(); }; \
	inline void   Set##KEY(bool x) { return m_Settings.Set##KEY(x); };

  // settings and parameters
	inline bool OpenSettings(const std::string &filename) { return m_Settings.OpenFile(filename); }
	inline bool SaveSettings(const std::string &filename, bool overwrite) const { return m_Settings.SaveFile(filename, overwrite); }

	void ResetParameters();
	BOOL(OpenResetsParameters)
	BOOL OpenFile(const std::string &szFile, BOOL bNoLocation = FALSE);
	BOOL OpenString(const std::string &szText, BOOL bNoLocation = FALSE);
	BOOL OpenMapB(const std::string &szFile, BOOL bReuseCenter = FALSE, double nZoomSize = 1);
	bool OpenMapEXR(const std::string &szFile);

	std::string ToText();
	BOOL SaveFile(const std::string &szFile, bool overwrite);
	int SaveJpg(const std::string &szFile, int nQuality, int nWidth = 0, int nHeight = 0);
	void SaveMap(const std::string &szFile);
	void SaveMapB(const std::string &szFile);



// Basics
	DOUBLE(ThreadsPerCore)
	INT(ThreadsReserveCore)
	// nParallel is calculated as GetThreadsPerCore() * sysinfo.dwNumberOfProcessors - GetThreadsReserveCore();
	// TODO refactor: export that as a method

// This part actually calculates a fractal
//
  // What and how do we calculate?
	inline NumberType GetNumberTypes() const { return m_Settings.GetNumberTypes(); };
	inline void SetNumberTypes(const NumberType x) { return m_Settings.SetNumberTypes(x); };

	bool m_UseHybridFormula;
	inline bool GetUseHybridFormula() const { return m_UseHybridFormula; };
	inline void SetUseHybridFormula(bool b)
	{
		SetReferenceStrictZero(true);
		m_UseHybridFormula = b;
	}

	hybrid_formula m_HybridFormula;
	const hybrid_formula &GetHybridFormula() const
	{
		return m_HybridFormula;
	};
	void SetHybridFormula(const hybrid_formula &h)
	{
		m_HybridFormula = h;
	};

	int m_nPower;
	int GetPower() const;
	void SetPower(int nPower);  // limits to [2;70]

	BOOL(Derivatives)       // also calculate slopes etc.
	//
	BOOL(UseNanoMB1)        // run experimental code?
	BOOL(UseNanoMB2)        // more experimental code?
	INT(OrderM)             // NanoMBx parameter
	INT(OrderN)             // NanoMBx parameter
	DOUBLE(RadiusScale)     // NanoMBx parameter 
	BOOL(InteriorChecking)  // NanoMBx: may speed up interior space calc
	//
	INT(SIMDVectorSize)     // CPU vector calc optimization
	INT(SIMDChunkSize)      // CPU vector calc optimization

  // Bail-out radius: deciding that a point is outside the set
	BailoutRadiusPreset m_nBailoutRadiusPreset;
	double m_nBailoutRadiusCustom;
	BailoutRadiusPreset GetBailoutRadiusPreset();
	void SetBailoutRadiusPreset(int nBailoutRadiusPreset);
	double GetBailoutRadiusCustom();
	void SetBailoutRadiusCustom(double nBailoutRadiusCustom);
	double GetBailoutRadius();
	floatexp GetBailoutSmall();

    // how to calculate the bail-out distance
	BailoutNormPreset m_nBailoutNormPreset;
	double m_nBailoutNormCustom;
	BailoutNormPreset GetBailoutNormPreset();
	void SetBailoutNormPreset(int nBailoutNormPreset);
	double GetBailoutNormCustom();
	void SetBailoutNormCustom(double nBailoutNormCustom);
	double GetBailoutNorm();

    // Smoothing. Accepts Log=0 and Sqrt=1 only. TODO inline these.
	SmoothMethod m_nSmoothMethod;
	SmoothMethod GetSmoothMethod();// { return m_nSmoothMethod; }
	void SetSmoothMethod(SmoothMethod nSmoothMethod);// { m_nSmoothMethod = m_nSmoothMethod; }
  // internals
	int *m_pnExpConsts;  // Pascal Triangle numbers for m_nPower
	int m_nPrevPower;    // current power of ExpConsts table. TODO simplify.
  //
  // Where do we calculate it? (Parameters etc)
	int m_nFractalType;
	void SetFractalType(int nFractalType);
	int GetFractalType() const;

	// Position of fractal view: where and how large
	CFixedFloat m_CenterRe, m_CenterIm, m_ZoomRadius;
	// radius is from center to top/bottom edge
	//
	// String versions of center and zoom
	std::string GetRe();
	std::string GetIm();
	std::string GetZoom();

	// String versions of address at specific positions.
	// TODO used only in Newton calc, most probably stupidly
	std::string GetRe(int nXPos, int nYPos);
	std::string GetIm(int nXPos, int nYPos);

	// internal/library use: set position, radius, possibly size
	void SetPosition(const CFixedFloat &re, const CFixedFloat &im, const CFixedFloat &radius, int nX, int nY);
	void SetPosition(const CDecNumber &re, const CDecNumber &im, const CDecNumber &radius);

	// GUI use: set position and zoom level.
	void SetPosition(const std::string &szR, const std::string &szI, const std::string &szZ);
	void SetPosition(const char *const szR, const char *const szI, const char *const szZ);


	BOOL(NoReuseCenter)     // when zooming out, re-use the center?

	// fractal size
	int m_nX, m_nY;
	void SetImageSize(int nx, int ny);
	int GetWidth();  // returns m_nX
	int GetHeight(); // TODO inline this
	inline int64_t GetImageWidth() { if (m_nX) return m_nX; return CalcImageWidth(); }
	inline int64_t GetImageHeight() { if (m_nY) return m_nY; return CalcImageHeight(); }
	int64_t CalcImageHeight();  // returns h*s; TODO should simply return m_nX
	int64_t CalcImageWidth();   // returns w*s; TODO should simply return m_nY

	// forwards to Settings: stores the target image size and a supersampling factor
	inline void GetTargetDimensions(int64_t *w, int64_t *h, int64_t *s) const { return m_Settings.GetTargetDimensions(w, h, s); }
	inline void SetTargetDimensions(int64_t w, int64_t h, int64_t s) { m_Settings.SetTargetDimensions(w, h, s); SetImageSize(w * s, h * s); }

	// These handle resizing. TODO clean up.
	bool m_bResized;
	int m_nXPrev, m_nYPrev;  // Y size of bitmap

	// distance between two pixels: 2*radius / m_nY
	floatexp m_fPixelSpacing;

	// Jitter, i.e. vary pixel position slightly
	INT(JitterSeed)
	INT(JitterShape)
	DOUBLE(JitterScale)

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

	void DeleteArrays(); // Clean up the above arrays

    // Pixel calculation sequence; when interactive, this encodes Adam7-style
	// incremental refinement.
	CPixels m_P;

	int64_t m_nMaxIter; // iterate this often before declaring a point to be within the set.

	// Accessors. TODO inline them, they don't do any work.
	int64_t GetIterations();
	void SetIterations(int64_t nIterations);

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

	BOOL(AutoIterations)   // auto-calculate the iteration limit

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
	mat2 m_TransformMatrix;
	void SetTransformMatrix(const mat2 &M);
	mat2 GetTransformMatrix() const;  // TODO inline this

    // GUI's original (polar form) of transform matrix
	polar2 m_TransformPolar;
	void SetTransformPolar(const polar2 &P);
	polar2 GetTransformPolar() const;

  // actually calculate a fractal
  //
	// main renderer. TODO: this waits for prev render to finish, changes
	// images size, might allocate the bitmap and whatnot, before doing the
	// actual work. All of which should be factored out.
	// XXX drop hWnd parameter (useless)
	void RenderFractal(int nX, int nY, int64_t nMaxIter, HWND hWnd, BOOL bNoThread = FALSE, BOOL bResetOldGlitch = TRUE);

	bool m_bIsRendering;
	inline bool GetIsRendering() { return m_bIsRendering; };

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

	int GetArea(itercount_array &Node, int nXStart, int nXStop, int nEqSpan, itercount_array &Pixels, int nDone, POINT *pQ, int nQSize);
	// XXX "Pixels" is only ever used with an invalid array
	// TODO what does this actually do?

	// XXX what exactly does this do?
	BOOL Center(int &rx, int &ry, BOOL bSkipM = FALSE, BOOL bQuick = FALSE);

  // … and stop doing so.
	BOOL m_bRunning;          // Render running?
	void Stop();              // user interrupted (Escape key, Zoom, …)
	bool m_bStop;             // flag to tell calculating threads to stop
	BOOL m_bNoPostWhenDone;   // inhibits colouring after Stop() is called
	BOOL m_bInhibitColouring; // inhibits colouring during noninteractive usage
	//
#ifdef KF_OPENCL
  // calculate faster with GPUs
	BOOL(UseOpenCL)        // use it?
	BOOL(OpenCLThreaded)   // run OpenCL in render thread?
	INT(OpenCLPlatform)    // select which OpenCL impl to use
	int clid;              // device index
	OpenCL *cl;            // device interface
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
	BOOL(ThreadedReference) // use multiple threads for ref calculation? MB2 only
	Reference_Type GetReferenceType(int64_t exponent10) const;
	BOOL AddReference(int x, int y, BOOL bEraseAll = FALSE, BOOL bNoGlitchDetection = FALSE, BOOL bResuming = FALSE);

	INT(GlitchCenterMethod)          // Menu: advanced > Reference Selection
	INT(IsolatedGlitchNeighbourhood) // Menu: adv > Ignore isolated
	//     either 0 (ignore), 4 (orthogonal) or 8 (+diagonal)
	//
	// set when too many glitches found
	BOOL m_bNoGlitchDetection;
	POINT m_pOldGlitch[OLD_GLITCH];
	// TODO add a var for current length instead of terminating with x==-1
	int m_nMaxOldGlitches; // XXX constant OLD_GLITCH

	int mg_bAutoGlitch;          // #references  XXX it's not a bool!
    INT(MaxReferences)           // 0…OLD_GLITCH  XXX max# of secondary refs
	//
	BOOL(AutoSolveGlitches)      // auto-find glitches
	BOOL(SolveGlitchNear)        // only re-render connected pixels

	void IgnoreIsolatedGlitches();
	int FindCenterOfGlitch(int &rx, int &ry);
	void FindCenterOfGlitch(int x0, int x1, int y0, int y1, TH_FIND_CENTER *p);

  // references, for faster calculation
	BOOL(ReuseReference)         // Do not re-calculate the reference for further zooming
	BOOL(ReferenceStrictZero)        // Use strict zero test for reference dynamic range

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

    //BOOL(DerivativeGlitch)     // Use derivative-based glitch detection for power 2 Mandelbrot
	inline bool GetDerivativeGlitch() const {
		switch (GetReferenceType(m_nZoom)) {
			// disable for single precision (does not work properly)
			case Reference_Float:
			case Reference_ScaledFloat:
			case Reference_FloatExpFloat:
				return false;
			default:
				return m_Settings.GetDerivativeGlitch();
		}
	};
	inline void SetDerivativeGlitch(bool x)
	{
		return m_Settings.SetDerivativeGlitch(x);
	};

  // resizing
	int m_nZoom;                 // base10 exponent of zoom factor
	int GetExponent();           // TODO inline

	double m_nZooms;             // XXX unused
	std::string ToZoom();        // return a human-readable zoom scale. Also, set "m_nZoom".
	std::string ToZoom(const CDecNumber &z, int &zoom);

	BOOL m_bMirrored;            // XXX never set
	BOOL(Mirror)                 // XXX never read
	void Mirror(int x, int y);   // set value corresponding to mirrored x/y


  // random parameters
	BOOL(Guessing)               // guess interior of same-itercount regions
	BOOL(NoApprox)               // disable series approximation

	double m_epsilon;            // used for glitch avoidance. Undocumented constant.
	// TODO this is a constant: it doesn't belong here.

  // perturbation tuning             // Menu: Advanced > Perturbation and …
    DOUBLE(GlitchLowTolerance)       // tolerance for glitch detection, 0…1
    DOUBLE(ApproxLowTolerance)       // tolerance for series approximation, 0…1
    BOOL(AutoApproxTerms)            // choose ApproxTerms based on remaining pixels
	//
    //INT(ApproxTerms)               // Number of terms for series approximation
	inline int64_t    GetApproxTerms() const { return m_Settings.GetApproxTerms(); };
	void   SetApproxTerms(int64_t t);// updates m_APr and m_APi
	// approximation terms. Large.
	floatexp *m_APr; // settings.approxTerms large
	floatexp *m_APi;
	SeriesR2<double,int64_t> *m_APs; // always MAX_APPROX_TERMS sized
  
	BOOL(UseRescaledSeries)          // Use rescaled version of series approximation, power 2 MB only

  // Newton-Raphson zoom support
	BOOL(SaveNewtonProgress)         // status files. No read-back, so of limited use

// This part creates a nice image from the fractal

  // Colors
	COLOR14 m_cPos[1025];   // pre-calculated spread of m_cKeys, 1024 wide
	COLOR14 GetColor(int i);

	COLOR14 m_cKeys[1025];  // the color keys (set in the dialog)
	COLOR14 GetKeyColor(int i);
	void SetKeyColor(COLOR14 col, int i);
	int m_nParts;           // how many color keys are filled

	COLOR14 m_cInterior;    // uniform
	COLOR14 GetInteriorColor() { return m_cInterior; };
	void SetInteriorColor(const COLOR14 &c) { m_cInterior = c; };

	double m_nIterDiv;      // basic scaling of mapping the iter count to colors
	double GetIterDiv();    // XXX inline
	void SetIterDiv(double nIterDiv);    // XXX inline; simply tests for >0

	int m_nColorOffset;     // start pos in color list. TODO inline
	void SetColorOffset(int nColorOffset);
	int GetColorOffset();
	double m_nPhaseColorStrength;
	void SetPhaseColorStrength(double nPhaseColorStrength);
	double GetPhaseColorStrength();

	// these end up setting pixels
	void OutputIterationData(int x, int y, int w, int h, bool bGlitch, int64_t antal, double test1, double test2, double phase, double nBailout, const complex<double> &de, int power);
	void OutputIterationData(int x, int y, int w, int h, bool bGlitch, int64_t antal, double test1, double smooth, double phase, double nBailout, const complex<double> &de);
	void OutputPixelData(int x, int y, int w, int h, bool bGlitch);
	Guess GuessPixel(int x, int y, int x0, int y0, int x1, int y1);
	Guess GuessPixel(int x, int y, int w, int h);


	int m_nSeed;            // Seed for random colorization. Inconsistently used
	int GetSeed();          // TODO inline me

	// fill cKeys with random colors
	void GenerateColors(int nParts, int nSeed = -1);
	// fill cKeys with even more random colors. XXX Unused.
	void GenerateColors2(int nParts, int nSeed = -1, int nWaves = 9);

	// set r/g/b/y (ncol=0,1,2,3) to a sine wave
	// if ncol&4, overlay a sine wave instead
	void AddWave(int nCol, int nPeriod = -1, int nStart = -1);
	// TODO rename to Set…
	void ChangeNumOfColors(int nParts);
	int GetNumOfColors();

	// Infinite waves?
	MULTIWAVE m_MW[MULTIWAVE_MAX];  // see defs.h
	int m_nMW;    // how many?
	BOOL m_bMW;   // enabled?
	BOOL m_bBlend; // blend them?

	int GetMWCount();
	void SetMW(BOOL bMW, BOOL bBlend);
	int GetMW(BOOL *pbBlend = NULL);
	BOOL GetMW(int nIndex, int &nPeriod, int &nStart, int &nType);
	BOOL AddMW(int nPeriod, int nStart, int nType);
	BOOL UpdateMW(int nIndex, int nPeriod, int nStart, int nType);
	BOOL DeleteMW(int nIndex);


  // bitmap for the fractal colors
    // TODO strictly 24bit. Needs a 32-bit mode for embedding / unmodified pixbuf lib.
	HANDLE m_hMutex;                  // protet the stuff below
	int m_nSizeImage;                 // bytes in m_bmi = m_lpBits
	BYTE *m_lpBits;                   // fractal image bits (RGB)
	BITMAPINFOHEADER *m_bmi;          // bitmap header of fractal image bitmap
	HBITMAP m_bmBmp;                  // corresponding Windows device-specific bitmap
	void ReinitializeBitmap();

	int m_row;                        // Y stride (32-bit aligned)
	size_t GetArrayHalfColourStride() { return m_row; };

  // how to transform iterations to color indices
	ColorMethod m_nColorMethod;
	void SetColorMethod(int nColorMethod);
	ColorMethod GetColorMethod();

  // color slopes
	BOOL m_bSlopes;                   // whether to do them at all
	int m_nSlopePower;
	int m_nSlopeRatio;
	int m_nSlopeAngle;
	double m_nSlopeX, m_nSlopeY;      // cos/sin of the slope angle
	// accessors
	BOOL GetSlopes(int &nSlopePower, int &nSlopeRatio, int &nSlopeAngle); // returns m_bSlopes
	void SetSlopes(BOOL bSlope, int nSlopePower, int nSlopeRatio, int nSlopeAngle);


  // More coloring parameters
	BOOL m_bFlat;      // flat colors. TODO inline accessors
	BOOL GetFlat();
	void SetFlat(BOOL bFlat);

	BOOL m_bTrans;     // smooth color transitions. TODO inline accessors
	BOOL GetTransition();
	void SetTransition(BOOL bTransition);

	BOOL m_bITrans;    // ?? inverse color transitions. TODO inline accessors
	BOOL GetITransition();
	void SetITransition(BOOL bITransition);

	BOOL(ShowGlitches) // show in uniform color?

  // Fast coloring? Use OpenGL!
	// OpenGL accessors
	bool UseOpenGL();  // initializes OpenGL if enabled+necessary. Returns true if useable
	int m_opengl_major; // info only
	int m_opengl_minor;

	// Only call these if UseOpenGL() is true!
	inline bool OpenGL_Configure(const request_configure_t &req)
		{ return m_OpenGL->configure(req); }
	inline bool OpenGL_Compile(const std::string &fragment_src, std::string &log)
		{ return m_OpenGL->compile(fragment_src, log); }
	inline bool OpenGL_Render(const request_render_t &req)
		{ return m_OpenGL->render(req); }

	inline bool GetUseOpenGL() { return m_bUseOpenGL; }  // use this for settings
	void SetUseOpenGL(bool gl);  // turns OpenGL off when !gl

	std::unique_ptr<OpenGL_processor> m_OpenGL;  // our OpenGL instance
	bool m_bUseOpenGL;       // use it at all?
	bool m_bBadOpenGL;       // init failed: unuseable.

	std::string m_sGLSL;     // shader code fragment
	inline std::string GetGLSL() { return m_sGLSL; } // current shader fragment
	inline void SetGLSL(const std::string &gl) { m_bGLSLChanged |= (m_sGLSL != gl); m_sGLSL = gl; }

	std::string m_sGLSLLog;  // compilation log
	inline std::string GetGLSLLog() { return m_sGLSLLog; } // compile log
	inline void SetGLSLLog(const std::string &gl) { m_sGLSLLog = gl; }

	bool m_bGLSLChanged;     // changed since last compile
	bool m_bGLSLCompiled;    // false == not compileable

	bool m_bUseSRGB;         // use SRGB colors?
	inline bool GetUseSRGB() { return m_bUseSRGB; } // shader's SRGB flag
	inline void SetUseSRGB(bool b) { m_bUseSRGB = b; }

	// Triangle Inequity Average coloring, requires OpenGL
	bool m_bTriangleInequalityAverage;
	bool GetTriangleInequalityAverage() { return m_bTriangleInequalityAverage; };
	void SetTriangleInequalityAverage(bool b) { m_bTriangleInequalityAverage = b; if (b) SetNoApprox(true); };

	// Distance estimation
	Differences m_nDifferences;
	void SetDifferences(int nDifferences);
	Differences GetDifferences();

  // Texture support
	BOOL m_bTexture;                    // apply at all?
	double m_nImgMerge;                 // 0…1 how much texture to apply
	double m_nImgPower;                 // warping power
	int m_nImgRatio;                    // Texture height/width ratio. Corrently fixed at 100

	bool m_bTextureResize;              // dies loading the texture resize it to 
	inline bool GetTextureResize() const { return m_bTextureResize; }
	inline void SetTextureResize(bool resize) { m_bTextureResize = resize; }

	BYTE *m_lpTextureBits;
#ifdef WINVER
	std::string m_szTexture;            // file name
	BITMAPINFOHEADER m_bmiBkg;
	int m_rowBkg;
	TextureParams m_ActiveTextureParams;  // loaded currently
#endif

	void LoadTexture();
	void SetTexture(int x, int y, srgb &s);

	// access texture params
	BOOL GetTexture(double &nImgMerge,double &nImgPower,int &nImgRatio,std::string &szTexture);
	void SetTexture(BOOL bTexture,double nImgMerge,double nImgPower,int nImgRatio,const std::string &szTexture);

  // Calculate m_cPos, run OpenGL / CPU coloring
	void ApplyColors();
  // CPU coloring of a single range, one 16x16 square at a time
	void ApplyColors(int x0, int x1, int y0, int y1);
  // CPU coloring of a single pixel
	void SetColor(int nIndex, int64_t nIter, double offs, int x, int y, int w = 1, int h = 1);
	// XXX delete the redundant nIndex parameter and actually compute it in SetColor

  // EXR support
	half *m_imageHalf;     // linear SRGB data, for EXR export
	half *GetArrayHalfColour() { return m_imageHalf; };

    //BOOL(HalfColour)     // pre-calculate m_imageHalf (if you know you'll export EXR eventually)
	// Setter needs to alloc/destroy data
	inline bool GetHalfColour() const { return m_Settings.GetHalfColour(); };
	void SetHalfColour(bool b);

    //INT(EXRChannels)          // bitmap which channels to save to EXR files
	// unpacked to a struct for faster access
	inline EXRChannels GetEXRChannels() const { return m_Settings.GetEXRChannels(); };
	inline void SetEXRChannels(const EXRChannels x) { return m_Settings.SetEXRChannels(x); };

	BOOL(EXRParallel)           // save w/ multiple tasks
    BOOL(ExponentialMap)         // coordinate transform, for reassembly with zoomasm

	//
  // strictly GUI
	bool m_bInteractive;     // do we have a GUI at all?
	std::vector<std::string> m_undo;  // settings string lists for undo/redo
	std::vector<std::string> m_redo;

	void UndoStore() { m_undo.push_back(ToText()); m_redo.clear(); };
	void Undo() { if (! m_undo.empty()) { auto s = m_undo.back(); m_undo.pop_back(); m_redo.push_back(s); OpenString(s); } };
	void Redo() { if (! m_redo.empty()) { auto s = m_redo.back(); m_redo.pop_back(); m_undo.push_back(s); OpenString(s); } };


    DOUBLE(ZoomSize)             // zoom factor
    BOOL(AnimateZoom)        // animate zooming; currently only zoom factor 2 works correctly
	void Zoom(double nZoomSize);
	void Zoom(int nXPos, int nYPos, double nZoomSize, BOOL bReuseCenter = FALSE, bool autoRender = true, bool center_view = false);
	// 
	int CountFrames(int nProcent); // == log2(zoomlevel)*nPercent/100+1
	//
    INT(WindowWidth)         // window size, showing the (scaled) output image
    INT(WindowHeight)
    INT(Shrink)              // shrink quality (enum: fast default best sRGB)
	//
	BOOL(ArbitrarySize)      // flag: output to window ratio is not 1, XXX remove from config
	BOOL(ShowCrossHair)      // small magnifier window. Doesn't work on WINE?
	//
    // show special coloring
	void ApplyIterationColors();
	void ApplyPhaseColors();
	void ApplySmoothColors();

  // OS GUI interface
  //
	INT(WindowTop)
	INT(WindowLeft)
	INT(WindowBottom)
	INT(WindowRight)
	//
	BOOL(SaveOverwrites)    // flag for Save to append a timestamp to the filename
	//

// XXX dead/unused
	CFixedFloat m_storedr, m_storedi;
	void StoreLocation();
    //BOOL(LongDoubleAlways)
    //BOOL(FloatExpAlways)
	int m_nTotal;
	double m_xrd, m_xid;
	char *m_szPosition;
	BOOL m_bReuseRef;
	int m_nStatus;
	int m_nFrameDone;
	std::vector< complex<CFixedFloat> > m_Inflections;
	void AddInflectionPont(int x, int y);
	void RemoveInflectionPoint();
	void CreateLists();
	int nPos; // possibly used for debugging
	std::string GetPosition();  // not defined

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
#ifdef WINVER
	HWND m_hWnd;  // XXX also set by RenderFractal(…hWnd…)
	inline void SetWindow(HWND hWnd) { m_hWnd = hWnd; };

	HBITMAP GetBitmap();
	HBITMAP ShrinkBitmap(HBITMAP bmSrc,int nNewWidth,int nNewHeight,int mode = 1);
	void UpdateBitmap();
#endif

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

// XXX global-ize all of these
extern int g_nAddRefX;
extern int g_nAddRefY;

extern double g_real;
extern double g_imag;
extern double g_SeedR;
extern double g_SeedI;
extern double g_FactorAR;
extern double g_FactorAI;

// XXX only used in main_color. Move there and make static.
extern void HSVToRGB(double hue, double sat, double bri, COLOR14 &cPos);

// singleton instance
extern CFraktalSFT g_SFT;

// 1 is considered prime, 0 is not. TODO should be unsigned. XXX move?
extern int MakePrime(int n);

// XXX move this to matrix.h or whatever
template<typename T>
inline complex<double> compute_de(T Dr, T Di, T Jxa, T Jxb, T Jya, T Jyb, T s, const mat2 &TK)
{
  vec2 u = { double(Dr), double(Di) };
  mat2 J = { double(Jxa * s), double(Jxb * s), double(Jya * s), double(Jyb * s) };
  complex<double> v(u[0], u[1]);
  complex<double> num = abs(v) * log(abs(v));
  vec2 den = normalize(u) * (transpose(J) * TK);
  return num / complex<double>(den[0], den[1]);
}

#define KF_DEFAULT_GLSL "vec3 colour() { return KF_Colour(); }\n"

#endif
