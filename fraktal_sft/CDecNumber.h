#ifdef KF_FLOAT_BACKEND_CUSTOM

#include <windows.h>                 // for printf
#include <stdio.h>                 // for printf
#ifndef __CDECNUMBER_H__
#define __CDECNUMBER_H__

#ifdef _DEBUG
#define  DECNUMDIGITS 1010
#else
#ifdef _WIN64
#define DECUSE64 1
#define  DECNUMDIGITS 101016
#else
#define  DECNUMDIGITS 10106
#endif
//#define  DECNUMDIGITS 1010
#endif
extern "C"{
			#if !defined(DECCONTEXT)
			  #define DECCONTEXT
			  #define DECCNAME     "decContext"			/* Short name */
			  #define DECCFULLNAME "Decimal Context Descriptor"   /* Verbose name */
			  #define DECCAUTHOR   "Mike Cowlishaw" 	      /* Who to blame */

			  #include "stdint.h"		   /* C99 standard integers	      */
/*				typedef int int32_t;
				typedef unsigned char uint8_t;
				typedef unsigned int uint32_t;
*/			  #include <stdio.h>		   /* for printf, etc.		      */
			  #include <signal.h>		   /* for traps 		      */

			  /* Extended flags setting -- set this to 0 to use only IEEE flags   */
			  #if !defined(DECEXTFLAG)
			  #define DECEXTFLAG 1		   /* 1=enable extended flags	      */
			  #endif

			  /* Conditional code flag -- set this to 0 for best performance      */
			  #if !defined(DECSUBSET)
			  #define DECSUBSET  0		   /* 1=enable subset arithmetic      */
			  #endif

			  /* Context for operations, with associated constants		      */
			  enum rounding {
				DEC_ROUND_CEILING,		   /* round towards +infinity	      */
				DEC_ROUND_UP,		   /* round away from 0 	      */
				DEC_ROUND_HALF_UP,		   /* 0.5 rounds up		      */
				DEC_ROUND_HALF_EVEN,	   /* 0.5 rounds to nearest even      */
				DEC_ROUND_HALF_DOWN,	   /* 0.5 rounds down		      */
				DEC_ROUND_DOWN,		   /* round towards 0 (truncate)      */
				DEC_ROUND_FLOOR,		   /* round towards -infinity	      */
				DEC_ROUND_05UP,		   /* round for reround 	      */
				DEC_ROUND_MAX		   /* enum must be less than this     */
				};
			  #define DEC_ROUND_DEFAULT DEC_ROUND_HALF_EVEN;

			  typedef struct {
				int32_t  digits;		   /* working precision 	      */
				int32_t  emax;		   /* maximum positive exponent       */
				int32_t  emin;		   /* minimum negative exponent       */
				enum     rounding round;	   /* rounding mode		      */
				uint32_t traps;		   /* trap-enabler flags	      */
				uint32_t status;		   /* status flags		      */
				uint8_t  clamp;		   /* flag: apply IEEE exponent clamp */
				#if DECSUBSET
				uint8_t  extended;		   /* flag: special-values allowed    */
				#endif
				} decContext;

			  /* Maxima and Minima for context settings			      */
			  #define DEC_MAX_DIGITS 999999999
			  #define DEC_MIN_DIGITS	 1
			  #define DEC_MAX_EMAX	 999999999
			  #define DEC_MIN_EMAX		 0
			  #define DEC_MAX_EMIN		 0
			  #define DEC_MIN_EMIN	-999999999
			  #define DEC_MAX_MATH	    999999 /* max emax, etc., for math funcs. */

			  /* Classifications for decimal numbers, aligned with 754 (note that */
			  /* 'normal' and 'subnormal' are meaningful only with a decContext   */
			  /* or a fixed size format).					      */
			  enum decClass {
				DEC_CLASS_SNAN,
				DEC_CLASS_QNAN,
				DEC_CLASS_NEG_INF,
				DEC_CLASS_NEG_NORMAL,
				DEC_CLASS_NEG_SUBNORMAL,
				DEC_CLASS_NEG_ZERO,
				DEC_CLASS_POS_ZERO,
				DEC_CLASS_POS_SUBNORMAL,
				DEC_CLASS_POS_NORMAL,
				DEC_CLASS_POS_INF
				};
			  /* Strings for the decClasses */
			  #define DEC_ClassString_SN  "sNaN"
			  #define DEC_ClassString_QN  "NaN"
			  #define DEC_ClassString_NI  "-Infinity"
			  #define DEC_ClassString_NN  "-Normal"
			  #define DEC_ClassString_NS  "-Subnormal"
			  #define DEC_ClassString_NZ  "-Zero"
			  #define DEC_ClassString_PZ  "+Zero"
			  #define DEC_ClassString_PS  "+Subnormal"
			  #define DEC_ClassString_PN  "+Normal"
			  #define DEC_ClassString_PI  "+Infinity"
			  #define DEC_ClassString_UN  "Invalid"

			  /* Trap-enabler and Status flags (exceptional conditions), and      */
			  /* their names.  The top byte is reserved for internal use	      */
			  #if DECEXTFLAG
				/* Extended flags */
				#define DEC_Conversion_syntax    0x00000001
				#define DEC_Division_by_zero     0x00000002
				#define DEC_Division_impossible  0x00000004
				#define DEC_Division_undefined   0x00000008
				#define DEC_Insufficient_storage 0x00000010 /* [when malloc fails]	*/
				#define DEC_Inexact 	     0x00000020
				#define DEC_Invalid_context      0x00000040
				#define DEC_Invalid_operation    0x00000080
				#if DECSUBSET
				#define DEC_Lost_digits	     0x00000100
				#endif
				#define DEC_Overflow	     0x00000200
				#define DEC_Clamped 	     0x00000400
				#define DEC_Rounded 	     0x00000800
				#define DEC_Subnormal	     0x00001000
				#define DEC_Underflow	     0x00002000
			  #else
				/* IEEE flags only */
				#define DEC_Conversion_syntax    0x00000010
				#define DEC_Division_by_zero     0x00000002
				#define DEC_Division_impossible  0x00000010
				#define DEC_Division_undefined   0x00000010
				#define DEC_Insufficient_storage 0x00000010 /* [when malloc fails]	*/
				#define DEC_Inexact 	     0x00000001
				#define DEC_Invalid_context      0x00000010
				#define DEC_Invalid_operation    0x00000010
				#if DECSUBSET
				#define DEC_Lost_digits	     0x00000000
				#endif
				#define DEC_Overflow	     0x00000008
				#define DEC_Clamped 	     0x00000000
				#define DEC_Rounded 	     0x00000000
				#define DEC_Subnormal	     0x00000000
				#define DEC_Underflow	     0x00000004
			  #endif

			  /* IEEE 754 groupings for the flags				      */
			  /* [DEC_Clamped, DEC_Lost_digits, DEC_Rounded, and DEC_Subnormal    */
			  /* are not in IEEE 754]					      */
			  #define DEC_IEEE_754_Division_by_zero  (DEC_Division_by_zero)
			  #if DECSUBSET
			  #define DEC_IEEE_754_Inexact		 (DEC_Inexact | DEC_Lost_digits)
			  #else
			  #define DEC_IEEE_754_Inexact		 (DEC_Inexact)
			  #endif
			  #define DEC_IEEE_754_Invalid_operation (DEC_Conversion_syntax |     \
								  DEC_Division_impossible |   \
								  DEC_Division_undefined |    \
								  DEC_Insufficient_storage |  \
								  DEC_Invalid_context |       \
								  DEC_Invalid_operation)
			  #define DEC_IEEE_754_Overflow 	 (DEC_Overflow)
			  #define DEC_IEEE_754_Underflow	 (DEC_Underflow)

			  /* flags which are normally errors (result is qNaN, infinite, or 0) */
			  #define DEC_Errors (DEC_IEEE_754_Division_by_zero |		      \
						  DEC_IEEE_754_Invalid_operation |		      \
						  DEC_IEEE_754_Overflow | DEC_IEEE_754_Underflow)
			  /* flags which cause a result to become qNaN			      */
			  #define DEC_NaNs    DEC_IEEE_754_Invalid_operation

			  /* flags which are normally for information only (finite results)   */
			  #if DECSUBSET
			  #define DEC_Information (DEC_Clamped | DEC_Rounded | DEC_Inexact    \
						  | DEC_Lost_digits)
			  #else
			  #define DEC_Information (DEC_Clamped | DEC_Rounded | DEC_Inexact)
			  #endif

			  /* IEEE 854 names (for compatibility with older decNumber versions) */
			  #define DEC_IEEE_854_Division_by_zero  DEC_IEEE_754_Division_by_zero
			  #define DEC_IEEE_854_Inexact		 DEC_IEEE_754_Inexact
			  #define DEC_IEEE_854_Invalid_operation DEC_IEEE_754_Invalid_operation
			  #define DEC_IEEE_854_Overflow 	 DEC_IEEE_754_Overflow
			  #define DEC_IEEE_854_Underflow	 DEC_IEEE_754_Underflow

			  /* Name strings for the exceptional conditions		      */
			  #define DEC_Condition_CS "Conversion syntax"
			  #define DEC_Condition_DZ "Division by zero"
			  #define DEC_Condition_DI "Division impossible"
			  #define DEC_Condition_DU "Division undefined"
			  #define DEC_Condition_IE "Inexact"
			  #define DEC_Condition_IS "Insufficient storage"
			  #define DEC_Condition_IC "Invalid context"
			  #define DEC_Condition_IO "Invalid operation"
			  #if DECSUBSET
			  #define DEC_Condition_LD "Lost digits"
			  #endif
			  #define DEC_Condition_OV "Overflow"
			  #define DEC_Condition_PA "Clamped"
			  #define DEC_Condition_RO "Rounded"
			  #define DEC_Condition_SU "Subnormal"
			  #define DEC_Condition_UN "Underflow"
			  #define DEC_Condition_ZE "No status"
			  #define DEC_Condition_MU "Multiple status"
			  #define DEC_Condition_Length 21  /* length of the longest string,   */
							   /* including terminator	      */

			  /* Initialization descriptors, used by decContextDefault	      */
			  #define DEC_INIT_BASE 	0
			  #define DEC_INIT_DECIMAL32   32
			  #define DEC_INIT_DECIMAL64   64
			  #define DEC_INIT_DECIMAL128 128
			  /* Synonyms */
			  #define DEC_INIT_DECSINGLE  DEC_INIT_DECIMAL32
			  #define DEC_INIT_DECDOUBLE  DEC_INIT_DECIMAL64
			  #define DEC_INIT_DECQUAD    DEC_INIT_DECIMAL128

			  /* decContext routines					      */
				#if !defined(DECCONTEXTSYMBOLS)
				#define DECCONTEXTSYMBOLS

				#ifdef IN_LIBGCC2
				#define decContextClearStatus __decContextClearStatus
				#define decContextDefault __decContextDefault
				#define decContextGetRounding __decContextGetRounding
				#define decContextGetStatus __decContextGetStatus
				#define decContextRestoreStatus __decContextRestoreStatus
				#define decContextSaveStatus __decContextSaveStatus
				#define decContextSetRounding __decContextSetRounding
				#define decContextSetStatus __decContextSetStatus
				#define decContextSetStatusFromString __decContextSetStatusFromString
				#define decContextSetStatusFromStringQuiet __decContextSetStatusFromStringQuiet
				#define decContextSetStatusQuiet __decContextSetStatusQuiet
				#define decContextStatusToString __decContextStatusToString
				#define decContextTestSavedStatus __decContextTestSavedStatus
				#define decContextTestStatus __decContextTestStatus
				#define decContextZeroStatus __decContextZeroStatus
				#define DECPOWERS __decPOWERS
				#define DECSTICKYTAB __decSTICKYTAB
				#endif

				#endif
			  #ifdef __cplusplus
			  extern "C" {
			  #endif

			  extern decContext  * decContextClearStatus(decContext *, uint32_t);
			  extern decContext  * decContextDefault(decContext *, int32_t);
			  extern enum rounding decContextGetRounding(decContext *);
			  extern uint32_t      decContextGetStatus(decContext *);
			  extern decContext  * decContextRestoreStatus(decContext *, uint32_t, uint32_t);
			  extern uint32_t      decContextSaveStatus(decContext *, uint32_t);
			  extern decContext  * decContextSetRounding(decContext *, enum rounding);
			  extern decContext  * decContextSetStatus(decContext *, uint32_t);
			  extern decContext  * decContextSetStatusFromString(decContext *, const char *);
			  extern decContext  * decContextSetStatusFromStringQuiet(decContext *, const char *);
			  extern decContext  * decContextSetStatusQuiet(decContext *, uint32_t);
			  extern const char  * decContextStatusToString(const decContext *);
			  extern int32_t       decContextTestEndian(uint8_t);
			  extern uint32_t      decContextTestSavedStatus(uint32_t, uint32_t);
			  extern uint32_t      decContextTestStatus(decContext *, uint32_t);
			  extern decContext  * decContextZeroStatus(decContext *);

			  #ifdef __cplusplus
			  }
			  #endif

			#endif

			/* Decimal number arithmetic module header for the decNumber C Library.
			   Copyright (C) 2005-2013 Free Software Foundation, Inc.
			   Contributed by IBM Corporation.  Author Mike Cowlishaw.

			   This file is part of GCC.

			   GCC is free software; you can redistribute it and/or modify it under
			   the terms of the GNU General Public License as published by the Free
			   Software Foundation; either version 3, or (at your option) any later
			   version.

			   GCC is distributed in the hope that it will be useful, but WITHOUT ANY
			   WARRANTY; without even the implied warranty of MERCHANTABILITY or
			   FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
			   for more details.

			Under Section 7 of GPL version 3, you are granted additional
			permissions described in the GCC Runtime Library Exception, version
			3.1, as published by the Free Software Foundation.

			You should have received a copy of the GNU General Public License and
			a copy of the GCC Runtime Library Exception along with this program;
			see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
			<http://www.gnu.org/licenses/>.  */

			/* ------------------------------------------------------------------ */
			/* Decimal Number arithmetic module header			      */
			/* ------------------------------------------------------------------ */

			#if !defined(DECNUMBER)
			  #define DECNUMBER
			  #define DECNAME     "decNumber"			/* Short name */
			  #define DECFULLNAME "Decimal Number Module"	      /* Verbose name */
			  #define DECAUTHOR   "Mike Cowlishaw"		      /* Who to blame */

			  #if !defined(DECCONTEXT)
				#include "decContext.h"
			  #endif

			  /* Bit settings for decNumber.bits				      */
			  #define DECNEG    0x80      /* Sign; 1=negative, 0=positive or zero */
			  #define DECINF    0x40      /* 1=Infinity			      */
			  #define DECNAN    0x20      /* 1=NaN				      */
			  #define DECSNAN   0x10      /* 1=sNaN 			      */
			  /* The remaining bits are reserved; they must be 0		      */
			  #define DECSPECIAL (DECINF|DECNAN|DECSNAN) /* any special value     */

			  /* Define the decNumber data structure.  The size and shape of the  */
			  /* units array in the structure is determined by the following      */
			  /* constant.	This must not be changed without recompiling the      */
			  /* decNumber library modules. */

			  #define DECDPUN 9	      /* DECimal Digits Per UNit [must be >0  */
							  /* and <10; 3 or powers of 2 are best]. */

			  /* DECNUMDIGITS is the default number of digits that can be held in */
			  /* the structure.  If undefined, 1 is assumed and it is assumed     */
			  /* that the structure will be immediately followed by extra space,  */
			  /* as required.  DECNUMDIGITS is always >0.			      */
			  #if !defined(DECNUMDIGITS)
				#define DECNUMDIGITS 1
			  #endif

			  /* The size (integer data type) of each unit is determined by the   */
			  /* number of digits it will hold.				      */
			  #if	DECDPUN<=2
				#define decNumberUnit uint8_t
			  #elif DECDPUN<=4
				#define decNumberUnit uint16_t
			  #else
				#define decNumberUnit uint32_t
			  #endif
			  /* The number of units needed is ceil(DECNUMDIGITS/DECDPUN)	      */
			  #define DECNUMUNITS ((DECNUMDIGITS+DECDPUN-1)/DECDPUN)

			  /* The data structure... */
			  typedef struct {
				int32_t digits;	 /* Count of digits in the coefficient; >0    */
				int32_t exponent;	 /* Unadjusted exponent, unbiased, in	      */
						 /* range: -1999999997 through 999999999      */
				uint8_t bits;	 /* Indicator bits (see above)		      */
						 /* Coefficient, from least significant unit  */
				decNumberUnit lsu[DECNUMUNITS];
				} decNumber;

			  /* Notes:							      */
			  /* 1. If digits is > DECDPUN then there will one or more	      */
			  /*	decNumberUnits immediately following the first element of lsu.*/
			  /*	These contain the remaining (more significant) digits of the  */
			  /*	number, and may be in the lsu array, or may be guaranteed by  */
			  /*	some other mechanism (such as being contained in another      */
			  /*	structure, or being overlaid on dynamically allocated	      */
			  /*	storage).						      */
			  /*								      */
			  /*	Each integer of the coefficient (except potentially the last) */
			  /*	contains DECDPUN digits (e.g., a value in the range 0 through */
			  /*	99999999 if DECDPUN is 8, or 0 through 999 if DECDPUN is 3).  */
			  /*								      */
			  /* 2. A decNumber converted to a string may need up to digits+14    */
			  /*	characters.  The worst cases (non-exponential and exponential */
			  /*	formats) are -0.00000{9...}# and -9.{9...}E+999999999#	      */
			  /*	(where # is '\0')					      */


			  /* ---------------------------------------------------------------- */
			  /* decNumber public functions and macros			      */
			  /* ---------------------------------------------------------------- */

				#if !defined(DECNUMBERSYMBOLS)
				#define DECNUMBERSYMBOLS

				#ifdef IN_LIBGCC2
				#define decNumberAbs __decNumberAbs
				#define decNumberAdd __decNumberAdd
				#define decNumberAnd __decNumberAnd
				#define decNumberClass __decNumberClass
				#define decNumberClassToString __decNumberClassToString
				#define decNumberCompare __decNumberCompare
				#define decNumberCompareSignal __decNumberCompareSignal
				#define decNumberCompareTotal __decNumberCompareTotal
				#define decNumberCompareTotalMag __decNumberCompareTotalMag
				#define decNumberCopy __decNumberCopy
				#define decNumberCopyAbs __decNumberCopyAbs
				#define decNumberCopyNegate __decNumberCopyNegate
				#define decNumberCopySign __decNumberCopySign
				#define decNumberDivide __decNumberDivide
				#define decNumberDivideInteger __decNumberDivideInteger
				#define decNumberExp __decNumberExp
				#define decNumberFMA __decNumberFMA
				#define decNumberFromInt32 __decNumberFromInt32
				#define decNumberFromString __decNumberFromString
				#define decNumberFromUInt32 __decNumberFromUInt32
				#define decNumberGetBCD __decNumberGetBCD
				#define decNumberInvert __decNumberInvert
				#define decNumberIsNormal __decNumberIsNormal
				#define decNumberIsSubnormal __decNumberIsSubnormal
				#define decNumberLn __decNumberLn
				#define decNumberLog10 __decNumberLog10
				#define decNumberLogB __decNumberLogB
				#define decNumberMax __decNumberMax
				#define decNumberMaxMag __decNumberMaxMag
				#define decNumberMin __decNumberMin
				#define decNumberMinMag __decNumberMinMag
				#define decNumberMinus __decNumberMinus
				#define decNumberMultiply __decNumberMultiply
				#define decNumberNextMinus __decNumberNextMinus
				#define decNumberNextPlus __decNumberNextPlus
				#define decNumberNextToward __decNumberNextToward
				#define decNumberNormalize __decNumberNormalize
				#define decNumberOr __decNumberOr
				#define decNumberPlus __decNumberPlus
				#define decNumberPower __decNumberPower
				#define decNumberQuantize __decNumberQuantize
				#define decNumberReduce __decNumberReduce
				#define decNumberRemainder __decNumberRemainder
				#define decNumberRemainderNear __decNumberRemainderNear
				#define decNumberRescale __decNumberRescale
				#define decNumberRotate __decNumberRotate
				#define decNumberSameQuantum __decNumberSameQuantum
				#define decNumberScaleB __decNumberScaleB
				#define decNumberSetBCD __decNumberSetBCD
				#define decNumberShift __decNumberShift
				#define decNumberSquareRoot __decNumberSquareRoot
				#define decNumberSubtract __decNumberSubtract
				#define decNumberToEngString __decNumberToEngString
				#define decNumberToInt32 __decNumberToInt32
				#define decNumberToIntegralExact __decNumberToIntegralExact
				#define decNumberToIntegralValue __decNumberToIntegralValue
				#define decNumberToString __decNumberToString
				#define decNumberToUInt32 __decNumberToUInt32
				#define decNumberTrim __decNumberTrim
				#define decNumberVersion __decNumberVersion
				#define decNumberXor __decNumberXor
				#define decNumberZero __decNumberZero
				#define LNnn __decLNnn
				#define d2utable __decd2utable
				#endif

				#endif
			  #ifdef __cplusplus
			  extern "C" {
			  #endif

			  /* Conversions						      */
			  decNumber * decNumberFromInt32(decNumber *, int32_t);
			  decNumber * decNumberFromUInt32(decNumber *, uint32_t);
			  decNumber * decNumberFromString(decNumber *, const char *, decContext *);
			  char	    * decNumberToString(const decNumber *, char *);
			  char	    * decNumberToEngString(const decNumber *, char *);
			  uint32_t    decNumberToUInt32(const decNumber *, decContext *);
			  int32_t     decNumberToInt32(const decNumber *, decContext *);
			  uint8_t   * decNumberGetBCD(const decNumber *, uint8_t *);
			  decNumber * decNumberSetBCD(decNumber *, const uint8_t *, uint32_t);

			  /* Operators and elementary functions 			      */
			  decNumber * decNumberAbs(decNumber *, const decNumber *, decContext *);
			  decNumber * decNumberAdd(decNumber *, const decNumber *, const decNumber *, decContext *);
			  decNumber * decNumberAnd(decNumber *, const decNumber *, const decNumber *, decContext *);
			  decNumber * decNumberCompare(decNumber *, const decNumber *, const decNumber *, decContext *);
			  decNumber * decNumberCompareSignal(decNumber *, const decNumber *, const decNumber *, decContext *);
			  decNumber * decNumberCompareTotal(decNumber *, const decNumber *, const decNumber *, decContext *);
			  decNumber * decNumberCompareTotalMag(decNumber *, const decNumber *, const decNumber *, decContext *);
			  decNumber * decNumberDivide(decNumber *, const decNumber *, const decNumber *, decContext *);
			  decNumber * decNumberDivideInteger(decNumber *, const decNumber *, const decNumber *, decContext *);
			  decNumber * decNumberExp(decNumber *, const decNumber *, decContext *);
			  decNumber * decNumberFMA(decNumber *, const decNumber *, const decNumber *, const decNumber *, decContext *);
			  decNumber * decNumberInvert(decNumber *, const decNumber *, decContext *);
			  decNumber * decNumberLn(decNumber *, const decNumber *, decContext *);
			  decNumber * decNumberLogB(decNumber *, const decNumber *, decContext *);
			  decNumber * decNumberLog10(decNumber *, const decNumber *, decContext *);
			  decNumber * decNumberMax(decNumber *, const decNumber *, const decNumber *, decContext *);
			  decNumber * decNumberMaxMag(decNumber *, const decNumber *, const decNumber *, decContext *);
			  decNumber * decNumberMin(decNumber *, const decNumber *, const decNumber *, decContext *);
			  decNumber * decNumberMinMag(decNumber *, const decNumber *, const decNumber *, decContext *);
			  decNumber * decNumberMinus(decNumber *, const decNumber *, decContext *);
			  decNumber * decNumberMultiply(decNumber *, const decNumber *, const decNumber *, decContext *);
			  decNumber * decNumberNormalize(decNumber *, const decNumber *, decContext *);
			  decNumber * decNumberOr(decNumber *, const decNumber *, const decNumber *, decContext *);
			  decNumber * decNumberPlus(decNumber *, const decNumber *, decContext *);
			  decNumber * decNumberPower(decNumber *, const decNumber *, const decNumber *, decContext *);
			  decNumber * decNumberQuantize(decNumber *, const decNumber *, const decNumber *, decContext *);
			  decNumber * decNumberReduce(decNumber *, const decNumber *, decContext *);
			  decNumber * decNumberRemainder(decNumber *, const decNumber *, const decNumber *, decContext *);
			  decNumber * decNumberRemainderNear(decNumber *, const decNumber *, const decNumber *, decContext *);
			  decNumber * decNumberRescale(decNumber *, const decNumber *, const decNumber *, decContext *);
			  decNumber * decNumberRotate(decNumber *, const decNumber *, const decNumber *, decContext *);
			  decNumber * decNumberSameQuantum(decNumber *, const decNumber *, const decNumber *);
			  decNumber * decNumberScaleB(decNumber *, const decNumber *, const decNumber *, decContext *);
			  decNumber * decNumberShift(decNumber *, const decNumber *, const decNumber *, decContext *);
			  decNumber * decNumberSquareRoot(decNumber *, const decNumber *, decContext *);
			  decNumber * decNumberSubtract(decNumber *, const decNumber *, const decNumber *, decContext *);
			  decNumber * decNumberToIntegralExact(decNumber *, const decNumber *, decContext *);
			  decNumber * decNumberToIntegralValue(decNumber *, const decNumber *, decContext *);
			  decNumber * decNumberXor(decNumber *, const decNumber *, const decNumber *, decContext *);

			  /* Utilities							      */
			  enum decClass decNumberClass(const decNumber *, decContext *);
			  const char * decNumberClassToString(enum decClass);
			  decNumber  * decNumberCopy(decNumber *, const decNumber *);
			  decNumber  * decNumberCopyAbs(decNumber *, const decNumber *);
			  decNumber  * decNumberCopyNegate(decNumber *, const decNumber *);
			  decNumber  * decNumberCopySign(decNumber *, const decNumber *, const decNumber *);
			  decNumber  * decNumberNextMinus(decNumber *, const decNumber *, decContext *);
			  decNumber  * decNumberNextPlus(decNumber *, const decNumber *, decContext *);
			  decNumber  * decNumberNextToward(decNumber *, const decNumber *, const decNumber *, decContext *);
			  decNumber  * decNumberTrim(decNumber *);
			  const char * decNumberVersion(void);
			  decNumber  * decNumberZero(decNumber *);

			  /* Functions for testing decNumbers (normality depends on context)  */
			  int32_t decNumberIsNormal(const decNumber *, decContext *);
			  int32_t decNumberIsSubnormal(const decNumber *, decContext *);

			  /* Macros for testing decNumber *dn				      */
			  #define decNumberIsCanonical(dn) (1)	/* All decNumbers are saintly */
			  #define decNumberIsFinite(dn)    (((dn)->bits&DECSPECIAL)==0)
			  #define decNumberIsInfinite(dn)  (((dn)->bits&DECINF)!=0)
			  #define decNumberIsNaN(dn)	   (((dn)->bits&(DECNAN|DECSNAN))!=0)
			  #define decNumberIsNegative(dn)  (((dn)->bits&DECNEG)!=0)
			  #define decNumberIsQNaN(dn)	   (((dn)->bits&(DECNAN))!=0)
			  #define decNumberIsSNaN(dn)	   (((dn)->bits&(DECSNAN))!=0)
			  #define decNumberIsSpecial(dn)   (((dn)->bits&DECSPECIAL)!=0)
			  #define decNumberIsZero(dn)	   (*(dn)->lsu==0 \
								&& (dn)->digits==1 \
								&& (((dn)->bits&DECSPECIAL)==0))
			  #define decNumberRadix(dn)	   (10)

			  #ifdef __cplusplus
			  }
			  #endif

			#endif
}

class CDecNumber{
	decNumber m_dec;
	char *m_szString;
	BOOL m_bStop;
public:
	CDecNumber();
	CDecNumber(const CDecNumber &A);
	CDecNumber(const char *sz);
	CDecNumber(int a);
	CDecNumber(double a);
	~CDecNumber();

	void InitLib();
	decNumber &GetDecNumber()
	{
		return m_dec;
	}
	const decNumber &GetDecNumber() const
	{
		return m_dec;
	}

	void Parse(const char *sz);
	void SetMaxSignificant(int n);
	CDecNumber operator ^(const CDecNumber &A) const;
	CDecNumber operator %(int nA) const;

	CDecNumber operator*(const CDecNumber &A) const;
	CDecNumber operator/(const CDecNumber &A) const;
	CDecNumber operator+(const CDecNumber &A) const;
	CDecNumber operator-(const CDecNumber &A) const;
	CDecNumber operator%(const CDecNumber &A) const;
	CDecNumber operator-() const;
	CDecNumber &operator ++();

	CDecNumber &operator*=(const CDecNumber &A);
	CDecNumber &operator/=(const CDecNumber &A);
	CDecNumber &operator+=(const CDecNumber &A);
	CDecNumber &operator-=(const CDecNumber &A);
	CDecNumber &operator%=(const CDecNumber &A);
	CDecNumber &operator*=(int A);
	CDecNumber &operator/=(int A);
	CDecNumber &operator+=(int A);
	CDecNumber &operator-=(int A);
	CDecNumber &operator%=(int A);

	BOOL operator >(const CDecNumber &A) const;
	BOOL operator <(const CDecNumber &A) const;
	BOOL operator <=(const CDecNumber &A) const;
	BOOL operator >=(const CDecNumber &A) const;
	BOOL operator ==(const CDecNumber &A) const;
	BOOL operator !=(const CDecNumber &A) const;

	CDecNumber &operator =(const CDecNumber &A);
	CDecNumber &operator =(const char *sz);
	CDecNumber &operator =(double a);
	CDecNumber &operator =(int a);

//	operator char *();
	char *ToText(int nRound=0);
	int ToInt() const;
	double ToDouble() const;
//	operator double();
//	operator int();

	void SetAsInteger();
	CDecNumber SGN(const CDecNumber &x);
	void AddFraction(CDecNumber F1T,CDecNumber F1N,CDecNumber F2T,CDecNumber F2N,CDecNumber *pRT,CDecNumber *pRN);
	CDecNumber pi() const;
	CDecNumber abs() const;
	CDecNumber utropstecken() const;
	CDecNumber sqrt() const;
	CDecNumber exp() const;
	CDecNumber cos() const;
	CDecNumber sin() const;
	CDecNumber tan() const;
	CDecNumber acos() const;
	CDecNumber asin() const;
	CDecNumber atan() const;
	CDecNumber cosh() const;
	CDecNumber sinh() const;
	CDecNumber tanh() const;
	CDecNumber acosh() const;
	CDecNumber asinh() const;
	CDecNumber atanh() const;
	CDecNumber cot() const;
	CDecNumber sec() const;
	CDecNumber csc() const;
	CDecNumber acot() const;
	CDecNumber asec() const;
	CDecNumber acsc() const;
	CDecNumber ln() const;
	CDecNumber log10() const;
	CDecNumber modpow(CDecNumber y,CDecNumber m) const;
	CDecNumber prime() const;
	CDecNumber maked(CDecNumber x,CDecNumber *pK=NULL) const;
	CDecNumber Bernoulli(CDecNumber *pT=NULL,CDecNumber *pN=NULL) const;
	void Stop();
};

BOOL operator==(const CDecNumber &A,int nB);
BOOL operator==(int nB,const CDecNumber &A);
BOOL operator!=(const CDecNumber &A,int nB);
BOOL operator!=(int nB,const CDecNumber &A);
BOOL operator>(const CDecNumber &A,int nB);
BOOL operator>(int nB,const CDecNumber &A);
BOOL operator>(const CDecNumber &A,double nB);
BOOL operator>(double nB,const CDecNumber &A);
BOOL operator<(const CDecNumber &A,int nB);
BOOL operator<(int nB,const CDecNumber &A);
BOOL operator<(const CDecNumber &A,double nB);
BOOL operator<(double nB,const CDecNumber &A);

CDecNumber operator*(const CDecNumber &A,int nB);
CDecNumber operator*(int nB,const CDecNumber &A);
CDecNumber operator*(const CDecNumber &A,double nB);
CDecNumber operator*(double nB,const CDecNumber &A);
CDecNumber operator+(const CDecNumber &A,int nB);
CDecNumber operator+(int nB,const CDecNumber &A);
CDecNumber operator+(const CDecNumber &A,double nB);
CDecNumber operator+(double nB,const CDecNumber &A);
CDecNumber operator/(const CDecNumber &A,int nB);
CDecNumber operator/(int nB,const CDecNumber &A);
CDecNumber operator/(const CDecNumber &A,double nB);
CDecNumber operator/(double nB,const CDecNumber &A);
CDecNumber operator-(const CDecNumber &A,int nB);
CDecNumber operator-(int nB,const CDecNumber &A);
CDecNumber operator-(const CDecNumber &A,double nB);
CDecNumber operator-(double nB,const CDecNumber &A);
CDecNumber operator^(const CDecNumber &A,long nB);

int operator+=(int &nB,const CDecNumber &A);
int operator+=(double &nB,const CDecNumber &A);
int operator+=(CDecNumber &A,const double &nB);
#endif //__CDECNUMBER_H__

#else

#ifndef KF_CDECNUMBER_H
#define KF_CDECNUMBER_H

//#define KF_FLOAT_BACKEND_MPFR
#ifdef KF_FLOAT_BACKEND_MPFR

#define BOOST_MULTIPRECISION_MPFR_DEFAULT_PRECISION 20u
#include <boost/multiprecision/mpfr.hpp>
typedef boost::multiprecision::number<boost::multiprecision::mpfr_float_backend<0>> decNumber;

#else

#include <boost/multiprecision/gmp.hpp>
typedef boost::multiprecision::number<boost::multiprecision::gmp_float<0>> decNumber;

#endif

#define LOW_PRECISION 20u
#define DECNUMDIGITS 101016

class Precision
{
	unsigned digits10;
public:
	inline Precision(unsigned digits10)
	: digits10(digits10)
	{
		decNumber::default_precision(digits10);
	};
	inline ~Precision()
	{
		decNumber::default_precision(digits10);
	};
};

class CDecNumber
{
public:
	decNumber m_dec;
	char *m_szString;

  inline CDecNumber()
  {
		m_dec.precision(std::max(decNumber::default_precision(), LOW_PRECISION));
		m_dec = 0;
		m_szString = nullptr;
	};
  inline CDecNumber(const CDecNumber &a)
  {
		m_dec.precision(std::max(decNumber::default_precision(), a.m_dec.precision()));
		m_dec = a.m_dec;
		m_szString = nullptr;
	};
  inline CDecNumber(const decNumber &a)
  {
		m_dec.precision(std::max(decNumber::default_precision(), a.precision()));
		m_dec = a;
		m_szString = nullptr;
	};
  inline CDecNumber(const char *a)
  {
		m_dec.precision(std::max(decNumber::default_precision(), unsigned(strlen(a))));
		Precision p(m_dec.precision());
		m_dec = decNumber(a);
		m_szString = nullptr;
	};
  inline CDecNumber(double a)
  {
		m_dec.precision(std::max(decNumber::default_precision(), LOW_PRECISION));
		m_dec = a;
		m_szString = nullptr;
	};
  inline CDecNumber(int a)
  {
		m_dec.precision(std::max(decNumber::default_precision(), LOW_PRECISION));
		m_dec = a;
		m_szString = nullptr;
	};
  inline ~CDecNumber()
  {
		if (m_szString)
		  delete[] m_szString;
		m_szString = nullptr;
	};

  inline CDecNumber &operator*=(double b)
  {
		if (m_szString)
			delete[] m_szString;
		m_szString = nullptr;
		m_dec *= b;
		return *this;
	}

  inline friend CDecNumber operator-(const CDecNumber &a);
  inline friend CDecNumber operator+(const CDecNumber &a, const CDecNumber &b);
  inline friend CDecNumber operator-(const CDecNumber &a, const CDecNumber &b);
  inline friend CDecNumber operator*(const CDecNumber &a, const CDecNumber &b);
  inline friend CDecNumber operator/(const CDecNumber &a, const CDecNumber &b);
  inline friend CDecNumber operator^(const CDecNumber &a, int b);

  inline friend bool operator>(const CDecNumber &a, int b);
  inline friend bool operator<(const CDecNumber &a, int b);
  inline friend bool operator==(const CDecNumber &a, int b);
  inline friend bool operator<(const CDecNumber &a, const CDecNumber &b);

  char *ToText();

  inline int ToInt() const
  {
		return int(m_dec);
	};
};

inline CDecNumber operator-(const CDecNumber &a)
{
	Precision p(std::max(decNumber::default_precision(), a.m_dec.precision()));
	return CDecNumber(-a.m_dec);
}

inline CDecNumber operator+(const CDecNumber &a, const CDecNumber &b)
{
	Precision p(std::max(decNumber::default_precision(), std::max(a.m_dec.precision(), b.m_dec.precision())));
	return CDecNumber(a.m_dec + b.m_dec);
}

inline CDecNumber operator-(const CDecNumber &a, const CDecNumber &b)
{
	Precision p(std::max(decNumber::default_precision(), std::max(a.m_dec.precision(), b.m_dec.precision())));
	return CDecNumber(a.m_dec - b.m_dec);
}

inline CDecNumber operator*(const CDecNumber &a, const CDecNumber &b)
{
	Precision p(std::max(decNumber::default_precision(), std::max(a.m_dec.precision(), b.m_dec.precision())));
	return CDecNumber(a.m_dec * b.m_dec);
}

inline CDecNumber operator/(const CDecNumber &a, const CDecNumber &b)
{
	Precision p(std::max(decNumber::default_precision(), std::max(a.m_dec.precision(), b.m_dec.precision())));
	return CDecNumber(a.m_dec / b.m_dec);
}

inline CDecNumber operator^(const CDecNumber &a, int b)
{
	Precision p(std::max(decNumber::default_precision(), a.m_dec.precision()));
	using std::pow;
	return CDecNumber(pow(a.m_dec, b));
}

inline bool operator>(const CDecNumber &a, int b)
{
	return a.m_dec > b;
}

inline bool operator<(const CDecNumber &a, int b)
{
	return a.m_dec < b;
}

inline bool operator==(const CDecNumber &a, int b)
{
	return a.m_dec == b;
}

inline bool operator<(const CDecNumber &a, const CDecNumber &b)
{
	return a.m_dec < b.m_dec;
}

#endif

#endif
