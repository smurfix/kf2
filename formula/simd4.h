typedef int64_t int16 __attribute__ ((vector_size (128)));
typedef double vdouble16 __attribute__ ((vector_size (128)));
struct double16
{
  vdouble16 v;
  inline double16() { vdouble16 r = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 }; v = r; };
  inline double16(double x) { vdouble16 r = { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x }; v = r;};
  inline double16(double x0, double x1, double x2, double x3, double x4, double x5, double x6, double x7, double x8, double x9, double x10, double x11, double x12, double x13, double x14, double x15) { vdouble16 r = { x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15 }; v = r;};
  inline double16(const int16 &x) { vdouble16 r = { double(x[0]), double(x[1]), double(x[2]), double(x[3]), double(x[4]), double(x[5]), double(x[6]), double(x[7]), double(x[8]), double(x[9]), double(x[10]), double(x[11]), double(x[12]), double(x[13]), double(x[14]), double(x[15]) }; v = r; };
  inline double16(const double16 &x) { v = x.v; };
  inline double16(const vdouble16 &x) { v = x; };
  inline operator vdouble16() const { return v; };
  inline double operator[](int ix) const { return v[ix]; }
  inline double& operator[](int ix) { return ((double *)&v)[ix]; }
};
static inline double16 operator-(const double16 &a) { return double16(-a.v); }
static inline double16 operator+(const double16 &a, const double16 &b) { return double16(a.v + b.v); }
static inline double16 operator-(const double16 &a, const double16 &b) { return double16(a.v - b.v); }
static inline double16 operator*(const double16 &a, const double16 &b) { return double16(a.v * b.v); }
static inline double16 operator/(const double16 &a, const double16 &b) { return double16(a.v / b.v); }
static inline int16 operator<(const double16 &a, const double &b) { return a.v < b; }
static inline int16 operator>(const double16 &a, const double &b) { return a.v > b; }
static inline int16 operator<=(const double16 &a, const double &b) { return a.v <= b; }
static inline int16 operator>=(const double16 &a, const double &b) { return a.v >= b; }
static inline int16 operator<(const double16 &a, const double16 &b) { return a.v < b.v; }
static inline int16 operator>(const double16 &a, const double16 &b) { return a.v > b.v; }
static inline int16 operator<=(const double16 &a, const double16 &b) { return a.v <= b.v; }
static inline int16 operator>=(const double16 &a, const double16 &b) { return a.v >= b.v; }
static inline double16 operator+(const double &a, const double16 &b) { return double16(a + b.v); }
static inline double16 abs(const double16 &a) { return double16(a.v < 0.0 ? -a.v : a.v); }
static inline double16 diffabs(const double &c, const double16 &d)
{
  const double16 cd = c + d;
  const double16 c2d = 2.0 * c + d;
  return double16(c >= 0.0 ? cd.v >= 0.0 ? d.v : -c2d.v : cd.v > 0.0 ? c2d.v : -d.v);
}
static inline int16 isnan(const double16 &a) { int16 r = { isnan(a[0]), isnan(a[1]), isnan(a[2]), isnan(a[3]), isnan(a[4]), isnan(a[5]), isnan(a[6]), isnan(a[7]), isnan(a[8]), isnan(a[9]), isnan(a[10]), isnan(a[11]), isnan(a[12]), isnan(a[13]), isnan(a[14]), isnan(a[15]) }; return r; }
static inline int16 isinf(const double16 &a) { int16 r = { isinf(a[0]), isinf(a[1]), isinf(a[2]), isinf(a[3]), isinf(a[4]), isinf(a[5]), isinf(a[6]), isinf(a[7]), isinf(a[8]), isinf(a[9]), isinf(a[10]), isinf(a[11]), isinf(a[12]), isinf(a[13]), isinf(a[14]), isinf(a[15]) }; return r; }
static inline double16 infnan_to_zero(const double16 &a) { double16 r = { infnan_to_zero(a[0]), infnan_to_zero(a[1]), infnan_to_zero(a[2]), infnan_to_zero(a[3]), infnan_to_zero(a[4]), infnan_to_zero(a[5]), infnan_to_zero(a[6]), infnan_to_zero(a[7]), infnan_to_zero(a[8]), infnan_to_zero(a[9]), infnan_to_zero(a[10]), infnan_to_zero(a[11]), infnan_to_zero(a[12]), infnan_to_zero(a[13]), infnan_to_zero(a[14]), infnan_to_zero(a[15]) }; return r; }
static inline double16 pow(const double16 &a, const double p) { using std::pow; double16 r = { pow(a[0], p), pow(a[1], p), pow(a[2], p), pow(a[3], p), pow(a[4], p), pow(a[5], p), pow(a[6], p), pow(a[7], p), pow(a[8], p), pow(a[9], p), pow(a[10], p), pow(a[11], p), pow(a[12], p), pow(a[13], p), pow(a[14], p), pow(a[15], p) }; return r; }
static inline double16 max(const double16 &a, const double16 &b) { return double16(a.v > b.v ? a.v : b.v); }
static inline double16 sqr(const double16 &a) { return a * a; }
#define F16(F) static inline double16 F(const double16 &x) { return double16(F(x[0]), F(x[1]), F(x[2]), F(x[3]), F(x[4]), F(x[5]), F(x[6]), F(x[7]), F(x[8]), F(x[9]), F(x[10]), F(x[11]), F(x[12]), F(x[13]), F(x[14]), F(x[15])); }
F16(sgn)
F16(sin)
F16(cos)
F16(exp)
F16(expm1)
F16(log)
F16(log1p)
F16(sinh)
F16(cosh)
#undef F16

static inline bool all(const int16 &i) {
  return (((i[0] && i[1]) && (i[2] && i[3])) &&
         ((i[4] && i[5]) && (i[6] && i[7]))) &&
         (((i[8] && i[9]) && (i[10] && i[11])) &&
         ((i[12] && i[13]) && (i[14] && i[15])));
}

static inline bool any(const int16 &i) {
  return (((i[0] || i[1]) || (i[2] || i[3])) ||
         ((i[4] || i[5]) || (i[6] || i[7]))) ||
         (((i[8] || i[9]) || (i[10] || i[11])) ||
         ((i[12] || i[13]) || (i[14] || i[15])));
}
