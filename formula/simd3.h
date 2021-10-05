typedef int64_t int8 __attribute__ ((vector_size (64)));
typedef double vdouble8 __attribute__ ((vector_size (64)));
struct double8
{
  vdouble8 v;
  inline double8() { vdouble8 r = { 0, 0, 0, 0, 0, 0, 0, 0 }; v = r; };
  inline double8(double x) { vdouble8 r = { x, x, x, x, x, x, x, x }; v = r;};
  inline double8(double x0, double x1, double x2, double x3, double x4, double x5, double x6, double x7) { vdouble8 r = { x0, x1, x2, x3, x4, x5, x6, x7 }; v = r;};
  inline double8(const int8 &x) { vdouble8 r = { double(x[0]), double(x[1]), double(x[2]), double(x[3]), double(x[4]), double(x[5]), double(x[6]), double(x[7]) }; v = r; };
  inline double8(const double8 &x) { v = x.v; };
  inline double8(const vdouble8 &x) { v = x; };
  inline operator vdouble8() const { return v; };
  inline double operator[](int ix) const { return v[ix]; }
  inline double& operator[](int ix) { return ((double *)&v)[ix]; }
};
static inline double8 operator-(const double8 &a) { return double8(-a.v); }
static inline double8 operator+(const double8 &a, const double8 &b) { return double8(a.v + b.v); }
static inline double8 operator-(const double8 &a, const double8 &b) { return double8(a.v - b.v); }
static inline double8 operator*(const double8 &a, const double8 &b) { return double8(a.v * b.v); }
static inline double8 operator/(const double8 &a, const double8 &b) { return double8(a.v / b.v); }
static inline int8 operator<(const double8 &a, const double &b) { return a.v < b; }
static inline int8 operator>(const double8 &a, const double &b) { return a.v > b; }
static inline int8 operator<=(const double8 &a, const double &b) { return a.v <= b; }
static inline int8 operator>=(const double8 &a, const double &b) { return a.v >= b; }
static inline int8 operator<(const double8 &a, const double8 &b) { return a.v < b.v; }
static inline int8 operator>(const double8 &a, const double8 &b) { return a.v > b.v; }
static inline int8 operator<=(const double8 &a, const double8 &b) { return a.v <= b.v; }
static inline int8 operator>=(const double8 &a, const double8 &b) { return a.v >= b.v; }
static inline double8 operator+(const double &a, const double8 &b) { return double8(a + b.v); }
static inline double8 abs(const double8 &a) { return double8(a.v < 0.0 ? -a.v : a.v); }
static inline double8 diffabs(const double &c, const double8 &d)
{
  const double8 cd = c + d;
  const double8 c2d = 2.0 * c + d;
  return double8(c >= 0.0 ? cd.v >= 0.0 ? d.v : -c2d.v : cd.v > 0.0 ? c2d.v : -d.v);
}
static inline int8 isnan(const double8 &a) { int8 r = { isnan(a[0]), isnan(a[1]), isnan(a[2]), isnan(a[3]), isnan(a[4]), isnan(a[5]), isnan(a[6]), isnan(a[7]) }; return r; }
static inline int8 isinf(const double8 &a) { int8 r = { isinf(a[0]), isinf(a[1]), isinf(a[2]), isinf(a[3]), isinf(a[4]), isinf(a[5]), isinf(a[6]), isinf(a[7]) }; return r; }
static inline double8 infnan_to_zero(const double8 &a) { double8 r = { infnan_to_zero(a[0]), infnan_to_zero(a[1]), infnan_to_zero(a[2]), infnan_to_zero(a[3]), infnan_to_zero(a[4]), infnan_to_zero(a[5]), infnan_to_zero(a[6]), infnan_to_zero(a[7]) }; return r; }
static inline double8 pow(const double8 &a, const double p) { using std::pow; double8 r = { pow(a[0], p), pow(a[1], p), pow(a[2], p), pow(a[3], p), pow(a[4], p), pow(a[5], p), pow(a[6], p), pow(a[7], p) }; return r; }
static inline double8 max(const double8 &a, const double8 &b) { return double8(a.v > b.v ? a.v : b.v); }
static inline double8 sqr(const double8 &a) { return a * a; }
#define F8(F) static inline double8 F(const double8 &x) { return double8(F(x[0]), F(x[1]), F(x[2]), F(x[3]), F(x[4]), F(x[5]), F(x[6]), F(x[7])); }
F8(sgn)
F8(sin)
F8(cos)
F8(exp)
F8(expm1)
F8(log)
F8(log1p)
F8(sinh)
F8(cosh)
#undef F8

static inline bool all(const int8 &i) {
  return (((i[0] && i[1]) && (i[2] && i[3])) &&
         ((i[4] && i[5]) && (i[6] && i[7])));
}

static inline bool any(const int8 &i) {
  return (((i[0] || i[1]) || (i[2] || i[3])) ||
         ((i[4] || i[5]) || (i[6] || i[7])));
}
