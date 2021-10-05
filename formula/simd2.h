typedef int64_t int4 __attribute__ ((vector_size (32)));
typedef double vdouble4 __attribute__ ((vector_size (32)));
struct double4
{
  vdouble4 v;
  inline double4() { vdouble4 r = { 0, 0, 0, 0 }; v = r; };
  inline double4(double x) { vdouble4 r = { x, x, x, x }; v = r;};
  inline double4(double x0, double x1, double x2, double x3) { vdouble4 r = { x0, x1, x2, x3 }; v = r;};
  inline double4(const int4 &x) { vdouble4 r = { double(x[0]), double(x[1]), double(x[2]), double(x[3]) }; v = r; };
  inline double4(const double4 &x) { v = x.v; };
  inline double4(const vdouble4 &x) { v = x; };
  inline operator vdouble4() const { return v; };
  inline double operator[](int ix) const { return v[ix]; }
  inline double& operator[](int ix) { return ((double *)&v)[ix]; }
};
static inline double4 operator-(const double4 &a) { return double4(-a.v); }
static inline double4 operator+(const double4 &a, const double4 &b) { return double4(a.v + b.v); }
static inline double4 operator-(const double4 &a, const double4 &b) { return double4(a.v - b.v); }
static inline double4 operator*(const double4 &a, const double4 &b) { return double4(a.v * b.v); }
static inline double4 operator/(const double4 &a, const double4 &b) { return double4(a.v / b.v); }
static inline int4 operator<(const double4 &a, const double &b) { return a.v < b; }
static inline int4 operator>(const double4 &a, const double &b) { return a.v > b; }
static inline int4 operator<=(const double4 &a, const double &b) { return a.v <= b; }
static inline int4 operator>=(const double4 &a, const double &b) { return a.v >= b; }
static inline int4 operator<(const double4 &a, const double4 &b) { return a.v < b.v; }
static inline int4 operator>(const double4 &a, const double4 &b) { return a.v > b.v; }
static inline int4 operator<=(const double4 &a, const double4 &b) { return a.v <= b.v; }
static inline int4 operator>=(const double4 &a, const double4 &b) { return a.v >= b.v; }
static inline double4 operator+(const double &a, const double4 &b) { return double4(a + b.v); }
static inline double4 abs(const double4 &a) { return double4(a.v < 0.0 ? -a.v : a.v); }
static inline double4 diffabs(const double &c, const double4 &d)
{
  const double4 cd = c + d;
  const double4 c2d = 2.0 * c + d;
  return double4(c >= 0.0 ? cd.v >= 0.0 ? d.v : -c2d.v : cd.v > 0.0 ? c2d.v : -d.v);
}
static inline int4 isnan(const double4 &a) { int4 r = { isnan(a[0]), isnan(a[1]), isnan(a[2]), isnan(a[3]) }; return r; }
static inline int4 isinf(const double4 &a) { int4 r = { isinf(a[0]), isinf(a[1]), isinf(a[2]), isinf(a[3]) }; return r; }
static inline double4 infnan_to_zero(const double4 &a) { double4 r = { infnan_to_zero(a[0]), infnan_to_zero(a[1]), infnan_to_zero(a[2]), infnan_to_zero(a[3]) }; return r; }
static inline double4 pow(const double4 &a, const double p) { using std::pow; double4 r = { pow(a[0], p), pow(a[1], p), pow(a[2], p), pow(a[3], p) }; return r; }
static inline double4 max(const double4 &a, const double4 &b) { return double4(a.v > b.v ? a.v : b.v); }
static inline double4 sqr(const double4 &a) { return a * a; }
#define F4(F) static inline double4 F(const double4 &x) { return double4(F(x[0]), F(x[1]), F(x[2]), F(x[3])); }
F4(sgn)
F4(sin)
F4(cos)
F4(exp)
F4(expm1)
F4(log)
F4(log1p)
F4(sinh)
F4(cosh)
#undef F4

static inline bool all(const int4 &i) {
  return (((i[0] && i[1]) && (i[2] && i[3])));
}

static inline bool any(const int4 &i) {
  return (((i[0] || i[1]) || (i[2] || i[3])));
}
