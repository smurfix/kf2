typedef int64_t int2 __attribute__ ((vector_size (16)));
typedef double vdouble2 __attribute__ ((vector_size (16)));
struct double2
{
  vdouble2 v;
  inline double2() { vdouble2 r = { 0, 0 }; v = r; };
  inline double2(double x) { vdouble2 r = { x, x }; v = r;};
  inline double2(double x, double y) { vdouble2 r = { x, y }; v = r;};
  inline double2(const int2 &x) { vdouble2 r = { double(x[0]), double(x[1]) }; v = r; };
  inline double2(const double2 &x) { v = x.v; };
  inline double2(const vdouble2 &x) { v = x; };
  inline operator vdouble2() const { return v; };
  inline double operator[](int ix) const { return v[ix]; }
  inline double& operator[](int ix) { return ((double *)&v)[ix]; }
};
static inline double2 operator-(const double2 &a) { return double2(-a.v); }
static inline double2 operator+(const double2 &a, const double2 &b) { return double2(a.v + b.v); }
static inline double2 operator-(const double2 &a, const double2 &b) { return double2(a.v - b.v); }
static inline double2 operator*(const double2 &a, const double2 &b) { return double2(a.v * b.v); }
static inline double2 operator/(const double2 &a, const double2 &b) { return double2(a.v / b.v); }
static inline int2 operator<(const double2 &a, const double &b) { return a.v < b; }
static inline int2 operator>(const double2 &a, const double &b) { return a.v > b; }
static inline int2 operator<=(const double2 &a, const double &b) { return a.v <= b; }
static inline int2 operator>=(const double2 &a, const double &b) { return a.v >= b; }
static inline int2 operator<(const double2 &a, const double2 &b) { return a.v < b.v; }
static inline int2 operator>(const double2 &a, const double2 &b) { return a.v > b.v; }
static inline int2 operator<=(const double2 &a, const double2 &b) { return a.v <= b.v; }
static inline int2 operator>=(const double2 &a, const double2 &b) { return a.v >= b.v; }
static inline double2 operator+(const double &a, const double2 &b) { return double2(a + b.v); }
static inline double2 abs(const double2 &a) { return double2(a.v < 0.0 ? -a.v : a.v); }
static inline double2 diffabs(const double &c, const double2 &d)
{
  const double2 cd = c + d;
  const double2 c2d = 2.0 * c + d;
  return double2(c >= 0.0 ? cd.v >= 0.0 ? d.v : -c2d.v : cd.v > 0.0 ? c2d.v : -d.v);
}
static inline int2 isnan(const double2 &a) { int2 r = { isnan(a[0]), isnan(a[1]) }; return r; }
static inline int2 isinf(const double2 &a) { int2 r = { isinf(a[0]), isinf(a[1]) }; return r; }
static inline double2 infnan_to_zero(const double2 &a) { double2 r = { infnan_to_zero(a[0]), infnan_to_zero(a[1]) }; return r; }
static inline double2 pow(const double2 &a, const double p) { using std::pow; double2 r = { pow(a[0], p), pow(a[1], p) }; return r; }
static inline double2 max(const double2 &a, const double2 &b) { return double2(a.v > b.v ? a.v : b.v); }
static inline double2 sqr(const double2 &a) { return a * a; }
#define F2(F) static inline double2 F(const double2 &x) { return double2(F(x[0]), F(x[1])); }
F2(sgn)
F2(sin)
F2(cos)
F2(exp)
F2(expm1)
F2(log)
F2(log1p)
F2(sinh)
F2(cosh)
#undef F2

static inline bool all(const int2 &i) {
  return (((i[0] && i[1])));
}

static inline bool any(const int2 &i) {
  return (((i[0] || i[1])));
}
