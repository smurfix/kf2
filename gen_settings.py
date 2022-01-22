#!/usr/bin/env python3


import sys
import os
from dataclasses import dataclass
import filecmp

CTYPES = {
    "s":"std::string",
    "i":"int",
    "f":"double",
    "b":"bool",
    "x":"std::string", # precision float

    "E":"int", # enum
    "F":"int", # bitfield
    "S":"std::string", # struct/list
    "L":"std::array", # std::array with separate length parameter
    # TODO: L should be converted to a std::vector
}

TYPES = {
    "ed": "entry data (descriptive data)",
    "cv": "class variables (m_NAME, for Settings)",
    "cp": "private variables (p_m_NAME, for SFT)",
    "ca": "read-only access references (m_NAME, for SFT)",
    "dv": "Initialisers (with comma prefix) attached to Settings' default constructor",
    "di": "Initialisation code for Settings' default constructor",
    "gt": "getter declarations, for Settings",
    "st": "setter declarations, for Settings",
    "gc": "getter code (GetNAME), for Settings",
    "sc": "setter code (SetNAME), for Settings",
    "gf": "inline getter code (GetNAME), for SFT",
    "sf": "inline setter code (SetNAME), for SFT",
    "hr": "hash read (Settings: copy data from string)",
    "hw": "hash write (Settings: copy data to string)",
    "cs": "Settings-to-CFraktal copy",
    "cc": "Settings-to-Settings copy",
    "eq": "Equality test (settings) to 'other'",
}

vars = []

def quote_str(val):
    if val is None:
        return '""'
    val = val.replace("\\","\\\\").replace('"','\\"')
    return '"'+val+'"'

def pairwise(l):
    # (a,b,c,d,e) > ((a,b),(c,d),(e,))
    it=iter(l)
    while True:
        try:
            a = next(it)
        except StopIteration:
            return
        try:
            b = next(it)
        except StopIteration:
            yield (a,None)
            return
        yield (a,b)

@dataclass(init=False)
class _Var:
    typ:str
    ctype:str
    varname:str
    name:str
    default:str
    descr:str
    tests:list[str,str]

    ref = ""
    const = ""
    skips = ()
    ename = ""
    dup_set = False

    def __init__(self, typ, loc, skip, ctype, varname, gsname, name, default=None, descr="", *tests, line=0):
        if ctype in ("",":"):
            ctype = CTYPES[typ]
        self.typ = typ
        self.loc = loc
        self.ctype = ctype
        self.gsname = name if gsname in ("",":") else gsname
        self.name = name
        self.default = self.cfg_to_py(default)
        self.descr = descr
        self.tests = ((a,self.cfg_to_py(b)) for a,b in pairwise(tests))
        self.line = line

        if skip not in ("",":"):
            neg = False
            if skip[0] == "!":
                neg = True
                skip = skip[1:]
            skips = set()
            while skip:
                skips.add(skip[0:2])
                skip = skip[2:]
            if neg:
                self.skips = skips
            else:
                self.skips = set()
                for k in TYPES:
                    if k not in skips:
                        self.skips.add(k)

        if varname in ("",":"):
            if "cv" in self.skips:
                self.varname = "v_"+name
            else:
                self.varname = "m_"+name
        else:
            self.varname = varname

    def skip(self,key):
        return key in self.skips

    ## Conversion stuff. We have
    ## - a string from the config file. If destined for an object it may be
    ##   - quoted: may be fed to the object's constructor
    ##   - unquoted: a C expression / name of a default object
    ##   Integers may be parseable or not; if not it's a C constant, to be used as-is.
    ## - an intermediate C representation ("ivar")
    ##     used esp. for limit-checking SetXXX values and config files
    ##   - string, for objects or strings
    ##   - int, for enums and bitfields
    ##   - otherwise a double/int/bool/whatever
    ## - the value of actual m_XXX variable
    ## - the string used to represent the value in the KFR settings/params file
    ##
    ## This is the default implementation. Its values are parseable by C.

    def cfg_to_py(self, value):
        """converts some text in the config file to a Python value"""
        raise RuntimeError("Override me",self.__class__.__name__)

    def ivar_to_data(self, expr):
        """how to convert the ivar-expr 'expr' to something assignable to m_XXX"""
        return expr

    def py_to_ivar(self, value):
        """convert the Python value to something assignable to an intermediate variable"""
        return value

    def mvar_to_str(self):
        """how to stringify the m_XXX variable: an expr that returns a std::string"""
        return f"std::to_string({self.varname})"

    def default_to_str(self):
        """how to stringify the default value. Used in gen_ed().
        Must return a C string *with* surrounding quotes."""
        return f'"{self.default}"'

    def str_to_ivar(self, var):
        """how to parse the string in 'var' to an intermediate repr"""
        raise RuntimeError("Override me",self.__class__.__name__)

    def py_to_data(self, value):
        if value is None:
            return ""
        return self.ivar_to_data(self.py_to_ivar(value))

    def str_to_data(self, value):
        return self.ivar_to_data(self.str_to_ivar(value))

#   def to_assign(self, value):
#       """converts the named value (maybe an int) to the data type (maybe an enum)"""
#       return value

#   def to_code(self, value):
#       """converts the Python value to a C constant"""
#       return str(value)

#   def to_str(self, value):
#       """converts the Python value to a quoted C string suitable for reading"""
#       if value is None:
#           return '""'
#       return f'"{value}"'

#   def from_string(self, data):
#       """wrap a string variable with a function that returns its data"""
#       return f"std::string({data})"

    def gen_ed(self):
        return f"""{{ '{self.typ}',"{self.ctype}","{self.name}",{self.default_to_str()},"{self.descr}"}},"""

    def gen_hr(self):
        """hash read"""
        if self.skip("sc"):
            return f"""\
                {{ int n = s.FindString(0, "{self.name}"); if (n != -1)
                   {self.varname} = {self.str_to_data('s[n][1]')};
                }}
"""
        return f"""\
                {{ int n = s.FindString(0, "{self.name}"); if (n != -1)
                   Set{self.gsname}({self.str_to_data('s[n][1]')});
                }}
"""

    def gen_hw(self):
        """hash write"""
        return f"""\
  {{ s.AddRow(); s.AddString(s.GetCount() - 1, "{self.name}");
    s.AddString(s.GetCount() - 1, {self.mvar_to_str()}); }}
"""

    def gen_cv(self):
        """Generates a variable declaration."""
        if self.descr:
            cmt = " " * max(1,30-len(self.name)-len(self.ctype)) + "// " + self.descr
        else:
            cmt = ""
        return f"""\
    {self.ctype} {self.varname};{cmt}
"""

    def gen_eq(self):
        """Equality test"""
        return f"""\
            if(!(Get{self.gsname}() == other->Get{self.gsname}()))
                return false;
"""

    def gen_cp(self):
        """Generates a private variable declaration."""
        return f"""\
    {self.ctype} p_{self.varname};
"""

    def gen_ca(self):
        """Generates a public alias"""
        return f"""\
    const {self.ctype}& {self.varname} = p_{self.varname};
"""

    def gen_cs(self):
        """copy to private"""
        return f"""\
    p_{self.varname} = orig.Get{self.gsname}();
"""

    def gen_cc(self):
        """copy to settings"""
        return f"""\
    {self.varname} = orig.{self.varname};
"""

    def gen_dv(self):
        """Generates an initialiser for Settings::Settings()."""
        return f"""
, {self.varname}({self.py_to_data(self.default)})
"""

    def gen_di(self):
        """Generates explici initialisation code."""
        return ""

#    def gen_gt(self):
#        """generates getter declarations """
#        res = f"""
#  {self.const} {self.ctype} {self.ref}Get{self.gsname}() const;
#"""
#        return res
#
#    def gen_gc(self):
#        """generates getter code for Settings"""
#        return f"""
#  {self.const} {self.ctype} {self.ref}Settings::Get{self.gsname}() const {{
#    return {self.varname};
#  }};
#"""
    def gen_gc(self):
        return ""

    def gen_gt(self):
        return f"""\
    inline {self.const} {self.ctype} {self.ref}Get{self.gsname}() const {{
        return {self.varname};
    }}
"""
    def gen_gf(self):
        """generates getter code for SFT"""
        if "cp" in self.skips:
            return f"""
  inline {self.const} {self.ctype} Get{self.gsname}() const {{
    return m_Settings->Get{self.gsname}();
  }};
"""
        return f"""
  inline {self.const} {self.ctype} {self.ref}Get{self.gsname}() const {{
    return p_{self.varname};
  }};
"""

    def gen_sf(self):
        """generates setter code for SFT"""
        return f"""
  inline void Set{self.gsname}({self.const} {self.ctype} {self.ref}var) {{
    ModSettings().Set{self.gsname}(var);
  }}
"""

    def gen_st(self):
        """generates setter declarations """
        res = f"""
  void Set{self.gsname}({self.const} {self.ctype} {self.ref}value);
"""
        if self.dup_set:
            res += f"""
  void Set{self.gsname}({self.const} {self.bctype} {self.ref}value);
"""
        return res

    def gen_sc(self):
        """setter code"""

        res = ""
        if self.dup_set:
            res = f"""\
  void Settings::Set{self.gsname}({self.const} {self.ctype} {self.ref}value) {{
    {self.varname} = value;
  }}
"""
            ctype = self.bctype

        else:
            ctype = self.ctype

        tests = []
        for t,v in self.tests:
            tt = t.replace("_X_","value")
            if v:
                if isinstance(v,str):
                    v = v.replace("_X_","value")
                tests.append(f"""
    if(!({tt})) {{
        value = {self.py_to_ivar(v)};
        if(!({tt}))
            throw_invalid("{self.name}", value);
    }}
""") # The compiler should be able to simplify this without our help
            else:
                tests.append(f"""
    if(!({tt}))
        throw_invalid("{self.name}", value);
""")
        res += f"""
  void Settings::Set{self.gsname}({self.const} {ctype} {self.ref}value) {{
{ "".join(tests) }
    {self.varname} = {self.ivar_to_data('value')};
  }}
"""
        return res;

class Var_s(_Var):
    ref = "&"
    const = "const"
    bctype="std::string"

    def cfg_to_py(self, value):
        return value

    def str_to_ivar(self, var):
        return var

    def default_to_str(self):
        dflt = self.default
        if not dflt:
            return '""'
        if dflt[0] == '"' and dflt[-1] == '"':
            return dflt
        return quote_str(self.default)

#   def to_str(self, value):
#       if value is None:
#           return '""'
#       if value and value[0] == '"' and value[-1] == '"':
#           return value
#       return self.to_code(value)

    def mvar_to_str(self):
        return f"({self.varname})"

class Var_x(Var_s):
    pass

class Var_b(_Var):
    bctype="bool"

    def cfg_to_py(self, value):
        if value in ("0","false","False"):
            return False
        if value in ("1","true","True"):
            return True
        raise ValueError(value)

    def str_to_ivar(self, data):
        return f"bool(str_atoi({data}))"

#   def to_str(self, value):
#       if value is None:
#           return '""'
#       return f'"{int(value)}"'

#   def to_assign(self, value):
#       """converts the named value (maybe an int) to the data type (maybe an enum)"""
#       return self.to_code(value)

    def py_to_ivar(self, value):
        if value in (0,"0"): return "false"
        if value in (1,"1"): return "true"
        return value

class Var_f(_Var):
    """A floating-point variable"""

    bctype="double"

    def cfg_to_py(self, value):
        if value is None:
            return value
        return float(value)

    def str_to_ivar(self, data):
        return f"str_atof({data})"

#   def to_code(self, data):
#       return str(data)

class Var_i(_Var):
    """An integer variable"""

    bctype="int64_t"

    def cfg_to_py(self, value):
        if value is None:
            return None
        try:
            return int(value, 0)
        except ValueError:
            # must be a C-level constant
            return value

    def str_to_ivar(self, data):
        return f"str_atoi({data})"

#   def to_code(self, data):
#       return str(data)

class Var_x(Var_s):
    """float"""
    def mvar_to_str(self):
        return f"({self.varname}).ToText()"

class Var_S(Var_s):
    """some random struct"""

    def mvar_to_str(self):
        return f"({self.varname}).to_string()"

#   def to_assign(self, value):
#       return f"{self.ctype}({self.to_code(value)})"

    def gen_hr(self):
        return f"""\
            {{ int n = s.FindString(0, "{self.name}"); if (n != -1)
                Set{self.gsname}({self.ctype}{{s[n][1]}});
            }}
"""

class Var_L(Var_s):
    """an array, with a separate length variable"""

    def __init__(self,*a,**kw):
        super().__init__(*a,**kw)
        self.varname,self.countname = self.varname.split(":")
        try:
            self.gsname,self.cgsname = self.gsname.split(":")
        except ValueError:
            self.cgsname = f"N_{self.gsname}"

    def gen_eq(self):
        """Equality test"""
        return f"""\
    for(int i=0;i<{self.countname};i++) {{
        if (!(Get{self.gsname}()[i] == other->Get{self.gsname}()[i]))
            return false;
    }}
"""

    def gen_hr(self):
        """hash read"""
        return f"""\
            {{ int n = s.FindString(0, "{self.name}"); if (n != -1)
                {self.countname} = {self.varname}.from_string(s[n][1]);
            }}
"""

    def gen_hw(self):
        """hash write"""
        return f"""\
  {{ s.AddRow(); s.AddString(s.GetCount() - 1, "{self.name}");
    s.AddString(s.GetCount() - 1, {self.varname}.to_string({self.countname})); }}
"""

    def gen_cp(self):
        """Generates a variable declaration."""
        res = super().gen_cp()
        res += f"""\
            int p_{self.countname};
"""
        return res

    def gen_ca(self):
        """Generates a shadow variable declaration."""
        res = super().gen_ca()
        res += f"""\
            const int& {self.countname} = p_{self.countname};
"""
        return res

    def gen_cv(self):
        """Generates a public-const variable."""
        res = super().gen_cv()
        res += f"""\
            int {self.countname};
"""
        return res

    def gen_cs(self):
        res = super().gen_cs()
        res += f"""\
            p_{self.countname} = orig.Get{self.cgsname}();
"""
        return res

    def gen_cc(self):
        res = super().gen_cc()
        res += f"""\
            {self.countname} = orig.{self.countname};
"""
        return res

#    def gen_gt(self):
#        res = super().gen_gt()
#        res += f"""
#  int Get{self.cgsname}() const;
#"""
#        return res
#
#    def gen_gc(self):
#        res = super().gen_gc()
#        res += f"""\
#    inline int Settings::Get{self.cgsname}() const {{ return {self.countname}; }}
#"""
    def gen_gt(self):
        res = super().gen_gt()
        res += f"""\
    inline int Get{self.cgsname}() const {{ return {self.countname}; }}
"""
        return res

    def gen_gf(self):
        res = super().gen_gf()
        res += f"""\
    inline int Get{self.cgsname}() const {{ return {self.countname}; }}
"""
        return res

    def gen_sf(self):
        #res = super().gen_sf()
        res = f"""\
    inline void Set{self.cgsname}(int val) {{ ModSettings().Set{self.cgsname}(val); }}
"""
        return res

    def gen_sc(self):
        #res = super().gen_st()
        return ""
#        res = f"""
#  void Set{self.cgsname}(int value);
#"""
#        return res

    def gen_st(self):
        #res = super().gen_sc()
        res = f"""\
    inline void Set{self.cgsname}(int value) {{ {self.countname} = value; }}
"""
        return res

    def gen_dv(self):
        if self.default:
            return ""
        return f"""\
, {self.countname}(0)
"""

    def gen_di(self):
        if not self.default:
            return ""
        return f"""\
    {self.countname} = {self.varname}.from_string({self.default_to_str()});
"""


class Var_E(Var_i):
    """an enum"""
    dup_set=True

    def ivar_to_data(self, expr):
        return f"{self.ctype}({expr})"

#   def to_code(self, value):
#       """converts the Python value to a C constant"""
#       return value

#   def to_assign(self, value):
#       """converts the Python value to a C constant"""
#       return f"{self.ctype}({value})"

#   def from_string(self, data):
#       return f"{self.ctype}(str_atoi({data}))"

    def mvar_to_str(self):
        return f"(std::to_string(static_cast<int>({self.varname})))"

    def gen_sf(self):
        res = super().gen_sf()
        res += f"""\
    inline void Set{self.gsname}(int val) {{ ModSettings().Set{self.gsname}({self.ivar_to_data('val')}); }}
"""
        return res
class Var_F(Var_E):
    """a bitfield"""
    def mvar_to_str(self):
        return f"({self.varname}).to_string()"


class Var(_Var):
    def __new__(cls,typ,*args,**kw):
        return globals()["Var_"+typ](typ,*args,**kw)


if len(sys.argv) == 2 and sys.argv[1] == "??":
    for k in TYPES.keys():
        print(k)
    sys.exit(0)

if len(sys.argv) != 5:
    print("""\
Usage: gen_settings LOC TYPE IN OUT

IN: Settings description (…/Settings.tab)
OUT: appropriate include file

LOCation:
l   Location
p   Parameter
s   Setting

Types:

"""+"\n".join(f"{a}  {b}" for a,b in TYPES.items())+"""
p   generate per-image params instead of settings.
l   location (may get skipped when reading)
s   location (may get skipped when reading)

Otherwise a Settings entry is built.
""", file=sys.stderr)
    sys.exit(1)

_,loc,typ,ifile,ofile = sys.argv

line=0
with (sys.stdin if ifile == "-" else open(ifile,"r")) as inf:
    for s in inf:
        line += 1
        s = s.strip()
        if not s or s[0] == "#" or s.startswith("'#"):
            # TODO
            continue
        if s[0] in ":.=":
            # TODO
            continue
        try:
            args = s.split("\t")
            v=Var(*args, line=line)
        except Exception as exc:
            print(f"Error: {args[2]} (line {line}): {exc!r}", file=sys.stderr)
            raise
            sys.exit(1)
        if v.loc == loc:
            vars.append(v)

otmp=ofile+".tmp"
with (sys.stdout if ofile == "-" else open(otmp,"w")) as outf:
    for v in vars:
        if v.skip(typ):
            continue
        try:
            print(getattr(v,"gen_"+typ)(), file=outf)
        except Exception as exc:
            print(f"Error: {v.name} (line {v.line}): {exc!r}", file=sys.stderr)
            raise
            sys.exit(1)
if ofile != "-":
    if os.path.exists(ofile) and filecmp.cmp(otmp,ofile):
        os.remove(otmp)
    else:
        os.rename(otmp, ofile)
