// ...continued
    x = xn;
    d = dn;
  }
  l->antal = antal;
  l->test1 = test1;
  l->test2 = test2;
  l->xr = Xxr;
  l->xi = Xxi;
  // Cauchy-Riemann
  l->dxa = d.re;
  l->dxb = fe_neg(d.im);
  l->dya = d.im;
  l->dyb = d.re;
}
