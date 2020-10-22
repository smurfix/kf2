// ...continued
    xr = xrn;
    xi = xin;
    dr = drn;
    di = din;
  }
  l->antal = antal;
  l->test1 = test1;
  l->test2 = test2;
  l->xr = Xxr;
  l->xi = Xxi;
  // Cauchy-Riemann
  l->dxa = dr;
  l->dxb = fe_neg(di);
  l->dya = di;
  l->dyb = dr;
}
