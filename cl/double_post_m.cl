// ...continued
    xr = xrn;
    xi = xin;
    dxa = dxan;
    dxb = dxbn;
    dya = dyan;
    dyb = dybn;
  }
  l->antal = antal;
  l->test1 = test1;
  l->test2 = test2;
  l->xr = Xxr;
  l->xi = Xxi;
  l->dr = (Xxr * dxa + Xxi * dya) / sqrt(test1);
  l->di = (Xxr * dxb + Xxi * dyb) / sqrt(test1);
}
