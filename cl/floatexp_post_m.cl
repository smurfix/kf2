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
  l->dr = fe_double(fe_add(fe_mul(Xxr, dxa), fe_mul(Xxi, dya))) / sqrt(test1);
  l->di = fe_double(fe_add(fe_mul(Xxr, dxb), fe_mul(Xxi, dyb))) / sqrt(test1);
}
