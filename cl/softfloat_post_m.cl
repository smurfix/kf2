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
  l->dr = sf_from_double(sf_to_double(sf_add(sf_mul(Xxr, dxa), sf_mul(Xxi, dya))) / sqrt(sf_to_double(test1)));
  l->di = sf_from_double(sf_to_double(sf_add(sf_mul(Xxr, dxb), sf_mul(Xxi, dyb))) / sqrt(sf_to_double(test1)));
}
