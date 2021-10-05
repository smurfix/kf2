// ...continued
    xr = xrn;
    xi = xin;
    dr = drn;
    di = din;
#ifdef TRIANGLE_INEQUALITY_AVERAGE
    {
      const floatexp abs_z2 = fe_add(fe_sqr(Xxr), fe_sqr(Xxi));
      const floatexp abs_z = fe_sqrt(abs_z2);
      const mantissa zsubcr = fe_double(Xxr) - Ccr;
      const mantissa zsubci = fe_double(Xxi) - Cci;
      const mantissa abs_zsubc2 = zsubcr * zsubcr + zsubci * zsubci;
      const mantissa abs_zsubc = sqrt(abs_zsubc2);
      const floatexp abs_z_sub_c_sub_abs_c_num = fe_sub(fe_sub(abs_z2, fe_muld(Xxr, 2 * Ccr)), fe_muld(Xxi, 2 * Cci));
      const mantissa abs_z_sub_c_sub_abs_c_den = sqrt(abs_zsubc2 + abs_c2) + abs_c;
      const floatexp abs_z_sub_c_sub_abs_c = fe_abs(fe_divd(abs_z_sub_c_sub_abs_c_num, abs_z_sub_c_sub_abs_c_den));
      const mantissa abs_z_sub_c_add_abs_c = abs_zsubc + abs_c;
      const floatexp tia_inc = fe_div(fe_sub(abs_z, abs_z_sub_c_sub_abs_c), fe_dsub(abs_z_sub_c_add_abs_c, abs_z_sub_c_sub_abs_c));
      if (antal != 0 && antal != g->nMaxIter - 1 && ! isnan(tia_inc.val) && ! isinf(tia_inc.val))
      {
        tia_sum_old = tia_sum;
        tia_sum = fe_add(tia_sum, tia_inc);
        tia_count += 1;
      }
    }
#endif
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
#ifdef TRIANGLE_INEQUALITY_AVERAGE
  {
    const mantissa Xxrd = fe_double(Xxr);
    const mantissa Xxid = fe_double(Xxi);
    const mantissa avg = tia_sum / tia_count;
    const mantissa avg_old = tia_sum_old / (tia_count - 1);
    const mantissa abs_z = sqrt(Xxrd * Xxrd + Xxid * Xxid);
    const mantissa abs_R = sqrt(g->m_nBailout2);
    const mantissa p = exp(l->log_m_nPower);
    mantissa f = (log(log(abs_z)) - log(log(abs_R))) / log(p);
    if (abs_z < abs_R)
    {
      f = 0;
    }
    const mantissa tia = avg + (avg_old - avg) * f;
    l->tia = tia;
  }
#endif
}
