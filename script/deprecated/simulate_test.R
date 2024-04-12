set.seed(1250211)
x=rgev(100,loc=10, scale=2, shape=0)
gev_fit_para_mix_stat(x)


sort(simulate_snow(loc =9.90722544, loctrend =0,scl =2.15829137,  shp =-0.02843616,
                    pr_zero =0,   n =100,  t=50))
sort(simulate_snow(loc =9.90722544, loctrend =-0.1,scl =2.15829137,  shp =-0.02843616,
               pr_zero =0,   n =100,  t=50))
sort(simulate_snow(loc =9.90722544, loctrend =0.1,scl =2.15829137,  shp =-0.02843616,
                   pr_zero =0,   n =100,  t=50))