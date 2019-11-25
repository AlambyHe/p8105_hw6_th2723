p8105\_hw6\_th2723
================
Tianhui He
2019/11/24

``` r
#basic setup
birthweight = read.csv("./data/birthweight.csv")
birthweight = birthweight %>% 
  mutate(babysex = as.factor(babysex),
         frace = as.factor(frace),
         mrace = as.factor(mrace),
         malform = as.factor(malform))
```

According to what I learned from biostatistic method class, "stepwise" modeling could help us to choose a best model automatically.

``` r
best_model = step(lm(bwt~., data = birthweight), direction = "backward")
```

    ## Start:  AIC=48717.83
    ## bwt ~ babysex + bhead + blength + delwt + fincome + frace + gaweeks + 
    ##     malform + menarche + mheight + momage + mrace + parity + 
    ##     pnumlbw + pnumsga + ppbmi + ppwt + smoken + wtgain
    ## 
    ## 
    ## Step:  AIC=48717.83
    ## bwt ~ babysex + bhead + blength + delwt + fincome + frace + gaweeks + 
    ##     malform + menarche + mheight + momage + mrace + parity + 
    ##     pnumlbw + pnumsga + ppbmi + ppwt + smoken
    ## 
    ## 
    ## Step:  AIC=48717.83
    ## bwt ~ babysex + bhead + blength + delwt + fincome + frace + gaweeks + 
    ##     malform + menarche + mheight + momage + mrace + parity + 
    ##     pnumlbw + ppbmi + ppwt + smoken
    ## 
    ## 
    ## Step:  AIC=48717.83
    ## bwt ~ babysex + bhead + blength + delwt + fincome + frace + gaweeks + 
    ##     malform + menarche + mheight + momage + mrace + parity + 
    ##     ppbmi + ppwt + smoken
    ## 
    ##            Df Sum of Sq       RSS   AIC
    ## - frace     4    124365 320848704 48712
    ## - malform   1      1419 320725757 48716
    ## - ppbmi     1      6346 320730684 48716
    ## - momage    1     28661 320752999 48716
    ## - mheight   1     66886 320791224 48717
    ## - menarche  1    111679 320836018 48717
    ## - ppwt      1    131132 320855470 48718
    ## <none>                  320724338 48718
    ## - fincome   1    193454 320917792 48718
    ## - parity    1    413584 321137922 48721
    ## - mrace     3    868321 321592659 48724
    ## - babysex   1    853796 321578134 48727
    ## - gaweeks   1   4611823 325336161 48778
    ## - smoken    1   5076393 325800732 48784
    ## - delwt     1   8008891 328733230 48823
    ## - blength   1 102050296 422774634 49915
    ## - bhead     1 106535716 427260054 49961
    ## 
    ## Step:  AIC=48711.51
    ## bwt ~ babysex + bhead + blength + delwt + fincome + gaweeks + 
    ##     malform + menarche + mheight + momage + mrace + parity + 
    ##     ppbmi + ppwt + smoken
    ## 
    ##            Df Sum of Sq       RSS   AIC
    ## - malform   1      1447 320850151 48710
    ## - ppbmi     1      6975 320855679 48710
    ## - momage    1     28379 320877083 48710
    ## - mheight   1     69502 320918206 48710
    ## - menarche  1    115708 320964411 48711
    ## - ppwt      1    133961 320982665 48711
    ## <none>                  320848704 48712
    ## - fincome   1    194405 321043108 48712
    ## - parity    1    414687 321263390 48715
    ## - babysex   1    852133 321700837 48721
    ## - gaweeks   1   4625208 325473911 48772
    ## - smoken    1   5036389 325885093 48777
    ## - delwt     1   8013099 328861802 48817
    ## - mrace     3  13540415 334389119 48885
    ## - blength   1 101995688 422844392 49908
    ## - bhead     1 106662962 427511666 49956
    ## 
    ## Step:  AIC=48709.53
    ## bwt ~ babysex + bhead + blength + delwt + fincome + gaweeks + 
    ##     menarche + mheight + momage + mrace + parity + ppbmi + ppwt + 
    ##     smoken
    ## 
    ##            Df Sum of Sq       RSS   AIC
    ## - ppbmi     1      6928 320857079 48708
    ## - momage    1     28660 320878811 48708
    ## - mheight   1     69320 320919470 48708
    ## - menarche  1    116027 320966177 48709
    ## - ppwt      1    133894 320984044 48709
    ## <none>                  320850151 48710
    ## - fincome   1    193784 321043934 48710
    ## - parity    1    414482 321264633 48713
    ## - babysex   1    851279 321701430 48719
    ## - gaweeks   1   4624003 325474154 48770
    ## - smoken    1   5035195 325885346 48775
    ## - delwt     1   8029079 328879230 48815
    ## - mrace     3  13553320 334403471 48883
    ## - blength   1 102009225 422859375 49906
    ## - bhead     1 106675331 427525481 49954
    ## 
    ## Step:  AIC=48707.63
    ## bwt ~ babysex + bhead + blength + delwt + fincome + gaweeks + 
    ##     menarche + mheight + momage + mrace + parity + ppwt + smoken
    ## 
    ##            Df Sum of Sq       RSS   AIC
    ## - momage    1     29211 320886290 48706
    ## - menarche  1    117635 320974714 48707
    ## <none>                  320857079 48708
    ## - fincome   1    195199 321052278 48708
    ## - parity    1    412984 321270064 48711
    ## - babysex   1    850020 321707099 48717
    ## - mheight   1   1078673 321935752 48720
    ## - ppwt      1   2934023 323791103 48745
    ## - gaweeks   1   4621504 325478583 48768
    ## - smoken    1   5039368 325896447 48773
    ## - delwt     1   8024939 328882018 48813
    ## - mrace     3  13551444 334408523 48881
    ## - blength   1 102018559 422875638 49904
    ## - bhead     1 106821342 427678421 49953
    ## 
    ## Step:  AIC=48706.02
    ## bwt ~ babysex + bhead + blength + delwt + fincome + gaweeks + 
    ##     menarche + mheight + mrace + parity + ppwt + smoken
    ## 
    ##            Df Sum of Sq       RSS   AIC
    ## - menarche  1    100121 320986412 48705
    ## <none>                  320886290 48706
    ## - fincome   1    240800 321127090 48707
    ## - parity    1    431433 321317724 48710
    ## - babysex   1    841278 321727568 48715
    ## - mheight   1   1076739 321963029 48719
    ## - ppwt      1   2913653 323799943 48743
    ## - gaweeks   1   4676469 325562760 48767
    ## - smoken    1   5045104 325931394 48772
    ## - delwt     1   8000672 328886962 48811
    ## - mrace     3  14667730 335554021 48894
    ## - blength   1 101990556 422876847 49902
    ## - bhead     1 106864308 427750598 49952
    ## 
    ## Step:  AIC=48705.38
    ## bwt ~ babysex + bhead + blength + delwt + fincome + gaweeks + 
    ##     mheight + mrace + parity + ppwt + smoken
    ## 
    ##           Df Sum of Sq       RSS   AIC
    ## <none>                 320986412 48705
    ## - fincome  1    245637 321232048 48707
    ## - parity   1    422770 321409181 48709
    ## - babysex  1    846134 321832545 48715
    ## - mheight  1   1012240 321998651 48717
    ## - ppwt     1   2907049 323893461 48743
    ## - gaweeks  1   4662501 325648912 48766
    ## - smoken   1   5073849 326060260 48771
    ## - delwt    1   8137459 329123871 48812
    ## - mrace    3  14683609 335670021 48894
    ## - blength  1 102191779 423178191 49903
    ## - bhead    1 106779754 427766166 49950

This stepwise function gives us the best model using babysex,bhead,blength, delwt,fincome, gaweeks, mheight, mrace, parity, ppwt and smoken as predictors. A brief glance of the 'best model' chosen by R using regression summary. The global p-value shows that it is significant under 0.05 significance level.

``` r
best_model %>% summary()
```

    ## 
    ## Call:
    ## lm(formula = bwt ~ babysex + bhead + blength + delwt + fincome + 
    ##     gaweeks + mheight + mrace + parity + ppwt + smoken, data = birthweight)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1097.18  -185.52    -3.39   174.14  2353.44 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -6098.8219   137.5463 -44.340  < 2e-16 ***
    ## babysex2       28.5580     8.4549   3.378 0.000737 ***
    ## bhead         130.7770     3.4466  37.944  < 2e-16 ***
    ## blength        74.9471     2.0190  37.120  < 2e-16 ***
    ## delwt           4.1067     0.3921  10.475  < 2e-16 ***
    ## fincome         0.3180     0.1747   1.820 0.068844 .  
    ## gaweeks        11.5925     1.4621   7.929 2.79e-15 ***
    ## mheight         6.5940     1.7849   3.694 0.000223 ***
    ## mrace2       -138.7925     9.9071 -14.009  < 2e-16 ***
    ## mrace3        -74.8868    42.3146  -1.770 0.076837 .  
    ## mrace4       -100.6781    19.3247  -5.210 1.98e-07 ***
    ## parity         96.3047    40.3362   2.388 0.017004 *  
    ## ppwt           -2.6756     0.4274  -6.261 4.20e-10 ***
    ## smoken         -4.8434     0.5856  -8.271  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 272.3 on 4328 degrees of freedom
    ## Multiple R-squared:  0.7181, Adjusted R-squared:  0.7173 
    ## F-statistic: 848.1 on 13 and 4328 DF,  p-value: < 2.2e-16

``` r
birthweight %>% 
  modelr::add_predictions(best_model) %>% 
  modelr::add_residuals(best_model) %>% 
  ggplot(aes(x = pred, y = resid)) + geom_point(alpha = 0.3) + theme_bw() + labs(x = "Fitted Value", y = 'Residuals', title = "Fitted value VS Residuals")
```

![](hw6_files/figure-markdown_github/unnamed-chunk-3-1.png)

``` r
set.seed(1)
model2 = lm(bwt ~ blength + gaweeks, data = birthweight)
model3 = lm(bwt ~ bhead + blength + babysex + bhead*blength + bhead*babysex + blength*babysex + bhead*blength*babysex, data = birthweight)

cv_df = 
  modelr::crossv_mc(birthweight, 100)
cv_df =
  cv_df %>% 
  mutate(
    train = map(train, as_tibble),
    test = map(test, as_tibble))

cv_df = 
  cv_df %>% 
  mutate(
  best_model= map(train, ~lm(formula = bwt ~ babysex + bhead + blength + delwt + fincome + gaweeks + mheight + mrace + parity + ppwt + smoken, data =.x)),
  model2  = map(train, ~lm(bwt ~ blength + gaweeks, data = .x)),
  model3  = map(train, ~lm(bwt ~ bhead + blength + babysex + bhead*blength + bhead*babysex + blength*babysex + bhead*blength*babysex, data = .x))) %>% 
  mutate(rmse_best_model = map2_dbl(best_model, test, ~ modelr::rmse(model = .x, data = .y)),
         rmse_model2 = map2_dbl(model2, test, ~ modelr::rmse(model = .x, data = .y)),
         rmse_model3 = map2_dbl(model3, test, ~ modelr::rmse(model = .x, data = .y)))
```

    ## Warning in predict.lm(model, data): prediction from a rank-deficient fit
    ## may be misleading

    ## Warning in predict.lm(model, data): prediction from a rank-deficient fit
    ## may be misleading

``` r
cv_df %>% 
  select(starts_with("rmse")) %>% 
  pivot_longer(
    everything(),
    names_to = "model", 
    values_to = "rmse",
    names_prefix = "rmse_") %>% 
  mutate(model = fct_inorder(model)) %>% 
  ggplot(aes(x = model, y = rmse)) + geom_violin()
```

![](hw6_files/figure-markdown_github/unnamed-chunk-4-1.png) After comparing the violin plot of rmse for the three models, my model using stepwise function has the lowest rmse among three models and thus has least predicted error. It is also reasonable because this model predictor package is automatically chosen by R.

``` r
#basic setup
weather_df = 
  rnoaa::meteo_pull_monitors(
    c("USW00094728"),
    var = c("PRCP", "TMIN", "TMAX"), 
    date_min = "2017-01-01",
    date_max = "2017-12-31") %>%
  mutate(
    name = recode(id, USW00094728 = "CentralPark_NY"),
    tmin = tmin / 10,
    tmax = tmax / 10) %>%
  select(name, id, everything())
```

    ## file path:          /Users/hetianhui/Library/Caches/rnoaa/ghcnd/USW00094728.dly

    ## file last updated:  2019-09-26 10:26:12

    ## file min/max dates: 1869-01-01 / 2019-09-30

``` r
lm(tmax ~ tmin, data = weather_df) %>% 
  broom::tidy() %>% 
  knitr::kable()
```

| term        |  estimate|  std.error|  statistic|  p.value|
|:------------|---------:|----------:|----------:|--------:|
| (Intercept) |  7.208502|  0.2263474|   31.84707|        0|
| tmin        |  1.039241|  0.0169919|   61.16096|        0|

``` r
x = weather_df
boot_sample = function(x) {
  sample_frac(x, replace = TRUE)
}#generate boot sample

bootstraps_data = 
  list(
    strap_number = 1:5000,
    strap_sample = rerun(5000, boot_sample(x))
  ) #Drawing 5000 bootstrap samples

weather_bootstrap = weather_df %>% 
  modelr::bootstrap(n = 5000) 

weather1 = weather_bootstrap %>% 
  mutate(
    models = map(strap, ~lm(tmax ~ tmin, data =.x)),
    results = map(models, broom::glance)) %>% 
    select(-strap, -models) %>% 
    unnest(results) %>% 
    janitor::clean_names() 
```

``` r
weather1 %>% 
  ggplot(aes(x = r_squared)) + geom_density(alpha = 0.5) + labs(title = "Distribution of R-square")
```

![](hw6_files/figure-markdown_github/unnamed-chunk-7-1.png) According to the above graph, r-squared for each strap is nearly normally distributed (bell-shape). However, it is a little left-skewed, implying that there are some extremely high r-squared score that drive the entire distribution to the right.

``` r
log_info = 
weather_bootstrap%>% 
  mutate(
    models = map(strap, ~lm(tmax ~ tmin, data =.x)),
    results = map(models, broom::tidy)) %>% 
    select(-strap, -models) %>% 
    unnest(results) %>% 
   janitor::clean_names() %>% 
   select(id, term, estimate) %>% 
   # spread(key = term, value = estimate)
  pivot_wider(
    names_from = "term",
    values_from = "estimate"
  )  %>%
  rename(intercept= '(Intercept)') %>% 
  mutate(log_calc = log(intercept*tmin)) 
```

``` r
log_info %>% 
  ggplot(aes(x = log_calc)) + geom_density(alpha = 0.5) + labs(title = "Distribution of log(beta0_hat * beta1_hat)")
```

![](hw6_files/figure-markdown_github/unnamed-chunk-9-1.png) Compared to the distribution of r-squared, we can see that the log(beta0\_hat \* beta1\_hat) estimates are more normally distributed.

95% confidence interval:

``` r
#for r-square
weather1 %>% 
  pull(r_squared) %>% 
  quantile(., probs = c(0.025, 0.975, 0.95), na.rm = TRUE) %>% 
  knitr::kable()
```

|       |          x|
|-------|----------:|
| 2.5%  |  0.8941667|
| 97.5% |  0.9270552|
| 95%   |  0.9249372|

``` r
#for log(beta0_hat * beta1_hat)
log_info %>% 
  pull(log_calc) %>% 
  quantile(., probs = c(0.025, 0.975, 0.95), na.rm = TRUE) %>% 
  knitr::kable()
```

|       |         x|
|-------|---------:|
| 2.5%  |  1.963307|
| 97.5% |  2.058308|
| 95%   |  2.051909|
