Boston data test
================

# Import

``` r
library(MASS) # 資料來源
library(car) # VIF函數來源
```

    ## Loading required package: carData

``` r
#mydata <- read.table(, header = TRUE, na.strings = "?")
#mydata <- read.csv(, header = TRUE, na.strings = "?")
```

# Tidy

``` r
my_Boston <- na.omit(Boston) # 移除含有空值的資料
```

## Begin Understand

# Transform

``` r
#summary(Boston)
```

crim : per capita crime rate by town.  
zn : proportion of residential land zoned for lots over 25,000 sq.ft.  
indus : proportion of non-retail business acres per town  
chas : Charles River dummy variable (= 1 if tract bounds river; 0
otherwise).  
nox : nitrogen oxides concentration (parts per 10 million).  
rm : average number of rooms per dwelling.  
age : proportion of owner-occupied units built prior to 1940.  
dis : weighted mean of distances to five Boston employment centres.  
rad : index of accessibility to radial highways.  
tax : full-value property-tax rate per $10,000.  
ptratio : pupil-teacher ratio by town.  
black : 1000(Bk - 0.63)^2 where Bk is the proportion of blacks by
town.  
lstat : lower status of the population (percent).  
**medv : median value of owner-occupied homes in $1000s.**

``` r
#plot(rm, lstat)
#hist(lstat, col=2, breaks = 15)
#pairs(my_Boston)
#pairs(~ + rm + crim + lstat)
#cor(my_Boston)
```

# Visualize

## interactive terms?

## polynomial terms?

Adjust this on preditors to make plot (residuals plot) a horizontal red
line.  
Adjust this on response to make residuals plot a uniform shape.

## outliers? High leverage point?

Residual plot tells which row

## colinear?

check if vif \> 5

# Model

``` r
attach(my_Boston)
lm.fit = lm(medv ~ ., data = my_Boston)
summary(lm.fit)
```

    ## 
    ## Call:
    ## lm(formula = medv ~ ., data = my_Boston)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -15.595  -2.730  -0.518   1.777  26.199 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  3.646e+01  5.103e+00   7.144 3.28e-12 ***
    ## crim        -1.080e-01  3.286e-02  -3.287 0.001087 ** 
    ## zn           4.642e-02  1.373e-02   3.382 0.000778 ***
    ## indus        2.056e-02  6.150e-02   0.334 0.738288    
    ## chas         2.687e+00  8.616e-01   3.118 0.001925 ** 
    ## nox         -1.777e+01  3.820e+00  -4.651 4.25e-06 ***
    ## rm           3.810e+00  4.179e-01   9.116  < 2e-16 ***
    ## age          6.922e-04  1.321e-02   0.052 0.958229    
    ## dis         -1.476e+00  1.995e-01  -7.398 6.01e-13 ***
    ## rad          3.060e-01  6.635e-02   4.613 5.07e-06 ***
    ## tax         -1.233e-02  3.760e-03  -3.280 0.001112 ** 
    ## ptratio     -9.527e-01  1.308e-01  -7.283 1.31e-12 ***
    ## black        9.312e-03  2.686e-03   3.467 0.000573 ***
    ## lstat       -5.248e-01  5.072e-02 -10.347  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 4.745 on 492 degrees of freedom
    ## Multiple R-squared:  0.7406, Adjusted R-squared:  0.7338 
    ## F-statistic: 108.1 on 13 and 492 DF,  p-value: < 2.2e-16

``` r
vif(lm.fit)
```

    ##     crim       zn    indus     chas      nox       rm      age      dis 
    ## 1.792192 2.298758 3.991596 1.073995 4.393720 1.933744 3.100826 3.955945 
    ##      rad      tax  ptratio    black    lstat 
    ## 7.484496 9.008554 1.799084 1.348521 2.941491

``` r
lm.fit = lm(medv ~ . -age -indus - tax, data = my_Boston)
summary(lm.fit)
```

    ## 
    ## Call:
    ## lm(formula = medv ~ . - age - indus - tax, data = my_Boston)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -16.2609  -2.9888  -0.5083   1.8041  26.2482 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  34.712342   5.102742   6.803 2.97e-11 ***
    ## crim         -0.104843   0.033132  -3.164 0.001650 ** 
    ## zn            0.036634   0.013412   2.731 0.006532 ** 
    ## chas          2.967868   0.860830   3.448 0.000614 ***
    ## nox         -20.314416   3.472292  -5.850 8.92e-09 ***
    ## rm            3.977104   0.407731   9.754  < 2e-16 ***
    ## dis          -1.429370   0.186922  -7.647 1.08e-13 ***
    ## rad           0.128761   0.040788   3.157 0.001692 ** 
    ## ptratio      -1.014914   0.129006  -7.867 2.30e-14 ***
    ## black         0.009700   0.002701   3.591 0.000363 ***
    ## lstat        -0.528147   0.047930 -11.019  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 4.79 on 495 degrees of freedom
    ## Multiple R-squared:  0.7342, Adjusted R-squared:  0.7288 
    ## F-statistic: 136.7 on 10 and 495 DF,  p-value: < 2.2e-16

``` r
plot(lm.fit)
```

![](Tester_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->![](Tester_files/figure-gfm/unnamed-chunk-5-2.png)<!-- -->![](Tester_files/figure-gfm/unnamed-chunk-5-3.png)<!-- -->![](Tester_files/figure-gfm/unnamed-chunk-5-4.png)<!-- -->

``` r
lm.fit = lm(medv ~ . - tax - indus - age , data = my_Boston[-c(369, 372, 373), ])
summary(lm.fit)
```

    ## 
    ## Call:
    ## lm(formula = medv ~ . - tax - indus - age, data = my_Boston[-c(369, 
    ##     372, 373), ])
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -15.6641  -2.8410  -0.3751   1.8066  19.2247 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  28.640030   4.680205   6.119 1.92e-09 ***
    ## crim         -0.095376   0.030100  -3.169  0.00163 ** 
    ## zn            0.030425   0.012193   2.495  0.01291 *  
    ## chas          2.405876   0.792192   3.037  0.00252 ** 
    ## nox         -19.218002   3.154500  -6.092 2.25e-09 ***
    ## rm            4.668916   0.377795  12.358  < 2e-16 ***
    ## dis          -1.229979   0.170831  -7.200 2.27e-12 ***
    ## rad           0.073739   0.037418   1.971  0.04932 *  
    ## ptratio      -1.025144   0.117171  -8.749  < 2e-16 ***
    ## black         0.009106   0.002454   3.711  0.00023 ***
    ## lstat        -0.433398   0.044591  -9.719  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 4.349 on 492 degrees of freedom
    ## Multiple R-squared:  0.7699, Adjusted R-squared:  0.7653 
    ## F-statistic: 164.6 on 10 and 492 DF,  p-value: < 2.2e-16

``` r
plot(lm.fit)
```

![](Tester_files/figure-gfm/unnamed-chunk-5-5.png)<!-- -->![](Tester_files/figure-gfm/unnamed-chunk-5-6.png)<!-- -->![](Tester_files/figure-gfm/unnamed-chunk-5-7.png)<!-- -->![](Tester_files/figure-gfm/unnamed-chunk-5-8.png)<!-- -->

``` r
lm.fit3 = lm(medv ~ . - tax - indus - age + I(rm^2), data = my_Boston[-c(372, 373, 369), ])
summary(lm.fit3)
```

    ## 
    ## Call:
    ## lm(formula = medv ~ . - tax - indus - age + I(rm^2), data = my_Boston[-c(372, 
    ##     373, 369), ])
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -28.1633  -2.1124  -0.1689   1.8064  21.1150 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 129.671088   8.111429  15.986  < 2e-16 ***
    ## crim         -0.119797   0.025401  -4.716 3.14e-06 ***
    ## zn            0.019304   0.010295   1.875 0.061376 .  
    ## chas          2.031042   0.667502   3.043 0.002470 ** 
    ## nox         -18.061706   2.657161  -6.797 3.10e-11 ***
    ## rm          -28.494774   2.348941 -12.131  < 2e-16 ***
    ## dis          -0.926868   0.145395  -6.375 4.23e-10 ***
    ## rad           0.048122   0.031555   1.525 0.127905    
    ## ptratio      -0.804577   0.099859  -8.057 5.98e-15 ***
    ## black         0.006917   0.002072   3.339 0.000904 ***
    ## lstat        -0.458099   0.037583 -12.189  < 2e-16 ***
    ## I(rm^2)       2.578384   0.180941  14.250  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 3.661 on 491 degrees of freedom
    ## Multiple R-squared:  0.8372, Adjusted R-squared:  0.8336 
    ## F-statistic: 229.6 on 11 and 491 DF,  p-value: < 2.2e-16

``` r
plot(lm.fit3)
```

![](Tester_files/figure-gfm/unnamed-chunk-5-9.png)<!-- -->![](Tester_files/figure-gfm/unnamed-chunk-5-10.png)<!-- -->![](Tester_files/figure-gfm/unnamed-chunk-5-11.png)<!-- -->![](Tester_files/figure-gfm/unnamed-chunk-5-12.png)<!-- -->

``` r
anova(lm.fit, lm.fit3)
```

    ## Analysis of Variance Table
    ## 
    ## Model 1: medv ~ (crim + zn + indus + chas + nox + rm + age + dis + rad + 
    ##     tax + ptratio + black + lstat) - tax - indus - age
    ## Model 2: medv ~ (crim + zn + indus + chas + nox + rm + age + dis + rad + 
    ##     tax + ptratio + black + lstat) - tax - indus - age + I(rm^2)
    ##   Res.Df    RSS Df Sum of Sq      F    Pr(>F)    
    ## 1    492 9303.9                                  
    ## 2    491 6581.9  1      2722 203.06 < 2.2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

## End Understand

# Communicate

``` r
lm.fit4 = lm(medv ~ . - tax - black - zn - rad - chas - indus - age + I(rm^2) + rm:lstat, data = my_Boston[-c(365, 370, 371, 372, 373, 369), ])
summary(lm.fit4)
```

    ## 
    ## Call:
    ## lm(formula = medv ~ . - tax - black - zn - rad - chas - indus - 
    ##     age + I(rm^2) + rm:lstat, data = my_Boston[-c(365, 370, 371, 
    ##     372, 373, 369), ])
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -9.5582 -1.9372 -0.2571  1.9227 11.8638 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  86.75059   10.77013   8.055 6.08e-15 ***
    ## crim         -0.13820    0.01880  -7.352 8.23e-13 ***
    ## nox         -12.89127    2.08698  -6.177 1.37e-09 ***
    ## rm          -18.61758    2.99217  -6.222 1.05e-09 ***
    ## dis          -0.67371    0.10572  -6.373 4.28e-10 ***
    ## ptratio      -0.67652    0.07478  -9.047  < 2e-16 ***
    ## lstat         0.92054    0.19315   4.766 2.48e-06 ***
    ## I(rm^2)       2.03884    0.21352   9.549  < 2e-16 ***
    ## rm:lstat     -0.23420    0.03297  -7.103 4.33e-12 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 3.076 on 491 degrees of freedom
    ## Multiple R-squared:  0.8806, Adjusted R-squared:  0.8786 
    ## F-statistic: 452.6 on 8 and 491 DF,  p-value: < 2.2e-16

``` r
plot(lm.fit4)
```

![](Tester_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->![](Tester_files/figure-gfm/unnamed-chunk-6-2.png)<!-- -->![](Tester_files/figure-gfm/unnamed-chunk-6-3.png)<!-- -->![](Tester_files/figure-gfm/unnamed-chunk-6-4.png)<!-- -->

``` r
# wierd data
my_Boston[c(365, 370, 371, 372, 373, 369), ]
```

    ##        crim zn indus chas   nox    rm   age    dis rad tax ptratio  black lstat
    ## 365 3.47428  0  18.1    1 0.718 8.780  82.9 1.9047  24 666    20.2 354.55  5.29
    ## 370 5.66998  0  18.1    1 0.631 6.683  96.8 1.3567  24 666    20.2 375.33  3.73
    ## 371 6.53876  0  18.1    1 0.631 7.016  97.5 1.2024  24 666    20.2 392.05  2.96
    ## 372 9.23230  0  18.1    0 0.631 6.216 100.0 1.1691  24 666    20.2 366.15  9.53
    ## 373 8.26725  0  18.1    1 0.668 5.875  89.6 1.1296  24 666    20.2 347.88  8.88
    ## 369 4.89822  0  18.1    0 0.631 4.970 100.0 1.3325  24 666    20.2 375.52  3.26
    ##     medv
    ## 365 21.9
    ## 370 50.0
    ## 371 50.0
    ## 372 50.0
    ## 373 50.0
    ## 369 50.0
