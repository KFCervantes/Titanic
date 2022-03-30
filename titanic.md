Titanic
================
Kaleb Cervantes
3/29/2022

Loading Libraries.

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(ggplot2)
library(knitr)
library(magrittr)
library(stringr)
library(titanic)
```

# Only Numeric

This part is to just learn some basic logistical reggression in R. In
order to do this, I decided to subset the data to only include numeric
columns and rows containing non null values.

``` r
train <- titanic_train %>%
  select_if(is.numeric) %>%
  subset(!(Age %>% is.na))

train %>%
  head %>%
  kable
```

|     | PassengerId | Survived | Pclass | Age | SibSp | Parch |    Fare |
|:----|------------:|---------:|-------:|----:|------:|------:|--------:|
| 1   |           1 |        0 |      3 |  22 |     1 |     0 |  7.2500 |
| 2   |           2 |        1 |      1 |  38 |     1 |     0 | 71.2833 |
| 3   |           3 |        1 |      3 |  26 |     0 |     0 |  7.9250 |
| 4   |           4 |        1 |      1 |  35 |     1 |     0 | 53.1000 |
| 5   |           5 |        0 |      3 |  35 |     0 |     0 |  8.0500 |
| 7   |           7 |        0 |      1 |  54 |     0 |     0 | 51.8625 |

In order to check colinearity, I decided to use a correlation matrix.

``` r
train %>%
  cor %>%
  kable
```

|             | PassengerId |   Survived |     Pclass |        Age |      SibSp |      Parch |       Fare |
|:------------|------------:|-----------:|-----------:|-----------:|-----------:|-----------:|-----------:|
| PassengerId |   1.0000000 |  0.0293402 | -0.0353491 |  0.0368472 | -0.0823977 | -0.0116174 |  0.0095918 |
| Survived    |   0.0293402 |  1.0000000 | -0.3596527 | -0.0772211 | -0.0173584 |  0.0933170 |  0.2681886 |
| Pclass      |  -0.0353491 | -0.3596527 |  1.0000000 | -0.3692260 |  0.0672474 |  0.0256831 | -0.5541825 |
| Age         |   0.0368472 | -0.0772211 | -0.3692260 |  1.0000000 | -0.3082468 | -0.1891193 |  0.0960667 |
| SibSp       |  -0.0823977 | -0.0173584 |  0.0672474 | -0.3082468 |  1.0000000 |  0.3838199 |  0.1383288 |
| Parch       |  -0.0116174 |  0.0933170 |  0.0256831 | -0.1891193 |  0.3838199 |  1.0000000 |  0.2051189 |
| Fare        |   0.0095918 |  0.2681886 | -0.5541825 |  0.0960667 |  0.1383288 |  0.2051189 |  1.0000000 |

Since there does not seem to be colinearity, we procede.

``` r
full_formula <- Survived ~ .

full_formula %>%
  glm(
    'binomial',
    train
  ) %>%
  summary
```

    ## 
    ## Call:
    ## glm(formula = ., family = "binomial", data = train)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.4061  -0.8476  -0.6208   0.9968   2.4030  
    ## 
    ## Coefficients:
    ##               Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)  3.3283957  0.5278971   6.305 2.88e-10 ***
    ## PassengerId  0.0001545  0.0003283   0.471  0.63781    
    ## Pclass      -1.1517899  0.1460712  -7.885 3.14e-15 ***
    ## Age         -0.0445935  0.0072119  -6.183 6.28e-10 ***
    ## SibSp       -0.2890863  0.1063055  -2.719  0.00654 ** 
    ## Parch        0.2461954  0.1091847   2.255  0.02414 *  
    ## Fare         0.0033048  0.0025419   1.300  0.19355    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 964.52  on 713  degrees of freedom
    ## Residual deviance: 814.96  on 707  degrees of freedom
    ## AIC: 828.96
    ## 
    ## Number of Fisher Scoring iterations: 4

Here we see that the variables `PassengerId` and `Fare` do not seem
significant, so for now I will drop them.

``` r
lm1 <- (Survived ~ . - PassengerId - Fare) %>%
  glm(
    'binomial',
    train
  )

lm1 %>%
  summary
```

    ## 
    ## Call:
    ## glm(formula = ., family = "binomial", data = train)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.3451  -0.8369  -0.6115   0.9829   2.4273  
    ## 
    ## Coefficients:
    ##              Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)  3.752019   0.437247   8.581  < 2e-16 ***
    ## Pclass      -1.266270   0.120874 -10.476  < 2e-16 ***
    ## Age         -0.045159   0.007192  -6.279 3.41e-10 ***
    ## SibSp       -0.273289   0.104373  -2.618  0.00883 ** 
    ## Parch        0.280807   0.105632   2.658  0.00785 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 964.52  on 713  degrees of freedom
    ## Residual deviance: 817.17  on 709  degrees of freedom
    ## AIC: 827.17
    ## 
    ## Number of Fisher Scoring iterations: 4

Now I only really understand the coefficents part of this. I will round
the fitted values and compare them to the original to see what
percentage I got correct.

``` r
100 * ((lm1 %$% fitted.values %>% round) == (train %$% Survived)) %>%
  mean
```

    ## [1] 69.60784

This method seems to have correctly guessed 69.60784% of the dataset.

# With Factors

``` r
titanic_train %>%
  head %>%
  kable
```

| PassengerId | Survived | Pclass | Name                                                | Sex    | Age | SibSp | Parch | Ticket           |    Fare | Cabin | Embarked |
|------------:|---------:|-------:|:----------------------------------------------------|:-------|----:|------:|------:|:-----------------|--------:|:------|:---------|
|           1 |        0 |      3 | Braund, Mr. Owen Harris                             | male   |  22 |     1 |     0 | A/5 21171        |  7.2500 |       | S        |
|           2 |        1 |      1 | Cumings, Mrs. John Bradley (Florence Briggs Thayer) | female |  38 |     1 |     0 | PC 17599         | 71.2833 | C85   | C        |
|           3 |        1 |      3 | Heikkinen, Miss. Laina                              | female |  26 |     0 |     0 | STON/O2. 3101282 |  7.9250 |       | S        |
|           4 |        1 |      1 | Futrelle, Mrs. Jacques Heath (Lily May Peel)        | female |  35 |     1 |     0 | 113803           | 53.1000 | C123  | S        |
|           5 |        0 |      3 | Allen, Mr. William Henry                            | male   |  35 |     0 |     0 | 373450           |  8.0500 |       | S        |
|           6 |        0 |      3 | Moran, Mr. James                                    | male   |  NA |     0 |     0 | 330877           |  8.4583 |       | Q        |

It is important to note that many of the values for `Cabin` are blank.
These are technically not null values, but they probably should be
treated as such. Assuming that `Name` does not contain duplicate values,
it may help to drop that column as well.

Before creating the training dataframe, we may have to check the
frequency of the values in `Cabin` and `Ticket`.

``` r
titanic_train %>%
  subset(Cabin != '') %>%
  count(Cabin) %$%
  n %>%
  summary
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   1.000   1.000   1.000   1.388   2.000   4.000

Here we can see that – for the most part – the non blank values for
`Cabin` only appear once. It may be helpful to drop this variable.

``` r
titanic_train %>%
  count(Ticket) %$%
  n %>%
  summary
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   1.000   1.000   1.000   1.308   1.000   7.000

We see the same thing happen with `Ticket`, so we also drop this
variable.

We now use this with the logistical model again.

We now create are training data.

``` r
train2 <- titanic_train %>%
  subset(
    !(Age %>% is.na),
    -c(Cabin, Name, Ticket)
  )

train %>%
  head %>%
  kable
```

|     | PassengerId | Survived | Pclass | Age | SibSp | Parch |    Fare |
|:----|------------:|---------:|-------:|----:|------:|------:|--------:|
| 1   |           1 |        0 |      3 |  22 |     1 |     0 |  7.2500 |
| 2   |           2 |        1 |      1 |  38 |     1 |     0 | 71.2833 |
| 3   |           3 |        1 |      3 |  26 |     0 |     0 |  7.9250 |
| 4   |           4 |        1 |      1 |  35 |     1 |     0 | 53.1000 |
| 5   |           5 |        0 |      3 |  35 |     0 |     0 |  8.0500 |
| 7   |           7 |        0 |      1 |  54 |     0 |     0 | 51.8625 |

``` r
full_formula %>%
  glm(
    'binomial',
    train
  ) %>%
  summary
```

    ## 
    ## Call:
    ## glm(formula = ., family = "binomial", data = train)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.4061  -0.8476  -0.6208   0.9968   2.4030  
    ## 
    ## Coefficients:
    ##               Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)  3.3283957  0.5278971   6.305 2.88e-10 ***
    ## PassengerId  0.0001545  0.0003283   0.471  0.63781    
    ## Pclass      -1.1517899  0.1460712  -7.885 3.14e-15 ***
    ## Age         -0.0445935  0.0072119  -6.183 6.28e-10 ***
    ## SibSp       -0.2890863  0.1063055  -2.719  0.00654 ** 
    ## Parch        0.2461954  0.1091847   2.255  0.02414 *  
    ## Fare         0.0033048  0.0025419   1.300  0.19355    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 964.52  on 713  degrees of freedom
    ## Residual deviance: 814.96  on 707  degrees of freedom
    ## AIC: 828.96
    ## 
    ## Number of Fisher Scoring iterations: 4

In this case, the only significant variables seems to be `Pclass`,
`Sex`, `Age`, and `SibSp`. As such I decided to make a new model with
only these.

``` r
lm2 <- (Survived ~ Pclass + Sex + Age + SibSp) %>%
  glm(
    'binomial',
    train2
  )

lm2 %>%
  summary
```

    ## 
    ## Call:
    ## glm(formula = ., family = "binomial", data = train2)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.7714  -0.6445  -0.3836   0.6276   2.4585  
    ## 
    ## Coefficients:
    ##              Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)  5.600846   0.543441  10.306  < 2e-16 ***
    ## Pclass      -1.317398   0.140900  -9.350  < 2e-16 ***
    ## Sexmale     -2.623483   0.214524 -12.229  < 2e-16 ***
    ## Age         -0.044385   0.008155  -5.442 5.26e-08 ***
    ## SibSp       -0.376119   0.121080  -3.106  0.00189 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 964.52  on 713  degrees of freedom
    ## Residual deviance: 636.72  on 709  degrees of freedom
    ## AIC: 646.72
    ## 
    ## Number of Fisher Scoring iterations: 5

This model seems to do well. In order to check the accuracy, I decided
to use the same method as before.

``` r
100 * ((lm2 %$% fitted.values %>% round) == (train2 %$% Survived)) %>%
  mean
```

    ## [1] 80.81232

This method seems to have correctly predicted 80.81232% of the dataset.
