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

Since there does not seem to be colinearity, we procede. In order to
find the best model, I start with the full model and use the `step`
function to pick the predictors.

``` r
full_formula <- Survived ~ .

lm1 <- full_formula %>%
  glm(
    'binomial',
    train
  ) %>%
  step(
    trace = 0,
    k = train %>% nrow %>% log
  )

lm1 %>%
  summary
```

    ## 
    ## Call:
    ## glm(formula = Survived ~ Pclass + Age + SibSp + Parch, family = "binomial", 
    ##     data = train)
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
|           1 |        0 |      3 | Braund, Mr.??Owen Harris                             | male   |  22 |     1 |     0 | A/5 21171        |  7.2500 |       | S        |
|           2 |        1 |      1 | Cumings, Mrs.??John Bradley (Florence Briggs Thayer) | female |  38 |     1 |     0 | PC 17599         | 71.2833 | C85   | C        |
|           3 |        1 |      3 | Heikkinen, Miss. Laina                              | female |  26 |     0 |     0 | STON/O2. 3101282 |  7.9250 |       | S        |
|           4 |        1 |      1 | Futrelle, Mrs.??Jacques Heath (Lily May Peel)        | female |  35 |     1 |     0 | 113803           | 53.1000 | C123  | S        |
|           5 |        0 |      3 | Allen, Mr.??William Henry                            | male   |  35 |     0 |     0 | 373450           |  8.0500 |       | S        |
|           6 |        0 |      3 | Moran, Mr.??James                                    | male   |  NA |     0 |     0 | 330877           |  8.4583 |       | Q        |

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

Here we can see that ??? for the most part ??? the non blank values for
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

train2 %>%
  head %>%
  kable
```

|     | PassengerId | Survived | Pclass | Sex    | Age | SibSp | Parch |    Fare | Embarked |
|:----|------------:|---------:|-------:|:-------|----:|------:|------:|--------:|:---------|
| 1   |           1 |        0 |      3 | male   |  22 |     1 |     0 |  7.2500 | S        |
| 2   |           2 |        1 |      1 | female |  38 |     1 |     0 | 71.2833 | C        |
| 3   |           3 |        1 |      3 | female |  26 |     0 |     0 |  7.9250 | S        |
| 4   |           4 |        1 |      1 | female |  35 |     1 |     0 | 53.1000 | S        |
| 5   |           5 |        0 |      3 | male   |  35 |     0 |     0 |  8.0500 | S        |
| 7   |           7 |        0 |      1 | male   |  54 |     0 |     0 | 51.8625 | S        |

``` r
lm2 <- full_formula %>%
  glm(
    'binomial',
    train2
  ) %>%
  step(
    trace = 0,
    k = train %>% nrow %>% log
  )
  
lm2 %>%
  summary
```

    ## 
    ## Call:
    ## glm(formula = Survived ~ Pclass + Sex + Age + SibSp, family = "binomial", 
    ##     data = train2)
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

# Dealing with `Age`

I decided to look at what the training data is like for the significant
variables.

``` r
titanic_test[c('Pclass', 'Sex', 'Age', 'SibSp')] %>%
  summary
```

    ##      Pclass          Sex                 Age            SibSp       
    ##  Min.   :1.000   Length:418         Min.   : 0.17   Min.   :0.0000  
    ##  1st Qu.:1.000   Class :character   1st Qu.:21.00   1st Qu.:0.0000  
    ##  Median :3.000   Mode  :character   Median :27.00   Median :0.0000  
    ##  Mean   :2.266                      Mean   :30.27   Mean   :0.4474  
    ##  3rd Qu.:3.000                      3rd Qu.:39.00   3rd Qu.:1.0000  
    ##  Max.   :3.000                      Max.   :76.00   Max.   :8.0000  
    ##                                     NA's   :86

Here I noticed that `Age` has a lot of null values. In order to fix
this, I decided to use a linear model to estimate age.

``` r
lm_age <- (Age ~ . - Survived) %>%
  lm(train2) %>%
  step(
    trace = 0,
    k = train2 %>% nrow %>% log
  )

lm_age %>%
  summary
```

    ## 
    ## Call:
    ## lm(formula = Age ~ Pclass + Sex + SibSp, data = train2)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -37.150  -8.081  -1.039   6.751  45.678 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  43.8659     1.4483  30.287  < 2e-16 ***
    ## Pclass       -6.4136     0.5792 -11.072  < 2e-16 ***
    ## Sexmale       3.6973     1.0107   3.658 0.000273 ***
    ## SibSp        -4.2279     0.5187  -8.151 1.63e-15 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 12.76 on 710 degrees of freedom
    ## Multiple R-squared:  0.2315, Adjusted R-squared:  0.2283 
    ## F-statistic: 71.29 on 3 and 710 DF,  p-value: < 2.2e-16

``` r
lm_age %>%
  plot
```

![](titanic_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->![](titanic_files/figure-gfm/unnamed-chunk-13-2.png)<!-- -->![](titanic_files/figure-gfm/unnamed-chunk-13-3.png)<!-- -->![](titanic_files/figure-gfm/unnamed-chunk-13-4.png)<!-- -->

It seems like the variability seems to increase with the fitted values.
The residuals also seem to be normally distributed. This indicates that
the constant variability assumption is not met for linear regression.

So not all the assumptions for linear regression are met and the model
does not do a great job of explaining variability. I???m probably still
going to use this anyway.

In order to test this out, I added this estimate to the training data

``` r
train3 <- titanic_train

ind <- train3 %$%
  Age %>%
  is.na

train_pred_age <- lm_age %>%
  predict(train3[ind, ])

# change negative age to min age
train_pred_age[train_pred_age < 0.17] <- 0.17

train3$Age[ind] <- train_pred_age

train3 %$%
  Age %>%
  summary
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##    0.17   21.00   28.32   29.30   36.00   80.00

This seemed to get rid of the null values.

I now tested the accuracy of the last logistical model with this info.

``` r
estimates <- lm2 %>%
  predict(
    train3,
    type = 'response'
  ) %>%
  round

100 * (estimates == train3$Survived) %>%
  mean
```

    ## [1] 81.59371

With these null values filled out, I was able to corretly guess
81.59371% of the results.

# Test Data

Now I will fill out the null values with the estimates.

``` r
test <- titanic_test

ind <- test %$%
  Age %>%
  is.na

test_pred_age <- lm_age %>%
  predict(test[ind, ])

# change negative age to min age
test_pred_age[test_pred_age < 0.17] <- 0.17

test$Age[ind] <- test_pred_age

test %$%
  Age %>%
  summary
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##    0.17   22.00   28.32   29.75   36.38   76.00

Now I estimate the predictions and output to a csv file.

``` r
test_survived <- lm2 %>%
  predict(
    test,
    type = 'response'
  ) %>%
  round

data.frame(
  PassengerId = test$PassengerId,
  Survived = test_survived
) %>%
  write.csv(
    'answer.csv',
    row.names = F
  )
```

This resulted in the score 0.75358.
