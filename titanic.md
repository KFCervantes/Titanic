Titanic
================
Kaleb Cervantes
3/29/2022

Loading Libraries.

``` r
library(knitr)
library(magrittr)
library(titanic)
```

First, we try to find what the data will look like.

``` r
<<<<<<< HEAD
library(ggplot2)
library(knitr)
library(magrittr)
library(stringr)
library(titanic)
```

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
(Survived ~ .) %>%
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
lm <- (Survived ~ . - PassengerId - Fare) %>%
  glm(
    'binomial',
    train
  )

lm %>%
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
100 * ((lm %$% fitted.values %>% round) == (train %$% Survived)) %>%
  mean
```

    ## [1] 69.60784

This method seems to have correctly guessed 69.60784 of the dataset.
=======
titanic_train %>%
  head
```

    ##   PassengerId Survived Pclass
    ## 1           1        0      3
    ## 2           2        1      1
    ## 3           3        1      3
    ## 4           4        1      1
    ## 5           5        0      3
    ## 6           6        0      3
    ##                                                  Name    Sex Age SibSp Parch
    ## 1                             Braund, Mr. Owen Harris   male  22     1     0
    ## 2 Cumings, Mrs. John Bradley (Florence Briggs Thayer) female  38     1     0
    ## 3                              Heikkinen, Miss. Laina female  26     0     0
    ## 4        Futrelle, Mrs. Jacques Heath (Lily May Peel) female  35     1     0
    ## 5                            Allen, Mr. William Henry   male  35     0     0
    ## 6                                    Moran, Mr. James   male  NA     0     0
    ##             Ticket    Fare Cabin Embarked
    ## 1        A/5 21171  7.2500              S
    ## 2         PC 17599 71.2833   C85        C
    ## 3 STON/O2. 3101282  7.9250              S
    ## 4           113803 53.1000  C123        S
    ## 5           373450  8.0500              S
    ## 6           330877  8.4583              Q

We notice that many of the values for `Cabin` seem to be blank. It may
also help to see where the null values are.

``` r
titanic_train %>%
  summary
```

    ##   PassengerId       Survived          Pclass          Name          
    ##  Min.   :  1.0   Min.   :0.0000   Min.   :1.000   Length:891        
    ##  1st Qu.:223.5   1st Qu.:0.0000   1st Qu.:2.000   Class :character  
    ##  Median :446.0   Median :0.0000   Median :3.000   Mode  :character  
    ##  Mean   :446.0   Mean   :0.3838   Mean   :2.309                     
    ##  3rd Qu.:668.5   3rd Qu.:1.0000   3rd Qu.:3.000                     
    ##  Max.   :891.0   Max.   :1.0000   Max.   :3.000                     
    ##                                                                     
    ##      Sex                 Age            SibSp           Parch       
    ##  Length:891         Min.   : 0.42   Min.   :0.000   Min.   :0.0000  
    ##  Class :character   1st Qu.:20.12   1st Qu.:0.000   1st Qu.:0.0000  
    ##  Mode  :character   Median :28.00   Median :0.000   Median :0.0000  
    ##                     Mean   :29.70   Mean   :0.523   Mean   :0.3816  
    ##                     3rd Qu.:38.00   3rd Qu.:1.000   3rd Qu.:0.0000  
    ##                     Max.   :80.00   Max.   :8.000   Max.   :6.0000  
    ##                     NA's   :177                                     
    ##     Ticket               Fare           Cabin             Embarked        
    ##  Length:891         Min.   :  0.00   Length:891         Length:891        
    ##  Class :character   1st Qu.:  7.91   Class :character   Class :character  
    ##  Mode  :character   Median : 14.45   Mode  :character   Mode  :character  
    ##                     Mean   : 32.20                                        
    ##                     3rd Qu.: 31.00                                        
    ##                     Max.   :512.33                                        
    ## 

Here we can see that the only column with null values is `Age`. This
also indicates that the empty entries in `Cabin` contain empty strings.
In order to fit the initial model, we will take the subset without null
values or empty strings.

``` r
train <- titanic_train %>%
  subset(
    !((Age %>% is.na) | (Cabin == ''))
  )

train %>%
  head
```

    ##    PassengerId Survived Pclass
    ## 2            2        1      1
    ## 4            4        1      1
    ## 7            7        0      1
    ## 11          11        1      3
    ## 12          12        1      1
    ## 22          22        1      2
    ##                                                   Name    Sex Age SibSp Parch
    ## 2  Cumings, Mrs. John Bradley (Florence Briggs Thayer) female  38     1     0
    ## 4         Futrelle, Mrs. Jacques Heath (Lily May Peel) female  35     1     0
    ## 7                              McCarthy, Mr. Timothy J   male  54     0     0
    ## 11                     Sandstrom, Miss. Marguerite Rut female   4     1     1
    ## 12                            Bonnell, Miss. Elizabeth female  58     0     0
    ## 22                               Beesley, Mr. Lawrence   male  34     0     0
    ##      Ticket    Fare Cabin Embarked
    ## 2  PC 17599 71.2833   C85        C
    ## 4    113803 53.1000  C123        S
    ## 7     17463 51.8625   E46        S
    ## 11  PP 9549 16.7000    G6        S
    ## 12   113783 26.5500  C103        S
    ## 22   248698 13.0000   D56        S

``` rcpp
std::cout << 10 << std::endl;
```
>>>>>>> 906e667c286433f6b8432aecc4ab604cce3a4ecb
