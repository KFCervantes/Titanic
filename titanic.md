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
