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
library(titanic)
```

The first thing I decided to do was check for where the null values in
dataframe might be.

``` r
titanic_train %>%
  is.na %>%
  summary
```

    ##  PassengerId      Survived         Pclass           Name        
    ##  Mode :logical   Mode :logical   Mode :logical   Mode :logical  
    ##  FALSE:891       FALSE:891       FALSE:891       FALSE:891      
    ##                                                                 
    ##     Sex             Age            SibSp           Parch        
    ##  Mode :logical   Mode :logical   Mode :logical   Mode :logical  
    ##  FALSE:891       FALSE:714       FALSE:891       FALSE:891      
    ##                  TRUE :177                                      
    ##    Ticket           Fare           Cabin          Embarked      
    ##  Mode :logical   Mode :logical   Mode :logical   Mode :logical  
    ##  FALSE:891       FALSE:891       FALSE:891       FALSE:891      
    ## 
