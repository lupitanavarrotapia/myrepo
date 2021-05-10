Project 1: Exploratory Data Analysis on Student Sleep and Online
Learning
================

## Lupita Navarro \| LGN282

*Introduction: In this data analysis, I will focus on the topic of
sleep, specifically whether students receive enough sleep (7 hours), and
how that may affect or is affected by aspects and qualities of online
learning. I find this theme to be interesting due to my own experience
as a college student taking online classes. I’m curious to explore
whether online classes take a toll on other students and their sleep
quality, since I feel like they certainly affect my own sleeping
patterns.*

*The datasets I chose, LosingSleep from the R package ‘Stat2Data’ and
StudentMH from a survey conducted regarding online learning during
Covid-19, both bear the variables of sleep and age. The latter dataset
also contains variables regarding online learning, such as mode of
learning, time spent on online classes, rating of online class
experience, as well as other measurable factors. Using both datasets, I
will attempt to merge the similar variables and analyze if age is
correlated with whether a student is sleeping a minimum of seven hours.
I expect to find a strong correlation between the variables.*

------------------------------------------------------------------------

#### The Data

``` r
# Dataset 1
library(Stat2Data)
data(LosingSleep)


# dataset 2
library(readr)
StudentMH <- read_csv("StudentMentalHealth.csv")
StudentMH
```

    ## # A tibble: 1,182 x 17
    ##    ID    `Age of Subject` `Time spent on ~ `Rating of Onli~ `Medium for onl~
    ##    <chr>            <dbl>            <dbl> <chr>            <chr>           
    ##  1 R1                  21                2 Good             Laptop/Desktop  
    ##  2 R2                  21                0 Excellent        Smartphone      
    ##  3 R3                  20                7 Very poor        Laptop/Desktop  
    ##  4 R4                  20                3 Very poor        Smartphone      
    ##  5 R5                  21                3 Good             Laptop/Desktop  
    ##  6 R6                  21                0 Very poor        Smartphone      
    ##  7 R7                  19                2 Very poor        Smartphone      
    ##  8 R8                  19                2 Very poor        Tablet          
    ##  9 R9                  21                3 Very poor        Laptop/Desktop  
    ## 10 R10                 20                0 Very poor        Laptop/Desktop  
    ## # ... with 1,172 more rows, and 12 more variables: `Time spent on self
    ## #   study` <dbl>, `Time spent on fitness` <dbl>, `Time spent on sleep` <dbl>,
    ## #   `Time spent on social media` <dbl>, `Prefered social media platform` <chr>,
    ## #   `Number of meals per day` <dbl>, `Change in your weight` <chr>, `Health
    ## #   issue during lockdown` <chr>, `Stress busters` <chr>, `Time
    ## #   utilized` <chr>, `Do you find yourself more connected with your family,
    ## #   close friends , relatives ?` <chr>, `What you miss the most` <chr>

*The dataset ‘LosingSleep’, comes from the package Stat2Data, found
within R, and includes the age of an individual and whether the
individual sleeps seven or more hours a day (1=yes; 0=no). Dataset 2
comes from an online site with public data, Kaggle, and is from a study
of “Covid-19 and its impacts on students.” (citation: Kunal Chaturvedi,
“COVID-19 and its Impact on Students.” Kaggle, 2020, doi:
10.34740/KAGGLE/DSV/1720642.)*

#### 1. Tidy

``` r
library(tidyr)
library(dplyr)


# rename sleep variable in losingsleep
losingsleep <- LosingSleep %>%
  rename(enoughsleep = Outcome)


# change var 'enoughsleep' to categorical
losingsleep$enoughsleep <- factor(ifelse(losingsleep$enoughsleep == 1, "Yes", "No"))
losingsleep
```

    ##    Person Age enoughsleep
    ## 1       1  14         Yes
    ## 2       2  18          No
    ## 3       3  17          No
    ## 4       4  18         Yes
    ## 5       5  16          No
    ## 6       6  15          No
    ## 7       7  16          No
    ## 8       8  17         Yes
    ## 9       9  15         Yes
    ## 10     10  15         Yes
    ## 11     11  18          No
    ## 12     12  16         Yes
    ## 13     13  17         Yes
    ## 14     14  16          No
    ## 15     15  15          No
    ## 16     16  17         Yes
    ## 17     17  16         Yes
    ## 18     18  16          No
    ## 19     19  17          No
    ## 20     20  16          No
    ## 21     21  17         Yes
    ## 22     22  15         Yes
    ## 23     23  16         Yes
    ## 24     24  17         Yes
    ## 25     25  15         Yes
    ## 26     26  16         Yes
    ## 27     27  16         Yes
    ## 28     28  16         Yes
    ## 29     29  14         Yes
    ## 30     30  15         Yes
    ## 31     31  14          No
    ## 32     32  17          No
    ## 33     33  18          No
    ##  [ reached 'max' / getOption("max.print") -- omitted 413 rows ]

``` r
# remove column 'Person' from 'losingsleep'
losingsleep$Person <- NULL

# remove 'Id' column from 'StudentMH'
StudentMH$ID <- NULL

# rename "age of subject" to "Age" in 'StudentMH'
StudentMH1 <- StudentMH %>%
  rename( Age = "Age of Subject")
StudentMH1 #view new variable name
```

    ## # A tibble: 1,182 x 16
    ##      Age `Time spent on ~ `Rating of Onli~ `Medium for onl~ `Time spent on ~
    ##    <dbl>            <dbl> <chr>            <chr>                       <dbl>
    ##  1    21                2 Good             Laptop/Desktop                  4
    ##  2    21                0 Excellent        Smartphone                      0
    ##  3    20                7 Very poor        Laptop/Desktop                  3
    ##  4    20                3 Very poor        Smartphone                      2
    ##  5    21                3 Good             Laptop/Desktop                  3
    ##  6    21                0 Very poor        Smartphone                      6
    ##  7    19                2 Very poor        Smartphone                      2
    ##  8    19                2 Very poor        Tablet                          1
    ##  9    21                3 Very poor        Laptop/Desktop                  4
    ## 10    20                0 Very poor        Laptop/Desktop                  1
    ## # ... with 1,172 more rows, and 11 more variables: `Time spent on
    ## #   fitness` <dbl>, `Time spent on sleep` <dbl>, `Time spent on social
    ## #   media` <dbl>, `Prefered social media platform` <chr>, `Number of meals per
    ## #   day` <dbl>, `Change in your weight` <chr>, `Health issue during
    ## #   lockdown` <chr>, `Stress busters` <chr>, `Time utilized` <chr>, `Do you
    ## #   find yourself more connected with your family, close friends , relatives
    ## #   ?` <chr>, `What you miss the most` <chr>

``` r
# rename sleep variable in 'StudentMH'
StudentMH2 <- StudentMH1 %>%
  rename( enoughsleep = `Time spent on sleep`)

# change var 'enoughsleep' in 'StudentMH2' to catergorical
StudentMH2$enoughsleep <- factor(ifelse(StudentMH2$enoughsleep >= 7, "Yes", "No"))
StudentMH2
```

    ## # A tibble: 1,182 x 16
    ##      Age `Time spent on ~ `Rating of Onli~ `Medium for onl~ `Time spent on ~
    ##    <dbl>            <dbl> <chr>            <chr>                       <dbl>
    ##  1    21                2 Good             Laptop/Desktop                  4
    ##  2    21                0 Excellent        Smartphone                      0
    ##  3    20                7 Very poor        Laptop/Desktop                  3
    ##  4    20                3 Very poor        Smartphone                      2
    ##  5    21                3 Good             Laptop/Desktop                  3
    ##  6    21                0 Very poor        Smartphone                      6
    ##  7    19                2 Very poor        Smartphone                      2
    ##  8    19                2 Very poor        Tablet                          1
    ##  9    21                3 Very poor        Laptop/Desktop                  4
    ## 10    20                0 Very poor        Laptop/Desktop                  1
    ## # ... with 1,172 more rows, and 11 more variables: `Time spent on
    ## #   fitness` <dbl>, enoughsleep <fct>, `Time spent on social media` <dbl>,
    ## #   `Prefered social media platform` <chr>, `Number of meals per day` <dbl>,
    ## #   `Change in your weight` <chr>, `Health issue during lockdown` <chr>,
    ## #   `Stress busters` <chr>, `Time utilized` <chr>, `Do you find yourself more
    ## #   connected with your family, close friends , relatives ?` <chr>, `What you
    ## #   miss the most` <chr>

*To help tidy the datasets, I got rid of unnecessary columns, renamed
some of the variables, and changed some variable types, “enoughsleep”
from both datasets, to allow for an easier merge.*

#### 2. Join/Merge

``` r
#join both datasets into a new dataset called "StudentSleep"

StudentSleep <- StudentMH2 %>%
  full_join(losingsleep) #joined by 'Age' and 'enoughsleep'
                           
StudentSleep #view 'StudentSleep'
```

    ## # A tibble: 15,334 x 16
    ##      Age `Time spent on ~ `Rating of Onli~ `Medium for onl~ `Time spent on ~
    ##    <dbl>            <dbl> <chr>            <chr>                       <dbl>
    ##  1    21                2 Good             Laptop/Desktop                  4
    ##  2    21                0 Excellent        Smartphone                      0
    ##  3    20                7 Very poor        Laptop/Desktop                  3
    ##  4    20                3 Very poor        Smartphone                      2
    ##  5    21                3 Good             Laptop/Desktop                  3
    ##  6    21                0 Very poor        Smartphone                      6
    ##  7    19                2 Very poor        Smartphone                      2
    ##  8    19                2 Very poor        Tablet                          1
    ##  9    21                3 Very poor        Laptop/Desktop                  4
    ## 10    20                0 Very poor        Laptop/Desktop                  1
    ## # ... with 15,324 more rows, and 11 more variables: `Time spent on
    ## #   fitness` <dbl>, enoughsleep <fct>, `Time spent on social media` <dbl>,
    ## #   `Prefered social media platform` <chr>, `Number of meals per day` <dbl>,
    ## #   `Change in your weight` <chr>, `Health issue during lockdown` <chr>,
    ## #   `Stress busters` <chr>, `Time utilized` <chr>, `Do you find yourself more
    ## #   connected with your family, close friends , relatives ?` <chr>, `What you
    ## #   miss the most` <chr>

``` r
distinct(StudentSleep)
```

    ## # A tibble: 1,179 x 16
    ##      Age `Time spent on ~ `Rating of Onli~ `Medium for onl~ `Time spent on ~
    ##    <dbl>            <dbl> <chr>            <chr>                       <dbl>
    ##  1    21                2 Good             Laptop/Desktop                  4
    ##  2    21                0 Excellent        Smartphone                      0
    ##  3    20                7 Very poor        Laptop/Desktop                  3
    ##  4    20                3 Very poor        Smartphone                      2
    ##  5    21                3 Good             Laptop/Desktop                  3
    ##  6    21                0 Very poor        Smartphone                      6
    ##  7    19                2 Very poor        Smartphone                      2
    ##  8    19                2 Very poor        Tablet                          1
    ##  9    21                3 Very poor        Laptop/Desktop                  4
    ## 10    20                0 Very poor        Laptop/Desktop                  1
    ## # ... with 1,169 more rows, and 11 more variables: `Time spent on
    ## #   fitness` <dbl>, enoughsleep <fct>, `Time spent on social media` <dbl>,
    ## #   `Prefered social media platform` <chr>, `Number of meals per day` <dbl>,
    ## #   `Change in your weight` <chr>, `Health issue during lockdown` <chr>,
    ## #   `Stress busters` <chr>, `Time utilized` <chr>, `Do you find yourself more
    ## #   connected with your family, close friends , relatives ?` <chr>, `What you
    ## #   miss the most` <chr>

*To join the data, I chose to conduct a full join because it merged the
data by all variables of the same name, ‘enoughsleep’ and ‘Age’. Because
the dataset, "losingsleep’ did not contain other variables included in
‘StudentMH’, they were automatically filled in instead of left ‘NA’.
This is a potential issue that could skew the data since it leads to
multiple repeated responses/rows.*

#### 3. Summary Statistics

``` r
#summary stats for dataset 1 
summary(losingsleep)
```

    ##       Age        enoughsleep
    ##  Min.   :14.00   No :150    
    ##  1st Qu.:15.00   Yes:296    
    ##  Median :16.00              
    ##  Mean   :16.08              
    ##  3rd Qu.:17.00              
    ##  Max.   :18.00

``` r
#summary statistics for dataset 2
summary(StudentMH2)
```

    ##       Age        Time spent on Online Class Rating of Online Class experience
    ##  Min.   : 7.00   Min.   : 0.000             Length:1182                      
    ##  1st Qu.:17.00   1st Qu.: 2.000             Class :character                 
    ##  Median :20.00   Median : 3.000             Mode  :character                 
    ##  Mean   :20.17   Mean   : 3.209                                              
    ##  3rd Qu.:21.00   3rd Qu.: 5.000                                              
    ##  Max.   :59.00   Max.   :10.000                                              
    ##  Medium for online class Time spent on self study Time spent on fitness
    ##  Length:1182             Min.   : 0.000           Min.   :0.0000       
    ##  Class :character        1st Qu.: 2.000           1st Qu.:0.0000       
    ##  Mode  :character        Median : 2.000           Median :1.0000       
    ##                          Mean   : 2.912           Mean   :0.7658       
    ##                          3rd Qu.: 4.000           3rd Qu.:1.0000       
    ##                          Max.   :18.000           Max.   :5.0000       
    ##  enoughsleep Time spent on social media Prefered social media platform
    ##  No :226     Min.   : 0.000             Length:1182                   
    ##  Yes:956     1st Qu.: 1.000             Class :character              
    ##              Median : 2.000             Mode  :character              
    ##              Mean   : 2.366                                           
    ##              3rd Qu.: 3.000                                           
    ##              Max.   :10.000                                           
    ##  Number of meals per day Change in your weight Health issue during lockdown
    ##  Min.   :1.000           Length:1182           Length:1182                 
    ##  1st Qu.:2.000           Class :character      Class :character            
    ##  Median :3.000           Mode  :character      Mode  :character            
    ##  Mean   :2.918                                                             
    ##  3rd Qu.:3.000                                                             
    ##  Max.   :8.000                                                             
    ##  Stress busters     Time utilized     
    ##  Length:1182        Length:1182       
    ##  Class :character   Class :character  
    ##  Mode  :character   Mode  :character  
    ##                                       
    ##                                       
    ##                                       
    ##  Do you find yourself more connected with your family, close friends , relatives  ?
    ##  Length:1182                                                                       
    ##  Class :character                                                                  
    ##  Mode  :character                                                                  
    ##                                                                                    
    ##                                                                                    
    ##                                                                                    
    ##  What you miss the most
    ##  Length:1182           
    ##  Class :character      
    ##  Mode  :character      
    ##                        
    ##                        
    ## 

*For dataset 1, losingsleep, we see that the mean age of the students
who engaged in the survey is about 16 years of age, and that a greater
majority of participants believe they get at least 7 hours of sleep (296
students), while about half believe they do not (150 students). For
dataset 2, we have a lot more descriptive statistics due to the greater
amount of variables in this dataset. We can note that the average age of
the participants of dataset “Student MH” is about 20 years of age.
Similar to the first dataset, we see that a greater amount of
participants believe they get enough - 7 hrs- of sleep (956
participants), while a small minority believe they do not (226
participants).*

*I focused on summarizing these two variables, age and enough sleep,
because they are the focus of my study, but something interesting to
note is that students of the second dataset averaged about 3 hrs of self
study and a mean of 2.4 hours on social media, indicating that almost
the same time is spent on studies and social media. I think it would be
interesting to see how these variables impact whether a student recieves
7 hours of sleep a night.*

------------------------------------------------------------------------

    ##        sysname        release        version       nodename        machine 
    ##      "Windows"       "10 x64"  "build 19042"   "GISELLESPC"       "x86-64" 
    ##          login           user effective_user 
    ##       "Lupita"       "Lupita"       "Lupita"
