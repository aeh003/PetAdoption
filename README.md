
# Pet Adoption

#### By Amelia Humphrey & Mariana Correa

### Data Description:

Pet Adoption:
<https://www.kaggle.com/datasets/rabieelkharoua/predict-pet-adoption-status-dataset>

``` r
library(readr)
pets <- read_csv("pet_adoption_data.csv")
```

    ## Rows: 2007 Columns: 13
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (4): PetType, Breed, Color, Size
    ## dbl (9): PetID, AgeMonths, WeightKg, Vaccinated, HealthCondition, TimeInShel...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
head(pets)
```

    ## # A tibble: 6 × 13
    ##   PetID PetType Breed  AgeMonths Color Size  WeightKg Vaccinated HealthCondition
    ##   <dbl> <chr>   <chr>      <dbl> <chr> <chr>    <dbl>      <dbl>           <dbl>
    ## 1   500 Bird    Parak…       131 Oran… Large     5.04          1               0
    ## 2   501 Rabbit  Rabbit        73 White Large    16.1           0               0
    ## 3   502 Dog     Golde…       136 Oran… Medi…     2.08          0               0
    ## 4   503 Bird    Parak…        97 White Small     3.34          0               0
    ## 5   504 Rabbit  Rabbit       123 Gray  Large    20.5           0               0
    ## 6   505 Dog     Labra…        70 Brown Large    21.0           0               0
    ## # ℹ 4 more variables: TimeInShelterDays <dbl>, AdoptionFee <dbl>,
    ## #   PreviousOwner <dbl>, AdoptionLikelihood <dbl>

``` r
summary(pets)
```

    ##      PetID        PetType             Breed             AgeMonths     
    ##  Min.   : 500   Length:2007        Length:2007        Min.   :  1.00  
    ##  1st Qu.:1002   Class :character   Class :character   1st Qu.: 48.00  
    ##  Median :1503   Mode  :character   Mode  :character   Median : 94.00  
    ##  Mean   :1503                                         Mean   : 92.28  
    ##  3rd Qu.:2004                                         3rd Qu.:138.00  
    ##  Max.   :2506                                         Max.   :179.00  
    ##     Color               Size              WeightKg        Vaccinated   
    ##  Length:2007        Length:2007        Min.   : 1.018   Min.   :0.000  
    ##  Class :character   Class :character   1st Qu.: 8.730   1st Qu.:0.000  
    ##  Mode  :character   Mode  :character   Median :15.925   Median :1.000  
    ##                                        Mean   :15.706   Mean   :0.701  
    ##                                        3rd Qu.:22.737   3rd Qu.:1.000  
    ##                                        Max.   :29.996   Max.   :1.000  
    ##  HealthCondition  TimeInShelterDays  AdoptionFee    PreviousOwner   
    ##  Min.   :0.0000   Min.   : 1.00     Min.   :  0.0   Min.   :0.0000  
    ##  1st Qu.:0.0000   1st Qu.:21.00     1st Qu.:127.0   1st Qu.:0.0000  
    ##  Median :0.0000   Median :45.00     Median :242.0   Median :0.0000  
    ##  Mean   :0.1963   Mean   :43.97     Mean   :249.1   Mean   :0.3019  
    ##  3rd Qu.:0.0000   3rd Qu.:66.00     3rd Qu.:375.0   3rd Qu.:1.0000  
    ##  Max.   :1.0000   Max.   :89.00     Max.   :499.0   Max.   :1.0000  
    ##  AdoptionLikelihood
    ##  Min.   :0.0000    
    ##  1st Qu.:0.0000    
    ##  Median :0.0000    
    ##  Mean   :0.3284    
    ##  3rd Qu.:1.0000    
    ##  Max.   :1.0000

``` r
dim(pets)
```

    ## [1] 2007   13

The dataset we chose provides a comprehensive look into various factors
that can influence the likelihood of a pet being adopted from a shelter,
covering various characteristics and attributes.

We do foresee a lot of data cleaning. We will be checking for missing
values, duplicates, and outliers that could skew the data. We also plan
to change columns with binary ‘1’ and ‘0’ values to boolean values.

``` r
# Load libraries
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
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ forcats   1.0.1     ✔ stringr   1.6.0
    ## ✔ lubridate 1.9.5     ✔ tibble    3.3.1
    ## ✔ purrr     1.2.1     ✔ tidyr     1.3.2

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
# Check for missing values
colSums(is.na(pets))
```

    ##              PetID            PetType              Breed          AgeMonths 
    ##                  0                  0                  0                  0 
    ##              Color               Size           WeightKg         Vaccinated 
    ##                  0                  0                  0                  0 
    ##    HealthCondition  TimeInShelterDays        AdoptionFee      PreviousOwner 
    ##                  0                  0                  0                  0 
    ## AdoptionLikelihood 
    ##                  0

There are no missing values in any of the columns of this dataset.

``` r
# Count duplicates
sum(duplicated(pets))
```

    ## [1] 0

There are no duplicate rows in this dataset.

``` r
# Convert binary
pets$Vaccinated <- as.logical(pets$Vaccinated)
pets$HealthCondition <- as.logical(pets$HealthCondition)
pets$PreviousOwner <- as.logical(pets$PreviousOwner)
pets$AdoptionLikelihood <- as.logical(pets$AdoptionLikelihood)
```

The columns `Vaccinated`, `HealthCondition`, `PreviousOwner` and
`AdoptionLikelihood` were changed from “0” and “1” to boolean values.

``` r
# Check datset
str(pets)
```

    ## spc_tbl_ [2,007 × 13] (S3: spec_tbl_df/tbl_df/tbl/data.frame)
    ##  $ PetID             : num [1:2007] 500 501 502 503 504 505 506 507 508 509 ...
    ##  $ PetType           : chr [1:2007] "Bird" "Rabbit" "Dog" "Bird" ...
    ##  $ Breed             : chr [1:2007] "Parakeet" "Rabbit" "Golden Retriever" "Parakeet" ...
    ##  $ AgeMonths         : num [1:2007] 131 73 136 97 123 70 169 13 49 60 ...
    ##  $ Color             : chr [1:2007] "Orange" "White" "Orange" "White" ...
    ##  $ Size              : chr [1:2007] "Large" "Large" "Medium" "Small" ...
    ##  $ WeightKg          : num [1:2007] 5.04 16.09 2.08 3.34 20.5 ...
    ##  $ Vaccinated        : logi [1:2007] TRUE FALSE FALSE FALSE FALSE FALSE ...
    ##  $ HealthCondition   : logi [1:2007] FALSE FALSE FALSE FALSE FALSE FALSE ...
    ##  $ TimeInShelterDays : num [1:2007] 27 8 85 61 28 87 70 3 69 73 ...
    ##  $ AdoptionFee       : num [1:2007] 140 235 385 217 14 301 440 137 405 231 ...
    ##  $ PreviousOwner     : logi [1:2007] FALSE FALSE FALSE TRUE TRUE TRUE ...
    ##  $ AdoptionLikelihood: logi [1:2007] FALSE FALSE FALSE FALSE FALSE FALSE ...
    ##  - attr(*, "spec")=
    ##   .. cols(
    ##   ..   PetID = col_double(),
    ##   ..   PetType = col_character(),
    ##   ..   Breed = col_character(),
    ##   ..   AgeMonths = col_double(),
    ##   ..   Color = col_character(),
    ##   ..   Size = col_character(),
    ##   ..   WeightKg = col_double(),
    ##   ..   Vaccinated = col_double(),
    ##   ..   HealthCondition = col_double(),
    ##   ..   TimeInShelterDays = col_double(),
    ##   ..   AdoptionFee = col_double(),
    ##   ..   PreviousOwner = col_double(),
    ##   ..   AdoptionLikelihood = col_double()
    ##   .. )
    ##  - attr(*, "problems")=<externalptr>

``` r
summary(pets)
```

    ##      PetID        PetType             Breed             AgeMonths     
    ##  Min.   : 500   Length:2007        Length:2007        Min.   :  1.00  
    ##  1st Qu.:1002   Class :character   Class :character   1st Qu.: 48.00  
    ##  Median :1503   Mode  :character   Mode  :character   Median : 94.00  
    ##  Mean   :1503                                         Mean   : 92.28  
    ##  3rd Qu.:2004                                         3rd Qu.:138.00  
    ##  Max.   :2506                                         Max.   :179.00  
    ##     Color               Size              WeightKg      Vaccinated     
    ##  Length:2007        Length:2007        Min.   : 1.018   Mode :logical  
    ##  Class :character   Class :character   1st Qu.: 8.730   FALSE:600      
    ##  Mode  :character   Mode  :character   Median :15.925   TRUE :1407     
    ##                                        Mean   :15.706                  
    ##                                        3rd Qu.:22.737                  
    ##                                        Max.   :29.996                  
    ##  HealthCondition TimeInShelterDays  AdoptionFee    PreviousOwner  
    ##  Mode :logical   Min.   : 1.00     Min.   :  0.0   Mode :logical  
    ##  FALSE:1613      1st Qu.:21.00     1st Qu.:127.0   FALSE:1401     
    ##  TRUE :394       Median :45.00     Median :242.0   TRUE :606      
    ##                  Mean   :43.97     Mean   :249.1                  
    ##                  3rd Qu.:66.00     3rd Qu.:375.0                  
    ##                  Max.   :89.00     Max.   :499.0                  
    ##  AdoptionLikelihood
    ##  Mode :logical     
    ##  FALSE:1348        
    ##  TRUE :659         
    ##                    
    ##                    
    ## 

Some summaries we plan to explore are the averages of numerical values
between the different types of pet, and among different breeds. We also
plan to investigate the data by exploring the different pet type and
breeds, as well as the distribution of different color, weight, and
type.

``` r
#different types of pet 
unique(pets$PetType)
```

    ## [1] "Bird"   "Rabbit" "Dog"    "Cat"

``` r
pets |> 
  filter(PetType == "Dog") |> 
  distinct(Breed)
```

    ## # A tibble: 3 × 1
    ##   Breed           
    ##   <chr>           
    ## 1 Golden Retriever
    ## 2 Labrador        
    ## 3 Poodle

``` r
pets |> 
  filter(PetType == "Cat") |> 
  distinct(Breed)
```

    ## # A tibble: 2 × 1
    ##   Breed  
    ##   <chr>  
    ## 1 Siamese
    ## 2 Persian

``` r
pets |> 
  filter(PetType == "Bird") |> 
  distinct(Breed)
```

    ## # A tibble: 1 × 1
    ##   Breed   
    ##   <chr>   
    ## 1 Parakeet

``` r
pets |> 
  filter(PetType == "Rabbit") |> 
  distinct(Breed)
```

    ## # A tibble: 1 × 1
    ##   Breed 
    ##   <chr> 
    ## 1 Rabbit

``` r
pets |>
  ggplot(aes(x = PetType, fill = PetType)) +
  geom_bar() +
  ggtitle("Number of Pets by Pet Type") +
  xlab("Pet Type") 
```

![](README_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

### Research Questions:

1.  What factors impact the likelihood of adoption? Does a longer time
    in shelter decrease the likelihood of adoption? What type of animal
    is most likely to get adopted? How does the likelihood vary between
    breed for the same pet type? Is there a certain pet color that is
    more likely to be adopted?

``` r
#On average how likely is a pet to be adopted?
mean(pets$AdoptionLikelihood)
```

    ## [1] 0.3283508

``` r
#The time in shleter is not related to the likelihood of adoption
cor(pets$TimeInShelterDays, pets$AdoptionLikelihood)
```

    ## [1] 0.008867397

``` r
#which pet is most likely to be adopted?

 pets |> 
  group_by(PetType) |> 
  summarize(
    n = n(),
    adoptionRate = mean(AdoptionLikelihood)
  ) |> 
  arrange(desc(adoptionRate))
```

    ## # A tibble: 4 × 3
    ##   PetType     n adoptionRate
    ##   <chr>   <int>        <dbl>
    ## 1 Dog       522        0.464
    ## 2 Bird      487        0.302
    ## 3 Cat       505        0.287
    ## 4 Rabbit    493        0.254

``` r
RateAdoption <- pets |> 
  group_by(PetType) |> 
  summarize(
    adoptionRate = mean(AdoptionLikelihood)
  ) |> 
  arrange(desc(adoptionRate))

#SOOOO UGLY FIXXXX
RateAdoption |> 
  ggplot(aes(x=adoptionRate, y = PetType)) +
  geom_col(fill = "#EE1289") 
```

![](README_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
#Does breed play a role 
#Note Rabbit and Birds were not considered as they each only have one Breed
pets |> 
  filter(PetType == "Dog") |> 
  group_by(Breed) |> 
  summarise(
    n = n(),
    rate  = mean(AdoptionLikelihood)
  ) |> 
  arrange(desc(rate)) |> 
  ggplot(aes(x = Breed, y = rate)) + 
  geom_col(fill = "darkslategray2") + 
  ggtitle("Breed and Adoption Rate for Dogs")+
  ylab("Adoption Rate")
```

![](README_files/figure-gfm/unnamed-chunk-7-2.png)<!-- -->

``` r
pets |> 
  filter(PetType == "Cat") |> 
  group_by(Breed) |> 
  summarise(
    n = n(),
    rate  = mean(AdoptionLikelihood)
  ) |> 
  arrange(desc(rate))  |> 
  ggplot(aes(x = Breed, y = rate)) + 
  geom_col(fill = "darkslategray") + 
  ggtitle("Breed and Adoption Rate for Cats")+
  ylab("Adoption Rate")
```

![](README_files/figure-gfm/unnamed-chunk-7-3.png)<!-- -->

``` r
#Does color

pets |> 
  group_by(Color) |> 
  summarise(
    n = n(),
    rate = mean(AdoptionLikelihood)
  ) |> 
  arrange(desc(rate)) |> 
  ggplot(aes(x = rate, y= Color)) + 
  geom_col(fill = "dodgerblue3") + 
  ggtitle("Color and adoption")
```

![](README_files/figure-gfm/unnamed-chunk-7-4.png)<!-- -->

``` r
#facet moment
pets |> 
  group_by(PetType, Color) |> 
  summarise(
    n = n(),
    rate = mean(AdoptionLikelihood)
  ) |> 
  ggplot(aes(x= Color, y = rate, fill = Color))  +
  geom_col() + 
  scale_fill_manual(values= c("Black" = "black", "Brown" = "burlywood4", "Gray" = "gray80", "Orange" = "darkorange" , "White" = "cornsilk" )) +
  facet_wrap(~PetType) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) + 
  ggtitle("Color vs Average Adoption Likelihood")+
  ylab("Average Adoption Likelihood")
```

    ## `summarise()` has regrouped the output.
    ## ℹ Summaries were computed grouped by PetType and Color.
    ## ℹ Output is grouped by PetType.
    ## ℹ Use `summarise(.groups = "drop_last")` to silence this message.
    ## ℹ Use `summarise(.by = c(PetType, Color))` for per-operation grouping
    ##   (`?dplyr::dplyr_by`) instead.

![](README_files/figure-gfm/unnamed-chunk-7-5.png)<!-- -->

ADD IN A CONCLUSION BASED ON THE DATA and the graphs

Note: the average was taken as each PetType does not have the same
amount of entries. Taking the mean make the data more meaningful in a
regardless of the amount of data for each type of animal.

2.  What factor has the largest impact on time spent in the shelter? Is
    age a contributor? What breed is more likely to spend a long time in
    the shelter? Does health condition impact the time spent in shelter?

``` r
#Putting more emphasis on the median is important as there are many outlier that can skew the mean results 

pets |> 
  ggplot(aes(x = TimeInShelterDays))  +
  geom_histogram(binwidth = 3, fill = "darkslategray4") + 
  ggtitle("Time Spent in Shelter") + 
  ylab("Number of Pets")
```

![](README_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

``` r
pets |> 
  ggplot(aes(x = TimeInShelterDays))  +
  geom_histogram(binwidth = 3) + 
  facet_wrap(~PetType) +
  ggtitle("Time Spent in Shelter") + 
  ylab("Number of Pets")  
```

![](README_files/figure-gfm/unnamed-chunk-8-2.png)<!-- -->

``` r
pets |> 
  group_by(PetType) |> 
  summarise(
    n = n(),
    avgDays = mean(TimeInShelterDays),
    medianDays = median(TimeInShelterDays)
  ) |> 
  arrange(desc(medianDays)) |> 
  ggplot(aes(x = PetType, y = medianDays)) + 
  geom_col(fill = "darkorchid4") + 
  ggtitle("Median Time in Shelter by Pet Type") + 
  ylab("Median") 
```

![](README_files/figure-gfm/unnamed-chunk-8-3.png)<!-- -->

``` r
pets |> 
  group_by(Breed) |> 
  summarise(
    n = n(),
    avgDays = mean(TimeInShelterDays),
    medianDays = median(TimeInShelterDays)
  ) |> 
  arrange(desc(medianDays)) |> 
  ggplot(aes(x = Breed, y = medianDays)) + 
  geom_col(fill = "darkolivegreen") + 
  ggtitle("Median Time in Shelter by Breed") + 
  ylab("Median") + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
```

![](README_files/figure-gfm/unnamed-chunk-8-4.png)<!-- -->

``` r
#making a facet
cor(pets$AgeMonths, pets$TimeInShelterDays, use = "complete.obs")
```

    ## [1] 0.03683713

``` r
cor(pets$WeightKg, pets$TimeInShelterDays, use = "complete.obs")
```

    ## [1] -0.000979996

``` r
cor(pets$AdoptionFee, pets$TimeInShelterDays, use = "complete.obs")
```

    ## [1] -0.007104482

``` r
#shows the randomness/lack of correlation
pets |> 
  ggplot(aes(x = AgeMonths, y = TimeInShelterDays)) + 
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "dodgerblue3") +
  facet_wrap(~PetType) + 
  ggtitle("Age vs Time in Shelter") + 
  xlab("Age (months)")  +
  ylab("Time in Shelter (days)")
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](README_files/figure-gfm/unnamed-chunk-8-5.png)<!-- -->

``` r
#Does being old increase shelter time 
#over 7 can be considered old for a pet

#add color
#add axises etitles and title
pets |> 
  mutate(AgeGroups = if_else(AgeMonths <24, "Young(<2 years old)", if_else(AgeMonths < 84, "Adult (2-7 years)", "Old(>7 years old"))) |> 
  group_by(AgeGroups) |> 
  summarise(
    median = median(TimeInShelterDays)
  )  |> 
  ggplot(aes(x = AgeGroups, y = median)) + 
  geom_col()
```

![](README_files/figure-gfm/unnamed-chunk-8-6.png)<!-- -->

``` r
#long stay calculate top 10% of the the total time shelter

pets2 <- pets |> 
  mutate(LongStay = TimeInShelterDays >= quantile(TimeInShelterDays, 0.9, na.rm = TRUE))


#Is there a breed that typical stays longer 

LongStayBreed <- pets2 |>
  group_by(Breed) |> 
  summarise(
    n = n(),
    medianDays = median(TimeInShelterDays), longStayRate = mean(LongStay),
    avgDays  =  mean(TimeInShelterDays)
  ) |> 
  arrange(desc(medianDays))
LongStayBreed
```

    ## # A tibble: 7 × 5
    ##   Breed                n medianDays longStayRate avgDays
    ##   <chr>            <int>      <dbl>        <dbl>   <dbl>
    ## 1 Golden Retriever   162       48         0.0988    45.6
    ## 2 Rabbit             493       48         0.114     45.4
    ## 3 Persian            252       45.5       0.107     43.6
    ## 4 Parakeet           487       43         0.107     43.3
    ## 5 Poodle             167       43         0.0719    43.4
    ## 6 Siamese            253       43         0.111     42.2
    ## 7 Labrador           193       42         0.155     43.8

``` r
#why n 30
#mess with the prettiness of the graph
LongStayBreed |> 
  filter(n >=30) |> 
  arrange(desc(longStayRate)) |> 
  ggplot(aes(x = longStayRate, y = reorder(Breed, longStayRate))) +
  geom_bar(stat = "identity") + 
  coord_flip()
```

![](README_files/figure-gfm/unnamed-chunk-8-7.png)<!-- -->

``` r
#Does having a healthCondition impact shelter time?

pets |> 
  group_by(HealthCondition) |> 
  summarise(
    n = n(),
    avgDays = mean(TimeInShelterDays),
    medianDays = median(TimeInShelterDays)
  ) 
```

    ## # A tibble: 2 × 4
    ##   HealthCondition     n avgDays medianDays
    ##   <lgl>           <int>   <dbl>      <dbl>
    ## 1 FALSE            1613    44.1         45
    ## 2 TRUE              394    43.3         45

``` r
#graph
pets |> 
  ggplot(aes(x = HealthCondition, y = TimeInShelterDays)) +
  geom_boxplot() + 
  xlab("Health Condition")
```

![](README_files/figure-gfm/unnamed-chunk-8-8.png)<!-- -->

``` r
#add titles and colors 
pets |> 
  ggplot(aes(x = HealthCondition, y = TimeInShelterDays, fill = HealthCondition)) +
  geom_boxplot() +
  coord_flip()+
  facet_wrap(~PetType)  +
  xlab("Health Condition")
```

![](README_files/figure-gfm/unnamed-chunk-8-9.png)<!-- -->

3.  Is there a breed that has a larger adoption fee? Does the adoption
    fee have any correlation to the size of the pet? Do pets with higher
    adoption fees have better health conditions/are vaccinated? Does the
    color of the pet impact the adoption fee?

``` r
# Is there a breed that has a larger adoption fee?
# Using median fee to avoid skewness from outliers
pets |> 
  group_by(Breed) |> 
  summarise(
    n = n(),
    avgFee = mean(AdoptionFee),
    medianFee = median(AdoptionFee)
  ) |> 
  arrange(desc(medianFee)) |> 
  ggplot(aes(x = reorder(Breed, medianFee), y = medianFee)) + 
  geom_col(fill = "lightseagreen") + 
  coord_flip() +
  ggtitle("Median Adoption Fee by Breed") +
  xlab("Breed") +
  ylab("Median Adoption Fee ($)")
```

![](README_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

``` r
# Does the adoption fee have any correlation to the size of the pet?
cor(pets$WeightKg, pets$AdoptionFee, use = "complete.obs")
```

    ## [1] -0.002367119

``` r
pets |> 
  ggplot(aes(x = WeightKg, y = AdoptionFee)) +
  geom_point(alpha = 0.5, color = "sienna") +
  geom_smooth(method = "lm", color = "black", se = FALSE) +
  facet_wrap(~PetType) + 
  ggtitle("Adoption Fee vs. Pet Weight by Pet Type") +
  xlab("Weight (Kg)") +
  ylab("Adoption Fee ($)")
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](README_files/figure-gfm/unnamed-chunk-9-2.png)<!-- -->

``` r
# Do pets with higher adoption fees have better health conditions/are vaccinated?
pets |> 
  ggplot(aes(x = HealthCondition, y = AdoptionFee, fill = HealthCondition)) +
  geom_boxplot() +
  ggtitle("Adoption Fee by Health Condition") +
  xlab("Health Condition") +
  ylab("Adoption Fee ($)")
```

![](README_files/figure-gfm/unnamed-chunk-9-3.png)<!-- -->

``` r
pets |> 
  ggplot(aes(x = Vaccinated, y = AdoptionFee, fill = Vaccinated)) +
  geom_boxplot() +
  scale_fill_manual(values = c("FALSE" = "tomato", "TRUE" = "springgreen4")) +
  ggtitle("Adoption Fee by Vaccination Status") +
  xlab("Vaccinated") +
  ylab("Adoption Fee ($)")
```

![](README_files/figure-gfm/unnamed-chunk-9-4.png)<!-- -->

``` r
# Does the color of the pet impact the adoption fee?
pets |> 
  ggplot(aes(x = reorder(Color, AdoptionFee, FUN = median), y = AdoptionFee, fill = Color)) +
  geom_boxplot() +
  scale_fill_manual(values= c("Black" = "grey30", "Brown" = "burlywood4", 
                              "Gray" = "gray70", "Orange" = "darkorange", "White" = "wheat1")) +
  ggtitle("Adoption Fee Distribution by Color") +
  xlab("Color") +
  ylab("Adoption Fee ($)") +
  theme(legend.position = "none")
```

![](README_files/figure-gfm/unnamed-chunk-9-5.png)<!-- -->

4.  Which type of pets are most likely to be healthy (HealthCondition =
    1), and how does that connect to the time spent in the shelter? Are
    medical consitions more common among older animals across all pet
    types? Are certain breeds more likely to have a medical condition?

``` r
# Which type of pets are most likely to be healthy (HealthCondition = FALSE)?
pets |> 
  group_by(PetType) |> 
  summarise(
    n = n(),
    # Since TRUE = Medical Condition, the proportion of healthy pets is 1 minus the mean
    HealthRate = 1 - mean(HealthCondition) 
  ) |> 
  arrange(desc(HealthRate)) |> 
  ggplot(aes(x = reorder(PetType, HealthRate), y = HealthRate)) +
  geom_col(fill = "mediumpurple3") +
  ggtitle("Proportion of Healthy Pets by Pet Type") +
  xlab("Pet Type") +
  ylab("Proportion Healthy (HealthCondition = FALSE)")
```

![](README_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

``` r
# How does that connect to the time spent in the shelter?
pets |> 
  ggplot(aes(x = HealthCondition, y = TimeInShelterDays, fill = HealthCondition)) +
  geom_boxplot() +
  scale_fill_manual(values = c("FALSE" = "springgreen4", "TRUE" = "tomato")) +
  facet_wrap(~PetType) +
  ggtitle("Time in Shelter by Medical Condition and Pet Type") +
  xlab("Has Medical Condition (TRUE = Yes, FALSE = No)") +
  ylab("Time in Shelter (Days)")
```

![](README_files/figure-gfm/unnamed-chunk-10-2.png)<!-- -->

``` r
# Are medical conditions more common among older animals across all pet types?
pets |> 
  mutate(AgeGroups = if_else(AgeMonths < 24, "Young(<2 years)", 
                     if_else(AgeMonths < 84, "Adult(2-7 years)", "Old(>7 years)"))) |> 
  mutate(AgeGroups = factor(AgeGroups, levels = c("Young(<2 years)", "Adult(2-7 years)", "Old(>7 years)"))) |>
  group_by(AgeGroups, PetType) |> 
  summarise(
    n = n(),
    # Since TRUE = Medical Condition, the mean gives the direct proportion of sick pets
    MedicalConditionRate = mean(HealthCondition), 
    .groups = 'drop'
  ) |> 
  ggplot(aes(x = AgeGroups, y = MedicalConditionRate, fill = PetType)) +
  geom_col(position = "dodge") + 
  ggtitle("Rate of Medical Conditions by Age Group and Pet Type") +
  xlab("Age Group") +
  ylab("Proportion with Medical Condition")
```

![](README_files/figure-gfm/unnamed-chunk-10-3.png)<!-- -->

``` r
# Are certain breeds more likely to have a medical condition?
pets |> 
  group_by(Breed) |> 
  summarise(
    n = n(),
    MedicalConditionRate = mean(HealthCondition)
  ) |> 
  arrange(desc(MedicalConditionRate)) |> 
  ggplot(aes(x = reorder(Breed, MedicalConditionRate), y = MedicalConditionRate)) +
  geom_col(fill = "indianred") +
  coord_flip() +
  ggtitle("Proportion of Pets with Medical Conditions by Breed") +
  xlab("Breed") +
  ylab("Proportion with Medical Condition")
```

![](README_files/figure-gfm/unnamed-chunk-10-4.png)<!-- -->

To investigate these questions, we plan on looking at boxplots,
histograms and scatter plots and perform linear regressions to identify
trends and correlations within the data.
