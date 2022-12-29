Bad Drivers Project - Part 1
================
Franceska Padilla Coo, Stephanie Fissel, Tatev Gomtsyan,
October 9, 2022

## Workspace Prep

- Data source: [Bad
  Drivers](https://github.com/fivethirtyeight/data/blob/master/bad-drivers/bad-drivers.csv)

# Load Packages

``` r
library(tidyverse)
library(ggplot2)
library(tibble)
```

# Read in Data

``` r
bad_drivers <- read.csv("/Users/stephaniefissel/Documents/GitHub/ds2003Midterm/bad-drivers.csv")
states_w_regions <- read.csv("/Users/stephaniefissel/Documents/GitHub/ds2003Midterm/us_states_w_regions.csv")
```

# Datasets

***Bad Drivers*** <br/> A dataset with information from the National
Highway Traffic Safety Administration and the National Association of
Insurance Commissioners from 2009 to 2012. Data includes fatal
collisions due to certain causes, car insurance premiums, and losses
incurred by insurance companies for collisions.

``` r
head(bad_drivers)
```

    ##        State Number.of.drivers.involved.in.fatal.collisions.per.billion.miles
    ## 1    Alabama                                                             18.8
    ## 2     Alaska                                                             18.1
    ## 3    Arizona                                                             18.6
    ## 4   Arkansas                                                             22.4
    ## 5 California                                                             12.0
    ## 6   Colorado                                                             13.6
    ##   Percentage.Of.Drivers.Involved.In.Fatal.Collisions.Who.Were.Speeding
    ## 1                                                                   39
    ## 2                                                                   41
    ## 3                                                                   35
    ## 4                                                                   18
    ## 5                                                                   35
    ## 6                                                                   37
    ##   Percentage.Of.Drivers.Involved.In.Fatal.Collisions.Who.Were.Alcohol.Impaired
    ## 1                                                                           30
    ## 2                                                                           25
    ## 3                                                                           28
    ## 4                                                                           26
    ## 5                                                                           28
    ## 6                                                                           28
    ##   Percentage.Of.Drivers.Involved.In.Fatal.Collisions.Who.Were.Not.Distracted
    ## 1                                                                         96
    ## 2                                                                         90
    ## 3                                                                         84
    ## 4                                                                         94
    ## 5                                                                         91
    ## 6                                                                         79
    ##   Percentage.Of.Drivers.Involved.In.Fatal.Collisions.Who.Had.Not.Been.Involved.In.Any.Previous.Accidents
    ## 1                                                                                                     80
    ## 2                                                                                                     94
    ## 3                                                                                                     96
    ## 4                                                                                                     95
    ## 5                                                                                                     89
    ## 6                                                                                                     95
    ##   Car.Insurance.Premiums....
    ## 1                     784.55
    ## 2                    1053.48
    ## 3                     899.47
    ## 4                     827.34
    ## 5                     878.41
    ## 6                     835.50
    ##   Losses.incurred.by.insurance.companies.for.collisions.per.insured.driver....
    ## 1                                                                       145.08
    ## 2                                                                       133.93
    ## 3                                                                       110.35
    ## 4                                                                       142.39
    ## 5                                                                       165.63
    ## 6                                                                       139.91

***States and their Regions*** <br/> A dataset with all US states, their
codes, and respective regions and divisions.

``` r
head(states_w_regions)
```

    ##        State State.Code Region           Division
    ## 1     Alaska         AK   West            Pacific
    ## 2    Alabama         AL  South East South Central
    ## 3   Arkansas         AR  South West South Central
    ## 4    Arizona         AZ   West           Mountain
    ## 5 California         CA   West            Pacific
    ## 6   Colorado         CO   West           Mountain

# Report Objectives

1.  Determining which states/geographic regions have the highest number
    of car accidents
2.  Determining if there is a relationship between insurance company
    losses and driver insurance premiums

``` r
bad_drivers['state_areas'] <- c(50750, 570641, 113642, 52075, 155973, 103730, 4845, 1955, 68.32, 53997, 57919, 6423, 82751, 55593, 35870, 55875, 81823, 39732, 43566, 30865, 9775, 7838, 56539, 79617, 46914, 68898, 145556, 76878, 109806, 8969, 7419, 121365, 47224, 48718, 68994, 40953, 68679, 96003, 44820, 1034, 30111, 75898, 41220, 261914, 83168, 9249, 39598, 66582, 24087, 54314, 97105)
colnames(bad_drivers)
```

    ## [1] "State"                                                                                                 
    ## [2] "Number.of.drivers.involved.in.fatal.collisions.per.billion.miles"                                      
    ## [3] "Percentage.Of.Drivers.Involved.In.Fatal.Collisions.Who.Were.Speeding"                                  
    ## [4] "Percentage.Of.Drivers.Involved.In.Fatal.Collisions.Who.Were.Alcohol.Impaired"                          
    ## [5] "Percentage.Of.Drivers.Involved.In.Fatal.Collisions.Who.Were.Not.Distracted"                            
    ## [6] "Percentage.Of.Drivers.Involved.In.Fatal.Collisions.Who.Had.Not.Been.Involved.In.Any.Previous.Accidents"
    ## [7] "Car.Insurance.Premiums...."                                                                            
    ## [8] "Losses.incurred.by.insurance.companies.for.collisions.per.insured.driver...."                          
    ## [9] "state_areas"

``` r
states_w_regions <- states_w_regions[order(states_w_regions$State), ]
bad_drivers['regions'] <- states_w_regions['Region']
bad_drivers['State'] <- states_w_regions['State.Code']
```

# Descriptive Statistics

## **SUMMARY STATS**

``` r
bad_drivers1 <- subset(bad_drivers, select = -c(Percentage.Of.Drivers.Involved.In.Fatal.Collisions.Who.Were.Speeding,
                                                Percentage.Of.Drivers.Involved.In.Fatal.Collisions.Who.Were.Alcohol.Impaired,
                                                Percentage.Of.Drivers.Involved.In.Fatal.Collisions.Who.Were.Not.Distracted,
                                                Percentage.Of.Drivers.Involved.In.Fatal.Collisions.Who.Had.Not.Been.Involved.In.Any.Previous.Accidents))

colnames(bad_drivers1)[2] ="Number of Drivers Involved in Fatal Collisions per Billion Miles"
colnames(bad_drivers1)[3] ="Car Insurance Premiums"
colnames(bad_drivers1)[4] ="Losses Incurred by Insurance Companies for Collisions per Insured Driver"

#install.packages("vtable")
library(vtable)
st(bad_drivers1)
```

<table class="table" style="margin-left: auto; margin-right: auto;">
<caption>
Summary Statistics
</caption>
<thead>
<tr>
<th style="text-align:left;">
Variable
</th>
<th style="text-align:left;">
N
</th>
<th style="text-align:left;">
Mean
</th>
<th style="text-align:left;">
Std. Dev.
</th>
<th style="text-align:left;">
Min
</th>
<th style="text-align:left;">
Pctl. 25
</th>
<th style="text-align:left;">
Pctl. 75
</th>
<th style="text-align:left;">
Max
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
Number of Drivers Involved in Fatal Collisions per Billion Miles
</td>
<td style="text-align:left;">
51
</td>
<td style="text-align:left;">
15.79
</td>
<td style="text-align:left;">
4.122
</td>
<td style="text-align:left;">
5.9
</td>
<td style="text-align:left;">
12.75
</td>
<td style="text-align:left;">
18.5
</td>
<td style="text-align:left;">
23.9
</td>
</tr>
<tr>
<td style="text-align:left;">
Car Insurance Premiums
</td>
<td style="text-align:left;">
51
</td>
<td style="text-align:left;">
886.958
</td>
<td style="text-align:left;">
178.296
</td>
<td style="text-align:left;">
641.96
</td>
<td style="text-align:left;">
768.43
</td>
<td style="text-align:left;">
1007.945
</td>
<td style="text-align:left;">
1301.52
</td>
</tr>
<tr>
<td style="text-align:left;">
Losses Incurred by Insurance Companies for Collisions per Insured Driver
</td>
<td style="text-align:left;">
51
</td>
<td style="text-align:left;">
134.493
</td>
<td style="text-align:left;">
24.836
</td>
<td style="text-align:left;">
82.75
</td>
<td style="text-align:left;">
114.645
</td>
<td style="text-align:left;">
151.87
</td>
<td style="text-align:left;">
194.78
</td>
</tr>
<tr>
<td style="text-align:left;">
state_areas
</td>
<td style="text-align:left;">
51
</td>
<td style="text-align:left;">
69359.575
</td>
<td style="text-align:left;">
85551.662
</td>
<td style="text-align:left;">
68.32
</td>
<td style="text-align:left;">
33367.5
</td>
<td style="text-align:left;">
80720
</td>
<td style="text-align:left;">
570641
</td>
</tr>
<tr>
<td style="text-align:left;">
regions
</td>
<td style="text-align:left;">
51
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
… Midwest
</td>
<td style="text-align:left;">
12
</td>
<td style="text-align:left;">
23.5%
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
… Northeast
</td>
<td style="text-align:left;">
9
</td>
<td style="text-align:left;">
17.6%
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
… South
</td>
<td style="text-align:left;">
17
</td>
<td style="text-align:left;">
33.3%
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
… West
</td>
<td style="text-align:left;">
13
</td>
<td style="text-align:left;">
25.5%
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
</tr>
</tbody>
</table>

## **BOXPLOTS**

``` r
#boxplots
boxplot(bad_drivers$Number.of.drivers.involved.in.fatal.collisions.per.billion.miles,
        main = "Number of Drivers Involved in Fatal Collisions per Billion Miles")
```

![](bad-drivers-part1_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
boxplot(bad_drivers$Car.Insurance.Premiums....,
        main = "Car Insurance Premiums")
```

![](bad-drivers-part1_files/figure-gfm/unnamed-chunk-7-2.png)<!-- -->

``` r
boxplot(bad_drivers$Losses.incurred.by.insurance.companies.for.collisions.per.insured.driver....,
        main = "Losses Incurred by Insurance Companies for Collisions per Insured Driver")
```

![](bad-drivers-part1_files/figure-gfm/unnamed-chunk-7-3.png)<!-- -->

``` r
bad_drivers1 <- subset(bad_drivers, select = -c(Percentage.Of.Drivers.Involved.In.Fatal.Collisions.Who.Were.Speeding,
                                                Percentage.Of.Drivers.Involved.In.Fatal.Collisions.Who.Were.Alcohol.Impaired,
                                                Percentage.Of.Drivers.Involved.In.Fatal.Collisions.Who.Were.Not.Distracted,
                                                Percentage.Of.Drivers.Involved.In.Fatal.Collisions.Who.Had.Not.Been.Involved.In.Any.Previous.Accidents))

colnames(bad_drivers1)[2] ="Number of Drivers Involved in Fatal Collisions per Billion Miles"
colnames(bad_drivers1)[3] ="Car Insurance Premiums"
colnames(bad_drivers1)[4] ="Losses Incurred by Insurance Companies for Collisions per Insured Driver"
```

## **SUMMARY OF NUMBER OF DRIVERS INVOLVED IN FATAL COLLISIONS PER BILLION MILES**

``` r
summary_stats1 <- summarise(bad_drivers, mean=mean(Number.of.drivers.involved.in.fatal.collisions.per.billion.miles, na.rm=TRUE),
                           ymin=min(Number.of.drivers.involved.in.fatal.collisions.per.billion.miles, na.rm=TRUE),
                           lower=quantile(Number.of.drivers.involved.in.fatal.collisions.per.billion.miles, 0.25),
                           ymax=max(Number.of.drivers.involved.in.fatal.collisions.per.billion.miles, na.rm=TRUE),
                           upper=quantile(Number.of.drivers.involved.in.fatal.collisions.per.billion.miles, 0.75))

summary_stats1
```

    ##      mean ymin lower ymax upper
    ## 1 15.7902  5.9 12.75 23.9  18.5

``` r
ggplot(summary_stats1, aes(x=1))+
  geom_boxplot(aes(ymin=ymin, lower=lower, 
                   middle=mean, upper=upper, ymax=ymax),
               stat = "identity")
```

![](bad-drivers-part1_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

## **SUMMARY FOR CAR INSURANCE PREMIUMS**

``` r
summary_stats2 <- summarise(bad_drivers, mean=mean(Car.Insurance.Premiums...., na.rm=TRUE),
                           ymin=min(Car.Insurance.Premiums...., na.rm=TRUE),
                           lower=quantile(Car.Insurance.Premiums...., 0.25),
                           ymax=max(Car.Insurance.Premiums...., na.rm=TRUE),
                           upper=quantile(Car.Insurance.Premiums...., 0.75))
summary_stats2
```

    ##       mean   ymin  lower    ymax    upper
    ## 1 886.9576 641.96 768.43 1301.52 1007.945

``` r
ggplot(summary_stats2, aes(x=1))+
  geom_boxplot(aes(ymin=ymin, lower=lower, 
                   middle=mean, upper=upper, ymax=ymax),
               stat = "identity")
```

![](bad-drivers-part1_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

## **SUMMARY FOR LOSSES INCURRED BY INSURANCE COMPANIES FOR COLLISIONS PER INSURED DRIVER**

``` r
summary_stats3 <- summarise(bad_drivers, mean=mean(Losses.incurred.by.insurance.companies.for.collisions.per.insured.driver...., na.rm=TRUE),
                           ymin=min(Losses.incurred.by.insurance.companies.for.collisions.per.insured.driver...., na.rm=TRUE),
                           lower=quantile(Losses.incurred.by.insurance.companies.for.collisions.per.insured.driver...., 0.25),
                           ymax=max(Losses.incurred.by.insurance.companies.for.collisions.per.insured.driver...., na.rm=TRUE),
                           upper=quantile(Losses.incurred.by.insurance.companies.for.collisions.per.insured.driver...., 0.75))
summary_stats3
```

    ##       mean  ymin   lower   ymax  upper
    ## 1 134.4931 82.75 114.645 194.78 151.87

``` r
ggplot(summary_stats3, aes(x=1))+
  geom_boxplot(aes(ymin=ymin, lower=lower, 
                   middle=mean, upper=upper, ymax=ymax),
               stat = "identity")
```

![](bad-drivers-part1_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

# Objective 1

**Understanding the number of drivers involved in fatal collisions per
billion miles by state and region**

## **Results (by State)**

``` r
colnames(bad_drivers)[2] = "Number of drivers involved in fatal collisions per billion miles"

question1 <- ggplot(bad_drivers, aes(x=State, y=`Number of drivers involved in fatal collisions per billion miles`))+
  geom_col(aes(fill=regions))+
  scale_x_discrete(guide = guide_axis(n.dodge=3))+
  labs(title='Number of fatal Collisions per Billion Miles per State', x='State Code')
question1
```

![](bad-drivers-part1_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

## **Results (by Region)**

``` r
byregion<- ggplot(bad_drivers, aes(x=regions, y=`Number of drivers involved in fatal collisions per billion miles`))+
  geom_col(aes(fill=regions))+
  scale_x_discrete(guide = guide_axis(n.dodge=3))+
  labs(title='Number of fatal Collisions per Billion Miles per Region', x='Region')
byregion
```

![](bad-drivers-part1_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

> Southern Region leads in number of drivers involved in fatal
> collisions per billion miles

``` r
ggsave('AccidentsVSState', plot=question1, device='png')
ggsave('AccidentsVSRegion', plot=byregion, device='png')
```

# Objective 2

**Determining if there is a relationship between insurance company
losses and driver insurance premiums**

``` r
colnames(bad_drivers)[8] = "Losses incurred by insurance companies for collisions per insured driver"
colnames(bad_drivers)[7] = "Car Insurance Premiums"
question2 <- ggplot(bad_drivers, aes(x=`Losses incurred by insurance companies for collisions per insured driver`, y=`Car Insurance Premiums`, color=regions))+
  geom_point()+
  geom_smooth(method='lm', se=FALSE)
  
question2
```

![](bad-drivers-part1_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

> Strong correlation found between premiums and losses in Midwest Region

Midwest region had the strongest correlation because of how tightly
bound those data points are to the line compared to the northeastern
region where even though it’s strongly positive, the points are very
scattered and the r^2 continues to get further from the line the greater
the losses are

``` r
ggsave('PremiumsAndLosses', plot=question2, device='png')
```
