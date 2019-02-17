---
title: "Impact of severe weather events in the US in 1950-2011"
output:
  html_document:
    keep_md: yes
---



## Synopsys

Storms and other severe weather events can cause both public health and economic problems for communities and municipalities. Many severe events can result in fatalities, injuries, and property damage, and preventing such outcomes to the extent possible is a key concern.

This project involves exploring the U.S. National Oceanic and Atmospheric Administration's (NOAA) storm database and aims at exploring the correlation between event types and the magnitude of health and economic impact they bear.

## Loading and Processing the Raw Data

The data for this project comes from the U.S. National Oceanic and Atmospheric Administration's (NOAA) storm database.

There is also some documentation of the database available.

*[National Weather Service Storm Data Documentation](https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2Fpd01016005curr.pdf)
*[National Climatic Data Center Storm Events FAQ](https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2FNCDC%20Storm%20Events-FAQ%20Page.pdf)


```r
library(plyr)
library(dplyr)
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2", "storms.bz2")
storms <- tbl_df(read.csv("storms.bz2"))
```

First, let's look at the data


```r
storms
```

```
## # A tibble: 902,297 x 37
##    STATE__ BGN_DATE BGN_TIME TIME_ZONE COUNTY COUNTYNAME STATE EVTYPE
##      <dbl> <fct>    <fct>    <fct>      <dbl> <fct>      <fct> <fct> 
##  1       1 4/18/19~ 0130     CST           97 MOBILE     AL    TORNA~
##  2       1 4/18/19~ 0145     CST            3 BALDWIN    AL    TORNA~
##  3       1 2/20/19~ 1600     CST           57 FAYETTE    AL    TORNA~
##  4       1 6/8/195~ 0900     CST           89 MADISON    AL    TORNA~
##  5       1 11/15/1~ 1500     CST           43 CULLMAN    AL    TORNA~
##  6       1 11/15/1~ 2000     CST           77 LAUDERDALE AL    TORNA~
##  7       1 11/16/1~ 0100     CST            9 BLOUNT     AL    TORNA~
##  8       1 1/22/19~ 0900     CST          123 TALLAPOOSA AL    TORNA~
##  9       1 2/13/19~ 2000     CST          125 TUSCALOOSA AL    TORNA~
## 10       1 2/13/19~ 2000     CST           57 FAYETTE    AL    TORNA~
## # ... with 902,287 more rows, and 29 more variables: BGN_RANGE <dbl>,
## #   BGN_AZI <fct>, BGN_LOCATI <fct>, END_DATE <fct>, END_TIME <fct>,
## #   COUNTY_END <dbl>, COUNTYENDN <lgl>, END_RANGE <dbl>, END_AZI <fct>,
## #   END_LOCATI <fct>, LENGTH <dbl>, WIDTH <dbl>, F <int>, MAG <dbl>,
## #   FATALITIES <dbl>, INJURIES <dbl>, PROPDMG <dbl>, PROPDMGEXP <fct>,
## #   CROPDMG <dbl>, CROPDMGEXP <fct>, WFO <fct>, STATEOFFIC <fct>,
## #   ZONENAMES <fct>, LATITUDE <dbl>, LONGITUDE <dbl>, LATITUDE_E <dbl>,
## #   LONGITUDE_ <dbl>, REMARKS <fct>, REFNUM <dbl>
```

Formatting the column names


```r
names(storms)<- tolower(make.names(names(storms), unique = T, allow_ = F))
```


### Population damage by event type

The columns we are interested are "fatalities" and "injuries". The storm database contain only *direct* injuries, in other words the ones that can be directly attributed to the observed weather event. Let's look at the summary of those columns and see if there's any missing data.


```r
summary(storms$fatalities)
```

```
##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
##   0.0000   0.0000   0.0000   0.0168   0.0000 583.0000
```


```r
summary(storms$injuries)
```

```
##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
##    0.0000    0.0000    0.0000    0.1557    0.0000 1700.0000
```

### Economic damage by event type

The most interesting columns here would be "propdmg" and "cropdmg" with their value magnitutes in "propdmgexp" and "cropdmgexp" respectively.

Let's take a look at the property data first


```r
summary(storms$propdmg)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##    0.00    0.00    0.00   12.06    0.50 5000.00
```


```r
summary(storms$propdmgexp)
```

```
##             -      ?      +      0      1      2      3      4      5 
## 465934      1      8      5    216     25     13      4      4     28 
##      6      7      8      B      h      H      K      m      M 
##      4      5      1     40      1      6 424665      7  11330
```

Unfortunately we can see that not all the numbers are properly formatted as per NWS storm data documentation. In particular, letters "K", "M" and "B" mean 10^3, 10^6 and 10^9 magnifyers respectively, and we can assume that "H/h" would mean hundreds. We can also assume that numbers directly signify magnitude, and "", "-", "0" mean magnitude of 1. Rest of the symbols remain mysterious and demand further investigation, but seeing that they account for relatively small amount of observations we would ignore them for this research.


```r
storms <- storms %>%
        mutate(propdmg.m = with(., case_when(
        (propdmgexp == "h" | propdmgexp == "H")~2,
        (propdmgexp == "k" | propdmgexp == "K")~3,
        (propdmgexp == "m" | propdmgexp == "M")~6,
        (propdmgexp == "b" | propdmgexp == "B")~9,
        (propdmgexp %in% c("","0","-"))~1,
        (propdmgexp %in% as.character(1:9))~ as.numeric(as.character(propdmgexp)))))
```

Calculate the final loss in USD

```r
storms<- storms %>% mutate(propdmg.v = propdmg*10^propdmg.m)
```

We deal similarly with crops damage data


```r
storms <- storms %>%
        mutate(cropdmg.m = with(., case_when(
        (cropdmgexp == "h" | cropdmgexp == "H")~2,
        (cropdmgexp == "k" | cropdmgexp == "K")~3,
        (cropdmgexp == "m" | cropdmgexp == "M")~6,
        (cropdmgexp == "b" | cropdmgexp == "B")~9,
        (cropdmgexp %in% c("","0","-"))~1,
        (cropdmgexp %in% as.character(1:9))~ as.numeric(as.character(cropdmgexp)))))

storms<- storms %>% mutate(cropdmg.v = cropdmg*10^cropdmg.m)
```

##Results

### Health impact

For the health impact we will look at the top 10 event types that brought most fatalities and injuries to US throughout the history of observations. 


```r
storms.toph <- storms %>%
        group_by(evtype) %>%
        summarize(total = sum(fatalities,injuries)) %>%
        top_n(10) %>%
        arrange (desc(total))
storms.toph
```

```
## # A tibble: 10 x 2
##    evtype            total
##    <fct>             <dbl>
##  1 TORNADO           96979
##  2 EXCESSIVE HEAT     8428
##  3 TSTM WIND          7461
##  4 FLOOD              7259
##  5 LIGHTNING          6046
##  6 HEAT               3037
##  7 FLASH FLOOD        2755
##  8 ICE STORM          2064
##  9 THUNDERSTORM WIND  1621
## 10 WINTER STORM       1527
```
We can see that TORNADO was the most harmful type of event in the explored preiod.

### Economic impact

For the economic impact we are looking into amount of property and crop damage


```r
storms.econ <- storms %>%
        group_by(evtype) %>%
        summarize(totalprop = sum(propdmg.v), totalcrop = sum(cropdmg.v), total = sum(propdmg.v+cropdmg.v)) %>%
        top_n(10, wt = total) %>%
        arrange (desc(total))
```


```r
storms.econ
```

```
## # A tibble: 10 x 4
##    evtype               totalprop   totalcrop        total
##    <fct>                    <dbl>       <dbl>        <dbl>
##  1 FLOOD             144657709870  5661968450 150319678320
##  2 HURRICANE/TYPHOON  69305840000  2607872800  71913712800
##  3 STORM SURGE        43323536000        5000  43323541000
##  4 DROUGHT             1046106000 13972566000  15018672000
##  5 HURRICANE          11868319010  2741910000  14610229010
##  6 RIVER FLOOD         5118945500  5029459000  10148404500
##  7 ICE STORM           3944928310  5022113500   8967041810
##  8 TROPICAL STORM      7703890550   678346000   8382236550
##  9 WINTER STORM        6688497260    26944000   6715441260
## 10 WILDFIRE            4765114000   295472800   5060586800
```

We can see that for crop the most damaging event is DROUGHT

And for property respectively:

```r
storms.econ$evtype[which.max(storms.econ$totalprop)]
```

```
## [1] FLOOD
## 985 Levels:    HIGH SURF ADVISORY  COASTAL FLOOD ... WND
```

However for total economic impact we are looking at overall cost


```r
library(ggplot2)
ggplot(data=storms.econ, aes(x=reorder(evtype, -total), y=total/1000000000)) + 
        geom_bar(stat="identity") + 
        labs(title="Weather event types associated with most property and crop damage", 
                y="Loss, bln USD", 
                x = "Event Type") + 
        theme(axis.text.x = element_text(angle = 90, vjust=0.5))
```

![](storms_files/figure-html/economy.plot-1.png)<!-- -->



