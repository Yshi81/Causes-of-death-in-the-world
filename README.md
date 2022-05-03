# Causes-of-death-in-the-world
Module project of PSY6422

## Data sources  
The data is sourced from a page on [Our world in data](https://ourworldindata.org/causes-of-death). There are five files of data that I downloaded. The author of the page has divided the raw data into five groups by age, including under 5, 5-14, 15-49, 50-69, and 70+. The columns for each group include Entity, Code of country, year, and a number of deaths by cause of death. There are four variables in total: age, year, region, and cause of death.  
These data are collected from a number of institutions, such as, [Institute of Health Metrics and Evaluation (IHME),Global Burden of Disease (GBD)](https://ghdx.healthdata.org/gbd-results-tool), [World Health Organization (WHO) Global Health Observatory (GHO)](https://www.who.int/data/gho/data/themes/mortality-and-global-health-estimates), [Global Terrorism Database (GTD)](http://www.start.umd.edu/gtd/), [Amnesty International](https://www.amnesty.org/en/what-we-do/death-penalty/)
## Research Questions
In the epidemiological framework of the Global Burden of Disease Study, each death has a specific cause. In their own words. Every death is attributed to a single underlying cause - the cause that initiates the sequence of events that lead to death. Analysis of the causes of death may be useful in identifying risk factors or effective preventive measures to reduce the number of deaths.  
According to the official website, approximately 56 million people die each year. This report analyses change over time in the number of deaths caused by different causes of death. It also looks at regional and age differences. In other words, the causes of death are different for people of different ages and in different regions, and the changes over time are also different. This report presents some of the results of this analysis.
## Data Preparation

```
#loading data  
library(here)
here()
data_under_5 <- read.csv(here("raw","under-5.csv"))
data_5_14 <-read.csv(here("raw","5-14.csv"))
data_15_49 <- read.csv(here("raw","15-49.csv"))
data_50_69 <- read.csv(here("raw","50-69.csv"))
data_70_ <- read.csv(here("raw","70-.csv"))

#clear names#
names(data_under_5) <-  names(data_under_5) %>%
  sub('Deaths...','',.) %>%
  sub('...Sex..Both...Age..Under.5..Number.','',.) %>% 
  sub('..iNTS.','',.) %>% 
  gsub('[.]','_',.) 
  
names(data_5_14) <-  names(data_5_14) %>% 
  sub('Deaths...','',.) %>% 
  sub('...Sex..Both...Age..5.14.years..Number.','',.) %>% 
  gsub('[.]','_',.) %>% 
  gsub('__','_',.)

names(data_15_49) <-  names(data_15_49) %>%
  sub('Deaths...','',.) %>%
  sub('...Sex..Both...Age..15.49.years..Number.','',.) %>% 
  gsub('[.]','_',.) %>% 
  gsub('__','_',.)

names(data_50_69) <-  names(data_50_69) %>% 
  sub('Deaths...','',.) %>% 
  sub('...Sex..Both...Age..50.69.years..Number.','',.) %>% 
  gsub('[.]','_',.) %>% 
  gsub('__','_',.)

names(data_70_) <-  names(data_70_) %>% 
  sub('Deaths...','',.) %>% 
  sub('...Sex..Both...Age..70..years..Number.','',.) %>% 
  gsub('[.]','_',.) %>% 
  gsub('__','_',.)

#add age column#
data_under_5$Age <- "0-5"
data_5_14$Age <- "05-14"
data_15_49$Age <- "15-49"
data_50_69$Age <- "50-69"
data_70_$Age <- "70_"

#combine all data frames#
if (!require('dplyr')) install.packages('dplyr')
library(dplyr)
data_all <- data_under_5 %>% 
  full_join(.,data_5_14) %>% 
  full_join(.,data_15_49) %>% 
  full_join(.,data_50_69) %>% 
  full_join(.,data_70_)

#fill in missing values#
data_all[is.na(data_all)] <- 0

#Adjust the order of columns
data_all <- subset(data_all, select=c(33,1:32,34:41))  
```
