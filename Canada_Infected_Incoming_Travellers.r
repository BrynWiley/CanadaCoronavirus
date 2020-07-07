#Made by Bryn Wiley - June 2020
#Using previously specified Statistics Canada and World in Data data, calculates an 
#expected value of persons from a selected number of countries entering Canada infected
#with covid-19

library(tidyverse)
library(lubridate)
library(ggplot2)
require(scales)

#This is data from "Our World in Data" containing estimates for Covid-19 infection rates
#in countries around the world. We estimate, for each day between January and April, the
#total number of active cases by summing the total number of active cases from the past
#14 days. Missing data is ignored, and no strategies are used to account for testing.

Infected_Counts <- read_csv("owid-covid-data.csv") %>%
  filter(date >= as_date("2020-01-01") & date <= as_date("2020-04-30")) %>%
  rename(country = location) %>%
  filter(country != "Canada" & country != "World") %>%
  mutate(day = as.integer(as_date(date) - as_date("2019-12-31")))%>%
  mutate(active_cases=0) %>%
  group_by(country) %>% 
  group_modify(function(X=.x,Y=.y){
    active <- NULL
    for(i in X$day){
      num_less_or_equal <- sum(X$day <=i)
      if(num_less_or_equal <=14){
        active <- c(active,sum(X$new_cases[X$day<=i],na.rm=TRUE))
      } else {
        active <- c(active,sum(X$new_cases[X$day<=i & X$day >=(i-13)],na.rm=TRUE))
      }
    }
    X$active_cases=active
    return(X)
  }) %>%
  select(active_cases,population,continent,date,country) %>%
  rowwise() %>%
  mutate(country=if_else(country=="Congo","Democratic Republic of the Congo",country))




#This is data containing an estimated total number of international travellers into Canada
#per each month from January-April 2020. This includes both residents and non-residents entering
#Canada. Trip origin in divided between the US and the rest of the world.

International_Trips <- rbind(read_csv("International Trips - April.csv"),
                             read_csv("International Trips - February.csv"),
                             read_csv("International Trips - January.csv"),
                             read_csv("International Trips - March.csv")) %>%
  mutate(REF_DATE = as_date(paste(REF_DATE,"-01",sep="")))

#Trips from the US per day are estimated first. For each month except March, travellers per day
#are estimated by dividing the total number of travellers per month by the number of days in 
#the month. However, on March 21st, travel between the US and Canada was severely restricted. As such, the 
#number of travellers per day from March 21st-31st is set the same as any day in April, and the 
#number of travellers per day in the rest of March is scaled such that the total number of travellers
#in March matches the data from Statistics Canada
US_Travellers <- International_Trips %>%
  rename(province=GEO,date=REF_DATE) %>%
  filter(`Traveller characteristics`=="United states Residents Entering Canada" | `Traveller characteristics` == "Canadian residents returning from the United States", province != "Canada") %>%
  group_by(province,date) %>%
  summarize(VALUE=sum(VALUE)) %>%
  mutate(Num_Travellers_Per_Day = -1) %>%
  select(province,date,Num_Travellers_Per_Day,VALUE) %>% 
  group_by(province) %>%
  group_modify(function(X=.x,Y=.y){
    orig_dates <- as_date(X$date)
    for(i in orig_dates){
      i <- as_date(i)
      if(month(i) != 3){
        X$Num_Travellers_Per_Day[X$date == i] <- X$VALUE[X$date == i]/days_in_month(i)
        for(j in 1:(days_in_month(i)-1)) {
          date = as_date(i + as.integer(j))
          Num_Travellers_Per_Day = X$VALUE[X$date == i]/days_in_month(date)
          VALUE=X$VALUE[X$date == i]
          X <- add_row(X,date=date,VALUE=VALUE,Num_Travellers_Per_Day=Num_Travellers_Per_Day)
        }
      } else {
        y=sum(X$VALUE[X$date == as_date("2020-04-01")])/30
        for(j in 20:30){
          date=as_date(i + as.integer(j))
          VALUE=X$VALUE[X$date == i]
          X <- add_row(X,date=date,VALUE=VALUE,Num_Travellers_Per_Day=y)
        }
        x= (X$VALUE[X$date == i]-11*y)/20
        X$Num_Travellers_Per_Day[X$date == i] <- x
        for(j in 1:19){
          date=as_date(i+as.integer(j))
          VALUE=X$VALUE[X$date==i]
          X <- add_row(X,date=date,VALUE=VALUE,Num_Travellers_Per_Day=x)
        }
      }
    }
    return(X)
  }) %>% mutate(country= "United States") %>%
  select(province,date,Num_Travellers_Per_Day,country)

#Trips from the rest of the world are estimated next. First, the estimated number of international non-US originating travellers 
#entering Canada per day is calculated in a similar fashion to US travellers. However, for non-US travellers
#travel was severely restricted into Canada on March 18th.

Non_Us_Travellers <- International_Trips %>%
  rename(province=GEO,date=REF_DATE) %>%
  filter(`Traveller characteristics`=="Residents of countries other than United States entering Canada" | `Traveller characteristics`=="Canadian residents returning from countries other than the United States",
         province != "Canada") %>%
  group_by(province,date) %>%
  summarize(VALUE=sum(VALUE)) %>%
  mutate(Num_Travellers_Per_Day = -1) %>%
  select(province,date,Num_Travellers_Per_Day,VALUE) %>% 
  group_by(province) %>%
  group_modify(function(X=.x,Y=.y){
    orig_dates <- as_date(X$date)
    for(i in orig_dates){
      i <- as_date(i)
      if(month(i) != 3){
        X$Num_Travellers_Per_Day[X$date == i] <- X$VALUE[X$date == i]/days_in_month(i)
        for(j in 1:(days_in_month(i)-1)) {
          date = as_date(i + as.integer(j))
          Num_Travellers_Per_Day = X$VALUE[X$date == i]/days_in_month(date)
          VALUE=X$VALUE[X$date == i]
          X <- add_row(X,date=date,VALUE=VALUE,Num_Travellers_Per_Day=Num_Travellers_Per_Day)
        }
      } else {
        y=sum(X$VALUE[X$date == as_date("2020-04-01")])/30
        for(j in 17:30){
          date=as_date(i + as.integer(j))
          VALUE=X$VALUE[X$date == i]
          X <- add_row(X,date=date,VALUE=VALUE,Num_Travellers_Per_Day=y)
        }
        x= (X$VALUE[X$date == i]-14*y)/17
        X$Num_Travellers_Per_Day[X$date == i] <- x
        for(j in 1:16){
          date=as_date(i+as.integer(j))
          VALUE=X$VALUE[X$date==i]
          X <- add_row(X,date=date,VALUE=VALUE,Num_Travellers_Per_Day=x)
        }
      }
    }
    return(X)
  }) %>%
  select(province,date,Num_Travellers_Per_Day)

#Next, the number or travellers from each country is estimated. Statistics Canada does not have this data,
#but does contain the estimated number of non-Canadian residents from each country entering Canada. This does not
#contain Canadian residents returning from abroad, so the fraction of non-residents entering Canada each month from each
#country is multiplied by the number of non-US travellers entering each day to obtain an estimate of the number of 
#travellers from each country entering Canada each day.

Non_Resident_Monthly_Totals <- read_csv("NonresidentsEnteringByCountry.csv") %>%
  mutate(REF_DATE = as_date(paste(REF_DATE,"-01",sep=""))) %>%
  rename(date=REF_DATE) %>%
  rename(province=GEO) %>%
  group_by(date,province) %>%
  summarize(sum=sum(VALUE)) %>%
  filter(sum > 0) #This is necessary to eliminate NaN in the next block.
  


Non_Us_Travellers <- read_csv("NonresidentsEnteringByCountry.csv") %>%
  mutate(REF_DATE = as_date(paste(REF_DATE,"-01",sep=""))) %>%
  rename(date=REF_DATE) %>%
  rename(country=`Country of residence`) %>%
  rename(province=GEO) %>%
  inner_join(Non_Resident_Monthly_Totals) %>%
  mutate(proportion = -1) %>%
  select(country,VALUE,sum,date,proportion,province) %>%
  group_by(country,province) %>%
  group_modify(function(X=.x,Y=.y){
    orig_dates <- as_date(X$date)
    for(i in orig_dates){
      i <- as_date(i)
      if(month(i) != 3){
        X$proportion[X$date == i] <- X$VALUE[X$date == i]/X$sum[X$date == i]
        for(j in 1:(days_in_month(i)-1)) {
          date = as_date(i + as.integer(j))
          sum=X$sum[X$date == i]
          proportion = X$VALUE[X$date == i]/X$sum[X$date == i]
          VALUE=X$VALUE[X$date == i]
          X <- add_row(X,VALUE=VALUE,date=date,sum=sum,proportion=proportion)
        }
      } else {
        VALUE = X$VALUE[X$date == as_date("2020-04-01")]
        sum = X$sum[X$date == as_date("2020-04-01")]
        proportion = VALUE/sum
        for(j in 17:30){
          date=as_date(i + as.integer(j))
          VALUE=X$VALUE[X$date == i]
          X <- add_row(X,VALUE=VALUE,date=date,sum=sum,proportion=proportion)
        }
        VALUE + X$VALUE[X$date == i]
        sum= X$sum[X$date == i]
        proportion = VALUE/sum
        X$proportion[X$date == i] = proportion
        for(j in 1:16){
          date=as_date(i + as.integer(j))
          VALUE=X$VALUE[X$date == i]
          X <- add_row(X,VALUE=VALUE,date=date,sum=sum,proportion=proportion)
        }
      }
    }
    return(X)
  }) %>%
  inner_join(Non_Us_Travellers) %>%
  mutate(Num_Travellers_Per_Day=Num_Travellers_Per_Day*proportion) %>%
  select(province,date,Num_Travellers_Per_Day,country)

#US and Non-US traveller counts are then joined, and country names are standardized
International_Travellers <- rbind(US_Travellers,Non_Us_Travellers)%>%
  select(date, Num_Travellers_Per_Day,province,country)%>%
  mutate(country=if_else(country=="Russian Federation","Russia",country)) %>%
  mutate(country=if_else(country=="Viet Nam","Vietnam",country)) %>%
  mutate(country=if_else(country=="Congo, Democratic Republic of the","Democratic Republic of the Congo",country)) %>%
  mutate(country=if_else(country=="South Africa, Republic of","South Africa",country)) %>%
  mutate(country=if_else(country=="Korea, South", "South Korea", country))

#Next, travellers per day from each country and infected counts of each country are combined to give
#an estimated expected value of number of infected travellers entering Canada per day. This is done simply
#by dividing active cases by population to give fraction infected per country, and by multiplying this fraction
#by the daily number of travellers entering Canda from each country.

#First, we assume that if a country does not have a record of active cases on a date, the number
#of cases in the country at that time is 0.
Infected_Counts<- Infected_Counts %>%
  filter(country %in% International_Travellers$country) %>% #This is done to cut down unecessary calculations
  group_by(country) %>%
  group_modify(function(X=.x,Y=.y){
    required_dates <- as_date(as_date("2020-01-01"):as_date("2020-04-01"))
    needed_dates <- as_date(setdiff(required_dates,X$date))
    for(j in needed_dates){
      X <- X %>% add_row(active_cases=0,population=X$population[1],continent=X$continent[1],
                         date=as_date(j))
    }
    return(X)
  }) %>%
  mutate(Fraction_Infected = active_cases/population)

#Then, we put it all together
Canada_Infected_Travellers <- inner_join(Infected_Counts,International_Travellers) %>%
  select(country,date,Fraction_Infected,Num_Travellers_Per_Day,province,continent)%>%
  rowwise() %>%
  mutate(Expected_Infected_Entering = Fraction_Infected*Num_Travellers_Per_Day)

#and make the graph
Canada_Infected_Travellers %>%
  group_by(continent,date) %>%
  summarise(Expected = sum(Expected_Infected_Entering)) %>%
  filter(Expected != 0) %>%
  ggplot(aes(x=date,y=(Expected),color=factor(continent))) + geom_line(size=2) + theme_bw() +
  scale_color_brewer(palette="Dark2") + ylab("Expected number of infected entering Canada") + labs(color="Continent") +
  scale_y_continuous(trans = log10_trans(),
                     breaks = trans_breaks("log10", function(x) 10^x),
                     labels = trans_format("log10", math_format(10^.x)))
