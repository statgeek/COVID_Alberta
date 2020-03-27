library(tidyverse)
library(readxl)
library(tibbletime)
library(directlabels)



#import raw data
confirmed <- read_csv(file="https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
deaths <- read_csv(file="https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")
recovered <-read_csv(file="https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv")
AB_data <- read_csv(file="https://raw.githubusercontent.com/statgeek/COVID_Alberta/master/ab_data.csv")


#flip to wide structure, clean up names, format variables to standardized
confirmed_long <- confirmed %>%
    pivot_longer(-c(`Province/State`,`Country/Region`, Lat, Long), names_to="Date", values_to="Confirmed" ) %>%
    rename(Country_Region = `Country/Region`, Province_State = `Province/State`) %>%
    #filter(Country_Region == 'Canada') %>%
    mutate(record_date = as.Date(Date, "%m/%d/%y", origin="01-01-1970")) %>%
    group_by(Country_Region, Province_State, record_date) %>%
    summarize(total_confirmed = sum(Confirmed))


deaths_long <- deaths %>%
    pivot_longer(-c(`Province/State`,`Country/Region`, Lat, Long), names_to="Date", values_to="Deaths" ) %>%
    rename(Country_Region = `Country/Region`, Province_State = `Province/State`) %>%
    #filter(Country_Region == 'Canada') %>%
    mutate(record_date = as.Date(Date, "%m/%d/%y", origin="01-01-1970")) %>%
    group_by(Country_Region, Province_State, record_date) %>%
    summarize(total_dead = sum(Deaths))

recovered_long <- recovered %>%
    pivot_longer(-c(`Province/State`,`Country/Region`, Lat, Long), names_to="Date", values_to="Recovered" ) %>%
    rename(Country_Region = `Country/Region`, Province_State = `Province/State`) %>%
    #filter(Country_Region == 'Canada') %>%
    mutate(record_date = as.Date(Date, "%m/%d/%y", origin="01-01-1970")) %>%
    group_by(Country_Region, Province_State, record_date) %>%
    summarize(total_recovered = sum(Recovered))



#merge data together
merged_data <- left_join(confirmed_long, deaths_long, by=c("Country_Region", "Province_State", "record_date")) %>%
               left_join(recovered_long, by=c("Country_Region", "Province_State", "record_date")) 

#get a list of countries
countries <- merged_data %>%
    filter(total_confirmed>=0) %>%
    group_by(Country_Region) %>%
    summarize(num_days = n())

#create list of countries to compare to
countries_compare <- c("Korea, South",
                       "Italy",
                       "France",
                       "Singapore",
                       "Japan", "Spain", "Canada", "China", "US", "Taiwan*")


#geta accurate AB data from my feed
Alberta_data <- AB_data %>%
    mutate(Country_Region = "Canada") %>%
    rename(Province_State = Region, record_date = Date, total_confirmed = Confirmed, total_dead = Deaths, total_recovered = Recovered) %>%
    filter(total_confirmed >= 10) %>%
    select(Country_Region, Province_State, record_date, total_confirmed, total_dead, total_recovered, NumberTests) %>%
    group_by(Country_Region) %>%
    mutate(day_count = row_number())

#get BC data alone
BC_data <- merged_data %>%
    filter(Country_Region == "Canada" & Province_State == "British Columbia" & total_confirmed>=10) %>%
    group_by(Country_Region, record_date) %>%
    group_by(Country_Region) %>%
    mutate(day_count = row_number())

#get comparison countries
country_comparisons_data <- merged_data %>%
    filter(Country_Region %in% countries_compare & total_confirmed>=10) %>%
    mutate(NumberTests = NA) %>%
    group_by(Country_Region, record_date) %>%
    summarize_at(c("total_confirmed", "total_dead", "total_recovered", "NumberTests"), sum, na.rm= TRUE) %>%
    ungroup() %>%
    group_by(Country_Region) %>%
    mutate(day_count = row_number())

#stack for reporting
reporting_data <- rbind(Alberta_data, BC_data, country_comparisons_data) %>% 
    mutate(reporting_region = coalesce(Province_State, Country_Region))

#graph it
ggplot(reporting_data, aes(x=day_count, y = total_confirmed, group=reporting_region, color = reporting_region)) +
         geom_line(size=1.5) +
        scale_colour_discrete(guide='none') +
         scale_y_continuous(trans='log10') +
       geom_dl(aes(label = reporting_region), method = list(dl.combine( "last.points"), cex=0.8))
