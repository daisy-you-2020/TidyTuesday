# set path to the data location
setwd("C:/Users/dyou/OneDrive - Daiichi Sankyo/Desktop/TidyTuesday")


#import library
library(xlsx)
library(dplyr)
library(purrr)
library(tidyr)
library(tidytuesdayR)
library(data.table)
library(ggplot2)
library(gganimate)


#load this week's data
tuesdata = tt_load('2020-08-04')
#make tables
energy = tuesdata$energy_types %>% filter(level != 'Level 2') %>% rename(e_2016 = `2016`, e_2017 = `2017`, e_2018 = `2018`) %>% mutate(country_name = ifelse(is.na(country_name), 'United Kingdom', country_name))
country = tuesdata$country_totals %>% rename(e_2016 = `2016`, e_2017 = `2017`, e_2018 = `2018`) %>% mutate(country_name = ifelse(is.na(country_name), 'United Kingdom', country_name))

#explore data
summary(energy) # UK is missing country name
summary(country) # 1 NA in 2016, UK is missing country name

#data processing
country = setDT(country)
country_ts = bind_rows(dcast(country, country ~type,  value.var = "e_2016") %>% mutate(year = '2016'), 
                       dcast(country, country ~type,  value.var = "e_2017") %>% mutate(year = '2017'),
                       dcast(country, country ~type,  value.var = "e_2018") %>% mutate(year = '2018'))
colnames(country_ts) = make.names(colnames(country_ts))




energy = setDT(energy)
energy_ts = bind_rows(dcast(energy, country ~type,  value.var = "e_2016") %>% mutate(year = '2016'), 
                       dcast(energy, country ~type,  value.var = "e_2017") %>% mutate(year = '2017'),
                       dcast(energy, country ~type,  value.var = "e_2018") %>% mutate(year = '2018'))
colnames(energy_ts) = make.names(colnames(energy_ts))
energy_ts = energy_ts %>% mutate(total = Conventional.thermal + Geothermal + Solar + Hydro + Wind + Nuclear + Other, renew = Geothermal + Solar + Hydro + Wind, conv_pct = round(Conventional.thermal/total*100, 0), ren_pct = round(renew/total*100, 0), nuc_pct = round(Nuclear/total*100,0))



#plot
theme_set(theme_classic() + theme(legend.position = "top", plot.title.position = 'plot', plot.title = element_text(hjust = 0.5)))

ggplot(data = energy_ts, aes(x = conv_pct)) +
  geom_histogram(aes(y=..density..), bins = 20, color = '#00AFBB', fill = '#00AFBB', alpha = 0.3)+
  facet_wrap(~year) +
  geom_density(color = '#00AFBB') +
  labs(x= 'Renewable Energy (%)', y = 'Number of countries', title = 'Distribution of Renewable Energy Production in European Countries')

ggplot(data = energy_ts, aes(x = reorder(country, ren_pct), y = ren_pct)) +
  geom_col(color = '#00AFBB', fill = '#00AFBB', alpha = 0.3) +
  coord_flip() +
  facet_wrap(~year)
  

#explore renewable energy supplied by country
ggplot(data = country_ts, aes(x = reorder(country, Energy.supplied), y = Energy.supplied)) + 
  geom_bar(stat = 'identity') + 
  coord_flip() +
  facet_wrap(~year)


ggplot(data, aes(x = specie, y = value, fill = condition)) +
  geom_bar(position = 'dodge', stat = 'identity')

