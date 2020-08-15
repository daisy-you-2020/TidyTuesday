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


#######################################
#                                     #
#         2020-08-04                  #
#         European Energy             #                
#                                     #
#######################################


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


#######################################
#                                     #
#         2020-08-11                  #
#         European Energy             #                
#                                     #
#######################################
#load this week's data
data = tt_load('2020-08-11')

#make tables
avatar = data$avatar
scene = data$scene_description

#explore dataset
summary(avatar)
summary(scene)

book_chapter_rating = avatar %>% distinct(book, chapter, imdb_rating) %>% group_by(book) %>% summarise(mean_rating = mean(imdb_rating, na.rm = TRUE), sd_rating = sd(imdb_rating, na.rm = TRUE)) %>% ungroup() # 3 book, ~20 chapters each


#plot
theme_set(theme_classic() + theme(legend.position = "top", plot.title.position = 'plot', plot.title = element_text(hjust = 0.5), axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)), axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0))))

ggplot(data = book_chapter_rating, aes(x = book, y = mean_rating, fill = book, color = book, alpha = 0.4)) +
  geom_col() +
  geom_errorbar(aes(ymin = mean_rating - sd_rating, ymax = mean_rating + sd_rating), width = 0.2, size = 1) +
  geom_text(aes(label = round(mean_rating, 1)), vjust = -5) +
  scale_y_continuous(breaks = seq(0, 10, 2), label = c('0', '2', '4', '6', '8', '10')) +
  guides(fill = FALSE, alpha = FALSE, color = FALSE) +
  labs(x = 'Avatar Books', y = 'IMDB Ratings', title = 'Ratings of Avatar')





