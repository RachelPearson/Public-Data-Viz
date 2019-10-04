#--------------------------------------------#
# Welsh infant entry to care                 #
#--------------------------------------------#

#-------------------#
# set up  ----
#-------------------#

# packages:
library(readr)
library(readxl)
library(tidyr)
library(dplyr)
library(tibble)
library(janitor)
library(snakecase)
library(purrr)
library(ggplot2)



#-------------------#
# get data files ----
#-------------------#

# download file from: https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/populationestimatesforukenglandandwalesscotlandandnorthernireland/mid2001tomid2017detailedtimeseries

setwd("YOUR FILEPATH")
pop_dat <- read.csv("mid_year_population_estimates_series_UK_2000_to_2017.csv", header = TRUE) %>% 
  gather(key = variables, value = value, population_2001:population_2017) %>%  # transform data from wide to long
  filter(country == "W") %>%  # welsh pop estimates 
  clean_names() %>% 
  mutate(year = as.numeric(substr(variables, nchar(variables)-3, nchar(variables))) + 1) %>% 
  filter(age == 0) %>% # under 1s
  group_by(year) %>% 
  summarise(pop_U1 = sum(value)) %>%  # sum by year to create under 1 pop estimate for Wales for all years
  ungroup() 



# download cleaned welsh cla data from github
# https://github.com/RachelPearson/Public-Data-Viz/blob/master/UK/Wales/clean_wales_CLA_31MAR_2003_2018.rds

# create counts of infants in care in Wales (as at 31st March), by year:
cla_wales_clean_U1 <- readRDS("clean_wales_CLA_31MAR_2003_2018.rds") %>% 
  filter(count_type %in% c("female_U1", "male_U1") & la_code == "W92000004") %>%  # take under 1 yr old counts only
  group_by(year) %>% 
  summarise(count_U1 = sum(as.numeric(count))) %>%  # count under 1s in care at 31st March, by year
  ungroup() %>% 
  left_join(pop_dat, by = "year") %>%   # merge on population estimates
  mutate(rate = count_U1*10000/pop_U1)

#--------------------------------#
# sensitivity - rounding ----
#--------------------------------#
# all counts published by welsh gov on infants in care have been rounded to nearest 5
# come up with range of all possible rates of infants in care at 31 march, by year

# create all possible combinations of unrounded counts:
cla_wales_clean_U1_sim <- readRDS("clean_wales_CLA_31MAR_2003_2018.rds") %>% 
  filter(count_type %in% c("female_U1", "male_U1") & la_code == "W92000004" & year %in% seq(2003,2018,3)) %>%
  group_by(year) %>% 
  summarise(count_U1 = sum(as.numeric(count))) %>% # add male U1 and female U1 counts together (both have error of between -2 to +2) 
  ungroup() %>% 
  mutate(count_1 = count_U1 - 4,  # adding two rounded counts together - new count has error of between -4 to +4
         count_2 = count_U1 - 3,
         count_3 = count_U1 - 2,
         count_4 = count_U1 - 1,
         count_5 = count_U1,
         count_6 = count_U1 + 1,
         count_7 = count_U1 + 2,
         count_8 = count_U1 + 3,
         count_9 = count_U1 + 4) %>% 
  dplyr::select(-count_U1) %>% 
  gather(key = "type", value = "count", count_1:count_9) %>% # reformat data set 
  spread(key = year, value = count) %>% # data set now has 6 columns (col per year) containing 9 rows of all poss values 
  dplyr::select(-type) %>% 
  expand.grid() %>% #   # create all possible combinations of yearly values
  mutate(id = row_number()) %>% # add id number to distinguish between diff combinations
  gather(key = "year", value = "count_U1", `2003`:`2018`, convert = TRUE) %>% # reformat data for merging on pop estimates
  left_join(pop_dat, by = "year") %>% 
  mutate(rate = count_U1*10000/pop_U1)  # create rates


#--------------------------------------------------#
# plot all possible rate trajectories over time ----
#--------------------------------------------------#
# rather than plot all possible rates, get min, max and median per year (for error bars):
plot_dat <- cla_wales_clean_U1_sim %>% 
  group_by(year) %>% 
  summarise(rate_min = round(min(rate/10000),4), 
           rate_max = round(max(rate/10000),4),
           rate_med = round(median(rate/10000),4)) %>% 
  ungroup()

# plot as bar chart:
ggplot(data = plot_dat, aes(x = year)) +
  geom_col(aes(y = rate_med, fill = as.factor(year))) +   # bars (up to median rate per yr)
  geom_errorbar(aes(ymin = rate_min, ymax = rate_max), width = 0.4, colour = "black") + # error bars (min-max)
  scale_x_continuous("", breaks = seq(2003,2018,3)) +
  scale_y_continuous("", labels = scales::percent_format(accuracy = 0.1),  # format y-axis as percentage
                     breaks = seq(0,0.012,0.004), limits = c(0,0.012)) +
  scale_fill_brewer() + 
  guides(fill = "none") +
  theme_classic() +
  theme(panel.background = element_rect(fill = "grey90",
                                        colour = "grey90",
                                        size = 0.5, linetype = "solid"),
        plot.caption = element_text(hjust = 0, size = 11),
        plot.title = element_text(size = 18),
        axis.text = element_text(size = 14),
        panel.grid.minor.y = element_line(colour = "white"),
        panel.grid.major.y = element_line(colour = "white")) +
  labs(title = "The proportion of infants in care in Wales (at 31st March), by year (2003 - 2018)\n",
       caption = "Data sources:\n1) http://gov.wales/statistics-and-research/children-looked-after-local-authorities/?lang=en (numerator) \n2) ONS UK mid-year population estimates (denominator) ")

