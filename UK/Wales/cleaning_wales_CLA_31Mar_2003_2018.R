#------------------------------------------------#
# Cleaning Welsh CLA (at 31st March) 2003 - 2018 #
#------------------------------------------------#

# data source: https://statswales.gov.wales/Catalogue/Health-and-Social-Care/Social-Services/Childrens-Services/Children-Looked-After/childrenlookedafterat31march-by-localauthority-gender-age

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

# download data from github to folder and set this as working directory
setwd("N:/Documents/Data & Programming/Public data/Welsh CLA/")

#-------------------#
# get data files ----
#-------------------#

cla_wales_raw <- map(seq(2003,2018,1), ~read_csv(paste0("CLA_31MAR_",
                                                        .x,
                                                        ".csv"),
                                                 na = c(".", ""),
                                                 col_names = FALSE)) 
names(cla_wales_raw) <- paste0("CLA_31MAR_", seq(2003,2018,1))


# check all table headers are the same before cleaning:
table_headers <- map(cla_wales_raw, ~filter(., row_number() %in% seq(1,4,1)))

# returns TRUE if all match, FALSE if any do not: 
all(map_df(table_headers, ~isTRUE(all_equal(target = .x, current = table_headers[[1]], 
                                                        ignore_row_order = FALSE,
                                                        ignore_col_order = FALSE))) == TRUE)
    

#-----------------------#
# produce clean file ----
#-----------------------#

# get welsh la codes:
la_lkup <- read_xlsx("lasregionew2019.xlsx",
                     skip = 4) %>% 
  clean_names() %>% 
  filter(tolower(region_name) == "wales") %>% 
  bind_rows(., data.frame(la_code = "W92000004", la_name = "Wales")) %>% # add a row for Wales with region code as la code
  mutate(la_name_clean = to_snake_case(la_name)) %>% 
  dplyr::select(-starts_with("region"), -la_name)


# assign clean headers:
cla_wales_clean <- cla_wales_raw %>% 
  map(., ~mutate(., X2 = ifelse(is.na(X2) & !is.na(X1), "Wales", X2))) %>% 
  map(., ~dplyr::select(., -X1,
                        la_name = X2,
                        male_U1 = X3,
                        male_1to4 = X4,
                        male_5to9 = X5,
                        male_10to15 = X6,
                        male_16to17 = X7,
                        male_18over = X8,
                        male_total = X9,
                        female_U1 = X10,
                        female_1to4 = X11,
                        female_5to9 = X12,
                        female_10to15 = X13,
                        female_16to17 = X14,
                        female_18over = X15,
                        female_total = X16,
                        total = X17)) %>%
  map(., ~filter(., !is.na(la_name))) %>% 
  map(., ~gather(., key = "count_type", value = "count", male_U1:total)) %>% # wide to long
  map2_df(., seq(2003,2018,1), ~mutate(.x, year =.y,
                                       la_name_clean = to_snake_case(la_name))) %>% # assign year variable before row bind
  left_join(la_lkup, by = c("la_name_clean"))  # merge on la codes

# check all rows have an la code:
any(is.na(cla_wales_clean$la_code))

# save clean data file:
saveRDS(cla_wales_clean, "clean_wales_CLA_31MAR_2003_2018.rds")



