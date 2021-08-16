setwd("C:/Users/dapon/Dropbox/Harvard/dissertation")
library(tidyverse)
library(DescTools) #for Herfindahl function
library(ggthemes)

raw <- read_csv("data/eurostat/gva_nuts3/gva_by_industry_nuts3_full_raw.csv")

#note: France has no data prior to 2015, annoyingly
#note: Germany does not disaggregate the "various" and "public/arts" categories, 
#so i'll just do germany on its own
#spain and austria have the same problem 
############################
#EVERYWHERE EXCEPT GERMANY, SPAIN, AUSTRIA
df <- raw %>%
  rename(year = TIME, sector = NACE_R2, sector_name = NACE_R2_LABEL, 
         geo_code = GEO, geo_label = GEO_LABEL, currency = CURRENCY, value = Value) %>% 
  #generate variable for clean sector namings - this is from the "label" dataset
  mutate(sector_clean = case_when(
    sector == "TOTAL" ~ "total", 
    sector == "A" ~ "agriculture", 
    sector == "B-E" ~ "industry", 
    sector == "C" ~ "manufacturing (industry)", 
    sector == "F" ~ "construction",
    sector == "G-J" ~ "Wholesale/retail trade, accom/food, transport, ICT", 
    sector == "G-I" ~ "Wholesale/retail trade, accom/food, transport", 
    sector == "J" ~ "ICT", 
    sector == "K-N" ~ "services", 
    sector == "K" ~ "financial", 
    sector == "L" ~ "real estate", 
    sector == "M_N" ~ "professional/admin", 
    sector == "O-U" ~ "public/arts", 
    sector == "O-Q" ~ "public", 
    sector == "R-U" ~ "arts"
  ), value = ifelse(is.na(value == ":"), NA, value)) %>% 
  mutate(value = as.numeric(str_remove(value, " ")))

#create wide dataset
wide <- df %>% 
  pivot_wider(id_cols = c("geo_code","geo_label","currency","year"),
              names_from = c("sector_clean"), 
              values_from = "value") %>% 
  #use only the big columns but keep the ICT distinction and public/arts distinction
  dplyr::select(-`manufacturing (industry)`, -financial, -`real estate`,-total) %>% 
  mutate(country = substr(geo_code, 1, 2))

#this tells us how geographically concentrated an industry is in each country

sector_concentration <- wide %>% 
  #exclude extra-territorial regions
  filter(str_detect(geo_code,'ZZZ') == FALSE) %>% 
  
  select(-geo_label, -currency, -`public/arts`, -`Wholesale/retail trade, accom/food, transport, ICT`) %>% 
  select(country, year, everything()) %>% 
  group_by(country, year) %>% 
  #what a great command summarize_at is
  summarize_at(vars(agriculture:arts), .funs = function(x) {Herfindahl(x, na.rm = TRUE)}) %>% 
  filter(str_detect(country, "-") == FALSE) %>% 
  filter(country %in% c("BE","DK","FR","FI",
                        "IE","IT","NL","PT","SE","SI")) %>% 
  mutate(country_name = case_when(
    country == "BE" ~ "Belgium", 
    country == "DK" ~ "Denmark",
    country == "FI" ~ "Finland", 
    country == "FR" ~ "France",
    country == "IE" ~ "Ireland",
    country == "IT" ~ "Italy",
    country == "NL" ~ "Netherlands",
    country == "PT" ~ "Portugal", 
    country == "SI" ~ "Slovenia"
  )) 

#create graph of geogtraphic concentration of industries
sector_plot <- sector_concentration %>%
  filter(year > 1999) %>% 
  pivot_longer(cols = agriculture:arts, names_to = "sector",
               values_to = "concentration")  %>% 
  #filter out industries with values of 1, 0, or NaN - these are data problems / missingness
  filter(concentration != 1 & concentration != 0 & !is.na(concentration)) %>% 
  mutate(Sector = str_wrap(sector, 20)) %>% 
  ggplot(., aes(x = year, y = concentration, color = Sector)) +
  geom_line() + 
  facet_wrap(~ country_name,  scales = "free") + 
  theme_few() + 
  labs(title = "Geographic concentration of economic sectors",
       subtitle = "Herfindahl index of sectoral GVA at NUTS-3 level",
      x = "Year", y = "Concentration") #+ 
  theme(legend.key.height=unit(0.5, "cm"))
ggsave("prospectus/figures/sector_concentration_nuts3.pdf", 
       sector_plot)
  

#this tells us how specialized each region is 
#uses six-industry classification
region_specialization <- df %>%
  select(-geo_label, -currency, -`Flag and Footnotes`) %>% 
  pivot_wider(id_cols = c("year","sector","sector_name","sector_clean"), 
              names_from = "geo_code", values_from = value) %>% 
  filter(!(sector_clean %in% c("manufacturing (industry)",
                               "financial","real estate", "total", "public/arts"))) %>% 
  group_by(year) %>% 
  #this magically goes by column to calculate Herfindahl indices
  summarize_at(vars(BE100:SEZZZ), .funs = function(x) {Herfindahl(x, na.rm = TRUE)}) 

#graph of mean regional specialization by country and year 
region_specialization %>% 
  pivot_longer(cols = BE100:SEZZZ, names_to = "region", 
               values_to = "specialization") %>% 
  mutate(country = substr(region, 1, 2)) %>% 
  filter(country %in% c("BE","DK","FR","FI",
                         "IE","IT","NL","PT","SE","SI")) %>% 
  filter(!is.na(specialization) & specialization != 0) %>% 
  filter(year < 2019 & year > 1999) %>% 
  filter(str_detect(region, "ZZZ") == FALSE) %>% 
  group_by(country, year) %>% 
  summarize(mean_specialization = mean(specialization, na.rm = TRUE)) %>% 
  mutate(country = factor(country, 
                          levels = c("DE","AT","ES","SI","IE","PT","BE","IT","FR","DK","NL","FI","SE"))) %>% 
  ggplot(., aes(x = year, y = mean_specialization, color = country)) + 
  geom_line()



########################################3
#3 Germany, Spain, Austria now
####################################33

wide <- df %>% 
  pivot_wider(id_cols = c("geo_code","geo_label","currency","year"),
              names_from = c("sector_clean"), 
              values_from = "value") %>% 
  #use only the big columns but keep the ICT distinction and public/arts distinction
  dplyr::select(-`manufacturing (industry)`, -financial, -`real estate`,-total) %>% 
  mutate(country = substr(geo_code, 1, 2))

#this tells us how geographically concentrated an industry is in each country

sector_concentration <- wide %>% 
  #exclude extra-territorial regions
  filter(str_detect(geo_code,'ZZZ') == FALSE) %>% 
  select(-geo_label, -currency) %>% 
  select(country, year, everything()) %>% 
  group_by(country, year) %>% 
  #what a great command summarize_at is
  summarize_at(vars(agriculture:arts), .funs = function(x) {Herfindahl(x, na.rm = TRUE)}) %>% 
  filter(str_detect(country, "-") == FALSE)

#create graph of geogtraphic concentration of industries
concen_plot_others <- sector_concentration %>%
  filter(year > 1999) %>% 
  pivot_longer(cols = agriculture:arts, names_to = "sector",
               values_to = "concentration") %>% 
  filter(country %in% c("AT","DE","ES")) %>% 
  #filter out industries with values of 1, 0, or NaN - these are data problems / missingness
  filter(concentration != 1 & concentration != 0 & !is.na(concentration)) %>% 
  mutate(Sector = str_wrap(sector, 20),
         country_name = case_when(
         country == "AT" ~ "Austria", 
         country == 'DE' ~ "Germany",
         country == "ES" ~ "Spain"
         )) %>%  
  ggplot(., aes(x = year, y = concentration, color = Sector)) +
  geom_line() + 
  facet_wrap(~ country_name,  scales = "free") + 
  theme_few() + 
  labs(x = "Year", y = "Concentration", 
       title = "Geographic concentration of economic sectors", 
       subtitle = "Herfindahl index of sectoral GVA at NUTS-3 level")
ggsave("prospectus/figures/sector_concentration_nuts3_others.pdf",
       concen_plot_others)


#this tells us how specialized each region is 
#uses six-industry classification
region_specialization <- df %>%
  select(-geo_label, -currency, -`Flag and Footnotes`) %>% 
  pivot_wider(id_cols = c("year","sector","sector_name","sector_clean"), 
              names_from = "geo_code", values_from = value) %>% 
  filter(!(sector_clean %in% c("manufacturing (industry)",
                               "financial","real estate", "total", "public/arts"))) %>% 
  group_by(year) %>% 
  #this magically goes by column to calculate Herfindahl indices
  summarize_at(vars(BE100:SEZZZ), .funs = function(x) {Herfindahl(x, na.rm = TRUE)}) 


#create graph of mean regional specialization by country and year 
region_specialization %>% 
  pivot_longer(cols = BE100:SEZZZ, names_to = "region", 
               values_to = "specialization") %>% 
  mutate(country = substr(region, 1, 2)) %>% 
  filter(country %in% c("BE","DK","FR","FI",
                        "IE","IT","NL","PT","SE","SI")) %>% 
  #do a bunch of filtering 
  filter(!is.na(specialization) & specialization != 0) %>% 
  filter(year < 2019 & year > 1999) %>% 
  filter(str_detect(region, "ZZZ") == FALSE) %>% 
  group_by(country, year) %>% 
  summarize(mean_specialization = mean(specialization, na.rm = TRUE)) %>% 
  mutate(country = factor(country, 
                          levels = c("DE","AT","ES","SI","IE","PT","BE","IT","FR","DK","NL","FI","SE"))) %>% 
  ggplot(., aes(x = year, y = mean_specialization, color = country)) + 
  geom_line()
