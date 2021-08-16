setwd("C:/Users/dapon/Dropbox/Harvard/dissertation")
library(tidyverse)
library(lfe)

#################################3
#run file "sector_concentration" before this one - uses objects from there
#####################################

parties <- read_csv("data/constituency_level_elections_archive/party.level.csv",
                    col_types = "dcdddcddddd")
nat <-  read_csv("data/constituency_level_elections_archive/national.level.csv") 
nat <- nat %>%
  rename(year = yr, country_name = ctr_n)


test <- left_join(sector_concentration, nat, by = c("country_name","year")) %>% 
  filter(agriculture != 0, !is.na(PSNS))

mod <- lm(data = test, PSNS ~ ICT + as.factor(country_name) + as.factor(year))
summary(mod)


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
  dplyr::select(-`manufacturing (industry)`, -financial, -`real estate`) %>% 
  mutate(country = substr(geo_code, 1, 2))

sector_shares <- wide %>%
  group_by(country, year) %>% 
  summarize_at(vars(total:arts), .funs = function(x) {sum(x, na.rm = TRUE)}) %>% 
  group_by(country, year) %>%
  summarize(agriculture = agriculture / total, 
            industry = industry / total, 
            construction = construction / total, 
            `Wholesale/retail trade, accom/food, transport, ICT` = `Wholesale/retail trade, accom/food, transport, ICT`/total,
            `Wholesale/retail trade, accom/food, transport` = `Wholesale/retail trade, accom/food, transport`/total, 
            ICT = ICT / total, 
            services = services / total, 
            `professional/admin` = `professional/admin`/total, 
            `public/arts` = `public/arts` / total, 
            public = public/total, 
            arts = arts/total) %>%
  pivot_longer(cols = agriculture:arts, names_to = "sector", values_to = "share") %>% 
  filter(share != "NaN") %>%
  mutate(share = ifelse(share == 0, NA, share))

  
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
  ))  %>% 
  pivot_longer(cols = agriculture:arts, 
               names_to = "sector", values_to = "concentration") %>%
  mutate(concentration = ifelse(concentration == 0, NA, concentration))


concentration_nat <- left_join(sector_concentration, sector_shares, 
                  by = c("country","year","sector")) %>%
  filter(!is.na(concentration)) %>%
#make weighted average 
  mutate(concen_weighted = share*concentration) %>% 
  group_by(country, year) %>%
  summarise(concen = weighted.mean(x = concentration, y = share)) %>% 
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

#get the data on individual sector shares by year and country
sector_gva <- wide %>%
  group_by(country, year) %>% 
  summarize_at(vars(total:arts), .funs = function(x) {sum(x, na.rm = TRUE)}) %>% 
  group_by(country, year) %>%
  summarize(agriculture = agriculture / total, 
            industry = industry / total, 
            construction = construction / total, 
            `Wholesale/retail trade, accom/food, transport, ICT` = `Wholesale/retail trade, accom/food, transport, ICT`/total,
            `Wholesale/retail trade, accom/food, transport` = `Wholesale/retail trade, accom/food, transport`/total, 
            ICT = ICT / total, 
            services = services / total, 
            `professional/admin` = `professional/admin`/total, 
            `public/arts` = `public/arts` / total, 
            public = public/total, 
            arts = arts/total) %>%
  filter(!(country %in% c("AT","ES","DE")), !is.na(agriculture)) %>% 
  select(-`Wholesale/retail trade, accom/food, transport, ICT`, -`public/arts`) 
  
#change names of sector_gva to match uk_df 
names(sector_gva) <- c("country","year","gva_share_agriculture",
                       "gva_share_industry","gva_share_construction",
                       "gva_share_Wholesale/retail trade, accom/food, transport",
                       "gva_share_ICT","gva_share_services", "gva_share_professional/admin",
                       "gva_share_public","gva_share_arts")

#join in the data on indiviudal sector concentrations 
sector_join <- sector_concentration %>%
  pivot_wider(id_cols = c("country","country_name","year"), 
              names_from = "sector", values_from = "concentration") %>% 
  filter(!is.na(agriculture))


concentration <- left_join(concentration_nat, sector_join, by = c("country","country_name", "year"))
#read in uk data
uk_sector_gva <- read_csv("data/eurostat/gva_nuts3/uk_concentration_nuts3.csv") %>% 
  select(-X1) %>% mutate(country_name = "UK") %>%
  select(country_name, year, starts_with("gva")) %>%
  rename(country = country_name)

rbind(uk_sector_gva, sector_gva) %>% as_tibble()

uk_sector_concen <- read_csv("data/eurostat/gva_nuts3/uk_concentration_nuts3.csv") %>% 
  select(-X1) %>% 
  mutate(country_name = "UK") %>%
  select(country_name, year, starts_with("concen"))


concentration <- plyr::rbind.fill(concentration, uk_df) %>% as_tibble()

test <- left_join(nat, concentration, by = c("country_name","year")) %>%
  filter(!is.na(concen)) %>%
  #indicator for decade
  mutate(decade = case_when(
    year < 2000 ~ 0, 
    year > 1999 & year < 2010 ~ 1, 
    year > 2010 ~ 2
  ), 
  concen_stand = scale(concen)) 

mod <- felm(data = test, concen_stand ~ ENEP_nat | country_name + year)
summary(mod)








