setwd("C:/Users/dapon/Dropbox/Harvard/dissertation")
library(tidyverse)
library(estimatr)
library(modelsummary)
library(lfe)
library(DescTools)

df <- read_csv("data/RosesWolf_RegionalGDP_v6.csv")
pop <- read_csv("data/RosesWolf_RegionalPop.csv")
###########################################
#read in party data
parties <- read_csv("data/constituency_level_elections_archive/party.level.csv",
                    col_types = "dcdddcddddd")
nat <-  read_csv("data/constituency_level_elections_archive/national.level.csv") 
nat <- nat %>%
  rename(year = yr, country = ctr_n) %>% 
  mutate(country = case_when(
    country == "UK" ~ "United Kingdom", 
    TRUE ~ country
  ))   

#clean the gdp data
df <- df %>%
  dplyr::select(-starts_with("X")) %>% 
  mutate_at(c("1900","1910","1925","1938","1950","1960"), as.numeric) %>% 
  rename(country = `Country (current borders)`) %>%
  rename(nuts = `NUTS-Codes`)
#clean pop a bit
pop <- pop %>% 
  rename(country = `Country (current borders)`, nuts = `NUTS-Codes`) %>%
  mutate_at(c("1900","1910","1925","1938","1950","1960"), as.numeric)

long <- df %>% 
  pivot_longer(cols = `1900`:`2015`, names_to = "year",
               values_to = "gdp") %>%
  mutate(year = as.numeric(year), 
         gdp = gdp*1000000) 
pop_long <- pop %>%
  pivot_longer(cols = `1900`:`2015`, names_to = "year",
               values_to = "pop") %>% 
  mutate(year = as.numeric(year), 
         pop = pop*1000)
#put these together, then make per-cap gdp variable 
long <- left_join(long, pop_long, by = c("country","Region","nuts","year")) %>%
  mutate(gdp_per_cap = gdp/pop)


#interpolate regional gdp in missing years
years <- seq(1900, 2015, 1)
regions <- unique(long$nuts)
out <- NULL
for(i in 1:(length(regions) - 2)){
  print(i)
  region_gdp <- long %>% 
    filter(nuts == regions[i]) %>% 
    pull(gdp_per_cap)
  region_year <- long %>%
    filter(nuts == regions[i]) %>% 
    pull(year)
  #interpolate 
  interp <- approx(x = region_year, y = region_gdp, 
                   method = "linear", xout = years)
  dat <- as.data.frame(cbind(interp$x, interp$y))
  names(dat) <- c("year", "gdp_per_cap")
  dat$nuts <- rep(regions[i], nrow(dat))
  out <- rbind(dat, out)
}
#create full interpolated dataset with region and country names 
nuts_regions <- long %>%
  distinct(nuts, Region, country)
full <- left_join(out, nuts_regions, by = "nuts") %>%
  as_tibble() %>%
  #make decade indicator 
  mutate(decade = case_when(
    year < 1910 ~ 1,
    1909 < year & year  < 1920 ~ 2, 
    1919 < year & year < 1930 ~ 3,
    1929 < year & year < 1940 ~ 4,
    1939 < year & year < 1950 ~ 5,
    1949 < year & year < 1960 ~ 6,
    1959 < year & year < 1970 ~ 7,
    1969 < year & year < 1980 ~ 8,
    1979 < year & year < 1990 ~ 9,
    1989 < year & year < 2000 ~ 10,
    1999 < year & year < 2010 ~ 11,
    2009 < year& year  < 2020 ~ 12,
  ), period = case_when(
    year < 1945 ~ 1, 
    year > 1944 & year < 1974 ~ 2, 
    year > 1973 & year < 2001 ~ 3, 
    year > 2000 ~ 4
  ))

#get gini coefficients by country per year
ginis <- full %>% 
  group_by(country, period, year) %>% 
  summarise(gini = Gini(gdp_per_cap, na.rm = TRUE)) %>% 
  filter(country != "IRELAND" & country != "LUXEMBOURG") %>% 
  filter(!is.na(country)) %>% 
  #make lag varaible 
  group_by(country) %>% 
  mutate(gini_lag1 = lag(gini, order_by = year),
         gini_lag2 = lag(gini, order_by = year, n = 2), 
         gini_diff1 = gini - gini_lag1,
         gini_diff2 = gini - gini_lag2) 

hist(ginis$gini)
ggplot(ginis, aes(x = year, y = gini)) + 
  geom_line() + 
  facet_wrap(~country)




########################################3
#####TESTING MODELS

moddat <- left_join(nat, ginis, by = c("country","year")) %>%
  filter(!is.na(gini)) %>%
  #make lagged party nationalization and delta party nationalization
  mutate(PSNS_sw_lag = lag(PSNS_sw), 
         PSNS_sw_change = PSNS_sw - PSNS_sw_lag)

ggplot(moddat, aes(x = gini_lag2, y  = PSNS_sw)) + 
  geom_point() + 
  geom_smooth(method = "lm")


base <- lm_robust(PSNS_sw ~ gini_lag2, data = moddat)
                 # se_type = "stata")
fixed_country <- lm_robust(PSNS_sw ~ gini_lag2,
                           fixed_effects = ~country, 
                          # se_type = "stata",
                           data = moddat)
fixed_decade <- lm_robust(PSNS_sw ~ gini_lag2,
                          fixed_effects = ~decade, 
                          #se_type = "stata",
                          data = moddat)
fixed_country_decade <- lm_robust(PSNS_sw ~ gini_lag2,
                                  fixed_effects = ~country + decade, 
                                  data = moddat)

fixed_country_cluster <- lm_robust(PSNS_sw ~ gini_lag2,
                                   fixed_effects = ~country, 
                                   #se_type = "stata",
                                   clusters = country,
                                      data = moddat)
fixed_decade_cluster <- lm_robust(PSNS_sw ~ gini_lag2,
                                  fixed_effects = ~decade, 
                                     #se_type = "stata",
                                    clusters = country,
                                     data = moddat)
fixed_country_decade_cluster <- lm_robust(PSNS_sw ~ gini_lag2,
                                          fixed_effects = ~country + decade, 
                                             #se_type = "stata",
                                          clusters = country,
                                             data = moddat)

models <- list()
models[["OLS"]] <- base
models[["Fixed country"]] <- fixed_country
models[["Fixed decade"]] <- fixed_decade
models[["Fixed country decade"]] <- fixed_country_decade
models[["Cluster fixed country"]] <- fixed_country_cluster
models[["Cluster fixed decade"]] <- fixed_decade_cluster
models[["Cluster fixed country decade"]] <- fixed_country_decade_cluster


#run models
fe1 <- felm(data = moddat, PSNS_sw ~ gini  | country )
fe2 <- felm(data = moddat, PSNS_sw ~ gini_lag1  | country )
fe3 <- felm(data = moddat, PSNS_sw ~ gini + ENEP_nat | country )
fe4 <- felm(data = moddat, PSNS_sw ~ gini_lag1  + ENEP_nat | country )
fe5 <- felm(data = moddat, PSNS_sw_change ~ gini_diff2  | country )
fe6 <- felm(data = moddat, PSNS_sw_change ~ gini_diff2  + ENEP_nat | country + period )



stargazer(fe1, fe2, fe3, fe4,
          add.lines=list(c('Country fixed effects', 'Yes','Yes',"Yes","Yes")),
          covariate.labels = c("Gini", "Gini (lag)","ENEP" ),
          dep.var.labels = "Party system nationalization (static)",
          column.sep.width = "2pt",
          label = "gini_models",
          omit.stat = c("ser", "adj.rsq")
)

