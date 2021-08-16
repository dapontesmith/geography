library(tidyverse)
library(DescTools)
nuts3 <- read_csv("data/eurostat/gva_nuts3/uk_gva_by_industry_nuts3_condensed.csv")
nuts3 <- nuts3 %>% 
  filter(SIC07 %in% c("AB", "C","D","E","F","G","H","I","J","K",
                            "L","M","N","O","P","Q","R","S","T","G-T",
                            "Total")) %>%
  rename(`2018` = `20183`)
nuts3 <- nuts3 %>% 
  pivot_longer(cols = `1998`:`2018`, names_to = "year", values_to = "value") %>% 
  filter(!(SIC07 %in% c("A-E","G-T"))) %>% 
  mutate(sector = case_when(
    `SIC07 description` == "Agriculture, forestry and fishing; mining and quarrying" ~ "Agriculture, mining",
    `SIC07 description` == "Mining and quarrying" ~ "Mining",
    `SIC07 description` == "Manufacturing" ~ "Manufacturing",
    `SIC07 description` == "Construction" ~ "Construction",
    `SIC07 description` == "Wholesale and retail trade; repair of motor vehicles" ~ "Wholesale and retail trade",
    `SIC07 description` == "Transportation and storage" ~ "Transport and storage", 
    `SIC07 description` == "Accomodation and food service activites" ~ "Accom. and food service", 
    `SIC07 description` == "Information and communication" ~ "ICT",
    `SIC07 description` == "Financial and insurance activities" ~ "Finance", 
    `SIC07 description` == "Real estate activities" ~ "Real estate",
    `SIC07 description` == "Professional, scientific and technical activities" ~ "Professional/technical",
    `SIC07 description` == "Administrative and support service activities" ~ "Administration",
    `SIC07 description` == "Public administration and defence" ~ "Public admin. and defense",
    `SIC07 description` == "Education" ~ "Education",
    `SIC07 description` == "Human health and social work activities" ~ "Health",
    `SIC07 description` == "Arts, entertainment and recreation" ~ "Arts/recreation", 
    `SIC07 description` == "Other service activities" ~ "Other services",
    `SIC07 description` == "Activities of households" ~ "Household activities",
    TRUE ~ `SIC07 description`
  )) %>% 
  pivot_wider(id_cols = c("Region code",'Region name', "year"), 
              names_from = "sector", values_from = "value")


new <- matrix(nrow = nrow(nuts3), ncol = 13) %>% as_tibble()
new$agriculture <- nuts3$`Agriculture, mining`
new$construction <- nuts3$Construction
new$industry <- nuts3$Manufacturing
new$`professional/admin` <- nuts3$`Professional/technical` + nuts3$Administration
new$ICT <- nuts3$ICT
new$`Wholesale/retail trade, accom/food, transport` <- nuts3$`Wholesale and retail trade` + 
  nuts3$`Transport and storage` + nuts3$`Accommodation and food service activities`
new$services <- nuts3$Finance + nuts3$`Real estate`
new$public <- nuts3$`Public admin. and defense` + nuts3$Education + nuts3$Health 
new$arts <- nuts3$`Arts/recreation` 
new$region_name <- nuts3$`Region name` ; new$region_code <- nuts3$`Region code` 
new$year <- nuts3$year
new <- new %>% select(-starts_with("V")) %>% 
  select(region_name, region_code, year, everything())

#tibble of sector sahres 
sector_shares <- new %>% 
  pivot_longer(cols = agriculture:arts, names_to = "sector", values_to = "gva") %>% 
  group_by(year, sector) %>% 
  summarise(gva_full = sum(gva, na.rm = TRUE)) %>% 
  group_by(year) %>% 
  mutate(total_gva = sum(gva_full, na.rm = TRUE), 
         gva_share = gva_full / total_gva, 
         year = as.numeric(year))

#tibble of sector concentrations 
sector_concen <- new %>% 
  group_by(year) %>% 
  summarize_at(vars(agriculture:arts), .funs = function(x) {Herfindahl(x, na.rm = TRUE)}) %>%
  pivot_longer(cols = agriculture:arts, names_to = "sector", values_to = "concen") %>% 
  mutate(year = as.numeric(year)) %>% 
  left_join(., sector_shares, by = c("year","sector")) 

#weighted average 
weighted <- sector_concen %>% 
  group_by(year) %>% 
  summarise(concen = weighted.mean(x = concen, w = gva_share))

#graph of sectoral concnetration of british economy over time 
weighted %>% 
  mutate(change = 100*(concen/0.0124 - 1)) %>% 
  ggplot(aes(x = year, y = change)) + 
  geom_line() +
  theme_minimal() + 
  labs(title = str_wrap("Cumulative pct. change in concentration of British economy", 50), 
       x = "Year", 
       y = "Cumulative pct. change", 
       subtitle = str_wrap("Average of NUTS-3 geographic concentration of sectors,
                           weighted by sector share of national GVA", 50)) 



full <- sector_concen %>% 
  pivot_wider(id_cols = "year", names_from = "sector", values_from = c("concen", "gva_share")) %>% 
  left_join(., weighted, by = "year")

write.csv(full, "data/eurostat/gva_nuts3/uk_concentration_nuts3.csv")

#graph of geographic concentration of ICT over time 
full %>% 
  mutate(change = 100*(concen_ICT/full$concen_ICT[1] - 1) ) %>% 
  ggplot(aes(x = year, y = change)) + 
    geom_line() + 
  theme_minimal() + 
  labs(title = str_wrap("Cumulative pct. change in geographic concentration of ICT"), 
       x = "Year", y = "Cumulative pct. change")


