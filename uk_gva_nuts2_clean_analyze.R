setwd("C:/Users/dapon/Dropbox/Harvard/dissertation")
library(tidyverse)
library(DescTools) #for Herfindahl function

uk <- read_csv("data/eurostat/gva_nuts3/uk_gva_by_industry_nuts2.csv")

uk <- uk %>% 
  rename(region_code = `Region code`, region_name = `Region name`,
         sector_code = SIC07, sector_name = `SIC07 description`, 
         `2018` = `20183`)

uk <- uk %>%
  #note that this combines A and B, while eurostat data keeps them apart
  filter(sector_code %in% c("A","B", "C","D","E","F","G","H","I","J","K",
                            "L","M","N","O","P","Q","R","S","T","G-T",
                            "Total")) %>%
  #get rid of a few categories which are aggregates; keep the data disaggregated
  filter(!(sector_code %in% c("A-E","G-T"))) %>% 
  mutate(sector_clean = case_when(
    sector_name == "Agriculture, forestry and fishing" ~ "Agriculture",
    sector_name == "Mining and quarrying" ~ "Mining",
    sector_name == "Manufacturing" ~ "Manufacturing",
    sector_name == "Construction" ~ "Construction",
    sector_name == "Wholesale and retail trade; repair of motor vehicles" ~ "Wholesale and retail trade",
    sector_name == "Transportation and storage" ~ "Transport and storage", 
    sector_name == "Accomodation and food service activites" ~ "Accom. and food service", 
    sector_name == "Information and communication" ~ "ICT",
    sector_name == "Financial and insurance activities" ~ "Finance", 
    sector_name == "Real estate activities" ~ "Real estate",
    sector_name == "Professional, scientific and technical activities" ~ "Professional/technical",
    sector_name == "Administrative and support service activities" ~ "Administration",
    sector_name == "Public administration and defence" ~ "Public admin. and defense",
    sector_name == "Education" ~ "Education",
    sector_name == "Human health and social work activities" ~ "Health",
    sector_name == "Arts, entertainment and recreation" ~ "Arts/recreation", 
    sector_name == "Other service activities" ~ "Other services",
    sector_name == "Activities of households" ~ "Household activities",
    TRUE ~ sector_name
  ))
#aggre





industry_concentration <- uk %>%
  select(-region_name, -sector_name) %>% 
  #get columns as regions - requires double pivot
  pivot_longer(cols = `1998`:`2018`, names_to = "year", values_to = "value") %>% 
  pivot_wider(id_cols = c("region_code","year"), names_from = "sector_clean", values_from = "value") %>%
  select(-`All industries`) %>% 
  group_by(year) %>% 
  summarize_at(vars(`Agriculture`:`Household activities`),
               .funs = function(x) {Herfindahl(x, na.rm = TRUE)}) 


tograph <- industry_concentration %>% 
  #ungroup() %>% 
  pivot_longer(cols = `Agriculture`:`Household activities`, names_to = "sector", values_to = "value") %>% 
  arrange(sector) %>% 
  mutate(year = as.numeric(year)) 

#pull order of sectors in 2018 for graphing purposes
order <- tograph %>% 
  filter(year == 2018) %>% 
  arrange(desc(value)) %>% pull(sector)

#make graph of spatial concentration of sectors
spatcon <- tograph %>% 
  mutate(Sector = factor(sector, levels = order)) %>% 
  ggplot(., aes(x = year, y = value, color = Sector)) + 
  geom_line() + 
  theme_minimal() + 
  scale_x_continuous(breaks = c(2000, 2004, 2008, 2012, 2016, 2020)) + 
  ggtitle("Spatial concentration of UK sectors") + 
  xlab("Year") + 
  ylab("Herfindahl index") + 
  labs(subtitle = "Calculated using Herfindahl index of sector-level GVA at NUTS-2 level")
ggsave('prospectus/figures/uk_sector_concen_nuts2.pdf',
       spatcon)


#now figure out which regions are the most sectorally concentrated 
regions <- uk %>% 
  pivot_longer(cols = `1998`:`2018`, names_to = "year", values_to = "value") %>% 
  group_by(region_code, region_name, year) %>% 
  summarize(concentration = Herfindahl(value, na.rm = TRUE)) %>% 
  mutate(year = as.numeric(year))

#get biggest industry in each region in 2018
biggest <- uk %>% 
  pivot_longer(cols = `1998`:`2018`, names_to = "year", values_to = "value") %>% 
  filter(year == 2018, sector_code != "Total") %>% 
  group_by(region_code) %>% 
  mutate(biggest = ifelse(value == max(value), 1, 0)) %>%
  filter(biggest == 1) 
biggest <- cbind(biggest$region_code, biggest$sector_clean) %>% 
  as_tibble()
names(biggest) <- c("region_code","Largest Sector in 2018")


#graph of regional concentration, by largest sector in region
region_concen_by_largest <- regions %>%
  left_join(., biggest, by = "region_code") %>% 
  mutate(label = ifelse(region_name == "Inner London - West" & year == 2018,
                        "Inner London (West)                             ", NA),
         London = as.factor(ifelse(str_detect(region_name, "London") == TRUE, 1, 0))) %>% 
  ggplot(., aes(x = year, y = concentration, 
                group = region_name)) + 
  geom_line(aes(color = `Largest Sector in 2018`)) + 
  geom_text(aes(label = label)) + 
  theme_minimal() + 
  xlab("Year") + ylab("Herfindahl index") + 
  ggtitle('Sectoral concentration by NUTS-2 Regions')+ 
  labs(subtitle = "Calculated using Herfindahl index of regional GVA")

#Note that the urban areas with the lowest sectoral concentration in 2018 are 
#Bristol/Bath and and Greater Manchester

ggsave("prospectus/figures/uk_nuts2_region_concentration_by_largest.pdf", region_concen_by_largest)

region_concen_by_london <- regions %>%
  left_join(., biggest, by = "region_code") %>% 
  mutate(label = ifelse(region_name == "Inner London - West" & year == 2018,
                        "Inner London (West)                            ", NA),
         London = as.factor(ifelse(str_detect(region_name, "London") == TRUE, 1, 0))) %>% 
  ggplot(., aes(x = year, y = concentration, 
                group = region_name)) + 
  geom_line(aes(color = London)) + 
  geom_text(aes(label = label)) + 
  theme_minimal() + 
  xlab("Year") + ylab("Herfindahl index") + 
  ggtitle('Sectoral concentration by NUTS-2 Regions') + 
  labs(subtitle = "Calculated using Herfindahl index of regional GVA")

ggsave("prospectus/figures/uk_nuts2_region_concentration_by_london.pdf", 
       region_concen_by_london)



#######################
#TRY AGGREGATING UPWARDS TO HIGHER INDUSTRY CLASSIFICATIONS
#put together:
#D, E, F, H (electricity, water supply, construction, transport) - infrastructure
#try putting together J, K, M, N - ICT plus financail + professiona.technical + adminsitrative = "Knowledge" 
#try putting together M and N - professioan/technicaly + administrative
#put together public sector O through Q 
#keep arts, entertainment, recreation (R) on its own

wider <- uk %>% 
  pivot_longer(cols = `1998`:`2018`, names_to = "year", values_to = "value") %>% 
  select(-sector_name, -sector_clean) %>%
  pivot_wider(names_from = "sector_code", values_from = "value") %>% 
  mutate(infrastructure = D + E + `F` + H, 
         knowledge = J + K + M + N, 
         public = O + Q) %>%
  select(-D, -E, -`F`, -H,  -J, -K, -M, -N, -O, -Q, -Total) %>% 
  select(-X26) 

region_concen <- wider %>% 
  pivot_longer(cols = A:public, names_to = "sector", values_to = "value") %>% 
  group_by(region_name, year) %>% 
  summarize(concentration = Herfindahl(value, na.rm = TRUE))


sector_concen <- wider %>% 
  pivot_longer(cols = A:public, names_to = "sector", values_to = "value") %>%
  group_by(sector, year) %>%
  summarize(concentration = Herfindahl(value, na.rm = TRUE)) %>%
  ungroup()
  
sector_concen %>%
  mutate(year = as.numeric(year)) %>% 
  ggplot(., aes(x = year, y = concentration, color = sector)) + 
  geom_line() + 
  theme_minimal() + 
  labs(title = "Geographic concentration of economic sectors", 
       subtitle = "Calculated using Herfindahl index at NUTS-2 level",
       y = "HH Index", x = "Year")
  
  
  
#############################################
sector_shares <- uk %>% 
  pivot_longer(cols = `1998`:`2018`,
               names_to = "year", values_to = "value") %>% 
  select(-sector_code, -X26, -sector_name) %>% 
  filter(sector_clean != "All industries") %>%
  group_by(sector_clean, year) %>% 
  summarise(sector_gva = sum(value, na.rm = TRUE)) %>% 
  group_by(year) %>% 
  mutate(total = sum(sector_gva, na.rm = TRUE),
         sector_share = sector_gva / total,
         year = as.numeric(year)) %>% 
  rename(sector = sector_clean)

sector_concen <- left_join(tograph, sector_shares, by = c("sector","year")) %>% 
  rename(sector_concen = value)
#get ICT concentration
tograph %>% 
  filter(sector == "ICT") %>%
  select(year, value) %>% 
  ggplot(aes(x = year, y = value)) + 
  geom_line() + 
  labs(title = "Geographic concentration of ICT, NUTS 2",
       subtitle = "Herfindahl index of ICT GVA",
       x = "Year", y = "Concentration") + 
  theme_minimal()

concen_by_year <- sector_concen %>% 
  group_by(year) %>% 
  summarise(concen = weighted.mean(x = sector_concen, w = sector_share)) %>% 
  left_join(., ict, by = "year")
write.csv(concen_by_year, file = "data/eurostat/uk_concen_by_year.csv")







  
  
  

