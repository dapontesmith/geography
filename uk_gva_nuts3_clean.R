setwd("C:/Users/dapon/Dropbox/Harvard/dissertation/data")
library(tidyverse)
library(DescTools) #for Herfindahl function

uk <- read_csv("eurostat/gva_nuts3/uk_gva_by_industry_nuts3_condensed.csv")
uk <- uk %>% 
  rename(region_code = `Region code`, region_name = `Region name`,
         sector_code = SIC07, sector_name = `SIC07 description`, 
         `2018` = `20183`)

#condense to certain industries
uk <- uk %>%
  #note that this combines A and B, while eurostat data keeps them apart
  filter(sector_code %in% c("AB","C","A-E","F","G","H","I","J","K",
                            "L","M","N","O","P","Q","R","S","T","G-T",
                            "Total")) %>%
  #get rid of a few categories which are aggregates; keep the data disaggregated
  filter(!(sector_code %in% c("A-E","G-T"))) %>% 
  mutate(sector_clean = case_when(
    sector_name == "Agriculture, forestry and fishing; mining and quarrying" ~ "Agriculture, mining",
    sector_name == "Manufacturing" ~ "Manufacturing",
    sector_name == "Construction" ~ "Construction",
    sector_name == "Wholesale and retail trade; repair of motor vehicles" ~ "Wholesale and retail trade",
    sector_name == "Transportation and storage" ~ "Transport and storage", 
    sector_name == "Accomodation and food service activites" ~ "Accom. and food service", 
    sector_name == "Information and communication" ~ "ICT",
    sector_name == "Financial and insurance activities" ~ "Finance", 
    sector_name == "Real estate activities" ~ "Real estate",
    sector_name == "Professional, scientific, and technical activities" ~ "Professional/technical",
    sector_name == "Administrative and support service activities" ~ "Administration",
    sector_name == "Public administration and defence" ~ "Public admin. and defense",
    sector_name == "Education" ~ "Education",
    sector_name == "Human health and social work activities" ~ "Health",
    sector_name == "Arts, entertainment and recreation" ~ "Arts/recreation", 
    sector_name == "Other service activities" ~ "Other services",
    sector_name == "Activities of households" ~ "Household activities",
    TRUE ~ sector_name
  ))
#aggregate to same industries as in the eurostat data
 
industry_concentration <- uk %>%
  select(-region_name, -sector_name) %>% 
  #get columns as regions - requires double pivot
  pivot_longer(cols = `1998`:`2018`, names_to = "year", values_to = "value") %>% 
  pivot_wider(id_cols = c("region_code","year"), names_from = "sector_clean", values_from = "value") %>%
 select(-`All industries`) %>% 
  group_by(year) %>% 
  summarize_at(vars(`Agriculture, mining`:`Household activities`),
               .funs = function(x) {Herfindahl(x, na.rm = TRUE)}) 

#make plot
tograph <- industry_concentration %>% 
  #ungroup() %>% 
  pivot_longer(cols = `Agriculture, mining`:`Household activities`, names_to = "sector", values_to = "value") %>% 
  arrange(sector) %>% 
  mutate(year = as.numeric(year)) 

#pull order of sectors in 2018 for graphing purposes
order <- tograph %>% 
  filter(year == 2018) %>% 
  arrange(desc(value)) %>% pull(sector)

#make graph of spatial concentration of sectors
tograph %>% 
  mutate(Sector = factor(sector, levels = order)) %>% 
  ggplot(., aes(x = year, y = value, color = Sector)) + 
  geom_line() + 
  theme_minimal() + 
  scale_x_continuous(breaks = c(2000, 2004, 2008, 2012, 2016, 2020)) + 
  ggtitle("Spatial concentration of UK sectors, NUTS 3") + 
  xlab("Year") + 
  ylab("Herfindahl index") + 
  labs(subtitle = "Calculated using Herfindahl index of industry-level GVA at NUTS-3 level")
  
#now figure out which regions are the most sectorally concentrated 
regions <- uk %>% 
  pivot_longer(cols = `1998`:`2018`, names_to = "year", values_to = "value") %>% 
  group_by(region_code, region_name, year) %>% 
  summarize(concentration = Herfindahl(value, na.rm = TRUE)) %>% 
  mutate(year = as.numeric(year))

#get biggest industry in each region in 2018
uk %>% 
  pivot_longer(cols = `1998`:`2018`, names_to = "year", values_to = "value") %>% 
  filter(year == 2018) %>% 
  group_by(region_name) %>% 
  filter(which(sector_clean) ==)

regions %>%
  ggplot(., aes(x = year, y = concentration, 
                group = region_name, alpha = 0.1)) + 
  geom_line()
  

