setwd("C:/Users/dapon/Dropbox/Harvard/dissertation")
library(tidyverse)
library(DescTools)
raw <- read_csv("data/us_econ/CAGDP9_MSA_2001_2019.csv")
pop <- read_csv("data/us_econ/pop_metro_to_2019.csv")

#clean pop a little 
pop <- pop %>% 
  mutate(geo = str_remove(geo, ".")) %>% 
  filter(str_detect(geo, "Metro Area") == TRUE) %>% 
  mutate(geo = str_remove(geo, " Metro Area")) %>% 
  select(geo, census2010)


df <- raw %>% 
  #this selection of varaibles is pretty finely-grained
  #using this introduces a fair amount of NAs, since many places are missing 
  #this finely grained data
  #filter(Description %in% c("All industry total", "Agriculture, forestry, fishing and hunting",
   #                         "Mining, quarrying, and oil and gas extraction", "Utilities",
    #                        "Construction","Manufacturing","Wholesale trade", "Retail trade",
     #                       "Transportation and warehousing","Information","Finance and insurance",
      #                      "Real estate and rental and leasing","Professional, scientific, and technical services",
       #                     "Professional and business services",
        #                    "Administrative and support and waste management and remediation services",
         #                   "Educational services","Health care and social assistance",
          #                  "Arts, entertainment, and recreation","Accomodation and food services",
           #                 "Other services (except government and government enterprises)",
            #                "Government and government enterprises")) %>%
  
  #an alternative selection here is less finely-grained
  filter(Description %in% c("Agriculture, forestry, fishing and hunting", 
                            "Mining, quarrying, and oil and gas extraction", 
                            "Utilities","Construction","Manufacturing", "Trade", 
                            "Arts, entertainment, recreation, accommodation, and food services", 
                            "Transportation and warehousing", "Information",
                            "Finance, insurance, real estate, rental, and leasing", 
                            "Professional and business services", 
                            "Educational services, health care, and social assistance",
                            "Other services (except government and government enterprises)",
                            "Government and government enterprises", 
                            "All industry total"
                            )) %>% 
  mutate(Description = case_when(
    Description == "Agriculture, forestry, fishing and hunting" ~ "Agriculture",
    Description == "Mining, quarrying, and oil and gas extraction" ~ "Mining and oil", 
    Description == "Other services (except government and government enterprises)"~ "Other services",
    Description == "Government and government enterprises" ~ "Government", 
    Description == "Arts, entertainment, recreation, accommodation, and food services" ~ 
      "Arts, entertainment, rec., accom., food", 
    Description == "Finance, insurance, real estate, rental, and leasing" ~ 
      "Finance, insurance, real estate", 
    Description == "Educational services, health care, and social assistance" ~ 
      "Education, health, social assistance",
    TRUE ~ Description
  )) %>% 
  select(-Unit, -TableName, -LineCode) %>% 
  mutate(geo = str_remove(GeoName, "(Metropolitan Statistical Area)")) %>% 
  mutate(geo = str_replace(geo, " \\(\\)", ""))

long <- df %>% 
  pivot_longer(cols = `2001`:`2019`, names_to = "year", values_to = "gdp") %>% 
  mutate(gdp = as.numeric(gdp)) %>% 
  select(-GeoName) %>%
  #make indicator for what percentage of the industires are missing data
  group_by(geo, year) %>% 
  mutate(pct_missing = sum(is.na(gdp))/n()) 
  #use ones where only 1 or 2 are missing?  
  #filter(pct_missing < 0.3)
  


#get sectoral concentration of each MSA
geo_concen <- long %>% 
  group_by(geo, GeoFIPS, year) %>% 
  mutate(gdp_total = gdp[Description == "All industry total"]) %>% 
  filter(Description != "All industry total") %>% 
  mutate(sector_share = gdp / gdp_total) %>% 
  ungroup() %>% group_by(geo, GeoFIPS, year) %>% 
  mutate(total = sum(sector_share, na.rm = TRUE)) %>% 
  #filter out entry if we don't have more than 3/4 of the economy
  filter(total > 0.75) %>% 
  summarize(concen = Herfindahl(sector_share, na.rm = TRUE)) %>% 
  filter(concen != 0)
write.csv(geo_concen, file = "data/us_econ/sectoral_concentration_MSA.csv")

#get geographic concentration of each sector
sector_concen <- long %>% 
  filter(Description != "All industry total") %>% 
  group_by(Description, year) %>% 
  summarise(concen = Herfindahl(gdp, na.rm = TRUE)) %>% 
  mutate(year = as.numeric(year))
write.csv(sector_concen, file = "data/us_econ/geo_concen_of_sector.csv")
sector_concen_2019 <- sector_concen %>% 
  filter(year == 2019) %>% 
  arrange(desc(concen)) %>% pull(Description)

#graph of geographic concentration of sectors
plot <- sector_concen %>% 
  mutate(Sector = factor(Description, levels = sector_concen_2019)) %>% 
  ggplot(aes(x = year, y = concen, color = Sector)) +
  geom_line() + 
  labs(title = "Geographic concentration of sectors in the United States",
       subtitle = "Herfindahl index of sectoral GDP at MSA level", 
       x = "Year", y = "Geographic concentration") + 
  theme_minimal()
ggsave("prospectus/figures/us_sector_geo_concentration_msa.pdf", 
       plot)


#Note: seems like lots of the most sectorally concentrated ones 
#are counties with large military bases (Hinesville GA, Jacksonville NC)

#get biggest sector in 2019 
biggest <- df %>% 
  pivot_longer(cols = `2001`:`2019`, names_to = "year", values_to = "value") %>% 
  filter(year == 2019) %>% 
  group_by(geo) %>% 
  mutate(biggest = ifelse(value == max(value), 1, 0)) %>%
  filter(biggest == 1) 
biggest <- cbind(biggest$GeoName, biggest$Description) %>% 
  as.data.frame() %>% 
  rename(geo = V1, sector = V2) %>% 
  mutate(geo = str_remove(geo, "(Metropolitan Statistical Area)")) %>% 
  mutate(geo = str_replace(geo, " \\(\\)", ""))
#join this with sector_concen
geo_concen <- left_join(geo_concen, biggest, by = "geo") 

#merge with population data
join <- left_join(geo_concen, pop, by = "geo")
#make variable for whether MSA has over 1000000
join <- join %>% 
  mutate(large = as.factor(ifelse(census2010 > 1000000, 1, 0)))

test <- join %>% 
  filter(census2010 > 1000000)


#make cleaned MSA variable
test$msa_clean<- NULL
for(i in 1:length(test$geo)){
  print(i)
 test$msa1_clean[i] <- str_split(test$geo[i], "-")[[1]][1]
 test$msa1_clean[i] <- str_split(test$msa1_clean[i],",")[[1]][1]
}

top_msa <- test %>%
  filter(year == 2019) %>%
  arrange(desc(concen)) %>% pull(msa1_clean)
test <- test %>%
  mutate(MSA = factor(msa1_clean, levels = top_msa)) %>% 
  filter(!is.na(MSA))


msa_concen_plot <- test %>% 
  #filter(MSA != "Cincinnati") %>% 
  ggplot(., aes(x = year, y = concen, group = 1)) + 
  geom_line() +
  facet_wrap(~ MSA) + 
  theme(legend.text=element_text(size=7)) + 
  scale_x_discrete(breaks = c(2005, 2010, 2015)) + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 330)) + 
  labs(title = "Sectoral concentration of 40 largest MSAs (Herfindahl index of sectoral GDP)", 
       x = "Year", y = "Sectoral concentration") 
ggsave("prospectus/figures/us_concen_of_msa.pdf", 
       msa_concen_plot)







