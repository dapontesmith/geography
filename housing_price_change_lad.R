setwd("C:/Users/dapon/Dropbox/Harvard/dissertation")

library(tidyverse)

prices <- read_csv("data/uk_geography/housing_data/prices_local_authorities_to_1995.csv")

prices$change_to_1995 <- 0
prices$change_to_1996 <- (prices$price1996 - prices$price1995)/prices$price1995 
prices$change_to_1997 <- (prices$price1997 - prices$price1995)/prices$price1995 
prices$change_to_1998 <- (prices$price1998 - prices$price1995)/prices$price1995 
prices$change_to_1999 <- (prices$price1999 - prices$price1995)/prices$price1995 
prices$change_to_2000 <- (prices$price2000 - prices$price1995)/prices$price1995 
prices$change_to_2001 <- (prices$price2001 - prices$price1995)/prices$price1995 
prices$change_to_2002 <- (prices$price2002 - prices$price1995)/prices$price1995 
prices$change_to_2003 <- (prices$price2003 - prices$price1995)/prices$price1995 
prices$change_to_2004 <- (prices$price2004 - prices$price1995)/prices$price1995 
prices$change_to_2005 <- (prices$price2005 - prices$price1995)/prices$price1995 
prices$change_to_2006 <- (prices$price2006 - prices$price1995)/prices$price1995 
prices$change_to_2007 <- (prices$price2007 - prices$price1995)/prices$price1995 
prices$change_to_2008 <- (prices$price2008 - prices$price1995)/prices$price1995 
prices$change_to_2009 <- (prices$price2009 - prices$price1995)/prices$price1995 
prices$change_to_2010 <- (prices$price2010 - prices$price1995)/prices$price1995 
prices$change_to_2011 <- (prices$price2011 - prices$price1995)/prices$price1995 
prices$change_to_2012 <- (prices$price2012 - prices$price1995)/prices$price1995 
prices$change_to_2013 <- (prices$price2013 - prices$price1995)/prices$price1995 
prices$change_to_2014 <- (prices$price2014 - prices$price1995)/prices$price1995 
prices$change_to_2015 <- (prices$price2015 - prices$price1995)/prices$price1995 
prices$change_to_2016 <- (prices$price2016 - prices$price1995)/prices$price1995 
prices$change_to_2017 <- (prices$price2017 - prices$price1995)/prices$price1995 
prices$change_to_2018 <- (prices$price2018 - prices$price1995)/prices$price1995 
prices$change_to_2019 <- (prices$price2019 - prices$price1995)/prices$price1995 
prices$change_to_2020 <- (prices$price2020 - prices$price1995)/prices$price1995 

plot <- prices %>% 
  pivot_longer(cols = change_to_1995:change_to_2020, names_to = "name",
               values_to = "value")  %>% 
  dplyr::select(-starts_with("price"), -starts_with("X")) %>% 
  mutate(year = as.numeric(substr(as.character(name), 11, 14)), 
         value = 100*value) %>% 
  mutate(London = as.factor(case_when(
    region_name == "London" ~ 1, 
    region_name != "London" ~ 0
  )), 
  Region = case_when(
    region_name == "London" ~ "London",
    region_name %in% c("North West","North East","Yorkshire and The Humber") ~ "North",
    region_name %in% c("East Midlands","West Midlands","East of England") ~ "Midlands", 
    region_name %in% c("South East","South West") ~ "South",
    TRUE ~ region_name
  )) %>% 
  filter(!(is.na(London))) %>% 
  ggplot(.) +  
  geom_line(aes(x = year, y = value, group = lad_name, color = London), alpha = 0.3) + 
  theme_minimal() + 
  xlab("Year") + ylab("Pct change in housing prices since 1995") + 
  ggtitle("Percent change in housing prices, by local authority") + 
  guides(colour = guide_legend(override.aes = list(alpha = 1)))

ggsave("prospectus/figures/uk_housing_change.jpeg", plot)


#read in region price data 
regions <- read_csv("data/uk_geography/housing_data/prices_regions_to_1995.csv")

test <- regions %>% 
  dplyr::select(region_code, region_name, contains("Dec")) 
names(test) <- str_replace(names(test), "Year ending Dec ", "prices_")

tograph <- test %>% 
  mutate(change_to_1995 = 0, 
         change_to_1996 = (prices_1996 - prices_1995) / prices_1995, 
         change_to_1997 = (prices_1997 - prices_1995) / prices_1995,
         change_to_1998 = (prices_1998 - prices_1995) / prices_1995,
         change_to_1999 = (prices_1999 - prices_1995) / prices_1995,
         change_to_2000 = (prices_2000 - prices_1995) / prices_1995,
         change_to_2001 = (prices_2001 - prices_1995) / prices_1995,
         change_to_2002 = (prices_2002 - prices_1995) / prices_1995,
         change_to_2003 = (prices_2003 - prices_1995) / prices_1995,
         change_to_2004 = (prices_2004 - prices_1995) / prices_1995,
         change_to_2005 = (prices_2005 - prices_1995) / prices_1995,
         change_to_2006 = (prices_2006 - prices_1995) / prices_1995,
         change_to_2007 = (prices_2007 - prices_1995) / prices_1995,
         change_to_2008 = (prices_2008 - prices_1995) / prices_1995,
         change_to_2009 = (prices_2009 - prices_1995) / prices_1995,
         change_to_2010 = (prices_2010 - prices_1995) / prices_1995,
         change_to_2011 = (prices_2011 - prices_1995) / prices_1995,
         change_to_2012 = (prices_2012 - prices_1995) / prices_1995,
         change_to_2013 = (prices_2013 - prices_1995) / prices_1995,
         change_to_2014 = (prices_2014 - prices_1995) / prices_1995,
         change_to_2015 = (prices_2015 - prices_1995) / prices_1995,
         change_to_2016 = (prices_2016 - prices_1995) / prices_1995,
         change_to_2017 = (prices_2017 - prices_1995) / prices_1995,
         change_to_2018 = (prices_2018 - prices_1995) / prices_1995,
         change_to_2019 = (prices_2019 - prices_1995) / prices_1995) %>% 
  pivot_longer(cols = change_to_1995:change_to_2019, 
               names_to = "name",values_to = "value") %>% 
  dplyr::select(-starts_with("price"), -starts_with("X")) %>% 
  mutate(year = as.numeric(substr(as.character(name), 11, 14)), 
         value = 100*value) %>%
  filter(!(is.na(region_name)), str_detect(region_name, "England") == FALSE) %>% 
  mutate(Region = region_name) 

#get change in 2019
values2019 <- tograph %>% 
  filter(name == "change_to_2019") %>% 
  dplyr::select(region_name, value) %>% 
  rename(value2019 = value)

region_plot <- tograph %>%
  left_join(., values2019, by = "region_name") %>% 
  arrange(desc(value2019)) %>% 
  #this is for ordering the legend in order of the 2019 values 
  mutate(Region = factor(Region, 
                         levels = c("London","South East","South West",
                                    "East Midlands","West Midlands","North West",
                                    "Wales","Yorkshire and The Humber",
                                    "North East"))) %>% 
  ggplot(.) +  
  geom_line(aes(x = year, y = value, color = Region)) + 
  theme_minimal() + 
  xlab("Year") + ylab("Pct change in housing prices since 1995") + 
  ggtitle("Percent change in housing prices, by region") + 
  guides(colour = guide_legend(override.aes = list(alpha = 1))) 
  
ggsave("prospectus/figures/uk_housing_change_region.jpeg",region_plot)



########################3
#make graph of sd of local authority prices over time
prices %>% 
  pivot_longer(cols = price1995:price2020, 
               names_to = "name", values_to = "value") %>%
  dplyr::select(-X31, -X32) %>% 
  mutate(year = substr(name, 6, 9)) %>% 
  group_by(year) %>% 
  #need to somehow adjust for rising price levels over time? 
  mutate(prices_scale = scale(value)) %>% 
  summarise(spread = sd(prices_scale, na.rm = TRUE)) %>% 
  ungroup() %>% 
  ggplot(.) + 
  geom_line(aes(x = year, y = spread, group = 1))
 