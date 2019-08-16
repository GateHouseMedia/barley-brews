library(tidyverse)
library(janitor)


# barley acres harvested from ag census data

# read in all census files
qs_census2002 <- read_delim("qs.census2002.txt",
                            "\t", escape_double = FALSE, trim_ws = TRUE)

qs_census2007 <- read_delim("qs.census2007.txt",
                            "\t", escape_double = FALSE, col_types = cols(ZIP_5 = col_character(),
                                                                          WATERSHED_DESC = col_character()),
                            trim_ws = TRUE)

qs_census2012 <- read_delim("qs.census2012.txt",
                            "\t", escape_double = FALSE, col_types = cols(CONGR_DISTRICT_CODE = col_character(),
                                                                          REGION_DESC = col_character(), WATERSHED_DESC = col_character(),
                                                                          WEEK_ENDING = col_character(), ZIP_5 = col_character()),
                            trim_ws = TRUE)

qs_census2017 <- read_delim("qs.census2017.txt",
                            "\t", escape_double = FALSE, trim_ws = TRUE)

# minor cleaning
qs_census2002_clean <- qs_census2002 %>% 
  mutate(VALUE = as.numeric(gsub(",", "", VALUE))) %>% # takes out all the (D) values
  clean_names()

qs_census2007_clean <- qs_census2007 %>% 
  mutate(VALUE = as.numeric(gsub(",", "", VALUE))) %>% # takes out all the (D) values
  clean_names()

qs_census2012_clean <- qs_census2012 %>% 
  mutate(VALUE = as.numeric(gsub(",", "", VALUE))) %>% # takes out all the (D) values
  clean_names() 

qs_census2017_clean <- qs_census2017 %>% 
  mutate(VALUE = as.numeric(gsub(",", "", VALUE))) %>% # takes out all the (D) values
  clean_names()

barley2002 <- qs_census2002_clean %>% 
  filter(sector_desc == "CROPS") %>% 
  filter(agg_level_desc == "STATE") %>% 
  filter(statisticcat_desc == "PRODUCTION") %>% 
  filter(commodity_desc == "BARLEY") %>% 
  filter(domain_desc == "TOTAL")

barley2007 <- qs_census2007_clean %>% 
  filter(sector_desc == "CROPS") %>% 
  filter(agg_level_desc == "STATE") %>% 
  filter(statisticcat_desc == "PRODUCTION") %>% 
  filter(commodity_desc == "BARLEY") %>% 
  filter(domain_desc == "TOTAL") 

barley2012 <- qs_census2012_clean %>% 
  filter(sector_desc == "CROPS") %>% 
  filter(agg_level_desc == "STATE") %>% 
  filter(statisticcat_desc == "PRODUCTION") %>% 
  filter(commodity_desc == "BARLEY") %>% 
  filter(domain_desc == "TOTAL")

barley2017 <- qs_census2017_clean %>% 
  filter(sector_desc == "CROPS") %>% 
  filter(agg_level_desc == "STATE") %>% 
  filter(statisticcat_desc == "PRODUCTION") %>% 
  filter(commodity_desc == "BARLEY") %>% 
  filter(domain_desc == "TOTAL")

bind_state_barley <- bind_rows(barley2002, barley2007, barley2012, barley2017)

# clean spreadsheet for markets
state_barley <- bind_state_barley %>% 
  select(commodity_desc, value, unit_desc, statisticcat_desc, state_name, year) %>% 
  spread(year, value)

write_csv(state_barley, "state_barley_production.csv")

bind_state_barley %>% 
  filter(year == "2002") %>% 
  arrange(desc(value)) %>% View()

bind_state_barley %>%
  select(commodity_desc, value, unit_desc, statisticcat_desc, state_name, year) %>% 
  spread(year, value) %>% 
  mutate(`2002` = replace_na(`2002`, 0)) %>% 
  mutate(`2007` = replace_na(`2007`, 0)) %>% 
  mutate(`2012` = replace_na(`2012`, 0)) %>% 
  mutate(`2017` = replace_na(`2017`, 0)) %>% 
  mutate(`2017-2002` = `2017` - `2002`) %>% 
  mutate(`2017-2007` = `2017` - `2007`) %>% 
  mutate(`2017-2012` = `2017` - `2012`) %>% 
  mutate(`2007-2002` = `2007` - `2002`) %>% 
  mutate(`2012-2007` = `2012` - `2007`) %>% 
  mutate(`2017-2002_percentchange` = abs(`2017-2002`) / `2017` *100) %>% 
  mutate(`2017-2007_percentchange` = abs(`2017-2007`) / `2017` *100) %>% 
  mutate(`2017-2012_percentchange` = abs(`2017-2012`) / `2017` *100) %>% 
  mutate(`2007-2002_percentchange` = abs(`2007-2002`) / `2007` *100) %>% 
  mutate(`2012-2007_percentchange` = abs(`2012-2007`) / `2012` *100) %>% View()

bind_state_barley %>% 
  filter(state_name == "NORTH DAKOTA") %>% 
  select(year, value)

bind_state_barley %>% 
  filter(state_name == "NORTH DAKOTA" |
         state_name == "MONTANA" |
         state_name == "IDAHO") %>% 
  ggplot(aes(year, value, group = state_name, color = state_name)) +
  geom_line()+
  scale_x_continuous(breaks = c(2002, 2007, 2012, 2017))

# graphic
barleygraphic1 <- bind_state_barley %>% 
  filter(state_name == "NORTH DAKOTA" |
           state_name == "MONTANA" |
           state_name == "IDAHO") %>% 
  select(state_name, year, value, unit_desc)

write_csv(barleygraphic1, "barleygraphic1.csv")

total_barley2002 <- qs_census2002_clean %>% 
  filter(sector_desc == "CROPS") %>% 
  filter(agg_level_desc == "NATIONAL") %>% 
  filter(statisticcat_desc == "PRODUCTION") %>% 
  filter(commodity_desc == "BARLEY") %>% 
  filter(domain_desc == "TOTAL")

total_barley2007 <- qs_census2007_clean %>% 
  filter(sector_desc == "CROPS") %>% 
  filter(agg_level_desc == "NATIONAL") %>% 
  filter(statisticcat_desc == "PRODUCTION") %>% 
  filter(commodity_desc == "BARLEY") %>% 
  filter(domain_desc == "TOTAL")

total_barley2012 <- qs_census2012_clean %>% 
  filter(sector_desc == "CROPS") %>% 
  filter(agg_level_desc == "NATIONAL") %>% 
  filter(statisticcat_desc == "PRODUCTION") %>% 
  filter(commodity_desc == "BARLEY") %>% 
  filter(domain_desc == "TOTAL")

total_barley2017 <- qs_census2017_clean %>% 
  filter(sector_desc == "CROPS") %>% 
  filter(agg_level_desc == "NATIONAL") %>% 
  filter(statisticcat_desc == "PRODUCTION") %>% 
  filter(commodity_desc == "BARLEY") %>% 
  filter(domain_desc == "TOTAL")

bind_total_barley <- bind_rows(total_barley2002, total_barley2007, total_barley2012, total_barley2017)

bind_total_barley %>% 
  ggplot(aes(year, value, group = 1)) +
  geom_line()+
  scale_x_continuous(breaks = c(2002, 2007, 2012, 2017))

# ers barley data
barley_supplydiss <- read_csv("barley_supply_disappearance.csv", skip = 3, skip_empty_rows = TRUE) %>% 
  remove_empty(which = c("rows", "cols")) %>% 
  rename(year = "X1") %>% 
  rename(quarter = "X2") %>% 
  clean_names() %>% 
  rename(total_supply = "total_supply_2") %>% 
  rename(total_disappearance = "total_disappearance_2") %>% 
  rename(total_domestic_use = "total_domestic_use_2") %>% 
  fill(year)
  
byyear_barley_supplydiss <- barley_supplydiss %>% 
  filter(str_detect(quarter, "MY"))

byyear_barley_supplydiss %>% 
  ggplot(aes(year, production, group = 1)) +
  geom_line()

# graphic  
barleygraphic2 <- byyear_barley_supplydiss %>% 
  select(year, production)

write_csv(barleygraphic2, "barleygraphic2.csv")

graphic3 <- byyear_barley_supplydiss %>% 
  select(year, total_disappearance, food_alcohol_and_industrial_use, feed_and_residual_use) %>% 
  mutate(percent_food_alcohol_industrial = (food_alcohol_and_industrial_use/total_disappearance)*100) %>% 
  mutate(percent_feed = (feed_and_residual_use/total_disappearance)*100)

graphic3 %>% 
  ggplot()+
  geom_line(aes(x = year, y = percent_food_alcohol_industrial), color = "blue", group = 1) +
  geom_line(aes(x = year, y = percent_feed), color = "red", group = 1)

graphic3 %>% 
  ggplot()+
  geom_line(aes(x = year, y = food_alcohol_and_industrial_use), color = "blue", group = 1) +
  geom_line(aes(x = year, y = feed_and_residual_use), color = "red", group = 1)

write_csv(graphic3, "barleygraphic3.csv")
