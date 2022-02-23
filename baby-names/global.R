##############################################.
# GLOBAL ----
##############################################.

##############################################.
# LOAD PACKAGES ----
##############################################.
library(dplyr) #data manipulation
library(plotly) #charts
library(shiny)
library(shinyWidgets)
library(tidyr)
library(magrittr)
library(readr)
library(janitor)
library(plotly)
library(forcats)
library(babynames)
library(ggwordcloud)
library(maps)
library(ggplot2)
library(mapdata)
library(stringr)
library(maptools)
library(rvest)  


##############################################.
# LOAD DATA ----
##############################################.
names <- babynames

# Top name in each year by sex and state
top_state <- readRDS("baby-names/data/top_name_per_state.rds") %>% 
  select(-state_abb, -counter)

# State shapefile for map
state <- map_data("state") %>% 
  select(long, lat, region) 
# 
# %>% 
#   group_by(region) %>% 
#   mutate(centre_lat = weighted.mean(lat), 
#          centre_long = weighted.mean(long)) %>% 
#   ungroup()

# Pull data for centre of each state from wikipedia
URL <- "https://en.wikipedia.org/wiki/List_of_geographic_centers_of_the_United_States"

temp <- URL %>% 
  html %>%
  html_nodes("table")

state_centres <- html_table(temp[2][[1]]) %>% 
  clean_names() %>% 
  select(-location) %>% 
  separate(coordinates, c("x1","x2", "x3", "x4"), sep = "([/(])") %>% 
  select(-c(x1, x2, x4)) %>% 
  separate(x3, c("centre_lat","centre_long"), sep = "([;])") %>% 
  mutate(state_name_lower = str_to_lower(state_orfederal_district)) %>% 
  select(-state_orfederal_district) %>% 
  mutate(centre_lat = as.numeric(centre_lat), 
         centre_long = as.numeric(substr(trimws(centre_long), 1, 
                                          nchar(trimws(centre_long)) - 1)))


##############################################.
# CREATE DATA FOR DASHBOARD ----
##############################################.
# Top 10 male/female names for each year
most_pop <- names %>% 
  group_by(year, sex) %>% 
  mutate(rank = rank(prop)) %>% 
  ungroup() %>% 
  group_by(year, sex) %>% 
  filter(rank >= max(rank) - 9 & rank <= max(rank)) %>%
  ungroup() %>% 
  select(-rank)


# Top Male/Female name each Decade
top <- names %>% 
  mutate(decade = year - year %% 10) %>% 
  group_by(year, sex) %>%
  filter(prop == max(prop)) %>%
  ungroup()

# Time spent in top 10
time_f <- most_pop %>% 
  filter(sex == "F") %>% 
  arrange(name, year) %>% 
  select(year, name, n) %>% 
  mutate(consec_year = 0) %>% 
  mutate(consec_year = if_else(year == lag(year, 1) + 1, consec_year, 1)) 

time_m <- most_pop %>% 
  filter(sex == "M") %>% 
  arrange(name, year) %>% 
  select(year, name, n) %>% 
  mutate(consec_year = 0) %>% 
  mutate(consec_year = if_else(year == lag(year, 1) + 1, consec_year, 1))

time_f$consec_year[1] <- 0
time_m$consec_year[1] <- 0

new_time_f <- time_f %>%
  group_by(name, grp = cumsum(consec_year == 1L)) %>%
  mutate(counter = if(first(consec_year) == 1L) row_number() - 1 
         else row_number()) %>%
  ungroup() %>%
  group_by(name, grp) %>% 
  mutate(start_year = min(year)) %>% 
  mutate(end_year = max(year)) %>% 
  ungroup()

new_time_m <- time_m %>%
  group_by(name, grp = cumsum(consec_year == 1L)) %>%
  mutate(counter = if(first(consec_year) == 1L) row_number() - 1 
         else row_number()) %>%
  ungroup() %>%
  group_by(name, grp) %>% 
  mutate(start_year = min(year)) %>% 
  mutate(end_year = max(year)) %>% 
  ungroup()

time_f %<>% 
  select(-n) %>% 
  left_join(new_time_f %>% select(-n), by = c("year", "name")) %>% 
  select(year, name, grp, start_year, end_year)

time_m %<>% 
  select(-n) %>% 
  left_join(new_time_m %>% select(-n), by = c("year", "name")) %>% 
  select(year, name, grp, start_year, end_year)

# Unique names
unique_both <- names %>% 
  group_by(year, name) %>% 
  summarise(n = sum(n)) %>% 
  ungroup() %>% 
  group_by(year) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  mutate(sex = "All")

unique <- names %>% 
  group_by(year, sex, name) %>% 
  summarise(n = sum(n)) %>% 
  ungroup() %>% 
  group_by(year, sex) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  bind_rows(unique_both)

ggplot(unique, aes(x = year, y = n, colour = sex)) +
  geom_line() 

# Top names by state - map
map_data <- top_state %>% 
  filter(year == 1993 & sex == "F") %>% 
  group_by(name, year, state_name) %>% 
  summarise() %>% 
  ungroup() %>% 
  mutate(state_name_lower = str_to_lower(state_name)) 

state %<>%  
  left_join(state_centres, by = c("region" = "state_name_lower")) %>% 
  left_join(map_data, by = c("region" = "state_name_lower"))

ggplot(data=state, aes(x=long, y=lat, fill= region, 
                       label = name)) + 
  geom_polygon(color = "white") + 
  guides(fill=FALSE) + 
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) + 
  ggtitle('U.S. Map with States') + 
  coord_fixed(1.3) +
 geom_text(aes(x = centre_long, y = centre_lat, label = name), 
           size = 3)


