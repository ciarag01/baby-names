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


##############################################.
# LOAD DATA ----
##############################################.
names <- babynames
top_state <- readRDS("data/top_name_per_state.rds")


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


# Top names by state
top_state %<>%
  select(-state_abb, -counter)

al <- top_state %>% 
  filter(state_name == "New York") %>% 
  group_by(name, sex) %>% 
  summarise(n = sum(n)) %>% 
  ungroup()


ggplot(al, aes(label = name, size = n, colour = sex)) +
  geom_text_wordcloud() +
  scale_size_area(max_size = 35) +
  theme_minimal()

# # Unisex Names
# uni <- names %>% 
#   select(name, year, sex, n) %>% 
#   pivot_wider(id_cols = name:year, names_from = sex, values_from = n) %>% 
#   mutate(female = if_else(!(is.na(`F`)), 1, 0), 
#          male = if_else(!(is.na(`M`)), 1, 0)) %>% 
#   mutate(both = if_else(female == 1 & male == 1, 1, 0)) %>% 
#     group_by(name) %>%
#     mutate(first_year = min(year)) %>%
#     ungroup() %>%
#     arrange(name) %>% 
#   mutate(female = if_else(both == 1, 0, female), 
#          male = if_else(both == 1, 0, male)) %>% 
#   group_by(name, first_year) %>% 
#   summarise(female = sum(female), 
#             male = sum(male), 
#             both = sum(both)) %>% 
#   ungroup()


# First Appearance
# first <- names %>% 
#   group_by(name, sex) %>% 
#   summarise(first_year = min(year)) %>% 
#   ungroup() %>% 
#   arrange(name)
# 
# top_50 <- names %>% 
#   group_by(year, sex) %>% 
#   mutate(rank = rank(prop)) %>% 
#   ungroup() %>% 
#   group_by(year, sex) %>% 
#   filter(rank >= max(rank) - 19 & rank <= max(rank)) %>%
#   ungroup() %>% 
#   select(-rank) %>% 
#   left_join(first, by = c("name", "sex")) %>% 
#   group_by(name, sex, first_year) %>% 
#   summarise(n = sum(n)) %>% 
#   ungroup()
# 
# 
# ggplot(top_50 %>% filter(sex == "F"), 
#        aes(x = first_year, y = n, colour = name, label = name)) +
#   geom_point(aes(label = name), show.legend = FALSE) 
# 
# +
#   geom_text(aes(x = first_year, y =n, label = name, colour = name),
#             size = 4,
#             family = "JetBrains Mono", 
#             show.legend = FALSE) 
