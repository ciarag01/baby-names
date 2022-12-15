df <- alice

df <- data.frame(ID = c(rep("1", 6), rep("2", 6), 
                        rep("3", 6), rep("4", 6)), 
                 response=c(0,0,0,1,0,0,1,0,0,0,0,1,0,0,0,1,1,0,0,0,0,1,1,0))  

df_new <- df %>%
  group_by(ID, grp = cumsum(response == 1L)) %>%
  mutate(counter = if(first(response) == 1L) row_number() - 1 
         else row_number()) %>%
  ungroup() %>%
  dplyr::select(-grp)

df <- alice %>% 
  mutate(consec_year = 0) %>% 
  mutate(consec_year = if_else(year == lag(year, 1) + 1, consec_year, 1)) 

df$consec_year[1] <- 0

df_new <- df %>%
  group_by(name, grp = cumsum(consec_year == 1L)) %>%
  mutate(counter = if(first(consec_year) == 1L) row_number() - 1 
         else row_number()) %>%
  ungroup() %>%
  group_by(name, grp) %>% 
  mutate(start_year = min(year)) %>% 
  mutate(end_year = max(year)) %>% 
  ungroup()

new_time_f <- time_f %>% 
  mutate(consec_year = 0) %>% 
  mutate(consec_year = if_else(year == lag(year, 1) + 1, consec_year, 1)) 

new_time_f$consec_year[1] <- 0

new_time_f %<>%
  group_by(name, grp = cumsum(consec_year == 1L)) %>%
  mutate(counter = if(first(consec_year) == 1L) row_number() - 1 
         else row_number()) %>%
  ungroup() %>%
  group_by(name, grp) %>% 
  mutate(start_year = min(year)) %>% 
  mutate(end_year = max(year)) %>% 
  ungroup()

# new_time_f %<>%
#   group_by(name, grp, start_year, end_year) %>% 
#   summarise(n = sum(n)) %>% 
#   ungroup()

ggplot(new_time_f %>% 
         filter(name == "Alice") %>% 
         mutate(name_f = as.factor(name)) %>% 
         mutate(name_f = fct_reorder(name, desc(name)))) +
 # geom_line(size = 10, show.legend = FALSE) +
  geom_point(aes(start_year, name_f, colour = name_f, group = grp), show.legend = FALSE) +
  geom_point(aes(end_year, name_f, colour = name_f), show.legend = FALSE) +
  labs(x="Project year", y=NULL, title="Project timeline") +
  theme_minimal()

check <- time_f %>% 
  select(-n) %>% 
  left_join(new_time_f %>% select(-n), by = c("year", "name")) %>% 
  select(year, name, grp, start_year, end_year)
#%>% 
  #pivot_wider(id_cols = name, names_from = year, values_from = n) %>% 
  filter(name == "Alice")
  #%>% 
  pivot_longer(cols = -c(name), names_to = "year", values_to = "n") %>% 
  arrange(year) %>% 
  mutate(year = if_else(is.na(n), "9999", year)) %>% 
  naniar::replace_with_na(replace = list(year = "9999"))

ggplot(time_m %>% 
         mutate(name_f = as.factor(name)) %>% 
         mutate(name_f = fct_reorder(name, desc(name))), 
       aes(year, name_f, colour = name_f, group = grp)) + 
  geom_line(show.legend = FALSE) + 
  geom_point(aes(start_year, name_f, colour = name_f, group = grp), 
             show.legend = F) +
  geom_point(aes(end_year, name_f, colour = name_f, group = grp), 
             show.legend = F)
