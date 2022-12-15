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
