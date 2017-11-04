library(ggplot2)

viz_data <- hh_final %>%
  filter(
  # reg_id == "100" 
  grp == "1pers_60plus")

# V1
ggplot(viz_data, aes(x = year, y = chg_ratio)) +
  geom_col() +
  facet_grid(reg_id ~ .)

# V2
ggplot(viz_data, aes(x = year, y = chg_ratio)) +
  geom_col(aes(fill = if_else(chg_ratio > 0, "p", "n"))) +
  facet_grid(reg_id ~ .) +
  scale_fill_manual(values = c("p" = "#2980b9", "n" = "#c0392b")) +
  guides(fill = "none")

# V3
ggplot(viz_data, aes(x = year, y = chg_ratio)) +
  geom_col(aes(fill = if_else(chg_ratio > 0, "p", "n"))) +
  facet_grid(reg_id ~ ., scales = "free_y", space = "free_y") +
  scale_fill_manual(values = c("p" = "#2980b9", "n" = "#c0392b")) +
  guides(fill = "none")

# V4
viz_data_v4 <- viz_data %>% 
  # < 100 Haushalte
  filter(!reg_id %in% c("142", "152", "161", "162", "182", "183", "192")) %>% 
  # < 250 Haushlate
  filter(!reg_id %in% c("11", "21", "71", "111", "133", "141", "151", "201"))

ggplot(viz_data_v4, aes(x = year, y = chg_ratio)) +
  geom_col(aes(fill = if_else(chg_ratio > 0, "p", "n"))) +
  facet_grid(reg_id ~ ., scales = "free_y", space = "free_y") +
  scale_fill_manual(values = c("p" = "#2ecc71", "n" = "#e74c3c")) +
  guides(fill = "none") +
  theme_void()
