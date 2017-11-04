library(ggplot2)

source("R/func.R")

# Change by year and district ---------------------------------------------

viz_data <- hh_final %>%
  filter(
  # reg_id == "100" 
  grp == "1pers_60plus")

# V1
ggplot(viz_data, aes(x = year, y = chg_ratio)) +
  geom_col() +
  facet_grid(reg_id ~ .) +
  labs(title = "Veränderung Anteil Einpersonenhaushalte 60plus")
ggsave("figures/1 plot.png", width = 12, height = 8)

# V2
ggplot(viz_data, aes(x = year, y = chg_ratio)) +
  geom_col(aes(fill = if_else(chg_ratio > 0, "p", "n"))) +
  facet_grid(reg_id ~ .) +
  scale_fill_manual(values = c("p" = "#2980b9", "n" = "#c0392b")) +
  guides(fill = "none") +
  labs(title = "Veränderung Anteil Einpersonenhaushalte 60plus")
ggsave("figures/2 plot.png", width = 12, height = 8)


# V3
ggplot(viz_data, aes(x = year, y = chg_ratio)) +
  geom_col(aes(fill = if_else(chg_ratio > 0, "p", "n"))) +
  facet_grid(reg_id ~ ., scales = "free_y", space = "free_y") +
  scale_fill_manual(values = c("p" = "#2980b9", "n" = "#c0392b")) +
  guides(fill = "none") +
  labs(title = "Veränderung Anteil Einpersonenhaushalte 60plus")
ggsave("figures/3 plot.png", width = 12, height = 8)


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
  theme_void() +
  labs(title = "Veränderung Anteil Einpersonenhaushalte 60plus")
ggsave("figures/4 plot.png", width = 12, height = 8)

# V5
ggplot(viz_data_v4, aes(x = year, y = chg_ratio)) +
  geom_col(aes(fill = if_else(chg_ratio > 0, "p", "n"))) +
  facet_grid(reg_name ~ ., scales = "free_y", space = "free_y", switch = "y") +
  scale_fill_manual(values = c("p" = "#2ecc71", "n" = "#e74c3c")) +
  guides(fill = "none") +
  labs(title = "Veränderung Anteil Einpersonenhaushalte 60plus") +
  theme_minimal() +
  theme(
    strip.text.y = element_text(angle = 180, hjust = 1),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.text.y = element_blank())
ggsave("figures/5 plot.png", width = 12, height = 8)




# Sum 2007 - 2016 ---------------------------------------------------------

viz_data_sum <- viz_data_v4 %>% 
  group_by(reg_id, reg_name, grp) %>% 
  summarise(chg_ratio = sum(chg_ratio)) %>% 
  arrange(desc(chg_ratio))

ggplot(viz_data_sum, aes(x = reorder_by(reg_name, chg_ratio), y = chg_ratio)) +
  geom_bar(stat = "identity", aes(fill = if_else(chg_ratio > 0, "p", "n"))) +
  coord_flip() +
  scale_fill_manual(values = c("p" = "#2ecc71", "n" = "#e74c3c")) +
  guides(fill = "none") +
  labs(
    title = "Veränderung Anteil Einpersonenhaushalte 60plus 2007 bis 2016", 
    x = NULL, 
    y = "Veränderung Anteil Einpersonenhaushalte 60plus") +
  theme_minimal() +
  theme(
    strip.text.y = element_text(angle = 180, hjust = 1),
    panel.grid.major.y = element_blank(),
    # panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank())
ggsave("figures/6 plot.png", width = 12, height = 8)


  
