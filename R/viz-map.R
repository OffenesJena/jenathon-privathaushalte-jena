library(rgdal)

# ortsteile <- rgdal::readOGR("data/ortsteile.geojson", "OGRGeoJSON")
statbez   <- rgdal::readOGR("data/statbez.geojson", "OGRGeoJSON")

# Convert to data frame readable by ggplot2
statbez_shp_points <- fortify(statbez, region = "id")


map_data <- statbez_shp_points %>%
  dplyr::left_join(viz_data_sum, by = c("id" = "reg_id"))

# geom_polygon doesn't respect holes. They aren't cut out. Workaround: forced
# layering.
# shapes_with_holes <- unique(map_data[map_data$hole == "TRUE", "id"])

# Cut one family house share into interval
map_data <- map_data %>% 
  mutate(chg_ratio_interval = cut(map_data$chg_ratio, 5))


ggplot() +
  # geom_polygon(
  #   data = subset(map_data, id %in% shapes_with_holes),
  #   aes(x = long, y = lat, group = group, fill = fill),
  #   color = "grey70", size = 0.1) +
  geom_polygon(
    data = map_data,
    aes(x = long, y = lat, group = group, fill = chg_ratio_interval),
    color = "grey35", size = 0.251) +
  coord_equal() +
  scale_y_continuous(labels = NULL, breaks = NULL) +
  scale_x_continuous(labels = NULL, breaks = NULL) +
  labs(title = "Veränderung Anteil Einpersonenhaushalte 60plus 2007 bis 2016 (Projektion noch nicht i.O.)", 
       x = NULL, y = NULL) +
  scale_fill_brewer(name = "Relative Veränderung", palette = "OrRd") +
  # scale_fill_manual(
  #   values = c("transparent", "#1abc9c"),
  #   # labels = c(value_min),
  #   na.translate = FALSE,
  #   drop = TRUE) +
  # guides(fill = "none") +
  theme_classic()
ggsave("figures/7 plot.png", width = 12, height = 8)
