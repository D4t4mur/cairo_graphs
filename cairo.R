# r script that creates for the known datasets a graphic containing 4 maps with
# the following parameters
# - mean distance to the 5 nearest neighbours
# - area
# - compactness
# - rectangularity



# loading the libraries
library(sf)
library(dplyr)
library(ggplot2)
library(scico)
library(patchwork)
library(grid)
library(maptiles)
library(ggspatial)

# setting the file-paths
Cairo_structured <- "C:/Daten/JMU - Dokumente/Scientific graphs/cairo_project/NewCairo_structured.gpkg"
Cairo_unstructured <- "C:/Daten/JMU - Dokumente/Scientific graphs/cairo_project/NewCairo_unstructured.gpkg"

# loading the polygons
structured <- st_read(Cairo_structured)
unstructured <- st_read(Cairo_unstructured)

# adding a column for polygon origin before merging both datasets into one
structured$origin <- "structured"
unstructured$origin <- "unstructured"

# combining the data
buildings <- rbind(structured, unstructured)

# reprojecting into a metric coordinate reference system (WGS 84 / UTM Zone 36N; EPSG: 32636)
buildings <- st_transform(buildings, crs = 32636)

# adding the area of the buildings
buildings$area <- st_area(buildings) %>% as.numeric()


# # calculating the mean distance to the next 5 buildings with complete distance matrix; time intensive
# distance <- st_distance(buildings)
# diag(distance) <- NA
# buildings$min_dist <- apply(distance, 1, function(x) {
#   x <- sort(x, na.last = NA)
#   mean(x[1:5], na.rm = TRUE)
# })


# setting a preliminary range limit for distance calculation
preliminary_dist <- 150

# listing neighbouring polygons within range for each polygon
neighbours_list <- st_is_within_distance(buildings, buildings, dist = preliminary_dist)

# calculating distance for filtered neighbours and sorting by size; calculating mean for the least five values
min5_mean_dist <- sapply(seq_along(neighbours_list), function(i) {
  others <- setdiff(neighbours_list[[i]], i)
  dists <- st_distance(buildings[i, ], buildings[others, ])
  dists <- sort(as.numeric(dists), na.last = NA)
  if (length(dists) >= 5) {
    mean(dists[1:5])
  } else {
    mean(dists, na.rm = TRUE)
  }
})

# adding values to dataframe
buildings$min_dist <- min5_mean_dist

# calculating the rectangularity rec = Area / (long_side * small_side)
buildings$rectangularity <- buildings$area / (buildings$long_side * buildings$small_side)

# calculating the perimeter and following the compactness com = 4 * pi * Area / perimeter²
buildings$perimeter <- st_length(st_cast(buildings, "MULTILINESTRING")) %>% as.numeric()
buildings$compactness <- 4 * pi * buildings$area / (buildings$perimeter)^2


# two maps with self determined discrete classes
# creating label categories for min5_mean distance
buildings <- buildings %>%
  mutate(
    dist_label = case_when(
      min_dist == 0 ~ "= 0",
      min_dist > 0 & min_dist < 6 ~ "0 - 6",
      min_dist >= 6 & min_dist < 12 ~ "6 - 12",
      min_dist >= 12 & min_dist < 18 ~ "12 – 18",
      min_dist >= 18 & min_dist < 24 ~ "18 – 24",
      min_dist >= 24 & min_dist < 30 ~ "24 – 30",
      min_dist >= 30 & min_dist < 36 ~ "30 – 36",
      min_dist >= 36 ~ "> 36",
      TRUE ~ NA_character_
    ),
    dist_label = factor(
      dist_label,
      levels = c("= 0", "0 - 6", "6 - 12", "12 – 18", "18 – 24", "24 – 30", "30 – 36", "> 36"),
      ordered = TRUE
    )
  )
# creating the corresponding colours
dist_colours <- c(
  "= 0"     = "#d73027",
  "0 - 6"   = "#fc8d59",
  "6 - 12"  = "#fee08b",
  "12 – 18" = "#d9ef8b",
  "18 – 24" = "#a6d96a",
  "24 – 30" = "#66bd63",
  "30 – 36" = "#1a9850",
  "> 36"    = "#006837"
)

# creating label categories for area
buildings <- buildings %>%
  mutate(
    area_label = case_when(
      area > 0 & area < 100 ~ "0 - 100",
      area >= 100 & area < 200 ~ "100 - 200",
      area >= 200 & area < 300 ~ "200 - 300",
      area >= 300 & area < 400 ~ "300 - 400",
      area >= 400 & area < 500 ~ "400 - 500",
      area >= 500 & area < 600 ~ "500 - 600",
      area >= 600 & area < 700 ~ "600 - 700",
      area >= 700 ~ "> 700",
      TRUE ~ NA_character_
    ),
    area_label = factor(
      area_label,
      levels = c(
        "0 - 100", "100 - 200", "200 - 300", "300 - 400",
        "400 - 500", "500 - 600", "600 - 700", "> 700"
      ),
      ordered = TRUE
    )
  )
# creating the corresponding colours
area_colours <- c(
  "0 - 100"   = "#fbf4d8",
  "100 - 200" = "#f5e29d",
  "200 - 300" = "#eccb56",
  "300 - 400" = "#e4b500",
  "400 - 500" = "#bd8c08",
  "500 - 600" = "#946507",
  "600 - 700" = "#613a02",
  "> 700"     = "#341e00"
)


# getting a basemap and necessary bounding box
bbox <- st_bbox(buildings) %>% st_as_sfc() %>% st_buffer(500) %>% st_bbox()
basemap <- get_tiles(bbox, provider = "Esri.WorldImagery", crop = TRUE, zoom = 15)

# creating a map for min5_mean distance with bar chart as legend
map_dist <- ggplot(buildings) +
  layer_spatial(basemap, alpha = 0.4) +
  annotation_scale(location = "bl", width_hint = 0.2) +
  annotation_north_arrow(location = "tl", which_north = "true", 
                         style = north_arrow_orienteering()) +
  geom_sf(aes(fill = dist_label), color = NA, alpha = 0.8) +
  scale_fill_manual(values = dist_colours) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    plot.margin = margin(0, 0, 0, 0),
    panel.grid.major = element_line(color = "black", linewidth = 0.2),
    panel.border = element_rect(color = "black", size = 1.5, fill = NA)
    ) +
  labs(
    subtitle = "Distance to neighbours (mean for 5 buildings)"
  ) +
  coord_sf(expand = FALSE)

hist_dist <- ggplot(buildings, aes(x = dist_label, fill = dist_label)) +
  geom_bar() +
  scale_fill_manual(values = dist_colours) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(size = 6, face = "bold"),
    axis.text.y = element_text(size = 6, face = "bold"),
    axis.title.x = element_text(size = 6, face = "italic"),
    axis.title.y = element_text(size = 6, face = "italic"),
    plot.background = element_rect(fill = "white")
  ) +
  labs(
    x = "Mean distance to 5 nearest buildings in metres",
    y = "Number of buildings"
  )

# fitting the bar chart into the map
layout_dist <- map_dist + inset_element(hist_dist, left = 0.55, bottom = 0.75, right = 0.98, top = 0.98)

# creating a map for area with bar chart as legend
map_area <- ggplot(buildings) +
  layer_spatial(basemap, alpha = 0.4) +
  annotation_scale(location = "bl", width_hint = 0.2) +
  annotation_north_arrow(location = "tl", which_north = "true", 
                         style = north_arrow_orienteering()) +
  geom_sf(aes(fill = area_label), color = NA, alpha = 0.8) +
  scale_fill_manual(values = area_colours) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    plot.margin = margin(0, 0, 0, 0),
    panel.grid.major = element_line(color = "black", linewidth = 0.2),
    panel.border = element_rect(color = "black", size = 1.5, fill = NA)
  ) +
  labs(
    subtitle = "Area per building"
  ) +
  coord_sf(expand = FALSE)

hist_area <- ggplot(buildings, aes(x = area_label, fill = area_label)) +
  geom_bar() +
  scale_fill_manual(values = area_colours) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(size = 6, face = "bold"),
    axis.text.y = element_text(size = 6, face = "bold"),
    axis.title.x = element_text(size = 6, face = "italic"),
    axis.title.y = element_text(size = 6, face = "italic"),
    plot.background = element_rect(fill = "white")
  ) +
  labs(
    x = "Area per building in squaremetres",
    y = "Number of buildings"
  )

# fitting the bar chart into the map
layout_area <- map_area + inset_element(hist_area, left = 0.55, bottom = 0.75, right = 0.98, top = 0.98)

# another two maps with continuous scale as by histogram
# creating a map for compactness with histogram as legend
map_com <- ggplot(buildings) +
  layer_spatial(basemap, alpha = 0.4) +
  annotation_scale(location = "bl", width_hint = 0.2) +
  annotation_north_arrow(location = "tl", which_north = "true", 
                         style = north_arrow_orienteering()) +
  geom_sf(aes(fill = compactness), color = NA, alpha = 0.8) +
  scale_fill_scico(palette = "vik") +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    plot.margin = margin(0, 0, 0, 0),
    panel.grid.major = element_line(color = "black", linewidth = 0.2),
    panel.border = element_rect(color = "black", size = 1.5, fill = NA)
  ) +
  labs(
    subtitle = "Compactness per building"
  ) +
  coord_sf(expand = FALSE)

hist_com <- ggplot(buildings, aes(x = compactness, fill = after_stat(x))) +
  geom_histogram(binwidth = 0.05, color = "white", breaks = seq(0, 1, by = 0.1)) +
  scale_fill_scico(palette = "vik") +
  scale_x_continuous(breaks = seq(0, 1, by = 0.1)) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(size = 6, face = "bold"),
    axis.text.y = element_text(size = 6, face = "bold"),
    axis.title.x = element_text(size = 6, face = "italic"),
    axis.title.y = element_text(size = 6, face = "italic"),
    plot.background = element_rect(fill = "white")
  ) +
  labs(
    x = "Compactness index",
    y = "Number of buildings"
  )

# fitting the bar chart into the map
layout_com <- map_com + inset_element(hist_com, left = 0.55, bottom = 0.75, right = 0.98, top = 0.98)

# creating a map for rectangularity with histogram as legend
map_rec <- ggplot(buildings) +
  layer_spatial(basemap, alpha = 0.4) +
  annotation_scale(location = "bl", width_hint = 0.2) +
  annotation_north_arrow(location = "tl", which_north = "true", 
                         style = north_arrow_orienteering()) +
  geom_sf(aes(fill = rectangularity), color = NA, alpha = 0.8) +
  scale_fill_scico(palette = "tokyo") +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    plot.margin = margin(0, 0, 0, 0),
    panel.grid.major = element_line(color = "black", linewidth = 0.2),
    panel.border = element_rect(color = "black", size = 1.5, fill = NA)
  ) +
  labs(
    subtitle = "Rectangularity per building"
  ) +
  coord_sf(expand = FALSE)

hist_rec <- ggplot(buildings, aes(x = rectangularity, fill = after_stat(x))) +
  geom_histogram(binwidth = 0.1, color = "white", breaks = seq(0, 1, by = 0.1)) +
  scale_fill_scico(palette = "tokyo") +
  scale_x_continuous(breaks = seq(0, 1, by = 0.1)) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(size = 6, face = "bold"),
    axis.text.y = element_text(size = 6, face = "bold"),
    axis.title.x = element_text(size = 6, face = "italic"),
    axis.title.y = element_text(size = 6, face = "italic"),
    plot.background = element_rect(fill = "white")
  ) +
  labs(
    x = "Rectangularity index",
    y = "Number of buildings"
  )

# fitting the bar chart into the map
layout_rec <- map_rec + inset_element(hist_rec, left = 0.55, bottom = 0.75, right = 0.98, top = 0.98)


# combining all four plots in a layout
layout_final <- (layout_dist | layout_area) / (layout_com | layout_rec)

# finalising the layout with headtitle and padding between the graphs
final <- layout_final + 
  plot_annotation(
  title = "Cairo Urban Morphology",
  theme = theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5))
  ) +
  plot_layout() & 
  theme(plot.margin = unit(c(5, 5, 5, 5), "mm"))

# saving the plot as a png
ggsave("Cairo_Urban_Morphology.png", final, width = 22, height = 18, dpi = 600)