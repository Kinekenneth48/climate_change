
library(tidyterra)
library(terra)
library(ggplot2)

# Temperatures
rastertemp <- rast(system.file("extdata/cyl_temp.tif", package = "tidyterra"))

rastertemp

# Rename
rastertemp <- rastertemp %>%
  rename(April = tavg_04, May = tavg_05, June = tavg_06)

# Facet all layers

ggplot() +
  geom_spatraster(data = rastertemp) +
  facet_wrap(~lyr, ncol = 1) +
  scale_fill_whitebox_c(
    palette = "muted",
    labels = scales::label_number(suffix = "º"),
    n.breaks = 12,
    guide = guide_legend(reverse = TRUE)
  ) +
  labs(
    fill = "",
    title = "Average temperature in Castille and Leon (Spain)",
    subtitle = "Months of April, May and June"
  )


################################################################################

# Create maximum differences of two months
variation <- rastertemp %>%
  mutate(diff = June - May) %>%
  select(variation = diff)

# Add also a overlay of a SpatVector
prov <- vect(system.file("extdata/cyl.gpkg", package = "tidyterra"))

ggplot(prov) +
  geom_spatraster(data = variation) +
  geom_spatvector(fill = NA) +
  scale_fill_whitebox_c(
    palette = "deep", direction = -1,
    labels = scales::label_number(suffix = "º"),
    n.breaks = 5
  ) +
  theme_minimal() +
  coord_sf(crs = 25830) +
  labs(
    fill = "variation",
    title = "Variation of temperature in Castille and Leon (Spain)",
    subtitle = "Average temperatures in June vs. May"
  )




################################################################################
rgb_tile <- rast(system.file("extdata/cyl_tile.tif", package = "tidyterra"))

plot <- ggplot(prov) +
  geom_spatraster_rgb(data = rgb_tile) +
  geom_spatvector(fill = NA) +
  theme_light()

plot


################################################################################


# Recognizes coord_sf()
plot +
  # Change crs and datum (for relabeling graticules)
  coord_sf(crs = 3857, datum = 3857)




################################################################################
vect(system.file("ex/lux.shp", package = "terra")) %>%
  mutate(pop_dens = POP / AREA) %>%
  glimpse() %>%
  autoplot(aes(fill = pop_dens)) +
  scale_fill_whitebox_c(palette = "pi_y_g") +
  labs(
    fill = "population per km2",
    title = "Population density of Luxembourg",
    subtitle = "By canton"
  )





################################################################################
# Get the path to the 'extdata' directory in the 'tidyterra' package
extdata_dir <- system.file("extdata", package = "tidyterra")

# List all files in the 'extdata' directory
files <- list.files(extdata_dir, full.names = TRUE)

# Print the list of files
print(files)
