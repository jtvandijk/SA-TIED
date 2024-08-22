# JTvD 2024

# load 
library(tidyverse)
library(sf)
library(tmap)
library(spdep)

# read
sa_municipality <- st_read('data/spatial/municipality-south-africa-2013.gpkg')
sa_no_schooling <- read_csv('data/attributes/sa-no-schooling.csv')

# prepare
sa_no_schooling <- sa_no_schooling |>
  mutate(mn_prop_no_schooling = mn_no_school / mn_pop)

sa_municipality <- sa_municipality |> 
  left_join(sa_no_schooling, by = c('mn_code' = 'mn_code'))

# coordinates
coords <- st_centroid(sa_municipality, of_largest_polygon = TRUE)

# knn, nb, weights
sa_mn_knn <- knearneigh(coords, k = 5)
sa_mn_nb_knn <- knn2nb(sa_mn_knn)
sa_mn_nb_knn_weights <- sa_mn_nb_knn |>
  nb2listw(style = 'W')

# moran's test
moran <- moran.test(sa_municipality$mn_prop_no_schooling, listw = sa_mn_nb_knn_weights)

# local moran's test
lmoran <- localmoran(sa_municipality$mn_prop_no_schooling, listw = sa_mn_nb_knn_weights)

# extract quadrants
lmoran_quadrants <- attr(lmoran, 'quadr')

# replace values if not significant
lmoran_quadrants[lmoran[, 5] > 0.05, ] <- NA

# replace names
names(lmoran_quadrants) <- c('lmoran_mean_sig', 'lmoran_median_sig', 'lmoran_pysal_sig')

# bind results
sa_municipality <- sa_municipality |>
  cbind(lmoran_quadrants)

# shape, polygons
tm_shape(sa_municipality) +
  
  # specify column, colours
  tm_polygons(
    col = 'lmoran_mean_sig',
    border.col = '#ffffff',
    border.alpha = 0.3,
    palette = c(
      'Low-Low' = '#0571b0',
      'Low-High' = '#92c5de',
      'High-Low' = '#f4a582',
      'High-High' = '#ca0020'
    ),
    title = 'Cluster type',
  ) +
  
  # set layout
  tm_layout(
    legend.outside = FALSE,
    legend.position = c('left', 'top'),
  )
