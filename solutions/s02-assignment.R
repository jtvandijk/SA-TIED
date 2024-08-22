# JTvD 2024

# load
library(tidyverse)
library(haven)
library(sf)
library(tmap)

# read
jhb_att <- read_dta('data/attributes/sa-internet-access.dta')
jhb_shp <- st_read('data/spatial/subplace-johannesburg-2013.gpkg')

# prepare
jhb_att <- jhb_att |>
  filter(dc_name == 'City of Johannesburg') 

jhb_att <- jhb_att |>
  rowwise() |>
  mutate(sal_pop = sum(across(starts_with('inter')), na.rm = TRUE))

jhb_att <- jhb_att |>
  group_by(sp_code) |>
  mutate(sp_pop = sum(sal_pop), 
        sp_inet = sum(inter_1)) |>
  ungroup() |>
  distinct(sp_code, sp_pop, sp_inet) |>
  mutate(sp_inet_prop = sp_inet/sp_pop) |>
  mutate(sp_code = as.character(sp_code))

# join
jhb_shp <- jhb_shp |>
  left_join(jhb_att, by = c('sp_code' = 'sp_code'))

# shape, polygons
tm_shape(jhb_shp) +
  
  # specify column, colours
  tm_polygons(
    col= 'sp_inet_prop',
    border.col = '#ffffff',
    border.alpha = 0.3,
    title = 'Internet at home',
  ) +
  
  # set layout
  tm_layout(
    legend.outside = TRUE,
    legend.position = c('left', 'top'),
  ) 