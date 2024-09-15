# JTvD 2024

# load 
library(tidyverse)
library(sf)
library(tmap)
library(spdep)
library(GWmodel)

# read
wrd <- st_read('data/spatial/ward-western-cape-2011.gpkg')
edu <- read_csv('data/attributes/sa-ward-education.csv')
inc <- read_csv('data/attributes/sa-ward-income.csv')

# prepare
edu <- edu |>
  rowwise() |>
  mutate(wrd_edu = sum(across(25:30), na.rm = TRUE),
         ward_id = as.character(ward_id)) |>
  select(ward_id, wrd_edu)

inc <- inc |>
  rowwise() |>
  mutate(wrd_inc = sum(across(10:13), na.rm = TRUE),
         ward_id = as.character(ward_id)) |>
  select(ward_id, wrd_inc)

# join
wrd <- wrd |>
  left_join(edu, by = c('ward_id' = 'ward_id')) |>
  left_join(inc, by = c('ward_id' = 'ward_id'))

# sp
wrd_sp <- as_Spatial(wrd)

# geographically weighted statistics: edu
edu_bw <- bw.gwr(wrd_edu ~ 1, data = wrd_sp, adaptive = TRUE, 
                 kernel = 'bisquare', longlat = T)

edu_gwss <- gwss(wrd_sp, vars = 'wrd_edu', bw = edu_bw, adaptive = TRUE,
                 kernel = 'bisquare', longlat = T)

# geographically weighted statistics: inc
inc_bw <- bw.gwr(wrd_inc ~ 1, data = wrd_sp, adaptive = TRUE, 
                 kernel = 'bisquare', longlat = T)

inc_gwss <- gwss(wrd_sp, vars = 'wrd_inc', bw = edu_bw, adaptive = TRUE,
                 kernel = 'bisquare', longlat = T)

# plot, correlation
plot(wrd$wrd_edu, wrd$wrd_inc, xlab = 'Education', ylab = 'Income')
cor(wrd$wrd_edu, wrd$wrd_inc)

# geographically weighted correlation: edu, inc
cor_bw <- bw.gwr(wrd_edu ~ wrd_inc, data = wrd_sp, adaptive = TRUE, 
                 kernel = 'bisquare', longlat = T)

cor_gwc <- gwss(wrd_sp, vars = c('wrd_edu','wrd_inc'), bw = cor_bw, 
                adaptive = TRUE, kernel = 'bisquare', longlat = T)

wrd$cor_gwc <- cor_gwc$SDF$Corr_wrd_edu.wrd_inc

# shape, polygons
tm_shape(wrd) +
  # specify column, classes
  tm_polygons(
    col = 'cor_gwc',
    n = 5,
    style = 'jenks',
    title = 'Local Correlation'
  )