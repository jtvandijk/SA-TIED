# JTvD 2024

# libraries
library(tidyverse)
library(sf)
library(tmap)
library(GWmodel)

# data
sa_municipality <- st_read('data/spatial/municipality-south-africa-2013.gpkg')
sa_no_schooling <- read_csv('data/attributes/sa-no-schooling.csv')
sa_average_age <- read_csv('data/attributes/sa-average-age.csv')

# calculate proportions
sa_no_schooling <- sa_no_schooling |> 
  mutate(mn_no_school_prop = round(mn_no_school/mn_pop, 3)) |>
  select(mn_code, mn_no_school_prop)

# join
sa_municipality <- sa_municipality |>
  left_join(sa_no_schooling, by = c("mn_code" = "mn_code")) |>
  left_join(sa_average_age, by = c("mn_code" = "mn_code"))

# to sp
sa_municipality_sp <- as_Spatial(sa_municipality)

# geographically weighted statistics: no schooling
sa_gwss_no_schooling <- gwss(sa_municipality_sp , vars = "mn_no_school_prop", 
                          bw = 10, kernel = "bisquare", adaptive = TRUE, longlat = T)

# geographically weighted statistics: average age
sa_gwss_avgerage_age <- gwss(sa_municipality_sp , vars = "mn_avg_age", 
                          bw = 10, kernel = "bisquare", adaptive = TRUE, longlat = T)

# extract local means
sa_municipality$mn_no_schooling_LM25 <- sa_gwss_no_schooling$SDF$mn_no_school_prop_LM
sa_municipality$mn_average_age_LM25 <- sa_gwss_avgerage_age$SDF$mn_average_age_LM

# shape, polygons
tm_shape(sa_municipality) +
  # specify column, classes
  tm_polygons(
    col = "mn_no_schooling_LM25", 
    n = 5, 
    style = "jenks",
    title = "Local Mean"
  ) 

# shape, polygons
tm_shape(sa_municipality) +
  # specify column, classes
  tm_polygons(
    col = "mn_average_age_LM25", 
    n = 5, 
    style = "jenks",
    title = "Local Mean"
  ) 

# bandwidth selection
bw_no_school <- bw.gwr(mn_no_school_prop ~ 1, data = sa_municipality_sp,
                      adaptive = TRUE, kernel = "bisquare", longlat = T)

bw_avg_age <- bw.gwr(mn_avg_age ~ 1, data = sa_municipality_sp,
                     adaptive = TRUE, kernel = "bisquare", longlat = T)

# correlation
cor(sa_municipality$mn_no_school_prop, sa_municipality$mn_avg_age)

# bivariate plot
plot(sa_municipality$mn_no_school_prop, sa_municipality$mn_avg_age, 
     xlab = 'No schooling', ylab = 'Average Age')

# geographically weighted statistics: correlation
sa_gwss_cor <- gwss(sa_municipality_sp , vars = c("mn_no_school_prop", "mn_avg_age"), 
                    bw = 10, kernel = "bisquare", adaptive = TRUE, longlat = T)

# extract correlation
sa_municipality$mn_noschool_age_cor <- sa_gwss_cor$SDF$Corr_mn_no_school_prop.mn_avg_age

# inspect
summary(sa_municipality$mn_noschool_age_cor)

# shape, polygons
tm_shape(sa_municipality) +
  # specify column, classes
  tm_polygons(
    col = "mn_noschool_age_cor", 
    n = 5, 
    style = "jenks",
    title = "Local Correlation"
  ) 

# significance
sa_gwss_cor_sig <- gwss.montecarlo(sa_municipality_sp , vars = c("mn_no_school_prop", "mn_avg_age"), 
                                  bw = 10, kernel = "bisquare", adaptive = TRUE, longlat = T) |> 
  as_tibble() |>
  select(Corr_mn_no_school_prop.mn_avg_age) 
names(sa_gwss_cor_sig) <- 'mn_no_school_age_cor_p'

# bind results
sa_municipality <- sa_municipality |>
  cbind(sa_gwss_cor_sig) |>
  mutate(sa_gwss_cor = if_else(mn_no_school_age_cor_p < 0.025, mn_noschool_age_cor,
                       if_else(mn_no_school_age_cor_p > 0.975, mn_noschool_age_cor, NA)))
  
# shape, polygons
tm_shape(sa_municipality) +
  # specify column, classes
  tm_polygons(
    col = "sa_gwss_cor", 
    n = 5, 
    style = "jenks",
    title = "Local Correlation"
  ) 