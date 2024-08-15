# JTvD 2024

# libraries
library(tidyverse)
library(tmap)
library(sf)
library(haven)

# data
cpt <- st_read('data/spatial/subplace-cape-town-2013.gpkg') |> st_make_valid()
att <- read_dta('data/attributes/language.dta')

# prepare data: filter out Cape Town
att <- att |>
  filter(dc_name == 'City of Cape Town')

# prepare data: data type
att <- att |>
  mutate(sp_code = as.character(sp_code))

# prepare data: sum 
att <- att |>
  rowwise() |>
  mutate(pop = sum(across(starts_with('lng')), na.rm = TRUE))

# prepare data: aggregate small areas to sub places
att <- att |>
  group_by(sp_code) |>
  mutate(sp_pop = sum(pop)) |>
  mutate(sp_xhosa = sum(lng_4)) |>
  ungroup() |>
  distinct(sp_code, sp_pop, sp_xhosa) 

# prepare data: join attribute data to spatial data
cpt <- cpt |> left_join(att, by = c('sp_code' = 'sp_code'))

# prepare data: replace NAs
cpt <- cpt |>
  mutate(sp_pop = if_else(is.na(sp_pop), 0, sp_pop),
         sp_xhosa = if_else(is.na(sp_xhosa), 0, sp_xhosa))

# prepare data: calculate percentages
cpt <- cpt |>
  mutate(sp_prop_xhosa = sp_xhosa / sp_pop)

# prepare data: map labels
cbd <- cpt |> filter(sp_code == '199041011') |> st_centroid()
  
# map
xhosa_map <- 
  
  # cpt polygons
  tm_shape(cpt) +
  
  # add grid
  tm_grid(labels.show = FALSE, col = '#f0f0f0') +
  
  # add polygons
  tm_fill(col= 'sp_prop_xhosa', n = 5, style = 'jenks',
          palette = c('#7a0177','#c51b8a','#f768a1','#fbb4b9','#feebe2'), 
          labels = c('Largest share', '2nd largest', '3rd largest', '4th largest', 'Smallest share'),
          title = 'Share of population',
          textNA = 'No population',) +
  
  # cbd centroid
  tm_shape(cbd) +
  
  # add points
  tm_dots(size = 0.4, col= '#000000') +
  
  # add labels
  tm_text(text='sp_name', xmod = 0, ymod = -0.6, col = '#000000', size = 0.8) +
  
  # set layout
  tm_layout(
    main.title = 'Share of population speaking isi-Xhosa',
    main.title.size = 0.9,
    main.title.position = c('right', 'top'),
    legend.outside = FALSE,
    legend.position = c('right', 'top'),
    legend.title.size = 0.7,
    legend.title.fontface = 'bold',
    legend.text.size = 0.5,
    frame = FALSE,
    inner.margins = c(0.05,0.05,0.05,0.05),
    fontfamily = 'Helvetica'
  ) +
  
  # add North arrow
  tm_compass(
    type = 'arrow',
    position = c('left', 'top'),
    size = 1,
    text.size = 0.7
  ) +
  
  # add scale bar
  tm_scale_bar(
    breaks = c(0, 5, 10, 15, 20),
    position = c('right', 'bottom'),
    text.size = 0.4
  ) +
  
  # add credits
  tm_credits('Data source: Census 2011, StatsSA',
             fontface = 'italic', 
             position = c('left', 'bottom'),
             size = 0.4) 

# save
tmap_save(tm = xhosa_map, 'support/cpt-xhosa.jpg', width=15, height=15, units=c('cm'))
