# JTvD 2024

# libraries
library(tidyverse)
library(sf)
library(janitor)

# load
wrd <- st_read('data/spatial/ward-western-cape-2011.gpkg')
edu <- read_csv('data/attributes/sa-ward-education-raw.csv') |> clean_names()
inc <- read_csv('data/attributes/sa-ward-income-raw.csv') |> clean_names()

# clean education table
edu <- edu |>
  pivot_wider(names_from = 'highest_educational_level', values_from = 'count') |>
  clean_names() |>
  select(-summation_options, -total) |>
  filter(geography != 'Western Cape') |>
  filter(geography != 'Total') 

# extract ward code, pretty
names(edu)[1] <- 'ward_id'
edu <- edu |>
  mutate_if(is.numeric, round, digits = 3) |>
  mutate(ward_id = substring(ward_id,1,8))

# extract ward code, pretty
names(inc)[1] <- 'ward_id'
inc <- inc |>
  mutate_if(is.numeric, round, digits = 3) |>
  mutate(ward_id = substring(ward_id,1,8)) |>
  semi_join(wrd, by = c('ward_id' = 'ward_id'))

# write
write_csv(edu, 'data/attributes/sa-ward-education.csv')
write_csv(inc, 'data/attributes/sa-ward-income.csv')

