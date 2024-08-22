# JTvD 2024

# load
library(tidyverse)
library(haven)

# read
cpt <- read_csv('data/attributes/sa-language.csv')
jhb <- read_dta('data/attributes/sa-internet-access.dta')
  
# prepare
cpt <- cpt |>
  mutate(sp_xhosa_prop = sp_xhosa/sp_pop)

jhb <- jhb |>
  filter(dc_name == "City of Johannesburg") 

jhb <- jhb |>
  rowwise() |>
  mutate(sal_pop = sum(across(starts_with("inter")), na.rm = TRUE))

jhb <- jhb |>
  group_by(sp_code) |>
  mutate(sp_pop = sum(sal_pop)) |>
  mutate(sp_inet = sum(inter_1)) |>
  ungroup() |>
  distinct(sp_code, sp_pop, sp_inet) |>
  mutate(sp_inet_prop = sp_inet/sp_pop)

# histogram - cpt
hist(cpt$sp_xhosa, breaks = 50, xlab = "Number of sub places", main = "Number of isiXhosa speakers")

# boxplot - cpt
boxplot(cpt$sp_xhosa_prop, horizontal = TRUE)

# histogram - jhb
hist(jhb$sp_inet, breaks = 50, xlab = "Number of sub places", main = "Number of people with internet access at home")

# boxplot - cpt
boxplot(jhb$sp_inet_prop, horizontal = TRUE)
