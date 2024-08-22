# JTvD 2024

# load
library(tidyverse)
library(haven)

# read
cpt_att <- read_csv('data/attributes/sa-language.csv')
jhb_att <- read_dta('data/attributes/sa-internet-access.dta')
  
# prepare
cpt_att <- cpt_att |>
  mutate(sp_xhosa_prop = sp_xhosa/sp_pop)

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
  mutate(sp_inet_prop = sp_inet/sp_pop)

# histogram - cpt
hist(cpt_att$sp_xhosa, breaks = 50, xlab = 'Number of sub places', main = 'Number of isiXhosa speakers')

# boxplot - cpt
boxplot(cpt_att$sp_xhosa_prop, horizontal = TRUE)

# histogram - jhb
hist(jhb_att$sp_inet, breaks = 50, xlab = 'Number of sub places', main = 'Number of people with internet access at home')

# boxplot - cpt
boxplot(jhb_att$sp_inet_prop, horizontal = TRUE)
