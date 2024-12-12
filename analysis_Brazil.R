### 
# Main step analysis


library(readxl)
library(tidyverse)
library(pacman)
p_load("geobr")
library(geobr)
library(sf)
library(dplyr)
library(ggplot2)
p_load(plotly)



# forest_dat municipalities
forest_dat = readxl::read_excel("./Brazilian_Municipalities_test_data.xlsx")
forest_dat %>% summary 
forest_dat$state_name %>% unique
forest_dat$state_name %>% unique 
forest_dat$municipality_code %>% unique


muni <- geobr::read_municipality(  code_muni = "all",
                            year=2020, 
                            showProgress = FALSE)
muni$abbrev_state %>% unique

my_list_muni = unique(forest_dat$municipality_code)
my_muni = muni %>% filter(code_muni %in% my_list_muni)


# map Brazil + GI_TOC test data
my_muni = my_muni %>% rename(municipality_code = code_muni)

ggplot() + 
  geom_sf(data = muni, color=NA, fill = 'forestgreen') + 
  geom_sf(data=my_muni,fill ="dodgerblue3")

# merging geobr data and GI_TOC test data
dat = full_join(x = forest_dat , y = my_muni, by = "municipality_code") %>% 
  st_as_sf()

# Computing 
# municipality areas ( in km2 ), 
# deforestation rate  (%)
# population density, (inhab.  per km2)
# rural conflict rate (1000 inhabitant)
dat = dat %>% mutate(municipality_area = st_area(geom)) %>% 
  mutate(municipality_area_km2 = as.vector(municipality_area/1e+6)) %>% 
  mutate(deforestation_rate = 100*deforestation_km2 / municipality_area_km2) %>% 
  mutate(pop_density = count_population / municipality_area_km2) %>% 
  mutate(rural_conflicts_per_1000inhab = 1000 * count_rural_conflicts / count_population) %>% 
  mutate(rural_conflicts_per_km2 = count_rural_conflicts / municipality_area_km2)


# map defor rate
ggplot() + 
  geom_sf(data=dat,aes(fill = deforestation_rate)) +
  scale_fill_viridis_c(option = "magma") 

# map rate rural conflicts par inhab
dat %>% ggplot() + 
  geom_sf(aes(fill = rural_conflicts_per_1000inhab)) +
  scale_fill_viridis_c(option = "magma") 

# map rate rural conflicts par km2
dat %>% ggplot() + 
  geom_sf(aes(fill = rural_conflicts_per_km2)) +
  scale_fill_viridis_c(option = "magma") 


# map defor rate and rulral conflict rate
tmp = dat %>% select(deforestation_rate,rural_conflicts_per_1000inhab,geom) %>% 
  pivot_longer(cols=c("deforestation_rate","rural_conflicts_per_1000inhab"))

tmp %>% ggplot() + 
  geom_sf(aes(fill = value)) +
  facet_wrap(~name) + 
  scale_fill_viridis_c(option = "magma") 

# p_load(ggspatial)
# tmp %>% ggplot() + 
#   ggspatial::geom_spatial(aes(fill = value)) +
#   facet_wrap(~name,scales = "free") 

# p_load(tmap)
# tmp %>% tm_shape(aes(fill = value)) +
#   tm_borders() +
#   tm_facets(by = "name")
# 








