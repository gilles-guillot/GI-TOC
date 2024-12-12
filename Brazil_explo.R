# quick load/explo of two datasets

library(readxl)
library(tidyverse)
library(pacman)

# forest_dat municipalities
forest_dat = readxl::read_excel("./Brazilian_Municipalities_test_data.xlsx")
forest_dat %>% summary 
forest_dat$state_name %>% unique
forest_dat$state_name %>% unique 
forest_dat$municipality_code %>% unique

iwt = readxl::read_excel("./IWT_test_data.xlsx")
iwt %>% summary 




# attempt to retrieve extra data on forest_dat municipalities
install.packages("geobr")
library(geobr)
library(sf)
library(dplyr)
library(ggplot2)
p_load(plotly)
? read_municipality



muni <- read_municipality(  code_muni = "all",
                            year=2020, 
                            showProgress = FALSE)
muni$abbrev_state %>% unique

my_list_muni = unique(forest_dat$municipality_code)
my_muni = muni %>% filter(code_muni %in% my_list_muni)


# map Brazil  
ggplot() + 
  # geom_sf(data = muni, fill = "forestgreen") + 
  geom_sf(data = muni , aes(fill=abbrev_state))


# map Brazil + GI_TOC test data
my_muni = my_muni %>% rename(municipality_code = code_muni)

ggplot() + 
  geom_sf(data = muni, color=NA, fill = 'forestgreen') + 
  geom_sf(data=my_muni,fill ="dodgerblue3")

# merging geobr data and GI_TOC test data
dat = full_join(x = forest_dat , y = my_muni, by = "municipality_code") %>% 
  st_as_sf()

# Computing municipality areas ( in km2 ), 
# deforestation rate 
# population density, 
# rural conflict rate (1000 inhabitant)
dat = dat %>% mutate(municipality_area = st_area(geom)) %>% 
  mutate(municipality_area_km2 = as.vector(municipality_area/1e+6)) %>% 
  mutate(deforestation_rate = 100*deforestation_km2 / municipality_area_km2) %>% 
  mutate(pop_density = count_population / municipality_area_km2) %>% 
  mutate(rate_rural_conflicts = 1000 * count_rural_conflicts / count_population)

# map raw deforestation values 
ggplot() + 
   geom_sf(data=dat,aes(fill = deforestation_km2)) +
  scale_fill_viridis_c(option = "magma") 

# munic. area
ggplot() + 
  geom_sf(data=dat,aes(fill = municipality_area_km2)) +
  scale_fill_viridis_c(option = "viridis") 

# defor vs pop count
dat %>% ggplot(aes(x=count_population,y=deforestation_km2)) +
  geom_point(size=3,alpha=.3) + 
  scale_x_log10() + scale_y_log10()  

# defor rate vs pop density
dat %>% ggplot(aes(x=pop_density,y=deforestation_rate)) +
  geom_point(size=3,alpha=.3) +
  scale_x_log10()  

# deforestation rate
ggplot() + 
  geom_sf(data=dat,aes(fill = deforestation_rate)) +
  scale_fill_viridis_c(option = "viridis") 

# mapping population count
dat$pop_density %>% summary
ggplot() + 
  geom_sf(data=dat,aes(fill = count_population)) +
  scale_fill_viridis_c(option = "viridis") 

# mapping population count(NA-ing Manaus)
dat %>%  mutate(count_population = case_when(name_muni == "Manaus" ~ NA, 
                                            .default=count_population)) %>% 
  ggplot(aes(x=count_population)) + 
  geom_histogram()
  
dat %>%  mutate(count_population = case_when(name_muni == "Manaus" ~ NA, 
                                             .default=count_population)) %>% 
  ggplot() + 
  geom_sf(aes(fill = count_population)) +
  scale_fill_viridis_c(option = "viridis") 

# mapping population density (NA-ing Manaus)
dat$pop_density %>% summary
dat %>%  mutate(pop_desnity = case_when(name_muni == "Manaus" ~ NA, 
                                             .default=pop_density)) %>% 
  ggplot() + 
  geom_sf(aes(fill = pop_density)) +
  scale_fill_viridis_c(option = "viridis") 

# plotting defor  against rural conflicts
dat %>% ggplot(aes(x=count_rural_conflicts,y=deforestation_km2)) + 
  geom_point(size=4,alpha=.3)


# plotting defor rate against rate rural confliact
dat %>% ggplot(aes(x=rate_rural_conflicts,y=deforestation_rate)) + 
  geom_point(size=4,alpha=.3)


# plotting homicide against rural conflicts
dat %>% ggplot(aes(x=count_rural_conflicts,y=count_homicides)) + 
  geom_point(size=4,alpha=.3)


# mapping rural conflict
ggplot() + 
  geom_sf(data=dat,aes(fill = count_rural_conflicts)) +
  scale_fill_viridis_c(option = "viridis") 



###
# rivers
# p_load(mapdata)
# map('rivers',ylim=c(-20,7),xlim=c(-75,-45),col='red')

# https://milospopovic.net/map-rivers-with-sf-and-ggplot2-in-r/
# p_load(httr)
# get_data <- function() {
#   
#   url <- "https://data.hydrosheds.org/file/HydroRIVERS/HydroRIVERS_v10_eu_shp.zip"
#   
#   res <- GET(url,
#              write_disk("eu_rivers.zip"),
#              progress())
#   unzip("eu_rivers.zip")
#   filenames <- list.files("HydroRIVERS_v10_eu_shp", pattern="*.shp", full.names=T)
#   
#   riv_list <- lapply(filenames, st_read)
#   
#   return(riv_list)
# }
# get_data()
# 
# load_rivers <- function() {
#   list_riv <- lapply(filenames, sf::st_read)
#   eu_riv <- list_riv[[1]] |>
#     sf::st_cast("MULTILINESTRING")
#   
#   return(eu_riv)
# }
# 
# eu_riv <- load_rivers()
# 
