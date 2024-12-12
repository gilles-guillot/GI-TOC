

library(tidyverse)
library(pacman)




# forest_dat municipalities
iwt = readxl::read_excel("./IWT_test_data.xlsx")
# iwt$`website/name` %>% unique
# iwt$`sale-item/type` %>% unique                               
# iwt$`sale-item/species` %>% unique()
# iwt$`sale-item/taxa` %>% unique()
# iwt$`sale-item/CITES` %>% unique()
# iwt$`sold-in`%>% unique()
# iwt$month %>% unique()

iwt = iwt %>% rename(website=`website/name`,
                             type = `sale-item/type`,
                             species = `sale-item/species`,
                             taxa = `sale-item/taxa` , 
                             sold_in = `sold-in`,
                             CITES = `sale-item/CITES`)

# iwt = iwt %>% mutate(ID=1:nrow(iwt))

iwt$species %>% unique %>% sort

# iwt = iwt %>% mutate(spec = case_when(species %in% c('bear','Bear') ~ 'Bear' ))
iwt = iwt %>% mutate(species = fct_collapse(species, Bear = c('bear','Bear')))
iwt = iwt %>% mutate(species = fct_collapse(species, `Radiated Tortoise` = c('Radiated tortoise','Radiated Tortoise')))

iwt$species %>% unique %>% sort

# tmp  = iwt %>% group_by(species, sold_in) %>% 
#   summarise(n=n())
# tmp %>% ggplot(aes(y=n,x=species,fill=sold_in)) + 
#   geom_bar() 


iwt %>% ggplot(aes(y=species,fill=sold_in)) + 
  geom_bar()


iwt %>% 
  ggplot(aes(y=species,fill=website)) + 
  geom_bar() + 
  facet_wrap(~ sold_in)

iwt %>% 
  ggplot(aes(x=species,fill=website)) + 
  geom_bar() + 
  facet_wrap(~ sold_in)


iwt %>% filter(CITES=="I") %>% 
  group_by(sold_in) %>% 
  ggplot(aes(y=species,fill=website)) + 
  geom_bar() + 
  facet_wrap(~ sold_in)


tmp = iwt %>% group_by(#species, sold_in, 
  month) %>% 
   summarise(n=n()) %>% 
  mutate(month = factor(month,levels = c('August','September', 'October')))
tmp %>%   ggplot(aes(x=month , y=n)) +
  geom_point(size=3,alpha=.3)



tmp = iwt %>% group_by(#species, 
  sold_in, 
  month) %>% 
  summarise(n=n()) %>% 
  mutate(month = factor(month,levels = c('August','September', 'October')))
tmp %>%   ggplot(aes(x=month , y=n, col=sold_in)) +
  geom_point(size=5,alpha=.7)


iwt %>% group_by(sold_in, month) %>% 
  summarise(n=n()) %>% 
  mutate(month = factor(month,levels = c('August','September', 'October'))) %>% 
 ggplot(aes(x=month , y=n, col=sold_in)) +
  geom_point(size=5,alpha=.7)



















