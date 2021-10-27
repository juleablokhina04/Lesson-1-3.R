library(tidyverse)
library(dplyr)
library(readr)
greendb <- read_csv("D:/Математическое моделирование/greendb.csv")
View(greendb)
data = greendb
rad = data$d_trunk_m / 2
basal = rad*rad*pi
basal
data$basal = (data$d_trunk_m/2)*(data$d_trunk_m/2)*pi


#Задание 1.Посчитать объем ствола, в таблицe data создать
#колонку Vtrunk

vtrunk = data$basal*data$height_m
vtrunk
data$vtrunk = data$basal*data$height_m
#

unique(data$species_ru)
data$species_ru %>% unique
data$species_ru |> unique()

data$species_ru = factor(data$species_ru)
summary(data$species_ru)


sum_table = greendb %>% group_by(species_ru) %>%
  summarise(
    diam_m = mean(d_trunk_m, na.rm=T),
    num = n(),
    height_m = mean(height_m, na.rm=T)
          )

sum_table = summarise (group_by(greendb, species_ru),
    diam_m = mean(d_trunk_m, na.rm=T),
    num = n(),
    height_m = mean(height_m, na.rm=T)
                      )


divers = greendb %>% group_by(adm_region, species_ru,) %>%                      
            summarise(
              nspecies = n()
                    )%>% select(-nspecies)%>% 
                         ungroup()%>% group_by(adm_region)%>% 
            summarise(
              nspecies = n()
                      )

#Задание 2.Получить сводную таблицу, где для каждого района будет 3 
#доминирующих вида и количество деревьев в этом районе
#для каждого из видов

divers1 = greendb %>% group_by(adm_region, species_ru) %>% 
             summarise(
                nspecies = n()
                      ) %>% group_by(adm_region) %>% 
                arrange(adm_region, desc(nspecies)) %>%
                mutate(ratio = order(nspecies, decreasing = T)) %>% 
                filter(order(nspecies, decreasing = T) <=3) %>% 
                select(-ratio)


library(tidyr)

transp = greendb %>% group_by(adm_region, species_ru) %>% 
            summarise(
               nspecies = n()
                      ) %>% pivot_wider(names_from = species_ru, values_from = nspecies) %>% 
select(starts_with("Липа"))  


### MAPS
library(sf)
library(ggplot2)
library(ggthemes)

map = st_read("D:/Математическое моделирование/boundary-polygon-lvl8.geojson",
              options = "ENCODING=UTF-8")

plot(map)

domin = greendb %>% group_by(adm_region, species_ru) %>%
  summarise(
    nspecies = n()
           ) %>% group_by(adm_region) %>% 
  arrange(adm_region, desc(nspecies)) %>%
  mutate(order = order(nspecies, decreasing = T)) %>%
  filter(order == 1) %>% select(-order, -nspecies) %>%
  rename(NAME = adm_region)


map1 = left_join(map, domin, by="NAME")

ggplot() + geom_sf(data = map, aes(fill=species_ru))+
  theme_foundation() + theme(legend.title = element_blank())

#Задание 3.Построить карту, на которой будет по районам отображена 
#средняя высота доминирующего вида растений

heights = greendb %>% group_by(adm_region) %>% 
  summarise(
    num = mean(height_m, na.rm=T),
    height_m = mean(height_m, na.rm=T)
            ) %>% group_by(adm_region) %>%
  arrange(adm_region, desc(height_m)) %>%
  mutate(order = order(height_m, decreasing = T)) %>%
  filter(order == 1) %>% select(-order, -height_m) %>%
  rename(NAME = adm_region)

map2 = left_join(map,heights, by="NAME")

ggplot() + geom_sf(data = map2, aes(fill=num))+
  theme_foundation() + theme(legend.title = element_blank())


    