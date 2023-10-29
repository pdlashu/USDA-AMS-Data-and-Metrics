
################################ RADAR CHART ####################################
setwd("D:/agecon_masters_uga/summer_sem/data_visualization_challenge/new_faff_maps")

library(ggradar)
library(readxl)
library(tidyverse)
library(scales)
library(purrr)
si_all <- read_excel("D:/agecon_masters_uga/summer_sem/data_visualization_challenge/new_faff_maps/si_all_faff.xlsx")

# filter only the state average observation and select only the rescaled variables for spyder chart (0-1)
si_rescaled_state <- si_all %>% 
  filter(is.na(county_name)) %>% 
  select(state_name, si_env_spyder, si_health_spyder, si_facc_spyder, si_faff_spyder)

#make a list of labels to be displayed on the spyder chart axes
dimensions <- c("Environment", "Food Access" ,"Food Affordability", "Health")

#make a list to provide custom colors to each of the state
lcols <- c("#F0474A", "#35F9F6")

#make a four dimensional radar chart for california and texas state level standardized index
radar_chart <- si_rescaled_state %>% 
  ggradar(background.circle.colour = "lightgrey",
          gridline.min.linetype = 1,
          gridline.mid.linetype = 1,
          gridline.max.linetype = 1,
          gridline.min.colour = "black",
          gridline.mid.colour = "black",
          gridline.max.colour = "black",
          group.colours = lcols,
          fill.alpha = 0.8,
          axis.labels = c("Environment", "Health", "Fd Access", "Fd Afford"), 
          axis.label.size = 5) +
  theme(legend.position = "bottom", 
        legend.justification = c(1,0),
        legend.text = element_text(size = 11), 
        legend.background = element_blank())

ggsave("radar_chart.png", width = 14, height = 7, dpi = 1200)


################################ RIDGE DENSITY CHART ####################################
library(ggridges)

si_ggridge <- si_all %>% 
  drop_na(county_name)

index_label  = c('si_env' = 'Environment', 'si_facc' = "Food Access", 'si_faff' =  "Food Affordability", 'si_health' = "Health")

ridge_plot <- si_ggridge %>% 
  pivot_longer(cols = 3:6) %>% 
  dplyr::select(state_name, name,value) %>% 
  ggplot()+
  facet_wrap(~name, scales = "free_x", ncol = 4, 
             labeller = labeller(name = index_label))+
  geom_density_ridges(aes(x = value, y = state_name, fill = state_name), alpha = 0.9)+
  labs(x = "Standardized value",
       y = "State name",
       title = "Distribution of standardized value of each dimensions across California and Texas",
       fill = "State name")+
  theme(legend.position = "none",
        axis.title = element_text(size = 14, face = "bold"),
        plot.title = element_text(hjust = 0.5, size = 16),
        axis.title.y  = element_blank(),
        axis.text.y = element_text(size = 12, face = "bold"))

#save the ridge_plot 
ggsave("ridge_plot.png", width = 14, height = 7, dpi = 1200)

################################ HEATMAP ####################################
###############################################################################
si_all_hmap <- si_all %>% 
  select(state_name, county_name, si_env, si_health, si_facc, si_faff) %>% 
  mutate(si_avg = (si_env + si_health + si_facc + si_faff)/4)

library(sf)
library(readxl)
library(patchwork)
library(stringr)
library(mapview)
library(transformr)

###################################HEATMAP OF CALIFORNIA################################

#select observations only for california from "si_all_hmap" dataframe 
ca_si_hmap <- si_all_hmap %>% 
  filter(state_name == 'CALIFORNIA') %>% 
  mutate(county_name = str_to_lower(county_name))

## only importing 'county_name" and "geometry" column from california geojson file. 
cacounty <- read_sf("D:/agecon_masters_uga/summer_sem/data_visualization_challenge/heatmap/California_County_Boundaries.geojson") %>% 
  st_set_crs(value = 4326) %>% 
  dplyr::select(county_name = CountyName,
                geometry) %>% 
  mutate(county_name = str_to_lower(county_name))

## #turning ca_si into a sf df by joining "geometry" column from "cacounty" sf df. 
ca_si_sf <- ca_si_hmap %>% 
  left_join(cacounty, by = "county_name" ) %>% 
  st_as_sf()

# making si_avg discrete 
ca_si_sf <- ca_si_sf %>% 
  mutate(si_avg_cat = case_when(
    si_avg >=0 & si_avg< 20 ~ 'very_low',
    si_avg>=20 & si_avg< 40 ~ 'low',
    si_avg>=40 & si_avg < 60 ~ 'medium',
    si_avg>=60 & si_avg< 80 ~ 'high',
    si_avg>= 80 & si_avg <100 ~ 'very_high'
  )) %>% 
  mutate(si_avg_cat = factor(si_avg_cat, levels = c("very_high", "high", "medium", "low", "very_low")))


#filter the ca_si_sf data so that only counties with very high, high, low and very low index are displayed in the heatmap
filtered_ca_si_sf <- ca_si_sf %>% 
  filter(si_avg_cat %in% c('very_high', 'high', 'low', 'very_low'))

# making a heatmap for california 
ca_cat_plot <- ggplot()+
  geom_sf(data = ca_si_sf, aes(fill = si_avg_cat))+
  scale_fill_manual(values = c("very_high" = "#53F7FF",  "high" ='#EF8F23', "medium" = "#28EE55",  "low" = "#F35EEC", "very_low" ="#FD4C5C"),
                    labels = c("very_high" = "very high", "high" = 'high',  "medium" = "medium", "low" = "low", "very_low" = "very low"), drop = FALSE)+
  theme_bw()+ 
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(), 
        plot.title = element_text(hjust = 0.5, size = 16),
        legend.title = element_blank(), 
        legend.text = element_blank(),
        legend.position = "none") + 
  labs(title = "CALIFORNIA", fill = "HEAL Index") #+ 
  #geom_sf_text(data = filtered_ca_si_sf, 
               #aes(label = county_name), 
               #size = 3, 
               #color = 'black')

#save california heatmap
ggsave("ca_hmap.png", width = 14, height = 7, dpi = 1200)

################################### HEATMAP OF TEXAS ################################
# selecting observations only for texas counties from "si" df

tx_si_hmap <- si_all_hmap %>% 
  filter(state_name == "TEXAS") %>% 
  mutate(county_name = str_to_lower(county_name))

#importing a geojson file for texas county and only selecting 'county_name" and "geometry" column  

txcounty <- read_sf("D:/agecon_masters_uga/summer_sem/data_visualization_challenge/heatmap/texas_county.json") %>% 
  st_set_crs(value = 4326) %>% 
  dplyr::select(county_name = NAME, geometry) %>% 
  mutate(county_name = str_to_lower(county_name)) %>% 
  arrange(county_name)


#turning tx_si_hmap into a sf df by joining "geometry" column from "txcounty" sf df. 
tx_si_sf <- tx_si_hmap %>% 
  mutate(county_name = str_replace(county_name, "de witt", "dewitt")) %>% 
  left_join(txcounty, by = "county_name") %>% 
  st_as_sf()


# making si_avg discrete 
tx_si_sf <- tx_si_sf %>% 
  mutate(si_avg_cat = case_when(
    si_avg >=0 & si_avg< 20 ~ 'very_low',
    si_avg>=20 & si_avg< 40 ~ 'low',
    si_avg>=40 & si_avg < 60 ~ 'medium',
    si_avg>=60 & si_avg< 80 ~ 'high',
    si_avg>= 80 & si_avg <100 ~ 'very_high'
  )) %>% 
  mutate(si_avg_cat = factor(si_avg_cat, levels = c("very_high", "high", "medium", "low", "very_low")))


#filter the tx_si_sf data so that only counties with very high, high, low and very low index are displayed in the heatmap
filtered_tx_si_sf <- tx_si_sf %>% 
  filter(si_avg_cat %in% c('very_high', 'high', 'low', 'very_low'))

# making a heatmap for Texas 
tx_cat_plot <- ggplot()+
  geom_sf(data = tx_si_sf, aes(fill = si_avg_cat))+
  scale_fill_manual(values = c("very_high" = "#53F7FF",  "high" ='#EF8F23', "medium" = "#28EE55",  "low" = "#F35EEC", "very_low" ="#FD4C5C"),
                    labels = c("very_high" = "very high", "high" = 'high',  "medium" = "medium", "low" = "low", "very_low" = "very low"), drop = FALSE)+
  theme_bw()+
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(), 
        plot.title = element_text(hjust = 0.5, size = 16),
        legend.title = element_text(size = 14), 
        legend.text = element_text(size = 12),
        legend.position = 'right') + 
  labs(title = "TEXAS", fill = "HEAL Index") #+ 
  #geom_sf_text(data = filtered_tx_si_sf, 
               #aes(label = county_name), 
               #size = 3, 
               #color = 'black') 

ggsave("tx_hmap.png", width = 14, height = 7, dpi = 1200)


#######
ca_si_sf %>% 
  group_by(si_avg_cat) %>% 
  summarize(count = n())

tx_si_hmap %>% 
  select(county_name, si_avg_cat) %>% 
  group_by(si_avg_cat) %>% 
  summarize(count = n())

tx_si_hmap <- tx_si_hmap %>% 
  mutate(si_avg_cat = case_when(
    si_avg >=0 & si_avg< 20 ~ 'very_low',
    si_avg>=20 & si_avg< 40 ~ 'low',
    si_avg>=40 & si_avg < 60 ~ 'medium',
    si_avg>=60 & si_avg< 80 ~ 'high',
    si_avg>= 80 & si_avg <100 ~ 'very_high'
  ))

pop <- read_dta("D:/agecon_masters_uga/summer_sem/data_visualization_challenge/new_faff_maps/pop2017.dta")
