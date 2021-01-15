library(tidyverse)
library(readxl)
library(sf)
library(GISTools) #for Georgia counties

#=======================================================
#Loeffler/Warnock Race
#data from https://www.politico.com/2020-election/results/georgia/senate-runoff-map-loeffler-warnock-20210105/
#Ossoff/Perdue Race
#data from https://www.politico.com/2020-election/results/georgia/senate-runoff-map-perdue-ossoff-20210105/
#=======================================================

data(georgia) #pull map of Georgia counties
georgia_sf  <-  st_as_sf(georgia)

georgia_sf <- 
  georgia_sf %>% 
  dplyr::select(Name, Latitude, Longitud, X, Y, ID, Name, geometry) %>% 
  mutate(area=as.numeric(st_area(geometry)))

wl_counties <- #warnock/loeffler race
  read_excel("wo ga 20210105.xlsx", sheet = "WL") %>% 
  mutate(Name=gsub(" County", "", COUNTY), #to help with the Join
         race="Warnock (D) / Loeffler (R)") %>% 
  rename(d_pct=warnock_pct) %>% 
  dplyr::select(Name, TOTAL, race, d_pct) 
  
op_counties <- #ossoff/perdue race
  read_excel("wo ga 20210105.xlsx", sheet = "OP") %>% 
  mutate(Name=gsub(" County", "", COUNTY), #to help with the Join
         race="Ossoff (D) / Perdue (R)") %>% 
  rename(d_pct=ossoff_pct) %>% 
  dplyr::select(Name, TOTAL, race, d_pct)

georgia_sf_wl <- left_join(georgia_sf, wl_counties) #join the data sets together
georgia_sf_op <- left_join(georgia_sf, op_counties) #join the data sets together

#join data for wl and op races together, calculate voter density
georgia_sf_wlop <- rbind(georgia_sf_wl, georgia_sf_op)
georgia_sf_wlop$density <- georgia_sf_wlop$TOTAL/georgia_sf_wlop$area
georgia_sf_wlop$scale <- georgia_sf_wlop$density/max(georgia_sf_wlop$density)

#rescale counties by voter density
georgia_sf_wlop_scaled <-
  georgia_sf_wlop %>% 
  mutate(centroid=st_centroid(geometry),
         geometry=(geometry-centroid)*scale+centroid,
         map="Counties Scaled by Voter\nDensity")

georgia_sf_wlop$map <- "Counties Scaled by Area"

#do this so d_pct bar goes from 0 to 100
georgia_sf_wlop_minmax <- bind_rows(georgia_sf_wl[1:2,], georgia_sf_op[1:2,])
georgia_sf_wlop_minmax <- 
  georgia_sf_wlop_minmax %>%
  mutate(d_pct = c(1, 0, 1, 0),
         Name = rep("minmax", 4),
         scale = rep(1e-12, 4),
         centroid=st_centroid(geometry),
         geometry=(geometry-centroid)*scale+centroid,
         map=c(rep("Counties Scaled by Voter\nDensity", 2), rep("Counties Scaled by Area", 2)))

georgia_sf_wlop2 <- bind_rows(georgia_sf_wlop, georgia_sf_wlop_scaled, georgia_sf_wlop_minmax)

#make labels, since pivot_longer doesn't work with sf data
map_race <- data.frame(map=c(rep("Counties Scaled by Voter\nDensity", 159), 
                           rep("Counties Scaled by Area", 159),
                          rep("Counties Scaled by Voter\nDensity", 159), 
                          rep("Counties Scaled by Area", 159)),
                     race=c(rep("Warnock (D) / Loeffler (R)", 159),
                            rep("Warnock (D) / Loeffler (R)", 159), 
                            rep("Ossoff (D) / Perdue (R)", 159),
                            rep("Ossoff (D) / Perdue (R)", 159)), 
                     stringsAsFactors = FALSE)

#put everything together
georgia_sf2 <- bind_rows(georgia_sf, georgia_sf, georgia_sf, georgia_sf)
georgia_sf2 <- bind_cols(georgia_sf2, map_race)

#plot
p.georgia <- 
  ggplot()+
  geom_sf(data=georgia_sf2, aes(geometry=geometry), fill="grey90", color=NA)+
  geom_sf(data=georgia_sf_wlop2, aes(geometry=geometry, fill=d_pct*100, color=d_pct*100))+
  geom_sf(data=georgia_sf2, aes(geometry=geometry), fill=NA, color="black", alpha=0.5)+
  theme_bw()+
  coord_sf()+
  facet_grid(race~map)+
  scale_fill_gradient2(low="red", mid="purple", high="blue", midpoint = 50,
                       breaks=seq(0, 100, 20))+
  scale_color_gradient2(low="red", mid="purple", high="blue", midpoint = 50,
                       breaks=seq(0, 100, 20))+
  labs(title="Georgia Voting by County, January 5, 2021\n",
       fill="Vote for the\nDemocratic Party\nCandidate (%)",
       color="Vote for the\nDemocratic Party\nCandidate (%)")+
  theme(plot.title = element_text(size = 18, hjust=0.5),
        strip.text = element_text(size = 11),
        legend.position="bottom",
        legend.key.width = unit(0.5, "in"),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        strip.background = element_rect(fill = "grey90"))
  
p.georgia  

ggsave("georgia2021.png", p.georgia, width=6, height=8, units="in")

georgia_sf_wlop3 <- 
  georgia_sf_wlop2 %>% 
  dplyr::select(Name, scale, race, map) %>% 
  filter(Name != "minmax", race=="Warnock (D) / Loeffler (R)", 
         map=="Counties Scaled by Area") %>% 
  arrange(-scale)
    
