#assignment #4
#Thailegeberel


# load packages
library(tidyverse)
library(ICPIutilities)
library(extrafont)
library(glamr)
library(glitr)
library(here)
library(scales)
library(sf)
library(gisr)
library(patchwork)
library(tmap)    # for static and interactive maps
library(leaflet)

# MER Data ----------------------------------------------------------------------------

# generate a list of files in your MSD folder using a text pattern of your choosing
file_psnu_im <- (list.files(path =si_path(type="path_msd"),
                            pattern = "Genie_PSNU_IM_South_Africa_.*.txt",
                            full.names = TRUE))
  #read_msd(file_psnu_im)

# read in MSD using ICPIUtilities
df<-read_msd(file_psnu_im) #uses file name we found and stored in lines 25-28

df%>%glimpse()

# GEO Data ------------------------------------------------------------------------------

# generate a list of files in your vector folder using a text pattern to
# catch shps for DATIM level 5 & set file as sf (spatial feature)
gis_5_sfc <- list.files(si_path(type="path_vector"),
                        pattern = "SouthAfricaDistrictLsib2016July.shp$", recursive = T, full.names = T) %>%
  set_names(basename(.) %>% str_remove("_.*.shp$")) %>%
  map(read_sf)

# generate a list of files in your vector folder using a text pattern to
# catch shps for DATIM level 4 & set file as sf (spatial feature)
gis_4_sfc <- list.files(si_path(type="path_vector"), pattern = "SouthAfrica_.*_4_.*.shp$", recursive = T, full.names = T) %>%
  set_names(basename(.) %>% str_remove("_.*.shp$")) %>%
  map(read_sf)


#check disaggs of TB_PREV

TB_disaggs<-df %>%
  filter(indicator=="TB_PREV") %>%
  distinct(indicator,standardizeddisaggregate)

prinf(TB_disaggs)

# create the TPT % completion metric for FY20Q4

TPT_comp <- df %>%
filter(numeratordenom == "N",
       standardizeddisaggregate =="Total Numerator",
       fundingagency != "Dedup") %>%
  group_by(psnu,psnuuid) %>%
  summarise(perc=sum(cumulative, na.rm = TRUE) / sum(targets, na.rm = TRUE) * 100) %>%
  ungroup() %>%
  mutate(benchmark=ifelse(perc <85, "no", "yes"))

# GEO Data Joins --------------------------------------------------------------------------
nga_geo<-st_as_sf(gis_5_sfc$SouthAfrica) %>% #create new spatial feature that is just SouthA frica's level 4
  left_join(TPT_comp, by = c("uid" = "psnuuid"))%>%
  filter(!is.na(perc))
print(nga_geo)

nga_geo_yes <- nga_geo%>%
  filter(benchmark=="yes")
nga_geo_no <- nga_geo%>%
  filter(benchmark=="no")

# map by agency for TLD
map_perc_TDT<-terrain_map(countries = "South Africa", terr_path = si_path(type = "path_raster"), mask = TRUE) +
  geom_sf(data = gis_5_sfc$SouthAfrica, fill = NA, color = grey40k) +
  geom_sf(data = nga_geo, aes(fill = perc), color = grey10k, lwd = .3) +
  geom_sf(data = nga_geo_yes, fill = NA, linetype = "dashed", lwd = .2) +
  geom_sf_text(data = nga_geo_yes, aes(label = paste0(round(perc), "%")), color = usaid_lightgrey, size = 4) +
  geom_sf_text(data = nga_geo_no, aes(label = paste0(round(perc), "%")), color = usaid_red, size = 4) +
  scale_fill_viridis_c(direction = -1) +
  si_style_map() +
  theme(
    legend.position =  "bottom",
    legend.key.width = ggplot2::unit(1, "cm"),
    legend.key.height = ggplot2::unit(.5, "cm"))+
  ggtitle("% TPT Completed \n by PSNU")

#print(map_perc_TDT)

plot_tpt<-TPT_comp %>% clean_psnu() %>%
  ggplot(aes(x = reorder(psnu, perc),
             y = perc, fill=perc)) +
  geom_col(show.legend = F)+
  geom_hline(yintercept = 85) +
  annotate("text", y = 100, x = 5, label = "Completion rate \n 85%") +
  scale_fill_viridis_c(direction = -1 ) +
  scale_y_continuous(breaks = seq(0,100,25))+
  coord_flip()+
  si_style_xgrid()+
  labs(y="",
       x="",
       title='TPT % completion | FY20 | Q4',
       caption="PLHIV on treatment have already completed TB preventive therapy")+
  theme(axis.text.y = element_text(size=8))


#print(plot_tpt)

print(map_perc_TDT + plot_tpt) #side by side mapping
#map_perc_TDT / plot_tpt top and bottom mapping or placement

ggsave(here("Graphics", "TPT_Complete_SouthAfrica_Hailegeberel.png"),
       scale = 1.2, dpi = 310, width = 10, height = 7, units = "in") #save our combined graphic

data(nga_geo)

tmap_mode("view")
## tmap mode set to plotting
tm_shape(nga_geo) +
  tm_borders("black", lwd = .5) +
  tm_fill("perc")
  tm_text(paste0(round(perc), "%"),size = .9) +
  tm_legend(show = FALSE)

