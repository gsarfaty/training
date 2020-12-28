# Munging MSD & Getting Mappy
# VL + Facility Mapping
# G.Sarfaty


# load packages
library(tidyverse)
library(sf)
library(glitr)
library(glamr)
library(gisr)
library(here)
library(scales)
library(patchwork)
library(ICPIutilities)
library(extrafont)

# set credentials (only have to do this once, like set_paths) ------------------------------
set_email() #enter usaid email
set_datim() #enter datim username; enter datim password when prompted


# load credentials
load_secrets()

# MER Site level import --------------------------------------------------------------------

zam_sites <- list.files(path = si_path(type="path_msd"),
                        pattern = "Structured_.*_Site_IM.*_\\d{8}_.*_Zambia.txt",
                        full.names = TRUE) %>%
  read_msd()


# geo data --------------------------------------------------------------------------------

# pull facility lat/long and add to site x IM MSD
df_geo<-extract_locations("Zambia", datim_user(),datim_pwd()) %>%
  extract_facilities(mer_sites = zam_sites)


# get adm1 from GADM
zam1 <- get_adm_boundaries("ZMB", adm_level = 1, geo_path = si_path(type="path_vector")) %>%
  st_as_sf() %>%
  select(country = name_0, province = name_1)


# MER Munge ---------------------------------------------------------------------------------------------
VL_Labs<-df_geo %>%
  filter(indicator =="LAB_PTCQI",
         standardizeddisaggregate %in% c("Lab/CQI"),
         otherdisaggregate_sub=="HIV Viral Load",
         fiscal_year=="2020") %>%
  group_by(id,indicator,standardizeddisaggregate,otherdisaggregate_sub,latitude,longitude) %>%
  summarise_at(vars(targets:cumulative),sum,na.rm=TRUE) %>%
  ungroup() %>%
  select(-c(targets:qtr4))


# Map ---------------------------------------------------------------------------------------------------

zam_map<-terrain_map(countries = "Zambia", terr_path = si_path(type = "path_raster"), mask = TRUE) +
  geom_sf(data = zam1, fill = NA, lwd = .2, color = grey30k) +
  geom_point(data=VL_Labs %>% filter (!is.na(longitude)),
             aes(x=longitude,
                 y=latitude),
             shape=21,alpha=.8, size=3, fill=usaid_red, color="white")+
  theme(
    legend.position =  "bottom",
    legend.key.width = ggplot2::unit(1, "cm"),
    legend.key.height = ggplot2::unit(.5, "cm"))+
  labs(subtitle = "Zambia | Viral Load Lab Locations",
       caption="Source: Site level MSD")+
  si_style_map()

print(zam_map)


ggsave(here("Graphics", "FY20Q4_Zambia_Labs.png"),
       scale = 1.2, dpi = 310, width = 10, height = 7, units = "in")
