#Fun Fun Fun 04_Mapping Assignment

#Instructions
#One of our treatment advisors has come to you for help. 
#They need to see % TPT completion by psnu in South Africa for FY20Q4. 
#Bonus if you can help them flag which psnus have a completion rate <85%

#download the correct datasets (MER & geospatial)
#manipulate the MSD to create the TPT % completion metric for FY20Q4
#verify your values against another source (eg. Pano) to ensure they are correct
#map the data & combine with another visualization type of your choosing (eg. bar chart) 
#to create one cohesive visual in R; hint: hello, patchwork


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


load_secrets()

#load in SA data

sa <- list.files(path = si_path(type="path_msd"),
                        pattern = "Structured_.*_PSNU_IM.*_\\d{8}_.*_South Africa.txt",
                        full.names = TRUE) %>%
  read_msd()

glimpse(sa)


gis_5_sfc <- list.files(si_path(type="path_vector"), pattern = "SouthAfrica.*.shp$", recursive = T, full.names = T) %>%
  set_names(basename(.) %>% str_remove("_.*.shp$")) %>%
  map(read_sf)
#cpg corrupt??

here(path_vector)
path_vector()
library(janitor)

sa1 <- get_adm_boundaries("ZAF", adm_level = 1, geo_path = si_path(type="path_vector")) %>%
  st_as_sf() %>%
  select(country = name_0, district = name_1)

distinct(sa_sites, indicator)


df_TPT<-sa %>%
  filter(fiscal_year =="2020", #== same as %in% c
         indicator %in% c("TB_PREV"),
         #standardizeddisaggregate %in% c("Numerator"),
         operatingunit %in% c("South Africa")) %>%
  mutate(indicator = ifelse(numeratordenom == "D", paste0(indicator, "_D"), indicator)) %>% #if indicator has denonm, paste D to end of indicator name
  group_by(fiscal_year,operatingunit,fundingagency,psnuuid,psnu,indicator) %>%
  summarise_at(vars(qtr1:cumulative), sum, na.rm = TRUE) %>%
  ungroup() %>%
  select(-c(qtr4:cumulative)) %>%
  mutate(indicator=case_when(
   indicator %in% c("TB_PREV_D", "TB_PREV") ~ indicator,
    TRUE ~ "other")) %>%
  reshape_msd(clean = TRUE) %>%
  spread(indicator, val) %>% #we are spreading indicator to value - indicator with values underneath it
  group_by(psnuuid,psnu) %>%
 summarise_at(vars(TB_PREV:TB_PREV_D),sum,na.rm=TRUE) %>%
 ungroup() %>%
  #select(-c(targets:qtr4,qtr1,qtr2))
 # group_by(fundingagency,psnu,snuuid) %>%
  mutate(TPT=(TB_PREV/TB_PREV_D), 
         TPT_cat=case_when(
           TPT <.85 ~ "Less than 85%",
           TPT >=.85 ~ "85% or Greater"))
 #WHY ARE THERE NEGATIVE #S
  view(sa)
  
  sa_geo<-st_as_sf(gis_5_sfc$SouthAfrica) %>%
    left_join(df_TPT, by = c("uid" = "psnuuid"))
  install.packages("sp")
  library(rgdal)
  library(sp)
  library(rnaturalearthhires)
  
  
  
sa_map<-terrain_map(countries = "South Africa", terr_path = si_path(type = "path_raster"), mask = TRUE) +
    geom_sf(data = sa_geo %>% filter(!is.na(TPT)), aes(fill = TPT), lwd = .2, color = grey40k) +
  scale_fill_viridis_c(option="viridis", alpha=0.9, direction = -1,
                       breaks = c(0.5,0.75, 1.00),
                       limits = c(0.50, 1.00),
                       labels=percent)+
    theme(
      legend.position =  "bottom",
      legend.key.width = ggplot2::unit(1, "cm"),
      legend.key.height = ggplot2::unit(.5, "cm"))+
    labs(subtitle = "South Africa FY20Q4 |Completion of TB Preventive Treatment (TPT)",
         caption="Source: PSNU Level/District Level MSD")+
    si_style_map()
  
  print(sa_map)
  
  ggsave(here("Graphics", "2_FY20Q4_South Africa_TPT.png"),
         scale = 1.2, dpi = 310, width = 10, height = 7, units = "in")
  
  ##adding a bar chart
  
  PSNU_viz<-df_TPT %>%
    ggplot(aes(x=psnu,
               y=TPT))+
    geom_col(show.legend = F)+
    coord_flip()+
    scale_y_continuous(labels = percent)+
    si_style_nolines()+
    labs(y="",
         x="",
         title='% of TPT Completion by District | FY20Q4',
         caption="MSD")+
    theme(axis.text.y = element_text(size=8))
  
  print(PSNU_viz)
  
  
  ggsave(here("Graphics","FY21_Under15_TX_CURR_T_Zambia.png"))
  
  
  # VIZ COMBINED & SAVED ---------------------------------------------------------------------------------
  library(patchwork)
  (sa_map + PSNU_viz) +
    plot_annotation(caption = "Source: FY20Q4i MSD
    TPT=(TB_PREV/TB_PREV_D)
    % of TPT Completion in FY20Q4")
  
  ggsave(here("Graphics", "SouthAfrica_TPT.png"),
         scale = 1.2, dpi = 310, width = 10, height = 7, units = "in")
  
 ########

  
