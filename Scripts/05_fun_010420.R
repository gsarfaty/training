#MADDIE MEDINA 01042020 Assignment #5

#You've been asked to pull data and create a visual to show FY20 proxy linkage by 
#psnu among the FSW key populations group for Kenya.
#manipulate the MSD to create the proxy linkage metric for FY20 (full year) for FSW
#TX_NEW/HTS_TST_POS??
#Definition: Estimate of the percentage of people who test HIV positive and are linked to treatment. 
#KP
#Standardized disaggregate: KeyPop/Result

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

fsw <- list.files(path = si_path(type="path_msd"),
                 pattern = "Structured_.*_PSNU_IM.*_\\d{8}_.*_Kenya.txt",
                 full.names = TRUE) %>%
  read_msd()

glimpse(fsw)

gis_vc_sfc <- list.files(si_path(type="path_vector"), pattern = "Vc.*.shp$", recursive = T, full.names = T) %>%
  set_names(basename(.) %>% str_remove("_.*.shp$")) %>%
  map(read_sf)

kenya1 <- get_adm_boundaries("KEN", adm_level = 1, geo_path = si_path(type="path_vector")) %>%
  st_as_sf() %>%
  select(country = name_0, province = name_1)

#checking disaggregates:
TX_disaggs<-fsw %>%
 filter(indicator %in% c("TX_NEW", "HTS_TST_POS", "TX_NET_NEW")) %>%
  distinct(indicator,standardizeddisaggregate,categoryoptioncomboname,otherdisaggregate,otherdisaggregate_sub)

#can we go over the results greater than 100%
df_proxy<-fsw %>%
  filter(fiscal_year =="2020", #== same as %in% c
         indicator %in% c("TX_NEW", "HTS_TST_POS"),
         standardizeddisaggregate %in% c("KeyPop/HIVStatus", "KeyPop/Result"),
         categoryoptioncomboname %in% c("FSW, Positive"),
         operatingunit %in% c("Kenya")) %>%
  group_by(fiscal_year,operatingunit,fundingagency,psnuuid,psnu,indicator,standardizeddisaggregate,categoryoptioncomboname,mech_name,mech_code) %>%
  summarise_at(vars(qtr1:cumulative), sum, na.rm = TRUE) %>%
  ungroup() %>%
  select(-c(qtr1:qtr4)) %>%
  mutate(indicator=case_when(
    indicator %in% c("TX_NEW", "HTS_TST_POS") ~ indicator,
    TRUE ~ categoryoptioncomboname)) %>%
  reshape_msd(clean = TRUE) %>%
  spread(indicator, val) %>% 
  group_by(period,psnuuid,psnu) %>%
  summarise_at(vars(HTS_TST_POS:TX_NEW),sum,na.rm=TRUE) %>%
  ungroup() %>%
  group_by(psnu,psnuuid) %>%
  mutate(Linkage=(TX_NEW/HTS_TST_POS)) %>%
  #is.finite()
  #remove_missing(df_proxy,na.rm = FALSE,vars=Linkage(df_proxy), name="Inf",finite=FALSE)
  
  df_proxy<-is

kenya_geo<-st_as_sf(gis_vc_sfc$VcPepfarPolygons.shp) %>%
  left_join(df_proxy, by = c("uid" = "psnuuid"))


library(rgdal)
library(sp)
library(rnaturalearthhires)

kenya_map<-terrain_map(countries = "Kenya", terr_path = si_path(type = "path_raster"), mask = TRUE) +
  geom_sf(data = kenya_geo %>% filter(!is.na(Linkage), !is.infinite(Linkage)), aes(fill = Linkage), lwd = .2, color = grey40k) +
  scale_fill_si(palette = "moody_blues", discrete=FALSE, alpha=0.9, reverse = FALSE,
                       breaks = c(0.25,0.5,0.75, 1.00,1.25,1.5,1.75,2.0, 2.25),
                       limits = c(0.25, 2.25),
                       labels=percent)+
  theme(
    legend.position =  "bottom",
    legend.key.width = ggplot2::unit(1, "cm"),
    legend.key.height = ggplot2::unit(.5, "cm"))+
  labs(subtitle = "Kenya FSW FY20 | % Proxy Linkage",
       caption="Source: PSNU Level MSD")+
  si_style_map()

print(kenya_map)

ggsave(here("Graphics", "2_FY20KENYA_ProxyLinkage_FSW.png"),
       scale = 1.2, dpi = 310, width = 10, height = 7, units = "in")

##adding a bar chart

FSW_viz<-df_proxy %>%
  ggplot(aes(x=reorder(psnu,desc(-Linkage)),
             y=Linkage, fill=Linkage))+#filter(!is.na(Linkage), !is.infinite(Linkage)))+
  geom_col(show.legend = F)+
  coord_flip()+
  scale_y_continuous(labels = percent)+
  scale_fill_si(palette = "moody_blues", discrete=FALSE, alpha=0.9, reverse = FALSE,
                breaks = c(0.25,0.5,0.75, 1.00,1.25,1.5,1.75,2.0, 2.25),
                limits = c(0.25, 2.25),
                labels=percent)+
  si_style_nolines()+
  labs(y="",
       x="",
       title='% Proxy Linkage among FSWs in Kenya | FY20',
       caption="MSD")+
  theme(axis.text.y = element_text(size=8))

print(FSW_viz)


ggsave(here("Graphics","FY20_FSW_ProxyLinkage_Zambia.png"))

##CHANGE BARS TO REFLECT COLOR SCALE AND ORDER BY HIGHEST TO LOWEST

# VIZ COMBINED & SAVED ---------------------------------------------------------------------------------
library(patchwork)
(kenya_map + FSW_viz) +
  plot_annotation(caption = "Source: FY20 MSD
    Linkage Proxy=(TX_NEW/HTS_TST_POS)
    % of Proxy Linkage Among FSWs in Kenya in FY20")

ggsave(here("Graphics", "Kenya_FSW_LinkageProxy.png"),
       scale = 1.2, dpi = 310, width = 10, height = 7, units = "in")
