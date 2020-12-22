# Munging MSD & Getting Mappy
# VLS & TLD example
# G.Sarfaty



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




# MER Data ----------------------------------------------------------------------------

# generate a list of files in your MSD folder using a text pattern of your choosing
file_psnu_im <- (list.files(path =si_path(type="path_msd"),
                            pattern = "Structured_.*_PSNU_IM.*_20201113_.*.txt",
                            recursive = FALSE,
                            full.names = TRUE))


# read in MSD using ICPIUtilities
df<-read_msd(file_psnu_im) #uses file name we found and stored in lines 25-28


# GEO Data ------------------------------------------------------------------------------

# generate a list of files in your vector folder using a text pattern to
# catch shps for DATIM level 5 & set file as sf (spatial feature)
gis_5_sfc <- list.files(si_path(type="path_vector"), pattern = ".*_5_.*.shp$", recursive = T, full.names = T) %>%
  set_names(basename(.) %>% str_remove("_.*.shp$")) %>%
  map(read_sf)

# generate a list of files in your vector folder using a text pattern to
# catch shps for DATIM level 4 & set file as sf (spatial feature)
gis_4_sfc <- list.files(si_path(type="path_vector"), pattern = ".*_4_.*.shp$", recursive = T, full.names = T) %>%
  set_names(basename(.) %>% str_remove("_.*.shp$")) %>%
  map(read_sf)

# use function get_adm_boundaries from the gisR package to download data from GADM
# set as sf (spatial feature)
nga1 <- get_adm_boundaries("NGA", adm_level = 1, geo_path = si_path(type="path_vector")) %>%
  st_as_sf() %>%
  select(country = name_0, province = name_1)



# MER Data Munge -------------------------------------------------------------------------

# VLS & TLD
df_VL<-df %>%
  filter(fiscal_year =="2020",
         indicator %in% c("TX_PVLS","SC_ARVDISP"),
         standardizeddisaggregate %in% c("DispensedARVBottles","Age/Sex/Indication/HIVStatus"),
         operatingunit %in% c("Nigeria")) %>%
  mutate(indicator = ifelse(numeratordenom == "D", paste0(indicator, "_D"), indicator)) %>% #if indicator has denonm, paste D to end of indicator name
  group_by(fiscal_year,operatingunit,fundingagency,psnuuid,psnu,indicator,otherdisaggregate) %>%
  summarise(across(starts_with("qtr"), sum, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(indicator=case_when(
    indicator %in% c("TX_PVLS_D", "TX_PVLS") ~ indicator,
    TRUE ~ otherdisaggregate)) %>%
  reshape_msd(clean = TRUE) %>%
  select(-period_type) %>%
  mutate(val=case_when(
    str_detect(indicator,"180-count") ~ val*6,
    str_detect(indicator, "90-count") ~ val*3,
    TRUE ~ val),
    indicator=case_when(
      indicator %in% c("TX_PVLS_D", "TX_PVLS") ~ indicator,
      str_detect(indicator, "TLD") ~ "TLD",
      TRUE ~ "other"
    )) %>%
  spread(indicator, val) %>%
  group_by(fundingagency,period,psnuuid,psnu) %>%
  summarise_at(vars(other:TX_PVLS_D),sum,na.rm=TRUE) %>%
  ungroup() %>%
  group_by(fundingagency,psnu,psnuuid) %>%
  mutate(VLS=(TX_PVLS/TX_PVLS_D),
         TLD_MOT=ifelse(other >0, (TLD/(TLD+other)), NA),
         VLS_cat=case_when(
           VLS <.8 ~ "Less than 80%",
           VLS >=.8 & VLS <.9 ~ "80-89%",
           VLS >= .9 ~ "Greater than 90%"),
         VLS_TLD_ratio=ifelse(TLD_MOT >0, (VLS/TLD_MOT), NA))



# GEO Data Joins --------------------------------------------------------------------------
nga_geo<-st_as_sf(gis_4_sfc$Nigeria) %>%
  left_join(df_VL, by = c("orgunit_in" = "psnuuid"))


# VIZ ------------------------------------------------------------------------------------

# map by agency for VLS
map_VLS<-terrain_map(countries = "Nigeria", terr_path = si_path(type = "path_raster"), mask = TRUE) +
  geom_sf(data = nga_geo %>% filter(period=="FY20Q4" & !is.na(VLS)),
          aes(fill=VLS), lwd=.2, color=grey50k)+
  scale_fill_viridis_c(option="viridis", alpha=0.9, direction = -1,
                       breaks = c(0.5,0.75, 1.00),
                       limits = c(0.50, 1.00),
                       labels=percent)+
  si_style_map()+
  theme(
    legend.position =  "bottom",
    legend.key.width = ggplot2::unit(1, "cm"),
    legend.key.height = ggplot2::unit(.5, "cm"))+
  ggtitle("Viral Load Suppression")+
  # theme(plot.title = element_text(size = 9, family = "Source Sans Pro", face=1))+
  facet_wrap(~fundingagency, nrow = 2)


# map by agency for TLD
map_TLD_MOT<-terrain_map(countries = "Nigeria", terr_path = si_path(type = "path_raster"), mask = TRUE) +
  geom_sf(data = nga_geo %>% filter(period=="FY20Q4" & !is.na(TLD_MOT)),
          aes(fill = TLD_MOT), lwd = .2, color = grey10k) +
  geom_sf(data = nga1, fill = NA, lwd = .2, color = grey30k) +
  scale_fill_viridis_c(option="magma", alpha=0.9, direction = -1,
                       breaks = c(0.5,0.75, 1.00),
                       limits = c(0.50, 1.00),
                       labels=percent)+
  si_style_map() +
  theme(
    legend.position =  "bottom",
    legend.key.width = ggplot2::unit(1, "cm"),
    legend.key.height = ggplot2::unit(.5, "cm"))+
  ggtitle("% TLD Months of TX (MOT) \n out of total ARVs Dispensed")+
  # theme(plot.title = element_text(size = 9, family = "Source Sans Pro", face=1))+
  facet_wrap(~fundingagency, nrow=2)



scatter<-df_VL %>%
  filter(period=="FY20Q4",
         fundingagency %in% c("HHS/CDC", "USAID")) %>%
  ggplot(aes(
    x=TLD_MOT,
    y=VLS))+
  geom_point(aes(fill=period),
             color="white",shape=21,size=4,alpha=0.7,
             show.legend = F)+
  scale_fill_manual(values=c(grey50k))+
  scale_x_continuous(limits=c(0.5,1), labels = percent)+
  scale_y_continuous(limits=c(0.5,1), labels = percent)+
  geom_abline(intercept=0,slope=1,linetype="dashed")+
  labs(x = "% TLD MOT Dispensed", y="VLS %")+
  si_style_nolines()+
  ggtitle("VLS vs. % TLD Months of TX \n (MOT) Dispensed")+
  theme(plot.title = element_text(size = 9, family = "Source Sans Pro", face=1))+
  facet_wrap(~fundingagency, nrow=2)


# VIZ COMBINED & SAVED ---------------------------------------------------------------------------------

(map_VLS + map_TLD_MOT + scatter) +
  plot_annotation(caption = "Source: FY20Q4i MSD
    VLS=(TX_PVLS_N/TX_PVLS_D)
    ARV Disp adjusted for Months of Treatments based on pill count")


ggsave(here("Graphics", "Nigeria_VLS_TLD_TLE_Ratio2.png"),
       scale = 1.2, dpi = 310, width = 10, height = 7, units = "in")
