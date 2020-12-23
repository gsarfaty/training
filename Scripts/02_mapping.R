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
gis_5_sfc <- list.files(si_path(type="path_vector"),
                        pattern = ".*_5_.*.shp$", recursive = T, full.names = T) %>%
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
         operatingunit=="Nigeria") %>% #filter down full MSD to just the OU and indicators of interest
  mutate(indicator = ifelse(numeratordenom == "D", paste0(indicator, "_D"), indicator)) %>% #if indicator has a denonminator, paste D to end of indicator name
  group_by(fiscal_year,operatingunit,fundingagency,psnuuid,psnu,indicator,otherdisaggregate) %>% #aggregate data up to agencyxpsnuxindicatorxotherdisagg
  summarise(across(starts_with("qtr"), sum, na.rm = TRUE)) %>% #sum columns that begin w qtr
  ungroup() %>%
  mutate(indicator=case_when(
    indicator %in% c("TX_PVLS_D", "TX_PVLS") ~ indicator,
    TRUE ~ otherdisaggregate)) %>% #when indicator is PVLS then populate indicator with indicator; else populate w other disagg
  reshape_msd(clean = TRUE) %>% #reshape MSD to long using ICPIUtilities
  select(-period_type) %>% #remove the period type column
  mutate(val=case_when(
    str_detect(indicator,"180-count") ~ val*6, #when the word 180-count is deteced in indicator name, multiply val column by 6
    str_detect(indicator, "90-count") ~ val*3, #when the word 90-count is deteced in indicator name, multiply val column by 3
    TRUE ~ val),
    indicator=case_when(
      indicator %in% c("TX_PVLS_D", "TX_PVLS") ~ indicator,
      str_detect(indicator, "TLD") ~ "TLD",
      TRUE ~ "other" #when indciator is PLVS, keep as is; when indicator includes text "TLD" populate indicator w TLD; otherwise populate with "other"
    )) %>%
  spread(indicator, val) %>% #make wide by indicator
  group_by(fundingagency,period,psnuuid,psnu) %>% #aggregate so that data for each psnu is confined to one row per period
  summarise_at(vars(other:TX_PVLS_D),sum,na.rm=TRUE) %>%
  mutate(VLS=(TX_PVLS/TX_PVLS_D), #calculate viral load suppression rate
         TLD_MOT=ifelse(other >0, (TLD/(TLD+other)), NA), #calculate what % dispensed was TLD out of total dispensation
         VLS_cat=case_when(
           VLS <.8 ~ "Less than 80%",
           VLS >=.8 & VLS <.9 ~ "80-89%",
           VLS >= .9 ~ "Greater than 90%"), #create a column to categorize VLS; not used in viz but could be useful in future
         VLS_TLD_ratio=ifelse(TLD_MOT >0, (VLS/TLD_MOT), NA)) %>% #create columm to see ration of VLS to TLD; not used in viz
  ungroup()



# GEO Data Joins --------------------------------------------------------------------------
nga_geo<-st_as_sf(gis_4_sfc$Nigeria) %>% #create new spatial feature that is just Nigeria's level 4
  left_join(df_VL, by = c("orgunit_in" = "psnuuid")) #join the spatial feature to our df_VL data frame so we can map it


# VIZ ------------------------------------------------------------------------------------

# map by agency for VLS
map_VLS<-terrain_map(countries = "Nigeria",
                     terr_path = si_path(type = "path_raster"), mask = TRUE) + #use terrain_map function from gisR to get topographic basemap
  geom_sf(data = nga_geo %>% filter(period=="FY20Q4" & !is.na(VLS)), #map the VLS data from the nga_geo file we made; filter it as needed
          aes(fill=VLS), lwd=.2, color=grey50k)+
  scale_fill_viridis_c(option="viridis", alpha=0.9, direction = -1, #pick color ramp via option; alpha sets transpaerency; direction can change direction of color ramp
                       breaks = c(0.5,0.75, 1.00), #sets breaks we want to see in our legend;
                       limits = c(0.50, 1.00), #sets min and max of our legend and color ramp;
                       labels=percent)+ #ensures values are displayed as percentages
  si_style_map()+ #apply OHA SI style to map
  theme(
    legend.position =  "bottom",
    legend.key.width = ggplot2::unit(1, "cm"),
    legend.key.height = ggplot2::unit(.5, "cm"))+ #change legend size
  ggtitle("Viral Load Suppression")+ #add plot title
  # theme(plot.title = element_text(size = 9, family = "Source Sans Pro", face=1))+ #change plot title font characteristics, if desired
  facet_wrap(~fundingagency, nrow = 2) #use facet by funding agency to make separate maps for each agency; nrows=2 means the plot will have 2 rows

print(map_VLS)

# map by agency for TLD
map_TLD_MOT<-terrain_map(countries = "Nigeria", terr_path = si_path(type = "path_raster"), mask = TRUE) +
  geom_sf(data = nga_geo %>% filter(period=="FY20Q4" & !is.na(TLD_MOT)),
          aes(fill = TLD_MOT), lwd = .2, color = grey10k) + #steps the same as VLS map only we are supplying the TLD % variable instead
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

print(map_TLD_MOT)


scatter<-df_VL %>%
  filter(period=="FY20Q4",
         fundingagency %in% c("HHS/CDC", "USAID")) %>% #filter VL dataset to include only most recent Q
  ggplot(aes(
    x=TLD_MOT,
    y=VLS))+ #set x and y of scatterplot
  geom_point(aes(fill=period),
             color="white",shape=21,size=4,alpha=0.7,
             show.legend = F)+ #change size, shape, and transparency of dots
  scale_fill_manual(values=c(grey50k))+ #fill dots with specific color
  scale_x_continuous(limits=c(0.5,1), labels = percent)+ #set x axis min and max (limits); ensure axis displays value as %
  scale_y_continuous(limits=c(0.5,1), labels = percent)+ #set y axis min and max (limits); ensure axis displays value as %
  geom_abline(intercept=0,slope=1,linetype="dotted")+ #create refrence line
  labs(x = "% TLD MOT Dispensed",
       y="VLS %")+ #change axis labels
  si_style_nolines()+ #apply OHA SI style from glitr
  ggtitle("VLS vs. % TLD Months of TX \n (MOT) Dispensed")+ #add plot title
  theme(plot.title = element_text(size = 9, family = "Gill Sans MT", face=1))+ #modify plot title characteristics, if desired
  facet_wrap(~fundingagency, nrow=2) #facet by agency so that we have a scatter for each agency

print(scatter)

# VIZ COMBINED & SAVED ---------------------------------------------------------------------------------

(map_VLS + map_TLD_MOT + scatter) + #put all 3 of our graphics together in one plot using patchwork
  plot_annotation(caption = "Source: FY20Q4i MSD
    VLS=(TX_PVLS_N/TX_PVLS_D)
    ARV Disp adjusted for Months of Treatments based on pill count") #add caption to overall graphic


ggsave(here("Graphics", "Nigeria_VLS_TLD_TLE_Ratio2.png"),
       scale = 1.2, dpi = 310, width = 10, height = 7, units = "in") #save our combined graphic
