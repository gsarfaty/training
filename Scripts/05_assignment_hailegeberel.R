#assignment #5
#Thailegeberel

#Assignment
# Training Assignment 2
# You've been asked to pull data and create a visual to show FY20 proxy linkage by psnu
# among the FSW key populations group for Kenya.
#
# Your job:
# download the correct datasets (MER & geospatial)
# manipulate the MSD to create the proxy linkage metric for FY20 (full year) for FSW
# verify your values against another source (eg. Pano) to ensure they are correct
# map the data & combine with another visualization type of your choosing (eg. bar chart)
# to create one cohesive visual in R; hint: hello, patchwork


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

#set_here('C:\\Users\\teddy\\Documents\\Github\\training\\Scripts')

# MER Data ----------------------------------------------------------------------------
file_psnu_im <- (list.files(path =si_path(type="path_msd"),
                            pattern = "Structured_.*_PSNU_IM.*_20201113_.*.txt",
                            recursive = FALSE,
                            full.names = TRUE))

# read in MSD using ICPIUtilities
df<-read_msd(file_psnu_im) #uses file name file_psnu_im

# generate a list of files in your vector folder using a text pattern to
# catch shps for DATIM level 4 & set file as sf (spatial feature)
gis_vc_poly <- list.files(si_path(type="path_vector"),
                          pattern = "VcPepfar.*.shp$",
                          recursive = T,
                          full.names = T) %>%
  read_sf

  #set_names(basename(.) %>% str_remove("_.*.shp$")) %>%
  #map(read_sf)



# Linkage Proxy
df_fsw<-df %>%
  filter(fiscal_year =="2020",
         indicator %in% c("TX_NEW", "HTS_TST_POS"),
         standardizeddisaggregate %in% c("KeyPop/Result","KeyPop/HIVStatus"),
         otherdisaggregate %in% ("FSW"),
         operatingunit=="Kenya") %>% #filter down full MSD to just the OU and indicators of interest
  #clean_agency() %>%      # Change HHS/CDC to CDC
  clean_psnu()  %>%          # Remove Districts, Country, etc from the end
  group_by(fiscal_year, operatingunit, snu1uid, snu1, psnuuid, psnu, indicator) %>%
  #summarise(across(starts_with("qtr"), sum, na.rm = TRUE)) %>%
  summarise_at(vars(cumulative), sum, na.rm = TRUE) %>%
  ungroup() %>%
  reshape_msd(clean = TRUE) %>%
  spread(indicator, val) %>%
  mutate(Linkage_Proxy = ((TX_NEW/HTS_TST_POS)),
         linkage_proxy_perc = (Linkage_Proxy*100))
  #mutate(Linkage_Proxy = replace_na(Linkage_Proxy,0))

#
# fsw_disaggs<-df %>%
#   filter(fiscal_year =="2020",
#          indicator %in% c("TX_NEW", "HTS_TST_POS"))%>%
#   distinct(indicator,standardizeddisaggregate,otherdisaggregate,categoryoptioncomboname)

# GEO Data Joins --------------------------------------------------------------------------
ken_geo<-st_as_sf(gis_vc_poly) %>% #create new spatial feature
  left_join(df_fsw, by = c("uid" = "psnuuid"))%>%
  filter(!is.na(psnu))
print(ken_geo)
#ken_geo%>%glimpse()


# map proxy linkage by psnu among the FSW

map_ken_fsw <- terrain_map(countries = "Kenya", terr_path = si_path(type = "path_raster"), mask = TRUE) +
  geom_sf(data = ken_geo,
          aes(fill = linkage_proxy_perc), lwd = .2, color = grey10k) + #steps the same as VLS map only we are supplying the TLD % variable instead
  geom_sf_text(data = ken_geo, aes(label = paste0(round(linkage_proxy_perc), "%")), color = usaid_red, size = 4) +
  scale_fill_viridis_c(direction = -1) +
  si_style_map() +
  theme(
    legend.position =  "bottom",
    legend.key.width = ggplot2::unit(1, "cm"),
    legend.key.height = ggplot2::unit(.5, "cm"))+
  ggtitle("FY20 proxy linkage by psnu among the FSW \n key populations group for Kenya.")


print(map_ken_fsw)

plot_fsw<-ken_geo %>%
  clean_psnu() %>%
  ggplot(aes(x = reorder(psnu, linkage_proxy_perc),
             y = linkage_proxy_perc, fill=linkage_proxy_perc)) +
  geom_col(show.legend = F)+
  scale_fill_viridis_c(direction = -1 ) +
  scale_y_continuous(breaks = seq(0,100,25))+
  coord_flip()+
  si_style_xgrid()+
  labs(y="",
       x="",
       title='FY20 proxy linkage by psnu among the \n FSW key populations group for Kenya.',
       caption="Taita Taveta, Laikipia, Kajado have NA values.")+
  theme(axis.text.y = element_text(size=8))

print(plot_fsw)

print(map_ken_fsw + plot_fsw) #side by side mapping
