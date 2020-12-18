# Munging MSD
# G.Sarfaty



# load packages
library(tidyverse)
library(ICPIutilities)
library(extrafont)
library(glamr)
library(glitr)
library(here)
library(scales)


# Read in PSNU x IM MSD using ICPI UItilities -------------------------------------
file_psnu_im <- (list.files(path = si_path(),
                            pattern = "Structured_.*_PSNU_IM.*_20201113_.*.txt",
                            recursive = FALSE,
                            full.names = TRUE))

df<-read_msd(file_psnu_im)


glimpse(df)


# check disaggs of TX_CURR ---------------------------------------------------------
TX_disaggs<-df %>%
  filter(indicator=="TX_CURR") %>%
  distinct(indicator,standardizeddisaggregate)

prinf(TX_disaggs)


TX_age_disaggs<-df %>%
  filter(indicator=="TX_CURR",
         standardizeddisaggregate=="Age/Sex/HIVStatus") %>%
  distinct(indicator,trendscoarse)


prinf(TX_age_disaggs)


# Find by PSNU x IM what share each IP has of overall OU <15 FY21 TX Targets ------------
U15xIP<-df %>%
  filter(operatingunit=="Zambia",
         indicator=="TX_CURR",
         standardizeddisaggregate=="Age/Sex/HIVStatus",
         trendscoarse=="<15",
         fiscal_year=="2021") %>%
  group_by(operatingunit,snu1,psnu,psnuuid,fundingagency,mech_code,mech_name) %>%
  summarise_at(vars(targets),sum,na.rm=TRUE) %>%
  ungroup() %>%
  mutate(total=sum(targets),
         prct_ou_t=targets/total,
         short_name=str_remove_all(psnu,"District"))


prinf(U15xIP)


# Graph it
IP_viz<-U15xIP %>%
  filter(fundingagency=="USAID") %>%
  mutate(mech_name=factor(mech_name,levels=c("SAFE","Local Treatment Partner",
                                             "USAID/District Coverage of Health Services (DISCOVER-H)"))) %>%
  ggplot(aes(x=reorder(short_name,desc(-prct_ou_t)),
             y=prct_ou_t,
             fill=mech_name))+
  geom_col(show.legend = F)+
  coord_flip()+
  scale_y_continuous(labels = percent)+
  facet_wrap(~mech_name)+
  si_style_nolines()+
  labs(y="",
       x="",
       title='% of OU total <15 TX_CURR target | FY21',
       caption="MSD")+
  theme(axis.text.y = element_text(size=8))

print(IP_viz)


ggsave(here("Graphics","FY21_Under15_TX_CURR_T_Zambia.png"))
