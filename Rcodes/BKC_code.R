library(tidyverse)
library(ggplot2)
library(dplyr)

##### Import data
base_bio<-read_csv("./Data/nbs_bkc_bio_baseline_v2022.csv")
alt_1_bio<-read_csv("./Data/nbs_bkc_bio_alt_strata_1.csv")
alt_2_bio<-read_csv("./Data/nbs_bkc_bio_alt_strata_2.csv")
alt_3_bio<-read_csv("./Data/nbs_bkc_bio_alt_strata_3.csv")
alt_4_bio<-read_csv("./Data/nbs_bkc_bio_alt_strata_4.csv")

base_abun<-read_csv("./Data/nbs_bkc_abun_baseline_v2022.csv")
alt_1_abun<-read_csv("./Data/nbs_bkc_abun_alt_strata_1.csv")
alt_2_abun<-read_csv("./Data/nbs_bkc_abun_alt_strata_2.csv")
alt_3_abun<-read_csv("./Data/nbs_bkc_abun_alt_strata_3.csv")
alt_4_abun<-read_csv("./Data/nbs_bkc_abun_alt_strata_4.csv")

base_abun
######### Mature male biomass
base_bio %>%
  select(SURVEY_YEAR, BIOMASS_MALE_GE94, CV_BIOMASS_MALE_GE94, CI_BIOMASS_MALE_GE94) %>%
  rename_all(~c("SURVEY_YEAR", "BASELINE_BIO_GE94", "BASELINE_CV_BIO_GE94", "BASELINE_CI_GE94")) -> base_matbio
  
alt_1_bio %>% 
  select(BIOMASS_MALE_GE94, CV_BIOMASS_MALE_GE94, CI_BIOMASS_MALE_GE94) %>%
     rename_all(~c("ALT1_BIO_GE94", "ALT1_CV_BIO_GE94", "ALT1_CI_GE94"))-> alt_1_matbio

alt_2_bio %>% 
  select(BIOMASS_MALE_GE94, CV_BIOMASS_MALE_GE94, CI_BIOMASS_MALE_GE94) %>%
   rename_all(~c("ALT2_BIO_GE94", "ALT2_CV_BIO_GE94", "ALT2_CI_GE94"))-> alt_2_matbio

alt_3_bio %>% 
  select(BIOMASS_MALE_GE94, CV_BIOMASS_MALE_GE94, CI_BIOMASS_MALE_GE94) %>%
   rename_all(~c("ALT3_BIO_GE94", "ALT3_CV_BIO_GE94", "ALT3_CI_GE94"))-> alt_3_matbio

alt_4_bio %>% 
  select(BIOMASS_MALE_GE94, CV_BIOMASS_MALE_GE94, CI_BIOMASS_MALE_GE94) %>%
   rename_all(~c("ALT4_BIO_GE94", "ALT4_CV_BIO_GE94", "ALT4_CI_GE94"))-> alt_4_matbio
   
all_bio<-as.data.frame(cbind(base_matbio, alt_1_matbio, alt_2_matbio, alt_3_matbio, alt_4_matbio))
###########

base_abun %>%
  select(SURVEY_YEAR, NUM_MALE_GE94, CV_NUM_MALE_GE94, CI_NUM_MALE_GE94) %>%
  rename_all(~("SURVEY_YEAR", "BASELINE_ABUN_GE94", "BASELINE_CV_ABUN_GE94","BASELINE_CI_ABUN_GE94")) -> base_matabun

alt_1_abun %>%
  select(NUM_MALE_GE94, CV_NUM_MALE_GE94, CI_NUM_MALE_GE94) %>%
  rename_all(~("ALT1_ABUN_GE94", "ALT1_CV_ABUN_GE94","ALT1_CI_ABUN_GE94")) -> alt1_matabun

alt_2_abun %>%
  select(NUM_MALE_GE94, CV_NUM_MALE_GE94, CI_NUM_MALE_GE94) %>%
  rename_all(~("ALT2_ABUN_GE94", "ALT2_CV_ABUN_GE94","ALT2_CI_ABUN_GE94")) -> alt2_matabun

alt_3_abun %>%
  select(NUM_MALE_GE94, CV_NUM_MALE_GE94, CI_NUM_MALE_GE94) %>%
  rename_all(~("ALT3_ABUN_GE94", "ALT3_CV_ABUN_GE94","ALT3_CI_ABUN_GE94")) -> alt3_matabun

alt_4_abun %>%
  select(NUM_MALE_GE94, CV_NUM_MALE_GE94, CI_NUM_MALE_GE94) %>%
  rename_all(~("ALT4_ABUN_GE94", "ALT4_CV_ABUN_GE94","ALT4_CI_ABUN_GE94")) -> alt4_matabun

all_abun<-as.data.frame(cbind(base_matabun, alt_1_matabun, alt_2_matabun, alt_3_matabun, alt_4_matabun))

################################ GE94 BIOMASS #########################################################################
names(all_bio)

ggplot(all_bio, aes(x=SURVEY_YEAR, y=BASELINE_BIO_GE94, color ="baseline", size = 3))+
  geom_point()+  
  geom_point(aes(y=ALT1_BIO_GE94, color = "alternative 1"), size = 3) +
  geom_point(aes(y=ALT2_BIO_GE94, color = "alternative 2"), size = 3)+
  geom_point(aes(y=ALT3_BIO_GE94, color = "alternative 3"), size = 3)+
  geom_point(aes(y=ALT4_BIO_GE94, color = "alternative 4"), size = 3)+
  labs(colour="Datasets",y="Male GE94 Biomass (MT)",x="Survey Year")+ 
  theme(legend.position = c(0, 1),legend.justification = c(0, 1))+
  scale_color_manual(values = c("orange","red", "blue", "green", "black"))
  
  
  
  

ggplot(all_bio, aes(x=SURVEY_YEAR))+
  geom_point(aes(y=BASELINE_BIO_GE94), color ="black", size = 3) +
  geom_point(aes(y=ALT1_BIO_GE94), color = "red", size = 3) +
  geom_point(aes(y=ALT2_BIO_GE94), color = "yellow", size = 3)+
  geom_point(aes(y=ALT3_BIO_GE94), color = "green", size = 3)+
  geom_point(aes(y=ALT4_BIO_GE94), color = "blue", size = 3)

################################ GE94 BIOMASS CV #########################################################################
names(all_bio)

ggplot(all_bio, aes(x=SURVEY_YEAR, y=BASELINE_CV_BIO_GE94, color ="baseline", size = 3))+
  geom_point()+  
  geom_point(aes(y=ALT1_CV_BIO_GE94, color = "alternative 1"), size = 3) +
  geom_point(aes(y=ALT2_CV_BIO_GE94, color = "alternative 2"), size = 3)+
  geom_point(aes(y=ALT3_CV_BIO_GE94, color = "alternative 3"), size = 3)+
  geom_point(aes(y=ALT4_CV_BIO_GE94, color = "alternative 4"), size = 3)+
  labs(colour="Datasets",y="Male GE94 Biomass CV",x="Survey Year")+ 
  theme(legend.position = c(0, 1),legend.justification = c(0, 1))+
  scale_color_manual(values = c("orange","red", "blue", "green", "black"))


ggplot(all_bio, aes(x=SURVEY_YEAR))+
  geom_point(aes(y=BASELINE_CV_BIO_GE94), color ="black", size = 3) +
  geom_point(aes(y=ALT1_CV_BIO_GE94), color = "red", size = 3) +
  geom_point(aes(y=ALT2_CV_BIO_GE94), color = "orange", size = 3)+
  geom_point(aes(y=ALT3_CV_BIO_GE94), color = "green", size = 3)+
  geom_point(aes(y=ALT4_CV_BIO_GE94), color = "blue", size = 3)
  

################################ GE94 BIOMASS CI #########################################################################
names(all_bio)

ggplot(all_bio, aes(x=SURVEY_YEAR, y=BASELINE_CI_BIO_GE94, color ="baseline", size = 3))+
  geom_point()+  
  geom_point(aes(y=ALT1_CI_BIO_GE94, color = "alternative 1"), size = 3)+
  geom_point(aes(y=ALT2_CI_BIO_GE94, color = "alternative 2"), size = 3)+
  geom_point(aes(y=ALT3_CI_BIO_GE94, color = "alternative 3"), size = 3)+
  geom_point(aes(y=ALT4_CI_BIO_GE94, color = "alternative 4"), size = 3)+
  labs(colour="Datasets",y="Male GE94 Biomass CI",x="Survey Year")+ 
  theme(legend.position = c(0, 1),legend.justification = c(0, 1))+
  scale_color_manual(values = c("orange","red", "blue", "green", "black"))


ggplot(all_bio, aes(x=SURVEY_YEAR))+
  geom_point(aes(y=BASELINE_CI_BIO_GE94), color ="black", size = 3)+
  geom_point(aes(y=ALT1_CI_BIO_GE94), color = "red", size = 3)+
  geom_point(aes(y=ALT2_CI_BIO_GE94), color = "orange", size = 3)+
  geom_point(aes(y=ALT3_CI_BIO_GE94), color = "green", size = 3)+
  geom_point(aes(y=ALT4_CI_BIO_GE94), color = "blue", size = 3)