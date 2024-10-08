library(tidyverse)
library(ggplot2)
library(dplyr)

##### Import data
base_bio<-read_csv("./Data/EBS/co_bio_baseline.csv")
alt_1_bio<-read_csv("./Data/EBS/co_bio_alt_strata_1.csv")
alt_2_bio<-read_csv("./Data/EBS/co_bio_alt_strata_2.csv")
alt_3_bio<-read_csv("./Data/EBS/co_bio_alt_strata_3.csv")
alt_4_bio<-read_csv("./Data/EBS/co_bio_alt_strata_4.csv")
alt_5_bio<-read_csv("./Data/EBS/co_bio_alt_strata_5.csv")
alt_6_bio<-read_csv("./Data/EBS/co_bio_alt_strata_6.csv")

base_abun<-read_csv("./Data/EBS/co_abun_baseline.csv")
alt_1_abun<-read_csv("./Data/EBS/co_abun_alt_strata_1.csv")
alt_2_abun<-read_csv("./Data/EBS/co_abun_alt_strata_2.csv")
alt_3_abun<-read_csv("./Data/EBS/co_abun_alt_strata_3.csv")
alt_4_abun<-read_csv("./Data/EBS/co_abun_alt_strata_4.csv")
alt_5_abun<-read_csv("./Data/EBS/co_abun_alt_strata_5.csv")
alt_6_abun<-read_csv("./Data/EBS/co_abun_alt_strata_6.csv")

base_abun
######### Mature male biomass
base_bio %>%
  select(SURVEY_YEAR, BIOMASS_MALE_LE94, CV_BIOMASS_MALE_LE94, CI_BIOMASS_MALE_LE94) %>%
  rename_all(~c("SURVEY_YEAR", "BASELINE_BIO_LE94", "BASELINE_CV_BIO_LE94", "BASELINE_CI_BIO_LE94")) -> base_matbio
  
alt_1_bio %>% 
  select(BIOMASS_MALE_LE94, CV_BIOMASS_MALE_LE94, CI_BIOMASS_MALE_LE94) %>%
     rename_all(~c("ALT1_BIO_LE94", "ALT1_CV_BIO_LE94", "ALT1_CI_BIO_LE94"))-> alt_1_matbio

alt_2_bio %>% 
  select(BIOMASS_MALE_LE94, CV_BIOMASS_MALE_LE94, CI_BIOMASS_MALE_LE94) %>%
   rename_all(~c("ALT2_BIO_LE94", "ALT2_CV_BIO_LE94", "ALT2_CI_BIO_LE94"))-> alt_2_matbio

alt_3_bio %>% 
  select(BIOMASS_MALE_LE94, CV_BIOMASS_MALE_LE94, CI_BIOMASS_MALE_LE94) %>%
   rename_all(~c("ALT3_BIO_LE94", "ALT3_CV_BIO_LE94", "ALT3_CI_BIO_LE94"))-> alt_3_matbio

alt_4_bio %>% 
  select(BIOMASS_MALE_LE94, CV_BIOMASS_MALE_LE94, CI_BIOMASS_MALE_LE94) %>%
   rename_all(~c("ALT4_BIO_LE94", "ALT4_CV_BIO_LE94", "ALT4_CI_BIO_LE94"))-> alt_4_matbio

alt_5_bio %>% 
  select(BIOMASS_MALE_LE94, CV_BIOMASS_MALE_LE94, CI_BIOMASS_MALE_LE94) %>%
  rename_all(~c("ALT5_BIO_LE94", "ALT5_CV_BIO_LE94", "ALT5_CI_BIO_LE94"))-> alt_5_matbio

alt_6_bio %>% 
  select(BIOMASS_MALE_LE94, CV_BIOMASS_MALE_LE94, CI_BIOMASS_MALE_LE94) %>%
  rename_all(~c("ALT6_BIO_LE94", "ALT6_CV_BIO_LE94", "ALT6_CI_BIO_LE94"))-> alt_6_matbio

all_bio<-as.data.frame(cbind(base_matbio, alt_1_matbio, alt_2_matbio, alt_3_matbio, alt_4_matbio, alt_5_matbio, alt_6_matbio))
###########

base_abun %>%
  select(SURVEY_YEAR, NUM_MALE_LE94, CV_NUM_MALE_LE94, CI_NUM_MALE_LE94) %>%
  rename_all(~c("SURVEY_YEAR", "BASELINE_ABUN_LE94", "BASELINE_CV_ABUN_LE94","BASELINE_CI_ABUN_LE94")) -> base_matabun

alt_1_abun %>%
  select(NUM_MALE_LE94, CV_NUM_MALE_LE94, CI_NUM_MALE_LE94) %>%
  rename_all(~c("ALT1_ABUN_LE94", "ALT1_CV_ABUN_LE94","ALT1_CI_ABUN_LE94")) -> alt_1_matabun

alt_2_abun %>%
  select(NUM_MALE_LE94, CV_NUM_MALE_LE94, CI_NUM_MALE_LE94) %>%
  rename_all(~c("ALT2_ABUN_LE94", "ALT2_CV_ABUN_LE94","ALT2_CI_ABUN_LE94")) -> alt_2_matabun

alt_3_abun %>%
  select(NUM_MALE_LE94, CV_NUM_MALE_LE94, CI_NUM_MALE_LE94) %>%
  rename_all(~c("ALT3_ABUN_LE94", "ALT3_CV_ABUN_LE94","ALT3_CI_ABUN_LE94")) -> alt_3_matabun

alt_4_abun %>%
  select(NUM_MALE_LE94, CV_NUM_MALE_LE94, CI_NUM_MALE_LE94) %>%
  rename_all(~c("ALT4_ABUN_LE94", "ALT4_CV_ABUN_LE94","ALT4_CI_ABUN_LE94")) -> alt_4_matabun

alt_5_abun %>%
  select(NUM_MALE_LE94, CV_NUM_MALE_LE94, CI_NUM_MALE_LE94) %>%
  rename_all(~c("ALT5_ABUN_LE94", "ALT5_CV_ABUN_LE94","ALT5_CI_ABUN_LE94")) -> alt_5_matabun

alt_6_abun %>%
  select(NUM_MALE_LE94, CV_NUM_MALE_LE94, CI_NUM_MALE_LE94) %>%
  rename_all(~c("ALT6_ABUN_LE94", "ALT6_CV_ABUN_LE94","ALT6_CI_ABUN_LE94")) -> alt_6_matabun

all_abun<-as.data.frame(cbind(base_matabun, alt_1_matabun, alt_2_matabun, alt_3_matabun, alt_4_matabun, alt_5_matabun, alt_6_matabun))

################################ LE94 BIOMASS #########################################################################
names(all_bio)

ggplot(all_bio, aes(x=SURVEY_YEAR, y=BASELINE_BIO_LE94, color ="baseline", size = 3))+
  geom_point()+  
  geom_point(aes(y=ALT1_BIO_LE94, color = "alternative 1"), size = 3) +
  geom_point(aes(y=ALT2_BIO_LE94, color = "alternative 2"), size = 3)+
  geom_point(aes(y=ALT3_BIO_LE94, color = "alternative 3"), size = 3)+
  geom_point(aes(y=ALT4_BIO_LE94, color = "alternative 4"), size = 3)+
  geom_point(aes(y=ALT5_BIO_LE94, color = "alternative 5"), size = 3)+
  geom_point(aes(y=ALT6_BIO_LE94, color = "alternative 6"), size = 3)+
  labs(colour="Datasets",y="Male LE94 Biomass (MT)",x="Survey Year")+ 
  theme(legend.position = c(0, 1),legend.justification = c(0, 1))+
  scale_color_manual(values = c("orange","red", "blue", "green", "black", "grey","brown"))
  
  
  
  

ggplot(all_bio, aes(x=SURVEY_YEAR))+
  geom_point(aes(y=BASELINE_BIO_LE94), color ="black", size = 3) +
  geom_point(aes(y=ALT1_BIO_LE94), color = "red", size = 3) +
  geom_point(aes(y=ALT2_BIO_LE94), color = "yellow", size = 3)+
  geom_point(aes(y=ALT3_BIO_LE94), color = "green", size = 3)+
  geom_point(aes(y=ALT4_BIO_LE94), color = "blue", size = 3)+
  geom_point(aes(y=ALT5_BIO_LE94), color = "grey", size = 3)+
  geom_point(aes(y=ALT6_BIO_LE94), color = "brown", size = 3)

################################ LE94 BIOMASS CV #########################################################################
names(all_bio)

ggplot(all_bio, aes(x=SURVEY_YEAR, y=BASELINE_CV_BIO_LE94, color ="baseline", size = 3))+
  geom_point()+  
  geom_point(aes(y=ALT1_CV_BIO_LE94, color = "alternative 1"), size = 3) +
  geom_point(aes(y=ALT2_CV_BIO_LE94, color = "alternative 2"), size = 3)+
  geom_point(aes(y=ALT3_CV_BIO_LE94, color = "alternative 3"), size = 3)+
  geom_point(aes(y=ALT4_CV_BIO_LE94, color = "alternative 4"), size = 3)+
  geom_point(aes(y=ALT5_CV_BIO_LE94, color = "alternative 5"), size = 3)+
  geom_point(aes(y=ALT6_CV_BIO_LE94, color = "alternative 6"), size = 3)+
  labs(colour="Datasets",y="Male LE94 Biomass CV",x="Survey Year")+ 
  theme(legend.position = c(0, 1),legend.justification = c(0, 1))+
  scale_color_manual(values = c("orange","red", "blue", "green", "black","grey","brown"))


ggplot(all_bio, aes(x=SURVEY_YEAR))+
  geom_point(aes(y=BASELINE_CV_BIO_LE94), color ="black", size = 3) +
  geom_point(aes(y=ALT1_CV_BIO_LE94), color = "red", size = 3) +
  geom_point(aes(y=ALT2_CV_BIO_LE94), color = "orange", size = 3)+
  geom_point(aes(y=ALT3_CV_BIO_LE94), color = "green", size = 3)+
  geom_point(aes(y=ALT4_CV_BIO_LE94), color = "blue", size = 3)+
  geom_point(aes(y=ALT5_CV_BIO_LE94), color = "blue", size = 3)+
  geom_point(aes(y=ALT6_CV_BIO_LE94), color = "brown", size = 3)
  

################################ LE94 BIOMASS CI #########################################################################
names(all_bio)

ggplot(all_bio, aes(x=SURVEY_YEAR, y=BASELINE_CI_BIO_LE94, color ="baseline", size = 3))+
  geom_point()+  
  geom_point(aes(y=ALT1_CI_BIO_LE94, color = "alternative 1"), size = 3)+
  geom_point(aes(y=ALT2_CI_BIO_LE94, color = "alternative 2"), size = 3)+
  geom_point(aes(y=ALT3_CI_BIO_LE94, color = "alternative 3"), size = 3)+
  geom_point(aes(y=ALT4_CI_BIO_LE94, color = "alternative 4"), size = 3)+
  geom_point(aes(y=ALT5_CI_BIO_LE94, color = "alternative 5"), size = 3)+
  geom_point(aes(y=ALT6_CI_BIO_LE94, color = "alternative 6"), size = 3)+
  labs(colour="Datasets",y="Male LE94 Biomass CI",x="Survey Year")+ 
  theme(legend.position = c(0, 1),legend.justification = c(0, 1))+
  scale_color_manual(values = c("orange","red", "blue", "green", "black", "grey","brown"))


ggplot(all_bio, aes(x=SURVEY_YEAR))+
  geom_point(aes(y=BASELINE_CI_BIO_LE94), color ="black", size = 3)+
  geom_point(aes(y=ALT1_CI_BIO_LE94), color = "red", size = 3)+
  geom_point(aes(y=ALT2_CI_BIO_LE94), color = "orange", size = 3)+
  geom_point(aes(y=ALT3_CI_BIO_LE94), color = "green", size = 3)+
  geom_point(aes(y=ALT4_CI_BIO_LE94), color = "blue", size = 3)+
  geom_point(aes(y=ALT5_CI_BIO_LE94), color = "grey", size = 3)+
  geom_point(aes(y=ALT5_CI_BIO_LE94), color = "brown", size = 3)

###########################################################################################################################
#################################### calculate averages for each group/column  across all years############################


all_bio %>%
  select(where(is.numeric)) %>% 
  colMeans(na.rm = TRUE) -> all_bio_avgs

all_abun %>%
  select(where(is.numeric)) %>% 
  colMeans(na.rm = TRUE) -> all_abun_avgs

all_bio %>%
  select(BASELINE_CV_BIO_LE94,ALT1_CV_BIO_LE94,ALT2_CV_BIO_LE94,ALT3_CV_BIO_LE94,ALT4_CV_BIO_LE94,ALT5_CV_BIO_LE94,ALT6_CV_BIO_LE94) %>% 
  colMeans(na.rm = TRUE) %>%
  as.matrix()-> bio_cv_avgs

bio_cv_avgs




  

#################################### Deprecated method ###################################################################
#all_bio %>%
#  select(where(is.numeric)) %>%
# summarise(across(everything(), 
#                  mean,
#                  na.rm = TRUE)) ->all_bio_avgs
#all_bio_avgs
