library(tidyverse)
library(ggplot2)
library(dplyr)

##### Import data
base_bio<-read_csv("./Data/rk_bb_leg1_biomass_baseline.csv")

alt_1_bio<-read_csv("./Data/RK_BB_BIOMASS_ALTERNATE_STRATA_1.csv")
alt_2_bio<-read_csv("./Data/RK_BB_BIOMASS_ALTERNATE_STRATA_2.csv")
alt_3_bio<-read_csv("./Data/RK_BB_BIOMASS_ALTERNATE_STRATA_3.csv")


base_abun<-read_csv("./Data/rk_bb_leg1_abundance_baseline.csv")

alt_1_abun<-read_csv("./Data/RK_BB_ABUN_ALTERNATE_STRATA_1.csv")
alt_2_abun<-read_csv("./Data/RK_BB_ABUN_ALTERNATE_STRATA_2.csv")
alt_3_abun<-read_csv("./Data/RK_BB_ABUN_ALTERNATE_STRATA_3.csv")




base_abun
names(base_bio)
######### Mature male biomass
base_bio %>%
  select(SURVEY_YEAR, BIOMASS_MALE_GE120, CV_BIOMASS_MALE_GE120, CI_BIOMASS_MALE_GE120) %>%
  rename_all(~c("SURVEY_YEAR", "BASELINE_BIO_MALE_GE120", "BASELINE_CV_BIO_MALE_GE120", "BASELINE_CI_BIO_MALE_GE120")) -> base_matbio
  
alt_1_bio %>% 
  select(BIOMASS_MALE_GE120, CV_BIOMASS_MALE_GE120, CI_BIOMASS_MALE_GE120) %>%
     rename_all(~c("ALT1_BIO_MALE_GE120", "ALT1_CV_BIO_MALE_GE120", "ALT1_CI_BIO_MALE_GE120"))-> alt_1_matbio

alt_2_bio %>% 
  select(BIOMASS_MALE_GE120, CV_BIOMASS_MALE_GE120, CI_BIOMASS_MALE_GE120) %>%
   rename_all(~c("ALT2_BIO_MALE_GE120", "ALT2_CV_BIO_MALE_GE120", "ALT2_CI_BIO_MALE_GE120"))-> alt_2_matbio

alt_3_bio %>% 
  select(BIOMASS_MALE_GE120, CV_BIOMASS_MALE_GE120, CI_BIOMASS_MALE_GE120) %>%
   rename_all(~c("ALT3_BIO_MALE_GE120", "ALT3_CV_BIO_MALE_GE120", "ALT3_CI_BIO_MALE_GE120"))-> alt_3_matbio
   
all_bio<-as.data.frame(cbind(base_matbio, alt_1_matbio, alt_2_matbio, alt_3_matbio))
###########

base_abun %>%
  select(SURVEY_YEAR, NUM_MALE_GE120, CV_NUM_MALE_GE120, CI_NUM_MALE_GE120) %>%
  rename_all(~c("SURVEY_YEAR", "BASELINE_ABUN_MALE_GE120", "BASELINE_CV_ABUN_MALE_GE120","BASELINE_CI_ABUN_MALE_GE120")) -> base_matabun

alt_1_abun %>%
  select(NUM_MALE_GE120, CV_NUM_MALE_GE120, CI_NUM_MALE_GE120) %>%
  rename_all(~c("ALT1_ABUN_MALE_GE120", "ALT1_CV_ABUN_MALE_GE120","ALT1_CI_ABUN_MALE_GE120")) -> alt_1_matabun

alt_2_abun %>%
  select(NUM_MALE_GE120, CV_NUM_MALE_GE120, CI_NUM_MALE_GE120) %>%
  rename_all(~c("ALT2_ABUN_MALE_GE120", "ALT2_CV_ABUN_MALE_GE120","ALT2_CI_ABUN_MALE_GE120")) -> alt_2_matabun

alt_3_abun %>%
  select(NUM_MALE_GE120, CV_NUM_MALE_GE120, CI_NUM_MALE_GE120) %>%
  rename_all(~c("ALT3_ABUN_MALE_GE120", "ALT3_CV_ABUN_MALE_GE120","ALT3_CI_ABUN_MALE_GE120")) -> alt_3_matabun


all_abun<-as.data.frame(cbind(base_matabun, alt_1_matabun, alt_2_matabun, alt_3_matabun))

################################ MALE_GE120 BIOMASS #########################################################################
names(all_bio)

ggplot(all_bio, aes(x=SURVEY_YEAR, y=BASELINE_BIO_MALE_GE120, color ="baseline", size = 5))+
  geom_point()+  
  geom_point(aes(y=ALT1_BIO_MALE_GE120, color = "alternative 1"), size = 3) +
  geom_point(aes(y=ALT2_BIO_MALE_GE120, color = "alternative 2"), size = 3)+
  geom_point(aes(y=ALT3_BIO_MALE_GE120, color = "alternative 3"), size = 3)+
  labs(colour="Datasets",y="Mature Male Biomass (MT)",x="Survey Year")+ 
  theme(legend.position = c(0, 1),legend.justification = c(0, 1))+
  scale_color_manual(values = c("black","red", "blue", "green"))
  
  
  
  

ggplot(all_bio, aes(x=SURVEY_YEAR))+
  geom_point(aes(y=BASELINE_BIO_MALE_GE120), color ="black", size = 5) +
  geom_point(aes(y=ALT1_BIO_MALE_GE120), color = "red", size = 3) +
  geom_point(aes(y=ALT2_BIO_MALE_GE120), color = "blue", size = 3)+
  geom_point(aes(y=ALT3_BIO_MALE_GE120), color = "green", size = 3)

################################ MALE_GE120 BIOMASS CV #########################################################################
names(all_bio)

ggplot(all_bio, aes(x=SURVEY_YEAR, y=BASELINE_CV_BIO_MALE_GE120, color ="baseline", size = 5))+
  geom_point()+  
  geom_point(aes(y=ALT1_CV_BIO_MALE_GE120, color = "alternative 1"), size = 3) +
  geom_point(aes(y=ALT2_CV_BIO_MALE_GE120, color = "alternative 2"), size = 3)+
  geom_point(aes(y=ALT3_CV_BIO_MALE_GE120, color = "alternative 3"), size = 3)+
  labs(colour="Datasets",y="Mature Male Biomass CV",x="Survey Year")+ 
  theme(legend.position = c(0, 1),legend.justification = c(0, 1))+
  scale_color_manual(values = c( "black","red", "blue", "green"))


ggplot(all_bio, aes(x=SURVEY_YEAR))+
  geom_point(aes(y=BASELINE_CV_BIO_MALE_GE120), color ="black", size = 5) +
  geom_point(aes(y=ALT1_CV_BIO_MALE_GE120), color = "red", size = 3) +
  geom_point(aes(y=ALT2_CV_BIO_MALE_GE120), color = "blue", size = 3)+
  geom_point(aes(y=ALT3_CV_BIO_MALE_GE120), color = "green", size = 3)
  

################################ MALE_GE120 BIOMASS CI #########################################################################
names(all_bio)

ggplot(all_bio, aes(x=SURVEY_YEAR, y=BASELINE_CI_BIO_MALE_GE120, color ="baseline", size = 5))+
  geom_point()+  
  geom_point(aes(y=ALT1_CI_BIO_MALE_GE120, color = "alternative 1"), size = 3)+
  geom_point(aes(y=ALT2_CI_BIO_MALE_GE120, color = "alternative 2"), size = 3)+
  geom_point(aes(y=ALT3_CI_BIO_MALE_GE120, color = "alternative 3"), size = 3)+
  labs(colour="Datasets",y="Mature Male Biomass CI",x="Survey Year")+ 
  theme(legend.position = c(0, 1),legend.justification = c(0, 1))+
  scale_color_manual(values = c("black","red", "blue", "green"))


ggplot(all_bio, aes(x=SURVEY_YEAR))+
  geom_point(aes(y=BASELINE_CI_BIO_MALE_GE120), color ="black", size = 5)+
  geom_point(aes(y=ALT1_CI_BIO_MALE_GE120), color = "red", size = 3)+
  geom_point(aes(y=ALT2_CI_BIO_MALE_GE120), color = "blue", size = 3)+
  geom_point(aes(y=ALT3_CI_BIO_MALE_GE120), color = "green", size = 3)


###########################################################################################################################
#################################### calculate averages for each group/column  across all years############################


all_bio %>%
  select(where(is.numeric)) %>% 
  colMeans(na.rm = TRUE) -> all_bio_avgs

all_abun %>%
  select(where(is.numeric)) %>% 
  colMeans(na.rm = TRUE) -> all_abun_avgs

all_bio %>%
  select(BASELINE_CV_BIO_MALE_GE120, ALT1_CV_BIO_MALE_GE120, ALT2_CV_BIO_MALE_GE120, ALT3_CV_BIO_MALE_GE120) %>% 
  colMeans(na.rm = TRUE) %>%
  as.matrix()-> bio_cv_avgs

bio_cv_avgs

