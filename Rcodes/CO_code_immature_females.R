library(tidyverse)
library(ggplot2)
library(dplyr)

##### Import data
base_bio<-read_csv("./Data/nbs_co_bio_baseline_v2022.csv")
alt_1_bio<-read_csv("./Data/nbs_co_bio_alt_strata_1.csv")
alt_2_bio<-read_csv("./Data/nbs_co_bio_alt_strata_2.csv")
alt_3_bio<-read_csv("./Data/nbs_co_bio_alt_strata_3.csv")
alt_4_bio<-read_csv("./Data/nbs_co_bio_alt_strata_4.csv")
alt_5_bio<-read_csv("./Data/nbs_co_bio_alt_strata_5.csv")

base_abun<-read_csv("./Data/nbs_co_abun_baseline_v2022.csv")
alt_1_abun<-read_csv("./Data/nbs_co_abun_alt_strata_1.csv")
alt_2_abun<-read_csv("./Data/nbs_co_abun_alt_strata_2.csv")
alt_3_abun<-read_csv("./Data/nbs_co_abun_alt_strata_3.csv")
alt_4_abun<-read_csv("./Data/nbs_co_abun_alt_strata_4.csv")
alt_5_abun<-read_csv("./Data/nbs_co_abun_alt_strata_5.csv")

base_abun
#########  biomass
base_bio %>%
  select(SURVEY_YEAR, BIOMASS_FEMALE_IMMATURE, CV_BIOMASS_FEMALE_IMMATURE, CI_BIOMASS_FEMALE_IMMATURE) %>%
  rename_all(~c("SURVEY_YEAR", "BASELINE_BIO_FEMALE_IMMATURE", "BASELINE_CV_BIO_FEMALE_IMMATURE", "BASELINE_CI_BIO_FEMALE_IMMATURE")) -> base_immatfembio
  
alt_1_bio %>% 
  select(BIOMASS_FEMALE_IMMATURE, CV_BIOMASS_FEMALE_IMMATURE, CI_BIOMASS_FEMALE_IMMATURE) %>%
     rename_all(~c("ALT1_BIO_FEMALE_IMMATURE", "ALT1_CV_BIO_FEMALE_IMMATURE", "ALT1_CI_BIO_FEMALE_IMMATURE"))-> alt_1_immatfembio

alt_2_bio %>% 
  select(BIOMASS_FEMALE_IMMATURE, CV_BIOMASS_FEMALE_IMMATURE, CI_BIOMASS_FEMALE_IMMATURE) %>%
   rename_all(~c("ALT2_BIO_FEMALE_IMMATURE", "ALT2_CV_BIO_FEMALE_IMMATURE", "ALT2_CI_BIO_FEMALE_IMMATURE"))-> alt_2_immatfembio

alt_3_bio %>% 
  select(BIOMASS_FEMALE_IMMATURE, CV_BIOMASS_FEMALE_IMMATURE, CI_BIOMASS_FEMALE_IMMATURE) %>%
   rename_all(~c("ALT3_BIO_FEMALE_IMMATURE", "ALT3_CV_BIO_FEMALE_IMMATURE", "ALT3_CI_BIO_FEMALE_IMMATURE"))-> alt_3_immatfembio

alt_4_bio %>% 
  select(BIOMASS_FEMALE_IMMATURE, CV_BIOMASS_FEMALE_IMMATURE, CI_BIOMASS_FEMALE_IMMATURE) %>%
   rename_all(~c("ALT4_BIO_FEMALE_IMMATURE", "ALT4_CV_BIO_FEMALE_IMMATURE", "ALT4_CI_BIO_FEMALE_IMMATURE"))-> alt_4_immatfembio

alt_5_bio %>% 
  select(BIOMASS_FEMALE_IMMATURE, CV_BIOMASS_FEMALE_IMMATURE, CI_BIOMASS_FEMALE_IMMATURE) %>%
  rename_all(~c("ALT5_BIO_FEMALE_IMMATURE", "ALT5_CV_BIO_FEMALE_IMMATURE", "ALT5_CI_BIO_FEMALE_IMMATURE"))-> alt_5_immatfembio

all_bio<-as.data.frame(cbind(base_immatfembio, alt_1_immatfembio, alt_2_immatfembio, alt_3_immatfembio, alt_4_immatfembio, alt_5_immatfembio))
###########

base_abun %>%
  select(SURVEY_YEAR, NUM_FEMALE_IMMATURE, CV_NUM_FEMALE_IMMATURE, CI_NUM_FEMALE_IMMATURE) %>%
  rename_all(~c("SURVEY_YEAR", "BASELINE_ABUN_FEMALE_IMMATURE", "BASELINE_CV_ABUN_FEMALE_IMMATURE","BASELINE_CI_ABUN_FEMALE_IMMATURE")) -> base_immatfemabun

alt_1_abun %>%
  select(NUM_FEMALE_IMMATURE, CV_NUM_FEMALE_IMMATURE, CI_NUM_FEMALE_IMMATURE) %>%
  rename_all(~c("ALT1_ABUN_FEMALE_IMMATURE", "ALT1_CV_ABUN_FEMALE_IMMATURE","ALT1_CI_ABUN_FEMALE_IMMATURE")) -> alt_1_immatfemabun

alt_2_abun %>%
  select(NUM_FEMALE_IMMATURE, CV_NUM_FEMALE_IMMATURE, CI_NUM_FEMALE_IMMATURE) %>%
  rename_all(~c("ALT2_ABUN_FEMALE_IMMATURE", "ALT2_CV_ABUN_FEMALE_IMMATURE","ALT2_CI_ABUN_FEMALE_IMMATURE")) -> alt_2_immatfemabun

alt_3_abun %>%
  select(NUM_FEMALE_IMMATURE, CV_NUM_FEMALE_IMMATURE, CI_NUM_FEMALE_IMMATURE) %>%
  rename_all(~c("ALT3_ABUN_FEMALE_IMMATURE", "ALT3_CV_ABUN_FEMALE_IMMATURE","ALT3_CI_ABUN_FEMALE_IMMATURE")) -> alt_3_immatfemabun

alt_4_abun %>%
  select(NUM_FEMALE_IMMATURE, CV_NUM_FEMALE_IMMATURE, CI_NUM_FEMALE_IMMATURE) %>%
  rename_all(~c("ALT4_ABUN_FEMALE_IMMATURE", "ALT4_CV_ABUN_FEMALE_IMMATURE","ALT4_CI_ABUN_FEMALE_IMMATURE")) -> alt_4_immatfemabun

alt_5_abun %>%
  select(NUM_FEMALE_IMMATURE, CV_NUM_FEMALE_IMMATURE, CI_NUM_FEMALE_IMMATURE) %>%
  rename_all(~c("ALT5_ABUN_FEMALE_IMMATURE", "ALT5_CV_ABUN_FEMALE_IMMATURE","ALT5_CI_ABUN_FEMALE_IMMATURE")) -> alt_5_immatfemabun

all_abun<-as.data.frame(cbind(base_immatfemabun, alt_1_immatfemabun, alt_2_immatfemabun, alt_3_immatfemabun, alt_4_immatfemabun, alt_5_immatfemabun))

################################ LE67 BIOMASS #########################################################################
names(all_bio)

ggplot(all_bio, aes(x=SURVEY_YEAR, y=BASELINE_BIO_FEMALE_IMMATURE, color ="baseline", size = 3))+
  geom_point()+  
  geom_point(aes(y=ALT1_BIO_FEMALE_IMMATURE, color = "alternative 1"), size = 3) +
  geom_point(aes(y=ALT2_BIO_FEMALE_IMMATURE, color = "alternative 2"), size = 3)+
  geom_point(aes(y=ALT3_BIO_FEMALE_IMMATURE, color = "alternative 3"), size = 3)+
  geom_point(aes(y=ALT4_BIO_FEMALE_IMMATURE, color = "alternative 4"), size = 3)+
  geom_point(aes(y=ALT5_BIO_FEMALE_IMMATURE, color = "alternative 5"), size = 3)+
  labs(colour="Datasets",y="Immature female Biomass (MT)",x="Survey Year")+ 
  theme(legend.position = c(0, 1),legend.justification = c(0, 1))+
  scale_color_manual(values = c("orange","red", "blue", "green", "black","grey"))
  
  
  
  

ggplot(all_bio, aes(x=SURVEY_YEAR))+
  geom_point(aes(y=BASELINE_BIO_FEMALE_IMMATURE), color ="black", size = 3) +
  geom_point(aes(y=ALT1_BIO_FEMALE_IMMATURE), color = "red", size = 3) +
  geom_point(aes(y=ALT2_BIO_FEMALE_IMMATURE), color = "yellow", size = 3)+
  geom_point(aes(y=ALT3_BIO_FEMALE_IMMATURE), color = "green", size = 3)+
  geom_point(aes(y=ALT4_BIO_FEMALE_IMMATURE), color = "blue", size = 3)+
  geom_point(aes(y=ALT5_BIO_FEMALE_IMMATURE), color = "grey", size = 3)

################################ LE67 BIOMASS CV #########################################################################
names(all_bio)

ggplot(all_bio, aes(x=SURVEY_YEAR, y=BASELINE_CV_BIO_FEMALE_IMMATURE, color ="baseline", size = 3))+
  geom_point()+  
  geom_point(aes(y=ALT1_CV_BIO_FEMALE_IMMATURE, color = "alternative 1"), size = 3) +
  geom_point(aes(y=ALT2_CV_BIO_FEMALE_IMMATURE, color = "alternative 2"), size = 3)+
  geom_point(aes(y=ALT3_CV_BIO_FEMALE_IMMATURE, color = "alternative 3"), size = 3)+
  geom_point(aes(y=ALT4_CV_BIO_FEMALE_IMMATURE, color = "alternative 4"), size = 3)+
  geom_point(aes(y=ALT5_CV_BIO_FEMALE_IMMATURE, color = "alternative 5"), size = 3)+
  labs(colour="Datasets",y="Immature female Biomass CV",x="Survey Year")+ 
  theme(legend.position = c(0, 1),legend.justification = c(0, 1))+
  scale_color_manual(values = c("orange","red", "blue", "green", "black","grey"))


ggplot(all_bio, aes(x=SURVEY_YEAR))+
  geom_point(aes(y=BASELINE_CV_BIO_FEMALE_IMMATURE), color ="black", size = 3) +
  geom_point(aes(y=ALT1_CV_BIO_FEMALE_IMMATURE), color = "red", size = 3) +
  geom_point(aes(y=ALT2_CV_BIO_FEMALE_IMMATURE), color = "orange", size = 3)+
  geom_point(aes(y=ALT3_CV_BIO_FEMALE_IMMATURE), color = "green", size = 3)+
  geom_point(aes(y=ALT4_CV_BIO_FEMALE_IMMATURE), color = "blue", size = 3)+
  geom_point(aes(y=ALT5_CV_BIO_FEMALE_IMMATURE), color = "grey", size = 3)
  

################################ LE67 BIOMASS CI #########################################################################
names(all_bio)

ggplot(all_bio, aes(x=SURVEY_YEAR, y=BASELINE_CI_BIO_FEMALE_IMMATURE, color ="baseline", size = 3))+
  geom_point()+  
  geom_point(aes(y=ALT1_CI_BIO_FEMALE_IMMATURE, color = "alternative 1"), size = 3)+
  geom_point(aes(y=ALT2_CI_BIO_FEMALE_IMMATURE, color = "alternative 2"), size = 3)+
  geom_point(aes(y=ALT3_CI_BIO_FEMALE_IMMATURE, color = "alternative 3"), size = 3)+
  geom_point(aes(y=ALT4_CI_BIO_FEMALE_IMMATURE, color = "alternative 4"), size = 3)+
  geom_point(aes(y=ALT5_CI_BIO_FEMALE_IMMATURE, color = "alternative 5"), size = 3)+
  labs(colour="Datasets",y="Immature female Biomass CI",x="Survey Year")+ 
  theme(legend.position = c(0, 1),legend.justification = c(0, 1))+
  scale_color_manual(values = c("orange","red", "blue", "green", "black", "grey"))


ggplot(all_bio, aes(x=SURVEY_YEAR))+
  geom_point(aes(y=BASELINE_CI_BIO_FEMALE_IMMATURE), color ="black", size = 3)+
  geom_point(aes(y=ALT1_CI_BIO_FEMALE_IMMATURE), color = "red", size = 3)+
  geom_point(aes(y=ALT2_CI_BIO_FEMALE_IMMATURE), color = "orange", size = 3)+
  geom_point(aes(y=ALT3_CI_BIO_FEMALE_IMMATURE), color = "green", size = 3)+
  geom_point(aes(y=ALT4_CI_BIO_FEMALE_IMMATURE), color = "blue", size = 3)+
  geom_point(aes(y=ALT5_CI_BIO_FEMALE_IMMATURE), color = "grey", size = 3)

###########################################################################################################################
#################################### calculate averages for each group/column  across all years############################


all_bio %>%
  select(where(is.numeric)) %>% 
  colMeans(na.rm = TRUE) -> all_bio_avgs

all_abun %>%
  select(where(is.numeric)) %>% 
  colMeans(na.rm = TRUE) -> ll_abun_avgs

all_bio %>%
  select(BASELINE_CV_BIO_FEMALE_IMMATURE,ALT1_CV_BIO_FEMALE_IMMATURE,ALT2_CV_BIO_FEMALE_IMMATURE,ALT3_CV_BIO_FEMALE_IMMATURE,ALT4_CV_BIO_FEMALE_IMMATURE,ALT5_CV_BIO_FEMALE_IMMATURE) %>% 
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
