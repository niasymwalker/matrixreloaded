library(ggplot2)
library(tidyverse)
library(fs)
library(readxl)
library(dplyr)
library(expss)
library(vctrs)
library(tidyr)
library(stringr)
library(janitor)
library(ggpubr)
library(moments)
library(forcats)
library(survival)
library(survminer)
library(cowplot)
library(ggpubr)
library(lubridate)
library(broom)
library(utile.visuals)
library(rstatix)
library(reshape2)
library(igraph)
library(qpdf)
library(bdscale)
library(scales)
library(coda)
library(lme4)
library(flextable)
library(magick)
library(patchwork)

setwd("~/Desktop/Research/CRL Lab/Projects/Matrix Reloaded/datasets")

####Temperature and Light Data####
d4<-read_csv("controller04 SP and PV Time Series-data-2023-04-27 14_54_26.csv")%>%
  separate(PV,into=c("temp","trash"),sep=" ")%>%select(-trash)%>%
  mutate(time=ymd_hms(Time))%>%select(-Time)%>%
  mutate(temp=as.numeric(temp))%>%
  mutate(HS=temp-27.5)%>%
  mutate(HS=case_when(HS<1~0,TRUE~as.numeric(HS)))%>%
  mutate(date=date(time),hour=hour(time),minute=minute(time))%>%
  mutate(cumulative=cumsum(HS)/60/24/7)%>%
  filter(hour==19)%>%
  group_by(date)%>%
  slice_min(order_by=minute,n=1)%>%
  select(date,cumulative)%>%
  rename(c4=cumulative)

d13<-read_csv("controller13 SP and PV Time Series-data-2023-04-27 14_54_53.csv")%>%
  separate(PV,into=c("temp","trash"),sep=" ")%>%select(-trash)%>%
  mutate(time=ymd_hms(Time))%>%select(-Time)%>%
  mutate(temp=as.numeric(temp))%>%
  mutate(HS=temp-27.5)%>%
  mutate(HS=case_when(HS<0~0,TRUE~as.numeric(HS)))%>%
  mutate(date=date(time),hour=hour(time),minute=minute(time))%>%
  mutate(cumulative=cumsum(HS)/60/24/7)%>%
  filter(hour==19)%>%
  group_by(date)%>%
  slice_min(order_by=minute,n=1)%>%
  select(date,cumulative)%>%
  rename(c13=cumulative)

d14<-read_csv("controller14 SP and PV Time Series-data-2023-11-01 10_47_23.csv")%>%
  separate(PV,into=c("temp","trash"),sep=" ")%>%select(-trash)%>%
  mutate(time=ymd_hms(Time))%>%select(-Time)%>%
  mutate(temp=as.numeric(temp))%>%
  mutate(HS=temp-27.5)%>%
  mutate(HS=case_when(HS<0~0,TRUE~as.numeric(HS)))%>%
  mutate(date=date(time),hour=hour(time),minute=minute(time))%>%
  mutate(cumulative=cumsum(HS)/60/24/7)%>%
  filter(hour==19)%>%
  group_by(date)%>%
  slice_min(order_by=minute,n=1)%>%
  select(date,cumulative)%>%
  rename(c14=cumulative)

output<-full_join(d4,d13,by="date")%>%full_join(.,d14,by="date")%>%
  gather(controller,temp,-date)%>%group_by(date)%>%mutate(cumulative=mean(temp))%>%
  select(-controller,-temp)%>%distinct()%>%
  filter(date!="2023-01-21",date!="2023-01-22")%>%
  rownames_to_column(var="dummy")%>%
  mutate(dummy=as.numeric(dummy)-1)%>%
  mutate(hs_timepoint=paste0("hs",dummy))%>%
  select(date,cumulative,hs_timepoint)

#write.table(output,"./output.txt",sep="\t",quote=FALSE,row.names=FALSE)

#heat controller data for heat ramp profile
#J1: tank8, c7
#J2: tank12, c21
#J3: tank13, c20
#A1: tank6, c4
#A2: tank7 (control), c5
#A3: tank14, c14
#A4: tank15, c15
#A1 Rec: tank9, c9
#A3 Rec: tank10, c11
#A4 Rec: tank11, c24

A1_c4<-read_csv("controller04.csv")%>%
  separate(PV,into=c("temp","trash"),sep=" ")%>%select(-trash) %>%
  select(Time,temp) %>%
  mutate(temp=as.numeric(temp)) %>%
  mutate(time=mdy_hm(Time)) %>%
  select(time,temp) %>%
  add_column(group= c("Heated")) %>%
  na.omit()

A2_c5<-read_csv("controller05.csv")%>%
  separate(PV,into=c("temp","trash"),sep=" ")%>%select(-trash) %>%
  select(Time,temp) %>%
  mutate(temp=as.numeric(temp)) %>%
  mutate(time=mdy_hm(Time)) %>%
  select(time,temp) %>%
  add_column(group= c("Ambient")) %>%
  na.omit()
  
A3_c14<-read_csv("controller14.csv")%>%
  separate(PV,into=c("temp","trash"),sep=" ")%>%select(-trash) %>%
  select(Time,temp) %>%
  mutate(temp=as.numeric(temp)) %>%
  mutate(time=mdy_hm(Time)) %>%
  select(time,temp) %>%
  add_column(group= c("Heated")) %>%
  na.omit()

A4_c15<-read_csv("controller15.csv")%>%
  separate(PV,into=c("temp","trash"),sep=" ")%>%select(-trash) %>%
  select(Time,temp) %>%
  mutate(temp=as.numeric(temp)) %>%
  mutate(time=mdy_hm(Time)) %>%
  select(time,temp) %>%
  add_column(group= c("Heated")) %>%
  na.omit()
  
J1_c7<-read_csv("controller07.csv")%>%
  separate(PV,into=c("temp","trash"),sep=" ")%>%select(-trash) %>%
  select(Time,temp) %>%
  mutate(temp=as.numeric(temp)) %>%
  mutate(time=mdy_hm(Time)) %>%
  select(time,temp) %>%
  add_column(group= c("Heated")) %>%
  na.omit()

J2_c21<-read_csv("controller21.csv")%>%
  separate(PV,into=c("temp","trash"),sep=" ")%>%select(-trash) %>%
  select(Time,temp) %>%
  mutate(temp=as.numeric(temp)) %>%
  mutate(time=mdy_hm(Time)) %>%
  select(time,temp) %>%
  add_column(group= c("Heated")) %>%
  na.omit()

J3_c20<-read_csv("controller20.csv")%>%
  separate(PV,into=c("temp","trash"),sep=" ")%>%select(-trash) %>%
  select(Time,temp) %>%
  mutate(temp=as.numeric(temp)) %>%
  mutate(time=mdy_hm(Time)) %>%
  select(time,temp) %>%
  add_column(group= c("Heated")) %>%
  na.omit()

#growth period tanks 8/25-11/17
#J1: tank8, c7
#J2: tank12, c21
#J3: tank13, c20
#A1: tank6, c4
#A2: tank7 (control), c5
#A3: tank14, c14
#A4: tank15, c15
#c4 (ambient), c6 (ambient), c10 (ambient), c11 (ambient)

A1_growth_c4<-read_csv("controller04_growth.csv")%>%
  separate(PV,into=c("temp","trash"),sep=" ")%>%select(-trash) %>%
  select(Time,temp) %>%
  mutate(temp=as.numeric(temp)) %>%
  mutate(time=mdy_hm(Time)) %>%
  select(time,temp) %>%
  mutate(time=as.Date(time)) %>%
  na.omit()

A1_growth_c4_avg <- A1_growth_c4 %>%
  mutate(day = floor_date(time, "day")) %>%
  group_by(day) %>%
  mutate(day_avg = mean(temp)) %>%
  select(day,day_avg) %>%
  mutate(time=day) %>%
  mutate(temp=day_avg) %>%
  select(time,temp) %>%
  add_column(group= c("Growth Period")) %>%
  distinct()

A2_growth_c6<-read_csv("controller06_growth.csv")%>%
  separate(PV,into=c("temp","trash"),sep=" ")%>%select(-trash) %>%
  select(Time,temp) %>%
  mutate(temp=as.numeric(temp)) %>%
  mutate(time=mdy_hm(Time)) %>%
  select(time,temp) %>%
  mutate(time=as.Date(time)) %>%
  na.omit()

A2_growth_c6_avg <- A2_growth_c6 %>%
  mutate(day = floor_date(time, "day")) %>%
  group_by(day) %>%
  mutate(day_avg = mean(temp)) %>%
  select(day,day_avg) %>%
  mutate(time=day) %>%
  mutate(temp=day_avg) %>%
  select(time,temp) %>%
  add_column(group= c("Growth Period")) %>%
  distinct()

A3_growth_c10<-read_csv("controller10_growth.csv")%>%
  separate(PV,into=c("temp","trash"),sep=" ")%>%select(-trash) %>%
  select(Time,temp) %>%
  mutate(temp=as.numeric(temp)) %>%
  mutate(time=mdy_hm(Time)) %>%
  select(time,temp) %>%
  mutate(time=as.Date(time)) %>%
  na.omit()

A3_growth_c10_avg <- A3_growth_c10 %>%
  mutate(day = floor_date(time, "day")) %>%
  group_by(day) %>%
  mutate(day_avg = mean(temp)) %>%
  select(day,day_avg) %>%
  mutate(time=day) %>%
  mutate(temp=day_avg) %>%
  select(time,temp) %>%
  add_column(group= c("Growth Period")) %>%
  distinct()

J1_growth_c11<-read_csv("controller11_growth.csv")%>%
  separate(PV,into=c("temp","trash"),sep=" ")%>%select(-trash) %>%
  select(Time,temp) %>%
  mutate(temp=as.numeric(temp)) %>%
  mutate(time=mdy_hm(Time)) %>%
  select(time,temp) %>%
  mutate(time=as.Date(time)) %>%
  na.omit()

J1_growth_c11_avg <- J1_growth_c11 %>%
  mutate(day = floor_date(time, "day")) %>%
  group_by(day) %>%
  mutate(day_avg = mean(temp)) %>%
  select(day,day_avg) %>%
  mutate(time=day) %>%
  mutate(temp=day_avg) %>%
  select(time,temp) %>%
  add_column(group= c("Growth Period")) %>%
  distinct()

#recovery period tanks 2/5-4/26
#A1 Rec: tank9, c9
#A2 Control: tank7, c5
#A3 Rec: tank10, c11
#A4 Rec: tank11, c24

#Actually used c8, c21, c7

A1_rec_c8<-read_csv("controller08_rec.csv")%>%
  separate(PV,into=c("temp","trash"),sep=" ")%>%select(-trash) %>%
  select(Time,temp) %>%
  mutate(temp=as.numeric(temp)) %>%
  mutate(time=mdy_hm(Time)) %>%
  select(time,temp) %>%
  mutate(time=as.Date(time)) %>%
  na.omit()

A1_rec_c8_avg <- A1_rec_c8 %>%
  mutate(day = floor_date(time, "day")) %>%
  group_by(day) %>%
  mutate(day_avg = mean(temp)) %>%
  select(day,day_avg) %>%
  mutate(time=day) %>%
  mutate(temp=day_avg) %>%
  select(time,temp) %>%
  add_column(group= c("Recovery Period")) %>%
  distinct()

A2_rec_c21<-read_csv("controller21_rec.csv")%>%
  separate(PV,into=c("temp","trash"),sep=" ")%>%select(-trash) %>%
  select(Time,temp) %>%
  mutate(temp=as.numeric(temp)) %>%
  mutate(time=mdy_hm(Time)) %>%
  select(time,temp) %>%
  mutate(time=as.Date(time)) %>%
  na.omit()

A2_rec_c21_avg <- A2_rec_c21 %>%
  mutate(day = floor_date(time, "day")) %>%
  group_by(day) %>%
  mutate(day_avg = mean(temp)) %>%
  select(day,day_avg) %>%
  mutate(time=day) %>%
  mutate(temp=day_avg) %>%
  select(time,temp) %>%
  add_column(group= c("Recovery Period")) %>%
  distinct()

#Light Logger Data
A1_growth_light<-read_csv("12070_120_growth.csv")%>%
  mutate(datetime=dmy_hms(paste(date,time))) %>%
  select(datetime,calibrated) %>%
  mutate(calibrated=as.numeric(calibrated)) %>%
  mutate(datetime=as.POSIXct(datetime)) %>%
  mutate(day = floor_date(datetime, "day")) %>%
  add_column(group= c("Growth Period")) %>%
  add_column(coral= c("Adults")) %>%
  na.omit()

mean(A1_growth_light$calibrated[A1_growth_light$calibrated > 0])
max(A1_growth_light$calibrated)

A2_growth_light<-read_csv("11848_118_growth.csv")%>%
  mutate(datetime=dmy_hms(paste(date,time))) %>%
  select(datetime,calibrated) %>%
  mutate(calibrated=as.numeric(calibrated)) %>%
  mutate(datetime=as.POSIXct(datetime)) %>%
  mutate(day = floor_date(datetime, "day")) %>%
  add_column(group= c("Growth Period")) %>%
  add_column(coral= c("Adults")) %>%
  na.omit()

mean(A2_growth_light$calibrated[A2_growth_light$calibrated > 0])
max(A2_growth_light$calibrated)

J1_growth_light<-read_csv("11849_118_growth.csv")%>%
  mutate(datetime=dmy_hms(paste(date,time))) %>%
  select(datetime,calibrated) %>%
  mutate(calibrated=as.numeric(calibrated)) %>%
  mutate(datetime=as.POSIXct(datetime)) %>%
  mutate(day = floor_date(datetime, "day")) %>%
  add_column(group= c("Growth Period")) %>%
  add_column(coral= c("Juveniles")) %>%
  na.omit()

mean(J1_growth_light$calibrated[J1_growth_light$calibrated > 0])
max(J1_growth_light$calibrated)

J2_growth_light<-read_csv("6377_063_growth.csv")%>%
  mutate(datetime=dmy_hms(paste(date,time))) %>%
  select(datetime,calibrated) %>%
  mutate(calibrated=as.numeric(calibrated)) %>%
  mutate(datetime=as.POSIXct(datetime)) %>%
  mutate(day = floor_date(datetime, "day")) %>%
  add_column(group= c("Growth Period")) %>%
  add_column(coral= c("Juveniles")) %>%
  na.omit()

mean(J2_growth_light$calibrated[J2_growth_light$calibrated > 0])
max(J2_growth_light$calibrated)

J1_niarox_light<-read_csv("12070_120_NIAROX.csv")%>%
  mutate(datetime=dmy_hms(paste(date,time))) %>%
  select(datetime,calibrated) %>%
  mutate(calibrated=as.numeric(calibrated)) %>%
  mutate(datetime=as.POSIXct(datetime)) %>%
  mutate(day = floor_date(datetime, "day")) %>%
  add_column(group= c("Heat Stress Period")) %>%
  add_column(coral= c("Juveniles")) %>%
  na.omit()

mean(J1_niarox_light$calibrated[J1_niarox_light$calibrated > 0])
max(J1_niarox_light$calibrated)

A1_niarox_light<-read_csv("11848_118_NIAROX.csv")%>%
  mutate(datetime=dmy_hms(paste(date,time))) %>%
  select(datetime,calibrated) %>%
  mutate(calibrated=as.numeric(calibrated)) %>%
  mutate(datetime=as.POSIXct(datetime)) %>%
  mutate(day = floor_date(datetime, "day")) %>%
  add_column(group= c("Heat Stress Period")) %>%
  add_column(coral= c("Adults")) %>%
  na.omit()

mean(A1_niarox_light$calibrated[A1_niarox_light$calibrated > 0])
max(A1_niarox_light$calibrated)

####list of adult genets, ramets, and treatment####
corals_list <- read_excel("matrixcorals_list.xlsx") %>% clean_names()
#list of juvenile info: plug id, gantry position, parent_cross, sperm and egg donor parents
juveniles_list <- read_excel("matrixjuveniles_list.xlsx") %>% clean_names() %>%
  mutate(plug_id=as.factor(plug_id)) %>%
  mutate(gantry_position=as.factor(gantry_position)) %>%
  mutate(sperm_parent=as.factor(sperm_parent)) %>%
  mutate(egg_parent=as.factor(egg_parent)) %>%
  arrange(cross_date) %>%
  slice(1:170)

####Adult Growth Datasets####  
#empty plug stem, still includes disk
empty <- read_excel("empty plug data.xlsx") %>% clean_names() %>% select(plug_bw) %>%
  mutate(plug_bw=as.numeric(plug_bw))
x <- mean(empty$plug_bw)

#buoyant weight initial timepoint
bw_t0 <- read_excel("08_25_2022 NSW.xlsx") %>% clean_names() %>% separate(buoyant_weight, into=c("buoyant_weight1","trash1"), " ") %>%
  separate(bw_2, into=c("buoyant_weight2","trash2"), " ") %>%
  separate(bw_3, into=c("buoyant_weight3","trash3"), " ") %>%
  select(coral_id,contains("buoy")) %>% gather(rep, bw, buoyant_weight1:buoyant_weight3) %>%
  mutate(bw=as.numeric(bw)) %>%
  group_by(coral_id) %>% summarise(avg_bw=mean(bw)-x) 

#buoyant weight initial and final timepoints, plus percent change bw
bw_data <- read_excel("bw_NSW_11.16.22.xlsx") %>% clean_names() %>% separate(bw1, into=c("buoyant_weight1","trash1"), " ") %>%
  separate(bw2, into=c("buoyant_weight2","trash2"), " ") %>%
  separate(bw3, into=c("buoyant_weight3","trash3"), " ") %>%
  select(coral_id,contains("buoy")) %>% gather(rep, bw, buoyant_weight1:buoyant_weight3) %>%
  mutate(bw=as.numeric(bw)) %>%
  group_by(coral_id) %>% summarise(avg_bw=mean(bw)-x) %>%
  left_join(.,bw_t0,by="coral_id") %>% rename(bw_post=2,bw_pre=3) %>%
  mutate(percentchange_bw = ((bw_post-bw_pre)/bw_pre)*100) %>%
  left_join(.,corals_list,by="coral_id") %>%
  select(treatment,genet,coral_id,percentchange_bw,bw_post,bw_pre) %>%
  mutate(genet=as.factor(genet)) %>%
  drop_na()

#pre surface area, volume data from metashape, normalized to plug SA and volume
metashape_pre <- read_csv("Matrix_Reloaded_T0_data_frags.csv") %>% clean_names() %>%
  mutate(sa_pre = surface_area_cm_2 - 3.11025526687023) %>%
  mutate(volume_pre = volume_cm_3 - 0.0170400140725387) %>%
  mutate(coral_id=model) %>%
  select(coral_id,sa_pre,volume_pre)

#pre and post surface area, volume data from metashape, normalized to plug SA and volume, joined to bw data
growth_data <- read_csv("Matrix_Reloaded_T6_data_frags.csv") %>% clean_names() %>%
  mutate(sa_post = surface_area_cm_2 - 3.11025526687023) %>%
  mutate(volume_post = volume_cm_3 - 0.0170400140725387) %>%
  mutate(coral_id=model) %>%
  select(coral_id,sa_post,volume_post) %>%
  left_join(.,metashape_pre,by="coral_id") %>%
  mutate(percentchange_sa = ((sa_post-sa_pre)/sa_pre)*100) %>%
  mutate(percentchange_volume = ((volume_post-volume_pre)/volume_pre)*100) %>%
  left_join(.,bw_data,by="coral_id") %>%
  group_by(coral_id) %>%
  mutate(density_pre = bw_pre/volume_pre) %>%
  mutate(density_post = bw_post/volume_post) %>%
  mutate(percentchange_density = ((density_post-density_pre)/density_pre)*100) %>%
  mutate(changevolume = volume_post-volume_pre) %>%
  mutate(changebw = bw_post-bw_pre) %>%
  mutate(changedensity = changebw/changevolume) %>%
  mutate(densityextension = changedensity/density_pre) %>%
  mutate(adult_growthrate=(sa_post-sa_pre)/83) %>%
  select(treatment,genet,coral_id,adult_growthrate,bw_pre,bw_post,percentchange_bw,sa_pre,sa_post,percentchange_sa,volume_pre,volume_post,percentchange_volume,density_pre,density_post,percentchange_density,changevolume,changebw,changedensity,densityextension) %>%
  drop_na()
#write.csv(growth_data,"adult_growthdata.csv")

min(growth_data$percentchange_sa)
max(growth_data$percentchange_sa)
sa_x <- length(growth_data$percentchange_sa[growth_data$percentchange_sa>0])
sa_x/239
min(growth_data$percentchange_bw)
max(growth_data$percentchange_bw)
bw_x <- length(growth_data$percentchange_bw[growth_data$percentchange_bw>0])
bw_x/239

#average percent change for bw, volume, and sa of adults
growth_data_avg <- growth_data %>%
  group_by(genet) %>%
  mutate(percentchange_bw=mean(percentchange_bw)) %>%
  mutate(percentchange_sa=mean(percentchange_sa)) %>%
  mutate(percentchange_volume=mean(percentchange_volume)) %>%
  mutate(adult_growthrate=mean(adult_growthrate)) %>%
  ungroup() %>%
  select(genet,adult_growthrate,percentchange_bw,percentchange_sa,percentchange_volume) %>%
  distinct()
write.csv(growth_data_avg,"./matrix_genet_growthdata.csv")

min(growth_data_avg$percentchange_sa)
max(growth_data_avg$percentchange_sa)
sa_avg_x <- length(growth_data_avg$percentchange_sa[growth_data_avg$percentchange_sa>0])
sa_avg_x/60
min(growth_data_avg$percentchange_bw)
max(growth_data_avg$percentchange_bw)
bw_avg_x <- length(growth_data_avg$percentchange_bw[growth_data_avg$percentchange_bw>0])
bw_avg_x/60

####Adult Heat Resistance and recovery experiment datasets####
#raw resistance and recovery data, no long format
resistrecover_data <- read_excel("resist_recover_data.xlsx", na="NA") %>% clean_names() %>%
  distinct() %>%
  mutate(percentdecline_pam = (1-(last_pam/pre_pam))*100) %>%
  mutate(percentdecline_pam = na_if(percentdecline_pam, percentdecline_pam[percentdecline_pam <30])) %>%
  mutate(mortpercentdecline_pam = (1-(last_pam/pre_pam))*100) %>%
  mutate(mortpercentdecline_pam = na_if(mortpercentdecline_pam, mortpercentdecline_pam[percentdecline_pam >29.99])) %>%
  mutate(genet=as.character(genet)) %>%
  distinct()

#just adult morality during heat stress experiment
resist_mortality <- read_excel("resist_recover_data.xlsx", na="NA") %>% clean_names() %>%
  gather(hs_day,mort,mort_hs0:mort_hs14) %>%
  distinct() %>%
  separate(hs_day,into = c("trash","hs_timepoint"), sep = "_") %>%
  select(treatment,genet,coral_id,hs_timepoint,mort) %>%
  distinct()

##long format heat recovery mort experiment data
resistrecover_data_long <- read_excel("resist_recover_data.xlsx", na="NA") %>% clean_names() %>%
  gather(rec_day,rec_mort,mort_r0:mort_r80) %>%
  distinct() %>%
  separate(rec_day,into = c("trash","rec_timepoint"), sep = "_") %>%
  mutate(percentdecline_pam = (1-(last_pam/pre_pam))*100) %>%
  mutate(percentdecline_pam = na_if(percentdecline_pam, percentdecline_pam[percentdecline_pam <30])) %>%
  mutate(mortpercentdecline_pam = (1-(last_pam/pre_pam))*100) %>%
  mutate(mortpercentdecline_pam = na_if(mortpercentdecline_pam, mortpercentdecline_pam[percentdecline_pam >29.99])) %>%
  select(treatment,genet,coral_id,pre_pam,last_pam,days_to_bleach,days_to_mortality,percentdecline_pam,mortpercentdecline_pam,rec_timepoint,rec_mort,rec_pam) %>%
  distinct()

recover_data_long <- read_excel("resist_recover_data.xlsx", na="NA") %>% clean_names() %>%
  gather(rec_day,rec_mort,mort_r0:mort_r80) %>%
  distinct() %>%
  separate(rec_day,into = c("trash","rec_timepoint"), sep = "_") %>%
  select(treatment,genet,coral_id,pre_pam,last_pam,rec_pam,rec_mort,rec_timepoint) %>%
  distinct()

####Adult PAM datasets, includes ED30####
#raw pam data, also pam decline percentages
pam_data <- read_excel("resist_recover_data.xlsx", na="NA") %>% clean_names() %>%
  gather(hs_day,hs_pam,pam_hs0:pam_hs14) %>%
  separate(hs_day,into = c("trash","hs_timepoint"), sep = "_") %>%
  mutate(percentdecline_pam = (1-(last_pam/pre_pam))*100) %>%
  mutate(percentdecline_pam = na_if(percentdecline_pam, percentdecline_pam[percentdecline_pam <30])) %>%
  mutate(mortpercentdecline_pam = (1-(last_pam/pre_pam))*100) %>%
  mutate(mortpercentdecline_pam = na_if(mortpercentdecline_pam, mortpercentdecline_pam[percentdecline_pam >29.99])) %>%
  mutate(percentdecline_rec = (1-(rec_pam/pre_pam))*100) %>%
  select(treatment, genet, coral_id, pre_pam, last_pam, rec_pam, hs_timepoint, hs_pam, percentdecline_pam, mortpercentdecline_pam, percentdecline_rec, days_to_mortality) %>%
  distinct()

#ED30 model values calculated using PAM data and heat logger data! ADULTS#
#DHW <- read_tsv("./output.txt")
resistrecover_pam <- pam_data %>%
  filter(is.na(days_to_mortality)) %>%
  select(hs_timepoint,hs_pam,coral_id,genet,treatment)%>%
  left_join(.,output)%>%
  filter(treatment=="Heated")%>%
  mutate(initial_pam=case_when(hs_timepoint=="hs0"~hs_pam))%>%
  arrange(coral_id)%>%
  group_by(coral_id)%>%
  fill(initial_pam,.direction=c("down"))%>%
  mutate(relative_pam=hs_pam/initial_pam)%>%
  drop_na()%>%
  mutate(genet=as.factor(genet))%>%
  filter(genet!="765")

list <- resistrecover_pam%>%
  ungroup()%>%
  select(genet)%>%
  distinct()
out<-data.frame(genet=character(),cumulative=numeric(),predicted=numeric(),pmin=numeric(),pmax=numeric())
ED<-data.frame(genet=character(),ed30=numeric(),se=numeric(),lwr=numeric(),upper=numeric())

for(i in list$genet){
  temp<-resistrecover_pam%>%filter(genet==paste0(i))
  model <- drc::drm(relative_pam~cumulative, data=temp, fct=drc::W1.3(fixed=c(NA,1,NA),names=c("Slope","Upper Limit","ED30")))
  summary(model)  
  summary <- drc::ED(model,c(30), interval="delta")
  ED<-ED%>%add_row(genet=i,ed30=summary[1],se=summary[2],lwr=summary[3],upper=summary[4])
  newdata <- expand.grid(cumulative=seq(min(resistrecover_pam$cumulative),max(8),length=100))
  pm <- predict(model,newdata=newdata,interval="confidence")
  newdata<-bind_cols(newdata,pm)
  out<-out%>%add_row(genet=i,cumulative=newdata$cumulative,predicted=newdata$Prediction,pmin=newdata$Lower,pmax=newdata$Upper)
  
}
#write.csv(ED,"NIAROX_ED30.csv")

min(ED$ed30)
max(ED$ed30)
mean(ED$ed30)

#JUST CONTROLS
resistrecover_pam_control <- pam_data %>%
  filter(is.na(days_to_mortality)) %>%
  select(hs_timepoint,hs_pam,coral_id,genet,treatment)%>%
  left_join(.,output)%>%
  filter(treatment=="Control")%>%
  mutate(initial_pam=case_when(hs_timepoint=="hs0"~hs_pam))%>%
  arrange(coral_id)%>%
  group_by(coral_id)%>%
  fill(initial_pam,.direction=c("down"))%>%
  mutate(relative_pam=hs_pam/initial_pam)%>%
  drop_na()%>%
  mutate(genet=as.factor(genet))%>%
  filter(genet!="765") %>%
  mutate(hs_timepoint = str_remove(hs_timepoint, "hs")) %>%
  mutate(hs_timepoint=as.numeric(hs_timepoint)) %>%
  mutate(relative_pam=as.numeric(relative_pam))
  
resistrecover_pam_control$relative_pam[resistrecover_pam_control$relative_pam > 1] <- 1
resistrecover_pam_control$relative_pam = as.numeric(resistrecover_pam_control$relative_pam)

#Adult Integral AUC data from ED30 curves
out_adult<-data.frame(genet=character(),integral=numeric())
for(i in list$genet){
  temp_adult<-out%>%filter(genet==paste0(i))
  auc_adult<-MESS::auc(temp_adult$cumulative,temp_adult$predicted,from=0,to=7.1)
  out_adult<-out_adult%>%add_row(genet=i,integral=auc_adult)
}

####ITS2 Type Profile Datasets####
#ITS2 Type Profiles, sum type profile reads and remove missing data
profiles <- read_excel("ITS2_typeprofile.xlsx") %>%
  mutate(genet=as.factor(genet)) %>%
  mutate(coral_id=as.factor(coral_id)) %>%
  mutate(sum = rowSums(.[4:31],na.rm=TRUE)) %>%
  gather(type_profile,profile_present,4:31,factor_key=TRUE) %>%
  mutate(type_profile_prop = profile_present/sum) %>%
  mutate(type_profile=as.factor(type_profile)) %>%
  mutate_all(na_if,"0") %>%
  arrange(genet) %>%
  drop_na() %>%
  add_column(CvsD = NA) %>%
  mutate(CvsD = if_else(grepl("C31",type_profile),"Cladocopium", CvsD)) %>%
  mutate(CvsD = if_else(grepl("D1",type_profile),"Durusdinium", CvsD)) %>%
  filter(!genet=="651")

profiles_sum_colony <- read_excel("ITS2_typeprofile.xlsx") %>%
  mutate(genet=as.factor(genet)) %>%
  mutate(coral_id=as.factor(coral_id)) %>%
  mutate(sum = rowSums(.[4:31],na.rm=TRUE)) %>%
  gather(type_profile,profile_present,4:31,factor_key=TRUE) %>%
  mutate(type_profile_prop = profile_present/sum) %>%
  mutate(type_profile=as.factor(type_profile)) %>%
  mutate_all(na_if,"0") %>%
  arrange(genet) %>%
  drop_na() %>%
  group_by(genet) %>%
  mutate(total_profile=sum(type_profile_prop)) %>%
  group_by(genet,type_profile) %>%
  mutate(total_type_profile=sum(type_profile_prop)) %>%
  mutate(colony_profile_prop=total_type_profile/total_profile) %>%
  dplyr::select(genet,type_profile,colony_profile_prop) %>%
  distinct() %>%
  filter(!genet=="651")

#Above profiles plus Cladocopium and Durusdinium specific column calculations
remove_D <- c("Durusdinium")
remove_C <- c("Cladocopium")
remove_value <- c("NA")

profiles_clades <- profiles %>%
  distinct(coral_id,sum, .keep_all = TRUE) %>%
  group_by(genet) %>%
  summarise(sum_colony = sum(sum)) %>%
  left_join(.,profiles,"genet") %>%
  group_by(genet,CvsD) %>%
  mutate(clade_sum = sum(profile_present)) %>%
  mutate(Cladocopium = clade_sum/sum_colony) %>%
  mutate(Cladocopium = if_else(CvsD %in% remove_D, NA_real_,Cladocopium)) %>%
  mutate(Durusdinium_working = 1-(clade_sum/sum_colony)) %>%
  mutate(Durusdinium_working = if_else(CvsD %in% remove_C, NA_real_,Durusdinium_working)) %>%
  unite(Cladocopium_prop, Cladocopium, Durusdinium_working, sep = "") %>%
  mutate(Cladocopium_prop = str_remove(Cladocopium_prop, remove_value)) %>%
  mutate(clade=case_when(Cladocopium_prop>=0.8~"C",
                         Cladocopium_prop<=0.2~"D",
                         TRUE~"mixed"))%>%
  mutate(Cladocopium_prop=as.numeric(Cladocopium_prop)) %>%
  mutate(Cladocopium_prop = round(Cladocopium_prop, 5)) %>%
  group_by(genet) %>%
  select(genet,Cladocopium_prop,clade) %>%
  distinct()
#write.csv(profiles_clades,"symbiont_summary.csv")

#Plot by ITS2 type profiles by genet
profiles2 <- profiles %>%
  mutate(genet = as.factor(genet)) %>%
  left_join(.,profiles_clades,by="genet") %>%
  mutate(genet = fct_reorder(genet, Cladocopium_prop, .fun = mean)) %>%
  mutate(Durusdinium_prop = (1-Cladocopium_prop)) %>%
  select(genet,type_profile,type_profile_prop,CvsD,clade,Cladocopium_prop,Durusdinium_prop)

write.csv(profiles2,"./matrix_symbiontpops.csv")

#Plot by Broad Sym Categories
profiles3 <- profiles2 %>%
  gather(prop_clade,props,Cladocopium_prop:Durusdinium_prop) %>%
  select(genet,prop_clade,props) %>%
  left_join(.,profiles2,by="genet",relationship="many-to-many") %>%
  select(genet,prop_clade,props,Cladocopium_prop) %>%
  mutate(genet = fct_reorder(genet, Cladocopium_prop, .fun = mean)) %>%
  distinct()

####juvenile datasets####
#general juvenile dataset
juveniles_list_data <- juveniles_list %>%
  mutate(genet = sperm_parent) %>%
  mutate(genet=as.factor(genet)) %>%
  left_join(.,ED,by="genet",relationship="many-to-many") %>%
  select(cross_date,plug_id,parent_cross,parent_cross,genet,egg_parent,gantry_position,ed30) %>%
  mutate(sperm_parent = genet) %>%
  mutate(sperm_resistance = ed30) %>%
  select(cross_date,plug_id,parent_cross,parent_cross,sperm_parent,egg_parent,gantry_position,sperm_resistance) %>%
  mutate(genet = egg_parent) %>%
  mutate(genet=as.factor(genet)) %>%
  left_join(.,ED,by="genet",relationship="many-to-many") %>%
  select(cross_date,plug_id,parent_cross,parent_cross,sperm_parent,genet,gantry_position,sperm_resistance,ed30) %>%
  mutate(egg_parent = genet) %>%
  mutate(egg_resistance = ed30) %>%
  select(cross_date,plug_id,gantry_position,parent_cross,parent_cross,sperm_parent,egg_parent,sperm_resistance,egg_resistance) %>%
  distinct() %>%
  mutate(genet = sperm_parent) %>%
  mutate(genet=as.factor(genet)) %>%
  left_join(.,growth_data_avg,by="genet",relationship="many-to-many") %>%
  select(cross_date,plug_id,gantry_position,parent_cross,parent_cross,genet,egg_parent,sperm_resistance,egg_resistance,percentchange_bw,percentchange_sa,percentchange_volume) %>%
  mutate(sperm_parent = genet) %>%
  mutate(sperm_percentchange_bw = percentchange_bw) %>%
  mutate(sperm_percentchange_sa = percentchange_sa) %>%
  mutate(sperm_percentchange_volume = percentchange_volume) %>%
  select(cross_date,plug_id,gantry_position,parent_cross,parent_cross,sperm_parent,egg_parent,sperm_resistance,egg_resistance,sperm_percentchange_bw,sperm_percentchange_sa,sperm_percentchange_volume) %>%
  mutate(genet = egg_parent) %>%
  mutate(genet = as.factor(genet)) %>%
  left_join(.,growth_data_avg,by="genet",relationship="many-to-many") %>%
  select(cross_date,plug_id,gantry_position,parent_cross,parent_cross,sperm_parent,genet,sperm_resistance,egg_resistance,sperm_percentchange_bw,sperm_percentchange_sa,sperm_percentchange_volume,percentchange_bw,percentchange_sa,percentchange_volume) %>%
  mutate(egg_parent = genet) %>%
  mutate(egg_percentchange_bw = percentchange_bw) %>%
  mutate(egg_percentchange_sa = percentchange_sa) %>%
  mutate(egg_percentchange_volume = percentchange_volume) %>%
  select(cross_date,plug_id,gantry_position,parent_cross,parent_cross,sperm_parent,egg_parent,sperm_resistance,egg_resistance,sperm_percentchange_bw,sperm_percentchange_sa,sperm_percentchange_volume,egg_percentchange_bw,egg_percentchange_sa,egg_percentchange_volume) %>%
  distinct() 
#rename instead of mutate
#by="genet=sperm_donor"

juvenile_mortality <- read_excel("juvenile_mortality_final.xlsx", na="NA") %>% clean_names() %>%
  left_join(.,juveniles_list,by="gantry_position") %>%
  gather(hs_day,mort,x1_19:x2_5) %>%
  mutate(mort = if_else(grepl("A",mort),"0", mort)) %>%
  mutate(mort = if_else(grepl("D",mort),"1", mort)) %>%
  select(parent_cross,sperm_parent,egg_parent,hs_day,mort) %>%
  mutate(mort=as.numeric(mort)) %>%
  distinct() %>%
  add_column(time_days = NA) %>%
  mutate(time_days = if_else(grepl("x1_19",hs_day),"0", time_days)) %>%
  mutate(time_days = if_else(grepl("x1_23",hs_day),"1", time_days)) %>%
  mutate(time_days = if_else(grepl("x1_27",hs_day),"5", time_days)) %>%
  mutate(time_days = if_else(grepl("x1_28",hs_day),"6", time_days)) %>%
  mutate(time_days = if_else(grepl("x1_29",hs_day),"7", time_days)) %>%
  mutate(time_days = if_else(grepl("x2_1",hs_day),"10", time_days)) %>%
  mutate(time_days = if_else(grepl("x2_2",hs_day),"11", time_days)) %>%
  mutate(time_days = if_else(grepl("x2_3",hs_day),"12", time_days)) %>%
  mutate(time_days = if_else(grepl("x2_4",hs_day),"13", time_days)) %>%
  mutate(time_days = if_else(grepl("x2_5",hs_day),"14", time_days)) %>%
  mutate(time_days=as.numeric(time_days)) %>%
  distinct()

juvenile_pam_data <- read_excel("juvenile iPAM dataset_final.xlsx", na="NA") %>% clean_names() %>%
  mutate(plug_id=as.factor(plug_id)) %>%
  mutate(gantry_position=as.factor(gantry_position)) %>%
  left_join(.,juveniles_list,by="plug_id") %>%
  mutate(gantry_position=gantry_position.x) %>%
  select(cross_date,plug_id, gantry_position, juvenile_coral_id, parent_cross, parent_cross, sperm_parent, egg_parent, pre_pam, final_pam) %>%
  mutate(percentdecline_pam = ((1-(final_pam/pre_pam))*100)) %>%
  group_by(parent_cross) %>%
  mutate(avg_percentdecline_pam=mean(percentdecline_pam,na.rm=TRUE)) %>%
  group_by(plug_id) %>%
  mutate(plug_percentdecline_pam=mean(percentdecline_pam,na.rm=TRUE)) %>%
  distinct()

juvenile_pam_long <- read_excel("juvenile iPAM dataset_final.xlsx", na="NA") %>% clean_names() %>%
  gather(hs_day,hs_pam,pam_hs0:pam_hs14) %>%
  separate(hs_day,into = c("trash","hs_timepoint"), sep = "_") %>%
  mutate(plug_id=as.factor(plug_id)) %>%
  mutate(gantry_position=as.factor(gantry_position)) %>%
  left_join(.,juveniles_list,by="plug_id") %>%
  mutate(gantry_position=gantry_position.x) %>%
  select(cross_date,plug_id, gantry_position, juvenile_coral_id, parent_cross, parent_cross, sperm_parent, egg_parent, pre_pam, hs_timepoint, hs_pam) %>%
  distinct()

#ED10 model values calculated using PAM data and heat logger data! JUVENILES#
juvenile_pam <- juvenile_pam_long %>%
  filter(!is.na(pre_pam)) %>%
  select(hs_timepoint,hs_pam,pre_pam,parent_cross)%>%
  left_join(.,output)%>%
  #filter(cross_date=="2022-05-31")%>%
  mutate(relative_pam=hs_pam/pre_pam)%>%
  drop_na()%>%
  mutate(parent_cross=as.factor(parent_cross)) %>%
  mutate(cumulative=as.numeric(cumulative)) %>%
  mutate(relative_pam=as.numeric(relative_pam))%>%
  #filter(relative_pam<1.1)%>%
  mutate(relative_pam=case_when(relative_pam>1~1,TRUE~as.numeric(relative_pam))) %>%
  filter(parent_cross!="E10")

#write.csv(juvenile_pam,"juvenilePAM.csv")

list_j <- juvenile_pam%>%
  ungroup()%>%
  select(parent_cross)%>%
  distinct()

out_j<-data.frame(parent_cross=character(),cumulative=numeric(),predicted=numeric(),pmin=numeric(),pmax=numeric())
ED_j<-data.frame(parent_cross=character(),ed10=numeric(),se=numeric(),lwr=numeric(),upper=numeric())
newdata_j<-as.data.frame(seq(min(juvenile_pam$cumulative),max(juvenile_pam$cumulative),0.1))%>%rename(cumulative=1)

for(i in list_j$parent_cross){
  temp_j<-juvenile_pam%>%filter(parent_cross==paste0(i))
  model_j <- drc::drm(relative_pam~cumulative, na.action=na.omit, data=temp_j, fct=drc::W1.3(fixed=c(NA,1,NA),names=c("Slope","Upper Limit","ED10")))
  summary(model_j)  
  summary_j <- drc::ED(model_j,c(10), interval="delta")
  ED_j<-ED_j%>%add_row(parent_cross=i,ed10=summary_j[1],se=summary_j[2],lwr=summary_j[3],upper=summary_j[4])
  pm_j <- as.data.frame(predict(model_j,newdata=newdata_j,interval="prediction"))
  out_j<-out_j%>%add_row(parent_cross=i,cumulative=newdata_j$cumulative,predicted=pm_j$Prediction,pmin=pm_j$Lower,pmax=pm_j$Upper)
  
}
#write.csv(ED_j,"NIAROX_ED10_juvenile.csv")

min(ED_j$ed10)
max(ED_j$ed10)
mean(ED_j$ed10)

juved10 <- out_j %>%
  ungroup() %>%
  select(parent_cross) %>%
  distinct()

out_juved10<-data.frame(parent_cross=character(),integral=numeric())
for(i in list_j$parent_cross){
  temp_juved10<-out_j%>%filter(parent_cross==paste0(i))
  auc_juved10<-MESS::auc(temp_juved10$cumulative,temp_juved10$predicted,from=0,to=7.1)
  out_juved10<-out_juved10%>%add_row(parent_cross=i,integral=auc_juved10)
}
#write.csv(out_juved10,"ED10_integral_juvenile.csv")

out_juved10 <- out_juved10 %>%
  mutate(integral=as.numeric(integral)) %>%
  drop_na()

min(out_juved10$integral)
max(out_juved10$integral)
mean(out_juved10$integral)

juveniles_integral_data <- juveniles_list %>%
  mutate(genet = sperm_parent) %>%
  mutate(genet=as.factor(genet)) %>%
  left_join(.,out_adult,by="genet") %>%
  select(cross_date,plug_id,parent_cross,parent_cross,genet,egg_parent,gantry_position,integral) %>%
  mutate(sperm_parent = genet) %>%
  mutate(sperm_resistance = integral) %>%
  select(cross_date,plug_id,parent_cross,parent_cross,sperm_parent,egg_parent,gantry_position,sperm_resistance) %>%
  mutate(genet = egg_parent) %>%
  mutate(genet=as.factor(genet)) %>%
  left_join(.,out_adult,by="genet") %>%
  select(cross_date,plug_id,parent_cross,parent_cross,sperm_parent,genet,gantry_position,sperm_resistance,integral) %>%
  mutate(egg_parent = genet) %>%
  mutate(egg_resistance = integral) %>%
  select(cross_date,plug_id,gantry_position,parent_cross,parent_cross,sperm_parent,egg_parent,sperm_resistance,egg_resistance) %>%
  distinct() %>%
  mutate(genet = sperm_parent) %>%
  mutate(genet=as.factor(genet)) %>%
  left_join(.,growth_data_avg,by="genet",relationship="many-to-many") %>%
  select(cross_date,plug_id,gantry_position,parent_cross,parent_cross,genet,egg_parent,sperm_resistance,egg_resistance,percentchange_bw,percentchange_sa,percentchange_volume) %>%
  mutate(sperm_parent = genet) %>%
  mutate(sperm_percentchange_bw = percentchange_bw) %>%
  mutate(sperm_percentchange_sa = percentchange_sa) %>%
  mutate(sperm_percentchange_volume = percentchange_volume) %>%
  select(cross_date,plug_id,gantry_position,parent_cross,parent_cross,sperm_parent,egg_parent,sperm_resistance,egg_resistance,sperm_percentchange_bw,sperm_percentchange_sa,sperm_percentchange_volume) %>%
  mutate(genet = egg_parent) %>%
  mutate(genet = as.factor(genet)) %>%
  left_join(.,growth_data_avg,by="genet",relationship="many-to-many") %>%
  select(cross_date,plug_id,gantry_position,parent_cross,parent_cross,sperm_parent,genet,sperm_resistance,egg_resistance,sperm_percentchange_bw,sperm_percentchange_sa,sperm_percentchange_volume,percentchange_bw,percentchange_sa,percentchange_volume) %>%
  mutate(egg_parent = genet) %>%
  mutate(egg_percentchange_bw = percentchange_bw) %>%
  mutate(egg_percentchange_sa = percentchange_sa) %>%
  mutate(egg_percentchange_volume = percentchange_volume) %>%
  select(cross_date,plug_id,gantry_position,parent_cross,parent_cross,sperm_parent,egg_parent,sperm_resistance,egg_resistance,sperm_percentchange_bw,sperm_percentchange_sa,sperm_percentchange_volume,egg_percentchange_bw,egg_percentchange_sa,egg_percentchange_volume) %>%
  distinct() 

#removed samples that fused
#removed samples that were not ind or agg the entire time
juvenile_growth_data <- read_excel("juvenile growth metadata_final.xlsx", na="NA") %>% clean_names() %>%
  arrange(cross_date) %>%
  slice(1:1132) %>%
  filter(!str_detect(id_notes_t0, "F")) %>%
  filter(!str_detect(id_notes_tf, "F")) %>%
  filter(ind_agg_same=="Y") %>%
  mutate(plug_id = as.factor(plug_id)) %>%
  group_by(plug_id) %>%
  mutate(surface_area_t0=as.numeric(surface_area_t0)) %>%
  mutate(surface_area_tf=as.numeric(surface_area_tf)) %>%
  mutate(growth_percentage_sa=as.numeric(growth_percentage_sa)) %>%
  mutate(plug_sa_t0 = mean(surface_area_t0, na.rm=TRUE)) %>%
  mutate(plug_sa_tf = mean(surface_area_tf, na.rm=TRUE)) %>%
  mutate(plug_growthpercentage = mean(growth_percentage_sa,na.rm=TRUE)) %>%
  mutate(parent_cross=as.factor(parent_cross)) %>%
  group_by(parent_cross) %>%
  mutate(pc_sa_t0 = mean(surface_area_t0,na.rm=TRUE)) %>%
  mutate(pc_sa_tf = mean(surface_area_tf,na.rm=TRUE)) %>%
  mutate(pc_growthpercentage = mean(growth_percentage_sa,na.rm=TRUE)) %>%
  select(parent_cross,plug_id,sperm_parent,egg_parent,number_polyps_t0,number_polyps_tf,surface_area_t0,surface_area_tf,ind_agg_t0,ind_agg_tf,ind_agg_same,plug_sa_t0,plug_sa_tf,plug_growthpercentage,pc_sa_t0,pc_sa_tf,pc_growthpercentage,growth_percentage_sa)

min(juvenile_growth$growth_percentage_sa)
max(juvenile_growth$growth_percentage_sa)
mean(juvenile_growth$growth_percentage_sa)

#raw juvenile data combined with growth data and adult resistance data
juvenile_raw_data <- juvenile_growth_data %>%
  select(parent_cross,pc_sa_t0,pc_sa_tf,growth_percentage_sa,ind_agg_tf,surface_area_t0,surface_area_tf) %>%
  mutate(surface_area_tf_cm = surface_area_tf/100) %>%
  mutate(surface_area_t0_cm = surface_area_t0/100) %>%
  mutate(juv_growthrate=(surface_area_tf_cm-surface_area_t0_cm)/83) %>%
  left_join(.,juvenile_pam_data,by="parent_cross",relationship="many-to-many") %>%
  select(parent_cross,growth_percentage_sa,juv_growthrate,sperm_parent,egg_parent,percentdecline_pam,ind_agg_tf) %>%
  left_join(.,juveniles_list_data,by="parent_cross",relationship="many-to-many") %>%
  select(parent_cross,growth_percentage_sa,juv_growthrate,sperm_parent.x,sperm_resistance,sperm_percentchange_bw,sperm_percentchange_sa,sperm_percentchange_volume,egg_parent.x,egg_resistance,egg_percentchange_bw,egg_percentchange_sa,egg_percentchange_volume,percentdecline_pam,ind_agg_tf) %>%
  mutate(sperm_parent=sperm_parent.x) %>%
  mutate(egg_parent=egg_parent.x) %>%
  group_by(parent_cross) %>%
  distinct() %>%
  drop_na() %>%
  select(-c(sperm_parent.x,egg_parent.x))

juvenile_raw_data$egg_parent <- factor(juvenile_raw_data$egg_parent, levels=c("657","661","672","760","762","764","913","915","918"))
juvenile_raw_data$sperm_parent <- factor(juvenile_raw_data$sperm_parent, levels=c("657","661","672","760","762","764","769","913","915","918"))

juvenile_raw_data <- juvenile_raw_data %>%
  filter(between(growth_percentage_sa,-100,400))

#parent cross averages
juvenile_pc_data <- juvenile_growth_data %>%
  group_by(parent_cross) %>%
  select(parent_cross,pc_sa_t0,pc_sa_tf,pc_growthpercentage,growth_percentage_sa) %>%
  distinct() %>%
  drop_na() %>%
  left_join(.,juvenile_pam_data,by="parent_cross",relationship="many-to-many") %>%
  select(parent_cross,pc_growthpercentage,sperm_parent,egg_parent,avg_percentdecline_pam,growth_percentage_sa,plug_percentdecline_pam) %>%
  group_by(parent_cross) %>%
  distinct() %>%
  drop_na() %>%
  left_join(.,juveniles_list_data,by="parent_cross",relationship="many-to-many") %>%
  select(parent_cross,pc_growthpercentage,sperm_parent.x,sperm_resistance,sperm_percentchange_bw,sperm_percentchange_sa,sperm_percentchange_volume,egg_parent.x,egg_resistance,egg_percentchange_bw,egg_percentchange_sa,egg_percentchange_volume,avg_percentdecline_pam,growth_percentage_sa,plug_percentdecline_pam) %>%
  mutate(sperm_parent=sperm_parent.x) %>%
  mutate(egg_parent=egg_parent.x) %>%
  group_by(parent_cross) %>%
  distinct() %>%
  drop_na() %>%
  select(-c(sperm_parent.x,egg_parent.x)) %>%
  mutate(sperm_parent=as.factor(sperm_parent)) %>%
  mutate(egg_parent=as.factor(egg_parent))

juvenile_pc$egg_parent <- factor(juvenile_pc$egg_parent, levels=c("657","661","672","760","762","764","913","915","918"))
juvenile_pc$sperm_parent <- factor(juvenile_pc$sperm_parent, levels=c("657","661","672","760","762","764","769","913","915","918"))

juvenile_pc <- juvenile_pc %>%
  filter(between(pc_growthpercentage,-100,400))

#juvenile mortality dataset wide#
juvenile_mortality_wide <- read_excel("juvenile_mortality_final.xlsx") %>% clean_names() %>%
  left_join(.,juveniles_list,by="gantry_position")

sum(stringr::str_count(juvenile_mortality$x2_5, pattern = "D"))
#194
sum(stringr::str_count(juvenile_mortality$x2_5, pattern = "A"))
#829
#194+829=1023
(194/1023)*100
#18.96 <- 19.0%

#Mini dataset to make a histogram of adult and juvenile integral data, also adult ED30 and juvenile ED10 values
#Histogram of adult and juvenile integral derived E10 values
id <- data.frame(group=c(out_adult$genet,out_juved10$parent_cross))
integral <- data.frame(integral=c(out_adult$integral,out_juved10$integral))
ED_data <- data.frame(ED=c(ED$ed30,ED_j$ed10))
group <- c("Adults","Adults","Adults","Adults","Adults","Adults","Adults","Adults","Adults","Adults","Adults","Adults","Adults","Adults","Adults","Adults","Adults","Adults","Adults","Adults","Adults","Adults","Adults","Adults","Adults",
           "Adults","Adults","Adults","Adults","Adults","Adults","Adults","Adults","Adults","Adults","Adults","Adults","Adults","Adults","Adults","Adults","Adults","Adults","Adults","Adults","Adults","Adults","Adults","Adults","Adults",
           "Adults","Adults","Adults","Adults","Adults","Juveniles","Juveniles","Juveniles","Juveniles","Juveniles","Juveniles","Juveniles","Juveniles","Juveniles","Juveniles","Juveniles","Juveniles","Juveniles","Juveniles","Juveniles",
           "Juveniles","Juveniles","Juveniles","Juveniles","Juveniles","Juveniles","Juveniles","Juveniles","Juveniles","Juveniles","Juveniles","Juveniles","Juveniles","Juveniles","Juveniles","Juveniles","Juveniles","Juveniles","Juveniles",
           "Juveniles","Juveniles","Juveniles","Juveniles","Juveniles","Juveniles","Juveniles","Juveniles","Juveniles","Juveniles","Juveniles","Juveniles","Juveniles","Juveniles","Juveniles","Juveniles","Juveniles","Juveniles","Juveniles",
           "Juveniles","Juveniles","Juveniles","Juveniles","Juveniles","Juveniles","Juveniles")
adult_juvenile_integrals <- data.frame(group,id,integral,ED_data)
  
juv_v_adultED <- aov(lm(integral ~ group, data = adult_juvenile_integrals))
#juv_v_adultED <- aov(lm(ED ~ group, data = adult_juvenile_integrals))
summary(juv_v_adultED)

#juvenile heat resistance by parent, using integral Heat Resistance, for heatmaps
juvenile_hs_heatmap <- out_juved10 %>%
  left_join(.,juveniles_list,by="parent_cross") %>%
  select(sperm_parent,egg_parent,parent_cross,integral) %>%
  distinct()
juvenile_hs_heatmap$egg_parent <- factor(juvenile_hs_heatmap$egg_parent, levels=c("657","661","672","760","762","764","913","915","918"))
juvenile_hs_heatmap$sperm_parent <- factor(juvenile_hs_heatmap$sperm_parent, levels=c("657","661","672","760","762","764","769","913","915","918"))

#juvenile vs. adult heat resistance for linear regressions
juvenile_hs_regressions <- juvenile_hs_heatmap %>%
  mutate(juv_integral = integral) %>%
  mutate(genet = sperm_parent) %>%
  select(genet,egg_parent,parent_cross,juv_integral) %>%
  left_join(.,out_adult,by="genet") %>%
  mutate(sperm_parent = genet) %>%
  mutate(sperm_integral = integral) %>%
  select(sperm_parent,egg_parent,parent_cross,sperm_integral,juv_integral) %>%
  mutate(genet = egg_parent) %>%
  left_join(.,out_adult,by="genet") %>%
  mutate(egg_parent=genet) %>%
  mutate(egg_integral=integral) %>%
  select(sperm_parent,egg_parent,parent_cross,sperm_integral,egg_integral,juv_integral) %>%
  distinct()

####Extra Adult and Juvenile Datasets Contingent on Above Datasets####
#adult mortality during heat stress experiment#
hs_survdata <- resist_mortality %>%
  mutate(genet=as.factor(genet)) %>%
  add_column(time_days = NA) %>%
  left_join(.,profiles_clades,by="genet") %>%
  left_join(.,ED,by="genet") %>%
  select(treatment,genet,coral_id,time_days, hs_timepoint,mort,clade,ed30) %>%
  mutate(time_days = if_else(grepl("hs0",hs_timepoint),"0", time_days)) %>%
  mutate(time_days = if_else(grepl("hs1",hs_timepoint),"1", time_days)) %>%
  mutate(time_days = if_else(grepl("hs2",hs_timepoint),"2", time_days)) %>%
  mutate(time_days = if_else(grepl("hs3",hs_timepoint),"3", time_days)) %>%
  mutate(time_days = if_else(grepl("hs4",hs_timepoint),"4", time_days)) %>%
  mutate(time_days = if_else(grepl("hs5",hs_timepoint),"5", time_days)) %>%
  mutate(time_days = if_else(grepl("hs6",hs_timepoint),"6", time_days)) %>%
  mutate(time_days = if_else(grepl("hs7",hs_timepoint),"7", time_days)) %>%
  mutate(time_days = if_else(grepl("hs8",hs_timepoint),"8", time_days)) %>%
  mutate(time_days = if_else(grepl("hs9",hs_timepoint),"9", time_days)) %>%
  mutate(time_days = if_else(grepl("hs10",hs_timepoint),"10", time_days)) %>%
  mutate(time_days = if_else(grepl("hs11",hs_timepoint),"11", time_days)) %>%
  mutate(time_days = if_else(grepl("hs12",hs_timepoint),"12", time_days)) %>%
  mutate(time_days = if_else(grepl("hs13",hs_timepoint),"13", time_days)) %>%
  mutate(time_days = if_else(grepl("hs14",hs_timepoint),"14", time_days)) %>%
  mutate(time_days=as.numeric(time_days)) %>%
  distinct()

#Adult ED30 and growth data
resistrecover_ED <- ED %>%
  left_join(.,growth_data,by="genet") %>%
  select(treatment,genet,coral_id,ed30,bw_pre,bw_post,percentchange_bw,sa_pre,sa_post,percentchange_sa,volume_pre,volume_post,percentchange_volume,density_pre,density_post,percentchange_density,changevolume,changebw,changedensity,densityextension) %>%
  distinct()

#Symbionts and Adult ED30, and Growth
symbiont_metashape <- profiles %>%
  left_join(.,resistrecover_ED,by="genet",relationship="many-to-many") %>%
  mutate(coral_id=coral_id.x) %>%
  select(treatment,genet,percentchange_bw,percentchange_sa,percentchange_volume,percentchange_density,ed30,type_profile,type_profile_prop,CvsD)

symbiont_clade_metashape <- profiles_clades %>%
  left_join(.,resistrecover_ED,by="genet",relationship="many-to-many") %>%
  select(treatment,genet,percentchange_bw,percentchange_sa,percentchange_volume,percentchange_density,ed30,Cladocopium_prop,clade)

symbiont_growth <- symbiont_clade_metashape %>%
  select(genet,percentchange_sa,percentchange_bw,percentchange_volume,percentchange_density,clade) %>%
  distinct() %>%
  na.omit()

symbiont_growth$clade <- factor(symbiont_growth$clade, levels=c("C", "mixed", "D"))

#Set ITS2 profile colors
my_colors_profile <- c("C31-C17d-C31.1-C21-C31a-C31f-C21ac-C31l"="#fcf992",
                       "C31-C17d-C21-C31.1-C31a-C17e-C31f"="#dbd865",
                       "C31-C17d-C31.1-C21-C31a-C17e-C21ac-C31.10"="#b8b546",
                       "C31-C31.1-C17d-C31a-C21-C31f-C31b-C31l"="#5e5c08",
                       "C31-C17d-C21-C31a-C21ac-C31.9"="#8ef6fa",
                       "C31-C17d-C21-C31a-C31.9"="#5ed2d6",
                       "C31-C17d-C21-C31.9-C31.5-C17i-C31.10-C21ac-C17"="#41bbbf",
                       "C31/C17d-C21-C31.9-C21ac-C31i-C31h"="#36b1b5",
                       "C31/C17d-C21-C21ac-C31a-C17e-C31.9-C17f"="#27a4a8",
                       "C31/C17d-C21-C21ac-C31k-C31.9"="#0a9dab",
                       "C31-C17d-C21-C21ac-C31k-C31a"="#0b7580",
                       "C31/C17d-C21-C21ac-C31j"="#0b4d54",
                       "C31/C17d"="#adadad",
                       "C31/C17d-C21-C31a-C21ac-C17i-C17"="#8a8a8a",
                       "C31/C17d-C21-C21ac-C17i-C17-C17e-C31j"="#5c5c5c",
                       "C31-C21-C17d-C21ac-C17i-C17"="#363636",
                       "C31-C17d-C21-C21ac-C17i-C31a"="#121212",
                       "D1/D4/D6-D1ab-D3h"="#e9b8fc",
                       "D1/D4/D1ab-D6-D4d"="#de98fa",
                       "D1/D4-D6-D1ab-D17d-D17j"="#cd7eed",
                       "D1/D4-D6-D17d-D1r-D17e-D17c"="#b45ed6",
                       "D1-D1ab-D4-D6-D1ca"="#ae41d9",
                       "D1/D6-D4-D1r"="#9814cc",
                       "D1/D4/D6/D1ab"="#793594",
                       "D1-D4-D6-D1ab-D17d"="#6c218a",
                       "D1/D4-D6-D1d"="#631085",
                       "D1/D6/D4"="#4f1d63",
                       "D1-D4-D1ab-D6-D10"="#450d5c")

#Restrict type profiles in legend to what are actually used
subset_profile <- c("C31-C17d-C31.1-C21-C31a-C31f-C21ac-C31l"=="#fcf992",
                    "C31-C17d-C21-C31.1-C31a-C17e-C31f"="#dbd865",
                    "C31-C17d-C31.1-C21-C31a-C17e-C21ac-C31.10"="#b8b546",
                    "C31-C31.1-C17d-C31a-C21-C31f-C31b-C31l"="#5e5c08",
                    "C31-C17d-C21-C31a-C21ac-C31.9"="#8ef6fa",
                    "C31-C17d-C21-C31a-C31.9"="#5ed2d6",
                    "C31-C17d-C21-C31.9-C31.5-C17i-C31.10-C21ac-C17"="#41bbbf",
                    "C31/C17d-C21-C31.9-C21ac-C31i-C31h"="#36b1b5",
                    "C31/C17d-C21-C21ac-C31a-C17e-C31.9-C17f"="#27a4a8",
                    "C31/C17d-C21-C21ac-C31k-C31.9"="#0a9dab",
                    "C31-C17d-C21-C21ac-C31k-C31a"="#0b7580",
                    "C31/C17d-C21-C21ac-C31j"="#0b4d54",
                    "C31/C17d"="#adadad",
                    "C31/C17d-C21-C31a-C21ac-C17i-C17"="#8a8a8a",
                    "C31/C17d-C21-C21ac-C17i-C17-C17e-C31j"="#5c5c5c",
                    "C31-C17d-C21-C21ac-C17i-C31a"="#121212",
                    "D1/D4/D6-D1ab-D3h"="#e9b8fc",
                    "D1/D4/D1ab-D6-D4d"="#de98fa",
                    "D1/D4-D6-D1ab-D17d-D17j"="#cd7eed",
                    "D1/D4-D6-D17d-D1r-D17e-D17c"="#b45ed6",
                    "D1/D6-D4-D1r"="#9814cc",
                    "D1/D4/D6/D1ab"="#793594",
                    "D1-D4-D6-D1ab-D17d"="#6c218a")

#Order ITS2 type profiles
profiles$type_profile = factor(profiles$type_profile,levels=c("C31-C17d-C31.1-C21-C31a-C31f-C21ac-C31l",
                                                              "C31-C17d-C21-C31.1-C31a-C17e-C31f",
                                                              "C31-C17d-C31.1-C21-C31a-C17e-C21ac-C31.10",
                                                              "C31-C31.1-C17d-C31a-C21-C31f-C31b-C31l",
                                                              "C31-C17d-C21-C31a-C21ac-C31.9",
                                                              "C31-C17d-C21-C31a-C31.9",
                                                              "C31-C17d-C21-C31.9-C31.5-C17i-C31.10-C21ac-C17",
                                                              "C31/C17d-C21-C31.9-C21ac-C31i-C31h",
                                                              "C31/C17d-C21-C21ac-C31a-C17e-C31.9-C17f",
                                                              "C31/C17d-C21-C21ac-C31k-C31.9",
                                                              "C31-C17d-C21-C21ac-C31k-C31a",
                                                              "C31/C17d-C21-C21ac-C31j",
                                                              "C31/C17d",
                                                              "C31/C17d-C21-C31a-C21ac-C17i-C17",
                                                              "C31/C17d-C21-C21ac-C17i-C17-C17e-C31j",
                                                              "C31-C21-C17d-C21ac-C17i-C17",
                                                              "C31-C17d-C21-C21ac-C17i-C31a",
                                                              "D1/D4/D6-D1ab-D3h",
                                                              "D1/D4/D1ab-D6-D4d",
                                                              "D1/D4-D6-D1ab-D17d-D17j",
                                                              "D1/D4-D6-D17d-D1r-D17e-D17c",
                                                              "D1-D1ab-D4-D6-D1ca",
                                                              "D1/D6-D4-D1r",
                                                              "D1/D4/D6/D1ab",
                                                              "D1-D4-D6-D1ab-D17d",
                                                              "D1/D4-D6-D1d",
                                                              "D1/D6/D4",
                                                              "D1-D4-D1ab-D6-D10"))

#juvenile growth by parent cross with individual and aggregate separate columns
juvenile_growth_parent <- juvenile_growth_ind %>%
  select(sperm_parent,egg_parent,parent_cross,plug_id,growth_percentage_sa) %>%
  rename(growth_percentage_ind=growth_percentage_sa) %>%
  left_join(.,juvenile_growth_agg,by="plug_id",relationship="many-to-many") %>%
  rename(growth_percentage_agg=growth_percentage_sa) %>%
  select(sperm_parent.x,egg_parent.x,parent_cross.x,plug_id,growth_percentage_ind,growth_percentage_agg) %>%
  rename(sperm_parent=sperm_parent.x) %>%
  rename(egg_parent=egg_parent.x) %>%
  rename(parent_cross=parent_cross.x) %>%
  gather(type,type_sa,growth_percentage_ind:growth_percentage_agg) %>%
  mutate(egg_parent=as.factor(egg_parent)) %>%
  mutate(sperm_parent=as.factor(sperm_parent)) %>%
  mutate(parent_cross=as.factor(parent_cross)) 

####FIGURE 1 ANALYSIS####
#Figure 1: heat resistance temperature ramp and bleaching days
M1A_group <- data.frame(M1A_group=c(A1_c4$group,A2_c5$group,A3_c14$group,A4_c15$group,J1_c7$group,J2_c21$group,J3_c20$group))
M1A_time <- data.frame(M1A_time=c(A1_c4$time,A2_c5$time,A3_c14$time,A4_c15$time,J1_c7$time,J2_c21$time,J3_c20$time))
M1A_temp <- data.frame(M1A_temp=c(A1_c4$temp,A2_c5$temp,A3_c14$temp,A4_c15$temp,J1_c7$temp,J2_c21$temp,J3_c20$temp))
M1A_dataset <- data.frame(M1A_group,M1A_time,M1A_temp)
M1A_dataset$M1A_group <- factor(M1A_dataset$M1A_group, levels = c("Heated", "Ambient"))

M1A <- ggplot() +
  geom_line(data=J3_c20, aes(x=time,y=temp), color=c("brown1"),alpha=0.6) +
  geom_line(data=A1_c4, aes(x=time,y=temp,color=group), alpha=0.6) +
  geom_line(data=A2_c5, aes(x=time,y=temp,color=group)) +
  geom_line(data=A3_c14, aes(x=time,y=temp), color=c("firebrick1"), alpha=0.6) +
  geom_line(data=A4_c15, aes(x=time,y=temp), color=c("firebrick2"), alpha=0.6) +
  geom_line(data=J1_c7, aes(x=time,y=temp), color=c("firebrick3"), alpha=0.6) +
  geom_line(data=J2_c21, aes(x=time,y=temp), color=c("firebrick4"),alpha=0.6) +
  scale_color_manual(values=c("Heated" = "firebrick", "Ambient" = "dodgerblue"), name="") +
  scale_y_continuous(breaks = c(23,24,26,28,30,32),
                     labels = c(23,24,26,28,30,32)) +
  scale_x_datetime(date_labels = "%b-%d", date_breaks = "2 days") +
  annotate("text", x=as.POSIXct("2023-1-23 00:00:00"),y=c(32.5), label=c(~Delta),hjust=1.3,size=4, parse=T) +
  annotate("text", x=as.POSIXct("2023-1-27 00:00:00"),y=c(32.5), label=c(~Delta),hjust=1.3,size=4, parse=T) +
  annotate("text", x=as.POSIXct("2023-1-28 00:00:00"),y=c(32.5), label=c(~Delta),hjust=1.3,size=4, parse=T) +
  annotate("text", x=as.POSIXct("2023-2-1 00:00:00"),y=c(32.5), label=c(~Delta),hjust=1.3,size=4, parse=T) +
  annotate("text", x=as.POSIXct("2023-2-2 00:00:00"),y=c(32.5), label=c(~Delta),hjust=1.3,size=4, parse=T) +
  annotate("text", x=as.POSIXct("2023-2-3 00:00:00"),y=c(32.5), label=c(~Delta),hjust=1.3,size=4, parse=T) +
  annotate("text", x=as.POSIXct("2023-2-5 00:00:00"),y=c(32.5), label=c(~Delta),hjust=1.3,size=4, parse=T) +
  annotate("text", x=as.POSIXct("2023-1-27 00:00:00"),y=c(22), label=c(~Delta*"= Juvenile iPAM collection dates"),parse=T,size=3) +
  labs(x="Heat Tolerance Profile Dates", y=expression("Temperature "(degree*C)~" ")) +
  theme_bw() + theme(axis.title = element_text(size=10), legend.title = element_blank(),
                     legend.background = element_blank(), legend.position = c(0.5,0.9),
                     legend.justification = c("left", "top"),
                     legend.box.just = "left",
                     legend.margin = margin(6, 6, 6, 6))

resistrecover_data2 <- resistrecover_data %>%
  mutate(genet=as.factor(genet)) %>%
  mutate(genet = fct_reorder(genet,days_to_bleach,.fun ="mean",.na_rm = TRUE)) %>%
  filter(genet!="666") %>%
  filter(genet!="669") %>%
  filter(genet!="671") %>%
  filter(genet!="775") %>%
  select(genet,days_to_bleach) %>%
  add_column(group="1+ Ramets Alive")

resistrecover_data3 <- resistrecover_data %>%
  mutate(genet=as.factor(genet)) %>%
  filter(genet=="666" | genet=="669" | genet=="671" | genet=="775") %>%
  select(genet,days_to_mortality) %>%
  add_column(group="All Ramets Dead")

days <- data.frame(days=c(resistrecover_data2$days_to_bleach,resistrecover_data3$days_to_mortality))
genet <- data.frame(genet=c(resistrecover_data2$genet,resistrecover_data3$genet))
groups <- data.frame(groups=c(resistrecover_data2$group,resistrecover_data3$group))
M1B_dataset <- data.frame(genet,days,groups)
M1B_dataset <- M1B_dataset %>%
  na.omit() %>%
  mutate(genet=as.factor(genet)) %>%
  mutate(days=as.numeric(days)) %>%
  mutate(genet = fct_reorder(genet,days,.fun ="mean",.na_rm = TRUE)) %>%
  left_join(.,profiles_clades,by="genet")
M1B_dataset$clade <- factor(M1B_dataset$clade, levels = c("C","D","mixed"))
  
M1B <- ggplot(M1B_dataset, aes(x=genet,y=days,fill=clade,shape=groups)) +
  geom_line() +
  scale_fill_manual(labels=c(expression(italic("Cladocopium"),italic("Durusdinium"),"Mixed",)), values = c("darkorange","blue","black")) +
  guides(fill=guide_legend(override.aes=list(color=c("darkorange","blue","black")),order=2)) +
  stat_summary(geom="point",fun="mean",size=2) +
  scale_shape_manual(values = c(21,24)) +
  labs(x = "Genet", y = expression("Days")) + 
  scale_y_continuous(breaks=seq(4,16,2)) +
  annotate("text", x=c(30),y=c(3.5), label=c("average days to removal after ≥30% Fv/Fm decline or mortality"),size=3) +
  theme_bw() + theme(axis.text.x=element_blank(),axis.ticks.x=element_blank(),axis.title = element_text(size=10),axis.text = element_text(),
                     legend.title = element_blank(),
                     legend.key.size = unit(2,"mm"),
                     legend.box="horizontal",
                     legend.background = element_blank(), legend.position = c(0.01,1.05),
                     legend.justification = c("left", "top"),
                     legend.box.just = "left",
                     legend.margin = margin(1, 1, 1, 1),
                     legend.text = element_text(hjust = 0))

quartz(w=4,h=4)
M1A
quartz.save("../figures/final_precon/Fig1A.pdf", type="pdf")
M1B
quartz.save("../figures/final_precon/Fig1B.pdf", type="pdf")

quartz(w=4,h=3.5)
plot_grid(M1A,M1B, rel_heights=c(1.1,1), nrow=2, labels=c("A","B"), axis="tb", align="h",label_size=12)
quartz.save("../figures/final/Fig1_final.pdf", type="pdf")

####FIGURE 2 ANALYSIS####
#Adult Heat Resistance performance and symbiont data
#Fig 2a: number of days to bleach
#Adult E30 figures, first is all samples facet wrap, second is Fig 2B everything thrown into 1 fig#
#ADD CONTROLS ED30 LINE
#Plot by Broad Sym Categories
M2A <- ggplot(profiles3, aes(x=reorder(genet,-Cladocopium_prop), y=props))+
  geom_col(aes(fill = prop_clade), colour="grey", linewidth=0.005)+
  labs(x="Genet", y="Proportion\nSymbiodiniaceae", subtitle = expression(~Delta*"= Mixed corals"))+
  theme_bw(base_size = 12)+
  scale_fill_manual(labels=c(expression(italic("Cladocopium  "),italic("Durusdinium  "))), values=c("darkorange","blue3"))+
  scale_y_continuous(labels=c("1" = "0", "1.25" = "0.25", "1.5" = "0.50", "1.75" = "0.75", "2" = "1.00")) +
  guides(color = guide_legend(override.aes = list(size=1.5)), fill=guide_legend(ncol=1, title.theme = element_blank()))+
  annotate("text", x = 9.5, y = 0.96, vjust = 0, hjust = 0, size = 3, label=c(~Delta),parse=T) +
  annotate("text", x = 10.5, y = 0.96, vjust = 0, hjust = 0, size = 3, label=c(~Delta),parse=T) +
  annotate("text", x = 11.5, y = 0.96, vjust = 0, hjust = 0, size = 3, label=c(~Delta),parse=T) +
  annotate("text", x = 12.5, y = 0.96, vjust = 0, hjust = 0, size = 3, label=c(~Delta),parse=T) +
  annotate("text", x = 13.5, y = 0.96, vjust = 0, hjust = 0, size = 3, label=c(~Delta),parse=T) +
  annotate("text", x = 14.5, y = 0.96, vjust = 0, hjust = 0, size = 3, label=c(~Delta),parse=T) +
  theme(axis.title = element_text(size=10),axis.text.x = element_blank(), axis.ticks.x=element_blank(),
        plot.subtitle = element_text(size=8),
        panel.grid = element_blank(), legend.position = "top",
        legend.key.size = unit(2,"mm"),
        legend.title=element_blank(),
        legend.margin = margin(0,0,0,0),
        legend.box.margin=margin(-10,-10,-10,-10))+
  guides(fill=guide_legend(nrow=1))

M2B <- ggplot() +
  geom_line(data=out,aes(x=cumulative,y=predicted,group=genet))+
  geom_smooth(data=out,aes(x=cumulative,y=predicted),color="firebrick", se=FALSE, linewidth=2)+
  geom_smooth(data=resistrecover_pam_control,aes(x=cumulative,y=relative_pam),color="dodgerblue", se=FALSE, linewidth=2)+
  geom_hline(yintercept=.7, linetype="dotted") +
  scale_x_continuous(sec.axis=sec_axis(~.*1.8181818182,name="Heat Tolerance Assay Days",breaks=seq(0,14,2))) +
  labs(x = "Degree Heating Weeks", y = expression(" \nRelative Fv/Fm")) + 
  theme_bw() + theme(legend.position="none", axis.title = element_text(size=10),axis.text = element_text(),axis.title.x.bottom = element_text(colour = "firebrick"),axis.title.x.top = element_text(colour = "dodgerblue"))

#Plot ED, metashape data based on cladocopium proportion#
##clade vs. ED
symbiont_ED <- symbiont_clade_metashape %>%
  select(genet,ed30,Cladocopium_prop,clade) %>%
  distinct() %>%
  na.omit()

symbiont_ED$clade <- factor(symbiont_ED$clade, levels=c("C", "mixed", "D"))

#write.csv(symbiont_ED,"symbiont_ED.csv")

ed30_clade_model <- aov(lm(ed30 ~ clade, data = symbiont_ED))
TukeyHSD(ed30_clade_model)

#all pairwise comparisons are significant, except for D vs. mixed KRUSKAL WALLIS
#C vs. D 0.0076
#C vs. mixed 0.0511
#D vs. mixed 0.0871 

#pval_CD2 <- c("**")
#pval_CM2 <- c("*")
#pval_MD2 <- c("")
pval_CD2 <- c("0.028")
pval_CM2 <- c("0.613")
pval_MD2 <- c("0.521")

M2C <- ggplot(symbiont_ED,aes(x=clade,y=ed30,color=clade)) +
  geom_boxplot() +
  geom_point() +
  labs(x = "Dominant Symbiont Type", y = expression("Tolerance (ED30)")) +
  annotate("text", x=1.5, y=5.5, label=pval_CM2, size=3) +
  annotate("text", x=2, y=6.3, label=pval_CD2, size=3) +
  annotate("text", x=2.5, y=7, label=pval_MD2, size=3) +
  annotate("segment", x = 1.1, xend = 1.9, y= 5.2, yend=5.2) +
  annotate("segment", x = 1.1, xend = 2.9, y= 6.0, yend=6.0) +
  annotate("segment", x = 2.1, xend = 2.9, y= 6.7, yend=6.7) +
  scale_x_discrete(labels=c('C', 'Mixed', 'D')) +
  scale_color_manual(values = c("darkorange", "black", "blue")) +
  theme_bw() + theme(axis.title = element_text(size=10),legend.position="none",axis.title.x=element_blank()) 

##Fisher's exact test for mortality Heat Resistance
sym_analysis <- matrix(c(0,28,0,18,25,111),ncol=2,nrow=3,dimnames=list(c("C spp.", "Mixed", "D spp."),c("Dead","Alive")),byrow=TRUE)
fisher.test(sym_analysis)
pairwise_fisher_test(as.matrix(sym_analysis))
tmp <- melt(sym_analysis)
names(tmp) <- c("Symbio", "Mortality", "Ramets")
tmp <- tmp %>%
  mutate(Mortality = as.factor(Mortality)) %>%
  mutate(Symbio = as.factor(Symbio))

#pval_CD <- c("*")
#pval_CM <- c("")
#pval_MD <- c("")
pval_CD <- c("0.0086")
pval_CM <- c("1.00")
pval_MD <- c("0.045")


M2D <- ggplot(tmp, aes(x=Symbio, y=Ramets, fill=Mortality)) +
  geom_bar(stat="identity", position="stack", color="black") +
  scale_fill_manual(values=c("orange4","seagreen4")) +
  annotate("text", x=1.5, y=50, label=pval_CM, size=3) +
  annotate("text", x=1.8, y=80, label=pval_CD, size=3) +
  annotate("text", x=2.2, y=110, label=pval_MD, size=3) +
  annotate("segment", x = 1.1, xend = 1.9, y= 40, yend=40) +
  annotate("segment", x = 1.1, xend = 2.5, y= 70, yend=70) +
  annotate("segment", x = 1.9, xend = 2.5, y= 100, yend=100) +
  scale_x_discrete(labels=c('C', 'Mixed', 'D')) +
  labs(x = "", y = "Ramet #") + 
  theme_bw() + theme(axis.title = element_text(size=10),
                     legend.title = element_blank(),legend.key.size = unit(2,"mm"),
                     legend.background = element_blank(), legend.position = c(.01, .99),
                     legend.justification = c("left", "top"),
                     legend.box.just = "left",
                     legend.margin = margin(6, 6, 6, 6))

quartz(w=7.2,h=4)
#M2A / (M2B + M2C + M2D + plot_layout(widths=c(1.1,1,1))) + plot_annotation(tag_levels="A")
plots<-cowplot::align_plots(M2A,M2B,M2C,M2D,align="v")
plot_grid(plots[[1]],NULL,plot_grid(plots[[2]],plots[[3]],plots[[4]],nrow=1,align="h",labels=c("B","C","D"),label_size=12),nrow=3,labels=c("A"),label_size=12,rel_heights=c(1,0,1.5))
quartz.save("../figures/final/Fig2_final.pdf", type="pdf")

####FIGURE 3 ANALYSIS####
#Recovery Figures
survivor_data <- resistrecover_data_long %>%
  select(treatment,genet,coral_id,rec_timepoint,rec_mort) %>%
  mutate(genet=as.factor(genet)) %>%
  left_join(.,profiles_clades,by="genet") %>%
  left_join(.,ED,by="genet",relationship="many-to-many") %>%
  select(treatment,genet,coral_id,rec_timepoint,rec_mort,clade,ed30) %>%
  add_column(time_days = NA) %>%
  mutate(time_days = if_else(grepl("r0",rec_timepoint),"0", time_days)) %>%
  mutate(time_days = if_else(grepl("r1",rec_timepoint),"1", time_days)) %>%
  mutate(time_days = if_else(grepl("r2",rec_timepoint),"2", time_days)) %>%
  mutate(time_days = if_else(grepl("r3",rec_timepoint),"3", time_days)) %>%
  mutate(time_days = if_else(grepl("r4",rec_timepoint),"4", time_days)) %>%
  mutate(time_days = if_else(grepl("r5",rec_timepoint),"5", time_days)) %>%
  mutate(time_days = if_else(grepl("r6",rec_timepoint),"6", time_days)) %>%
  mutate(time_days = if_else(grepl("r7",rec_timepoint),"7", time_days)) %>%
  mutate(time_days = if_else(grepl("r14",rec_timepoint),"14", time_days)) %>%
  mutate(time_days = if_else(grepl("r30",rec_timepoint),"30", time_days)) %>%
  mutate(time_days = if_else(grepl("r60",rec_timepoint),"60", time_days)) %>%
  mutate(time_days = if_else(grepl("r80",rec_timepoint),"80", time_days)) %>%
  mutate(time_days=as.numeric(time_days)) %>%
  mutate(treatment = case_when(
    treatment == "Control" ~ "Ambient",
    TRUE ~ treatment)) %>%
  na.omit() %>%
  distinct()

survivor_sub <- survivor_data %>%
  mutate(treatment = case_when(
    treatment == "Control" ~ "Ambient",
    TRUE ~ treatment)) %>%
  filter(!treatment=="Ambient") %>%
  filter(time_days=="80") %>%
  filter(rec_mort=="0") %>%
  select(genet) %>%
  distinct()

fit_rec <- survfit(Surv(time_days,rec_mort) ~ factor(treatment), data = survivor_data)

#extract data from survfit object
tidy_fit_rec <- tidy(fit_rec) %>%
  add_column(strata2=NA) %>%
  mutate(strata2 = if_else(grepl("Ambient",strata),"Ambient", strata2)) %>%
  mutate(strata2 = if_else(grepl("Heated",strata),"Heated", strata2))

M3A <- ggplot(tidy_fit_rec, aes(x=time,y=estimate,group=strata2,fill=strata2)) +
  geom_step(aes(time,estimate,color=strata2)) +
  geom_point(aes(time,estimate,color=strata2),shape=3,size=2) +
  geom_stepconfint(aes(ymin = conf.low, ymax = conf.high), fill="firebrick",alpha = 0.4) +
  labs(x = "Time in Days", y = expression("Survival Probability")) + 
  annotate("text", x=c(15),y=c(0.2), label=c("p < 0.0001")) +
  scale_x_continuous(breaks = c(0,7,14,30,60,80),
                     labels = c(0,7,14,30,60,80)) +
  scale_color_manual(values = c("dodgerblue","firebrick"),labels = c("Ambient","Heated")) +
  scale_fill_manual(values = c("dodgerblue","firebrick")) +
  guides(fill=guide_legend(override.aes=list(fill=c("dodgerblue","firebrick")))) +
  theme_bw() + theme(axis.title = element_text(size=10),
                     legend.title = element_blank(),legend.background=element_blank(), legend.position = c(.95, .99),
                     legend.justification = c("right", "top"),
                     legend.key.size = unit(2,"mm"),
                     legend.box.just = "right",
                     legend.margin = margin(6, 6, 6, 6)
  )

#pam mortality across recovery
rec_pam_data <- pam_data %>%
  filter(is.na(days_to_mortality)) %>%
  mutate(rec_died=case_when(rec_pam>=0~"Alive", TRUE~"Dead"))%>%
  select(coral_id,genet,treatment,pre_pam,last_pam,rec_pam,rec_died)%>%
  gather(timepoint,pam,pre_pam:rec_pam) %>%
  distinct() %>%
  filter(treatment=="Heated")%>%
  mutate(initial_pam=case_when(timepoint=="pre_pam"~pam))%>%
  arrange(coral_id)%>%
  group_by(coral_id)%>%
  fill(initial_pam,.direction=c("down"))%>%
  mutate(relative_pam=pam/initial_pam)%>%
  drop_na()%>%
  mutate(genet=as.factor(genet)) %>%
  add_column(new_time=NA) %>%
  mutate(new_time = if_else(grepl("pre_pam",timepoint),"0", new_time)) %>%
  mutate(new_time = if_else(grepl("last_pam",timepoint),"1", new_time)) %>%
  mutate(new_time = if_else(grepl("rec_pam",timepoint),"2", new_time)) %>%
  mutate(new_time=as.numeric(new_time))
#write.csv(rec_pam_data,"rec_pam_data.csv")

M3B <- ggplot(rec_pam_data, aes(x=new_time,y=relative_pam,group=coral_id,color=rec_died)) +
  geom_hline(yintercept = 0.7, linetype="dotted", color="black",linewidth=1.25) +
  geom_point(aes(color=rec_died), alpha=0.8)+
  geom_line(aes(color=rec_died), alpha=0.5) +
  scale_color_manual(values = c("seagreen4","orange4")) +
  scale_x_continuous(limits=c(0,2),breaks=seq(0,2,by=1),labels=c("0" = "Pre\nStress", "1" = "End of\nStress", "2" = "Recovery\nDay 80")) +
  labs(y="Relative Fv/Fm")+
  guides(color=guide_legend(title="Survival on\nRecovery Day 80",title.hjust = 0.5)) +
  theme_bw() + theme(axis.title = element_text(size=10),
                     axis.title.x=element_blank(),legend.position="none")
#legend.background = element_blank(), 
#legend.position = c(.32, .99),legend.justification = c("left", "top"),
#legend.box.just = "left",legend.margin = margin(6, 6, 6, 6))

#Recovery/Mortality by Dominant Symbiont spp.
sym_analysis_rec80 <- matrix(c(19,9,13,5,75,36),ncol=2,nrow=3,dimnames=list(c("C spp.", "Mixed", "D spp."),c("Dead","Alive")),byrow=TRUE)
fisher.test(sym_analysis_rec80)
pairwise_fisher_test(as.matrix(sym_analysis_rec80))

sym_analysis_rec7 <- matrix(c(16,12,12,6,69,42),ncol=2,nrow=3,dimnames=list(c("Cladocopium", "Mixed", "Durusdinium"),c("Dead","Alive")),byrow=TRUE)
fisher.test(sym_analysis_rec7)
pairwise_fisher_test(as.matrix(sym_analysis_rec7))

tmp2 <- melt(sym_analysis_rec80)
names(tmp2) <- c("Symbio", "Mortality", "Ramets")
tmp2 <- tmp2 %>%
  mutate(Mortality = as.factor(Mortality)) %>%
  mutate(Symbio = as.factor(Symbio))

pval_CD_rec <- c("p = 1")
pval_CM_rec <- c("p = 1")
pval_MD_rec <- c("p = 0.79")

M3C <- ggplot(tmp2, aes(x=Symbio, y=Ramets, fill=Mortality)) +
  geom_bar(stat="identity", position="stack", color="black") +
  scale_fill_manual(values=c("orange4","seagreen4")) +
  #annotate("text", x=1.5, y=43, label=c("n.s.")) +
  #annotate("text", x=1.8, y=63, label=c("n.s.")) +
  #annotate("text", x=2, y=83, label=c("n.s.")) +
  #annotate("segment", x = 1.1, xend = 1.9, y= 35, yend=35) +
  #annotate("segment", x = 1.1, xend = 2.4, y= 55, yend=55) +
  #annotate("segment", x = 1.6, xend = 2.4, y= 75, yend=75) +
  annotate("text", x=1, y=48, label=c("all n.s."), size=3) +
  annotate("text", x = 1.48, y= 120, label=c("Recovery Day 80"), size=3) +
  labs(x = "", y = "Ramet #") + 
  scale_x_discrete(labels=c('C', 'Mixed', 'D')) +
  theme_bw() + theme(axis.title = element_text(size=10),
                     legend.title = element_blank(),legend.background = element_blank(), 
                     legend.position = c(.01, .99),legend.justification = c("left", "top"),
                     legend.key.size = unit(2,"mm"),legend.box.just = "left",legend.margin = margin(6, 6, 6, 6))

quartz(w=7.2,h=2)
plot_grid(M3A,M3B,NULL,M3C, rel_widths=c(1.3,1.1,0.1,1), nrow=1,labels=c("A","B","","C"), label_size=12, axis="tb", align="hv")
#Fig3_BC <- plot_grid(M3B,NULL,M3C, labels=c("B","","C"), rel_widths = c(1.2,0.1,1), label_size=12,nrow=1)
#plot_grid(M3A,Fig3_BC,rel_heights=c(1,1), nrow=2,labels=c("A",""), label_size=12)
quartz.save("../figures/final/Fig3_final.pdf", type="pdf")

####FIGURE 4 ANALYSIS####
##surface area vs ED
#transform data and normality test
resistrecover_ED_sa <- resistrecover_ED %>%
  group_by(genet) %>%
  mutate(percentchange_avgsa = mean(percentchange_sa))
  
resistrecover_ED_sa$percentchange_avgsa2 <- log10(resistrecover_ED_sa$percentchange_avgsa)
resistrecover_ED_sa$percentchange_sa2 <- log10(resistrecover_ED_sa$percentchange_sa)
resistrecover_ED_sa$ed30_2 <- resistrecover_ED_sa$ed30^2

resistrecover_ED_sa <- resistrecover_ED_sa %>%
  na.omit()

ggplot(resistrecover_ED_sa, aes(x=percentchange_avgsa2)) +
  geom_histogram()

ggdensity(resistrecover_ED_sa$percentchange_avgsa2, main="Density plot of sa", xlab="surface area")
ggqqplot(resistrecover_ED_sa$percentchange_avgsa2)
skewness(resistrecover_ED_sa$percentchange_avgsa2, na.rm=TRUE)

#log tranformation, added 5 constant
sa_lm <- lm(percentchange_avgsa2 ~ ed30, data=resistrecover_ED_sa) 
sa_quad <- lm(percentchange_avgsa2 ~ ed30 + ed30_2, data=resistrecover_ED_sa)
#linear and quadratic fit sig

M4A <- ggplot(resistrecover_ED_sa,aes(x=ed30,y=percentchange_avgsa2)) +
  geom_point() +
  geom_smooth(method="lm") +
  geom_smooth(method="lm",formula = y ~ x + I(x^2),color="red") +
  labs(x = "", y = expression(log*"%"*Delta~Surface~Area)) + 
  annotate("text", x=c(5.35,5.2),y=c(-0.07,.07), label=c("linear p = 0.0039","quad p = 2.27e-05"),size=2.8) +
  theme_bw() + theme(legend.position="none", axis.title = element_text(size=9),axis.text = element_text()) 

##Buoyant Weight analysis
resistrecover_ED_bw <- resistrecover_ED %>%
  group_by(genet) %>%
  mutate(percentchange_avgbw = mean(percentchange_bw))

resistrecover_ED_bw$percentchange_avgbw2 <- log(resistrecover_ED_bw$percentchange_avgbw)
resistrecover_ED_bw$percentchange_bw2 <- log(resistrecover_ED_bw$percentchange_bw)
resistrecover_ED_bw$ed30_2 <- resistrecover_ED_bw$ed30^2

ggplot(resistrecover_ED_bw, aes(x=percentchange_avgbw2)) +
  geom_histogram() +
  theme_bw()

ggdensity(resistrecover_ED_bw$percentchange_avgbw2, main="Density plot of bw", xlab="buoyant weight")
ggqqplot(resistrecover_ED_bw$percentchange_avgbw2)
skewness(resistrecover_ED_bw$percentchange_avgbw2, na.rm=TRUE)

#log tranformation
bw_lm <- lm(percentchange_avgbw2 ~ ed30, data=resistrecover_ED_bw)
bw_quad <- lm(percentchange_avgbw2 ~ ed30 + ed30_2, data=resistrecover_ED_bw)
#quadratic fit yields p-value 0.01494 but low R2 = 0.02914

M4B <- ggplot(resistrecover_ED_bw,aes(x=ed30,y=percentchange_avgbw2)) +
  geom_point() +
  geom_smooth(method="lm") +
  geom_smooth(method="lm",formula = y ~ x + I(x^2),color="red") +
  labs(x = "", y = expression(log*"%"*Delta~"Buoyant Weight")) + 
  annotate("text", x=c(5.65,5.3),y=c(2.15,2.3), label=c("linear p = 0.88","quad p = 0.00028"),size=2.8) +
  theme_bw() + theme(legend.position="none", axis.title = element_text(size=9),axis.text = element_text()) 

#dataset for logistic regression morality
mortdays_ED <- ED %>%
  left_join(.,resistrecover_data,by="genet") %>%
  filter(is.na(days_to_mortality)) %>%
  filter(!treatment=="Control") %>%
  select(genet,coral_id,ed30,mort_r1,mort_r2,mort_r3,mort_r4,mort_r5,mort_r6,mort_r7,mort_r14,mort_r30,mort_r60,mort_r80) %>%
  left_join(.,profiles_clades,by="genet") %>%
  select(genet,coral_id,ed30,clade,mort_r1,mort_r2,mort_r3,mort_r4,mort_r5,mort_r6,mort_r7,mort_r14,mort_r30,mort_r60,mort_r80) %>%
  add_column(surv_r80 = NA) %>%
  mutate(surv_r80 = if_else(grepl("0", mort_r80),"1", surv_r80)) %>%
  mutate(surv_r80 = if_else(grepl("1", mort_r80),"0", surv_r80)) %>%
  mutate(surv_r80=as.numeric(surv_r80))

mortdays_ED$clade <- factor(mortdays_ED$clade,levels=c("C","mixed","D"))

##survivorship logistic regression based on ED values, 
day80.glm <- glm(surv_r80 ~ ed30, mortdays_ED, family = binomial)
summary(day80.glm, corr = FALSE)
#0.00123 generalized linear model, logistic regression

M4C <- ggplot(mortdays_ED, aes(x = ed30, y = surv_r80)) +
  geom_point(position = position_jitter(width = 0.3, height = 0)) +
  stat_smooth(aes(x = ed30), method = "glm", color = "black",  
              method.args = list(family = "binomial"), 
              se = FALSE) +
  labs(x = "Heat Tolerance",
       y = "Survival") + 
  annotate("text", x=c(6.1),y=c(0.1), label=c("p = 0.0012"),size=2.8) +
  annotate("text", x=c(3.5),y=c(0.85), label=c("Recovery Day 80"), size=2.8) +
  scale_x_continuous(breaks=seq(2, 7, 1)) +
  ylim(0,1) + theme_bw() + theme(axis.title = element_text(size=9),axis.title.y = element_text(hjust = 0.5))

M4A
M4B
M4C
quartz(w=2,h=5.2)
plot_grid(M4A,NULL,M4B,NULL,M4C, rel_heights=c(1,-0.19,1,-0.19,0.9), ncol=1, labels=c("A","","B","","C"), align="hv", axis="tb", label_size=12)
quartz.save("../figures/final/Fig4_final.pdf", type="pdf")

####FIGURE 5 ANALYSIS####
#juvenile heat stress by donor parent
M5A <- ggplot(juvenile_hs_heatmap,aes(x=sperm_parent,y=egg_parent,fill=integral)) +
  geom_tile() +
  labs(fill = expression("Juvenile\nTolerance\n∫ED10"), x = "Sire", y = expression("Dam")) + 
  scale_fill_gradientn(colors=c("blue","purple","red")) +
  coord_fixed() +
  annotate("rect", xmin=c(1.5,3.5), xmax=c(2.5,4.5), ymin=c(3.5,7.5), ymax=c(4.5,8.5), color="black", fill="transparent", linewidth=0.8) +
  #guides(fill = guide_colorbar(title.position = "top")) +
  theme(axis.title=element_text(size=10),
        panel.background = element_rect(fill="lightgray"),
        legend.title = element_text(size=8,hjust=0.5),
        legend.key.size = unit(3,"mm"), 
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-10,-10,-10,-10),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        panel.border = element_rect(color = "black",
                                    fill=NA,
                                    linewidth = 0.4),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

#scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
#guides(fill = guide_legend(title.position="top", title.hjust = 0.5)) +
#anova for dam*sire interaction, can't actually do...
juv_hs <- aov(integral~sperm_parent*egg_parent, data=juvenile_hs_heatmap)
kruskal.test(integral ~ parent_cross, data = juvenile_hs_heatmap)


juv_hs2 <- aov(integral~parent_cross, data=juvenile_hs_heatmap)
car::Anova(juv_hs)
summary(juv_hs)

ggplot(juvenile_hs_heatmap,aes(x=parent_cross,y=integral)) +
  geom_boxplot()

#Juvenile Heat Resistance and egg donor symbiont type
symbiont_hs_sub <- symbiont_growth %>%
  mutate(egg_parent=genet) %>%
  mutate(egg_parent=as.factor(egg_parent))

symbiont_hs_juv <- juvenile_hs_regressions %>%
  mutate(egg_parent=as.factor(egg_parent)) %>%
  left_join(.,symbiont_hs_sub,by="egg_parent",relationship="many-to-many") %>%
  select(juv_integral,clade) %>%
  mutate(clade=as.factor(clade))

symbiont_hs_juv$clade <- factor(symbiont_hs_juv$clade, levels=c("C","mixed","D"))
sym_hs_juv_model <- aov(lm(juv_integral~clade,data=symbiont_hs_juv))
TukeyHSD(sym_hs_juv_model)
CDj<-c("< 0.001")
CMj=0.0026
MDj=0.123 

#kruskal.test(juv_integral ~ clade, data = symbiont_hs_juv)
#pairwise.wilcox.test(symbiont_hs_juv$juv_integral, symbiont_hs_juv$clade)
#CDj=2.4e-6
#CMj=0.0053
#MDj=0.0403 

M5B <- ggplot(symbiont_hs_juv,aes(x=clade,y=juv_integral,color=clade)) +
  geom_boxplot() +
  geom_point() +
  labs(x = "Dam Symbiont Type", y = expression("Juvenile Tolerance")) + 
  annotate("text", x=1.5, y=6.71, label=CMj, size=2.8) +
  annotate("text", x=2, y=6.94, label=CDj,size=2.8) +
  annotate("text", x=2.5, y=7.09, label=MDj,size=2.8) +
  scale_color_manual(values = c("darkorange", "black", "blue")) +
  annotate("segment", x = 1.1, xend = 1.9, y= 6.64, yend=6.64) +
  annotate("segment", x = 1.1, xend = 2.9, y= 6.87, yend=6.87) +
  annotate("segment", x = 2.1, xend = 2.9, y= 7.02, yend=7.02) +
  scale_x_discrete(labels=c('C', 'Mixed', 'D')) +
  theme_bw() + theme(axis.title=element_text(size=10),legend.position="none")

#parent vs. juvenile tolerance
juvenile_hs_regressions<-juvenile_hs_regressions %>%
  mutate(sperm_parent=as.factor(sperm_parent),egg_parent=as.factor(egg_parent)) %>%
  mutate(midparent_integral=(egg_integral+sperm_integral)/2) %>%
  mutate(genet=sperm_parent) %>%
  left_join(.,ED,by="genet",relationship="many-to-many") %>%
  group_by(sperm_parent) %>%
  mutate(sperm_ed30=ed30) %>%
  select(sperm_parent,egg_parent,parent_cross,juv_integral,sperm_integral,egg_integral,sperm_ed30,midparent_integral) %>%
  mutate(genet=egg_parent) %>%
  left_join(.,ED,by="genet",relationship="many-to-many") %>%
  group_by(egg_parent) %>%
  mutate(egg_ed30=ed30) %>%
  select(sperm_parent,egg_parent,parent_cross,juv_integral,sperm_integral,egg_integral,sperm_ed30,egg_ed30,midparent_integral) %>%
  mutate(midparent_ed30=(egg_ed30+sperm_ed30)/2)

ggdensity(juvenile_hs_regressions$juv_integral)
ggqqplot(juvenile_hs_regressions$juv_integral)
skewness(juvenile_hs_regressions$juv_integral, na.rm=TRUE)
shapiro.test(juvenile_hs_regressions$juv_integral)

summary(lm(juv_integral ~ midparent_integral, data = juvenile_hs_regressions))
juv_hs_model <- lm(juv_integral ~ midparent_integral, data = juvenile_hs_regressions)
#summary(lm(juv_integral ~ midparent_ed30, data = juvenile_hs_regressions))
# Fit a linear regression model

coefficients <- coef(juv_hs_model)

# Extract the slope
slope <- coefficients["midparent_integral"]

M5C <- ggplot(juvenile_hs_regressions, aes(x=midparent_integral,y=juv_integral)) +
  geom_smooth(method="lm") +
  geom_point() +
  labs(x = "Mid-Parent Tolerance", y = expression("Juvenile Tolerance")) + 
  annotate("text", x=c(5.10),y=c(7.1), label=c("p = 0.029"),size=2.8) +
  theme_bw() + theme(axis.title=element_text(size=10),legend.position="none") 

#growth heatmap w/ individuals and aggregates
values <- c("657","661","672","760","762","764","769","913","915","918")
adult_growth_datafig5 <- growth_data_avg %>%
  mutate(sperm_parent=genet) %>%
  filter(sperm_parent %in% values) %>%
  mutate(sperm_growthrate=adult_growthrate) %>%
  select(sperm_parent,sperm_growthrate) %>%
  left_join(.,juveniles_list,by="sperm_parent",relationship="many-to-many") %>%
  
  
  filter(sperm_parent %in% values) %>% 
  mutate(egg_parent=egg_parent.x) %>%
  select(sperm_parent,egg_parent,adult_growthrate) %>%
  group_by(sperm_parent) %>%
  mutate(sperm_growthrate=adult_growthrate) %>%
  group_by(egg_parent) %>%
  mutate(egg_growthrate=adult_growthrate) %>%
  mutate(midparent_growthrate=(sperm_growthrate+egg_growthrate)/2) %>%
  distinct()
  

juvenile_growth_heatmap <- juvenile_raw_data %>%
  select(sperm_parent,egg_parent,parent_cross,growth_percentage_sa,juv_growthrate) %>%
  distinct() %>%
  group_by(parent_cross) %>%
  mutate(average_growth=mean(growth_percentage_sa)) %>%
  mutate(genet=sperm_parent) %>%
  mutate(genet=as.factor(genet)) %>%
  left_join(.,growth_data_avg,by="genet",relationship="many-to-many") %>%
  mutate(sperm_parent=genet) %>%
  select(sperm_parent,parent_cross,adult_growthrate,juv_growthrate,growth_percentage_sa,average_growth,percentchange_sa,percentchange_bw) %>%
  group_by(sperm_parent) %>%
  mutate(sperm_sa=percentchange_sa) %>%
  mutate(sperm_growthrate=adult_growthrate) %>%
  mutate(sperm_bw=percentchange_bw) %>%
  left_join(.,juvenile_raw_data,by="parent_cross",relationship="many-to-many") %>%
  distinct() %>%
  select(parent_cross,sperm_parent.x,egg_parent,sperm_sa,sperm_bw,sperm_growthrate,juv_growthrate.x,average_growth,growth_percentage_sa.x) %>%
  mutate(juv_growthrate=juv_growthrate.x) %>%
  mutate(growth_percentage_sa=growth_percentage_sa.x) %>%
  mutate(genet=egg_parent) %>%
  left_join(.,growth_data_avg,by="genet",relationship="many-to-many") %>%
  mutate(egg_parent=genet) %>%
  group_by(egg_parent) %>%
  mutate(egg_sa=percentchange_sa) %>%
  mutate(egg_growthrate=adult_growthrate) %>%
  mutate(egg_bw=percentchange_bw) %>%
  mutate(sperm_parent=sperm_parent.x) %>%
  mutate(midparent_sa=(sperm_sa+egg_sa)/2) %>%
  mutate(midparent_growthrate=(sperm_growthrate+egg_growthrate)/2) %>%
  mutate(midparent_bw=(sperm_bw+egg_bw)/2) %>%
  distinct() %>%
  left_join(.,juvenile_growth_data,by="growth_percentage_sa") %>%
  mutate(parent_cross=parent_cross.x) %>%
  mutate(sperm_parent=sperm_parent.x) %>%
  mutate(egg_parent=egg_parent.x) %>%
  mutate(egg_parent=egg_parent.x) %>%
  mutate(ind_agg_tf=as.factor(ind_agg_tf)) %>%
  mutate(sperm_parent=as.factor(sperm_parent)) %>%
  mutate(egg_parent=as.factor(egg_parent)) %>%
  select(sperm_parent,egg_parent,parent_cross,juv_growthrate,midparent_growthrate,growth_percentage_sa,ind_agg_tf,average_growth,percentchange_sa,percentchange_bw,sperm_sa,sperm_bw,egg_sa,egg_bw,midparent_sa,midparent_bw) %>%
  distinct() %>%
  mutate(scaledjuvenile=scale(juv_growthrate,center=TRUE,scale=TRUE)[,1]) %>%
  mutate(scaledadult=scale(midparent_growthrate,center=TRUE,scale=TRUE)[,1])

M5D <- ggplot(juvenile_growth_heatmap,aes(x=sperm_parent,y=egg_parent,fill=average_growth)) +
  geom_tile() +
  scale_fill_gradientn(colors=c("blue","purple","red")) +
  coord_fixed() +
  labs(fill = expression("Surface\nArea"), x = "Sire", y = expression("Dam")) +
  annotate("rect", xmin=c(1.5,9.5), xmax=c(2.5,10.5), ymin=c(.5,4.5), ymax=c(1.5,5.5), color="black", fill="transparent", linewidth=0.8) +
  theme(axis.title=element_text(size=10),
        panel.background = element_rect(fill="lightgray"),
        legend.title = element_text(size=8,hjust=0.5),
        legend.key.size = unit(3,"mm"), 
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-10,-10,-10,-10),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        panel.border = element_rect(color = "black",
                                    fill=NA,
                                    linewidth = 0.4),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

#m1 <- lmer(growth_percentage_sa ~ egg_parent*sperm_parent + (1|ind_agg_tf), data=juvenile_growth_heatmap)


m2 <- lm(growth_percentage_sa ~ egg_parent*sperm_parent + ind_agg_tf, data = juvenile_growth_heatmap)
anova(m2)

m3 <- lm(growth_percentage_sa ~ egg_parent*sperm_parent, data = juvenile_growth_heatmap)
anova(m3)

#symbiont type vs. juvenile growth
symbiont_growth_juv <- juvenile_growth_heatmap %>%
  mutate(egg_parent=as.factor(egg_parent)) %>%
  left_join(.,symbiont_hs_sub,by="egg_parent",relationship="many-to-many") %>%
  select(growth_percentage_sa,clade,ind_agg_tf) %>%
  mutate(clade=as.factor(clade)) %>%
  distinct()

symbiont_growth_juv$clade <- factor(symbiont_growth_juv$clade, levels=c("C","mixed","D"))

clade_juv_model <- lm(growth_percentage_sa ~ clade + ind_agg_tf, data = symbiont_growth_juv)
clade_juv_model_aov <- aov(clade_juv_model)
TukeyHSD(clade_juv_model_aov)

#multcomp::glht(clade_juv_model, linfct = multcomp::mcp(clade = "Tukey"))
CDj2=0.0015
CMj2=0.136
MDj2=0.086

M5E <- ggplot(symbiont_growth_juv,aes(x=clade,y=growth_percentage_sa,color=clade)) +
  geom_boxplot() +
  geom_point() +
  labs(x = "Dam Symbiont Type", y = expression("Juvenile %"*Delta~"Surface Area")) + 
  annotate("text", x=1.5, y=355, label=CMj2, size=2.8) +
  annotate("text", x=2, y=415, label=CDj2,size=2.8) +
  annotate("text", x=2.5, y=475, label=MDj2,size=2.8) +
  scale_color_manual(values = c("darkorange", "black", "blue")) +
  annotate("segment", x = 1.1, xend = 1.9, y= 330, yend=330) +
  annotate("segment", x = 1.1, xend = 2.9, y= 390, yend=390) +
  annotate("segment", x = 2.1, xend = 2.9, y= 450, yend=450) +
  scale_x_discrete(labels=c('C', 'Mixed', 'D')) +
  theme_bw() + theme(axis.title=element_text(size=10),legend.position="none")

#mid-parent growth vs. juvenile growth
#summary(lmer(growth_percentage_sa ~ midparent_sa + (1|ind_agg_tf), data=juvenile_growth_heatmap))
#juv_growth_model <- lm(growth_percentage_sa_cm ~ midparent_sa + ind_agg_tf, data=juvenile_growth_heatmap)

#juv_growth_model <- lm(scaledjuvenile ~ scaledadult + ind_agg_tf, data=juvenile_growth_heatmap)
juv_growth_model2 <- lm(juv_growthrate ~ midparent_growthrate + ind_agg_tf, data=juvenile_growth_heatmap)


summary(juv_growth_model)
summary(juv_growth_model2)
#modeltest <- lmer(growth_percentage_sa ~ midparent_sa*ind_agg_tf + (1|egg_parent) + (1|sperm_parent), data=juvenile_growth_heatmap)
#juv_growth_model2 <- lm(growth_percentage_sa ~ midparent_sa, data=juvenile_growth_heatmap)

M5F <- ggplot(juvenile_growth_heatmap, aes(x=midparent_growthrate,y=juv_growthrate)) +
  geom_smooth(method="lm") +
  geom_point() +
  labs(x = expression("Mid-Parent Growth Rate"), y = expression("Juvenile Growth Rate")) + 
  annotate("text", x=c(0.085),y=c(0.0013), label=c("p = 0.379"),size=2.8) +
  theme_bw() + theme(axis.title=element_text(size=10),legend.position="none");M5F

Fig5_ABC <- plot_grid(M5A,NULL,M5B,M5C,NULL, labels=c("A","","B","C",""), rel_widths = c(1,0.03,1.1,1,0.01), label_size=12,nrow=1, axis="tb", align="hv")
Fig5_DEF <- plot_grid(M5D,NULL,M5E,M5F,NULL, labels=c("D","","E","F",""), rel_widths = c(1,0.03,1.1,1,0.01), label_size=12,nrow=1, axis="tb", align="hv")
quartz(w=7.2,h=4.6)
plot_grid(Fig5_ABC,Fig5_DEF, rel_heights=c(1,1.2), nrow=2, labels=c("",""), label_size=12, axis="tb", align = "hv")
quartz.save("../figures/final/Fig5_final.pdf", type="pdf")

####SUPP FIGURE 1 ANALYSIS####
#Experimental tank system conditions
#S1A: temperature profiles during growth and heat stress recovery experiments
S1A_dates <- as.Date(c(A1_growth_c4_avg$time,A1_rec_c8_avg$time))
S1A_time <- data.frame(S1A_time=c(A1_growth_c4_avg$time,A2_growth_c6_avg$time,A3_growth_c10_avg$time,J1_growth_c11_avg$time,A1_rec_c8_avg$time,A2_rec_c21_avg$time))
S1A_temp <- data.frame(S1A_temp=c(A1_growth_c4_avg$temp,A2_growth_c6_avg$temp,A3_growth_c10_avg$temp,J1_growth_c11_avg$temp,A1_rec_c8_avg$temp,A2_rec_c21_avg$temp))
S1A_group <- data.frame(S1A_group=c(A1_growth_c4_avg$group,A2_growth_c6_avg$group,A3_growth_c10_avg$group,J1_growth_c11_avg$group,A1_rec_c8_avg$group,A2_rec_c21_avg$group))

S1A_dataset <- data.frame(S1A_time,S1A_temp,S1A_group)

S1A <- ggplot(S1A_dataset, aes(x=S1A_time,y=S1A_temp,color=S1A_group)) +
  geom_point() +
  geom_line() +
  scale_x_bd(business.dates = S1A_dates, max.major.breaks = 5, labels = scales::date_format("%m/%d/%y")) +
  labs(x=expression("Growth and Heat Recovey Timeline"), y=expression("Temperature "(degree*C))) +
  scale_color_manual(values = c("black","blue")) +
  theme_bw() + theme(axis.title.x=element_blank(), legend.title = element_blank(),
                     legend.position = c(.99, .99),
                     legend.background = element_blank(),
                     legend.justification = c("right", "top"),
                     legend.box.just = "right",
                     legend.margin = margin(6, 6, 6, 6))

#S1B: light logger profiles during growth period and heat stress experiment period for adults
S1B_dates <- as.Date(c(A1_growth_light$day,A1_niarox_light$day))
S1B_datetime <- data.frame(S1B_datetime=c(A1_growth_light$datetime,A2_growth_light$datetime,J1_growth_light$datetime,J2_growth_light$datetime,A1_niarox_light$datetime,J1_niarox_light$datetime))
S1B_day <- data.frame(S1B_day=c(A1_growth_light$day,A2_growth_light$day,J1_growth_light$day,J2_growth_light$day,A1_niarox_light$day,J1_niarox_light$day))
S1B_calibrated <- data.frame(S1B_calibrated=c(A1_growth_light$calibrated,A2_growth_light$calibrated,J1_growth_light$calibrated,J2_growth_light$calibrated,A1_niarox_light$calibrated,J1_niarox_light$calibrated))
S1B_group <- data.frame(S1B_group=c(A1_growth_light$group,A2_growth_light$group,J1_growth_light$group,J2_growth_light$group,A1_niarox_light$group,J1_niarox_light$group))
S1B_coral <- data.frame(S1B_coral=c(A1_growth_light$coral,A2_growth_light$coral,J1_growth_light$coral,J2_growth_light$coral,A1_niarox_light$coral,J1_niarox_light$coral))

S1B_dataset <- data.frame(S1B_datetime,S1B_day,S1B_calibrated,S1B_group,S1B_coral)


S1B <- ggplot(S1B_dataset, aes(x=S1B_day,y=S1B_calibrated,color=S1B_coral)) +
  geom_point(alpha=0.5) +
  geom_line(alpha=0.5) +
  labs(x="", y=expression(PAR~Light~mu*mol~m^{"−2"}~s^{"−1"})) +
  geom_vline(xintercept = as.POSIXct(as.Date("2023-01-24")), linetype = "dotted", color="black") +
  geom_vline(xintercept = as.POSIXct(as.Date("2023-02-06")), linetype = "dotted", color="black") +
  scale_x_bd(business.dates = S1B_dates, max.major.breaks = 5, labels = scales::date_format("%m/%d/%y")) +
  theme_bw() + theme(axis.title.x=element_blank(), legend.title = element_blank(),
                     legend.position = c(.99, .99),
                     legend.background = element_blank(),
                     legend.justification = c("right", "top"),
                     legend.box.just = "right",
                     legend.margin = margin(6, 6, 6, 6))

quartz(w=7.5,h=6)
S1A
quartz.save("../figures/final_precon/S1A.pdf", type="pdf")
S1B
quartz.save("../figures/final_precon/S1B.pdf", type="pdf")

quartz(w=7.2,h=4.2)
plot_grid(S1A,S1B, labels=c("A","B"), rel_heights = c(1,1.2), label_size=12,nrow=2, axis="tblr", align="v")
quartz.save("../figures/final/FigS1.pdf", type="pdf")

####SUPP FIGURE 2 ANALYSIS####
#Adult fragment growth information
#Fig. S2A: percent change surface area of adult ramets
S2A_main <- ggplot(resistrecover_ED_sa, aes(x=percentchange_avgsa)) +
  geom_histogram() +
  labs(x = Delta~"Percent Surface Area", y = expression("Adult Ramet Count")) + 
  theme_bw() + theme(legend.position="none") 

S2A_inset <- ggplot(resistrecover_ED_sa, aes(x=percentchange_avgsa2)) +
  geom_histogram() +
  labs(x = "SA log", y=expression("")) + 
  theme_bw() + theme(legend.position="none") 

S2A <- ggdraw() +
  draw_plot(S2A_main) +
  draw_plot(S2A_inset, x=.55,y=.62,width=.4,height=.35)

#Fig. S2B: percent change surface area of adult genets
S2B <- ggplot(resistrecover_ED_sa, aes(x=reorder(genet,percentchange_sa2),y=percentchange_sa2)) +
  geom_line() +
  stat_summary(geom="point",fun="mean",size=2) +
  labs(x = "Genet", y=expression(log*"%"*Delta~Surface~Area)) + 
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  theme_bw() + theme(axis.text.x=element_text(angle=90,vjust=1,hjust=1),legend.position="none") 

adult_sa_aov <- aov(percentchange_avgsa2~genet, data=resistrecover_ED_sa)
summary(adult_sa_aov)

#Fig. S2C: percent change buoyant weight ramets

S2C_main <- ggplot(resistrecover_ED_bw, aes(x=percentchange_avgbw)) +
  geom_histogram() +
  labs(x = Delta~"Percent Buoyant Weight", y = expression("Adult Ramet Count")) + 
  theme_bw() + theme(legend.position="none") 

S2C_inset <- ggplot(resistrecover_ED_bw, aes(x=percentchange_avgbw2)) +
  geom_histogram() +
  labs(x = "BW log", y=expression("")) + 
  theme_bw() + theme(legend.position="none") 

S2C <- ggdraw() +
  draw_plot(S2C_main) +
  draw_plot(S2C_inset, x=.55,y=.62,width=.4,height=.35)

#Fig. S2D: percent change buoyant weight of adult genets

S2D <- ggplot(resistrecover_ED_bw, aes(x=reorder(genet,percentchange_bw2),y=percentchange_bw2)) +
  geom_line() +
  stat_summary(geom="point",fun="mean",size=2) +
  labs(x = "Genet", y=expression(log*"%"*Delta~Buoyant~Weight)) + 
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  theme_bw() + theme(axis.text.x=element_text(angle=90,vjust=1,hjust=1),legend.position="none") 

adult_bw_aov <- aov(percentchange_avgbw2~genet, data=resistrecover_ED_bw)
summary(adult_bw_aov)

quartz(w=8,h=6)
S2A
quartz.save("../figures/final_precon/S2A.pdf", type="pdf")
S2B
quartz.save("../figures/final_precon/S2B.pdf", type="pdf")
S2C
quartz.save("../figures/final_precon/S2C.pdf", type="pdf")
S2D
quartz.save("../figures/final_precon/S2D.pdf", type="pdf")

quartz(w=8,h=6)
FigS2_AB <- plot_grid(S2A,S2B, labels=c("A","B"), rel_widths = c(1,1.2), ncol=2)
FigS2_CD <- plot_grid(S2C,S2D, labels=c("C","D"), rel_widths = c(1,1.2), ncol=2)
plot_grid(FigS2_AB,FigS2_CD, labels=c("",""),nrow=2, align="hv", axis="tb")
quartz.save("../figures/final/FigS2.pdf", type="pdf")

####SUPP FIGURE 3 ANALYSIS####
#Adult and juvenile Heat Resistance

#Adult ED30 curves by each genet
S3 <- ggplot() +
  geom_line(data=out,aes(x=cumulative,y=predicted))+
  geom_hline(yintercept=.7, linetype="dotted") +
  facet_wrap(~genet) +
  labs(x = "Degree Heating Weeks (DHW)", y = expression("Genet Relative Fv/Fm")) + 
  theme_bw() + theme(legend.position="none") 

quartz(w=5,h=6)
plot_grid(S3)
quartz.save("../figures/final/FigS3.pdf", type = "pdf")

####SUPP FIGURE 4 ANALYSIS####
#Juvenile ED10 curves by each parent cross
S4 <- ggplot() +
  geom_line(data=out_j,aes(x=cumulative,y=predicted))+
  geom_hline(yintercept=.9, linetype="dotted") +
  facet_wrap(~parent_cross) +
  labs(x = "Degree Heating Weeks (DHW)", y = expression("Juvenile Relative Fv/Fm")) + 
  theme_bw() + theme(legend.position="none") 

quartz(w=5,h=6)
plot_grid(S4)
quartz.save("../figures/final/FigS4.pdf", type = "pdf")

####SUPP FIGURE 5 ANALYSIS####
#integral histogram of adult and juvenile values
S5 <- ggplot(adult_juvenile_integrals,aes(x=integral,fill=group)) +
  geom_histogram(alpha=0.5,bins=50,color="black") +
  scale_fill_manual(values=c("blue","orange")) +
  annotate("text", x=3,y=15, label="p < 0.001") +
  labs(x = "Area Under Effective Dose Response Curves", y = "Sample Count") +
  theme(text = element_text(size = 4)) +  
  theme_bw() + theme(legend.title = element_blank())

quartz(w=5,h=4)
plot_grid(S5)
quartz.save("../figures/final/FigS5.pdf", type = "pdf")

####SUPP FIGURE 6 ANALYSIS####
#Plot by ITS2 type profiles by genet: Fig. S4A
S6A <- ggplot(profiles2, aes(x=reorder(genet,-Cladocopium_prop), y=type_profile_prop))+
  geom_col(aes(fill = type_profile), colour="grey", size=0.005)+
  labs(x="Genet", y="Symbiodiniaceae\nProportion")+
  theme_bw(base_size = 12)+
  scale_y_continuous(labels=function(Prop)Prop+1)+
  scale_fill_manual(values=my_colors_profile, name="ITS2 Type Profile")+
  scale_y_continuous(labels=c("1" = "0", "2" = "0.33", "3" = "0.67","4" = "1.00")) +
  guides(color = guide_legend(override.aes = list(linewidth=1.5)), fill=guide_legend(ncol=3, title.theme = element_text(angle = 90)))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, color = c(rep("darkorange",9),rep("grey15",6),rep("blue",45))),axis.ticks.x=element_blank(), text=element_text(size=7),panel.grid = element_blank(), strip.text = element_text(size = 7), legend.position = "bottom", legend.key.size = unit(0.3, "cm"))

#S6B
S6Baov <- aov(lm(percentchange_sa ~ clade, data = symbiont_growth))
TukeyHSD(S6Baov)
#all are not significant
#CD: 0.232
#CM: 0.324
#MD: 0.927

pval_CD3 <- c("")
pval_CM3 <- c("")
pval_MD3 <- c("")

S6B <- ggplot(symbiont_growth,aes(x=clade,y=percentchange_sa,color=clade)) +
  geom_boxplot() +
  geom_point() +
  labs(x = "Dominant Symbiont spp.", y = expression("%"~Delta~Surface~Area)) + 
  annotate("text", x=2, y=30, label=c("all n.s.")) +
  #annotate("text", x=1.5, y=22, label=pval_CM3) +
  #annotate("text", x=2, y=27, label=pval_CD3) +
  #annotate("text", x=2.5, y=32, label=pval_MD3) +
  #annotate("segment", x = 1.1, xend = 1.9, y= 20, yend=20) +
  #annotate("segment", x = 1.1, xend = 2.9, y= 25, yend=25) +
  #annotate("segment", x = 2.1, xend = 2.9, y= 30, yend=30) +
  scale_x_discrete(labels=c('Cladocopium', 'Mixed', 'Durusdinium')) +
  scale_color_manual(values = c("darkorange", "black", "blue")) +
  theme_bw() + theme(legend.position="none",axis.title.x=element_blank()) 

##clade vs. BW
S6Caov <- aov(lm(percentchange_bw ~ clade, data = symbiont_growth))
TukeyHSD(S6Caov)
#all are not significant

#pval_CD4 <- c("p = 0.841")
#pval_CM4 <- c("p = 0.621")
#pval_MD4 <- c("p = 0.792")

pval_CD4 <- c("")
pval_CM4 <- c("")
pval_MD4 <- c("")

S6C <- ggplot(symbiont_growth,aes(x=clade,y=percentchange_bw,color=clade)) +
  geom_boxplot() +
  geom_point() +
  labs(x = "Dominant Symbiont spp.", y = expression("%"~Delta~Buoyant~Weight)) + 
  annotate("text", x=2, y=40, label=c("all n.s.")) +
  #annotate("text", x=1.5, y=42, label=pval_CM4) +
  #annotate("text", x=2, y=47, label=pval_CD4) +
  #annotate("text", x=2.5, y=52, label=pval_MD4) +
  #annotate("segment", x = 1.1, xend = 1.9, y= 40, yend=40) +
  #annotate("segment", x = 1.1, xend = 2.9, y= 45, yend=45) +
  #annotate("segment", x = 2.1, xend = 2.9, y= 50, yend=50) +
  scale_x_discrete(labels=c('Cladocopium', 'Mixed', 'Durusdinium')) +
  scale_color_manual(values = c("darkorange", "black", "blue")) +
  theme_bw() + theme(legend.position="none",axis.title.x=element_blank()) 

quartz(w=7,h=6)
FigS6_BC <- plot_grid(S6B,S6C, labels=c("B","C"), rel_widths = c(1,1), nrow=1)
plot_grid(S6A,FigS6_BC, labels=c("A",""), axis = "tb", rel_heights = c(1.2,1), nrow=2)
quartz.save("../figures/final/FigS6.pdf", type="pdf")

####SUPP FIGURE 7 ANALYSIS####
#Juvenile growth by parent cross
juvenile_pc_ind_integral <- juvenile_pc_ind %>%
  left_join(.,out_juved10,by="parent_cross")

ggplot(juvenile_pc_ind_integral, aes(x=growth_percentage_sa)) +
  geom_histogram() +
  theme_bw()

pc_heat_growth_ind <- juvenile_pc_ind_integral %>%
  filter(between(growth_percentage_sa,-100,400))

ggplot(pc_heat_growth_ind, aes(x=growth_percentage_sa)) +
  geom_histogram() +
  theme_bw()

mean(pc_heat_growth_ind$growth_percentage_sa)
sd(pc_heat_growth_ind$growth_percentage_sa)

pc_heat_growth_ind$growth_percentage_sa <- pc_heat_growth_ind$growth_percentage_sa + 100
pc_heat_growth_ind$growth_percentage_sa2 <- sqrt(pc_heat_growth_ind$growth_percentage_sa)
pc_heat_growth_ind$integral2 <- pc_heat_growth_ind$integral^2

ggdensity(pc_heat_growth_ind$growth_percentage_sa2, main="Density plot of juvenile growth", xlab="surface area percent change")
ggqqplot(pc_heat_growth_ind$growth_percentage_sa2)
skewness(pc_heat_growth_ind$growth_percentage_sa2, na.rm=TRUE)
shapiro.test(pc_heat_growth_ind$growth_percentage_sa2)

pc_heat_growth_ind_long <- pc_heat_growth_ind %>%
  select(sperm_percentchange_sa,egg_percentchange_sa,growth_percentage_sa2) %>%
  gather(group,adult_sa,sperm_percentchange_sa:egg_percentchange_sa) %>%
  mutate(group = if_else(grepl("egg",group),"Dam", group)) %>%
  mutate(group = if_else(grepl("sperm",group),"Sire", group))

juvenile_pc_agg_integral <- juvenile_pc_agg %>%
  left_join(.,out_juved10,by="parent_cross")

ggplot(juvenile_pc_agg_integral, aes(x=growth_percentage_sa)) +
  geom_histogram() +
  theme_bw()

pc_heat_growth_agg <- juvenile_pc_agg_integral %>%
  filter(between(growth_percentage_sa,-100,400))

ggplot(pc_heat_growth_agg, aes(x=growth_percentage_sa)) +
  geom_histogram() +
  theme_bw()

mean(pc_heat_growth_agg$growth_percentage_sa)
sd(pc_heat_growth_agg$growth_percentage_sa)

pc_heat_growth_agg$growth_percentage_sa <- pc_heat_growth_agg$growth_percentage_sa + 100
pc_heat_growth_agg$growth_percentage_sa2 <- sqrt(pc_heat_growth_agg$growth_percentage_sa)
pc_heat_growth_agg$integral2 <- pc_heat_growth_agg$integral^2

ggdensity(pc_heat_growth_agg$growth_percentage_sa2, main="Density plot of juvenile growth", xlab="surface area percent change")
ggqqplot(pc_heat_growth_agg$growth_percentage_sa2)
skewness(pc_heat_growth_agg$growth_percentage_sa2, na.rm=TRUE)
shapiro.test(pc_heat_growth_agg$growth_percentage_sa2)

pc_heat_growth_agg_long <- pc_heat_growth_agg %>%
  select(sperm_percentchange_sa,egg_percentchange_sa,growth_percentage_sa2) %>%
  gather(group,adult_sa,sperm_percentchange_sa:egg_percentchange_sa) %>%
  mutate(group = if_else(grepl("egg",group),"Dam", group)) %>%
  mutate(group = if_else(grepl("sperm",group),"Sire", group))

S7A <- ggplot(juvenile_growth_parent, aes(x=egg_parent,y=type_sa,fill=type)) +
  geom_boxplot() +
  #geom_point(aes(color=type),position=position_dodge(width=0.75),show.legend = F) +
  scale_color_manual(values = c("black","black")) +
  scale_fill_discrete(labels=c("Aggregates","Individuals")) +
  ylim(-100,400) +
  labs(fill = "Juvenile Type", x = "Dam", y=expression("%"~Delta~Surface~Area)) + 
  theme_bw() + theme(legend.position = "none")

S7B <- ggplot(juvenile_growth_parent, aes(x=sperm_parent,y=type_sa,fill=type)) +
  geom_boxplot() +
  #geom_point(aes(color=type),position=position_dodge(width=0.75),show.legend = F) +
  scale_color_manual(values = c("black","black")) +
  scale_fill_discrete(labels=c("Aggregates","Individuals")) +
  ylim(-100,400) +
  labs(fill = "Juvenile Type", x = "Sire", y=expression("%"~Delta~Surface~Area)) + 
  theme_bw() 

S7C <- ggplot(juvenile_growth_parent, aes(x=reorder(parent_cross,sperm_parent),y=type_sa,fill=type)) +
  geom_boxplot() +
  #geom_point(aes(color=type),position=position_dodge(width=0.75),show.legend = F) +
  scale_color_manual(values = c("black","black")) +
  scale_fill_discrete(labels=c("Aggregates","Individuals")) +
  ylim(-100,400) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  labs(fill = "Juvenile Type", x = "Parent Cross", y=expression("%"~Delta~Surface~Area)) + 
  theme_bw() + theme(legend.position="none",axis.text.x = element_text(angle = 45, hjust = 1))

#Create a table summarizing parent key
parentcross_key1 <- juveniles_list %>%
  select(parent_cross,sperm_parent,egg_parent) %>%
  mutate(sperm_id = substring(parent_cross, 1, 1),
         egg_id = substring(parent_cross, 2)) %>%
  select(sperm_parent,sperm_id) %>%
  mutate(sperm_parent=as.factor(sperm_parent)) %>%
  distinct() %>%
  arrange(sperm_parent) 

add769 <- data.frame(egg_parent = "769", egg_id ="NA")
parentcross_key2 <- juveniles_list %>%
  select(parent_cross,sperm_parent,egg_parent) %>%
  mutate(sperm_id = substring(parent_cross, 1, 1),
         egg_id = substring(parent_cross, 2)) %>%
  select(egg_parent,egg_id) %>%
  distinct()
parentcross_key2 <- rbind(parentcross_key2,add769) %>%
  arrange(egg_parent)

parentcross_key <- bind_cols(parentcross_key1,parentcross_key2) %>%
  mutate(Genet = sperm_parent) %>%
  mutate('Sire ID' = sperm_id) %>%
  mutate('Dam ID' = egg_id) %>%
  select(Genet,'Sire ID','Dam ID')

pc_key <- flextable(parentcross_key)

pc_key <- ggplot() +
  theme_void() +
  annotation_custom(grid::rasterGrob(as_raster(pc_key)),
                    xmin = -Inf,
                    xmax = Inf,
                    ymin = -Inf,
                    ymax = Inf)

quartz(w=8,h=6)
FigS7_AB <- plot_grid(S7A,S7B, labels=c("A","B"),nrow=1, rel_widths = c(0.7,1))
FigS7_CD <- plot_grid(S7C,pc_key, labels=c("C",""),nrow=1, rel_widths = c(2,.6))
plot_grid(FigS7_AB,FigS7_CD, labels=c("",""), nrow=2, rel_heights = c(0.6,1))
quartz.save("../figures/final/FigS7.pdf", type="pdf")

####SUPP FIGURE 8 ANALYSIS####
#individuals but not average parent cross, just all values not averaged
ggplot(juvenile_growth_heatmap, aes(x=growth_percentage_sa)) +
  geom_histogram() +
  theme_bw()

juvenile_growth_S8 <- juvenile_growth_heatmap %>%
  left_join(.,out_juved10,by="parent_cross",relationship="many-to-many") %>%
  select(growth_percentage_sa,integral) %>%
  left_join(.,juvenile_raw_data,by="growth_percentage_sa") %>%
  select(parent_cross,growth_percentage_sa,integral,ind_agg_tf) %>%
  mutate(ind_agg_tf = case_when(
    ind_agg_tf == "A" ~ "Aggregates",
    ind_agg_tf == "I" ~ "Individuals")) %>%
  distinct()

juvenile_growth_S8$growth_percentage_sa <- juvenile_growth_S8$growth_percentage_sa + 100
juvenile_growth_S8$growth_percentage_sa2 <- sqrt(juvenile_growth_S8$growth_percentage_sa)
juvenile_growth_S8$integral2 <- juvenile_growth_S8$integral^2

#ggdensity(pc_heat_growth_ind$growth_percentage_sa2, main="Density plot of juvenile growth", xlab="surface area percent change")
#ggqqplot(pc_heat_growth_ind$growth_percentage_sa2)
#skewness(pc_heat_growth_ind$growth_percentage_sa2, na.rm=TRUE)
#shapiro.test(pc_heat_growth_ind$growth_percentage_sa2)

S8A <- ggplot(juvenile_growth_S8,aes(x=integral,y=growth_percentage_sa2)) +
  geom_point() +
  geom_smooth(method="lm") +
  annotate("text", x=c(5.88),y=c(2), label=c("p = 0.171"),size=3) +
  labs(x = "Juvenile Heat Tolerance", y = expression(Juvenile~sqrt("%"*Delta~Surface~Area)),size=10) + 
  theme_bw()

S8B <- ggplot(juvenile_growth_S8,aes(x=ind_agg_tf,y=growth_percentage_sa2)) +
  geom_boxplot() +
  geom_point() +
  annotate("text", x=c(0.7),y=c(3), label=c("p < 0.001"),size=3) +
  labs(x = "", y = expression(Juvenile~sqrt("%"*Delta~Surface~Area)),size=10) + 
  theme_bw()

S8C <- ggplot(juvenile_growth_S8,aes(x=ind_agg_tf,y=integral)) +
  geom_boxplot() +
  geom_point() +
  annotate("text", x=c(0.7),y=c(5.88), label=c("p = 0.019"),size=3) +
  labs(x = "", y = expression("Juvenile Heat Tolerance"),size=10) + 
  theme_bw() + theme(legend.position = "none")

#S8 <- ggplot(juvenile_growth_S8,aes(x=integral,y=growth_percentage_sa2)) +
  geom_point() +
  geom_smooth(method="lm") +
  geom_smooth(method="lm",formula = y ~ x + I(x^2),color="red") +
  annotate("text", x=c(5.88,5.86),y=c(2,3), label=c("linear p = 0.171","quad p = 0.02")) +
  labs(x = "Juvenile Heat Tolerance", y = expression(Juvenile~sqrt("%"*Delta~Surface~Area))) + 
  theme_bw() 

juvenile_growth_S8_lm <- lm(growth_percentage_sa2 ~ integral + ind_agg_tf, data=juvenile_growth_S8)
juvenile_growth_S8_aov <- aov(lm(growth_percentage_sa2 ~ ind_agg_tf, data=juvenile_growth_S8))
juvenile_tolerance_S8_aov <- aov(lm(integral ~ ind_agg_tf, data=juvenile_growth_S8))
#juvenile_growth_S8_quad <- lm(growth_percentage_sa2 ~ integral + integral2 + ind_agg_tf, data=juvenile_growth_S8)
summary(juvenile_growth_S8_lm)
summary(juvenile_growth_S8_aov)
summary(juvenile_tolerance_S8_aov)
#summary(juvenile_growth_S8_quad)

quartz(w=7.2,h=5.5)
FigS8BC <- plot_grid(S8B,S8C, labels=c("B","C"), nrow = 2)
plot_grid(S8A,FigS8BC, labels=c("A",""),ncol = 2)
quartz.save("../figures/final/FigS8.pdf", type = "pdf")

####SUPP FIGURE 9 ANALYSIS####
#S9: mortality by parent cross survivorship curves
hsfit_j2 <- survfit(Surv(time_days,mort)~parent_cross, data = juvenile_mortality)

#extract Adult data from survfit object
tidy_hsfit_j2 <- tidy(hsfit_j2)
tidy_hsfit_j2$strata<-gsub("parent_cross=","",as.character(tidy_hsfit_j2$strata))

S9 <- ggplot(tidy_hsfit_j2, aes(x=time,y=estimate)) +
  geom_step(color="firebrick") +
  geom_point(shape=3,size=2,color="firebrick") +
  geom_stepconfint(aes(ymin = conf.low, ymax = conf.high), alpha = 0) +
  facet_wrap(~strata) +
  labs(x = "Time in Days", y = expression("Juvenile Survival Probability")) + 
  scale_x_continuous(breaks = c(0,4,8,12),
                     labels = c(0,4,8,12)) +
  theme_bw()

quartz(w=8,h=6.5)
plot_grid(S9)
quartz.save("../figures/final/FigS9.pdf", type="pdf")

####Graphical Abstract####
M2C
M4B
M4C
M5B
M5C
M5E

M2C_GA <- ggplot(symbiont_ED,aes(x=clade,y=ed30,color=clade)) +
  geom_boxplot() +
  geom_point() +
  labs(x = "Dominant Symbiont Type", y = expression("Heat Tolerance")) +
  annotate("text", x=1.5, y=5.5, label=pval_CM2, size=3) +
  annotate("text", x=2, y=6.3, label=pval_CD2, size=3) +
  annotate("text", x=2.5, y=7, label=pval_MD2, size=3) +
  annotate("segment", x = 1.1, xend = 1.9, y= 5.2, yend=5.2) +
  annotate("segment", x = 1.1, xend = 2.9, y= 6.0, yend=6.0) +
  annotate("segment", x = 2.1, xend = 2.9, y= 6.7, yend=6.7) +
  scale_x_discrete(labels=c('C', 'Mixed', 'D')) +
  scale_color_manual(values = c("darkorange", "black", "blue")) +
  theme_bw() + theme(axis.title = element_text(size=10),legend.position="none") 

M4B_GA <- ggplot(resistrecover_ED_bw,aes(x=ed30,y=percentchange_avgbw2)) +
  geom_point() +
  geom_smooth(method="lm") +
  geom_smooth(method="lm",formula = y ~ x + I(x^2),color="red") +
  labs(x = "Adult Heat Tolerance", y = expression(log*"%"*Delta~"Buoyant Weight")) + 
  annotate("text", x=c(5.65,5.3),y=c(2.15,2.3), label=c("linear p = 0.88","quad p = 0.00028"),size=2.8) +
  theme_bw() + theme(legend.position="none", axis.title = element_text(size=10)) 


M4C_GA <- ggplot(mortdays_ED, aes(x = ed30, y = surv_r80)) +
  geom_point(position = position_jitter(width = 0.3, height = 0)) +
  stat_smooth(aes(x = ed30), method = "glm", color = "black",  
              method.args = list(family = "binomial"), 
              se = FALSE) +
  labs(x = "Adult Heat Tolerance",
       y = "Adult Survival") + 
  annotate("text", x=c(6.1),y=c(0.1), label=c("p = 0.0012"),size=2.8) +
  annotate("text", x=c(3.5),y=c(0.85), label=c("Recovery Day 80"), size=2.8) +
  scale_x_continuous(breaks=seq(2, 7, 1)) +
  ylim(0,1) + theme_bw() + theme(axis.title = element_text(size=10),axis.title.y = element_text(hjust = 0.5))

quartz(w=7.2,h=6)
title_theme1 <- ggdraw() +
  draw_label("Adult heat tolerance and recovery, symbiont community, and baseline growth",size=10,x=0.05,hjust=0)
title_theme2 <- ggdraw() +
  draw_label("Juvenile heat tolerance and growth, symbionts, and heat tolerance heritability",size=10,x=0.05,hjust=0)
GA_ABC <- plot_grid(M2C_GA,M4B_GA,M4C_GA,ncol=3,axis="tb",align="hv")
GA_DEF <- plot_grid(M5B,M5E,M5C,ncol=3,axis="tb",align="hv")
plot_grid(title_theme1,GA_ABC,title_theme2,GA_DEF,nrow=4,axis="tb",align="hv", rel_heights = c(0.06,1,0.06,1))
quartz.save("../figures/final/GraphicalAbstract.pdf", type="pdf")

