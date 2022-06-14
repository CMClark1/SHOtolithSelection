####################
# Silver Hake Otolith Selection - Survey Backlog
# Caira Clark
# 2022
####################

library(ROracle)
library(ggplot2)
library(dplyr)
library(tidyr)

channel <- ROracle::dbConnect(DBI::dbDriver("Oracle"), username=oracle.username, password=oracle.password, oracle.dsn)  

##LOAD SURVEY DATA-------------------------

#Select fish with otoliths collected during missing otolith years
survey <-dbGetQuery(channel, "select a.mission, a.area, a.strat, a.slat, a.slong, a.setno, b.fshno, b.fsex, b.flen, b.fwt, b.fmat from groundfish.gsinf a, groundfish.gsdet b where a.mission in ('NED2017020', 'TEL2018023', 'NED2019030', 'NED2020025', 'CAR2021240') and b.spec=14 and b.agmat='1' and a.mission=b.mission and a.setno=b.setno and b.fshno is not null")

survey$LATITUDE = (as.numeric(substr(survey$SLAT,1,2))+(survey$SLAT - as.numeric(substr(survey$SLAT,1,2))*100)/60)
survey$LONGITUDE = (as.numeric(substr(survey$SLONG,1,2))+(survey$SLONG - as.numeric(substr(survey$SLONG,1,2))*100)/60)*-1

surveyTweaked <- Mar.utils::identify_area(df=survey, lat.field = "LATITUDE", lon.field = "LONGITUDE",
                                          agg.poly.shp = "S:/Science/Population Ecology/Silver Hake/2021/Assessment/data/shapefiles/SH_fishing_areas.shp",
                                          agg.poly.field = "mgmt_area")

survey <- surveyTweaked

survey <- survey %>%  mutate(
  LOCATION = case_when(
    mgmt_area == "<outside known areas>" ~ 'Scotian Shelf'
    , mgmt_area %in% c("Emerald", "LaHave") ~ 'Basins'
    , mgmt_area == "SMGL" ~ 'Small Mesh Gear Line'))

#Add a year column
survey$YEAR <- as.factor(substr(survey$MISSION, start = 4, stop = 7))

#Select Scotian Shelf strata
survey <- survey %>% filter(STRAT %in% c(440:483))

#Select only sex codes 1 and 2
survey <- survey %>% filter(FSEX %in% c(1, 2))

survey_copy <- survey

#CREATE SELECTION GROUPS--------------

#Create four selection groups (1 is <26 and male, 2 is >26 and male, 3 is <28 and female, 4 is >28 and female)
survey_copy <- survey_copy %>%  mutate(
  GROUP = case_when(
    FLEN < 26 & FSEX == 1 ~ 1
    , FLEN >= 26 & FSEX == 1 ~ 2
    , FLEN < 28 & FSEX == 2 ~ 3
    , FLEN >= 28 & FSEX == 2 ~ 4))

#Group data into 2cm length intervals
survey_copy$LEN_GROUP <- cut(survey_copy$FLEN, breaks = seq(1,60,2), labels=seq(2,58,2))

#Divide data into four data frames based on selection group

select1 <- survey_copy %>% filter(GROUP==1)
select2 <- survey_copy %>% filter(GROUP==2)
select3 <- survey_copy %>% filter(GROUP==3)
select4 <- survey_copy %>% filter(GROUP==4)

#SELECT FROM SELECTION GROUPS----------

#For group 1 (<26cm male, all years), select 10 per 2cm length group

split_up <- split(select1, f = select1$LEN_GROUP)
sel_5 <- lapply(split_up, function(x) {x %>% sample_n(ifelse(nrow(x) < 10, nrow(x), 10))}) #change the 5 in here to change the subsample size
sel_5 <- do.call("rbind", sel_5)
select1_tba <- sel_5

#For group 2 (>26cm male, divided by year), select 10 per 2cm length group

years<-unique(select2$YEAR) #pick out years
output<-select2[1,] #Create a base 'output' file
output$YEAR<-NA
for (i in years){
  select_subset<-subset(select2, YEAR==i)
  split_up <- split(select_subset, f = select2$LEN_GROUP)
  sel_5 <- lapply(split_up, function(x) {x %>% sample_n(ifelse(nrow(x) < 10, nrow(x), 10))}) #change the 10 in here to change the subsample size
  sel_5 <- do.call("rbind", sel_5)
  select2_tba <- sel_5
  output<-rbind(output, select2_tba)
}

select2_tba<-subset(output, !is.na(YEAR)) #remove that very first record used to mimick the structure of the output data frame in line #2

#For group 3 (<28cm female, all years), select 10 per 2cm length group

split_up <- split(select3, f = select3$LEN_GROUP)
sel_5 <- lapply(split_up, function(x) {x %>% sample_n(ifelse(nrow(x) < 10, nrow(x), 10))}) #change the 10 in here to change the subsample size
sel_5 <- do.call("rbind", sel_5)
select3_tba <- sel_5

#For group 4 (>28cm female, divided by year), select 10 per 2cm length group

years<-unique(select4$YEAR) #pick out years
output<-select4[1,] #Create a base 'output' file
output$YEAR<-NA
for (i in years){
  select_subset<-subset(select4, YEAR==i)
  split_up <- split(select_subset, f = select4$LEN_GROUP)
  sel_5 <- lapply(split_up, function(x) {x %>% sample_n(ifelse(nrow(x) < 10, nrow(x), 10))}) #change the 10 in here to change the subsample size
  sel_5 <- do.call("rbind", sel_5)
  select4_tba <- sel_5
  output<-rbind(output, select4_tba)
}

select4_tba<-subset(output, !is.na(YEAR)) #remove that very first record used to mimic the structure of the output data frame in line #2

#Bind all the selections together to give the randomly selected list of otoliths that will be aged

otoselection <- rbind(select1_tba, select2_tba, select3_tba, select4_tba)

#Identify duplicates in the list; replace them if there are any so that you still get 10 per sex per 2cm

otoselection$DUPLICATED <- duplicated(otoselection)

table(otoselection$DUPLICATED)
