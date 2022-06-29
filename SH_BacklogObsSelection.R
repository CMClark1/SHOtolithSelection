####################
# Silver Hake Otolith Selection - Observer Sample Backlog
# Caira Clark
# 20 June 2022
####################

library(ROracle)
library(ggplot2)
library(dplyr)
library(tidyr)

channel <- ROracle::dbConnect(DBI::dbDriver("Oracle"), username=oracle.username, password=oracle.password, oracle.dsn)  

#INDIVIDUAL OTOLITHS -------------------------------------------

##LOAD OBSERVER SAMPLE DATA-------------------------

#Select fish with otoliths collected during missing otolith years

isdb <- dbGetQuery(channel, "select
                    t.trip, t.board_date,
                    f.nafarea_id,
                    f1.fish_length, f1.fish_weight, f1.sexcd_id, f1.fish_age
                    from observer.isfishsets f, 
                      observer.istrips t,
                      observer.issettypecodes s,
                      observer.iscatches c,
                      observer.isfish f1, 
                      observer.issetprofile a
                    where t.trip_id = f.trip_id
                    and t.tripcd_id = 14
                    and f.setcd_id = s.setcd_id
                    and f.fishset_id = c.fishset_id
                    and c.catch_id = f1.catch_id
                    and a.fishset_id = f.fishset_id
                    and c.speccd_id = 14")

isdb$YEAR <- as.numeric(substr(isdb$BOARD_DATE, start = 1, stop = 4))

isdb <- isdb %>% filter(YEAR %in% c(2016:2021))

isdb$MONTH <- as.numeric(substr(isdb$BOARD_DATE, start = 6, stop = 7))

isdb <- isdb %>%  mutate(
  QUARTER = case_when(
    MONTH %in% c(1:6) ~ 'H1'
    , MONTH %in% c(7:9) ~ 'Q3'
    , MONTH %in% c(10:12) ~ 'Q4' ) )

P_copy <- isdb

#CREATE SELECTION GROUPS--------------

#Create four selection groups (1 is <26 and male, 2 is >26 and male, 3 is <28 and female, 4 is >28 and female)
P_copy <- P_copy %>%  mutate(
  GROUP = case_when(
    FISH_LENGTH < 26 & SEXCD_ID == 1 ~ 1
    , FISH_LENGTH >= 26 & SEXCD_ID == 1 ~ 2
    , FISH_LENGTH < 28 & SEXCD_ID == 2 ~ 3
    , FISH_LENGTH >= 28 & SEXCD_ID == 2 ~ 4))

#Group data into 2cm length intervals
P_copy$LEN_GROUP <- cut(P_copy$FISH_LENGTH, breaks = seq(1,60,2), labels=seq(2,58,2))

#Divide data into four dataframes based on Quarters

quarter1 <- P_copy %>% filter(QUARTER=="H1")
quarter3 <- P_copy %>% filter(QUARTER=="Q3")
quarter4 <- P_copy %>% filter(QUARTER=="Q4")

#FOR QUARTER 1

#Divide data into four data frames based on selection group

select1 <- quarter1 %>% filter(GROUP==1)
select2 <- quarter1 %>% filter(GROUP==2)
select3 <- quarter1 %>% filter(GROUP==3)
select4 <- quarter1 %>% filter(GROUP==4)

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

otoselection_Q1 <- rbind(select1_tba, select2_tba, select3_tba, select4_tba)

#FOR QUARTER 3

#Divide data into four data frames based on selection group

select1 <- quarter3 %>% filter(GROUP==1)
select2 <- quarter3 %>% filter(GROUP==2)
select3 <- quarter3 %>% filter(GROUP==3)
select4 <- quarter3 %>% filter(GROUP==4)

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

otoselection_Q3 <- rbind(select1_tba, select2_tba, select3_tba, select4_tba)

#FOR QUARTER 4

#Divide data into four data frames based on selection group

select1 <- quarter4 %>% filter(GROUP==1)
select2 <- quarter4 %>% filter(GROUP==2)
select3 <- quarter4 %>% filter(GROUP==3)
select4 <- quarter4 %>% filter(GROUP==4)

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

#Bind all the selections from that quarter together

otoselection_Q4 <- rbind(select1_tba, select2_tba, select3_tba, select4_tba)

#Bind the selections for all four quarters together to give the randomly selected list of otoliths that will be aged

otoselection <- rbind(otoselection_Q1, otoselection_Q3, otoselection_Q4)

str(otoselection)

#Identify duplicates in the list; replace them if there are any so that you still get 10 per sex per 2cm

otoselection$DUPLICATED <- duplicated(otoselection)

table(otoselection$DUPLICATED)


#WHOLE OBSERVER SAMPLES -------------------------------------------


##Port Sample Data-------------------------

#Select fish with otoliths collected during missing otolith years
isdb <- dbGetQuery(channel, "select
                    t.trip, t.board_date,
                    f.nafarea_id, f.fishset_id,
                    f1.fish_length, f1.fish_weight, f1.sexcd_id, f1.fish_age
                    from observer.isfishsets f, 
                      observer.istrips t,
                      observer.issettypecodes s,
                      observer.iscatches c,
                      observer.isfish f1, 
                      observer.issetprofile a
                    where t.trip_id = f.trip_id
                    and t.tripcd_id = 14
                    and f.setcd_id = s.setcd_id
                    and f.fishset_id = c.fishset_id
                    and c.catch_id = f1.catch_id
                    and a.fishset_id = f.fishset_id
                    and c.speccd_id = 14")

isdb$YEAR <- as.numeric(substr(isdb$BOARD_DATE, start = 1, stop = 4))

isdb <- isdb %>% filter(YEAR %in% c(2016:2021))

isdb$MONTH <- as.numeric(substr(isdb$BOARD_DATE, start = 6, stop = 7))

isdb <- isdb %>%  mutate(
  QUARTER = case_when(
    MONTH %in% c(1:6) ~ 'H1'
    , MONTH %in% c(7:9) ~ 'Q3'
    , MONTH %in% c(10:12) ~ 'Q4' ) )

P <- isdb

P$n <- 1
P2 <- P %>% 
  group_by(YEAR, QUARTER, FISHSET_ID) %>% 
  summarise(n = n()) %>% 
  count(FISHSET_ID) %>% 
  group_by(YEAR, QUARTER) %>% 
  summarise(n=n()) %>%
  mutate(n2 = ifelse(n>5, 5,
                     ifelse(n<=5, n, NA)))

