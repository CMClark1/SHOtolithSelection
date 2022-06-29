####################
# Silver Hake Otolith Selection - Observer Otolith Supplementation
# Caira Clark & Ellen MacEachern
# 22 June 2022
####################

#This script supplements silver hake port sample age keys with observer otoliths to ensure that at least 10 fish per sex per 2cm length bin are aged.

library(ROracle)
library(ggplot2)
library(dplyr)
library(tidyr)

channel <- ROracle::dbConnect(DBI::dbDriver("Oracle"), username=oracle.username, password=oracle.password, oracle.dsn)  

###Generate summary of port samples aged per sex per length bin per quarter.

P <- dbGetQuery(channel, "select 
                   a.SAMPLE, a.DATELANDED, a.AREA, 
                   b.fishlen, b.otolith, b.age, b.sex
                   from mfd_port_samples.gpsamples a, mfd_port_samples.gpages b 
                   where extract(YEAR from a.DATELANDED) in (2011) 
                   and a.SPECIES=14 
                   and a.sample=b.sample 
                   and b.otolith is not null")

P$YEAR <- as.factor(substr(P$SAMPLE, start = 1, stop = 4))

P$MONTH <- as.numeric(substr(P$DATELANDED, start = 6, stop = 7))

P <- P %>%  mutate(
  QUARTER = case_when(
    MONTH %in% c(1:6) ~ 'H1'
    , MONTH %in% c(7:9) ~ 'Q3'
    , MONTH %in% c(10:12) ~ 'Q4' ) )

P <- P %>% filter(AREA %in% c(440:483)) %>% filter(!is.na(AGE))

P$LENGTH_BIN<-with(P, cut(FISHLEN, seq(10,50,2))) #Add 2cm length bins

P_summary <- P %>% group_by(QUARTER, LENGTH_BIN, SEX) %>% tally()

xtabs(~LENGTH_BIN+QUARTER+SEX, P)

###Figure out which observer otoliths are needed to supplement the above keys to 10 fish aged per length bin

O <- dbGetQuery(channel, "select
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

O$YEAR <- as.numeric(substr(O$BOARD_DATE, start = 1, stop = 4))

O <- O %>% filter(YEAR %in% c(2011))

O$MONTH <- as.numeric(substr(O$BOARD_DATE, start = 6, stop = 7))

O <- O %>%  mutate(
  QUARTER = case_when(
    MONTH %in% c(1:6) ~ 'H1'
    , MONTH %in% c(7:9) ~ 'Q3'
    , MONTH %in% c(10:12) ~ 'Q4' ) )

O$LENGTH_BIN<-with(O, cut(FISH_LENGTH, seq(10,50,2))) #Add 2cm length bins

O_summary <- O %>% group_by(QUARTER, LENGTH_BIN, SEXCD_ID) %>% tally()


#Merge the port sample (aged) summary and the observer sample (unaged) summary, and then see if for sex/length bins with <10 otoliths, there are observer otoliths available.

head(P_summary)
head(O_summary)

O_summary <- rename(O_summary, SEX = SEXCD_ID)
O_summary <- rename(O_summary, nO = n)
P_summary$SEX <- as.integer(P_summary$SEX)

joined <- full_join(P_summary, O_summary) 

joined2 <- joined %>% filter(n<=10) %>% mutate(dif = 10-n) #These are the port sample length bins that could be supplemented with observer otoliths. The dif column is how many more observer otoliths would be aged to equal 10 per sex per 2cm in the port samples.

sum(joined2$dif) #This is how many additional observer samples would be aged to get 10 per sex per 2cm for commercial otoliths



