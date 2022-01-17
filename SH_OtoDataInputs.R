library(ROracle)
library(ggplot2)
library(dplyr)
library(tidyr)
library(Mar.fleets)

channel <- ROracle::dbConnect(DBI::dbDriver("Oracle"), username="username", password="password", "PTRAN")

##Survey Data-------------------------

#Select fish with otoliths collected during missing otolith years
survey <-dbGetQuery(channel, "select a.mission, a.setno, b.fshno, b.fsex, b.flen, b.fwt, b.age from 
                    groundfish.gsinf a, groundfish.gsdet b where a.mission in ('NED2011025') 
                    and b.spec=14 and a.mission=b.mission 
                    and a.setno=b.setno and b.fshno is not null")

#Add a year column
survey$YEAR <- as.factor(substr(survey$MISSION, start = 4, stop = 7))

#Add a quarter column
survey$QUARTER <- "Q3"

#Select Scotian Shelf strata
survey <- survey %>% filter(STRAT %in% c(440:483))

#Select only sex codes 1 and 2
survey <- survey %>% filter(FSEX %in% c(1, 2))

summary(survey)

ggplot(survey, aes(x=FLEN)) + 
  geom_histogram()+
    facet_wrap(~FSEX)

##Observer Data----------------

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

isdb <- isdb %>% filter(YEAR == 2011)

isdb$MONTH <- as.numeric(substr(isdb$BOARD_DATE, start = 6, stop = 7))

isdb <- isdb %>%  mutate(
  QUARTER = case_when(
    MONTH %in% c(1:3) ~ 'Q1'
    , MONTH %in% c(4:6) ~ 'Q2'
    , MONTH %in% c(7:9) ~ 'Q3'
    , MONTH %in% c(10:12) ~ 'Q4' ) )

summary(isdb)


##Port Sample Data------------------------

port <- dbGetQuery(channel, "select 
                   a.SAMPLE, a.DATELANDED, a.AREA, 
                   b.fishlen, b.otolith, b.age, b.sex, b.weight 
                   from mfd_port_samples.gpsamples a, mfd_port_samples.gpages b 
                   where extract(YEAR from a.DATELANDED) in (2011) 
                   and a.SPECIES=14 
                   and a.sample=b.sample 
                   and b.otolith is not null")

port$YEAR <- as.factor(substr(port$SAMPLE, start = 1, stop = 4))

port$MONTH <- as.numeric(substr(port$DATELANDED, start = 6, stop = 7))

port <- port %>%  mutate(
  QUARTER = case_when(
    MONTH %in% c(1:3) ~ 'Q1'
    , MONTH %in% c(4:6) ~ 'Q2'
    , MONTH %in% c(7:9) ~ 'Q3'
    , MONTH %in% c(10:12) ~ 'Q4' ) )

port <- port %>% filter(AREA %in% c(440:483))

summary(port)


