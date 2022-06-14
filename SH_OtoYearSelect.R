####################
# SH Otoliths - Simulation Year Selection
# Caira Clark
# 14 June 2022
####################

library(ROracle)
library(ggplot2)
library(dplyr)
library(tidyr)
library(Mar.fleets)

channel <- ROracle::dbConnect(DBI::dbDriver("Oracle"), username=oracle.username, password=oracle.password, oracle.dsn)  

##Port Sample-------------------

shake <- dbGetQuery(channel, "select 
                   a.SAMPLE, a.DATELANDED, a.AREA, 
                   b.fishlen, b.otolith, b.age, b.sex, b.weight 
                   from mfd_port_samples.gpsamples a, mfd_port_samples.gpages b
                   where extract(YEAR from a.DATELANDED) in (2007, 2008, 2009, 2010, 2011) 
                   and a.SPECIES=14 
                   and a.sample=b.sample 
                   and b.otolith is not null")

shake$YR <- as.numeric(substr(shake$DATELANDED, start = 1, stop = 4))

shake$MONTH <- as.numeric(substr(shake$DATELANDED, start = 6, stop = 7))

shake <- shake %>%  mutate(
  QUARTER = case_when(
    MONTH %in% c(1:3) ~ 'Q1'
    , MONTH %in% c(4:6) ~ 'Q2'
    , MONTH %in% c(7:9) ~ 'Q3'
    , MONTH %in% c(10:12) ~ 'Q4' ) )


#Number of otoliths sampled 2007-2011 by year

table(shake$YR)

#Number of otoliths aged 2007-2011 by year

shake1 <- shake %>% filter(!is.na(AGE))

table(shake1$YR)

#Length frequency (yearly) for observer samples 2007-2011 by year

shake2 <- shake %>% filter(SEXCD_ID %in% c(1, 2)) %>% dplyr::count(YR, FISH_LENGTH, SEXCD_ID)

ggplot(shake2, aes(x=FISH_LENGTH, y=n, group=SEXCD_ID, fill=SEXCD_ID, colour=as.factor(SEXCD_ID))) +
  geom_line() +
  scale_color_manual(values=c("green", "red", "blue"), name="Sex") +
  facet_wrap(~YR) +
  ggtitle("Length Frequency of SH Otoliths 2007-2011") +
  theme_bw() +
  xlab("Fish Length (cm)") +
  ylab("Number")

#Age length scatterplot for observer samples 2007-2011 by year

ggplot(shake1, aes(x=FISH_LENGTH, y=FISH_AGE, group=SEXCD_ID, colour=as.factor(SEXCD_ID))) +
  geom_point() +
  scale_color_manual(values=c("green", "red", "blue"), name="Sex") +
  facet_wrap(~YR) +
  ggtitle("Age-Length of SH Otoliths 2007-2011") +
  theme_bw() +
  xlab("Fish Length (cm)") +
  ylab("Fish Age (years)")

#Observer Sample

shake <- dbGetQuery(channel, "select
                    t.trip, t.board_date, f.nafarea_id,
                    f1.fish_length, f1.sexcd_id, f1.fish_age,
                    a.latitude, a.longitude, a.pntcd_id
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
                    and a.pntcd_id = 2
                    and f1.otolith_collected in ('S','Y')
                    and c.speccd_id = 14")

shake$YR <- as.numeric(substr(shake$BOARD_DATE, start = 1, stop = 4))

shake<- shake %>% filter(YR %in% c(2007:2011))

shake$MONTH <- as.numeric(substr(shake$BOARD_DATE, start = 6, stop = 7))

shake <- shake %>%  mutate(
  QUARTER = case_when(
    MONTH %in% c(1:3) ~ 'Q1'
    , MONTH %in% c(4:6) ~ 'Q2'
    , MONTH %in% c(7:9) ~ 'Q3'
    , MONTH %in% c(10:12) ~ 'Q4' ) )

shake$LONGITUDE <- shake$LONGITUDE*-1

shake <- Mar.utils::identify_area(df=shake, lat.field = "LATITUDE", lon.field = "LONGITUDE",
                                        agg.poly.shp = "S:/Science/Population Ecology/Silver Hake/2021/Assessment/data/shapefiles/SH_fishing_areas.shp",
                                        agg.poly.field = "mgmt_area")

#Number of otoliths sampled 2007-2011 by year

table(shake$YR)

#Number of otoliths aged 2007-2011 by year

shake1 <- shake %>% filter(!is.na(FISH_AGE))

table(shake1$YR)

#Length frequency (yearly) for observer samples 2007-2011 by year

shake2 <- shake %>% filter(SEXCD_ID %in% c(1, 2)) %>% dplyr::count(YR, FISH_LENGTH, SEXCD_ID)

ggplot(shake2, aes(x=FISH_LENGTH, y=n, group=SEXCD_ID, fill=SEXCD_ID, colour=as.factor(SEXCD_ID))) +
  geom_line() +
  scale_color_manual(values=c("green", "red", "blue"), name="Sex") +
  facet_wrap(~YR) +
  ggtitle("Length Frequency of SH Otoliths 2007-2011") +
  theme_bw() +
  xlab("Fish Length (cm)") +
  ylab("Number")

#Age length scatterplot for observer samples 2007-2011 by year

ggplot(shake1, aes(x=FISH_LENGTH, y=FISH_AGE, group=SEXCD_ID, colour=as.factor(SEXCD_ID))) +
  geom_point() +
  scale_color_manual(values=c("green", "red", "blue"), name="Sex") +
  facet_wrap(~YR) +
  ggtitle("Age-Length of SH Otoliths 2007-2011") +
  theme_bw() +
  xlab("Fish Length (cm)") +
  ylab("Fish Age (years)")



