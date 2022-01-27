####################
# Backlog Silver Hake Otolith Selection with Length Grouping
# Caira Clark
# 5 November 2021
####################

#Part 1. Load Data---------------

library(ROracle)
library(ggplot2)
library(kableExtra)
library(shapefiles)
library(tidyr)
require(dplyr)

##SURVEY-------------------------

##Load survey data and select only Scotian Shelf strata used in SH assessment

channel <- ROracle::dbConnect(DBI::dbDriver("Oracle"), username=oracle.username, password=oracle.password, oracle.dsn)  

#Select fish with otoliths collected during missing otolith years
survey <-dbGetQuery(channel, "select a.mission, a.area, a.strat, a.slat, a.slong, a.setno, b.fshno, b.fsex, b.flen, b.fwt, b.fmat from groundfish.gsinf a, groundfish.gsdet b where a.mission in ('NED2015017', 'NED2016016', 'NED2016116', 'NED2017020', 'TEL2018023', 'NED2019030', 'NED2020025') and b.spec=14 and b.agmat='1' and a.mission=b.mission and a.setno=b.setno and b.fshno is not null")

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

survey_copy <- survey_copy %>%  mutate(
  GROUP = case_when(
    FLEN < 26 & FSEX == 1 ~ 1
    , FLEN >= 26 & FSEX == 1 ~ 2
    , FLEN < 26 & FSEX == 2 ~ 3
    , FLEN >= 26 & FSEX == 2 ~ 4))

survey_copy$LEN_GROUP <- cut(survey_copy$FLEN, breaks = seq(1,60,2), labels=seq(2,58,2))


##OBSERVER-----------------------

isdb <- dbGetQuery(channel, "select
                    t.trip, t.board_date, f.nafarea_id,
                    f1.fish_length, f1.sexcd_id,
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

isdb$YR <- as.numeric(substr(isdb$BOARD_DATE, start = 1, stop = 4))

isdb <- isdb %>% filter(YR %in% c(2012:2020))

isdb$MONTH <- as.numeric(substr(isdb$BOARD_DATE, start = 6, stop = 7))

isdb <- isdb %>%  mutate(
  QUARTER = case_when(
    MONTH %in% c(1:3) ~ 'Q1'
    , MONTH %in% c(4:6) ~ 'Q2'
    , MONTH %in% c(7:9) ~ 'Q3'
    , MONTH %in% c(10:12) ~ 'Q4' ) )

isdb$LONGITUDE <- isdb$LONGITUDE*-1

isdbTweaked <- Mar.utils::identify_area(df=isdb, lat.field = "LATITUDE", lon.field = "LONGITUDE",
                                        agg.poly.shp = "S:/Science/Population Ecology/Silver Hake/2021/Assessment/data/shapefiles/SH_fishing_areas.shp",
                                        agg.poly.field = "mgmt_area")

isdb <- isdbTweaked

isdb <- isdb %>%  
  filter(SEXCD_ID %in% c(1,2)) %>%
  mutate(
    LOCATION = case_when(
      mgmt_area == "<outside known areas>" ~ 'Scotian Shelf'
      , mgmt_area %in% c("Emerald", "LaHave") ~ 'Basins'
      , mgmt_area == "SMGL" ~ 'Small Mesh Gear Line'))

isdb$LEN_GROUP <- cut(isdb$FISH_LENGTH, breaks = seq(1,48,2), labels=seq(2,46,2))

isdb <- isdb %>%  mutate(
  GROUP = case_when(
    FISH_LENGTH < 26 & SEXCD_ID == 1 ~ 1
    , FISH_LENGTH >= 26 & SEXCD_ID == 1 ~ 2
    , FISH_LENGTH < 26 & SEXCD_ID == 2 ~ 3
    , FISH_LENGTH >= 26 & SEXCD_ID == 2 ~ 4))

isdb_copy <- dplyr::count(isdb, YR, SEXCD_ID, GROUP)



##PORT SAMPLE----------------------

port <- dbGetQuery(channel, "select a.SAMPLE, a.DATELANDED, a.SPECIES, a.area, a.fishing, b.fishlen, b.otolith, b.age, b.sex, a.remark from mfd_port_samples.gpsamples a, mfd_port_samples.gpages b where extract(YEAR from a.DATELANDED) in (2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020) and a.SPECIES=14 and a.sample=b.sample AND b.OTOLITH is not null")

port$YEAR <- as.factor(substr(port$SAMPLE, start = 1, stop = 4))

port$MONTH <- as.numeric(substr(port$DATELANDED, start = 6, stop = 7))

port <- port %>%  mutate(
  QUARTER = case_when(
    MONTH %in% c(1:3) ~ 'Q1'
    , MONTH %in% c(4:6) ~ 'Q2'
    , MONTH %in% c(7:9) ~ 'Q3'
    , MONTH %in% c(10:12) ~ 'Q4' ) )

port <- port %>% filter(AREA %in% c(440:483))

port <- port %>%
  mutate(., LOCATION = with(., case_when(
    (grepl('Basin', REMARK)) ~ 'Basin',
    (grepl('Bsin', REMARK)) ~ 'Basin',
    (grepl('Patch', REMARK)) ~ 'Basin',
    (grepl('South of Emerald Bank', REMARK)) ~ 'SMGL',
    (grepl('Northeast Channel', REMARK)) ~ 'Shelf',
    (grepl('southeast of Western', REMARK)) ~ 'SMGL', 
    (grepl('Bank', REMARK)) ~ 'Shelf',
    (grepl('Bonnecamps', REMARK)) ~ 'SMGL',
    is.na(REMARK) ~ 'Unknown',
    TRUE ~ 'Unknown'
  )))

port$LEN_GROUP <- cut(port$FISHLEN, breaks = seq(1,46,2), labels=seq(2,45,2))

port <- port %>%  mutate(
  GROUP = case_when(
    FISHLEN < 26 & SEX == 1 ~ 1
    , FISHLEN >= 26 & SEX == 1 ~ 2
    , FISHLEN < 26 & SEX == 2 ~ 3
    , FISHLEN >= 26 & SEX == 2 ~ 4))

#Part 2. Otolith Summary---------------

##SURVEY--------------

#Figure 1. Length frequency of unaged survey otoliths (2015-2020), 1cm length grouping.

survey1cm <- survey_copy%>% filter(FSEX %in% c(1, 2)) %>% dplyr::count(YEAR, FLEN, FSEX)

ggplot(survey1cm, aes(x=FLEN, y=n, group=FSEX, fill=FSEX, colour=as.factor(FSEX))) +
  geom_line() +
  scale_color_manual(values=c("green", "red", "blue"), name="Sex") +
  facet_wrap(~YEAR, scales = "free_y") +
  ggtitle("Scotian Shelf Survey Otoliths 2015-2020, 1cm length grouping") +
  theme_bw() +
  xlab("Fish Length (cm)") +
  ylab("Number") 

#Figure 2. Length frequency of unaged survey otoliths (2015-2020), 2cm length grouping.

survey2cm <- survey_copy %>% filter(FSEX %in% c(1,2)) %>% dplyr:: count(YEAR, LEN_GROUP, FSEX)
survey2cm <- na.omit(survey2cm)
survey2cm$LEN_GROUP <- as.numeric(survey2cm$LEN_GROUP)

ggplot(survey2cm, aes(x=LEN_GROUP, y=n, group=FSEX, fill=FSEX, colour=as.factor(FSEX))) +
  geom_line() +
  scale_color_manual(values=c("green", "red", "blue"), name="Sex") +
  facet_wrap(~YEAR, scales = "free_y") +
  ggtitle("Scotian Shelf Survey Otoliths 2015-2020, 2cm length grouping") +
  theme_bw() +
  xlab("Fish Length (cm)") +
  ylab("Number")

#Table 1. All unaged survey otoliths (2015-2020), categorized by sex and length group.

copy <- dplyr::count(survey_copy, YEAR, FSEX, GROUP)

copy2 <- copy %>% pivot_wider(names_from = c(FSEX, GROUP), values_from = n) %>%
  dplyr::mutate("Year Total" = rowSums(across(where(is.numeric))))

df <- c(NA, sum(copy2$"1_1"), sum(copy2$"1_2"), sum(copy2$"2_3"), sum(copy2$"2_4"), sum(copy2$"Year Total"))

copy2 <- as.data.frame(rbind(copy2, df))

copy2$YEAR <- as.character(copy2$YEAR)

copy2[7, 1] <- "Total"

colnames(copy2) <- c("Year", "Males<26cm", "Males>=26cm", "Females<28cm", "Females>=26cm", "Total")

copy2 %>%
  kbl() %>%
  kable_styling()

#Table 2. All unaged survey otoliths (2015-2020), categorized by group selection criteria and length grouping (1cm or 2cm). For males <26cm and females <28cm, all unaged years were grouped and a selection of 10 per sex per cm is used. For males >26cm and females >26cm, years are not grouped and a selection of 10 per sex per cm is used.

low <- survey_copy %>% filter(GROUP %in% c(1,3))
low2 <- count(low, FSEX, FLEN)
low2$n2 <- ifelse(low2$n > 10, 10, low2$n)

#Deal with high length samples - grouped by 2cm
high <- survey_copy %>% filter(GROUP %in% c(2,4))
high2 <- count(high, YEAR, FSEX, FLEN)
high2$n2 <- ifelse(high2$n > 10, 10, high2$n)

old <- as.data.frame(c(sum(low2$n), sum(low2$n2), sum(high2$n), sum(high2$n2)))
rownames(old) <- c("All M<26 F<28", "Select M<26 F<28", "All M>26 F>28", "Select M>26 F>28")
colnames(old) <- c("Num 1cm")

#2cm length grouping

#Column for 2cm length groups
survey_copy$LEN_GROUP <- cut(survey_copy$FLEN, breaks = seq(1,60,2), labels=seq(2,58,2))

#Low length samples - grouped by 2cm
low <- survey_copy %>% filter(GROUP %in% c(1,3))
low2 <- count(low, FSEX, LEN_GROUP)
low2$n2 <- ifelse(low2$n > 10, 10, low2$n)

#High length samples - grouped by 2cm
high <- survey_copy %>% filter(GROUP %in% c(2,4))
high2 <- count(high, YEAR, FSEX, LEN_GROUP)
high2$n2 <- ifelse(high2$n > 10, 10, high2$n)

new <- as.data.frame(c(sum(low2$n), sum(low2$n2), sum(high2$n), sum(high2$n2)))
rownames(new) <- c("All M<26 F<28", "Select M<26 F<28", "All M>26 F>28", "Select M>26 F>28")
colnames(new) <- c("Num 2cm")

#join tables

selectoto <- cbind(old, new) #This shows how many otoliths would be aged with those selection requirements.

selectoto %>%
  kbl() %>%
  kable_styling()

##OBSERVER---------------

#Figure 3. Length frequency of unaged observer otoliths (2012-2020), 1cm length grouping.

isdb1cm <- isdb %>% filter(SEXCD_ID %in% c(1, 2)) %>% dplyr::count(YR, FISH_LENGTH, SEXCD_ID)

ggplot(isdb1cm, aes(x=FISH_LENGTH, y=n, group=SEXCD_ID, fill=SEXCD_ID, colour=as.factor(SEXCD_ID))) +
  geom_line() +
  scale_color_manual(values=c("green", "red", "blue"), name="Sex") +
  facet_wrap(~YR, scales = "free_y") +
  ggtitle("Scotian Shelf Observer Otoliths 2012-2020, 1cm length grouping") +
  theme_bw() +
  xlab("Fish Length (cm)") +
  ylab("Number")

#Figure 4. Length frequency of unaged observer otoliths (2012-2020), 2cm length grouping.

isdb2cm <- isdb %>% filter(SEXCD_ID %in% c(1,2)) %>% dplyr:: count(YR, LEN_GROUP, SEXCD_ID)
isdb2cm$LEN_GROUP <- as.numeric(isdb2cm$LEN_GROUP)

ggplot(isdb2cm, aes(x=LEN_GROUP, y=n, group=SEXCD_ID, fill=SEXCD_ID, colour=as.factor(SEXCD_ID))) +
  geom_line() +
  scale_color_manual(values=c("green", "red", "blue"), name="Sex") +
  facet_wrap(~YR, scales = "free_y") +
  ggtitle("Scotian Shelf Observer Otoliths 2012-2020, 2cm length grouping") +
  theme_bw() +
  xlab("Fish Length (cm)") +
  ylab("Number")

#Table 3. All unaged observer otoliths (2012-2020), categorized by sex and length group.

isdb_copy2 <- isdb_copy %>% pivot_wider(names_from = c(SEXCD_ID, GROUP), values_from = n) %>%
  select(c('1_1', '1_2', '2_3', '2_4')) %>%
  dplyr::mutate("Year Total" = rowSums(across(where(is.numeric))))

isdb_copy2 <- as.data.frame(isdb_copy2)

df <- c(sum(isdb_copy2$"1_1"), sum(isdb_copy2$"1_2"), sum(isdb_copy2$"2_3"), sum(isdb_copy2$"2_4"), sum(isdb_copy2$"Year Total"))

isdb_copy2 <- as.data.frame(rbind(isdb_copy2, df))

rownames(isdb_copy2) <- c('2012', '2013', '2014', '2015', '2019', '2020', 'Total')

colnames(isdb_copy2) <- c("Males<26cm", "Males>=26cm", "Females<28cm", "Females>=26cm", "Total")

isdb_copy2 %>%
  kbl() %>%
  kable_styling()

#Table 4. All unaged observer otoliths (2012-2020), categorized by group selection criteria and length grouping (1cm or 2cm). For males <26cm and females <28cm, all unaged years were grouped and a selection of 10 per sex per cm is used. For males >26cm and females >26cm, years are not grouped and a selection of 10 per sex per cm is used.

#1cm length grouping

low <- isdb %>% filter(GROUP %in% c(1,3))
low2 <- count(low, SEXCD_ID, FISH_LENGTH)
low2$n2 <- ifelse(low2$n > 10, 10, low2$n)

#Deal with high length samples - grouped by 2cm
high <- isdb %>% filter(GROUP %in% c(2,4))
high2 <- count(high, YR, SEXCD_ID, FISH_LENGTH)
high2$n2 <- ifelse(high2$n > 10, 10, high2$n)

old <- as.data.frame(c(sum(low2$n), sum(low2$n2), sum(high2$n), sum(high2$n2)))
rownames(old) <- c("All M<26 F<28", "Select M<26 F<28", "All M>26 F>28", "Select M>26 F>28")
colnames(old) <- c("Num 1cm")

#2cm length grouping

#Column for 2cm length groups

isdb$LEN_GROUP <- cut(isdb$FISH_LENGTH, breaks = seq(1,60,2), labels=seq(2,58,2))

#Low length samples - grouped by 2cm
low <- isdb %>% filter(GROUP %in% c(1,3))
low2 <- count(low, SEXCD_ID, LEN_GROUP)
low2$n2 <- ifelse(low2$n > 10, 10, low2$n)

#High length samples - grouped by 2cm
high <- isdb %>% filter(GROUP %in% c(2,4))
high2 <- count(high, YR, SEXCD_ID, LEN_GROUP)
high2$n2 <- ifelse(high2$n > 10, 10, high2$n)

new <- as.data.frame(c(sum(low2$n), sum(low2$n2), sum(high2$n), sum(high2$n2)))
rownames(new) <- c("All M<26 F<28", "Select M<26 F<28", "All M>26 F>28", "Select M>26 F>28")
colnames(new) <- c("Num 2cm")

#join tables

isdboto <- cbind(old, new) #This shows how many otoliths would be aged with those selection requirements.

isdboto %>%
  kbl() %>%
  kable_styling()

##PORT SAMPLE------------

#Figure 5. Length frequency of unaged port sample otoliths (2012-2020), 1cm length grouping.

port1cm <- port %>% filter(SEX %in% c(1, 2)) %>% dplyr::count(YEAR, FISHLEN, SEX)

ggplot(port1cm, aes(x=FISHLEN, y=n, group=SEX, fill=SEX, colour=as.factor(SEX))) +
  geom_line() +
  scale_color_manual(values=c("green", "red", "blue"), name="Sex") +
  facet_wrap(~YEAR, scales = "free_y") +
  ggtitle("Scotian Shelf Port Sample Otoliths 2012-2020, 1cm length grouping") +
  theme_bw() +
  xlab("Fish Length (cm)") +
  ylab("Number")

#Figure 6. Length frequency of unaged port sample otoliths (2012-2020), 2cm length grouping.

port2cm <- port %>% filter(SEX %in% c(1,2)) %>% dplyr:: count(YEAR, LEN_GROUP, SEX)
port2cm$LEN_GROUP <- as.numeric(port2cm$LEN_GROUP)

ggplot(port2cm, aes(x=LEN_GROUP, y=n, group=SEX, fill=SEX, colour=as.factor(SEX))) +
  geom_line() +
  scale_color_manual(values=c("green", "red", "blue"), name="Sex") +
  facet_wrap(~YEAR, scales = "free_y") +
  ggtitle("Scotian Shelf Port Sample Otoliths 2012-2020, 2cm length grouping") +
  theme_bw() +
  xlab("Fish Length (cm)") +
  ylab("Number")

#Table 5. All unaged port sample otoliths (2012-2020), categorized by sex and length group.

port_copy <- dplyr::count(port, YEAR, SEX, GROUP)

port_copy2 <- port_copy %>% pivot_wider(names_from = c(SEX, GROUP), values_from = n) %>%
  dplyr::mutate("Year Total" = rowSums(across(where(is.numeric))))

df <- c(NA, sum(port_copy2$"1_1"), sum(port_copy2$"1_2"), sum(port_copy2$"2_3"), sum(port_copy2$"2_4"), sum(port_copy2$"Year Total"))

port_copy2 <- as.data.frame(rbind(port_copy2, df))

port_copy2$YEAR <- as.character(port_copy2$YEAR)

port_copy2[10, 1] <- "Total"

colnames(port_copy2) <- c("Year", "Males<26cm", "Males>=26cm", "Females<28cm", "Females>=26cm", "Total")

port_copy2 %>%
  kbl() %>%
  kable_styling()

#Table 6. All unaged port sample otoliths (2012-2020), categorized by group selection criteria and length grouping (1cm or 2cm). For males <26cm and females <28cm, all unaged years were grouped and a selection of 10 per sex per cm is used. For males >26cm and females >26cm, years are not grouped and a selection of 10 per sex per cm is used.

#1cm length grouping

low <- port %>% filter(GROUP %in% c(1,3))
low2 <- count(low, SEX, FISHLEN)
low2$n2 <- ifelse(low2$n > 10, 10, low2$n)

#Deal with high length samples - grouped by 2cm
high <- port %>% filter(GROUP %in% c(2,4))
high2 <- count(high, YEAR, SEX, FISHLEN)
high2$n2 <- ifelse(high2$n > 10, 10, high2$n)

old <- as.data.frame(c(sum(low2$n), sum(low2$n2), sum(high2$n), sum(high2$n2)))
rownames(old) <- c("All M<26 F<28", "Select M<26 F<28", "All M>26 F>28", "Select M>26 F>28")
colnames(old) <- c("Num 1cm")

#2cm length grouping

#Column for 2cm length groups

port$LEN_GROUP <- cut(port$FISHLEN, breaks = seq(1,60,2), labels=seq(2,58,2))

#Low length samples - grouped by 2cm
low <- port %>% filter(GROUP %in% c(1,3))
low2 <- count(low, SEX, LEN_GROUP)
low2$n2 <- ifelse(low2$n > 10, 10, low2$n)

#High length samples - grouped by 2cm
high <- port %>% filter(GROUP %in% c(2,4))
high2 <- count(high, YEAR, SEX, LEN_GROUP)
high2$n2 <- ifelse(high2$n > 10, 10, high2$n)

new <- as.data.frame(c(sum(low2$n), sum(low2$n2), sum(high2$n), sum(high2$n2)))
rownames(new) <- c("All M<26 F<28", "Select M<26 F<28", "All M>26 F>28", "Select M>26 F>28")
colnames(new) <- c("Num 2cm")

#join tables

portoto <- cbind(old, new) #This shows how many otoliths would be aged with those selection requirements.

portoto %>%
  kbl() %>%
  kable_styling()