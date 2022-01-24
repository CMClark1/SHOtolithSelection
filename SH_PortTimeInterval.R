####################
# Silver Hake Otolith Selection - Port Sample Time Interval
# Caira Clark
# 21 January 2021
####################

#This script determines the time interval (quarters, trimesters, months, etc.) that should be used for port sample otolith selection. It does so using an average age-at-length by month. 

library(ROracle)
library(ggplot2)
library(dplyr)
library(tidyr)

channel <- ROracle::dbConnect(DBI::dbDriver("Oracle"), username="username", password="password", "PTRAN")

P <- dbGetQuery(channel, "select 
                   a.DATELANDED, a.AREA, 
                   b.fishlen, b.age, b.sex
                   from mfd_port_samples.gpsamples a, mfd_port_samples.gpages b 
                   where extract(YEAR from a.DATELANDED) in (2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011) 
                   and a.SPECIES=14 
                   and a.sample=b.sample
                   and b.age is not null")

P$YEAR <- as.factor(substr(P$DATELANDED, start = 1, stop = 4))

P$MONTH <- as.numeric(substr(P$DATELANDED, start = 6, stop = 7))

P <- P %>%  mutate(
  QUARTER = case_when(
    MONTH %in% c(1:3) ~ 'Q1'
    , MONTH %in% c(4:6) ~ 'Q2'
    , MONTH %in% c(7:9) ~ 'Q3'
    , MONTH %in% c(10:12) ~ 'Q4' ) )

P <- P %>% filter(AREA %in% c(440:483))

#Average age-at-length by month

ageatlength <- P %>% group_by(MONTH, FISHLEN) %>% dplyr::summarize(Mean = mean(AGE, na.rm=TRUE)) 

ageatlength$MONTH <- as.factor(ageatlength$MONTH)

ageatlength_Wide <- ageatlength %>% pivot_wider(names_from=MONTH, values_from = Mean)

ggplot(ageatlength, aes(x=FISHLEN, y=Mean, group=MONTH, colour=MONTH)) + geom_line() + ylab("Fish Length (cm)") + xlab("Mean Age")

ggplot(ageatlength, aes(x=MONTH, y=FISHLEN, size = Mean)) +
  geom_point(aes(fill= "#00AFBB"), alpha=0.5, colour = "black", pch=21) +
  ylab("Month") + xlab("Fish Length (cm)") +
  ggtitle("Survey Age at Length by Month (2002-2011)")

#Average age-at-length by quarter

aal2 <- P %>% group_by(QUARTER, FISHLEN) %>% dplyr::summarize(Mean = mean(AGE, na.rm=TRUE)) 

aal2_Wide <- aal2 %>% pivot_wider(names_from=QUARTER, values_from = Mean)

ggplot(aal2, aes(x=FISHLEN, y=Mean, group=QUARTER, colour=QUARTER)) + geom_line() + ylab("Fish Length (cm)") + xlab("Mean Age")

ggplot(aal2, aes(x=QUARTER, y=FISHLEN, size = Mean)) +
  geom_point(aes(fill= "#00AFBB"), alpha=0.5, colour = "black", pch=21) +
  ylab("Quarter") + xlab("Fish Length (cm)") +
  ggtitle("Survey Age at Length by Quarter (2002-2011)")

#Average age-at-length by half

P <- P %>%  mutate(
  HALF = case_when(
    MONTH %in% c(1:6) ~ 'H1'
    , MONTH %in% c(7:12) ~ 'H2' ) )

aal3 <- P %>% group_by(HALF, FISHLEN) %>% dplyr::summarize(Mean = mean(AGE, na.rm=TRUE)) 

aal3_Wide <- aal3 %>% pivot_wider(names_from=HALF, values_from = Mean)

ggplot(aal3, aes(x=FISHLEN, y=Mean, group=HALF, colour=HALF)) + geom_line() + ylab("Fish Length (cm)") + xlab("Mean Age")

ggplot(aal3, aes(x=HALF, y=FISHLEN, size = Mean)) +
  geom_point(aes(fill= "#00AFBB"), alpha=0.5, colour = "black", pch=21) +
  ylab("Half") + xlab("Fish Length (cm)") +
  ggtitle("Survey Age at Length by Half (2002-2011)")
       