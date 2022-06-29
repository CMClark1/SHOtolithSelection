####################
# Silver Hake Otolith Selection - Observer Sample by Individual
# Caira Clark & Tim Barrett
# 20 June 2022
####################

library(ROracle)
library(ggplot2)
library(dplyr)
library(tidyr)

channel <- ROracle::dbConnect(DBI::dbDriver("Oracle"), username=oracle.username, password=oracle.password, oracle.dsn)  

##Observer Sample Data-------------------------

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

isdb <- isdb %>% filter(YEAR == 2011)

isdb$MONTH <- as.numeric(substr(isdb$BOARD_DATE, start = 6, stop = 7))

isdb <- isdb %>%  mutate(
  QUARTER = case_when(
    MONTH %in% c(1:6) ~ 'H1'
    , MONTH %in% c(7:9) ~ 'Q3'
    , MONTH %in% c(10:12) ~ 'Q4' ) )

P <- isdb %>% filter(QUARTER == "H1")


sex <- 2 #Selects sex (1 male, 2 female)
A <- P[!is.na(P$FISH_AGE) & !is.na(P$FISH_LENGTH) & P$SEXCD_ID == sex,] #Clean data - no NAs for age or length
nrow(A)
hist(A$FISH_LENGTH)
maxage <- max(A$FISH_AGE)
lmax <- max(A$FISH_LENGTH)
lmin <- min(A$FISH_LENGTH)

#Choose bin size here
#lbins <- lmin:lmax #Sets bins to 1cm
lbins <- seq(round(lmin/2-0.1)*2,round(lmax/2-0.1)*2,2) #Sets bins to 2cm

nperbin <- c(1:20) #Sets number per bin (e.g. 2 per 2cm for the selected sex)

binlist <- list()

for(l in 1:length(nperbin)){#loop1
  
  nsim <- 1000 #Sets number of simulations
  
  set.seed(86)
  list_alk <- list()
  for(i in 1:nsim){
    ALK <- as.data.frame(matrix(0,nrow=length(lbins), ncol=maxage+1))
    rownames(ALK)=lbins
    colnames(ALK)=0:maxage
    for(j in 1:(nrow(ALK)-2)){
      for(k in 1:nperbin[l]){
        A_sub <- A[A$FISH_LENGTH>=as.numeric(rownames(ALK)[j]) & A$FISH_LENGTH<as.numeric(rownames(ALK)[j+1]),]
        if(nrow(A_sub)!=0){
          rowid <- sample(1:nrow(A_sub), 1)
          ALK[j,colnames(ALK)==A_sub$FISH_AGE[rowid]] <- ALK[j,colnames(ALK)==A_sub$FISH_AGE[rowid]]+1
        }
        A_sub <- A[A$FISH_LENGTH>=as.numeric(rownames(ALK)[j]) & A$FISH_LENGTH<as.numeric(rownames(ALK)[j+1]),]
        if(nrow(A_sub)==0){ #If there is a length bin with missing age, this takes the ages from the length bin above and below
          A_sub <- A[A$FISH_LENGTH>=(as.numeric(rownames(ALK)[j])-1) & A$FISH_LENGTH<(as.numeric(rownames(ALK)[j+1])+1),]
          rowid <- sample(1:nrow(A_sub), 1)
          if(nrow(A_sub)!=0){ ##################Added this here###############################
            ALK[j,colnames(ALK)==A_sub$FISH_AGE[rowid]] <- ALK[j,colnames(ALK)==A_sub$FISH_AGE[rowid]]+1
          }
        }
        if(nrow(A_sub)==0){ #nrow(A_sub) is still zero then look 2 length bins above and below
          A_sub <- A[A$FISH_LENGTH>=(as.numeric(rownames(ALK)[j])-2) & A$FISH_LENGTH<(as.numeric(rownames(ALK)[j+1])+2),]
          rowid <- sample(1:nrow(A_sub), 1)
          ALK[j,colnames(ALK)==A_sub$FISH_AGE[rowid]] <- ALK[j,colnames(ALK)==A_sub$FISH_AGE[rowid]]+1
        }
      }
    }  
    for(j in nrow(ALK)){ # for last row of ALK
      for(k in 1:nperbin[l]){
        A_sub <- A[A$FISH_LENGTH>=as.numeric(rownames(ALK)[j]),]
        rowid <- sample(1:nrow(A_sub), 1)
        ALK[j,A_sub$FISH_AGE[rowid]] <- ALK[j,A_sub$FISH_AGE[rowid]]+1
      } 
    }
    list_alk[[i]] <- ALK  
  }
  
  #list_alk is a list with nsim ALKs in it
  
  #get LFdist
  
  LF0 <- P[!is.na(P$FISH_LENGTH) & P$SEX == sex,]
  
  LF <- data.frame(FISH_LENGTH=lbins,COUNT=0)
  for(i in 1:nrow(LF0)){
    obsl <- LF0$FISH_LENGTH[i]
    id <- max(which(obsl-lbins >= 0))
    LF$COUNT[LF$FISH_LENGTH==lbins[id]] <-  LF$COUNT[LF$FISH_LENGTH==lbins[id]]+1
  }
  
  CAA <- as.data.frame(matrix(0,nrow=nsim, ncol=maxage+1))
  colnames(CAA) <- 0:maxage
  
  for(i in 1:nsim){
    ALKp <- list_alk[[i]]/nperbin[l]
    CAA[i,] <- colSums(LF$COUNT*ALKp)
  }
  
  binlist[[l]]<-CAA
}#end loop1 (n per bin)


#Calculate CV

M <- lapply(binlist, function(i) {
  summarise_all(i, mean)
}) #calculate mean for each age at each sampling frequency (1-20)

V <- lapply(binlist, function(i) {
  summarise_all(i, var)
}) #calculate variance for each age at each sampling frequency (1-20)

Msum <- lapply(M, function(i) {
  rowSums(i)
}) ### total count = sum across ages

Vsum <- lapply(V, function(i) {
  rowSums(i)
}) ### total variance = sum of the variances across ages

Msum <- as.data.frame(Msum)
colnames(Msum) <- c(1:20)
Msum <- Msum %>% pivot_longer(everything(), names_to = "sample", values_to = "totalcount")

Vsum <- as.data.frame(Vsum)
colnames(Vsum) <- c(1:20)
Vsum <- Vsum %>% pivot_longer(everything(), names_to = "sample", values_to = "totalvariance")

CV <- Msum %>% inner_join(Vsum, by="sample")
CV$cv <- sqrt(CV$"totalvariance")/CV$"totalcount" ### CV = sqrt(total variance)/total count

CV_female2cm <- CV

#Run the code above for each variation (female 1 and 2 cm, male 1 and 2cm), then save the data as CV_male1cm, CV_male2cm, CV_female1cm, CV_female2cm. Then, run the code below to create the graphs used in the report.

CV_1cm <- CV_male1cm %>% inner_join(CV_female1cm, by="sample") %>% select(sample, cv.x, cv.y)
colnames(CV_1cm) <- c("Sample", "Male", "Female")
CV_1cm <- CV_1cm %>% pivot_longer(cols=c(Male, Female), names_to = "Sex", values_to = "CV")
CV_1cm$Sample <- as.numeric(CV_1cm$Sample)

ggplot(CV_1cm, aes(x=Sample, y=CV, group=Sex, colour=Sex)) + 
  geom_point() +
  theme_bw() +
  xlab("No. Subsampled per 1cm")


CV_2cm <- CV_male2cm %>% inner_join(CV_female2cm, by="sample") %>% select(sample, cv.x, cv.y)
colnames(CV_2cm) <- c("Sample", "Male", "Female")
CV_2cm <- CV_2cm %>% pivot_longer(cols=c(Male, Female), names_to = "Sex", values_to = "CV")
CV_2cm$Sample <- as.numeric(CV_2cm$Sample)

ggplot(CV_2cm, aes(x=Sample, y=CV, group=Sex, colour=Sex)) + 
  geom_point() +
  theme_bw() +
  xlab("No. Subsampled per 2cm")

#Same thing but with Quarters 1 and 2 grouped as Half 1----------


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
    MONTH %in% c(1:6) ~ 'H1'
    , MONTH %in% c(7:9) ~ 'Q3'
    , MONTH %in% c(10:12) ~ 'Q4' ) )

P <- isdb %>% filter(QUARTER == "H1")

P <- P %>%
  mutate(FISH_AGE = ifelse(MONTH==1 & !is.na(FISH_AGE), FISH_AGE+1,
                      ifelse(is.na(FISH_AGE), NA, FISH_AGE))) #If there were any January fish, 1 has to be added to account for their birthday


sex <- 2 #Selects sex (1 male, 2 female)
A <- P[!is.na(P$FISH_AGE) & !is.na(P$FISH_LENGTH) & P$SEXCD_ID == sex,] #Clean data - no NAs for age or length
nrow(A)
hist(A$FISH_LENGTH)
maxage <- max(A$FISH_AGE)
lmax <- max(A$FISH_LENGTH)
lmin <- min(A$FISH_LENGTH)

#Choose bin size here
lbins <- lmin:lmax #Sets bins to 1cm
#lbins <- seq(round(lmin/2-0.1)*2,round(lmax/2-0.1)*2,2) #Sets bins to 2cm

nperbin <- c(1:20) #Sets number per bin (e.g. 2 per 2cm for the selected sex)

binlist <- list()

for(l in 1:length(nperbin)){#loop1
  
  nsim <- 1000 #Sets number of simulations
  
  set.seed(86)
  list_alk <- list()
  for(i in 1:nsim){
    ALK <- as.data.frame(matrix(0,nrow=length(lbins), ncol=maxage+1))
    rownames(ALK)=lbins
    colnames(ALK)=0:maxage
    for(j in 1:(nrow(ALK)-2)){
      for(k in 1:nperbin[l]){
        A_sub <- A[A$FISH_LENGTH>=as.numeric(rownames(ALK)[j]) & A$FISH_LENGTH<as.numeric(rownames(ALK)[j+1]),]
        if(nrow(A_sub)!=0){
          rowid <- sample(1:nrow(A_sub), 1)
          ALK[j,colnames(ALK)==A_sub$FISH_AGE[rowid]] <- ALK[j,colnames(ALK)==A_sub$FISH_AGE[rowid]]+1
        }
        A_sub <- A[A$FISH_LENGTH>=as.numeric(rownames(ALK)[j]) & A$FISH_LENGTH<as.numeric(rownames(ALK)[j+1]),]
        if(nrow(A_sub)==0){ #If there is a length bin with missing age, this takes the ages from the length bin above and below
          A_sub <- A[A$FISH_LENGTH>=(as.numeric(rownames(ALK)[j])-1) & A$FISH_LENGTH<(as.numeric(rownames(ALK)[j+1])+1),]
          rowid <- sample(1:nrow(A_sub), 1)
          if(nrow(A_sub)!=0){ ##################Added this here###############################
            ALK[j,colnames(ALK)==A_sub$FISH_AGE[rowid]] <- ALK[j,colnames(ALK)==A_sub$FISH_AGE[rowid]]+1
          }
        }
        if(nrow(A_sub)==0){ #nrow(A_sub) is still zero then look 2 length bins above and below
          A_sub <- A[A$FISH_LENGTH>=(as.numeric(rownames(ALK)[j])-2) & A$FISH_LENGTH<(as.numeric(rownames(ALK)[j+1])+2),]
          rowid <- sample(1:nrow(A_sub), 1)
          ALK[j,colnames(ALK)==A_sub$FISH_AGE[rowid]] <- ALK[j,colnames(ALK)==A_sub$FISH_AGE[rowid]]+1
        }
      }
    }  
    for(j in nrow(ALK)){ # for last row of ALK
      for(k in 1:nperbin[l]){
        A_sub <- A[A$FISH_LENGTH>=as.numeric(rownames(ALK)[j]),]
        rowid <- sample(1:nrow(A_sub), 1)
        ALK[j,A_sub$FISH_AGE[rowid]] <- ALK[j,A_sub$FISH_AGE[rowid]]+1
      } 
    }
    list_alk[[i]] <- ALK  
  }
  
  #list_alk is a list with nsim ALKs in it
  
  #get LFdist
  
  LF0 <- P[!is.na(P$FISH_LENGTH) & P$SEX == sex,]
  
  LF <- data.frame(FISH_LENGTH=lbins,COUNT=0)
  for(i in 1:nrow(LF0)){
    obsl <- LF0$FISH_LENGTH[i]
    id <- max(which(obsl-lbins >= 0))
    LF$COUNT[LF$FISH_LENGTH==lbins[id]] <-  LF$COUNT[LF$FISH_LENGTH==lbins[id]]+1
  }
  
  CAA <- as.data.frame(matrix(0,nrow=nsim, ncol=maxage+1))
  colnames(CAA) <- 0:maxage
  
  for(i in 1:nsim){
    ALKp <- list_alk[[i]]/nperbin[l]
    CAA[i,] <- colSums(LF$COUNT*ALKp)
  }
  
  binlist[[l]]<-CAA
}#end loop1 (n per bin)


#Calculate CV

M <- lapply(binlist, function(i) {
  summarise_all(i, mean)
}) #calculate mean for each age at each sampling frequency (1-20)

V <- lapply(binlist, function(i) {
  summarise_all(i, var)
}) #calculate variance for each age at each sampling frequency (1-20)

Msum <- lapply(M, function(i) {
  rowSums(i)
}) ### total count = sum across ages

Vsum <- lapply(V, function(i) {
  rowSums(i)
}) ### total variance = sum of the variances across ages

Msum <- as.data.frame(Msum)
colnames(Msum) <- c(1:20)
Msum <- Msum %>% pivot_longer(everything(), names_to = "sample", values_to = "totalcount")

Vsum <- as.data.frame(Vsum)
colnames(Vsum) <- c(1:20)
Vsum <- Vsum %>% pivot_longer(everything(), names_to = "sample", values_to = "totalvariance")

CV <- Msum %>% inner_join(Vsum, by="sample")
CV$cv <- sqrt(CV$"totalvariance")/CV$"totalcount" ### CV = sqrt(total variance)/total count

CV_male1cm <- CV

#Run the code above for each variation (female 1 and 2 cm, male 1 and 2cm), then save the data as CV_male1cm, CV_male2cm, CV_female1cm, CV_female2cm. Then, run the code below to create the graphs used in the report.

CV_1cm <- CV_male1cm %>% inner_join(CV_female1cm, by="sample") %>% select(sample, cv.x, cv.y)
colnames(CV_1cm) <- c("Sample", "Male", "Female")
CV_1cm <- CV_1cm %>% pivot_longer(cols=c(Male, Female), names_to = "Sex", values_to = "CV")
CV_1cm$Sample <- as.numeric(CV_1cm$Sample)

ggplot(CV_1cm, aes(x=Sample, y=CV, group=Sex, colour=Sex)) + 
  geom_point() +
  theme_bw() +
  xlab("No. Subsampled per 1cm")


CV_2cm <- CV_male2cm %>% inner_join(CV_female2cm, by="sample") %>% select(sample, cv.x, cv.y)
colnames(CV_2cm) <- c("Sample", "Male", "Female")
CV_2cm <- CV_2cm %>% pivot_longer(cols=c(Male, Female), names_to = "Sex", values_to = "CV")
CV_2cm$Sample <- as.numeric(CV_2cm$Sample)

ggplot(CV_2cm, aes(x=Sample, y=CV, group=Sex, colour=Sex)) + 
  geom_point() +
  theme_bw() +
  xlab("No. Subsampled per 2cm")