####################
# Silver Hake Otolith Selection - Port Sample by Individual
# Caira Clark & Tim Barrett
# 21 January 2021
####################

library(ROracle)
library(ggplot2)
library(dplyr)
library(tidyr)

channel <- ROracle::dbConnect(DBI::dbDriver("Oracle"), username="username", password="password", "PTRAN")

##Port Sample Data-------------------------

#Select fish with otoliths collected during missing otolith years
P <- dbGetQuery(channel, "select 
                   a.SAMPLE, a.DATELANDED, a.AREA, 
                   b.fishlen, b.otolith, b.age, b.sex, b.weight 
                   from mfd_port_samples.gpsamples a, mfd_port_samples.gpages b 
                   where extract(YEAR from a.DATELANDED) in (2011) 
                   and a.SPECIES=14 
                   and a.sample=b.sample 
                   and b.otolith is not null")

P$YEAR <- as.factor(substr(P$SAMPLE, start = 1, stop = 4))

P$MONTH <- as.numeric(substr(P$DATELANDED, start = 6, stop = 7))

P <- P %>%  mutate(
  QUARTER = case_when(
    MONTH %in% c(1:3) ~ 'Q1'
    , MONTH %in% c(4:6) ~ 'Q2'
    , MONTH %in% c(7:9) ~ 'Q3'
    , MONTH %in% c(10:12) ~ 'Q4' ) )

P <- P %>% filter(AREA %in% c(440:483))

P <- P %>% filter(QUARTER == "Q4")


sex <- 2 #Selects sex (1 male, 2 female)
A <- P[!is.na(P$AGE) & !is.na(P$FISHLEN) & P$SEX == sex,] #Clean data - no NAs for age or length
nrow(A)
hist(A$FISHLEN)
maxage <- max(A$AGE)
lmax <- max(A$FISHLEN)
lmin <- min(A$FISHLEN)

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
        A_sub <- A[A$FISHLEN>=as.numeric(rownames(ALK)[j]) & A$FISHLEN<as.numeric(rownames(ALK)[j+1]),]
        if(nrow(A_sub)!=0){
          rowid <- sample(1:nrow(A_sub), 1)
          ALK[j,colnames(ALK)==A_sub$AGE[rowid]] <- ALK[j,colnames(ALK)==A_sub$AGE[rowid]]+1
        }
        A_sub <- A[A$FISHLEN>=as.numeric(rownames(ALK)[j]) & A$FISHLEN<as.numeric(rownames(ALK)[j+1]),]
        if(nrow(A_sub)==0){ #If there is a length bin with missing age, this takes the ages from the length bin above and below
          A_sub <- A[A$FISHLEN>=(as.numeric(rownames(ALK)[j])-1) & A$FISHLEN<(as.numeric(rownames(ALK)[j+1])+1),]
          rowid <- sample(1:nrow(A_sub), 1)
          if(nrow(A_sub)!=0){ ##################Added this here###############################
            ALK[j,colnames(ALK)==A_sub$AGE[rowid]] <- ALK[j,colnames(ALK)==A_sub$AGE[rowid]]+1
          }
        }
        if(nrow(A_sub)==0){ #nrow(A_sub) is still zero then look 2 length bins above and below
          A_sub <- A[A$FISHLEN>=(as.numeric(rownames(ALK)[j])-2) & A$FISHLEN<(as.numeric(rownames(ALK)[j+1])+2),]
          rowid <- sample(1:nrow(A_sub), 1)
          ALK[j,colnames(ALK)==A_sub$AGE[rowid]] <- ALK[j,colnames(ALK)==A_sub$AGE[rowid]]+1
        }
      }
    }  
    for(j in nrow(ALK)){ # for last row of ALK
      for(k in 1:nperbin[l]){
        A_sub <- A[A$FISHLEN>=as.numeric(rownames(ALK)[j]),]
        rowid <- sample(1:nrow(A_sub), 1)
        ALK[j,A_sub$AGE[rowid]] <- ALK[j,A_sub$AGE[rowid]]+1
      } 
    }
    list_alk[[i]] <- ALK  
  }
  
  #list_alk is a list with nsim ALKs in it
  
  #get LFdist
  
  LF0 <- P[!is.na(P$FISHLEN) & P$SEX == sex,]
  
  LF <- data.frame(FISHLEN=lbins,COUNT=0)
  for(i in 1:nrow(LF0)){
    obsl <- LF0$FISHLEN[i]
    id <- max(which(obsl-lbins >= 0))
    LF$COUNT[LF$FISHLEN==lbins[id]] <-  LF$COUNT[LF$FISHLEN==lbins[id]]+1
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

CV_female1cm <- CV

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
