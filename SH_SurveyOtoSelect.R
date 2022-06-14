####################
# Silver Hake Otolith Selection
# Caira Clark & Tim Barrett
# 30 November 2021
####################

library(ROracle)
library(ggplot2)
library(dplyr)
library(tidyr)

channel <- ROracle::dbConnect(DBI::dbDriver("Oracle"), username=oracle.username, password=oracle.password, oracle.dsn)  

##Survey Sample Data-------------------------

#Select fish with otoliths collected during missing otolith years
S <- dbGetQuery(channel, "select a.mission, a.setno, a.strat, b.fshno, b.fsex, b.flen, b.fwt, b.age from 
                    groundfish.gsinf a, groundfish.gsdet b where a.mission in ('NED2011025') 
                    and b.spec=14 and a.mission=b.mission 
                    and a.setno=b.setno and b.fshno is not null")

#Add a year column
S$YEAR <- as.factor(substr(S$MISSION, start = 4, stop = 7))

#Add a quarter column
S$QUARTER <- "Q3"

#Select only sex codes 1 and 2
S <- S %>% filter(FSEX %in% c(1, 2))

#Select Scotian Shelf strata
S <- S %>% filter(STRAT %in% c(440:483))


sex <- 1 #Selects sex (1 male, 2 female)
A <- S[!is.na(S$AGE) & !is.na(S$FLEN) & S$FSEX == sex,] #Clean data - no NAs for age or length
nrow(A)
hist(A$FLEN)
maxage <- max(A$AGE)
lmax <- max(A$FLEN)
lmin <- min(A$FLEN)

#Choose bin size here
#lbins <- lmin:lmax #Sets bins to 1cm
lbins <- seq(round(lmin/2-0.1)*2,round(lmax/2-0.1)*2,2) #Sets bins to 2cm

nperbin <- c(1:20) #Sets number per bin (e.g. 2 per 2cm for the selected sex)

binlist <- list()

for(l in 1:length(nperbin)){#loop1
  
nsim <- 10 #Sets number of simulations

set.seed(86)
list_alk <- list()
for(i in 1:nsim){
  ALK <- as.data.frame(matrix(0,nrow=length(lbins), ncol=maxage+1))
  rownames(ALK)=lbins
  colnames(ALK)=0:maxage
  for(j in 1:(nrow(ALK)-1)){
    for(k in 1:nperbin[l]){
      A_sub <- A[A$FLEN>=as.numeric(rownames(ALK)[j]) & A$FLEN<as.numeric(rownames(ALK)[j+1]),]
      if(nrow(A_sub)!=0){
        rowid <- sample(1:nrow(A_sub), 1)
        ALK[j,colnames(ALK)==A_sub$AGE[rowid]] <- ALK[j,colnames(ALK)==A_sub$AGE[rowid]]+1
      }
      A_sub <- A[A$FLEN>=as.numeric(rownames(ALK)[j]) & A$FLEN<as.numeric(rownames(ALK)[j+1]),]
      if(nrow(A_sub)==0){ #If there is a length bin with missing age, this takes the ages from the length bin above and below
        A_sub <- A[A$FLEN>=(as.numeric(rownames(ALK)[j])-1) & A$FLEN<(as.numeric(rownames(ALK)[j+1])+1),]
        rowid <- sample(1:nrow(A_sub), 1)
        ALK[j,colnames(ALK)==A_sub$AGE[rowid]] <- ALK[j,colnames(ALK)==A_sub$AGE[rowid]]+1
      }
    }
  }  
  for(j in nrow(ALK)){ # for last row of ALK
    for(k in 1:nperbin[l]){
      A_sub <- A[A$FLEN>=as.numeric(rownames(ALK)[j]),]
      rowid <- sample(1:nrow(A_sub), 1)
      ALK[j,A_sub$AGE[rowid]] <- ALK[j,A_sub$AGE[rowid]]+1
    } 
  }
  list_alk[[i]] <- ALK  
}

#list_alk is a list with nsim ALKs in it

#get LFdist

LF0 <- S[!is.na(S$FLEN) & S$FSEX == sex,]

LF <- data.frame(FLEN=lbins,COUNT=0)
for(i in 1:nrow(LF0)){
  obsl <- LF0$FLEN[i]
  id <- max(which(obsl-lbins >= 0))
  LF$COUNT[LF$FLEN==lbins[id]] <-  LF$COUNT[LF$FLEN==lbins[id]]+1
}

#use ALKs to get numbers
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


