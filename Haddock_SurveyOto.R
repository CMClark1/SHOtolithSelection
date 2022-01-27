####################
# Haddock Otolith Selection
# Caira Clark & Tim Barrett
# 17 December 2021
####################

library(ROracle)
library(ggplot2)
library(dplyr)
library(tidyr)

channel <- ROracle::dbConnect(DBI::dbDriver("Oracle"), username=oracle.username, password=oracle.password, oracle.dsn)  

##Survey Data-------------------------

#Select fish with otoliths collected during missing otolith years
S <-dbGetQuery(channel, "select a.mission, a.setno, b.fshno, b.flen, b.fwt, b.age from 
                    groundfish.gsinf a, groundfish.gsdet b where a.mission in ('TEM2004004') 
                    and b.spec=11 and a.mission=b.mission 
                    and a.setno=b.setno")

A <- S[!is.na(S$AGE) & !is.na(S$FLEN),]  #Clean data - no NAs for age or length
nrow(A)
hist(A$FLEN)
maxage <- max(A$AGE)
lmax <- max(A$FLEN)
lmin <- min(A$FLEN)

#Choose bin size here
lbins <- lmin:lmax #Sets bins to 1cm
#lbins <- seq(round(lmin/2-0.1)*2,round(lmax/2-0.1)*2,2) #Sets bins to 2cm

#Choose the number per bin here. You can specify a minimum and maximum as 1:20.
nperbin <- c(1:20) #sets number of otoliths aged per bin (e.g. 1 per 1cm or 20 per 1cm)

binlist <- list() #This sets up an empty list that will be filled with numbers at age using the loop below.

#This loop randomly samples from the aged fish at the rate that you specify nperbin to create an age-length key. It repeats it the number of times that you specify in nsim. It then applies the ALKs to the length frequency of the original sample (both aged and unaged fish) to create 1000 numbers at age.

for(l in 1:length(nperbin)){#loop1
  
  nsim <- 1000 #Set the number of simulations so that the resulting curve is 'smooth' (e.g. 1000)
  
  set.seed(86)
  list_alk <- list() #This sets up an empty list for the age-length keys that are calculated in the next loop.
  
  #This is the loop that creates the 1000 ALKs (or however many simulations that you specify) 
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
        #If there is a length bin with missing age, this next loop takes the ages from the length bin above and below
        if(nrow(A_sub)==0){ 
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
    list_alk[[i]] <- ALK  #list_alk is a list with nsim ALKs in it
  }
  
  #This creates the length frequency of the original sample
  LF0 <- S[!is.na(S$FLEN),]
  LF <- data.frame(FLEN=lbins,COUNT=0)
  
  for(i in 1:nrow(LF0)){
    obsl <- LF0$FLEN[i]
    id <- max(which(obsl-lbins >= 0))
    LF$COUNT[LF$FLEN==lbins[id]] <-  LF$COUNT[LF$FLEN==lbins[id]]+1
  }
  
  #This applies the 1000 (or nsim) ALKs to the length frequency to create a numbers-at-age (referred to as CAA)
  CAA <- as.data.frame(matrix(0,nrow=nsim, ncol=maxage+1))
  colnames(CAA) <- 0:maxage
  
  for(i in 1:nsim){
    ALKp <- list_alk[[i]]/nperbin[l]
    CAA[i,] <- colSums(LF$COUNT*ALKp)
  }
  
  binlist[[l]]<-CAA
}#end loop1


#Calculate CV -----------------

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
CV$sample <- as.numeric(CV$sample)

ggplot(CV, aes(x=sample, y=cv)) + 
  geom_point() +
  theme_bw() +
  xlab("No. Subsampled per 1cm")  #change the caption if you choose 2cm length grouping
  