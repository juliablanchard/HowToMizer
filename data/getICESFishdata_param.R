# # # # Get most recent annual ICES information (assumes have already downloaded and converted and saved file from http://www.ices.dk/datacentre/StdGraphDB.asp, called "Fishdata.txt")
# # # # 1) Computes time-averaged values (here, over 1985-1995) for SSB, Landings and Fishing mortality rates for calibrating the size-based model to a reference period
# # # # 2) Creates species-specific and time-varying matrices for: SSB, Landings and Fishing mortality rates for all years available, for simulating dynamical changes through time
# # # # 3) Also calculates species-specific CVs for recruitment, for use in stochastic simulations.



##### JLB UPDATED 3/9/2020

# get most recent ICES stock assessment summary outputs for North Sea model species

# install.packages("icesSAG") # archived
# library(devtools)
# install_version("icesVocab","1.1.8")
# install_version("icesSAG","1.3-6")

library(icesSAG)
library(tidyverse)
# ?icesSAG


### get summary data for each species by assessmentkey:

#Sprat: https://standardgraphs.ices.dk/ViewSourceData.aspx?key=13322

Sprat_sumtab <- getSummaryTable(13322)

# Sandeel : https://standardgraphs.ices.dk/ViewCharts.aspx?key=13303
# https://standardgraphs.ices.dk/ViewCharts.aspx?key=13301
# https://standardgraphs.ices.dk/ViewCharts.aspx?key=13298
# https://standardgraphs.ices.dk/ViewCharts.aspx?key=13304

Sandeel_sumtab <- getSummaryTable(c(13303,13301,13298,13304))

# N.pout - https://standardgraphs.ices.dk/ViewCharts.aspx?key=13166

N.pout_sumtab <- getSummaryTable(c(13166))


# Herring - https://standardgraphs.ices.dk/ViewCharts.aspx?key=13422

Herring_sumtab <- getSummaryTable(c(13422))

# Dab - https://standardgraphs.ices.dk/ViewCharts.aspx?key=13184

Dab_sumtab <- getSummaryTable(c(13184))

# Whiting - https://standardgraphs.ices.dk/ViewCharts.aspx?key=13525
# https://standardgraphs.ices.dk/ViewCharts.aspx?key=13506

Whiting_sumtab <- getSummaryTable(c(13525))

# Sole - https://standardgraphs.ices.dk/ViewCharts.aspx?key=13743
# https://standardgraphs.ices.dk/ViewCharts.aspx?key=13495
# https://standardgraphs.ices.dk/ViewCharts.aspx?key=13828
 
Sole_sumtab <- getSummaryTable(c(13743))

# Gurnard - https://standardgraphs.ices.dk/ViewCharts.aspx?key=13493

Gurnard_sumtab <- getSummaryTable(c(13493))

# Plaice - https://standardgraphs.ices.dk/ViewCharts.aspx?key=13484
# https://standardgraphs.ices.dk/ViewCharts.aspx?key=13744

Plaice_sumtab <- getSummaryTable(c(13484))

# Haddock - https://standardgraphs.ices.dk/ViewCharts.aspx?key=13204

Haddock_sumtab <- getSummaryTable(c(13204))

# Cod - https://standardgraphs.ices.dk/ViewCharts.aspx?key=13740
# https://standardgraphs.ices.dk/ViewCharts.aspx?key=13838

Cod_sumtab <- getSummaryTable(c(13740,13838))

# Saithe - https://standardgraphs.ices.dk/ViewCharts.aspx?key=13511

Saithe_sumtab <- getSummaryTable(c(13511))



############ Need to use the data above and create new versions of the following matrices (rows: year, cols: species):

SpIdx <- c("Sprat","Sandeel","N.pout","Herring","Dab","Whiting","Sole","Gurnard","Plaice","Haddock","Cod","Saithe")
# dataIdx <- c("Sprat","Sandeel","N.pout","Herring","Dab","Whiting","Sole","Gurnard","Plaice","Haddock","Cod","Saithe")

# Which data set is the longest?
dim_sum <- NULL
for(iSpecies in SpIdx)
{
switch (iSpecies,
        Sprat = {myVar = Sprat_sumtab[[1]]},
        Sandeel = {myVar = Sandeel_sumtab[[1]]},
        N.pout = {myVar = N.pout_sumtab[[1]]},
        Herring = {myVar = Herring_sumtab[[1]]},
        Dab = {myVar = Dab_sumtab[[1]]},
        Whiting = {myVar = Whiting_sumtab[[1]]},
        Sole = {myVar = Sole_sumtab[[1]]},
        Gurnard = {myVar = Gurnard_sumtab[[1]]},
        Plaice = {myVar = Plaice_sumtab[[1]]},
        Haddock = {myVar = Haddock_sumtab[[1]]},
        Cod = {myVar = Cod_sumtab[[1]]},
        Saithe = {myVar = Saithe_sumtab[[1]]},
        {}
)
dim_sum <- c(dim_sum,dim(myVar)[1])    
print(max(dim_sum)) 
}
# Herring it is
timePeriod <- Herring_sumtab[[1]]$Year

# Extract "F" column and create time series of fishing mortalty rates: fmat.csv

fmat <- matrix(NA,nrow = length(timePeriod), ncol = length(SpIdx), dimnames = list("year" = timePeriod, "species" = SpIdx))

for(iSpecies in SpIdx)
{
switch (iSpecies,
    Sprat = {myVar = Sprat_sumtab[[1]]$F},
    Sandeel = {
        myMat <- matrix(NA, nrow = dim(Sandeel_sumtab[[1]])[1], ncol = 4)
        mySSB <- matrix(NA, nrow = dim(Sandeel_sumtab[[1]])[1], ncol = 4)
        
        for(icol in 1:4)
        {
            tempF <- Sandeel_sumtab[[icol]]$F*Sandeel_sumtab[[icol]]$SSB
            while(length(tempF)<dim(Sandeel_sumtab[[1]])[1]) tempF <- c(NA,tempF)
            myMat[,icol] <- tempF 
            
            tempSSB <- Sandeel_sumtab[[icol]]$SSB
            while(length(tempSSB)<dim(Sandeel_sumtab[[1]])[1]) tempSSB <- c(NA,tempSSB)
            mySSB[,icol] <- tempSSB
            
        }
        ssbSum <- apply(mySSB,1,sum,na.rm=T)
        myVar <- apply(myMat,1,mean,na.rm=T)/ssbSum
        myVar[is.nan(myVar)] <- NA # in case there was a mean of NAs only, keeping the NA in place
        },
    N.pout = {myVar = N.pout_sumtab[[1]]$F},
    Herring = {myVar = Herring_sumtab[[1]]$F},
    Dab = {myVar = Dab_sumtab[[1]]$F},
    Whiting = {myVar = Whiting_sumtab[[1]]$F},
    Sole = {myVar = Sole_sumtab[[1]]$F},
    Gurnard = {myVar = Gurnard_sumtab[[1]]$F},
    Plaice = {myVar = Plaice_sumtab[[1]]$F},
    Haddock = {myVar = Haddock_sumtab[[1]]$F},
    Cod = {
      myMat <- matrix(NA, nrow = dim(Cod_sumtab[[1]])[1], ncol = 2)
      mySSB <- matrix(NA, nrow = dim(Cod_sumtab[[1]])[1], ncol = 2)
      
      for(icol in 1:2)
      {
        tempF <- Cod_sumtab[[icol]]$F*Cod_sumtab[[icol]]$SSB
        while(length(tempF)<dim(Cod_sumtab[[1]])[1]) tempF <- c(NA,tempF)
        myMat[,icol] <- tempF 
        
        tempSSB <- Cod_sumtab[[icol]]$SSB
        while(length(tempSSB)<dim(Cod_sumtab[[1]])[1]) tempSSB <- c(NA,tempSSB)
        mySSB[,icol] <- tempSSB
        
      }
      ssbSum <- apply(mySSB,1,sum,na.rm=T)
      myVar <- apply(myMat,1,mean,na.rm=T)/ssbSum
      myVar[is.nan(myVar)] <- NA # in case there was a mean of NAs only, keeping the NA in place
    },
    Saithe = {myVar = Saithe_sumtab[[1]]$F},
    {}
)
    while (length(myVar)<length(timePeriod)) myVar <- c(NA,myVar)
  fmat[,iSpecies] <- myVar  
}
write.csv(fmat, file = "data/fmatWeighted.csv")

#nseaparams.csv , "catachabliity column" - here puttinhg in the time-averaged F as a baseline reference value

fAvg <- fmat[which(rownames(fmat) == "2014"):which(rownames(fmat) == "2019"),]
fAvg <- apply(fAvg,2,mean,na.rm=T)
fAvg[is.nan(fAvg)] <- NA

nsparams <- readr::read_csv("data/old/nsparams.csv")
GurnardCatch <- nsparams$catchability[8]
nsparams$catchability <- fAvg
nsparams$catchability[8] <- GurnardCatch # there is no data so using catchability from old df to not have NAs
write.csv(nsparams, file = "data/nsparams.csv")
# Extract "catches" column and create time series of total catches (including discards) | RF catches column is already total catches

catchesMat <- matrix(NA,nrow = length(timePeriod), ncol = length(SpIdx), dimnames = list("year" = timePeriod, "species" = SpIdx))

for(iSpecies in SpIdx)
{
    switch (iSpecies,
            Sprat = {myVar = Sprat_sumtab[[1]]$catches},
            Sandeel = {
                myMat <- matrix(NA, nrow = dim(Sandeel_sumtab[[1]])[1], ncol = 4)
                for(icol in 1:4)
                {
                    tempF <- Sandeel_sumtab[[icol]]$catches
                    while(length(tempF)<dim(Sandeel_sumtab[[1]])[1]) tempF <- c(NA,tempF)
                    myMat[,icol] <- tempF 
                }
                myVar <- apply(myMat,1,sum,na.rm=T)
                myVar[which(myVar == 0)] <- NA # summing NA will do this, keeping the NA in place
            },
            N.pout = {myVar = N.pout_sumtab[[1]]$landings}, # N.pout does not have catches data, just landings (and no discards)
            Herring = {myVar = Herring_sumtab[[1]]$catches},
            Dab = {myVar = Dab_sumtab[[1]]$catches},
            Whiting = {myVar = Whiting_sumtab[[1]]$catches},
            Sole = {myVar = Sole_sumtab[[1]]$catches},
            Gurnard = {myVar = Gurnard_sumtab[[1]]$catches},
            Plaice = {myVar = Plaice_sumtab[[1]]$catches},
            Haddock = {myVar = Haddock_sumtab[[1]]$catches},
            Cod = {
                myMat <- matrix(NA, nrow = dim(Cod_sumtab[[1]])[1], ncol = 2)
                for(icol in 1:2)
                {
                    tempF <- Cod_sumtab[[icol]]$catches
                    while(length(tempF)<dim(Cod_sumtab[[1]])[1]) tempF <- c(NA,tempF)
                    myMat[,icol] <- tempF 
                }
                myVar <- apply(myMat,1,sum,na.rm=T)
                myVar[which(myVar == 0)] <- NA # summing NA will do this, keeping the NA in place
            },
            Saithe = {myVar = Saithe_sumtab[[1]]$catches},
            {}
    )
    while (length(myVar)<length(timePeriod)) myVar <- c(NA,myVar)
    catchesMat[,iSpecies] <- myVar  
}
write.csv(catchesMat, file = "data/catchesMat.csv") # loading from csv won't work with below code due to losing "years" as rownames
#time-averaged-catches.csv
# averaged subset
catchAvg <- catchesMat[which(rownames(catchesMat) == "2014"):which(rownames(catchesMat) == "2019"),]
catchAvg <- apply(catchAvg,2,mean,na.rm=T)
catchAvg[is.nan(catchAvg)] <- NA
catchAvg <- data.frame("species" = SpIdx, "Catch_1419_tonnes" = catchAvg,row.names = NULL)

write.csv(catchAvg, file = "data/time-averaged-catches.csv",row.names = F)

# Extract "SSB" column and create time series of SSB

SSBmat <- matrix(NA,nrow = length(timePeriod), ncol = length(SpIdx), dimnames = list("year" = timePeriod, "species" = SpIdx))

for(iSpecies in SpIdx)
{
    switch (iSpecies,
            Sprat = {myVar = Sprat_sumtab[[1]]$SSB},
            Sandeel = {
                myMat <- matrix(NA, nrow = dim(Sandeel_sumtab[[1]])[1], ncol = 4)
                for(icol in 1:4)
                {
                    tempF <- Sandeel_sumtab[[icol]]$SSB
                    while(length(tempF)<dim(Sandeel_sumtab[[1]])[1]) tempF <- c(NA,tempF)
                    myMat[,icol] <- tempF 
                }
                myVar <- apply(myMat,1,sum,na.rm=T)
                myVar[which(myVar == 0)] <- NA # summing NA will do this, keeping the NA in place
            },
            N.pout = {myVar = N.pout_sumtab[[1]]$landings}, # N.pout does not have catches data, just landings (and no discards)
            Herring = {myVar = Herring_sumtab[[1]]$SSB},
            Dab = {myVar = Dab_sumtab[[1]]$SSB},
            Whiting = {myVar = Whiting_sumtab[[1]]$SSB},
            Sole = {myVar = Sole_sumtab[[1]]$SSB},
            Gurnard = {myVar = Gurnard_sumtab[[1]]$SSB},
            Plaice = {myVar = Plaice_sumtab[[1]]$SSB},
            Haddock = {myVar = Haddock_sumtab[[1]]$SSB},
            Cod = {
                myMat <- matrix(NA, nrow = dim(Cod_sumtab[[1]])[1], ncol = 2)
                for(icol in 1:2)
                {
                    tempF <- Cod_sumtab[[icol]]$SSB
                    while(length(tempF)<dim(Cod_sumtab[[1]])[1]) tempF <- c(NA,tempF)
                    myMat[,icol] <- tempF 
                }
                myVar <- apply(myMat,1,sum,na.rm=T)
                myVar[which(myVar == 0)] <- NA # summing NA will do this, keeping the NA in place
            },
            Saithe = {myVar = Saithe_sumtab[[1]]$SSB},
            {}
    )
    while (length(myVar)<length(timePeriod)) myVar <- c(NA,myVar)
    SSBmat[,iSpecies] <- myVar  
}
write.csv(SSBmat, file = "data/SSBmat.csv")
#time-averaged-SSB.csv
# averaged subset
SSBavg <- SSBmat[which(rownames(SSBmat) == "2014"):which(rownames(SSBmat) == "2019"),]
SSBavg <- apply(SSBavg,2,mean,na.rm=T)
SSBavg[is.nan(SSBavg)] <- NA
SSBavg <- data.frame("species" = SpIdx, "SSB_1419" = SSBavg,row.names = NULL)
write.csv(SSBavg, file = "data/time-averaged-SSB.csv", row.names = F)



#### Then apply these new files to the toyexample1.R (time-averaged calibration) and toyexample2.R (time-series evaluation/exploration)
#### Then apply these using Mike's code to do time-series fitting.





#' how to get more F per year/species
#' Fmat has data from 1983 up to 2010 for all species / should we use it?
#' comparing old and new code over 2000-2010 period
#' 

newF <- fMatW[54:64,]
oldF <- Fmat[,44:54]
oldF <-t(oldF)
oldF <- cbind(2000:2010,oldF)

diff <- newF - oldF
diff$X <- 2000:2010

plot_dat <- reshape2::melt(diff,"X")

ggplot(plot_dat)+
  geom_line(aes(x = X, y = value, color = variable ))+
ggtitle("newF - oldF")





###########______________ OLDER VERSION

rm(list=ls())

setwd("/Users/jlblanch/Dropbox/sizeBasedStuff/NorthSea")

Fishdata<-read.table(file="input/Fishdata.txt",sep="\t",header=T)[,1:9]
levels(Fishdata$FishStock)
NSstocklist<-c("cod-347d",  "had-34"   ,    "her-47d3"   ,   "nop-34"    ,    "ple-nsea"  ,   "sai-3a46"   , "san-ns1"   ,   "san-ns2"   ,   "san-ns3"     , "sol-nsea"   , "whg-47d" )
ns<-Fishdata[is.element(Fishdata$FishStock,NSstocklist),]
ns$FishStock<-as.factor(as.character(ns$FishStock))

#meanF values for Sprat from Niels:
Fsprat<-read.table(file="input/FbarSprat.csv",sep=",",header=T)

#ICES catch statistics values for Sprat,Dab,Gurnard from Niels:
LandingsSDG <- read.table(file="Niels LH & selectivities/ICES_1950-2010dabgurnardsprat.csv",sep=",",header=T,row.names="Species")


param<-read.table(file="input/IMAGE_paper_params_new975.txt",header=T, sep = "\t")
species<-as.character(param$species)

# 2) Select reference period to compute time-avergaed values for calibrating the model
 
refYears <- 1985:1995 
SSB_8595 <-matrix(NA,nrow=length(species),ncol=1)
Catch_8595 <-matrix(NA,nrow=length(species),ncol=1)
F0_8595 <-matrix(NA,nrow=length(species),ncol=1)
rownames(F0_8595) <- species
rownames(Catch_8595) <- species
rownames(SSB_8595) <- species


SSB_8595["Sandeel",1]<-mean(rowSums(cbind(ns[ns$FishStock=="san-ns1" & is.element(ns$Year, refYears),"SSB"],ns[ns$FishStock=="san-ns2" & is.element(ns$Year, refYears),"SSB"],ns[ns$FishStock=="san-ns3"& is.element(ns$Year, refYears),"SSB"])))
SSB_8595["N.pout",1]<-mean(ns[ns$FishStock=="nop-34" & is.element(ns$Year, refYears),"SSB"])
SSB_8595["Herring",1]<-mean(ns[ns$FishStock=="her-47d3" & is.element(ns$Year, refYears),"SSB"])
SSB_8595["Whiting",1]<-mean(ns[ns$FishStock=="whg-47d" & is.element(ns$Year, refYears),"SSB"])
SSB_8595["Sole",1]<-mean(ns[ns$FishStock=="sol-nsea" & is.element(ns$Year, refYears),"SSB"])
SSB_8595["Plaice",1]<-mean(ns[ns$FishStock=="ple-nsea" & is.element(ns$Year, refYears),"SSB"])
SSB_8595["Haddock",1]<-mean(ns[ns$FishStock=="had-34" & is.element(ns$Year, refYears),"SSB"])
SSB_8595["Cod",1]<-mean(ns[ns$FishStock=="cod-347d" & is.element(ns$Year, refYears),"SSB"])
SSB_8595["Saithe",1]<-mean(ns[ns$FishStock=="sai-3a46" & is.element(ns$Year, refYears),"SSB"])


Catch_8595["Sandeel",1]<-mean(rowSums(cbind(ns[ns$FishStock=="san-ns1" & is.element(ns$Year, refYears),"Landings"],ns[ns$FishStock=="san-ns2" & is.element(ns$Year, refYears),"Landings"],ns[ns$FishStock=="san-ns3"& is.element(ns$Year, refYears),"Landings"])))
Catch_8595["N.pout",1]<-mean(ns[ns$FishStock=="nop-34" & is.element(ns$Year, refYears),"Landings"])
Catch_8595["Herring",1]<-mean(ns[ns$FishStock=="her-47d3" & is.element(ns$Year, refYears),"Landings"])
Catch_8595["Whiting",1]<-mean(ns[ns$FishStock=="whg-47d" & is.element(ns$Year, refYears),"Landings"])
Catch_8595["Sole",1]<-mean(ns[ns$FishStock=="sol-nsea" & is.element(ns$Year, refYears),"Landings"])
Catch_8595["Plaice",1]<-mean(ns[ns$FishStock=="ple-nsea" & is.element(ns$Year, refYears),"Landings"])
Catch_8595["Haddock",1]<-mean(ns[ns$FishStock=="had-34" & is.element(ns$Year, refYears),"Landings"])
Catch_8595["Cod",1]<-mean(ns[ns$FishStock=="cod-347d" & is.element(ns$Year, refYears),"Landings"])
Catch_8595["Saithe",1]<-mean(ns[ns$FishStock=="sai-3a46" & is.element(ns$Year, refYears),"Landings"])


# # # # Added mean 85-95 catches in for Sprat, Dab and Gurnard afterwards, see values in param file. 


# # # Add in Discards/Bycatch for available species????

# # # NOT AT PRESENT - Niels is looking into this 

# # # Fmax: to calculate F at weight, the model  needs a maximum F value for each species, this is multiplied by the selectivity  at weight, such that Fmax will be achieved when selectivity is equal to one, and lower values of F for selectivities that are less than 1

# # # The meanFs from Fishdata are averaged over the following ages for each species:

meanFages <- list(Sprat=c(1:2),Sandeel=c(0:4),N.pout=c(1:2),Herring=c(2:6),Dab=NA,Whiting=c(2:6),Sole=c(2:6),Gurnard=NA,Plaice=c(2:6),Haddock=c(2:4),Cod=c(2:4),Saithe=c(3:6))

# # # To convert these values to Fmax, we need to get the estimated selection at age ( from Niels) for the target ages that were used in the meanF etsimates. 

load("/Users/jlblanch/Dropbox/sizebasedstuff/NorthSea/Niels LH & selectivities/Sel.RData")

# # # We can then divide the meanF by the mean selectivities-at-age to get an estimate of Fmax for each species. This assumes we can apply a single selctivity curve to the whole time series of meanFs.
#selectivity equation is:
#sel ~ 1/(1 + exp(S1 - S2 * age))

meanSel<-F0_8595

meanSel["Sprat",1]<-mean(1/(1 + exp(coef(Sel$Sprat)[1] -coef(Sel$Sprat)[2]*meanFages$Sprat)))
meanSel["Sandeel",1]<-mean(1/(1 + exp(coef(Sel$Sandeel)[1] -coef(Sel$Sandeel)[2]*meanFages$Sandeel)))
meanSel["N.pout",1]<-mean(1/(1 + exp(coef(Sel$N.pout)[1] -coef(Sel$N.pout)[2]*meanFages$N.pout)))
meanSel["Herring",1]<-mean(1/(1 + exp(coef(Sel$Herring)[1] -coef(Sel$Herring)[2]*meanFages$Herring)))
meanSel["Whiting",1]<-mean(1/(1 + exp(coef(Sel$Whiting)[1] -coef(Sel$Whiting)[2]*meanFages$Whiting)))
meanSel["Sole",1]<-mean(1/(1 + exp(coef(Sel$Sole)[1] -coef(Sel$Sole)[2]*meanFages$Sole)))
meanSel["Plaice",1]<-mean(1/(1 + exp(coef(Sel$Plaice)[1] -coef(Sel$Plaice)[2]*meanFages$Plaice)))
meanSel["Haddock",1]<-mean(1/(1 + exp(coef(Sel$Haddock)[1] -coef(Sel$Haddock)[2]*meanFages$Haddock)))
meanSel["Cod",1]<-mean(1/(1 + exp(coef(Sel$Cod)[1] -coef(Sel$Cod)[2]*meanFages$Cod)))
meanSel["Saithe",1]<-mean(1/(1 + exp(coef(Sel$Saithe)[1] -coef(Sel$Saithe)[2]*meanFages$Saithe)))

meanSel["Dab",1]<-meanSel["Plaice",1]
meanSel["Gurnard",1]<-meanSel["Whiting",1]

save(meanSel,file="input/meanSel.Rdata")

#need to get the mean Selectivities for the above ages


F0_8595["Sandeel",1]<-mean(rowMeans(cbind(ns[ns$FishStock=="san-ns1" & is.element(ns$Year, refYears),"MeanF"],ns[ns$FishStock=="san-ns2" & is.element(ns$Year, refYears),"MeanF"],ns[ns$FishStock=="san-ns3"& is.element(ns$Year, refYears),"MeanF"])))/meanSel["Sprat",1]
F0_8595["N.pout",1]<-mean(ns[ns$FishStock=="nop-34" & is.element(ns$Year, refYears),"MeanF"])/meanSel["N.pout",1]
F0_8595["Herring",1]<-mean(ns[ns$FishStock=="her-47d3" & is.element(ns$Year, refYears),"MeanF"])/meanSel["Herring",1]
F0_8595["Whiting",1]<-mean(ns[ns$FishStock=="whg-47d" & is.element(ns$Year, refYears),"MeanF"])/meanSel["Whiting",1]
F0_8595["Sole",1]<-mean(ns[ns$FishStock=="sol-nsea" & is.element(ns$Year, refYears),"MeanF"])/meanSel["Sole",1]
F0_8595["Plaice",1]<-mean(ns[ns$FishStock=="ple-nsea" & is.element(ns$Year, refYears),"MeanF"])/meanSel["Plaice",1]
F0_8595["Haddock",1]<-mean(ns[ns$FishStock=="had-34" & is.element(ns$Year, refYears),"MeanF"])/meanSel["Haddock",1]
F0_8595["Cod",1]<-mean(ns[ns$FishStock=="cod-347d" & is.element(ns$Year, refYears),"MeanF"])/meanSel["Cod",1]
F0_8595["Saithe",1]<-mean(ns[ns$FishStock=="sai-3a46" & is.element(ns$Year, refYears),"MeanF"])/meanSel["Saithe",1]


# Values for Sprat, Gurnard & Dab: there are some estimates available from exploratory assessments.

# the meanFs for Sprat over the 1991-2011 period are reported in ICES HAWG (2008), but the assessment is exploratory only.
# for Dab and Gurnard Fs are not estimated although landings are given ICES WGNEW (2007), also exploratory only and thought to be unreliable.
# Dab selectivities were assumed to be the same as Plaice since it is bycatch in the flatfish fisheries 
# Gurnard is a demersal fisheries bycatch, and was assumed to have the same fisheries selctivity as Whiting 

F0_8595["Sprat",1] <- mean(Fsprat[is.element(Fsprat$Year,refYears),3])/meanSel["Sprat",1]

# Pope 2000 ICE J Mar Sci estimate mean F in 1989 using length-at-age chort analysis and swpt area methods from the fleet
# SInce usingthe ratio of landing results in very low F estimates dab: plaice landings, it is very low, adjust upwards, use Pope's estimates (approx 0.4) for each) as a rough average F. Then for time series of Fs use the ratio of avg Fs with the associated target species

F0_8595["Dab",1] <- 0.4

F0_8595["Gurnard",1] <-  0.4

param$F0_8595<-F0_8595


write.table(param, file="input/IMAGE_paper_params_new975.txt",row.names=F, col.names=T,sep="\t")

 
#2) Time varying SSB, Catches and F

Years<-sort(unique(ns$Year))
SSB<-array(NA, dim=c(12,length(Years)))
rownames(SSB)<-species
colnames(SSB)<-Years



SSB["Sandeel",(length(Years)-length(ns[ns$FishStock=="san-ns1","MeanF"])+1):length(Years)]<-rowSums(cbind(ns[ns$FishStock=="san-ns1","SSB"],ns[ns$FishStock=="san-ns2","SSB"],ns[ns$FishStock=="san-ns3","SSB"]))
SSB["N.pout",(length(Years)-length(ns[ns$FishStock=="nop-34","SSB"])+1):length(Years)]<-ns[ns$FishStock=="nop-34","SSB"]
SSB["Herring",(length(Years)-length(ns[ns$FishStock=="her-47d3","SSB"])+1):length(Years)]<-ns[ns$FishStock=="her-47d3","SSB"]
SSB["Whiting",(length(Years)-length(ns[ns$FishStock=="whg-47d","SSB"])+1):length(Years)]<-ns[ns$FishStock=="whg-47d","SSB"]
SSB["Sole",(length(Years)-length(ns[ns$FishStock=="sol-nsea","SSB"])+1):length(Years)]<-ns[ns$FishStock=="sol-nsea","SSB"]
SSB["Plaice",(length(Years)-length(ns[ns$FishStock=="ple-nsea","SSB"])+1):length(Years)]<-ns[ns$FishStock=="ple-nsea","SSB"]
SSB["Haddock",(length(Years)-length(ns[ns$FishStock=="had-34","SSB"])+1):length(Years)]<-ns[ns$FishStock=="had-34","SSB"]
SSB["Cod",(length(Years)-length(ns[ns$FishStock=="cod-347d","SSB"])+1):length(Years)]<-ns[ns$FishStock=="cod-347d","SSB"]
SSB["Saithe",(length(Years)-length(ns[ns$FishStock=="sai-3a46","SSB"])+1):length(Years)]<-ns[ns$FishStock=="sai-3a46","SSB"]

save(SSB,file="input/SSB.RData")

Landings<-array(NA, dim=c(12,length(Years)))
rownames(Landings)<-species
colnames(Landings)<-Years

Landings["Cod",]<-NA
Landings["Cod",(length(Years)-length(ns[ns$FishStock=="cod-347d","Landings"])+1):length(Years)]<-ns[ns$FishStock=="cod-347d","Landings"]
Landings["Haddock",]<-NA
Landings["Haddock",(length(Years)-length(ns[ns$FishStock=="had-34","Landings"])+1):length(Years)]<-ns[ns$FishStock=="had-34","Landings"]
Landings["Herring",]<-NA
Landings["Herring",(length(Years)-length(ns[ns$FishStock=="her-47d3","Landings"])+1):length(Years)]<-ns[ns$FishStock=="her-47d3","Landings"]
Landings["N.pout",]<-NA
Landings["N.pout",(length(Years)-length(ns[ns$FishStock=="nop-34","Landings"])+1):length(Years)]<-ns[ns$FishStock=="nop-34","Landings"]
Landings["Plaice",]<-NA
Landings["Plaice",(length(Years)-length(ns[ns$FishStock=="ple-nsea","Landings"])+1):length(Years)]<-ns[ns$FishStock=="ple-nsea","Landings"]
Landings["Saithe",]<-NA
Landings["Saithe",(length(Years)-length(ns[ns$FishStock=="sai-3a46","Landings"])+1):length(Years)]<-ns[ns$FishStock=="sai-3a46","Landings"]
Landings["Sandeel",]<-NA
Landings["Sandeel",(length(Years)-length(ns[ns$FishStock=="san-ns1","Landings"])+1):length(Years)]<-rowSums(cbind(ns[ns$FishStock=="san-ns1","Landings"],ns[ns$FishStock=="san-ns2","Landings"],ns[ns$FishStock=="san-ns3","Landings"]))
Landings["Sole",]<-NA
Landings["Sole",(length(Years)-length(ns[ns$FishStock=="sol-nsea","Landings"])+1):length(Years)]<-ns[ns$FishStock=="sol-nsea","Landings"]
Landings["Whiting",]<-NA
Landings["Whiting",(length(Years)-length(ns[ns$FishStock=="whg-47d","Landings"])+1):length(Years)]<-ns[ns$FishStock=="whg-47d","Landings"]

Landings["Sprat",]<-LandingsSDG[paste(1957:2011),"Sprat"]
Landings["Dab",]<-LandingsSDG[paste(1957:2011),"Dab"]
Landings["Gurnard",]<-LandingsSDG[paste(1957:2011),"Gurnard"]

save(Landings,file="input/Landings.RData")

Fmat<-array(NA, dim=c(12,length(Years)))
rownames(Fmat)<-species
colnames(Fmat)<-Years

Fmat["Cod",]<-NA
Fmat["Cod",(length(Years)-length(ns[ns$FishStock=="cod-347d","MeanF"])+1):length(Years)]<-ns[ns$FishStock=="cod-347d","MeanF"]/meanSel["Cod",1]
Fmat["Haddock",]<-NA
Fmat["Haddock",(length(Years)-length(ns[ns$FishStock=="had-34","MeanF"])+1):length(Years)]<-ns[ns$FishStock=="had-34","MeanF"]/meanSel["Haddock",1]
Fmat["Herring",]<-NA
Fmat["Herring",(length(Years)-length(ns[ns$FishStock=="her-47d3","MeanF"])+1):length(Years)]<-ns[ns$FishStock=="her-47d3","MeanF"]/meanSel["Herring",1]
Fmat["N.pout",]<-NA
Fmat["N.pout",(length(Years)-length(ns[ns$FishStock=="nop-34","MeanF"])+1):length(Years)]<-ns[ns$FishStock=="nop-34","MeanF"]/meanSel["N.pout",1]
Fmat["Plaice",]<-NA
Fmat["Plaice",(length(Years)-length(ns[ns$FishStock=="ple-nsea","MeanF"])+1):length(Years)]<-ns[ns$FishStock=="ple-nsea","MeanF"]/meanSel["Plaice",1]
Fmat["Saithe",]<-NA
Fmat["Saithe",(length(Years)-length(ns[ns$FishStock=="sai-3a46","MeanF"])+1):length(Years)]<-ns[ns$FishStock=="sai-3a46","MeanF"]/meanSel["Saithe",1]
Fmat["Sandeel",]<-NA
Fmat["Sandeel",(length(Years)-length(ns[ns$FishStock=="san-ns1","MeanF"])+1):length(Years)]<-rowMeans(cbind(ns[ns$FishStock=="san-ns1","MeanF"],ns[ns$FishStock=="san-ns2","MeanF"],ns[ns$FishStock=="san-ns3","MeanF"]))/meanSel["Sandeel",1]
Fmat["Sole",]<-NA
Fmat["Sole",(length(Years)-length(ns[ns$FishStock=="sol-nsea","MeanF"])+1):length(Years)]<-ns[ns$FishStock=="sol-nsea","MeanF"]/meanSel["Sole",1]
Fmat["Whiting",]<-NA
Fmat["Whiting",(length(Years)-length(ns[ns$FishStock=="whg-47d","MeanF"])+1):length(Years)]<-ns[ns$FishStock=="whg-47d","MeanF"]/meanSel["Whiting",1]



# # # # # ASSUMPTIONS FOR MISSING Fmat VALUES!!!!

# Past Sprat fishing mortalities not known, but landings are known

Fmat["Sprat",(length(Years)-length(Fsprat$Year)+1):length(Years)]<-Fsprat[,3]/meanSel["Sprat",1]

# Fmat["Sprat",]<-Fmat["N.pout",]
ratioFL<-mean(Fmat["Sprat",paste(1991:2010)]/Landings["Sprat",paste(1991:2010)])
Fmat["Sprat", paste(1957:1990)]<-ratioFL*Landings["Sprat", paste(1957:1990)]


# Past whiting fishing mortalities and landings are not known, but Fs and landings are very closely correlated with Haddock so using this relationship to fill in the gaps

 lmWhit<-coef(lm(Fmat["Whiting",]~Fmat["Haddock",]))
 Fmat["Whiting",]<-ifelse(is.na(Fmat["Whiting",]),lmWhit[1]+ lmWhit[2]*Fmat["Haddock",is.na(Fmat["Whiting",])],Fmat["Whiting",])


# Gurnard and Dab neither fishing moratlity nor biomass are known, assume bycatch in other fisheries inferred from mean ratio of Fs with target species ( and mean assumed form Pope beam trawl study for Dab and Gurnard)


# ratio of gurnard: whiting landings
 
 #f0_8595 is catchability col from old_nsparams

ratioGugWhg<-F0_8595["Gurnard",1]/F0_8595["Whiting",1] 
ratioDabPle<-F0_8595["Dab",1]/F0_8595["Plaice",1] 



Fmat["Gurnard",]<-ratioGugWhg*Fmat["Whiting",]

Fmat["Dab",]<- ratioDabPle*Fmat["Plaice",]




Fmat <- ifelse(is.na(Fmat),0,Fmat)



save(Fmat,file="input/Fmat.RData")



# 3) Get recruitment CVs for stochastic model

pdf("hists.pdf")
ns$standardRec<-ns$Recruitment
list<-as.character(unique(ns$FishStock))
for (i in 1:length(list)) {
	
	plot(ns$Year[which(ns$FishStock==list[i])],log(ns$Recruitment[which(ns$FishStock==list[i])]))
	
	points(ns$Year[which(ns$FishStock==list[i] & !is.na(ns$Recruitment))],lm(log(ns$Recruitment[which(ns$FishStock==list[i])])~ns$Year[which(ns$FishStock==list[i])],na.rm=T)$fitted.values,typ="l")
    
    ns$standardRec[which(ns$FishStock==list[i] & !is.na(ns$Recruitment))]<-log(ns$Recruitment[which(ns$FishStock==list[i] & !is.na(ns$Recruitment))]) - lm(log(ns$Recruitment[which(ns$FishStock==list[i])])~ns$Year[which(ns$FishStock==list[i])],na.rm=T)$fitted.values
    
    hist(ns$standardRec[which(ns$FishStock==list[i] & !is.na(ns$Recruitment))])
    
    }
dev.off()

sdlog<-aggregate(ns$standardRec,list(ns$FishStock),sd,na.rm=T)
