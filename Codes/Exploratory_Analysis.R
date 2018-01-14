#########################################
###Prediction Model for Premier League###
#########################################

#Load libraries
library(RCurl)
require(MASS)

###Import data (start with 1 season)###
PL2018 <-read.csv(text = getURL("https://raw.githubusercontent.com/Emna91Cas92/PROJECT_FOOTBALL/master/DATA/PremierLeague_Season2016-2017.csv"))

###Exploratory analysis###
### Analysis for all teams###
str(PL2018)
#HomeGoals
hist(PL2018$FTHG) #zero inflated Poisson distribution ?
mle_HG<-fitdistr(PL2018$FTHG, densfun="poisson")
#AwayGoals
hist(PL2018$FTAG) #zero inflated Poisson distribution ?
mle_AG<-fitdistr(PL2018$FTAG, densfun="poisson")

#Homeshots
hist(PL2018$HS) #Right skewed
mean(PL2018$HS)
#Awayshots
hist(PL2018$AS) #Right skewed
mean(PL2018$AS)

#HomeshotsOnTarget
hist(PL2018$HST) #Poisson distributed zero inflated?
fitdistr(PL2018$HST, densfun="poisson")
#AwayshotsOnTarget
hist(PL2018$AST) #Poisson distributed zero inflated?
fitdistr(PL2018$AST, densfun="poisson")
#Also look into Ratio of shots (on target) versus goals

#Homecorners
hist(PL2018$HC) #Right skewed
fitdistr(PL2018$HC, densfun="poisson")
#Awaycorners
hist(PL2018$AC) #Right skewed
fitdistr(PL2018$AC, densfun="poisson")

#HomeFouls
hist(PL2018$HF) #Approx Normal
mean(PL2018$HF)
#AwayFouls
hist(PL2018$AF) #Approx Normal
mean(PL2018$AF)

#HomeYellow
hist(PL2018$HY) #Poisson
fitdistr(PL2018$HY, densfun="poisson")
#AwayYellow
hist(PL2018$AF) #Poisson
fitdistr(PL2018$AY, densfun="poisson")

#HomeRed
hist(PL2018$HR) #zero inflated poisson
mean(PL2018$HR)
#AwayRed
hist(PL2018$AR) #zero inflated poisson
mean(PL2018$AR)

#Correlation Matrix
myvars <- c("FTHG","FTAG","HS","AS","HST",
                "AST","HC","AC","HF",
                "AF","HY","AY"
                ,"HR","AR")

subset_PL <- PL2018[myvars]
cor_mat <- cor(subset_PL)

###Analysis per team###
summary_stat <- data.frame(matrix(0, ncol = 22, nrow = 20))
###Check per team###
list_teams = unique(PL2018$HomeTeam)
for (i in list_teams) {
  j = which(list_teams == i)
  hist_HG = hist(PL2018$FTHG[which(PL2018$HomeTeam==i)]) #zero inflated Poisson distribution ?
  hist_AG = hist(PL2018$FTAG[which(PL2018$AwayTeam==i)]) #zero inflated Poisson distribution ?
  #Final Goals
  mle_HG<-as.numeric(fitdistr(PL2018$FTHG[which(PL2018$HomeTeam==i)], densfun="poisson")$estimate[1])
  mle_AG<-as.numeric(fitdistr(PL2018$FTAG[which(PL2018$AwayTeam==i)], densfun="poisson")$estimate[1])
  diff_G <- mle_HG - mle_AG
  
  #Shots
  mle_HS<-mean(PL2018$HS[which(PL2018$HomeTeam==i)])
  mle_AS<-mean(PL2018$AS[which(PL2018$AwayTeam==i)])
  diff_S <- mle_HS - mle_AS
  
  #Shots on Target
  mle_HST<-as.numeric(fitdistr(PL2018$HST[which(PL2018$HomeTeam==i)], densfun="poisson")$estimate[1])
  mle_AST<-as.numeric(fitdistr(PL2018$AST[which(PL2018$AwayTeam==i)], densfun="poisson")$estimate[1])
  diff_ST <- mle_HST - mle_AST
  
  #Corners
  mle_HC<-as.numeric(fitdistr(PL2018$HC[which(PL2018$HomeTeam==i)], densfun="poisson")$estimate[1])
  mle_AC<-as.numeric(fitdistr(PL2018$AC[which(PL2018$AwayTeam==i)], densfun="poisson")$estimate[1])
  diff_C <- mle_HC - mle_AC
  
  #Fouls
  mle_HF<-as.numeric(fitdistr(PL2018$HF[which(PL2018$HomeTeam==i)], densfun="poisson")$estimate[1])
  mle_AF<-as.numeric(fitdistr(PL2018$AF[which(PL2018$AwayTeam==i)], densfun="poisson")$estimate[1])
  diff_F <- mle_HF - mle_AF
  
  #Yellow
  mle_HY<-as.numeric(fitdistr(PL2018$HY[which(PL2018$HomeTeam==i)], densfun="poisson")$estimate[1])
  mle_AY<-as.numeric(fitdistr(PL2018$AY[which(PL2018$AwayTeam==i)], densfun="poisson")$estimate[1])
  diff_Y <- mle_HY - mle_AY
  
  #Red
  mle_HR <-as.numeric(fitdistr(PL2018$HC[which(PL2018$HomeTeam==i)], densfun="poisson")$estimate[1])
  mle_AR <-as.numeric(fitdistr(PL2018$AC[which(PL2018$AwayTeam==i)], densfun="poisson")$estimate[1])
  diff_R <- mle_HR - mle_AR
  
  
  summary_stat[j,]=cbind(Team = i, MLE_HG = mle_HG, MLE_AG = mle_AG, DIFF_G = diff_G, 
                         MLE_HS = mle_HS, MLE_AS =mle_AS, diff_s = diff_S,
                         MLE_HST = mle_HST, MLE_AST = mle_AST, diff_ST = diff_ST, MLE_HC = mle_HC, 
                         MLE_AC = mle_AC, DIFF_C = diff_C,
                         MLE_HF = mle_HF, MLE_AF = mle_AF, DIFF_E = diff_F, MLE_HY = mle_HY, 
                         MLE_AY = mle_AY, DIFF_Y = diff_Y,
                         MLE_HR = mle_HR, MLE_AR = mle_AR, DIFF_R = diff_R)
}

colnames(summary_stat)=c("Team", "MLE_HG", "MLE_AG", "DIFF_G" , 
                       "MLE_HS" , "MLE_AS", "diff_S" ,
                       "MLE_HST" , "MLE_AST" , "diff_ST" , "MLE_HC" , 
                       "MLE_AC" , "DIFF_C" ,
                       "MLE_HF" , "MLE_AF" , "DIFF_E" , "MLE_HY" , 
                       "MLE_AY" , "DIFF_Y" ,
                       "MLE_HR" , "MLE_AR" , "DIFF_R" )

###Modeling###
### Check research paper - Poisson model: I remember we need to program the minimization ourselves
### so we need to find a good minization function in R (similar to setting up Heston model) ###



