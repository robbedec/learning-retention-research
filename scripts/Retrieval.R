library(dplyr)

#manueel inlezen csv file 
Rt_RD <- read.csv(file.choose() , header = TRUE, sep=",")
attach(Rt_RD)
# geef het aantal variabelen en hun betekenis
Rt_RD_df <- tbl_df(Rt_RD)
Rt_RD_df

str(Rt_RD)
# 39 variabelen is heel veel -> selecteren essentiele om steekproef te omschrijven = inherente variabelen 
#selecteer basis kenmerken populatie
variables <- c("rt","gender", "geboortejaar", "score_1","score_2")
Rt_RD_ESS  <- Rt_RD[variables] 
Rt_RD_ESS

# totaal aantal rijen = aantal personen die aan test deelnamen
nrow(Rt_RD_ESS)



#aantal rijen met volledige basisgegevens : man of vrouw (er zijn 2 rijen niet ingevuld) // rt of niet  // leeftijd tss 9 en 99
Rt_RD_ESS_COMPL <- subset(Rt_RD_ESS, gender == 'Vrouw' | gender == 'Man', select =c(rt,gender, geboortejaar,score_1,score_2))


nrow(Rt_RD_ESS_COMPL)

# aantal rijen mannen

Rt_Gender <- Rt_RD_ESS_COMPL["gender"]
nrow(Rt_Gender)
summary(Rt_Gender)

Rt_Rt <- Rt_RD_ESS_COMPL["rt"]
nrow(Rt_Rt)
summary(Rt_Rt)
#
# het aantal mensen met geslacht 'anders' is zeer klein -> geen statistische betekenis. Deze paar lijnen worden weggelaten om factor geslacht (m/v) 
# te kunnen behouden 
Rt_Gender <- Rt_Gender[(Rt_Gender$gender == 'Man')| (Rt_Gender$gender == 'Vrouw'),]
summary(Rt_Gender)
#verdeling volgens leeftijd"
Rt_Age <- Rt_RD_ESS_COMPL["geboortejaar"]
nrow(Rt_Age)

#hist(Rt_Age,5)

# wegdoen rijen met NA 
Rt_RD_ESS_COMPL <- na.omit(Rt_RD_ESS_COMPL)
Rt_RD_ESS_COMPL
nrow(Rt_RD_ESS_COMPL)
# aantal met en zonder RT

Rt_Rt <- Rt_RD_ESS_COMPL["rt"]
nrow(Rt_Rt)
summary(Rt_Rt)

# scores met retentie
Rt_Rt <- subset(Rt_RD_ESS_COMPL, rt == 'Ja ', select = c(score_1,score_2))
#Rt_Rt
summary(Rt_Rt)
# scores zonder retentie
Rt_NRt <- subset(Rt_RD_ESS_COMPL, rt == 'Nee', select = c(score_1,score_2))
#Rt_NRt
summary(Rt_NRt)
# vraag is of beide gemiddelden verschillend zijn
# voor score_1
Rt_Rt_Score_1 = Rt_Rt["score_1"]
Rt_Rt_Score_1
Rt_NRt_Score_1 = Rt_NRt["score_1"]
Rt_NRt_Score_1



t.test(Rt_Rt_Score_1,Rt_NRt_Score_1, mu=0)

## als je laat lopen : t = -0.7 >> kritische grens -2 >> dus geen verschil in score tss rt en nrt voor scor_1
# we herhalen voor score 2
Rt_Rt_Score_2 = Rt_Rt["score_2"]
Rt_Rt_Score_2
Rt_NRt_Score_2 = Rt_NRt["score_2"]
Rt_NRt_Score_2



t.test(Rt_Rt_Score_2,Rt_NRt_Score_2, mu=0)

## zelfde besluit als voor score_1
# we kunnne beide dus op een hoop gooien om de invloed van andere factoren te bekijken
summary(Rt_RD_ESS_COMPL)

# nu kijken we naar factoren die (bijna) zeker losstaan van andere factoren (geen onderlinge beinvloeding)
# leeftijd
# leeftijd is een nominale variabele -> correlatie met studieresultaten. Aangezien we mogen aannemen dat de leeftijden random verdeeld
# zijn over rt/niet-rt en rt/niet-rt bovendien geen verschil maken kunnen we gewoon testen of er een correlatie is tss score
# en leeftijd

#install.packages("Hmisc")

library("Hmisc")

Rt_Age <- subset(Rt_RD, select =c("geboortejaar", "score_1","score_2"))
nrow(Rt_Age)
Rt_Age_complete <- na.omit(Rt_Age)
nrow(Rt_Age_complete)
head(Rt_Age_complete)
Rt_Age_score_1 <- subset (Rt_Age_complete, select = c("geboortejaar","score_1"))
head(Rt_Age_score_1)
Rt_Age_score_1_M <- as.matrix(Rt_Age_score_1)
correl <- rcorr(Rt_Age_score_1_M)
correl
# significantie level om te kunnen besluiten dat score_1 & leeftijd gecorreleerd zijn is maar 0.78 -> te weinig
plot(Rt_Age_score_1)

# Nwe herhalen voor score2
Rt_Age_score_2 <- subset (Rt_Age_complete, select = c("geboortejaar","score_2"))
head(Rt_Age_score_2)
Rt_Age_score_2_M <- as.matrix(Rt_Age_score_2)
correl <- rcorr(Rt_Age_score_2_M)
correl
# significantie level om te kunnen besluiten dat score_2 & leeftijd gecorreleerd zijn is al 0.88 -> nog te weinig
# maar lijkt alsof het langetermijn geheugen er niet op vooruit gaat met de leeftijd
plot(Rt_Age_score_2)

#  woonplaats is een moeilijke: hoe quoteer je "andere" en hoe homogeen is die groep dan (woonboot, vuurtoren, caravan,..)

# diploma vader :
# niet zo mooi, maar laat ons stelln : Geen (0), Lagere school (1), Lager secundair (2), Hoger secundair (3), Bachelor of equivalent (4), Master of equivalent buiten universiteit (5), Master of equivalent universiteit (6), Doctoraat (7), Weet niet (valt nuiten de boot)
nrow(Rt_RD)
Rt_Dipl_vader <- subset(Rt_RD, select = c("diploma_vader", "score_1","score_2"))
nrow(Rt_Dipl_vader)
Rt_Dipl_vader
Rt_Dipl_vader <- na.omit(Rt_Dipl_vader)
nrow(Rt_Dipl_vader)
Rt_Dipl_vader

# weet niet uit diploma verwijderen
Rt_Dipl_vader<- subset(Rt_Dipl_vader, diploma_vader != "Weet niet", select = c("diploma_vader","score_1","score_2"))
nrow(Rt_Dipl_vader)

library(plyr)
Rt_Dipl_vader$diploma_vader <- revalue(Rt_Dipl_vader$diploma_vader, c("Geen" = 0,"Lagere school"=  1,"Lager secundair" = 2,"Hoger secundair" = 3,"Bachelor of equivalent"=4 ,"Master of equivalent buiten universiteit" = 5 , "Master of equivalent universiteit" =6 ,"Doctoraat" =7))

# invloed van diploma vader op score 1 
Rt_Dipl_vader_score1 <- subset(Rt_Dipl_vader, select = c("diploma_vader", "score_1" ))
Rt_Dipl_vader_score1
library(ggplot2)
attach((Rt_Dipl_vader_score1))
# lukt nog niet 
#scatterplot(diploma_vader, score_1)
#Rt_Dipl_vader_score1_M <- as.matrix(Rt_Dipl_vader_score1)
#correl_DiplVader <- rcorr(Rt_Dipl_vader_score1_M)
#correl_DiplVader
# correlatie coeff : 0.03 -> gn correlatie
# idem score_2
Rt_Dipl_vader_score2 <- subset(Rt_Dipl_vader, select = c("diploma_vader", "score_2" ))
Rt_Dipl_vader_score2
library(ggplot2)
attach((Rt_Dipl_vader_score2))
# lukt nog niet 
scatterplot(diploma_vader, score_2)
Rt_Dipl_vader_score2_M <- as.matrix(Rt_Dipl_vader_score2)
correl_DiplVader <- rcorr(Rt_Dipl_vader_score2_M)
correl_DiplVader
# correlatie coeff : 0.02 -> gn correlatie

# idem voor diploma moeder

Rt_Dipl_moeder <- subset(Rt_RD, select = c("diploma_moeder", "score_1","score_2"))
nrow(Rt_Dipl_moeder)
Rt_Dipl_moeder
Rt_Dipl_moeder <- na.omit(Rt_Dipl_moeder)
nrow(Rt_Dipl_moeder)
Rt_Dipl_moeder

# weet niet uit diploma verwijderen
Rt_Dipl_moeder<- subset(Rt_Dipl_moeder, diploma_moeder != "Weet niet", select = c("diploma_moeder","score_1","score_2"))
nrow(Rt_Dipl_moeder)

Rt_Dipl_moeder$diploma_moeder <- revalue(Rt_Dipl_moeder$diploma_moeder, c("Geen" = 0,"Lagere school"=  1,"Lager secundair" = 2,"Hoger secundair" = 3,"Bachelor of equivalent"=4 ,"Master of equivalent buiten universiteit" = 5 , "Master of equivalent universiteit" =6 ,"Doctoraat" =7))

# invloed van diploma vader op score 1 
Rt_Dipl_moeder_score1 <- subset(Rt_Dipl_moeder, select = c("diploma_moeder", "score_1" ))
Rt_Dipl_moeder_score1
library(ggplot2)
attach((Rt_Dipl_moeder_score1))
# lukt nog niet 
scatterplot(diploma_moeder, score_1)
Rt_Dipl_moeder_score1_M <- as.matrix(Rt_Dipl_moeder_score1)
correl_DiplMoeder <- rcorr(Rt_Dipl_moeder_score1_M)
correl_DiplMoeder
# correlatie coeff : 0.15 - > "enige zwakke " correlatie !!!!!
# idem score_2
# invloed van diploma vader op score 1 
Rt_Dipl_moeder_score2 <- subset(Rt_Dipl_moeder, select = c("diploma_moeder", "score_2" ))
Rt_Dipl_moeder_score2
library(ggplot2)
attach((Rt_Dipl_moeder_score2))
# lukt nog niet 
scatterplot(diploma_moeder, score_2)
Rt_Dipl_moeder_score2_M <- as.matrix(Rt_Dipl_moeder_score2)
correl_DiplMoeder <- rcorr(Rt_Dipl_moeder_score2_M)
correl_DiplMoeder
# correlatie coeff : 0.11 enige zwakke correlatie
# idem score_2

Rt_RD
Rt_RD_muziek <- subset(Rt_RD , select = c("muziek", "score_1", "score_2"))
nrow(Rt_RD_muziek)
Rt_RD_muziek
Rt_RD_muziek <- na.omit(Rt_RD_muziek)
nrow(Rt_RD_muziek)
Rt_RD_muziekJa <- subset(Rt_RD_muziek,  muziek == "Ja" ,     select =c("score_1","score_2"))
Rt_RD_muziekJa
nrow(Rt_RD_muziekJa)
Rt_RD_muziekNee <- subset(Rt_RD_muziek,  muziek == "Nee" ,     select =c("score_1","score_2"))
nrow(Rt_RD_muziekNee)
Rt_RD_muziekJa_score1 <- subset(Rt_RD_muziekJa, select=c("score_1"))
Rt_RD_muziekJa_score2 <- subset(Rt_RD_muziekJa, select=c("score_2"))
Rt_RD_muziekNee_score1 <- subset(Rt_RD_muziekNee, select=c("score_1"))
Rt_RD_muziekNee_score2 <- subset(Rt_RD_muziekNee, select=c("score_2"))

t.test(Rt_RD_muziekJa_score1,Rt_RD_muziekNee_score1)
t.test(Rt_RD_muziekJa_score2,Rt_RD_muziekNee_score2)
# geen verschil (sign 95%) tss muziek en geen mzuziek
