#install.packages("dplyr")
library(dplyr)

# Inlezen csv file
experiment.data <- read.csv(file.choose(), header = TRUE, sep = ",")

# Selecteer de nodige variabelen
variables <- c("rt", "uren_wiskunde", "score_1", "score_2", "muziek", "secundair")
experiment.variables <- experiment.data[variables]

# Enkel de volledige rijen gebruiken
experiment.variables <- subset(experiment.variables, score_1 != "" & score_2 != "" & uren_wiskunde != "" & secundair != "")

## Vergelijking gemiddelden
aggData <- aggregate(experiment.variables, by=list(experiment.variables$rt), FUN = mean, na.rm = TRUE)
aggData <- data.frame(aggData$score_1, aggData$score_2)

matrix <- as.matrix(sapply(aggData, as.numeric))
colnames(matrix) <- c('Score 1', 'Score 2')
rownames(matrix) <- c('RT', 'lezen')
barplot(matrix, main ="Gemiddelde scores", ylab = "Herinnerde feiten", beside = TRUE, legend.text = TRUE, args.legend = list(x = "topright", bty = "n", inset = c(0,-0.1,0,0)))


# Datasets voor elke categorie
AsoRt <- subset(experiment.variables, secundair=="ASO" & rt=="Ja ", select = c(score_1,score_2))
NAsoRt <- subset(experiment.variables, secundair!="ASO" & rt=="Ja ", select = c(score_1,score_2))

# Test of het gemiddelde gelijk is (t = 2.8597 -> niet gelijk)
t.test(AsoRt["score_1"], NAsoRt["score_1"], mu = 0)

AsoRtMuziek <- experiment.variables[ which(secundair=="ASO" & muziek=="Ja", rt=="Ja"),]
Aso <- experiment.variables[ which(secundair=="ASO" & muziek=="Nee", rt=="Nee "),]
AsoMuziek <- experiment.variables[ which(secundair=="ASO" & muziek=="Ja", rt=="Nee"),]


NAsoRtMuziek <- experiment.variables[ which(secundair!="ASO" & muziek=="Ja", rt=="Ja"),]
NAso <- experiment.variables[ which(secundair!="ASO" & muziek=="Nee", rt=="Nee "),]
NAsoMuziek <- experiment.variables[ which(secundair!="ASO" & muziek=="Ja", rt=="Nee"),]

# Gemiddelden per categorie
GemiddeldesAsoScore1 <- c(mean(AsoRt$score_1), mean(AsoRtMuziek$score_1), mean(Aso$score_1), mean(AsoMuziek$score_1))
GemiddeldesAsoScore2 <- c(mean(AsoRt$score_2), mean(AsoRtMuziek$score_2), mean(Aso$score_2), mean(AsoMuziek$score_2))
GemiddeldesTsoScore1 <- c(mean(NAsoRt$score_1), mean(NAsoRtMuziek$score_1), mean(NAso$score_1), mean(NAsoMuziek$score_1))
GemiddeldesNAsoScore2 <- c(mean(NAsoRt$score_2), mean(NAsoRtMuziek$score_2), mean(NAso$score_2), mean(NAsoMuziek$score_2))

# Gebruiken in de console
t.test(AsoRt["score_1"], NAsoRt["score_1"], mu = 0)
t.test(AsoRt["score_2"], NAsoRt["score_2"], mu = 0)