#install.packages("dplyr")
library(dplyr)

# Inlezen csv file
experiment.data <- read.csv(file.choose(), header = TRUE, sep = ",")

# Selecteer de nodige variabelen
variables <- c("rt", "uren_wiskunde", "score_1", "score_2", "muziek", "secundair")
experiment.variables <- experiment.data[variables]

# Enkel de volledige rijen gebruiken
experiment.variables <- subset(experiment.variables, score_1 != "" & score_2 != "" & uren_wiskunde != "" & secundair != "" &  uren_wiskunde < 9 & uren_wiskunde > 1)

boxplot(experiment.variables$score_1 ~ experiment.variables$uren_wiskunde, main = "Spreiding van testronde 1", xlab ="Uren wiskunde", ylab ="Herinnerde feiten")
boxplot(experiment.variables$score_2 ~ experiment.variables$uren_wiskunde, main = "Spreiding van testronde 2", xlab ="Uren wiskunde", ylab ="Herinnerde feiten")

