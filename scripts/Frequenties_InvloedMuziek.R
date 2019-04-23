#install.packages("dplyr")
library(dplyr)

# Inlezen csv file
experiment.data <- read.csv(file.choose(), header = TRUE, sep = ",")

# Selecteer de nodige variabelen
variables <- c("rt", "frq_cannabis", "score_1", "score_2", "muziek")
experiment.variables <- experiment.data[variables]

# Enkel de volledige rijen gebruiken
experiment.variables <- subset(experiment.variables, score_1 != "" & score_2 != "" & frq_cannabis != "")

dev.new()
boxplot(experiment.variables$score_1~experiment.variables$muziek, horizontal = TRUE, main = "Invloed van muziek", ylab ="Luistern naar Mozart")
dev.new()
hist(experiment.variables$score_1, main = "Frequentie van 1e ronde", xlab = "Aantal herinnerde feiten", breaks = 4)
dev.new()
hist(experiment.variables$score_2, main = "Frequentie van 2e ronde", xlab = "Aantal herinnerde feiten", breaks = 4)
