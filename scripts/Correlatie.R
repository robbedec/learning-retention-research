#install.packages("dplyr")
library(dplyr)

# Inlezen csv file
experiment.data <- read.csv(file.choose(), header = TRUE, sep = ",")

# Selecteer de nodige variabelen
variables <- c("rt", "uren_wiskunde", "score_1", "score_2", "muziek", "secundair")
experiment.variables <- experiment.data[variables]

# Enkel de volledige rijen gebruiken
experiment.variables <- subset(experiment.variables, score_1 != "" & score_2 != "" & uren_wiskunde != "" & secundair != "")

mS1 <- mean(experiment.variables$score_1)
mS2 <- mean(experiment.variables$score_2)

# Correlatie = 0.400861, betekenis: Een zwakke correlatie, in dit geval kunnen we voorgaande testen niet gebruiken om toekomstige testen te voorspellen
cor(experiment.variables$score_1, experiment.variables$score_2)

