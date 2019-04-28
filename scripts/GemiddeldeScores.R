# Inlezen csv file
experiment.data <- read.csv(file.choose(), header = TRUE, sep = ",")

# Selecteer de nodige variabelen
variables <- c("rt", "uren_wiskunde", "score_1", "score_2", "muziek", "secundair")
experiment.variables <- experiment.data[variables]

# Enkel de volledige rijen gebruiken
experiment.variables <- subset(experiment.variables, score_1 != "" & score_2 != "" & uren_wiskunde != "" & secundair != "")

# barplot opmaken
barData <- aggregate(experiment.variables, by=list(experiment.variables$rt), FUN = mean, na.rm = TRUE)
barData <- data.frame(barData$score_1, barData$score_2)

matrix <- as.matrix(sapply(aggData, as.numeric))
colnames(matrix) <- c('Score 1', 'Score 2')
rownames(matrix) <- c('Aso', 'Andere richtingen')
barplot(matrix, main ="Gemiddelde scores", ylab = "Herinnerde feiten", beside = TRUE, legend.text = TRUE, args.legend = list(x = "topright", bty = "n", inset = c(-0.1,-0.1,0,0)))
