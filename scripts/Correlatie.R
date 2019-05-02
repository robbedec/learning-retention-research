#install.packages("dplyr")
#install.packages("Hmisc")
#install.packages("corrgram")
##install.packages("corrplot")
library(dplyr)
library("Hmisc")
library(corrplot)
#library(corrgram)

# Inlezen csv file
experiment.data <- read.csv(file.choose(), header = TRUE, sep = ",")

# Selecteer de nodige variabelen
variables <- c("rt", "uren_wiskunde", "score_1", "score_2", "muziek", "secundair")
experiment.variables <- experiment.data[variables]

# Enkel de volledige rijen gebruiken
experiment.variables <- subset(experiment.variables, score_1 != "" & score_2 != "" & uren_wiskunde != "" & secundair != "")

WiskundeScore1 <- subset(experiment.variables, select = c("uren_wiskunde", "score_1"))
WiskundeScore2 <- subset(experiment.variables, select = c("uren_wiskunde", "score_2"))
WiskundeScoreCombined <- subset(experiment.variables, select = c("uren_wiskunde", "score_1", "score_2"))

# Converteren naar een 
matrixWiskundeScore1 <- as.matrix(WiskundeScore1)
matrixWiskundeScore2 <- as.matrix(WiskundeScore2)
matrixWiskundeScoreCombined <- as.matrix(WiskundeScoreCombined)

correlatie <- rcorr(matrixWiskundeScore1)

M<-cor(matrixWiskundeScoreCombined)
head(round(M,2))

plot(matrixWiskundeScore1, main = "Visualisatie tussen Score 1 en uren wiskunde",xlim = c(0, 10), ylab = "Score 1", xlab = "Uren wiskunde")
plot(matrixWiskundeScore2, main = "Visualisatie tussen Score 2 en uren wiskunde",xlim = c(0, 10), ylim = c(0, 25), ylab = "Score 1", xlab = "Uren wiskunde")

corrplot(M, method="number")

# Plot voor correlatie inclusief de functie om de waarden te berekenen

cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}
p.mat <- cor.mtest(matrixWiskundeScoreCombined)
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(M, method="color", col=col(200),  
         type="upper", order="hclust", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         # Combine with significance
         p.mat = p.mat, sig.level = 0.01, insig = "blank", 
         # hide correlation coefficient on the principal diagonal
         diag=FALSE,
)

# QQPlot genereren om de standaarnormaalverdeling te bijkeken
qqnorm(matrixWiskundeScoreCombined, main = "Spreiding van de z-waarden", ylab = "Aantal herinnerde feiten", xlab = "Normale theoretische kwantielen")
qqline(matrixWiskundeScoreCombined)
