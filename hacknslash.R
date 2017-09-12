data <- read.table("data/winequality-red.csv", header = TRUE, sep = ";", dec = ".")
library(ggplot2)
plot(data$quality)

hist(data$quality)
data$quality <- factor(data$quality)
levels(data$quality)

count(data$quality[data$quality == "low"])
table(data$quality)
levels(data$quality) <- c("low", "low", "low", "low", "high", "high")

write.csv(data, file = "winequality-moddedlowhigh.csv")

quality.lm <- lm(quality ~ ., data = data)
summary(quality.lm)
plot(quality.lm)

library("RWeka")

resultJ48 <- J48(classificationTry~., data)

summary(resultJ48)

ggplot(aes(quality, free.sulfur.dioxide), data = data) + geom_boxplot(alpha = 0.1) + geom_jitter()
