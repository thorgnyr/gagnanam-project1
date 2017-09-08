data <- read.table("data/winequality-red.csv", header = TRUE, sep = ";", dec = ".")

plot(data$quality)

data$quality <- factor(data$quality)
levels(data$quality)

levels(data$quality) <- c("low", "low", "medium", "medium", "high", "high")

write.csv(data, file = "winequality-modded.csv")
