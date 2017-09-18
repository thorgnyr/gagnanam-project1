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


  
library(directlabels)


REPFalsePrePruneTo35 <- read.table("data/REPFalse-Fold3-Conf25-preprun1-35.csv", header = TRUE, sep = ",", dec = ".")


lineNumber <- 
  REPFalsePrePruneTo35[REPFalsePrePruneTo35$lowAsHigh + 
                         REPFalsePrePruneTo35$HighAsLow + 
                         REPFalsePrePruneTo35$Size.of.tree == min(REPFalsePrePruneTo35$lowAsHigh + 
                                                                 REPFalsePrePruneTo35$HighAsLow + 
                                                                   REPFalsePrePruneTo35$Size.of.tree), ][, 1]

  
plot1 <- 
  ggplot(data = REPFalsePrePruneTo35, aes()) + 
  
  geom_line(aes(y = lowAsHigh, 
                x = seq(1, 
                        length(REPFalsePrePruneTo35$lowAsHigh))), 
            color = "Red") +
  
  geom_line(aes(y = HighAsLow, 
                x = seq(1, 
                        length(REPFalsePrePruneTo35$HighAsLow))), 
            color = "Blue") +
  
  geom_line(aes(y = Size.of.tree,
                x = seq(1,
                        length(REPFalsePrePruneTo35$Size.of.tree))),
            color = "Green") +
  
  geom_vline(aes(xintercept = lineNumber)) +
  
  geom_text(aes(lineNumber, 
                150, 
                label = lineNumber, 
                hjust = 2)) +
  
  xlab("Prepruning: Minimum number of objects per leaf") + 
  ylab("Miscategorizations and Tree Size") + 
  labs(title = "Miscategorizations using High/Low quality seperations")

plot1


##Plott2
REPTrueFold3Conf25preprunTo35 <- read.table("data/REPTrue-Fold3-Conf25-preprun1-35.csv", header = TRUE, sep = ",", dec = ".")

lineNumber <- 
  REPTrueFold3Conf25preprunTo35[REPTrueFold3Conf25preprunTo35$lowAsHigh + 
                                     REPTrueFold3Conf25preprunTo35$HighAsLow + 
                                     REPTrueFold3Conf25preprunTo35$Size.of.tree == min(REPTrueFold3Conf25preprunTo35$lowAsHigh + 
                                                                                            REPTrueFold3Conf25preprunTo35$HighAsLow + 
                                                                                            REPTrueFold3Conf25preprunTo35$Size.of.tree), ][, 1]


plot2 <- 
  ggplot(data = REPTrueFold3Conf25preprunTo35, aes()) + 
  
  geom_line(aes(y = lowAsHigh, 
                x = seq(1, 
                        length(REPTrueFold3Conf25preprunTo35$lowAsHigh))), 
            color = "Red") +
  
  geom_line(aes(y = HighAsLow, 
                x = seq(1, 
                        length(REPTrueFold3Conf25preprunTo35$HighAsLow))), 
            color = "Blue") +
  
  geom_line(aes(y = Size.of.tree,
                x = seq(1,
                        length(REPTrueFold3Conf25preprunTo35$Size.of.tree))),
            color = "Green") +
  
  geom_vline(aes(xintercept = lineNumber)) +
  
  geom_text(aes(lineNumber, 
                150, 
                label = lineNumber, 
                hjust = 2)) +
  
  xlab("Prepruning: Minimum number of objects per leaf") + 
  ylab("Miscategorizations and Tree Size") + 
  labs(title = "Miscategorizations using High/Low quality seperations. Postpruned.")

plot2


##Plott3
REPFalsePrePrune3Conf5To25 <- read.table("data/REPFalse-PrePrun3-Conf005-025.csv", header = TRUE, sep = ",", dec = ".")

lineNumber <- 
  REPFalsePrePrune3Conf5To25[REPFalsePrePrune3Conf5To25$lowAsHigh + 
                               REPFalsePrePrune3Conf5To25$HighAsLow + 
                               REPFalsePrePrune3Conf5To25$Size.of.tree == min(REPFalsePrePrune3Conf5To25$lowAsHigh + 
                                                                                REPFalsePrePrune3Conf5To25$HighAsLow + 
                                                                                REPFalsePrePrune3Conf5To25$Size.of.tree), ][, 1]


plot3 <- 
  ggplot(data = REPFalsePrePrune3Conf5To25, aes()) + 
  
  geom_line(aes(y = lowAsHigh, 
                x = Confidence.level), 
            color = "Red") +
  
  geom_line(aes(y = HighAsLow, 
                x = Confidence.level), 
            color = "Blue") +
  
  geom_line(aes(y = Size.of.tree,
                x = Confidence.level),
            color = "Green") +
  
  geom_vline(aes(xintercept = lineNumber)) +
  
  geom_text(aes(lineNumber, 
                150, 
                label = lineNumber, 
                hjust = -0.5)) +
  
  xlab("Value of Confidence Interval: From 0.05 - 0.50") + 
  ylab("Miscategorizations and Tree Size") + 
  labs(title = "Miscategorizations using High/Low quality seperations. Not Postpruned.")

plot3



##Plott4
REPTruePrePrun3Conf25folds2To15 <- read.table("data/REPTrue-PrePrun3-Conf25-folds2-15.csv", header = TRUE, sep = ",", dec = ".")

lineNumber <- 
  REPTruePrePrun3Conf25folds2To15[REPTruePrePrun3Conf25folds2To15$lowAsHigh + 
                                    REPTruePrePrun3Conf25folds2To15$HighAsLow + 
                                    REPTruePrePrun3Conf25folds2To15$Size.of.tree == min(REPTruePrePrun3Conf25folds2To15$lowAsHigh + 
                                                                                          REPTruePrePrun3Conf25folds2To15$HighAsLow + 
                                                                                          REPTruePrePrun3Conf25folds2To15$Size.of.tree), ][, 1]


plot4 <- 
  ggplot(data = REPTruePrePrun3Conf25folds2To15, aes()) + 
  
  geom_line(aes(y = lowAsHigh, 
                x = folds), 
            color = "Red") +
  
  geom_line(aes(y = HighAsLow, 
                x = folds), 
            color = "Blue") +
  
  geom_line(aes(y = Size.of.tree,
                x = folds),
            color = "Green") +
  
  geom_vline(aes(xintercept = lineNumber)) +
  
  geom_text(aes(lineNumber, 
                150, 
                label = lineNumber, 
                hjust = -0.5)) +
  
  xlab("Number of folds: From 2 - 15") + 
  ylab("Miscategorizations and Tree Size") + 
  labs(title = "Miscategorizations using High/Low quality seperations. Postpruned.")

plot4



##Nú er komið að þriggja stiga faktorinum.

#Plot5

ThreeREPFalsePrePruneTo15 <- read.table("data/3REPFalseFold3Conf25preprun1To35.csv", header = TRUE, sep = ",", dec = ".")

lineNumber <- 
  ThreeREPFalsePrePruneTo15[ThreeREPFalsePrePruneTo15$lowAsHigh + 
                              ThreeREPFalsePrePruneTo15$HighAsLow + 
                              ThreeREPFalsePrePruneTo15$Size.of.tree +
                              ThreeREPFalsePrePruneTo15$HighAsMedium +
                              ThreeREPFalsePrePruneTo15$mediumAsHigh == min(ThreeREPFalsePrePruneTo15$lowAsHigh + 
                                                                              ThreeREPFalsePrePruneTo15$HighAsLow + 
                                                                              ThreeREPFalsePrePruneTo15$HighAsMedium +
                                                                              ThreeREPFalsePrePruneTo15$mediumAsHigh +
                                                                              ThreeREPFalsePrePruneTo15$Size.of.tree), ][, 1]


plot5 <- 
  ggplot(data = ThreeREPFalsePrePruneTo15, aes()) + 
  
  geom_line(aes(y = lowAsHigh, 
                x = minNumObj), 
            color = "Red") +
  
  geom_line(aes(y = mediumAsHigh, 
                x = minNumObj), 
            color = "Black") +
  
  geom_line(aes(y = HighAsMedium, 
                x = minNumObj), 
            color = "Purple") +
  
  geom_line(aes(y = HighAsLow, 
                x = minNumObj), 
            color = "Blue") +
  
  geom_line(aes(y = Size.of.tree,
                x = minNumObj),
            color = "Green") +
  
  geom_vline(aes(xintercept = lineNumber)) +
  
  geom_text(aes(lineNumber, 
                150, 
                label = lineNumber, 
                hjust = -0.5)) +
  
  xlab("Prepruning: Minimum number of objects per leaf") + 
  ylab("Miscategorizations and Tree Size") + 
  labs(title = "Miscategorizations using High/Medium/Low quality seperations. Not Postpruned.")

plot5


#Plot 6

ThreeREPTrueFold3Conf25preprun1To35 <- read.table("data/3REPTrueFold3Conf25preprun1To35.csv", header = TRUE, sep = ",", dec = ".")

lineNumber <- 
  ThreeREPTrueFold3Conf25preprun1To35[ThreeREPTrueFold3Conf25preprun1To35$lowAsHigh + 
                                        ThreeREPTrueFold3Conf25preprun1To35$HighAsLow + 
                                        ThreeREPTrueFold3Conf25preprun1To35$Size.of.tree +
                                        ThreeREPTrueFold3Conf25preprun1To35$HighAsMedium +
                                        ThreeREPTrueFold3Conf25preprun1To35$mediumAsHigh == min(ThreeREPTrueFold3Conf25preprun1To35$lowAsHigh + 
                                                                                                  ThreeREPTrueFold3Conf25preprun1To35$HighAsLow + 
                                                                                                  ThreeREPTrueFold3Conf25preprun1To35$HighAsMedium +
                                                                                                  ThreeREPTrueFold3Conf25preprun1To35$mediumAsHigh +
                                                                                                  ThreeREPTrueFold3Conf25preprun1To35$Size.of.tree), ][, 1]


plot6 <- 
  ggplot(data = ThreeREPTrueFold3Conf25preprun1To35, aes()) + 
  
  geom_line(aes(y = lowAsHigh, 
                x = minNumObj), 
            color = "Red") +
  
  geom_line(aes(y = mediumAsHigh, 
                x = minNumObj), 
            color = "Black") +
  
  geom_line(aes(y = HighAsMedium, 
                x = minNumObj), 
            color = "Purple") +
  
  geom_line(aes(y = HighAsLow, 
                x = minNumObj), 
            color = "Blue") +
  
  geom_line(aes(y = Size.of.tree,
                x = minNumObj),
            color = "Green") +
  
  geom_vline(aes(xintercept = lineNumber)) +
  
  geom_text(aes(lineNumber, 
                150, 
                label = lineNumber, 
                hjust = -0.5)) +
  
  xlab("Prepruning: Minimum number of objects per leaf") + 
  ylab("Miscategorizations and Tree Size") + 
  labs(title = "Miscategorizations using High/Medium/Low quality seperations. Postpruned.")

plot6

#Plot 7

ThreeREPFalsePrePrun3Conf005To050 <- read.table("data/3REPFalsePrePrun3Conf005To050.csv", header = TRUE, sep = ",", dec = ".")

lineNumber <- 
  ThreeREPFalsePrePrun3Conf005To050[ThreeREPFalsePrePrun3Conf005To050$lowAsHigh + 
                                      ThreeREPFalsePrePrun3Conf005To050$HighAsLow + 
                                      ThreeREPFalsePrePrun3Conf005To050$Size.of.tree +
                                      ThreeREPFalsePrePrun3Conf005To050$HighAsMedium +
                                      ThreeREPFalsePrePrun3Conf005To050$mediumAsHigh == min(ThreeREPFalsePrePrun3Conf005To050$lowAsHigh + 
                                                                                              ThreeREPFalsePrePrun3Conf005To050$HighAsLow + 
                                                                                              ThreeREPFalsePrePrun3Conf005To050$HighAsMedium +
                                                                                              ThreeREPFalsePrePrun3Conf005To050$mediumAsHigh +
                                                                                              ThreeREPFalsePrePrun3Conf005To050$Size.of.tree), ][, 1]


plot7 <- 
  ggplot(data = ThreeREPFalsePrePrun3Conf005To050, aes()) + 
  
  geom_line(aes(y = lowAsHigh, 
                x = ConfidenceInterval), 
            color = "Red") +
  
  geom_line(aes(y = mediumAsHigh, 
                x = ConfidenceInterval), 
            color = "Black") +
  
  geom_line(aes(y = HighAsMedium, 
                x = ConfidenceInterval), 
            color = "Purple") +
  
  geom_line(aes(y = HighAsLow, 
                x = ConfidenceInterval), 
            color = "Blue") +
  
  geom_line(aes(y = Size.of.tree,
                x = ConfidenceInterval),
            color = "Green") +
  
  geom_vline(aes(xintercept = lineNumber)) +
  
  geom_text(aes(lineNumber, 
                150, 
                label = lineNumber, 
                hjust = -0.5)) +
  
  xlab("Value of Confidence Interval: From 0.05 - 0.50") + 
  ylab("Miscategorizations and Tree Size") + 
  labs(title = "Miscategorizations using High/Medium/Low quality seperations. Not Postpruned.")

plot7

#plot 8

ThreeREPTruePrePrun3Conf25folds2To15 <- read.table("data/3REPTruePrePrun3Conf25folds2To15.csv", header = TRUE, sep = ",", dec = ".")

lineNumber <- 
  ThreeREPTruePrePrun3Conf25folds2To15[ThreeREPTruePrePrun3Conf25folds2To15$lowAsHigh + 
                                         ThreeREPTruePrePrun3Conf25folds2To15$HighAsLow + 
                                         ThreeREPTruePrePrun3Conf25folds2To15$Size.of.tree +
                                         ThreeREPTruePrePrun3Conf25folds2To15$HighAsMedium +
                                         ThreeREPTruePrePrun3Conf25folds2To15$mediumAsHigh == min(ThreeREPTruePrePrun3Conf25folds2To15$lowAsHigh + 
                                                                                                    ThreeREPTruePrePrun3Conf25folds2To15$HighAsLow + 
                                                                                                    ThreeREPTruePrePrun3Conf25folds2To15$HighAsMedium +
                                                                                                    ThreeREPTruePrePrun3Conf25folds2To15$mediumAsHigh +
                                                                                                    ThreeREPTruePrePrun3Conf25folds2To15$Size.of.tree), ][, 1]


plot8 <- 
  ggplot(data = ThreeREPTruePrePrun3Conf25folds2To15, aes()) + 
  
  geom_line(aes(y = lowAsHigh, 
                x = Folds), 
            color = "Red") +
  
  geom_line(aes(y = mediumAsHigh, 
                x = Folds), 
            color = "Black") +
  
  geom_line(aes(y = HighAsMedium, 
                x = Folds), 
            color = "Purple") +
  
  geom_line(aes(y = HighAsLow, 
                x = Folds), 
            color = "Blue") +
  
  geom_line(aes(y = Size.of.tree,
                x = Folds),
            color = "Green") +
  
  geom_vline(aes(xintercept = lineNumber)) +
  
  geom_text(aes(lineNumber, 
                150, 
                label = lineNumber, 
                hjust = -0.5)) +
  
  xlab("Value of Confidence Interval: From 0.05 - 0.50") + 
  ylab("Miscategorizations and Tree Size") + 
  labs(title = "Miscategorizations using High/Medium/Low quality seperations. Not Postpruned.")

plot8
