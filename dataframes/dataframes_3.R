setwd("~/Aulas/Projetos/LearningR/dataframes")

library(Amelia)

# Exercicio Dataframes

data_frame <- read.csv('mpg_exercicio-200916-144746.csv')
summary(data_frame)

data_frame$horsepower <- as.numeric(data_frame$horsepower)

missmap(data_frame, col = c('yellow', 'black'), legend = F, main = '')

hist(data_frame$horsepower)
hist(data_frame$mpg)

vazios_horsepower <- which(is.na(data_frame$horsepower))
data_frame$horsepower[vazios_horsepower] <- median(data_frame$horsepower, na.rm = T)

vazios_mpg <- which(is.na(data_frame$mpg))
data_frame$mpg[vazios_mpg] <- mean(data_frame$mpg, na.rm = T)
