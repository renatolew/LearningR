setwd("~/Aulas/Projetos/LearningR/dataframes")

library(Amelia)

# Dataframes 2

df <- read.csv('titanic_R.csv')

str(df)
summary(df)
colnames(df)[8] <- 'Taxa'
posicao_troca <- which(colnames(df) == 'Taxa')
colnames(df)[posicao_troca] <- 'Tarifa'

df$Tarifa <- as.numeric(df$Tarifa)

df$Sex <- as.factor(df$Sex)

missmap(df, col = c('yellow', 'black'), legend = F, main = '')

posicao_age <- which(is.na(df$Age))
df$Age[posicao_age] <- median(df$Age, na.rm = T)


hist(df$Tarifa)

posicao_tarifa <- (which(is.na(df$Tarifa)))

df$Tarifa[posicao_tarifa] <- median(df$Tarifa, na.rm = T)
