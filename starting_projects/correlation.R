# Primeiros passos
setwd("~/Aulas/Projetos/LearningR/starting_projects")
rm(list=ls())

# Carregando os pacotes
library(corrplot)
library(psych)
library(dplyr)
library(tidyr)

# Lendo os dados
data <- read.csv("Aval_Lider.csv")
head(data)

data_long <- data %>% 
  pivot_longer(cols = INTEGRIDADE:GERAL, names_to = 'DIMENSAO', values_to = 'NOTA')

data_wider <- data_long %>% 
  pivot_wider(names_from = 'DIMENSAO', values_from = 'NOTA')

# Corrigindo os dados
colnames(data)[1] <- "NOME"
summary(data)

# Histogramas
hist(data$GERAL, main = "Avaliação geral", xlab = "Nota", ylab = "Frequência")
multi.hist(data[,-1])

# Correla??es
Mcor <- cor(data[,-1])
corrplot(Mcor, method = "square", type = "lower", tl.col = "black", tl.srt = 45,
         addCoef.col = "white", tl.cex = 0.7, number.cex = 0.7)

# Investigando
plot(x = data$TECNICO, y = data$GERAL, main = "Relação entre GERAL e TECNICO",
     xlab = "TECNICO", ylab = "GERAL")

# Conte?do extra
# Regressão linear
fit <- lm(data$GERAL ~ data$TECNICO)
summary(fit)
plot(x = data$TECNICO, y = data$GERAL, main = "Relação entre GERAL e TECNICO",
     xlab = "TECNICO", ylab = "GERAL")
abline(fit, col = "blue")
