# Ambiente
setwd("~/Aulas/Projetos/LearningR/ggplot")

# Bibliotecas
library(dplyr)
library(ggplot2)

# Dados
df <- read.csv('gapminder.csv')
summary(df)
str(df)

# Método 1 para transformar caracteres em fatores
df$nacao <- as.factor(df$nacao)

# Método 2 para transformar caracteres em fatores
df <- df %>%
  mutate(continente = as.factor(continente))

