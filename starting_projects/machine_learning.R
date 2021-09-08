#Iniciando
setwd("~/Aulas/Projetos/LearningR/starting_projects")
rm(list=ls())

#Bibliotecas
library(rpart)
library(rpart.plot)
library(caret)
library(tidyverse)
library(lattice)
library(e1071)
library(ggplot2)

#Unindo bases de dados - apenas um exemplo não relacionado a machine learning para referências futuras
df_ag <- read.csv('avaliacao_desempenho.csv')
df_ag <- df_ag[,-1] #Apagando primeira coluna (índice sem valor semântico)

df_comeco <- df_ag %>% 
  select(ID:hora_extra)

df_fim <- df_ag %>% 
  select(ID, idade:ninebox)

df_unido <- left_join(df_comeco, df_fim, by = c('ID' = 'ID'))
df_unido_alt <- df_comeco %>%
  left_join(df_fim, by = c('ID' = 'ID'))

#Dados

load("saved_dt.RData")
str(data)

data$saiu <- factor(data$saiu)


#Treinamento e teste
train_index <- sample(seq, 0.75 * nrow(data))
smpsize <- floor(0.75*nrow(data))
set.seed(76)
seq <- seq_len(nrow(data))
train_ind <- sample(seq, size = smpsize)
train <- data[train_ind,]
test <- data[-train_ind,]

#Modelo
rpartmodel <- rpart(saiu ~ ., data = train, method = "class")
rpart.plot(rpartmodel, cex = 0.75)
rpart.plot::rpart.plot(rpartmodel, sub = "Árvore de decisão", min.inter.height = 10)

#Avaliando o modelo
predictions <- predict(rpartmodel, test, type = "class")
hr_model_tree <- cbind(test, predictions)
conf_matrix <- confusionMatrix(hr_model_tree$predictions,factor(hr_model_tree$saiu))
conf_matrix

#Salvando este modelo de machine learning caso queira utiliza-lo futuramente na mesma empresa.
save(rpartmodel, file = 'modelo_predicao_turnover.RData')

#Segundo modelo
rpartmodel2 <- rpart(saiu ~ ., data = train, method = "class",
                     control = rpart.control(minsplit=2, cp=0))
rpart.plot::rpart.plot(rpartmodel2, sub = "Árvore de decisão")

#Avaliando o modelo 2
predictions2 <- predict(rpartmodel2, test, type = "class")
hr_model_tree2 <- cbind(test, predictions2)
confusionMatrix2 <- confusionMatrix(hr_model_tree2$predictions2, factor(hr_model_tree2$saiu))
confusionMatrix2

#Salvando o modelo
save(rpartmodel, file = "modelo.RData")

#Aplicando o modelo
load("modelo.RData")
data_mod <- read.csv("Pred.csv")
previsoes <- predict(rpartmodel, data_mod, type = "class")
data_mod <- cbind(data_mod,previsoes)
write.csv(data_mod, "Previsões.csv")
