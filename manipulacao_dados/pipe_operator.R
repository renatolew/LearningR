setwd("~/Aulas/Projetos/LearningR/manipulacao_dados")

library(dplyr)

load('../dataframes/df_tratado.RData')

# Pipe Operator = %>%

colnames(df)

df_tratado <- df[c('Survived', 'Pclass', 'Sex', 'Age', 'Tarifa')]

df_tratado2 <- df %>%
  select(Survived, Pclass, Sex, Age, Tarifa)

hist(df_tratado$Tarifa)

df2 <- df %>%
  select(-Siblings.Spouses.Aboard, -Parents.Children.Aboard, -Name) %>%
  filter(Tarifa < 200) %>%
  mutate(Idade_meses = Age * 12,
         Peso = Survived * Pclass)

df_agrupado <- df2 %>%
  group_by(Pclass) %>%
  summarise(Ticket_medio = mean(Tarifa))

barplot(df_agrupado$Ticket_medio, names.arg = df_agrupado$Pclass)

df_agr2 <- df2 %>%
  group_by(Survived, Sex) %>%
  summarise(qtde = n(),
            ticket_medio = mean(Tarifa))

original <- c(1, 2, 3)
novo_dado <- c('Primeira', 'Segunda', 'Terceira')
aleatorio <- c(4, 45, 456)

auxiliar <- data.frame(original, novo_dado, aleatorio)

df_ajust <- df2 %>%
  mutate(Sobrevivente = ifelse(Survived == 1, 'Sim','Nao')) %>%
  left_join(auxiliar[c('original', 'novo_dado')]
            , by = c('Pclass' = 'original')) %>%
  rename(Classe = 'novo_dado') %>%
  select(-Survived, -Pclass) %>%
  group_by(Sobrevivente, Classe) %>%
  summarise(qtde = n(), 
            total_pago = sum(Tarifa)) %>%
  mutate(media_pessoa = total_pago/qtde)

data <- read.csv('mpg_exercicio.csv')
