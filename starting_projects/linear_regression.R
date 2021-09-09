# Primeiros passos
setwd("~/Aulas/Projetos/LearningR/starting_projects")
rm(list=ls())

# Bibliotecas
library(corrplot)
library(dplyr)

# Dados
aval_desmp <- read.csv('avaliacao_desempenho.csv')

# Dividindo o banco entre os melhores e piores em performance de acordo com a matriz ninebox.
L_sup <- aval_desmp %>% 
  filter(ninebox >= 7)

L_inf <- aval_desmp %>% 
  filter(ninebox <= 3)

# Confirmando se a média das amostrar é significativamente diferente nos quisitos idade e tempo de casa.
mean(L_sup$idade)
mean(L_inf$idade)

t.test(L_sup$idade, L_inf$idade)

mean(L_sup$tempo_casa)
mean(L_inf$tempo_casa)

t.test(L_sup$tempo_casa, L_inf$tempo_casa)


# Plotando resultados para observar padrões.

ggplot(aval_desmp) + 
  aes(x = idade) + 
  geom_boxplot()


ggplot(aval_desmp) + 
  aes(x = tempo_casa) + 
  geom_boxplot()


df_num <- aval_desmp %>% 
  select_if(is.numeric)

mcor <- cor(df_num)

corrplot(mcor, method = "color", type = "lower", tl.col = "black", tl.srt = 45,
         addCoef.col = "black", tl.cex = 0.7, number.cex = 0.7)

# Criando modelos de regressão linear para entender a influência de cada variável

modelo1 <- lm(ninebox ~ idade, data = aval_desmp)
summary(modelo1)

modelo2 <- lm(ninebox ~ idade + genero, data = aval_desmp)
summary(modelo2)





