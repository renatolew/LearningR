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

# Gráficos univariados
ggplot(data = df) + 
  aes(x = expVida) + 
  geom_histogram(binwidth = 4)

ggplot(df) + aes(x = continente) + geom_bar()

ggplot(data = df %>% filter(continente %in% c('Africa', 'Americas'))) + 
  aes(x = expVida, fill = continente) + 
  geom_histogram(binwidth = 4, position = 'identity', alpha = 0.5)

ggplot(data = df) + 
  aes(x = pibPerCap, fill = continente) + 
  geom_histogram(bins = 30, position = 'stack', alpha = 0.5)

ggplot(data = df) + 
  aes(x = expVida, fill = continente) + 
  geom_histogram(bins = 30, position = 'identity', alpha = 0.5)

ggplot(data = df) + 
  aes(x = expVida, color = continente) + 
  geom_freqpoly(bins = 45)

ggplot(data = df) + 
  aes(x = expVida, fill = continente, color = continente) + 
  geom_density(adjust = 0.5, alpha = 0.2)

ggplot(df, aes(expVida, color = continente)) + geom_boxplot()

ggplot(data = df) + 
  aes(x = expVida, color = continente) + 
  geom_density(position = 'identity', alpha = 0.5, fill = 'lightblue')


# Gráficos Bivariados

ggplot(df) + 
  aes(x = expVida, y = continente, color = continente) + 
  geom_boxplot(fill = 'grey') + 
  theme(legend.position = 'none')

ggplot(df) + 
  aes(x = pibPerCap, y = expVida) + 
  geom_point()

ggplot( df %>% filter(nacao == 'Australia')) + 
  aes(x = ano, y = pibPerCap) + 
  geom_point()

ggplot( df %>% filter(nacao == 'Australia')) + 
  aes(x = ano, y = pibPerCap) + 
  geom_line(color = 'darkgrey') + 
  geom_point(color = 'blue')

df_count <- df %>%
  filter(ano == 2007) %>%
  group_by(continente) %>%
  summarise(qtde_paises = n())

ggplot(df_count) + 
  aes(x = continente, y = qtde_paises) + 
  geom_col() + 
  coord_flip()

df_count2 <- df %>%
  filter(ano == 1997) %>%
  group_by(continente) %>%
  summarise(expVidaMedia = round(mean(expVida), 1))

ggplot(df_count2) + 
  aes(x = continente, y = expVidaMedia) + 
  geom_col() + 
  geom_text(aes(label = expVidaMedia), vjust = -0.5)
