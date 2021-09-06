# Primeiros passos
setwd("~/Aulas/Projetos/LearningR/starting_projects")
rm(list=ls())
library(ggplot2)
library(tidyverse)
library(colorBlindness)

# Dados
load("saved_dt1.RData")

cores_cat <- c('gray70', '#5288db')
cores <- colorRampPalette(c('#4d4d4d', 'white', '#5288db'))

# Criando um gráfico de dispersão
ggplot(data, aes(x=satisfacao, y=ultima_avaliacao)) +
  geom_point(aes(colour=saiu), alpha=0.5, size=2) + 
  scale_color_manual(values = cores_cat) + 
  labs(colour="Saiu", x="Satisfação", y= "Avaliação",
       title="Status de colaboradores em função da avalição e satisfação") +
  theme_light()

# Facetando por salário e número de projetos
ggplot(data, aes(satisfacao, ultima_avaliacao)) +
  geom_point(aes(fill=saiu, colour=saiu), alpha=.5) +
  scale_color_manual(values = cores_cat) + 
  facet_grid(salario ~ volume_projetos, labeller = label_both) +
  theme_light() + labs(x = "Satisfação", y = "Última avaliação")

# Facetando por número de projetos e tempo de cia
ggplot(data) + geom_point(aes(satisfacao, ultima_avaliacao, fill=saiu,
                              colour=saiu), alpha=.5, position = "identity") +
  scale_color_manual(values = cores_cat) + 
  facet_grid(tempo_cia ~ volume_projetos, labeller = label_both) +
  theme_light() +
  labs(x = "Satisfação", y = "Última avaliação")

# Explorando outras possibilidades

ggplot(data) + geom_point(aes(satisfacao, ultima_avaliacao, fill=saiu,
                              colour=saiu), alpha=.5, position = "identity", size = 3) +
  scale_color_manual(values = cores_cat) + 
  facet_grid(. ~ volume_projetos, labeller = label_both) +
  theme_light() +
  labs(x = "Satisfação", y = "Última avaliação")

# Gráfico de densidade de probabilidade
data_dens <- data[data$saiu == "Saiu",]
hm <- ggplot(data_dens, aes(x=satisfacao, y=ultima_avaliacao)) +
  stat_density2d(geom = "raster", aes(fill = ..density..), contour = FALSE) +
  # scale_fill_distiller(palette = "RdYlBu") + 
  scale_fill_gradientn(colours = cores(100)) + 
  labs(fill="Dens. de prob. de saída",
       x="Satisfação",
       y= "Avaliação",
       title="Status de colaboradores em função da avaliação e satisfação") +
  theme_light()

# Checando se o gráfico é amigável para o público dautônico
cvdPlot(hm)

# Transformando os dados para um gráfico de barras
library(dplyr)
dt1 <- data
dt1$volume_projetos <- as.factor(dt1$volume_projetos)
dt1$tempo_cia <- as.factor(dt1$tempo_cia)
dt1 <- dt1 %>% select(saiu, tempo_cia, volume_projetos) %>%
  group_by(saiu, tempo_cia, volume_projetos) %>%
  summarise(Qtde = n())

#Criando um gráfico de barras
ggplot(dt1) +
  geom_col(aes(y = Qtde, x = saiu, fill = saiu), position = "dodge") +
  geom_text(aes(y = Qtde, x = saiu, label=Qtde, group = saiu, colour=saiu),
            position = position_dodge(width = 1), vjust = 0) +
  facet_grid(tempo_cia ~ volume_projetos, labeller = label_both) + theme_light()

#Criando um boxplot para satisfação
dx1 <- data %>%
  filter(volume_projetos %in% c(4,5,6)) %>%
  filter(tempo_cia %in% c(4,5,6)) %>%
  select(satisfacao,saiu, tempo_cia, volume_projetos) %>%
  mutate(var = "satisfacao", value = satisfacao)

ggplot(dx1) + geom_boxplot(aes(y = value, x = var, fill=saiu,
                               colour=saiu), alpha=.5, position = "dodge") +
  facet_grid(tempo_cia ~ volume_projetos, labeller = label_both) + theme_light()

#Criando um boxplot para última avaliação
dx2 <- data %>%
  filter(volume_projetos %in% c(4,5,6)) %>%
  filter(tempo_cia %in% c(4,5,6)) %>%
  select(ultima_avaliacao,saiu, tempo_cia, volume_projetos) %>%
  mutate(var = "ultima_avaliacao", value = ultima_avaliacao)

ggplot(dx2) + geom_boxplot(aes(y = value, x = var, fill=saiu,
                               colour=saiu), alpha=.5, position = "dodge") +
  facet_grid(tempo_cia ~ volume_projetos, labeller = label_both) + theme_light()

#Criando um único boxplot para comparação das duas variáveis
dx3 <- rbind(dx1[,-c(1)], dx2[,-c(1)])
dx3$tempo_cia <- as.factor(dx3$tempo_cia)
dx3$volume_projetos <- as.factor(dx3$volume_projetos)

ggplot(dx3) + geom_boxplot(aes(y = value, x = var, fill=saiu,
                               colour=saiu), alpha=.5, position = "dodge") +
  facet_grid(tempo_cia ~ volume_projetos, labeller = label_both) + theme_light()

#Criando um gráfico ridgeline
data_ridge <- dx3 %>% mutate(ridge = paste(saiu, var, sep = " - "))
library(ggridges)
ggplot(data_ridge, aes(x = value, y = ridge, alpha = 0.5, fill=saiu, colour = saiu)) +
  geom_density_ridges() +
  theme_ridges() +
  theme(legend.position = "none") +
  facet_grid(tempo_cia ~ volume_projetos, labeller = label_both)


