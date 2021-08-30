# Ambiente
setwd("~/Aulas/Projetos/LearningR/ggplot")

# Bibliotecas
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(cowplot)

# Temas
tema_grafico <- theme(panel.grid = element_blank(),
                      panel.background = element_rect(fill = '#FFFFFF'),
                      strip.background = element_rect(fill = '#F3F3F3', color = '#D4D4D4'),
                      axis.text = element_text(color = 'blue', family = 'verdana', size = 12),
                      axis.title = element_text(color = 'red'),
                      legend.position = 'right')

tema2 <- function(cor_eixo = 'blue') {
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = '#FFFFFF'),
        strip.background = element_rect(fill = '#F3F3F3', color = '#D4D4D4'),
        axis.text = element_text(color = cor_eixo, family = 'verdana', size = 12),
        axis.title = element_text(color = 'red'),
        legend.position = 'right')
}

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

ggplot(df %>% filter(ano == 2007)) + 
  aes(x = pibPerCap, y = expVida, color = continente) + 
  geom_point(aes(size = pop), alpha = 0.5) + 
  scale_x_log10() + 
  scale_size(range = c(1, 10))

ggplot(df %>% filter(ano %in% c(min(df$ano), median(df$ano), max(df$ano)), continente != 'Oceania')) + 
  aes(x = pibPerCap, y = expVida, color = continente) + 
  geom_point(aes(size = pop), alpha = 0.5) + 
  scale_x_log10() + 
  scale_size(range = c(1, 10)) + 
  facet_wrap(~continente) + 
  theme(legend.position = 'none')

ggplot(df %>% filter(ano == 2007, continente != 'Oceania')) + 
  aes(x = pibPerCap, y = expVida, color = continente) + 
  geom_point(aes(size = pop), alpha = 0.5) + 
  scale_x_log10() + 
  guides(size = F) + 
  scale_size(range = c(1, 20)) + 
  facet_wrap(~continente)

ggplot(df %>% filter(ano %in% c(min(df$ano), 1982, max(df$ano)), continente != 'Oceania')) + 
  aes(x = pibPerCap, y = expVida, color = continente) + 
  geom_point(aes(size = pop), alpha = 0.5) + 
  scale_x_log10() + 
  guides(size = F) + 
  scale_size(range = c(1, 20)) + 
  facet_grid(ano ~ continente)

ggplot(df %>% filter(ano %in% c(min(df$ano), 1982, max(df$ano)), continente != 'Oceania')) + 
  aes(x = pibPerCap, y = expVida, color = continente) + 
  geom_point(aes(size = pop), alpha = 0.5) + 
  scale_x_log10() + 
  guides(size = F) + 
  scale_size(range = c(1, 20)) + 
  facet_grid(continente ~ .)

ggplot(df %>% filter(ano %in% c(min(df$ano), 1982, max(df$ano)), continente != 'Oceania')) + 
  aes(x = factor(ano), y = expVida, color = continente) + 
  geom_boxplot() + 
  geom_jitter(alpha = 0.5, width = 0.25) + 
  facet_wrap(~continente)
  

ggplot(df %>% filter(ano %in% c(min(df$ano), 1982, max(df$ano)), continente != 'Oceania')) + 
  aes(x = factor(ano), y = expVida, color = continente) + 
  geom_violin() + 
  geom_jitter(alpha = 0.5, width = 0.25) + 
  facet_wrap(~continente)



# display.brewer.all() para visualizar paletas disponíveis.
ggplot(df %>% filter(ano %in% c(min(df$ano), 1982, max(df$ano)), continente != 'Oceania')) + 
  aes(x = factor(ano), y = expVida, color = continente) + 
  geom_violin() + 
  geom_jitter(alpha = 0.5, width = 0.25) + 
  facet_wrap(~continente) + 
  scale_color_brewer(palette = 'Dark2')

# scale_color_manual para definir a própria paleta de cores.
cores <- c('#ababab', "#5288db", "#A82311", "#51db6f")
ggplot(df %>% filter(ano %in% c(min(df$ano), 1982, max(df$ano)), continente != 'Oceania')) + 
  aes(x = factor(ano), y = expVida, fill = continente) + 
  geom_violin() + 
  geom_jitter(alpha = 0.5, width = 0.25, shape = 21) + 
  facet_wrap(~continente) + 
  scale_color_manual(values = cores)


ggplot(df %>% filter(ano %in% c(min(df$ano), 1982, max(df$ano)), continente != 'Oceania')) + 
  aes(x = factor(ano), y = expVida) + 
  geom_violin(aes(color = continente)) + 
  geom_jitter(aes(fill = continente), alpha = 0.5, width = 0.25, shape = 21) + 
  facet_wrap(~continente) + 
  scale_fill_manual(values = cores) + 
  scale_color_manual(values = cores) + 
  labs(title = 'Evolução da expectativa de vida', x = 'Ano', y = 'Expectativa de Vida', fill = 'Continente', color = 'Continente') + 
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = '#FFFFFF'),
        strip.background = element_rect(fill = '#F3F3F3', color = '#D4D4D4'),
        axis.text = element_text(color = 'blue', family = 'verdana', size = 12),
        axis.title = element_text(color = 'red'),
        legend.position = 'right')

# Utilizando tema salvo para tornar código menos verboso.
ggplot(df %>% filter(ano %in% c(min(df$ano), 1982, max(df$ano)), continente != 'Oceania')) + 
  aes(x = factor(ano), y = expVida) + 
  geom_boxplot(aes(color = continente)) + 
  tema2(cor_eixo = 'black')

ggplot(df) + 
  aes(x = pop, y = expVida) + 
  geom_point(aes(color = continente), alpha = 0.5)

# Salvando o padrão do gráfico em uma função. 
grafico_dispersao <- function(x, y, cor = continente, data = df, eixo = 'blue') {
  ggplot(data) + 
    aes({{x}}, {{y}}) + 
    geom_point(aes(color = {{cor}}), alpha = 0.5) + 
    tema2(cor_eixo = eixo)
}

grafico_dispersao(pop, expVida)  

grafico_dispersao(ano, pop)

# Exercício final:

plot1 <- ggplot(df %>% filter(nacao %in% c('Brazil', 'China', 'Japan'))) + 
  aes(x = ano, y = pop, color = nacao) + 
  geom_line() + geom_point(color = 'orange', alpha = 0.5) + 
  scale_color_manual(values = c("#5288db", "#00BBAB", "#4d4d4d")) + 
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = 'white'))

plot2 <- ggplot(df %>% filter(nacao %in% c('Brazil', 'China', 'Japan'))) + 
  aes(x = ano, y = pibPerCap, color = nacao) + 
  geom_line() + geom_point(color = 'orange', alpha = 0.5) + 
  scale_color_manual(values = c("#5288db", "#00BBAB", "#4d4d4d")) +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = 'white'),
        legend.position = 'none')

plot_grid(plot2, plot1, rel_widths = c(0.45, 0.55))
