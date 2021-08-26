setwd("~/Aulas/Projetos/LearningR/dataframes")

library(openxlsx)

# Dataframes

idade <- c(21, 25, 23, 28)
alturas <- c(1.75, 1.62, 1.69, 1.83)

df <- data.frame(idade, alturas)

colnames(df) <- c('idades', 'altura')
colnames(df)[1] <- 'idade'
rownames(df) <- c('Jose', 'Rafael', 'Maria', 'Rita')
rownames(df) <- NULL


df_titanic <- read.table('titanic_R.csv',
                         header = TRUE,
                         sep = ',',
                         quote = '',
                         stringsAsFactors = FALSE
                         )

df_titanic2 <- read.csv('titanic_R.csv')

df_excel <- read.xlsx('nomes_idades.xlsx')

head(df_titanic)

df_excel$Idade

df_excel['Idade']

df_excel[4 , 'Altura']

df_excel[1:3, 'Altura']
