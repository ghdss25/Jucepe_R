setwd("/home/gustavo/Projeto de Dados/Análise_R/Jucepe")
getwd() 

install.packages("lattice")

library(tidyr)
library(dplyr)
library(esquisse)
library(ggplot2)

jucepe_abertura <- read.csv("jucepe_abertura.csv", sep = ";")
jucepe_alteracao <- read.csv("jucepe_alteracao.csv", sep = ";")
jucepe_fechamento <- read.csv("jucepe_encerramento.csv", sep = ";")

View(jucepe_abertura)

summary(jucepe_abertura)

typeof(jucepe_abertura$ANO)

## Total e Média de Aberturas por Ano e Mês da Jucepe 

abert_Ano <- jucepe_abertura %>% group_by(ANO) %>% summarise(Total = sum(TOTAL), Média = mean(TOTAL, round(2)))
View(abert_Ano) 

abert_Mes$MES <- as.character(abert_Mes$MES)
typeof(Mes)

abert_Mes$MES

abert_Meses <- jucepe_abertura %>% group_by(MES) %>% summarise(Total = sum(TOTAL), Média = mean(TOTAL, round(2)))
View(abert_Mes)

## Valor Total de Abertura por Ano 
sum(jucepe_abertura$TOTAL)
mean(jucepe_abertura$TOTAL, round(2))

#################################### Gráficos ################################

## 1 Gráfico - Média de Abertura por Ano 

esquisser(abert_Ano)

library(ggplot2)

ggplot(abert_Ano) +
 aes(x = ANO, y = Média) +
 geom_area(size = 1.5) +
 labs(title = "Média de Abertura por Ano", 
 subtitle = "Relatório de Aberturas") +
 theme_minimal()

## 2 Gráfico - Total de Abertura por Ano 

data <- data.frame(
  
  Ano = abert_Ano$ANO, 
  Total = abert_Ano$Total
  
)

ggplot(data, aes(x=Ano, y=Total)) +
  labs(title = "Total de Abertura por Ano") +
  geom_bar(stat = "identity")

# 3 Gráfico - Total de Abertura por Mês

esquisser(abert_Mes)

library(ggplot2)

ggplot(abert_Mes) +
 labs(title = "Total de Abertura por Mês") +
 aes(x = MES, weight = Total) +
 geom_bar(fill = "#2D4611") +
 theme_minimal()

# Gráfico - Média de Abertura por Mês

esquisser(abert_Mes)

library(ggplot2)

ggplot(abert_Mes) +
  aes(x = MES, weight = Média) +
  geom_bar(fill = "#FDCE1C") +
  labs(title = "Abertura Média por Mês", 
       subtitle = "Relatório dos Meses") +
  theme_minimal()

# 4 Gráfico - Total de Abertura por Mês

ggplot(abert_Mes) +
 aes(x = MES, weight = Total) +
 geom_bar(fill = "#9400D3") +
 labs(title = "Abertura Total por Mês ", 
 subtitle = "Relatório dos Meses") +
 theme_minimal()

######################### Alteração da Jucepe #################################

## Agrupamento 

View(jucepe_alteracao)

alt_ano = jucepe_alteracao %>% group_by(ANO) %>% summarise(Total = sum(TOTAL), Média = mean(TOTAL, round(2)))
alt_mes = jucepe_alteracao %>% group_by(MES) %>% summarise(Total = sum(TOTAL), Média = mean(TOTAL, round(2)))

View(alt_ano)

## Transformando a coluna Ano e Mês em Caractere

alt_ano$ANO <- as.character(alt_ano$ANO)
typeof(alt_ano)

alt_ano$ANO

View(alt_ano)

alt_mes$MES <- as.character(alt_mes$MES)
typeof(alt_mes)

alt_mes$MES

View(alt_mes)

## Gráficos para a parte de Ano e Mês

# 5 Gráfico - Média de Alteração de Receita por Ano

esquisser(alt_ano)

library(ggplot2)

ggplot(alt_ano) +
 aes(x = ANO, weight = Média) +
 geom_bar(fill = "#FF8C00") +
 labs(title = "Média de Alteração de Receita por Ano", 
 subtitle = "Relatório de Alterações") +
 theme_dark()

# 6 Gráfico -Alteração de Receita Total por Ano

library(ggplot2)

ggplot(alt_ano) +
 aes(x = ANO, weight = Total) +
 geom_bar(fill = "#B22222") +
 labs(title = "Alteração de Receita Total por Ano", 
 subtitle = "Relatório de Receitas") +
 theme_minimal()

# 7 Gráfico - Alteração de Receita Total por Mês 

esquisser(alt_mes)

library(ggplot2)

ggplot(alt_mes) +
 aes(x = MES, weight = Total) +
 geom_bar(fill = "#228B22") +
 labs(title = "Alteração de Receita Total por Mês ", 
 subtitle = "Relatório de Alterações de Receitas") +
 theme_minimal()

# 8 Gráfico - Média de Alteração por Mês
esquisser(alt_mes)

library(ggplot2)

ggplot(alt_mes) +
 aes(x = MES, y = Média) +
 geom_boxplot(fill = "#4682B4") +
 labs(title = "Média de Alteração por Mês", 
 subtitle = "Relatório de Alterações") +
 theme_minimal()

######################### Fechamento da Jucepe #################################

## Agrupamento 

View(jucepe_fechamento)

fec_ano = jucepe_fechamento %>% group_by(ANO) %>% summarise(Total = sum(TOTAL), Média = mean(TOTAL, round(2)))
fec_mes = jucepe_fechamento %>% group_by(MES) %>% summarise(Total = sum(TOTAL), Média = mean(TOTAL, round(2)))

View(alt_ano)

## Transformando a coluna Ano e Mês em Caractere

fec_ano$ANO <- as.character(fec_ano$ANO)
typeof(alt_ano)

fec_ano$ANO

View(fec_ano)

fec_mes$MES <- as.character(fec_mes$MES)
typeof(fec_mes)

fec_mes$MES

View(fec_mes)

#################################### Gráficos ################################

# 9 Gráfico - Total de Fechamento por Mês

data <- data.frame(
  
  Mes <- fec_mes$MES, 
  Total <- fec_mes$Total,
  Media <- fec_mes$Média
)

data %>% 
  
  arrange(Total) %>% 
  mutate(Mes=factor(Mes, levels = Mes)) %>% 
  ggplot(aes(x=Mes, y=Total)) + 
  labs(title = "Total de Fechamento por Mês") +
  geom_segment(aes(xend=Mes, yend=0)) +
  geom_point(size=4, color="orange") + 
  coord_flip() + 
  theme_bw() + 
  xlab("")

# 10 Gráfico - Média de Fechamento por Mês

data %>% 
  
  arrange(Media) %>%
  mutate(Mes = factor(Mes, levels = c(Mes))) %>%
  ggplot(aes(x=Mes, y=Media)) +
  labs(title = "Média de Fechamento por Mês") +
  geom_segment(aes(xend=Mes, yend=0)) + 
  geom_point(size=4, color="blue") + 
  theme_bw() + xlab("")
