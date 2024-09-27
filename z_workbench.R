#============================================================================
#### SCRIPT FOR A WORKBENCH ####
#============================================================================
# SETUP ----
## definir notação cientifica off em numeros menores que 1.000.000.000.000 ----
## definir qual o tipo do separador (ponto ou virgula) ----
options(scipen = 10, digits = 10, OutDec = ".")

## packages  ----
{
### basic
library(writexl)               # Salva as tabelas elaboradas em formato .xls
library(openxlsx)              # Salva os dataframes em formato .xlsx (melhor)
library(readxl)                # Reads Microsoft Excel spreadsheets.
library(knitr)                 # tabelas kable
library(kableExtra)            # Build common complex HTML tables and manipulate table styles.
library(readr)                 # A fast and friendly way to read tabular data into R.
library(MASS)                  # visualiza decimal em fracoes
library(xtable)                # transforma tabela Excel para Latex
library(rmarkdown)
library(tinytex)
### data manipulation
library(tidyverse)             # Inclui dplyr, forcats, ggplto2, lubridate, purrr, stringr, tibble, tidyr
library(broom)                 # Converte saídas de modelos estatísticos em tibbles
library(dbplyr)                # Interface dplyr para bancos de dados
library(forcats)               # Ferramentas para trabalhar com fatores
library(googlesheets4)         # Interage com Google Sheets
library(haven)                 # Importa e exporta dados de softwares estatísticos
library(hms)                   # Trabalha com horas, minutos, segundos
library(httr)                  # Facilita requisições HTTP e APIs web
library(jsonlite)              # Parser JSON rápido e robusto
library(lubridate)             # Simplifica trabalho com datas e horas
library(modelr)                # Funções para modelagem em conjunto com dplyr/ggplot2
library(reprex)                # Cria exemplos reproduzíveis facilmente
library(rlang)                 # Ferramentas para programação e metaprogramação em R
library(rvest)                 # Ferramentas para web scraping
library(tibble)                # Versão moderna do data frame do R
library(xml2)                  # Leitura, escrita e processamento de XML
library(tidytext)              # Análise de dados textuais
library(ggthemes)              # Layout gráfico semelhante ao usado no Stata
library(janitor)               # limpeza e manipulacao de dados
library(mosaic)                # padronização de variaveis
library(zoo)                   # Provides the most popular format for saving time series objects in R.
library(xts)                   # Very flexible tools for manipulating time series data sets.
#### statistics
library(stargazer)             # analise estatistica
library(skimr)                 # Compact and flexible summaries of data, a frictionless, pipeable approach to dealing with summary statistic
library(broom)                 # Convert statistical analysis objects into tidy data frames.
library(lmtest)                # Hypothesis testing for linear regression models.
library(modelsummary)          # faz um grafico de intervalo bom
library(strucchange)           # analisar quebras estruturais (Chow Test)
library(mctest)                # teste para multicolinearidad
library(performance)           # Analisa regressão 
library(broom)                 # takes the messy output of built-in functions in R, such as lm, nls, or t.test, and turns them into tidy tibbles.
### databases
library(sidrar)                # Acesso a bases de dados do SIDRA-IGBE
library(ipeadatar)             # Acesso a bases de dados do IPEA
library(rbcb)                  # Acesso a base de dados do BACEN (SGS)
}

# MAINTENANCE ----
## limpar a bancada de trabalho ----
rm(list = ls())
## remover algo específico ----
rm()

# WORKBENCH ----
## settings ----
set.seed()


## work 01 ----
