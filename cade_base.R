
# SETUP ----
# Instalar e carregar pacotes necessários
if(!require(readxl)) install.packages("readxl", dependencies = TRUE)
if(!require(janitor)) install.packages("janitor", dependencies = TRUE)
if(!require(dplyr)) install.packages("dplyr", dependencies = TRUE)
if(!require(lubridate)) install.packages("lubridate", dependencies = TRUE)
if(!require(ggplot2)) install.packages("ggplot2", dependencies = TRUE)
if(!require(tidyr)) install.packages("tidyr", dependencies = TRUE)
if(!require(stringr)) install.packages("stringr", dependencies = TRUE)

library(readxl)
library(janitor)
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)
library(stringr)

rm(list = ls())

# GET DATA ----
# Carregar a base de dados no dataframe df.cade
df.cade <- read_excel('./data/cade.xlsx', sheet = 'database_adj')

# Ajustar os nomes das variáveis para snake_case e remover caracteres especiais
df.cade <- df.cade %>% clean_names()

# Visualizar o resumo do dataframe
summary(df.cade)

# Verificar as classes das colunas de data
str(df.cade)


# CLEAN DATA ---
# Verificar duplicatas na variável numero_do_processo
duplicados <- df.cade %>%
  group_by(numero_do_processo) %>%
  filter(n() > 1)

# Exibir processos duplicados
if(nrow(duplicados) > 0) {
  print("Processos Duplicados:")
  print(unique(duplicados$numero_do_processo))
} else {
  print("Não foram encontrados processos duplicados.")
}

# Excluir duplicatas mantendo a primeira ocorrência
df.cade <- df.cade %>%
  distinct(numero_do_processo, .keep_all = TRUE)
# Remover duplicados
rm(duplicados)

# Filtrar linhas onde conduta_especifica contém 'internacional'
df.cade <- df.cade %>%
  filter(str_detect(conduta_especifica, regex('internacional', ignore_case = TRUE)))


# ANALISYS DATA ---
# Criar o novo dataframe
df.cade.tempo <- df.cade

# Visualizar o resumo do dataframe
summary(df.cade.tempo)

# Verificar as classes das colunas de data
str(df.cade.tempo)

# Ajustar o formato data com o lubridate
df.cade.tempo <- df.cade %>%
  mutate(
    data_de_autuacao = as.Date(data_de_autuacao),
    data_decisao_sg = as.Date(data_decisao_sg),
    data_da_decisao_tr = as.Date(data_da_decisao_tr),
    data_decisao_final = as.Date(data_decisao_final)
    )

# Verificar as classes das colunas de data
str(df.cade.tempo)


# Substituir datas ausentes pela data de hoje
data_hoje <- Sys.Date()
df.cade.tempo <- df.cade.tempo %>%
  mutate(
    data_decisao_sg = coalesce(data_decisao_sg, data_hoje),
    data_da_decisao_tr = coalesce(data_da_decisao_tr, data_hoje),
    data_decisao_final = coalesce(data_decisao_final, data_hoje)
  )

# Calcular tempos de tramitação em dias
df.cade.tempo <- df.cade.tempo %>%
  mutate(
    tempo_decisao_sg = as.numeric(difftime(data_decisao_sg, data_de_autuacao, units = "days")),
    tempo_decisao_tr = as.numeric(difftime(data_da_decisao_tr, data_de_autuacao, units = "days")),
    tempo_decisao_final = as.numeric(difftime(data_decisao_final, data_de_autuacao, units = "days"))
  )


# Preparar dados para o gráfico
df_grafico <- df.cade.tempo %>%
  select(coordenacao_da_sg_responsavel, tempo_decisao_sg, tempo_decisao_tr, tempo_decisao_final) %>%
  pivot_longer(
    cols = starts_with('tempo_'),
    names_to = 'tipo_decisao',
    values_to = 'dias'
  )

# Criar o gráfico
ggplot(df_grafico, aes(x = coordenacao_da_sg_responsavel, y = dias, fill = tipo_decisao)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  labs(
    title = 'Tempos de Tramitação dos Processos em Dias por Coordenação',
    x = 'Coordenação da SG Responsável',
    y = 'Dias',
    fill = 'Tipo de Decisão'
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



