# ANALISE DOS PROCESSOS DO CADE ENVOLVENDO CARTEIS INTERNACIONAIS #######
# CONFIG ----
# Instalar e carregar pacotes necessários
if(!require(readxl)) install.packages("readxl", dependencies = TRUE)
if(!require(janitor)) install.packages("janitor", dependencies = TRUE)
if(!require(dplyr)) install.packages("dplyr", dependencies = TRUE)
if(!require(lubridate)) install.packages("lubridate", dependencies = TRUE)
if(!require(ggplot2)) install.packages("ggplot2", dependencies = TRUE)
if(!require(tidyr)) install.packages("tidyr", dependencies = TRUE)
if(!require(stringr)) install.packages("stringr", dependencies = TRUE)
if(!require(purrr)) install.packages("stringr", dependencies = TRUE)
if (!require(writexl)) install.packages("writexl", dependencies = TRUE)


library(readxl)
library(janitor)
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)
library(stringr)
library(purrr)
library(knitr)
library(writexl)

# remove all ----
rm(list = ls())

# OBTENDO DADOS -----
# Carregar a base de dados no dataframe df.cade
df.cade <- read_excel('./data/cade.xlsx', sheet = 'database_adj')
# Visualizar o resumo do dataframe
summary(df.cade)
# Verificar as classes das colunas de data
str(df.cade)

# LIMPANDO DADOS ----
# Ajustar os nomes das variáveis para snake_case e remover caracteres especiais
df.cade <- df.cade %>% clean_names()

# Verificar duplicatas na variável numero_do_processo
duplicados <- df.cade %>%
  group_by(numero_do_processo) %>%
  filter(n() > 1)

# Função para verificar se todas as colunas das linhas duplicadas são idênticas
comparar_duplicados <- function(grupo) {
  # Identificar as colunas que têm valores diferentes
  diferentes <- apply(grupo, 2, function(x) length(unique(x)) > 1)
  if (all(!diferentes)) {
    return("Idêntica")
  } else {
    # Retornar as colunas que têm valores diferentes
    return(paste("Diferente nas colunas:", paste(names(grupo)[diferentes], collapse = ", ")))
  }
}

# Adicionar uma coluna para identificar se os dados são idênticos ou diferentes
if(nrow(duplicados) > 0) {
  print("Processos Duplicados:")
  print(unique(duplicados$numero_do_processo))
  
  duplicados_status <- duplicados %>%
    group_by(numero_do_processo) %>%
    nest() %>%
    mutate(status = map_chr(data, comparar_duplicados)) %>%
    select(numero_do_processo, status)
  # Exibir o status dos duplicados e onde estão as diferenças
  print(duplicados_status)
  # Exibir as diferenças detalhadas para os processos marcados como "Diferente"
  duplicados_diferentes <- duplicados_status %>%
    filter(str_detect(status, "Diferente"))
  
  if (nrow(duplicados_diferentes) > 0) {
    for (processo in duplicados_diferentes$numero_do_processo) {
      print(paste("Diferenças encontradas no processo:", processo))
      # Exibir as linhas do processo específico que possuem diferenças
      print(df.cade %>% filter(numero_do_processo == processo))
    }
  } else {
    print("Não foram encontradas diferenças nos processos duplicados.")
  }
  
} else {
  print("Não foram encontrados processos duplicados.")
}
# Excluir duplicatas mantendo a primeira ocorrência
df.cade <- df.cade %>%
  distinct(numero_do_processo, .keep_all = TRUE)
# Remover duplicados
rm(duplicados)

write_xlsx(df.cade, path = "./output/cartel_internacional.xlsx")

# Filtrar linhas onde conduta_especifica contém 'internacional'
df.cade <- df.cade %>%
  filter(str_detect(conduta_especifica, regex('internacional', ignore_case = TRUE)))

# Ajustar o formato data com o lubridate
df.cade <- df.cade %>%
  mutate(
    data_de_autuacao = as.Date(data_de_autuacao),
    data_decisao_sg = as.Date(data_decisao_sg),
    data_da_decisao_tr = as.Date(data_da_decisao_tr),
    data_decisao_final = as.Date(data_decisao_final)
  )

# Visualizar o resumo do dataframe
summary(df.cade)
# Verificar as classes das colunas de data
str(df.cade)


# ANALISANDO DADOS ----
## Calcular quantidade de "Sim", "Não" e Percentual para cada métrica

## Quantidade e percentual de Leniência ----
### Considera processos cuja 'origem' é 'Leniência'
leniencia_counts <- df.cade %>%
  summarise(
    Sim = sum(str_detect(origem, regex('leniência', ignore_case = TRUE))),
    Nao = n() - Sim,
    Percentual = Sim / n() * 100
  )

## Quantidade e percentual de Trânsito em Julgado ----
### Considera processos com 'transito_em_julgado' igual a 'Sim'
transito_counts <- df.cade %>%
  summarise(
    Sim = sum(str_detect(transito_em_julgado, regex('sim', ignore_case = TRUE))),
    Nao = n() - Sim,
    Percentual = Sim / n() * 100
  )

## Quantidade e percentual de Arquivamento ----
### Considera processos cuja 'teor_da_decisao_sg' ou 'teor_da_decisao_tr' é 'Arquivamento'
arquivamento_counts <- df.cade %>%
  summarise(
    Sim = sum(str_detect(teor_da_decisao_sg, regex('arquivamento', ignore_case = TRUE)) | 
                str_detect(teor_da_decisao_tr, regex('arquivamento', ignore_case = TRUE))),
    Nao = n() - Sim,
    Percentual = Sim / n() * 100
  )

## Quantidade e percentual de Condenação ----
### Considera processos cuja 'teor_da_decisao_sg' ou 'teor_da_decisao_tr' é 'Condenação'
condenacao_counts <- df.cade %>%
  summarise(
    Sim = sum(str_detect(teor_da_decisao_sg, regex('condenação', ignore_case = TRUE)) | 
                str_detect(teor_da_decisao_tr, regex('condenação', ignore_case = TRUE))),
    Nao = n() - Sim,
    Percentual = Sim / n() * 100
  )

## Quantidade e percentual de Multas Aplicadas ----
### Considera processos cuja 'multas_aplicadas' é maior que 0
multas_counts <- df.cade %>%
  summarise(
    Sim = sum(multas_aplicadas > 0),
    Nao = n() - Sim,
    Percentual = Sim / n() * 100
  )

# OUTPUT ----
## Criar um dataframe com os resultados ----
df_resultados <- data.frame(
  "Variável" = c("Leniência", "Trânsito em julgado", "Arquivamento", "Condenação", "Multas aplicadas"),
  "Não" = c(leniencia_counts$Nao, transito_counts$Nao, arquivamento_counts$Nao, condenacao_counts$Nao, multas_counts$Nao),
  "Sim" = c(leniencia_counts$Sim, transito_counts$Sim, arquivamento_counts$Sim, condenacao_counts$Sim, multas_counts$Sim),
  "Percentual (%)" = c(leniencia_counts$Percentual, transito_counts$Percentual, arquivamento_counts$Percentual, condenacao_counts$Percentual, multas_counts$Percentual)
)
df_resultados

## Exportar para um arquivo .xlsx ----
write_xlsx(df_resultados, path = "./output/percentuais_cartel_internacional.xlsx")

