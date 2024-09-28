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
## analisar o tempo de tramitação de cada um dos processos de cartéis internacionais

# 1. Calcular o tempo em dias decorrido da data_de_autuacao até a data_decisao_sg ----
df.cade <- df.cade %>%
  mutate(tempo_autuacao_decisao_sg = as.numeric(difftime(data_decisao_sg, data_de_autuacao, units = "days")))

# 2. Calcular o tempo em dias decorrido entre a data_decisao_sg até a data_da_decisao_tr ----
df.cade <- df.cade %>%
  mutate(tempo_decisao_sg_decisao_tr = as.numeric(difftime(data_da_decisao_tr, data_decisao_sg, units = "days")))

# Verificar o resumo do tempo de tramitação dos processos
summary(df.cade$tempo_autuacao_decisao_sg)
summary(df.cade$tempo_decisao_sg_decisao_tr)

## Tempo em dias decorrido da data_de_autuacao até a data_decisao_sg agregado ----
tempo_agregado_autuacao_decisao_sg <- df.cade %>%
  summarise(tempo_medio = mean(tempo_autuacao_decisao_sg, na.rm = TRUE),
            tempo_minimo = min(tempo_autuacao_decisao_sg, na.rm = TRUE),
            tempo_maximo = max(tempo_autuacao_decisao_sg, na.rm = TRUE))

print("Tempo agregado (médio, mínimo e máximo) entre a autuação e decisão SG:")
print(tempo_agregado_autuacao_decisao_sg)

## Tempo em dias decorrido da data_de_autuacao até a data_decisao_sg desagregado por coordenacao_da_sg_responsavel ----
tempo_desagregado_por_coordenacao <- df.cade %>%
  group_by(coordenacao_da_sg_responsavel) %>%
  summarise(tempo_medio = mean(tempo_autuacao_decisao_sg, na.rm = TRUE),
            tempo_minimo = min(tempo_autuacao_decisao_sg, na.rm = TRUE),
            tempo_maximo = max(tempo_autuacao_decisao_sg, na.rm = TRUE),
            n = n()) %>%
  arrange(desc(tempo_medio))

print("Tempo de tramitação desagregado por coordenação responsável:")
print(tempo_desagregado_por_coordenacao)

## Tempo em dias decorrido entre a data_decisao_sg até a data_da_decisao_tr agregado ----
tempo_agregado_decisao_sg_decisao_tr <- df.cade %>%
  summarise(tempo_medio = mean(tempo_decisao_sg_decisao_tr, na.rm = TRUE),
            tempo_minimo = min(tempo_decisao_sg_decisao_tr, na.rm = TRUE),
            tempo_maximo = max(tempo_decisao_sg_decisao_tr, na.rm = TRUE))

print("Tempo agregado (médio, mínimo e máximo) entre a decisão SG e decisão TR:")
print(tempo_agregado_decisao_sg_decisao_tr)


# OUTPUT ----
## Exportar tabela com os tempos apurados para um arquivo Excel

# Criar um dataframe com os tempos apurados
df_tempos_apurados <- data.frame(
  "Coordenacao SG" = tempo_desagregado_por_coordenacao$coordenacao_da_sg_responsavel,
  "Tempo Médio Autuação-Decisão SG (dias)" = tempo_desagregado_por_coordenacao$tempo_medio,
  "Tempo Mínimo Autuação-Decisão SG (dias)" = tempo_desagregado_por_coordenacao$tempo_minimo,
  "Tempo Máximo Autuação-Decisão SG (dias)" = tempo_desagregado_por_coordenacao$tempo_maximo,
  "Número de Processos" = tempo_desagregado_por_coordenacao$n
)
df_tempos_apurados

# Exportar para um arquivo .xlsx
write_xlsx(df_tempos_apurados, path = "./output/tempos_tramitacao_cade.xlsx")
write_xlsx(df.cade, path = "./output/dfcade2.xlsx")
