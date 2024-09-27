
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
library(purrr)

rm(list = ls())

# GET DATA -----
# Carregar a base de dados no dataframe df.cade
df.cade <- read_excel('./data/cade.xlsx', sheet = 'database_adj')

# Visualizar o resumo do dataframe
summary(df.cade)
# Verificar as classes das colunas de data
str(df.cade)

# CLEAN DATA ----
# Ajustar os nomes das variáveis para snake_case e remover caracteres especiais
df.cade <- df.cade %>% clean_names()

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



# ANALISYS DATA ----
# Criar o novo dataframe
df.cade.tempo <- df.cade


## Analise 01 ----
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


## Analise 02 ----
# Criar uma coluna de status para cada etapa
df.cade <- df.cade %>%
  mutate(
    status_sg = if_else(is.na(data_decisao_sg), "Aguardando julgamento pela SG", "Julgado pela SG"),
    status_tr = case_when(
      !is.na(data_decisao_sg) & is.na(data_da_decisao_tr) ~ "Aguardando julgamento pela TR",
      !is.na(data_da_decisao_tr) ~ "Julgado pela TR",
      TRUE ~ NA_character_
    ),
    status_final = case_when(
      !is.na(data_da_decisao_tr) & is.na(data_decisao_final) ~ "Aguardando julgamento final",
      !is.na(data_decisao_final) ~ "Julgado final",
      TRUE ~ NA_character_
    )
  )


# Contar processos aguardando julgamento pela SG
aguardando_sg <- df.cade %>%
  filter(status_sg == "Aguardando julgamento pela SG") %>%
  nrow()

# Contar processos aguardando julgamento pela TR
aguardando_tr <- df.cade %>%
  filter(status_tr == "Aguardando julgamento pela TR") %>%
  nrow()

# Contar processos aguardando julgamento final
aguardando_final <- df.cade %>%
  filter(status_final == "Aguardando julgamento final") %>%
  nrow()

# Exibir os resultados
cat("Quantidade de processos aguardando julgamento pela SG:", aguardando_sg, "\n")
cat("Quantidade de processos aguardando julgamento pela TR:", aguardando_tr, "\n")
cat("Quantidade de processos aguardando julgamento final:", aguardando_final, "\n")


# Filtrar processos julgados pela SG
processos_sg <- df.cade %>%
  filter(!is.na(data_decisao_sg))

# Calcular o tempo de tramitação até a decisão da SG
processos_sg <- processos_sg %>%
  mutate(tempo_sg = as.numeric(difftime(data_decisao_sg, data_de_autuacao, units = "days")))

# Estatísticas descritivas
summary_sg <- summary(processos_sg$tempo_sg)
print("Tempo de tramitação dos processos julgados pela SG (em dias):")
print(summary_sg)


# Filtrar processos julgados pela TR
processos_tr <- df.cade %>%
  filter(!is.na(data_da_decisao_tr))

# Calcular o tempo de tramitação até a decisão da TR
processos_tr <- processos_tr %>%
  mutate(tempo_tr = as.numeric(difftime(data_da_decisao_tr, data_de_autuacao, units = "days")))

# Estatísticas descritivas
summary_tr <- summary(processos_tr$tempo_tr)
print("Tempo de tramitação dos processos julgados pela TR (em dias):")
print(summary_tr)


# Filtrar processos com decisão final
processos_final <- df.cade %>%
  filter(!is.na(data_decisao_final))

# Calcular o tempo de tramitação até a decisão final
processos_final <- processos_final %>%
  mutate(tempo_final = as.numeric(difftime(data_decisao_final, data_de_autuacao, units = "days")))

# Estatísticas descritivas
summary_final <- summary(processos_final$tempo_final)
print("Tempo de tramitação dos processos com decisão final (em dias):")
print(summary_final)


# Unir os dados de tempos em um único dataframe
df_tempos <- data.frame(
  etapa = c(rep("SG", nrow(processos_sg)), rep("TR", nrow(processos_tr)), rep("Final", nrow(processos_final))),
  tempo = c(processos_sg$tempo_sg, processos_tr$tempo_tr, processos_final$tempo_final)
)

# Criar o boxplot dos tempos de tramitação
ggplot(df_tempos, aes(x = etapa, y = tempo, fill = etapa)) +
  geom_boxplot() +
  labs(
    title = "Tempo de Tramitação dos Processos por Etapa",
    x = "Etapa",
    y = "Tempo (dias)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

## Analise 03 ----
### Moving sands

# Continuando após o seu código

# Criar colunas de status para cada processo
df.cade <- df.cade %>%
  mutate(
    status = case_when(
      is.na(data_decisao_sg) ~ "Aguardando julgamento pela SG",
      !is.na(data_decisao_sg) & is.na(data_da_decisao_tr) ~ "Aguardando julgamento pela TR",
      !is.na(data_da_decisao_tr) & is.na(data_decisao_final) ~ "Aguardando julgamento final",
      !is.na(data_decisao_final) ~ "Julgado final"
    )
  )

# Verificar as colunas de data e o status
head(df.cade[, c("numero_do_processo", "data_de_autuacao", "data_decisao_sg", "data_da_decisao_tr", "data_decisao_final", "status")])

# Criar uma sequência de datas desde a data mínima até a data atual
data_inicio <- min(df.cade$data_de_autuacao, na.rm = TRUE)
data_fim <- Sys.Date()
datas <- seq.Date(from = data_inicio, to = data_fim, by = "month")

# Criar um dataframe para armazenar o número de processos em cada status ao longo do tempo
status_por_tempo <- map_df(datas, function(data_atual) {
  df_temp <- df.cade %>%
    mutate(
      status_atual = case_when(
        data_de_autuacao <= data_atual &
          (is.na(data_decisao_sg) | data_decisao_sg > data_atual) ~ "Aguardando julgamento pela SG",
        data_decisao_sg <= data_atual &
          (is.na(data_da_decisao_tr) | data_da_decisao_tr > data_atual) ~ "Aguardando julgamento pela TR",
        data_da_decisao_tr <= data_atual &
          (is.na(data_decisao_final) | data_decisao_final > data_atual) ~ "Aguardando julgamento final",
        data_decisao_final <= data_atual ~ "Julgado final",
        TRUE ~ NA_character_
      )
    ) %>%
    group_by(status_atual) %>%
    summarise(contagem = n(), .groups = "drop") %>%
    mutate(data = data_atual)
  return(df_temp)
})

# Remover NAs e organizar os níveis de status
status_por_tempo <- status_por_tempo %>%
  filter(!is.na(status_atual))

status_levels <- c("Aguardando julgamento pela SG", "Aguardando julgamento pela TR", "Aguardando julgamento final", "Julgado final")
status_por_tempo$status_atual <- factor(status_por_tempo$status_atual, levels = status_levels)

# Verificar o dataframe criado
head(status_por_tempo)

# Criar o gráfico de "moving sands"
ggplot(status_por_tempo, aes(x = data, y = contagem, fill = status_atual)) +
  geom_area(alpha = 0.8 , size = 0.5, colour = "white") +
  labs(
    title = "Evolução dos Processos ao Longo do Tempo",
    x = "Data",
    y = "Número de Processos",
    fill = "Status do Processo"
  ) +
  theme_minimal() +
  theme(legend.position = "top")
