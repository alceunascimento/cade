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
library(ggridges)

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
    return(paste("Diferente nas colunas:", paste(names(grupo)[diferentes], 
                                                 collapse = ", ")))
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
  filter(str_detect(conduta_especifica, regex('internacional', 
                                              ignore_case = TRUE)))

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


# DELETANDO COLUNAS ----
df.cade <- df.cade %>%
  select(-conduta_especifica,
         -celebracao_do_tcc,
         -suspensao_do_processo_por_tcc,
         -suspensao_judicial,
         -transito_em_julgado,
         -cooperacao_com_orgaos_do_governo,
         -cooperacao_internacional,
         -houve_recurso)

# Visualizar o resumo do dataframe após a exclusão das colunas
summary(df.cade)

# EXCLUINDO LINHAS ONDE data_de_autuacao É MAIOR QUE data_decisao_sg ----
df.cade <- df.cade %>%
  filter(data_de_autuacao <= data_decisao_sg | is.na(data_decisao_sg))

# Visualizar o resumo do dataframe após a exclusão das linhas
summary(df.cade)



# ANALISANDO DADOS ----
## tempo de tramitação de cada um dos processos de cartéis internacionais

# tempo em dias decorrido da data_de_autuacao até a data_decisao_sg ----
df.cade <- df.cade %>%
  mutate(tempo_autuacao_decisao_sg = as.numeric(difftime(data_decisao_sg, 
                                                         data_de_autuacao, 
                                                         units = "days")))

# tempo em dias decorrido entre a data_decisao_sg até a data_da_decisao_tr ----
df.cade <- df.cade %>%
  mutate(tempo_decisao_sg_decisao_tr = as.numeric(difftime(data_da_decisao_tr, 
                                                           data_decisao_sg, 
                                                           units = "days")))

# Verificar o resumo do tempo de tramitação dos processos
summary(df.cade$tempo_autuacao_decisao_sg)
summary(df.cade$tempo_decisao_sg_decisao_tr)

## Tempo em dias decorrido da data_de_autuacao até a data_decisao_sg  ----
tempo_agregado_autuacao_decisao_sg <- df.cade %>%
  summarise(tempo_medio = mean(tempo_autuacao_decisao_sg, na.rm = TRUE),
            tempo_minimo = min(tempo_autuacao_decisao_sg, na.rm = TRUE),
            tempo_maximo = max(tempo_autuacao_decisao_sg, na.rm = TRUE))

print("Tempo agregado (médio, mínimo e máximo) entre a autuação e decisão SG:")
print(tempo_agregado_autuacao_decisao_sg)



# OUTPUT ----
# Exportar para um arquivo .xlsx
write_xlsx(df.cade, path = "./output/cade2.xlsx")


# PLOTS ------

## Plot densidade ridge das CGAA 6 e 7 ----
# Filtrar apenas as coordenadorias CGAA 6 e CGAA 7
df_filtrado <- df.cade %>%
  filter(coordenacao_da_sg_responsavel %in% c("CGAA 6", "CGAA 7"))

# Gerar o gráfico de densidade com os dados filtrados e ajustar os rótulos
ggplot(df_filtrado, aes(x = tempo_autuacao_decisao_sg, 
                        y = coordenacao_da_sg_responsavel, 
                        fill = stat(x))) +
  geom_density_ridges_gradient(rel_min_height = 0.005, 
                               jittered_points = TRUE, 
                               bandwidth = 300) +
  scale_fill_viridis_c(
    name = "Dias", 
    option = "C",
    labels = scales::label_number(big.mark = ".", decimal.mark = ",")
  ) +
  scale_x_continuous(labels = scales::label_number(big.mark = ".", 
                                                   decimal.mark = ",")) + 
  coord_cartesian(clip = "off") +
  theme_minimal() +
  labs(
    x = "Dias desde a Autuação", 
    y = "Coordenações-Gerais de Análise Antitruste",
    title = "Distribuição de Tempos de Decisão por Coordenação",
    fill = "Dias" 
  )

## Plot de densidade com estatistica das CGaa 6 e 7 ----
ggplot(df_filtrado, aes(x = tempo_autuacao_decisao_sg, 
                        y = coordenacao_da_sg_responsavel)) +
  stat_density_ridges(
    geom = "density_ridges_gradient",
    calc_ecdf = TRUE,
    quantiles = c(0.025, 0.975),
    aes(fill = ..quantile..) 
  ) +
  scale_fill_manual(
    name = "Probabilidade",
    values = c("#FF0000A0", "#A0A0A0A0", "#0000FFA0"), 
    labels = c("(0, 0.025]", "(0.025, 0.975]", "(0.975, 1]")  
  ) +
  scale_x_continuous(
    breaks = c(quantile(df_filtrado$tempo_autuacao_decisao_sg, 
                        probs = c(0.025, 0.975), 
                        na.rm = TRUE)),
    labels = scales::label_number(big.mark = ".", decimal.mark = ",")
  ) + 
  labs(
    x = "Dias desde a Autuação",
    y = "Coordenações-Gerais de Análise Antitruste"
  ) +
  theme_minimal()



## Testes de diferenças entre as médias -----
# Calcular a média de 'tempo_autuacao_decisao_sg' para cada coordenacao
medias <- df_filtrado %>%
  group_by(coordenacao_da_sg_responsavel) %>%
  summarise(media = mean(tempo_autuacao_decisao_sg, na.rm = TRUE))

# Verificar a normalidade com o teste de Shapiro-Wilk
shapiro_CGAA6 <- shapiro.test(df_filtrado$tempo_autuacao_decisao_sg[df_filtrado$coordenacao_da_sg_responsavel == "CGAA 6"])
shapiro_CGAA7 <- shapiro.test(df_filtrado$tempo_autuacao_decisao_sg[df_filtrado$coordenacao_da_sg_responsavel == "CGAA 7"])
print(shapiro_CGAA6)
print(shapiro_CGAA7)

# Testes t de Student ou Wilcoxon dependendo da normalidade
teste_t <- t.test(tempo_autuacao_decisao_sg ~ coordenacao_da_sg_responsavel, 
                  data = df_filtrado)
print("Teste t de Student:")
print(teste_t)

teste_wilcox <- wilcox.test(tempo_autuacao_decisao_sg ~ coordenacao_da_sg_responsavel, 
                            data = df_filtrado)
print("Teste de Wilcoxon:")
print(teste_wilcox)
