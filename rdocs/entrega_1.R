source("rdocs/source/packages.R")

# ---------------------------------------------------------------------------- #

#        ______   _____  ________      ________ 
#      |  ____| / ____| |__   __| /\  |__   __|
#     | |__    | (___     | |   /  \    | |   
#    |  __|    \___ \    | |  / /\ \   | |   
#   | |____   ____) |   | |  /____ \  | |   
#  |______   |_____/   |_| /_/    \_\|_|   
#  
#         Consultoria estatística 
#

# ---------------------------------------------------------------------------- #
# ############################## README ###################################### #
# Consultor, favor utilizar este arquivo .R para realizar TODAS as análises
# alocadas a você neste projeto pelo gerente responsável, salvo instrução 
# explícita do gerente para mudança.
#
# Escreva seu código da forma mais clara e legível possível, eliminando códigos
# de teste depreciados, ou ao menos deixando como comentário. Dê preferência
# as funções dos pacotes contidos no Tidyverse para realizar suas análises.
# ---------------------------------------------------------------------------- #

library(dplyr)
library(readxl)
library(lubridate)
library(ggplot2)


RELATORIO_VENDAS <- read_excel("C:/Users/gabic/OneDrive/Documentos/relatorio_old_town_road.xlsx",sheet = "relatorio_vendas")
INFO_PROD <- read_excel("C:/Users/gabic/OneDrive/Documentos/relatorio_old_town_road.xlsx",sheet = "infos_produtos")
INFO_VENDAS <- read_excel("C:/Users/gabic/OneDrive/Documentos/relatorio_old_town_road.xlsx",sheet = "infos_vendas")





#JUNTAR AS TABELAS 
DF <- RELATORIO_VENDAS %>% 
  left_join(INFO_VENDAS,by = c( "SaleID" = "Sal3ID")) %>% 
  left_join(INFO_PROD,by= c("ItemID"= "Ite3ID"))



#SELECIONAR AS VARIÁVEIS QUE EU QUERO TRABALHAR
Dados_analise_1 <- DF %>%
  select(
    `CHAVE DA VENDA` = SaleID,
    `DATA`           = Date,
    `CHAVE DA LOJA`  = StoreID,
    `QUANTIDADE VENDIDA`     = Quantity,
    `CHAVE DO PRODUTO` = ItemID,
    `NOME DO PRODUTO`  = NameProduct,
    `PRECOUNITARIODOLAR`   = UnityPrice
  )

#CONVERTER O PREÇO UNITÁRIO PARA REAIS E CALCULAR VENDA
DOLAR <- 5.31

DADOS_ATUALIZADO_ANALISE_1 <- Dados_analise_1 %>%
  mutate(
    PREÇO_UNITÁRIO_BRL = DOLAR * `PRECOUNITARIODOLAR`,
    VENDAS_EM_REAIS = `QUANTIDADE VENDIDA` * PREÇO_UNITÁRIO_BRL
  )
#ADICIONAR O ANO DE VENDA E VER FATURAMENTO TOTAL DE CADA ANO E CADA LOJA
FATURAMENTO_ANUAL_por_loja_ano <- DADOS_ATUALIZADO_ANALISE_1 %>%
  mutate(ANO = year(as.Date(DATA))) %>%
  group_by(`CHAVE DA LOJA`, ANO) %>%
  summarise(Faturamento_anual = sum(VENDAS_EM_REAIS, na.rm = TRUE), .groups = "drop")
#CALCULAR FATURAMENTO MÉDIO POR ANO
FATURAMENTO_MÉDIO_POR_ANO <- FATURAMENTO_ANUAL_por_loja_ano %>%
  group_by(ANO) %>%
  summarise(
    soma_faturamento = sum(Faturamento_anual, na.rm = TRUE),
    numero_lojas = n_distinct(`CHAVE DA LOJA`),
    faturamento_medio = soma_faturamento / numero_lojas) %>%
  select(ANO,faturamento_medio)
#FAZENDO CRIAÇÃO DO GRÁFICO DE DISPERSÃO COM LINHAS PARA MELHOR VISUALIZAÇÃO DO FATURAMENTO MÉDIO DAS LOJAS POR ANO
ANÁLISE_1 <- ggplot(FATURAMENTO_MÉDIO_POR_ANO) +
  aes(x = ANO, y = faturamento_medio, group = 1) +
  geom_line(size = 1, colour = "#A11D21") +
  geom_point(colour = "#A11D21", size = 2) +
  scale_x_continuous(breaks = seq(min(FATURAMENTO_MÉDIO_POR_ANO$ANO),
                                  max(FATURAMENTO_MÉDIO_POR_ANO$ANO), 1)) +
  labs(x = "Ano", y = "Faturamento médio das lojas") +
  theme_estat()


library(tidyverse)
library(knitr)
library(dplyr)
library(kableExtra)

#GERAR TABELA EM LATEX
FATURAMENTO_MÉDIO_POR_ANO %>%
  kable(
    col.names = c("\\textbf{Ano}", "\\textbf{Faturamento Médio (R\\$)}"),
    align = c("c", "r"),
    format = "latex",
    booktabs = TRUE,
    caption = "Faturamento médio por ano nas lojas",
    escape = FALSE                     # permite comandos LaTeX como \textbf{}
  ) %>%
  kable_styling(
    latex_options = c("hold_position"),
    font_size = 11
  )
