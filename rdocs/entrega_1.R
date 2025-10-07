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

