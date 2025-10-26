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


INFO_CIDADES <- read_excel("C:/Users/gabic/OneDrive/Documentos/relatorio_old_town_road.xlsx",sheet = "infos_cidades")
INFO_CLIENTES <- read_excel("C:/Users/gabic/OneDrive/Documentos/relatorio_old_town_road.xlsx",sheet = "infos_clientes")
INFO_LOJAS <- read_excel("C:/Users/gabic/OneDrive/Documentos/relatorio_old_town_road.xlsx",sheet = "infos_lojas")
RELATORIO_VENDAS <- read_excel("C:/Users/gabic/OneDrive/Documentos/relatorio_old_town_road.xlsx",sheet = "relatorio_vendas")
LOJAS_CIDADE_2 <- INFO_LOJAS %>% 
  left_join(INFO_CIDADES,by= c("CityID"= "C1tyID")) %>%
  filter(CityID==2)
DF <- RELATORIO_VENDAS %>% left_join(INFO_CLIENTES,by=c("ClientID"="Cli3ntID"))%>%
  filter(StoreID %in% c(2,6,8,9))
Dados_analise_3 <- DF %>%
  select(
    `CHAVE_DO_CLIENTE` = ClientID,
    `IDADE`           = Age,
    `CHAVE_DA_LOJA`  = StoreID,
    `SEXO`     = Sex,
  ) %>%
  distinct(CHAVE_DO_CLIENTE, .keep_all = TRUE) %>%
  left_join(LOJAS_CIDADE_2, by = c("CHAVE_DA_LOJA" = "Stor3ID"))



 GRAFICO_3 <- ggplot(Dados_analise_3) +
  aes(x = reorder(factor(NameStore), IDADE, FUN = median), y = IDADE) +
  geom_boxplot(fill = "#A11D21", width = 0.5) +
  stat_summary(
    fun = mean,
    geom = "point",
    shape = 23,
    size = 3,
    fill = "white"
  ) +
  labs(
    x = "Loja",
    y = "Idade dos Clientes",
  ) +
  theme_minimal()
  

 