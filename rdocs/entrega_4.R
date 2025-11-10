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
library(stringr)
library(forcats)
library(ggplot2)



theme_estat <- function(...) {
  theme <- ggplot2::theme_bw() +
    ggplot2::theme(
      axis.title.y = ggplot2::element_text(colour = "black", size = 12),
      axis.title.x = ggplot2::element_text(colour = "black", size = 12),
      axis.text = ggplot2::element_text(colour = "black", size = 9.5),
      panel.border = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(colour = "black"),
      legend.position = "top",
      ...
    )
  return(theme)
}


RELATORIO_VENDAS <- read_excel("C:/Users/gabic/OneDrive/Documentos/relatorio_old_town_road.xlsx",sheet = "relatorio_vendas")
INFO_PROD <- read_excel("C:/Users/gabic/OneDrive/Documentos/relatorio_old_town_road.xlsx",sheet = "infos_produtos")
INFO_VENDAS <- read_excel("C:/Users/gabic/OneDrive/Documentos/relatorio_old_town_road.xlsx",sheet = "infos_vendas")
INFO_LOJAS <- read_excel("C:/Users/gabic/OneDrive/Documentos/relatorio_old_town_road.xlsx",sheet = "infos_lojas")
#JUNTAR AS TABELAS 
DF <- RELATORIO_VENDAS %>% 
  left_join(INFO_VENDAS,by = c( "SaleID" = "Sal3ID")) %>% 
  left_join(INFO_PROD,by= c("ItemID"= "Ite3ID"))




#SELECIONAR AS VARIÁVEIS QUE EU QUERO TRABALHAR
Dados_analise_4 <- DF %>%
  select(
    `CHAVE DA VENDA` = SaleID,
    `DATA`           = Date,
    `CHAVE DA LOJA`  = StoreID,
    `QUANTIDADE VENDIDA`     = Quantity,
    `CHAVE DO PRODUTO` = ItemID,
    `NOME DO PRODUTO`  = NameProduct,
    `PRECOUNITARIODOLAR`   = UnityPrice
  )

#ADICIONAR O ANO DE VENDA E VER FATURAMENTO TOTAL DE CADA ANO E CADA LOJA
FATURAMENTO_ANUAL_por_loja_ano <- Dados_analise_4 %>%
  mutate(ANO = year(as.Date(DATA))) %>%
  group_by(`CHAVE DA LOJA`, ANO) %>%
  mutate(
    VENDAS_EM_DOLAR = `QUANTIDADE VENDIDA` * PRECOUNITARIODOLAR
  ) %>%
  summarise(Faturamento_anual = sum(VENDAS_EM_DOLAR, na.rm = TRUE), .groups = "drop")
#filtrar para o ano de 1889 e trabalhar com o nome das lojas em vez de chave
DADOS <- FATURAMENTO_ANUAL_por_loja_ano %>% 
  left_join(INFO_LOJAS,by= c("CHAVE DA LOJA"= "Stor3ID")) %>%
  filter(ANO==1889)

# Suponha que seu data frame se chame df
# Ordenar em ordem decrescente e pegar as 3 primeiras lojas
df_ordenado <- DADOS[order(-DADOS$Faturamento_anual), ]
TOP_3 <- head(df_ordenado, 3)


cores_personalizadas <- c("#A11D21", "#003366", "#CC9900")

# Gráfico de barras
# Folga no eixo (com base no valor máximo)
LOJAS_3 <- ggplot(TOP_3, aes(x = reorder(NameStore, -Faturamento_anual), y = Faturamento_anual, fill = NameStore)) + geom_bar(stat = "identity", width = 0.6, color = "black") + scale_fill_manual(values = cores_personalizadas) + geom_text(aes(label = round(Faturamento_anual, 0)), vjust = -0.5, size = 3) + labs(x = "Loja", y = "Faturamento Anual (US$)") + theme_minimal(base_size = 13) + theme( plot.title = element_text(hjust = 0.5, face = "bold"), legend.position = "none", axis.text.x = element_text(angle = 15, hjust = 1) )
#VENDO TOP 3 PRODUTOS MAIS VENDIDOS:

# Se suas colunas têm espaços, renomeie para facilitar (opcional, mas ajuda muito):
dados_filtrados <- Dados_analise_4 %>%
  rename(
    CHAVE_DA_LOJA      = `CHAVE DA LOJA`,
    NOME_DO_PRODUTO    = `NOME DO PRODUTO`,
    QUANTIDADE_VENDIDA = `QUANTIDADE VENDIDA`
  )

# cria a coluna do nome a partir da chave
dados_filtrados2 <- dados_filtrados %>%
  mutate(
    NOME_DA_LOJA = dplyr::recode(
      CHAVE_DA_LOJA,
      `5`  = "Loja TendTudo",
      `7`  = "Loja Ouro Fino",
      `17` = "Ferraria Apache",
      .default = NA_character_
    )
  ) %>%
  mutate(ANO = year(as.Date(DATA))) %>%
  filter(CHAVE_DA_LOJA %in% c(5, 7, 17))
# 1) Base 1889
base_1889 <- dados_filtrados2 %>% filter(ANO == 1889)

# 2) Top 3 produtos no ano (somando quantidade vendida)
top3_prod <- base_1889 %>%
  group_by(NOME_DO_PRODUTO) %>%
  summarise(freq = sum(QUANTIDADE_VENDIDA, na.rm = TRUE), .groups = "drop") %>%
  slice_max(freq, n = 3, with_ties = FALSE) %>%
  pull(NOME_DO_PRODUTO)

# 3) “trans_drv” no seu padrão: Produto × Loja
trans_drv <- base_1889 %>%
  filter(NOME_DO_PRODUTO %in% top3_prod) %>%
  group_by(NOME_DO_PRODUTO, NOME_DA_LOJA) %>%
  summarise(freq = sum(QUANTIDADE_VENDIDA, na.rm = TRUE), .groups = "drop") %>%
  group_by(NOME_DO_PRODUTO) %>%
  mutate(freq_relativa = round(freq / sum(freq) * 100, 1)) %>%
  ungroup() %>%
  mutate(
    trans = NOME_DO_PRODUTO,
    drv   = NOME_DA_LOJA
  )

# rótulos "qtd (xx,x%)"
porcentagens <- stringr::str_c(trans_drv$freq_relativa, "%") %>%
  stringr::str_replace("\\.", ",")
legendas <- stringr::str_squish(stringr::str_c(trans_drv$freq, " (", porcentagens, ")"))

# 4) Gráfico
g <- ggplot(trans_drv) +
  aes(
    x = fct_reorder(trans, trans_drv$freq, .fun = sum, .desc = TRUE),
    y = freq, fill = drv, label = legendas
  ) +
  geom_col(position = position_dodge2(preserve = "single", padding = 0)) +
  geom_text(position = position_dodge(width = .9), vjust = -0.5, hjust = 0.5, size = 3) +
  scale_fill_manual(values = cores_personalizadas)+
  labs(
    x = "Produto", y = "Quantidade vendida", fill = "Loja"
  ) +
  theme_estat()


