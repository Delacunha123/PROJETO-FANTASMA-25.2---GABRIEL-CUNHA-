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
INFO_LOJAS <- read_excel("C:/Users/gabic/OneDrive/Documentos/relatorio_old_town_road.xlsx",sheet = "infos_lojas")
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

#ADICIONAR O ANO DE VENDA E VER FATURAMENTO TOTAL DE CADA ANO E CADA LOJA
FATURAMENTO_ANUAL_por_loja_ano <- Dados_analise_1 %>%
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
dados_filtrados <- Dados_analise_1 %>%
  mutate(
    ANO = year(as.Date(DATA)),
    NOME_DA_LOJA = case_when(
      `CHAVE DA LOJA` == 7  ~ "Loja Ouro Fino",
      `CHAVE DA LOJA` == 5  ~ "Loja TendTudo",
      `CHAVE DA LOJA` == 17 ~ "Ferraria Apache",
      TRUE ~ as.character(`CHAVE DA LOJA`)
    )
  ) %>%
  filter(
    ANO == 1889,
    `CHAVE DA LOJA` %in% c(7, 5, 17)
  )

top3_produtos_lojas <- dados_filtrados %>%
  group_by(NOME_DA_LOJA, `NOME DO PRODUTO`) %>%
  summarise(Total_Vendido = sum(`QUANTIDADE VENDIDA`, na.rm = TRUE)) %>%
  arrange(NOME_DA_LOJA, desc(Total_Vendido)) %>%
  group_by(NOME_DA_LOJA) %>%
  slice_head(n = 3)
cores_personalizadas <- c("#A11D21", "#003366", "#CC9900")


PRODUTO_3 <- ggplot(top3_produtos_lojas,
                     aes(x = reorder(`NOME DO PRODUTO`, -Total_Vendido),
                         y = Total_Vendido,
                         fill = NOME_DA_LOJA)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7, color = "black") +
  scale_fill_manual(values = cores_personalizadas) +
  geom_text(aes(label = Total_Vendido),
            vjust = -0.2, size = 3, fontface = "bold") +
  facet_wrap(~NOME_DA_LOJA, scales = "free_x") +
  labs(
    x = "Produto",
    y = "Quantidade Vendida",
    fill = "Loja"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text( face = "bold", size = 17),
    strip.text = element_text(size = 10, face = "bold"),  
    axis.text.x = element_text(angle = 25, hjust = 1, size = 8, face = "bold"),
    axis.text.y = element_text(size = 10),
    axis.title.x = element_text(size = 10, face = "bold"),
    axis.title.y = element_text(size = 10, face = "bold"),
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 10),
    panel.grid.minor = element_blank()
  )


