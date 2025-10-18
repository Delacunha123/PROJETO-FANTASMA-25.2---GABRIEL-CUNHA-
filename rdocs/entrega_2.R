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

INFO_CLIENTES <- read_excel("~/GitHub/PROJETO-FANTASMA-25.2---GABRIEL-CUNHA-/relatorio_old_town_road.xlsx",sheet = "infos_clientes")

TABELA_CLIENTES_DESATUALIZADAS <- INFO_CLIENTES %>% mutate(ALTURA_CM = Height_dm * 10,PESO_KG = Weight_lbs / 2.20462)

TABELA_CLIENTES <- TABELA_CLIENTES_DESATUALIZADAS %>% select(`CLIENTE` = Cli3ntID, 
                                                             `PESO_EM_KG` = PESO_KG,
                                                             `ALTURA_EM_CM`  = ALTURA_CM)

ANALISE_2 <- ggplot(TABELA_CLIENTES) +
  aes(x = PESO_EM_KG, y = ALTURA_EM_CM) +
  geom_point(colour = "#A11D21", size = 1) +
  labs(
    x = "Peso (kg)",
    y = "Altura (cm)"
  ) +
  theme_estat()



#TESTE DE NORMALIDADE PARA AS VARIÁVEIS
#h0:OS DADOS VEM DE UMA DISTRIBUIÇÃO NORMAL
#H1:OS DADOS NÃO VEM DE UMA DISTRIBUIÇÃO NORMAL
shapiro_peso <- shapiro.test(TABELA_CLIENTES$PESO_EM_KG)
shapiro_altura <- shapiro.test(TABELA_CLIENTES$ALTURA_EM_CM)
#HA EVIDENCIAS DE QUE A  VARIÁVEL PESO E A VARIÁVEL ALTURA NÃO SEGUEM UMA DISTRIBUIÇÃO NORMAL, SENDO O MAIS RECOMENDADO POSSÍVEL A FAZER O TESTE DE SPEARMAN
spearman <- cor.test(TABELA_CLIENTES$PESO_EM_KG,TABELA_CLIENTES$ALTURA_EM_CM,method = "spearman")
#Correlação de Spearman: ρ = 0.6865
#p < 0.05 → rejeita H₀
#Conclusão: existe correlação positiva e significativa entre peso e altura, de força moderada a forte.
#Isso significa que, à medida que a altura aumenta, o peso tende a aumentar também — o que faz sentido biologicamente.
# Criar a tabela
tabela_testes <- data.frame(
  Teste = c("Shapiro-Wilk", "Shapiro-Wilk", "Spearman"),
  Variável = c("Peso (Kg)", "Altura (cm)", "Peso × Altura"),
  Estatística = c(
    round(shapiro_peso$statistic, 4),
    round(shapiro_altura$statistic, 4),
    round(spearman$estimate, 4)
  ),
  `p-valor` = c(
    round(shapiro_peso$p.value, 6),
    round(shapiro_altura$p.value, 6),
    round(spearman$p.value, 6)
  ),
  Decisão = c(
    ifelse(shapiro_peso$p.value < 0.05, "Rejeita H0", "Não rejeita H0"),
    ifelse(shapiro_altura$p.value < 0.05, "Rejeita H0", "Não rejeita H0"),
    ifelse(spearman$p.value < 0.05, "Rejeita H0", "Não rejeita H0")
  ),
  Interpretação = c(
    ifelse(shapiro_peso$p.value < 0.05, "Não segue normalidade", "Segue normalidade"),
    ifelse(shapiro_altura$p.value < 0.05, "Não segue normalidade", "Segue normalidade"),
    ifelse(spearman$p.value < 0.05, "Correlação significativa", "Sem correlação significativa")
  )
)

# Gerar a tabela formatada em LaTeX
tabela_testes %>%
  kable(
    format = "latex",
    booktabs = TRUE,
    align = "lccrrll",
    col.names = c("Teste", "Variável", "Estatística", "p-valor", "Decisão", "Interpretação")
  ) %>%
  kable_styling(latex_options = c("hold_position"))

