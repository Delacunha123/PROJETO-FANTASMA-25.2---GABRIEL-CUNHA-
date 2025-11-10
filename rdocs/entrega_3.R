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

 
   
 print_quadro_resumo <- function(data, var_name, 
                                 title = "Medidas resumo da(o) [nome da variável]", 
                                 label = "quad:quadro_resumo1") {
   # Pacotes necessários
   require(dplyr)
   require(stringr)
   require(tibble)
   
   # Captura o nome da variável
   var_name <- substitute(var_name)
   
   # Calcula as medidas resumo
   data <- data %>%
     summarize(
       `Média` = round(mean(!!sym(var_name), na.rm = TRUE), 2),
       `Desvio Padrão` = round(sd(!!sym(var_name), na.rm = TRUE), 2),
       `Variância` = round(var(!!sym(var_name), na.rm = TRUE), 2),
       `Mínimo` = round(min(!!sym(var_name), na.rm = TRUE), 2),
       `1o Quartil` = round(quantile(!!sym(var_name), probs = .25, na.rm = TRUE), 2),
       `Mediana` = round(quantile(!!sym(var_name), probs = .5, na.rm = TRUE), 2),
       `3o Quartil` = round(quantile(!!sym(var_name), probs = .75, na.rm = TRUE), 2),
       `Máximo` = round(max(!!sym(var_name), na.rm = TRUE), 2)
     ) %>%
     t() %>%
     as.data.frame() %>%
     tibble::rownames_to_column("Estatística")
   
   # Início do LaTeX
   latex <- stringr::str_c(
     "\\begin{quadro}[H]\n",
     "\t\\caption{", title, "}\n",
     "\t\\centering\n",
     "\t\\begin{adjustbox}{max width=\\textwidth}\n",
     "\t\\begin{tabular}{| l | S[table-format = 6.2] |}\n",
     "\t\\toprule\n",
     "\t\\textbf{Estatística} & \\textbf{Valor} \\\\\n",
     "\t\\midrule\n"
   )
   
   # Corpo da tabela
   for (i in seq_len(nrow(data))) {
     latex <- stringr::str_c(
       latex,
       "\t", data$Estatística[i], " & ", data[i, 2], " \\\\\n"
     )
   }
   
   # Fechamento
   latex <- stringr::str_c(
     latex,
     "\t\\bottomrule\n",
     "\t\\end{tabular}\n",
     "\t\\label{", label, "}\n",
     "\t\\end{adjustbox}\n",
     "\\end{quadro}"
   )
   
   # Exibe o resultado
   writeLines(latex)
 }
 
 library(dplyr)
 library(purrr)
 
# Dados_analise_3 %>%
 #  group_by(NameStore) %>%
  # group_split() %>%                                # lista de data.frames por loja
   #iwalk(~ print_quadro_resumo(
    # data     = .x,
     #var_name = "IDADE",
     #title    = paste0("Medidas resumo da variável Idade — Loja: ", unique(.x$NameStore)),
     #label    = paste0("quad:idade_", gsub("\\s+","_", tolower(unique(.x$NameStore))))
   #))
 
 
  
 