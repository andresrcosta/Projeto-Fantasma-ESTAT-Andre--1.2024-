#############################
### CARREGANDO OS PACOTES ###
#############################


# Lista de pacotes necessários
pacotes <- c("xtable", "tidyverse", "stringr", "purrr","nortest")

# Verificar e carregar pacotes ausentes
pacotes_faltantes <- pacotes[!(pacotes %in% installed.packages()[,"Package"])]
if (length(pacotes_faltantes) > 0) {
  install.packages(pacotes_faltantes, dependencies = TRUE)
}

# Carregar pacotes
lapply(pacotes, require, character.only = TRUE)

#######################################
### PADRONIZAÇÃO CORES DOS GRÁFICOS ###
#######################################

estat_colors <- c(
  "#A11D21", "#003366", "#CC9900",
  "#663333", "#FF6600", "#CC9966",
  "#999966", "#006606", "#008091",
  "#041835", "#666666" )

theme_estat <- function(...) {
  theme <- ggplot2::theme_bw() +
    ggplot2::theme(
      axis.title.y = ggplot2::element_text(colour = "black", size = 12),
      axis.title.x = ggplot2::element_text(colour = "black", size = 12),
      axis.text = ggplot2::element_text(colour = "black", size = 9.5)
      ,
      panel.border = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(colour = "black"),
      legend.position = "top",
    )
  return(
    list(
      theme,
      scale_fill_manual(values = estat_colors),
      scale_colour_manual(values = estat_colors)
    )
  )
}

#############
# Diretório #
#############

setwd("")

#######################
# IMPORTANDO OS DADOS #
#######################

dados <- read.csv("banco_final.csv", encoding = "UTF-8")

###################
###################
#### ANALISE 1 ####
###################
###################

# Duplicando o dataset para não corromper os dados originais (importados) em caso de erro

dados_analise_1 <-dados

# Verificando as opções únicas da coluna "format" e as frequência

unique(dados_analise_1$format)

table(dados_analise_1$format)

# Renomeando os valores da coluna "format"

dados_analise_1 <- dados_analise_1 %>%
  mutate(format = case_when(
    format == "Serie" ~ "Série",
    format == "CrossOver" ~ "CrossOver",
    format == "Movie" ~ "Filme",
    TRUE ~ format  # Mantém os valores que não correspondem a nenhum dos acima
  ))

# Criando uma coluna "ano" para ordenar o dataset por décadas

dados_analise_1$ano <- as.numeric(format(as.Date(dados_analise_1$date_aired), "%Y"))

# Verificando a década de ínico e fim do dataset

min(dados_analise_1$ano)

max(dados_analise_1$ano)

###########################
## ANALISANDO AS DÉCADAS ##
###########################

# Função para atribuir rótulos por década

atribuir_decada <- function(ano) {
  decadas <- c("1960", "1970", "1980", "1990", "2000", "2010", "2020")
  decada_indices <- cut(ano, breaks = c(1960, 1970, 1980, 1990, 2000, 2010, 2020, 2030),
                        labels = decadas, right = FALSE)
  return(decada_indices)
}

# Aplicando a função à coluna "date_aired" para criar a coluna "Decada"

dados_analise_1 <- dados_analise_1 %>%
  mutate(decada = atribuir_decada(as.numeric(format(as.Date(date_aired), "%Y"))))


# Criando uma tabela para visualizar (e posteriormente imprimir latex)

contagem_por_decada <- table(dados_analise_1$decada, dados_analise_1$format)

print(contagem_por_decada)

# Agrupando os tipos de formato por década

contagem_formato <- dados_analise_1 %>%
  group_by(decada, format) %>%
  summarise(contagem = n())

# Gráfico de barras multivariado (aumentei o range de Y, pois estava cortando no Overleaf)

ggplot(contagem_formato) +
  aes(x = decada, y = contagem, group = format, colour = format) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(x = "Décadas", y = "Quantidade dos tipos de lançamento",colour= "Formato de Lançamento") +
  theme_estat()
ggsave("formato_linhas_multivariado.pdf", width = 158, height = 93, units = "mm")

######################
######################
##### ANALISE 2 ######
######################
######################

# Duplicando o dataset da análise 1

dados_analise_2 <-dados_analise_1

# Filtrando apenas as séries

dados_serie <- filter(dados_analise_2, format == "Série")

# Renomeando os valores da coluna "season" e remover "Special" (pdf. contextualização)

dados_serie <- dados_serie %>%
  mutate(season = case_when(
    season == "1" ~ "1ª temporada",
    season == "2" ~ "2ª temporada",
    season == "3" ~ "3ª temporada",
    season == "4" ~ "4ª temporada",
    TRUE ~ season  # Mantém os valores que não correspondem a nenhum dos acima
  )) %>%
  filter(season != "Special")

# Grafico Boxplot multivariado

ggplot(dados_serie) +
  aes(x = season, y = imdb) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  labs(x = "Temporadas dos episódios", y = "Nota imbdb") +
  theme_estat()
ggsave("box_multi_imdb.pdf", width = 158, height = 93, units = "mm")

# Testando a normalidade dos dados para aplicar ANOVA

resultado_teste_norm_analise_2 <- ad.test(dados_serie$imdb)

print(resultado_teste_norm_analise_2)

# Anova para a nota IMDP por temporada

modelo_anova <- aov(imdb ~ season, data = dados_serie)

summary(modelo_anova)

# Gerando o quadro para escrever o texto de análise.

quadro_2 <- dados_serie %>%
  group_by(season) %>%
  summarize(
    max_nota = max(imdb),
    media_nota = mean(imdb),
    mediana_nota = median(imdb),
    desvio_padrao_nota = sd(imdb)
  )
print(quadro_2)
