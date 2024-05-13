#############################
### CARREGANDO OS PACOTES ###
#############################


# Lista de pacotes necessários

pacotes <- c("xtable", "tidyverse", "stringr", "purrr","nortest")

# Verificando e carregando pacotes ausentes

pacotes_faltantes <- pacotes[!(pacotes %in% installed.packages()[,"Package"])]

if (length(pacotes_faltantes) > 0) {
  install.packages(pacotes_faltantes, dependencies = TRUE)
}

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
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white") +
  labs(x = "Temporadas dos episódios", y = "Nota IMDB") +
  theme_estat()
ggsave("box_multi_imdb.pdf", width = 158, height = 93, units = "mm")

# Testando a normalidade dos dados para aplicar ANOVA

resultado_teste_norm_analise_2 <- ad.test(dados_serie$imdb)

print(resultado_teste_norm_analise_2)

# Teste de Homogeneidade de Variância de Bartlett

resultado_bartlett <- bartlett.test(imdb ~ season, data = dados_serie)

print(resultado_bartlett)

# Anova para a nota IMDP por temporada

modelo_anova <- aov(imdb ~ season, data = dados_serie)

summary(modelo_anova)

# quadro

quadro_resumo <- dados_serie %>%
  group_by( season ) %>% # caso mais de uma categoria
  summarize (Média = round(mean(imdb),2),
             `Desvio Padrão ` = round(sd(imdb),2),
             `Variância ` = round(var(imdb),2),
             `Mínimo ` = round(min(imdb),2),
             `1º Quartil ` = round(quantile(imdb , probs = .25),2),
             Mediana = round(quantile(imdb , probs = .5),2),
             `3º Quartil ` = round(quantile(imdb , probs = .75),2),
             `Máximo ` = round(max(imdb),2)) %>% t() %>% as.data.frame () 

xtable :: xtable(quadro_resumo)


#################
### ANALISE 3 ###
#################

################################
#### DUPLICANDO OS DATASETS ####
################################

dados_analise_3 <-dados

# Definindo terrenos mais frequentes

frequencia_terreno <- dados_analise_3 %>%
  count(setting_terrain, sort = TRUE)

top_3_terrenos <- frequencia_terreno$setting_terrain[1:3]

dataset_top_3_terrenos <- dados_analise_3 %>%
  filter(setting_terrain %in% top_3_terrenos)

# Retirando NAS e espaços vazios e Formatando o banco para criar o grafico com frequencias

dados_agrupados_terrenos <- dataset_top_3_terrenos %>%
  filter(!is.na(setting_terrain) & setting_terrain != "") %>%
  filter(!is.na(trap_work_first) & trap_work_first != "") %>%
  group_by(setting_terrain, trap_work_first) %>%
  summarise(freq = n()) %>%
  mutate(
    freq_relativa = round(freq / sum(freq) * 100, 1)
  )

dados_agrupados_terrenos <- dados_agrupados_terrenos %>%
  mutate(
    setting_terrain = case_when(
      setting_terrain == "Urban" ~ "Urbano",
      setting_terrain == "Rural" ~ "Rural",
      setting_terrain == "Forest" ~ "Floresta",
      TRUE ~ setting_terrain
    ),
    trap_work_first = ifelse(trap_work_first, "Verdadeiro", "Falso")
  )

dados_agrupados_terrenos <- dados_agrupados_terrenos %>%
  arrange(desc(freq))


porcentagens <- str_c(dados_agrupados_terrenos$freq_relativa, "%") %>% str_replace("
\\.", ",")
legendas <- str_squish(str_c(dados_agrupados_terrenos$freq, " (", porcentagens, ")")
)

# Grafico

ggplot(dados_agrupados_terrenos) +
  aes(
    x = fct_reorder(setting_terrain, freq, .desc = T), y = freq,
    fill = trap_work_first, label = legendas
  ) +
  geom_col(position = position_dodge2(preserve = "single", padding =0)) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, hjust = 0.5,
    size = 3
  ) +
  labs(x = "Terreno", y = "Frequência", fill = "Armadilha funcionou de primeira?") +
  scale_y_continuous(limits = c(0, 80), breaks = seq(0, 80, by = 20))+
  theme_estat()
ggsave("terreno_ativacao_barras_multivariado_1.pdf", width = 158, height = 93, units = "mm")


# Xtabs para criar a tabela de contingência 

tabela_contingencia <- xtabs(freq ~ setting_terrain + trap_work_first, data = dados_agrupados_terrenos)

print(tabela_contingencia)

# Teste chisq de independência

teste_analise_3 <- chisq.test(tabela_contingencia)
teste_analise_3

# Mesmo teste com outra formatação de tabela (dataset)

dataset_top_3_terrenos_teste <- dataset_top_3_terrenos

dataset_top_3_terrenos_teste <- dataset_top_3_terrenos_teste %>%
  filter(!is.na(trap_work_first) & trap_work_first != "") 

table(dataset_top_3_terrenos_teste$setting_terrain,dataset_top_3_terrenos_teste$trap_work_first)

chisq.test(table(dataset_top_3_terrenos_teste$setting_terrain,dataset_top_3_terrenos_teste$trap_work_first))


