############################################################################
####### OLHAR O CÓDGIO COMPLETO PARA PACOTES, IMPORTAÇÃO E ETC !!!!!! ######
############################################################################

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

resultado_teste_norm_analise_2 <- shapiro.test(dados_serie$imdb)

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
