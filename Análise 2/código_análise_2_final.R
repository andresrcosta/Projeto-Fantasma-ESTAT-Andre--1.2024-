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
