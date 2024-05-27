# Coluna indicando quem capturou o monstro

dados_analise_5 <- dados_analise_5 %>%
  mutate(capturador = case_when(
    caught_fred == TRUE ~ 'Fred',
    caught_daphnie == TRUE ~ 'Daphne',
    caught_velma == TRUE ~ 'Velma',
    caught_shaggy == TRUE ~ 'Shaggy',
    caught_scooby == TRUE ~ 'Scooby'
  ))


dados_analise_5 <- na.omit(dados_analise_5)

# Transformar as colunas de captura em fatores para facilitar a análise
dados_analise_5 <- dados_analise_5 %>%
  mutate(across(starts_with("caught_"), as.logical),
         across(starts_with("captured_"), as.logical))


dados_analise_5 <- dados_analise_5 %>%
  rowwise() %>%
  mutate(character_caught = case_when(
    caught_fred ~ "Fred",
    caught_daphnie ~ "Daphne",
    caught_velma ~ "Velma",
    caught_shaggy ~ "Shaggy",
    caught_scooby ~ "Scooby",
    TRUE ~ "None"
  )) %>%
  ungroup()

dados_analise_5 <- dados_analise_5 %>%
  filter (character_caught !="None")

ggplot(dados_analise_5) +
  aes(x = reorder(character_caught, engagement, FUN = median), y = engagement) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  labs(x = "Personagem que capturou o monstro", y = "Engajamento") +
  theme_estat()
ggsave("box_bi_analise_5.pdf", width = 158, height = 93, units = "mm")


quadro_resumo_5 <- dados_analise_5 %>%
  group_by( character_caught ) %>% # caso mais de uma categoria
  summarize (Média = round(mean(engagement),2),
             `Desvio Padrão ` = round(sd(engagement),2),
             `Variância ` = round(var(engagement),2),
             `Mínimo ` = round(min(engagement),2),
             `1º Quartil ` = round(quantile(engagement , probs = .25),2),
             Mediana = round(quantile(engagement , probs = .5),2),
             `3º Quartil ` = round(quantile(engagement , probs = .75),2),
             `Máximo ` = round(max(engagement),2)) %>% t() %>% as.data.frame()
xtable :: xtable(quadro_resumo_5)


shapiro_test_5 <- shapiro.test(dados_analise_5$engagement)
print(shapiro_test_5)


resultado_bartlett_5 <- bartlett.test( engagement ~ character_caught, data = dados_analise_5)

print(resultado_bartlett_5)

anova_5 <- aov(engagement ~ character_caught, data = dados_analise_5)
summary(anova_5)
