################################################################
#### OLHAR O CÓDGIO COMPLETO PARA PACOTES, IMPORTAÇÃO E ETC ####
################################################################

# DUPLICANDO OS DATASETS 

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

