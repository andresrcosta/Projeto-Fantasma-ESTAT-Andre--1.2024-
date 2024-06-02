############################################################################
####### OLHAR O CÓDGIO COMPLETO PARA PACOTES, IMPORTAÇÃO E ETC !!!!!! ######
############################################################################


# Criando um novo data frame para armazenar os resultados

novo_dataset <- data.frame(Capturador = character(), Engajamento = numeric())

# Função para extrair e processar os dados de cada coluna
processar_coluna <- function(coluna_nome, nome_capturador) {
  
  # Filtrando os dados da coluna onde o valor é True
  dados_filtrados <- filter(dados, !!sym(coluna_nome) == "True")
  
  # Adicionando os dados filtrados ao novo dataset
  novo_dataset <<- bind_rows(novo_dataset, data.frame(Capturador = nome_capturador, Engajamento = dados_filtrados$engagement))
}

# Listando das colunas de interesse e seus respectivos nomes de capturador formatados

colunas_interesse <- c("caught_fred", "caught_daphnie", "caught_velma", "caught_shaggy", "caught_scooby", "caught_other", "caught_not")
nomes_capturadores <- c("Fred", "Daphnie", "Velma", "Salsicha", "Scooby", "Outro", "Não capturado")

# Processando cada coluna
for (i in 1:length(colunas_interesse)) {
  processar_coluna(colunas_interesse[i], nomes_capturadores[i])
}

# Quadro de medidas

quadro_resumo_5 <- novo_dataset %>%
  group_by( Capturador ) %>% 
  summarize (Média = round(mean(Engajamento),2),
             `Desvio Padrão ` = round(sd(Engajamento),2),
             `Variância ` = round(var(Engajamento),2),
             `Mínimo ` = round(min(Engajamento),2),
             `1º Quartil ` = round(quantile(Engajamento , probs = .25),2),
             Mediana = round(quantile(Engajamento , probs = .5),2),
             `3º Quartil ` = round(quantile(Engajamento , probs = .75),2),
             `Máximo ` = round(max(Engajamento),2)) %>% t() %>% as.data.frame ()

xtable :: xtable(quadro_resumo_5)

# Boxplot

ggplot(novo_dataset) +
  aes(x = reorder(Capturador, Engajamento, FUN = median), y = Engajamento) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  labs(x = "Personagem que capturou o monstro", y = "Enagjamento do episódio") +
  theme_estat()
ggsave("box_bi_ultimo.pdf", width = 158, height = 93, units = "mm")
