#############################################################################
#### ATENÇÃO: OLHAR O CÓDGIO COMPLETO PARA PACOTES, IMPORTAÇÃO E ETC !!! ####
#############################################################################


# Duplicando os dataset para não corromper os dados originais

dados_analise_4 <-dados

# Gráfico de disperção

ggplot(dados) +
  aes(x = engagement, y = imdb) +
  geom_point(colour = "#A11D21", size = 3) +
  labs(
    x = "Engajamento",
    y = "Nota IMDB"
  ) +
  theme_estat()
ggsave("disp_uni_analise_4.pdf", width = 158, height = 93, units = "mm")


# Testando a normalidade dos dados para aplicar o teste de correlação

# Definindo uma função para realizar o teste e exibir os resultados

teste_normalidade_analise_4 <- function(dados, coluna) {
  resultado_teste <- ad.test(dados[[coluna]])
  print(resultado_teste)
}

# Aplicando a função para as colunas imdb e engagement

teste_normalidade_analise_4(dados_analise_4, "imdb")
teste_normalidade_analise_4(dados_analise_4, "engagement")

# Teste de correlação de Pearson

teste_de_correlacao_4 <- cor.test(dados_analise_4$imdb, dados_analise_4$engagement, method = "pearson")
print(teste_correlacao_de_correlacao_4)
