############################################################
############################################################
##                 !!!!!!ATENÇÃO!!!!!!                   ###
### OLHAR O CÓDIGO COMPLETO PARA A PADRONIZAÇÃO DE CORES ###
###                E PACOTES NECESSÁRIOS                 ###
############################################################
############################################################


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
