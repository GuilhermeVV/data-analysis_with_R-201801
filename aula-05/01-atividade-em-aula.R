# Carregue a biblioteca tidyverse. Lembre que outras bibliotecas serão carregadas junto ao tidyverse
library(tidyverse)
library(lubridate)
library(dplyr)

# Crie um dataframe com o conteúdo do arquivo ted_main.csv.gz. 
ted <- read_csv("aula-05/data/ted_main.csv.gz")

# Visualize o resumo dos dados do dataframe. Verifique os mínimos, máximos, médias e medianas das variáveis numéricas.
# As variáveis duration, film_date e published_date estão no tipo de dados apropriado?
summary(ted)

# Converta as seguintes variáveis utilizando o pacote Lubridate:
#     * duration, para duração (em segundos). Experimente utilizar as funções as.duration e duration. Mantenha aquela que considerar mais apropriada.
#     * film_date, para data, com a função as_datetime.
#     * published_date, para data, com a função as_datetime..
ted %>%
  mutate(duration = as.duration(duration),
         film_date = as_datetime(film_date),
         published_date = as_datetime(published_date)
         ) -> ted

# Converta as seguintes variáveis character para variáveis categóricas com a função factor.
#     * event
#     * speaker_occupation
ted %>%
  mutate(event = factor(event),
         speaker_occupation = factor(event)
  ) -> ted

# Retire do dataframe a variável name
ted %>% 
  select(-name) -> ted

# Visualize novamente o resumo dos dados do dataframe. Verifique os mínimos, máximos, médias e medianas das variáveis numéricas. Verifique as contagens das variáveis categóricas
summary(ted)

# Verifique quais registros possuem a menor quantidade de línguas. Corrija para que possuam no mínimo 1 idioma.
ted %>%
  mutate(languages = if_else(languages == 0, as.integer(1), languages)) -> ted

# Verifique os 15 registros com menor data de filmagem. 
ted %>%
  arrange(film_date) %>%
  head(15)

# Crie um dataframe com a contagem de apresentações por ano de filmagem e visualize todo o seu conteúdo
ted %>%
  group_by(year_film_date = year(film_date)) %>%
  summarize(count_film_date = n()) %>%
  ungroup() -> ted_count

# Analise os 10 quantis da quantidade de apresentações por ano.
# Descarte, do data frame de apresentações do TED Talks, aqueles cujo ano de filmagem tiver quantidade de apresentações menor ou igual à quantidade do quarto quantil.
count_ref = c(quantile(ted_count$count_film_date, probs = seq(0, 1, 0.1)))[5]

ted %>%
  group_by(year_film_date = year(film_date)) %>%
  mutate(count_film_date = n()) %>%
  ungroup() %>%
  filter(count_film_date > count_ref) %>%
  select(comments:views) -> ted

# Verifique novamente o resumo dos dados do dataframe
summary(ted)

# Verifique os 10 registros com maior duração.
ted %>%
  arrange(desc(duration)) %>%
  head(10)

# Existem apresentações com duração maior que 3 desvios padrão acima da média? Liste elas

# Calcule os 4 quartis e o IQR da duração das apresentações. Liste as apresentações cuja duração supera 1.5 * o IQR + o terceiro quartil

# Visualize os 10 quantis da quantidade de visualizações

# Compare as seguintes estatísticas descritivas da quantidade de visualizações:
#   * Média e Mediana. Qual é maior?
#   * Desvio Absoluto da Mediana e Desvio Padrão. Qual é maior?
#   * Desvio Absoluto da Mediana e IQR. Quantas vezes o IQR é maior que o Desvio Absoluto da Mediana?
#   * Com base na média e na mediana, e na razão entre o IQR e o Desvio Absoluto da Mediana, 
#     você conclui que as quantidades de visualização estão distribuidas de forma simétrica em torno da média?

# Calcule a média, o desvio padrão, a mediana e o IQR da quantidade de línguas dos seguintes grupos:
#     * 10% de vídeos com maior número de visualizações
#     * 10% de vídeos com menor número de visualizações

# Determine a quantidade de apresentações por evento cujo nome inicie com TED. Utilize a função str_detect para este filtro

# Determine, por evento cujo nome inicie com TED e que a quantidade de visualizações dos vídeos foi maior que a mediana calculada anteriormente.
#   * a quantidade de apresentações resultante do filtro, por evento
#   * o ano do evento (utilizar o menor ano da data de publicação)
#   * a quantidade média de línguas das apresentações
#   * o desvio padrão da quantidade de línguas
#   * o coeficiente de variação da quantidade de línguas
### EXIBA SOMENTE OS EVENTOS COM MAIS DE 10 APRESENTAÇÕES

# Calcule e classifique as seguintes correlações
#     * Quantidade de visualizações e Quantidade de línguas
#     * Quantidade de visualizações e Duração
#     * Quantidade de visualizações e Quantidade de Comentários
#     * Quantidade de Comentários e Quantidade de línguas

# Descarte os vídeos cuja duração seja maior que 3 desvios padrões da média. Calcule novamente as 5 correlações solicitadas

# Utilizando o data frame original, crie um dataframe com a mediana da duração dos vídeos por ano de filmagem. Calcule a correlação entre o ano e a mediana da duração
# e interprete o resultado