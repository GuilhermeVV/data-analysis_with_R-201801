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
  summarise(count_film_date = n()) %>%
  ungroup() -> ted_count

# Analise os 10 quantis da quantidade de apresentações por ano.
# Descarte, do data frame de apresentações do TED Talks, aqueles cujo ano de filmagem tiver quantidade de apresentações menor ou igual à quantidade do quarto quantil.
count_ref = quantile(ted_count$count_film_date, probs = seq(0, 1, 0.1))[5]

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
three_sd = as.duration(mean(ted$duration) + (sd(ted$duration) * 3))

ted %>%
  filter(duration > three_sd) %>%
  View()

# Calcule os 4 quartis e o IQR da duração das apresentações. Liste as apresentações cuja duração supera 1.5 * o IQR + o terceiro quartil
duration_quantiles = as.duration(quantile(as.integer(ted$duration), probs = seq(0, 1, 0.25)))
duration_IQR = IQR(ted$duration)

ted %>%
  filter(duration > 1.5 * duration_IQR + duration_quantiles[4]) %>%
  View()

# Visualize os 10 quantis da quantidade de visualizações
quantile(ted$views, probs = seq(0, 1, 0.1))

# Compare as seguintes estatísticas descritivas da quantidade de visualizações:
#   * Média e Mediana. Qual é maior?
summary(ted$views)

views_mean = mean(ted$views)

# A Média.

#   * Desvio Absoluto da Mediana e Desvio Padrão. Qual é maior?
sd(ted$views)
median(abs(ted$views - median(ted$views)))

# O Desvio Padrão.

#   * Desvio Absoluto da Mediana e IQR. Quantas vezes o IQR é maior que o Desvio Absoluto da Mediana?
IQR(ted$views) / median(abs(ted$views - median(ted$views)))

#   * Com base na média e na mediana, e na razão entre o IQR e o Desvio Absoluto da Mediana, 
#     você conclui que as quantidades de visualização estão distribuidas de forma simétrica em torno da média?

# Os valores estão concentrados na mediana, porém existem alguns casos em que altos valores puxam a média para cima.

# Calcule a média, o desvio padrão, a mediana e o IQR da quantidade de línguas dos seguintes grupos:
#     * 10% de vídeos com maior número de visualizações
#     * 10% de vídeos com menor número de visualizações
rbind(
  ted %>%
    arrange(views) %>%
    head(count(ted) * 0.1) %>%
    mutate(views_group = '10% menores'),
  ted %>%
    arrange(views) %>%
    tail(count(ted) * 0.1) %>%
    mutate(views_group = '10% maiores')
  ) %>%
  group_by(views_group) %>%
  summarise(languages_mean = mean(languages),
            languages_sd = sd(languages),
            languages_median = median(languages),
            languages_IQR = IQR(languages)) %>%
  ungroup() %>%
  arrange(desc(views_group)) %>%
  View()

# Determine a quantidade de apresentações por evento cujo nome inicie com TED. Utilize a função str_detect para este filtro
ted %>%
  filter(str_detect(event, "TED")) %>%
  distinct(event) %>%
  View()

# Determine, por evento cujo nome inicie com TED e que a quantidade de visualizações dos vídeos foi maior que a mediana calculada anteriormente.
#   * a quantidade de apresentações resultante do filtro, por evento
ted %>%
  filter(str_detect(event, "TED") & views > views_mean) %>%
  count(event) %>%
  filter(n > 10) %>%
  View()

#   * o ano do evento (utilizar o menor ano da data de publicação)
ted %>%
  filter(str_detect(event, "TED") & views > views_mean) %>%
  group_by(event) %>%
  summarise(published_year_min = min(year(published_date)), total = n()) %>%
  ungroup() %>%
  filter(total > 10) %>%
  View()

#   * a quantidade média de línguas das apresentações
ted %>%
  filter(str_detect(event, "TED") & views > views_mean) %>%
  summarise(languages_mean = mean(languages))

#   * o desvio padrão da quantidade de línguas
ted %>%
  filter(str_detect(event, "TED") & views > views_mean) %>%
  summarise(languages_sd = sd(languages))

#   * o coeficiente de variação da quantidade de línguas
ted %>%
  filter(str_detect(event, "TED") & views > views_mean) %>%
  summarise(languages_vc = sd(languages) / mean(languages))
  
### EXIBA SOMENTE OS EVENTOS COM MAIS DE 10 APRESENTAÇÕES

# Calcule e classifique as seguintes correlações
#     * Quantidade de visualizações e Quantidade de línguas
ted %>%
  summarise(cor = cor(views, languages)) %>%
  mutate(case_when(
    abs(cor) >= 0.0 & abs(cor) < 0.3 ~ 'Desprezível',
    abs(cor) >= 0.3 & abs(cor) < 0.5 ~ 'Fraca',
    abs(cor) >= 0.5 & abs(cor) < 0.7 ~ 'Moderada',
    abs(cor) >= 0.7 & abs(cor) < 0.9 ~ 'Forte',
    abs(cor) >= 0.9 ~ 'Muito forte'
  ))

#     * Quantidade de visualizações e Duração
ted %>%
  summarise(cor = cor(views, duration)) %>%
  mutate(case_when(
    abs(cor) >= 0.0 & abs(cor) < 0.3 ~ 'Desprezível',
    abs(cor) >= 0.3 & abs(cor) < 0.5 ~ 'Fraca',
    abs(cor) >= 0.5 & abs(cor) < 0.7 ~ 'Moderada',
    abs(cor) >= 0.7 & abs(cor) < 0.9 ~ 'Forte',
    abs(cor) >= 0.9 ~ 'Muito forte'
  ))

#     * Quantidade de visualizações e Quantidade de Comentários
ted %>%
  summarise(cor = cor(views, comments)) %>%
  mutate(case_when(
    abs(cor) >= 0.0 & abs(cor) < 0.3 ~ 'Desprezível',
    abs(cor) >= 0.3 & abs(cor) < 0.5 ~ 'Fraca',
    abs(cor) >= 0.5 & abs(cor) < 0.7 ~ 'Moderada',
    abs(cor) >= 0.7 & abs(cor) < 0.9 ~ 'Forte',
    abs(cor) >= 0.9 ~ 'Muito forte'
  ))

#     * Quantidade de Comentários e Quantidade de línguas
ted %>%
  summarise(cor = cor(comments, languages)) %>%
  mutate(case_when(
    abs(cor) >= 0.0 & abs(cor) < 0.3 ~ 'Desprezível',
    abs(cor) >= 0.3 & abs(cor) < 0.5 ~ 'Fraca',
    abs(cor) >= 0.5 & abs(cor) < 0.7 ~ 'Moderada',
    abs(cor) >= 0.7 & abs(cor) < 0.9 ~ 'Forte',
    abs(cor) >= 0.9 ~ 'Muito forte'
  ))

# Descarte os vídeos cuja duração seja maior que 3 desvios padrões da média. Calcule novamente as 5 correlações solicitadas
ted %>%
  filter(duration <= three_sd) -> ted

ted %>%
  summarise(cor = cor(views, languages)) %>%
  mutate(case_when(
    abs(cor) >= 0.0 & abs(cor) < 0.3 ~ 'Desprezível',
    abs(cor) >= 0.3 & abs(cor) < 0.5 ~ 'Fraca',
    abs(cor) >= 0.5 & abs(cor) < 0.7 ~ 'Moderada',
    abs(cor) >= 0.7 & abs(cor) < 0.9 ~ 'Forte',
    abs(cor) >= 0.9 ~ 'Muito forte'
  ))

#     * Quantidade de visualizações e Duração
ted %>%
  summarise(cor = cor(views, duration)) %>%
  mutate(case_when(
    abs(cor) >= 0.0 & abs(cor) < 0.3 ~ 'Desprezível',
    abs(cor) >= 0.3 & abs(cor) < 0.5 ~ 'Fraca',
    abs(cor) >= 0.5 & abs(cor) < 0.7 ~ 'Moderada',
    abs(cor) >= 0.7 & abs(cor) < 0.9 ~ 'Forte',
    abs(cor) >= 0.9 ~ 'Muito forte'
  ))

#     * Quantidade de visualizações e Quantidade de Comentários
ted %>%
  summarise(cor = cor(views, comments)) %>%
  mutate(case_when(
    abs(cor) >= 0.0 & abs(cor) < 0.3 ~ 'Desprezível',
    abs(cor) >= 0.3 & abs(cor) < 0.5 ~ 'Fraca',
    abs(cor) >= 0.5 & abs(cor) < 0.7 ~ 'Moderada',
    abs(cor) >= 0.7 & abs(cor) < 0.9 ~ 'Forte',
    abs(cor) >= 0.9 ~ 'Muito forte'
  ))

#     * Quantidade de Comentários e Quantidade de línguas
ted %>%
  summarise(cor = cor(comments, languages)) %>%
  mutate(case_when(
    abs(cor) >= 0.0 & abs(cor) < 0.3 ~ 'Desprezível',
    abs(cor) >= 0.3 & abs(cor) < 0.5 ~ 'Fraca',
    abs(cor) >= 0.5 & abs(cor) < 0.7 ~ 'Moderada',
    abs(cor) >= 0.7 & abs(cor) < 0.9 ~ 'Forte',
    abs(cor) >= 0.9 ~ 'Muito forte'
  ))

# Utilizando o data frame original, crie um dataframe com a mediana da duração dos vídeos por ano de filmagem. Calcule a correlação entre o ano e a mediana da duração
# e interprete o resultado
ted %>%
  group_by(film_date_year = year(film_date)) %>%
  summarise(duration_median = median(duration)) -> ted_median

cor(ted_median$film_date_year, ted_median$duration_median)

# Existe uma correlação muito forte, que mostra que quanto mais recente o filme, mais curto ele será.
