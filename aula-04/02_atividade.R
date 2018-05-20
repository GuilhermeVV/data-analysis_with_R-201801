library(tidyverse)
library(lubridate)

## Nesta atividade você deve utilizar o resultado do exercício 01 da Atividade da aula 03 (remuneração em dólares convertida para reais)
## Utilize o código daquele exercício como ponto de partida para esta atividade. 
## Sempre utilize o caminho relativo, não o caminho absoluto, pois não funcionará na correção do exercício.

### IMPORTANTE ###
## Se você utilizar alguma função própria ou do material de aula, o código da(s) função(ões) deve estar neste arquivo da atividade.

## Vamos começar carregando o arquivo de dados preparado para esta aula

salarios <- read_csv("aula-03/data/201802_dados_salarios_servidores.csv.gz")

salarios %>%
  mutate(REMUNERACAO_FINAL = REMUNERACAO_REAIS + (REMUNERACAO_DOLARES * 3.24)) %>%
  filter(REMUNERACAO_FINAL >= 900) -> salarios

### 1 ####
## 
## Correlação de ano de ingresso por cargo
## - Determine o coeficiente de correlação entre o tempo em anos desde a DATA_INGRESSO_ORGAO e o tempo em anos desde a DATA_DIPLOMA_INGRESSO_SERVICOPUBLICO
##   para todos os cargos que possuem no mínimo 200 servidores.
## - Crie uma coluna que determina se a correlação é positiva ou negativa, e outra coluna que define a força da correlação de acordo com 
##   o material visto em aula sobre interpretação do coeficiente.
## - O resultado desta atividade deve ser um Data Frame com as variáveis de Cargo, Coeficiente de Correlação, Direção da Correlação e Força da Correlação
## 
### # ####

salarios %>%
  group_by(DESCRICAO_CARGO) %>%
  summarize(qtd = n()) %>%
  ungroup() %>%
  filter(qtd >= 200) %>%
  pull(DESCRICAO_CARGO) -> cargosMais200

# ou

salarios %>%
  count(DESCRICAO_CARGO) %>%
  filter(n >= 200) %>%
  pull(DESCRICAO_CARGO) -> cargosMais200

salarios %>%
  filter(DESCRICAO_CARGO %in% cargosMais200) %>%
  group_by(DESCRICAO_CARGO) %>%
  summarize(corAnoIngressoDiploma = cor(year(DATA_INGRESSO_ORGAO), year(DATA_DIPLOMA_INGRESSO_SERVICOPUBLICO))) %>%
  ungroup() %>%
  mutate(direcaoCorrelacao = if_else(sign(corAnoIngressoDiploma) == 1, 'Positiva', 'Negativa'),
         forcaCorrelacao =
          case_when(
           abs(corAnoIngressoDiploma) >= 0.0 & abs(corAnoIngressoDiploma) < 0.3 ~ 'Correlação desprezível',
           abs(corAnoIngressoDiploma) >= 0.3 & abs(corAnoIngressoDiploma) < 0.5 ~ 'Correlação fraca',
           abs(corAnoIngressoDiploma) >= 0.5 & abs(corAnoIngressoDiploma) < 0.7 ~ 'Correlação moderada',
           abs(corAnoIngressoDiploma) >= 0.7 & abs(corAnoIngressoDiploma) < 0.9 ~ 'Correlação forte',
           abs(corAnoIngressoDiploma) >= 0.9 ~ 'Correlação muito forte'
           )
    ) -> salariosCor

### 2 ###
##
## - A partir do dataset do exercício anterior, selecione os 10 cargos de correlação mais forte (seja positiva ou negativa) e os 
##   10 cargos de correlação mais fraca (de novo, independente de ser positiva ou negativa)
## - Para estes 20 cargos, determine a Moda do órgão de lotação (ORGSUP_LOTACAO) e de exercício (ORGSUP_EXERCICIO)
## - Reponda se existe diferença entre as modas e se existe relação entre a Força da Correlação e a diferença entre as modas 
##   (caso haja diferença)
##
### # ###

union(
  salariosCor %>%
    arrange(abs(corAnoIngressoDiploma)) %>%
    head(10) %>%
    pull(DESCRICAO_CARGO),
  salariosCor %>%
    arrange(abs(corAnoIngressoDiploma)) %>%
    tail(10) %>%
    pull(DESCRICAO_CARGO)
) -> cargosCor

salarios %>%
  filter(DESCRICAO_CARGO %in% cargosCor) %>%
  group_by(DESCRICAO_CARGO, ORGSUP_LOTACAO) %>%
  summarize(modaLotacao = n()) %>%
  mutate(maiorQtd = max(modaLotacao)) %>%
  ungroup() %>%
  filter(modaLotacao == maiorQtd) %>%
  select(DESCRICAO_CARGO:modaLotacao) -> salariosModaLotacao

salarios %>%
  filter(DESCRICAO_CARGO %in% cargosCor) %>%
  group_by(DESCRICAO_CARGO, ORGSUP_EXERCICIO) %>%
  summarize(modaExercicio = n()) %>%
  mutate(maiorQtd = max(modaExercicio)) %>%
  ungroup() %>%
  filter(modaExercicio == maiorQtd) %>%
  select(DESCRICAO_CARGO:modaExercicio) -> salariosModaExercicio

merge(
  merge(
    salariosCor %>%
      filter(DESCRICAO_CARGO %in% cargosCor),
    salariosModaLotacao
  ),
  salariosModaExercicio
) %>%
  arrange(abs(corAnoIngressoDiploma)) %>%
  View()

# Parece seguro dizer que quando há forte correlação no ano de diploma e de ingresso no órgão, o orgão atual de exercício é o mesmo órgão de lotação.
# Em resumo, esses funcionários públicos parecem se manter em seus cargos desde o início.
