#' ---
#' title: "Operadores comuns para tipos numéricos"
#' output: html_notebook
#' ---
#' 
#' #### Operadores Aritméticos
#' 
#' As [operações aritméticas](https://stat.ethz.ch/R-manual/R-devel/library/base/html/Arithmetic.html) definidas na linguagem R são as seguintes:
#' 
#' | Símbolo   | Operador       | Exemplo               |
#' |----------:|:---------------|:---------------------:|
#' | `+ x`     | Valor positivo | `+1` = `r +1`         | 
#' | `- x`     | Valor negativo | `-2` = `r -2`         |
#' | `x + y`   | Soma           | `1 + 1`  = `r 1 + 1`  |
#' | `x - y`   | Subtração      | `1 - 1`  = `r 1 - 1`  |
#' | `x * y`   | Multiplicação  | `7 * 6`  = `r 7 * 6`  |
#' | `x / y`   | Divisão        | `9 / 2`  = `r 9 / 2`  |
#' | `x ^ y`   | Potência       | `8 ^ 3`  = `r 8 ^ 3`  |
#' | `x %% y`  | Resto          | `9 %% 2` = `r 9 %% 2` |
#' | `x %/% y` | Divisão Inteira| `9 %/% 2`= `r 9 %/% 2`|
#' 

+1       # Valor positivo 
-2       # Valor negativo    
1 + 1    # Soma             
1 - 1    # Subtração       
7 * 6    # Multiplicação   
9 / 2    # Divisão        
8 ^ 3    # Potência       
9 %% 2   # Resto          
9 %/% 2  # Divisão Inteira

#' #### Operadores Relacionais
#' 
#' As [operações relacionais](https://stat.ethz.ch/R-manual/R-devel/library/base/html/Comparison.html) definidas na linguagem R são as seguintes:
#' 
#' | Símbolo   | Operador        | Exemplo                                               |
#' |----------:|:----------------|:------------------------------------------------------|
#' | >         | Maior que       | `5 > 3` = `r 5 > 3`                                   |
#' | <         | Menor que       | `3 < 5` = `r 3 < 5`                                   |
#' | >=        | Maior ou igual  | `5 >= 5` = `r 5 >= 5`                                 |
#' | <=        | Menor ou igual  | `2 <= 3` = `r 2 <= 3`                                 |
#' | ==        | Igual a         | `0 == 0` = `r 0 == 0`                                 |
#' | !=        | Diferente de    | `2 != 3` = `r 2 != 3`                                 |
#' 

5 > 3  # Maior que       
3 < 5  # Menor que       
5 >= 5 # Maior ou igual
2 <= 3 # Menor ou igual
0 == 0 # Igual a       
2 != 3 # Diferente de  

#' 
#' #### Operadores Lógicos
#' 
#' As [operações lógicas](https://stat.ethz.ch/R-manual/R-devel/library/base/html/Logic.html) definidas na linguagem R são as seguintes:
#' 
#' | Símbolo   | Operador        | Exemplo                                               |
#' |----------:|:----------------|:------------------------------------------------------|
#' | ! x       | Negação         | `! FALSE` = `r !FALSE`                                |
#' | x & y     | E vetorial      | `c(TRUE, FALSE, TRUE, FALSE) & c(TRUE, TRUE, FALSE, FALSE)` = `r c(TRUE, FALSE, TRUE, FALSE) & c(TRUE, TRUE, FALSE, FALSE)`|
#' | x && y    | E vetor unário  | `TRUE & FALSE` = `r TRUE && FALSE`                    |
#' | x \| y    | OU vetorial     | `c(TRUE, FALSE, TRUE, FALSE) | c(TRUE, TRUE, FALSE, FALSE)` = `r c(TRUE, FALSE, TRUE, FALSE) | c(TRUE, TRUE, FALSE, FALSE)`|
#' | x \|\| y  | OU vetor unário | `TRUE | FALSE` = `r TRUE || FALSE`                    |
#' | xor(x, y) | OU exclusivo    | `xor(TRUE, TRUE)` = `r xor(TRUE, TRUE)`               |
#' 
#' As operações E e OU unitárias existem para serem aplicadas em testes condicionais e predicados. Falaremos mais sobre predicados na aula de programação funcional.
#' 

!FALSE                                                     # Negação        
c(TRUE, FALSE, TRUE, FALSE) & c(TRUE, TRUE, FALSE, FALSE)  # E vetorial     
TRUE && FALSE                                              # E vetor unário 
c(TRUE, TRUE) && c(FALSE, TRUE)                            # E vetor unário :: VER RESULTADO
c(TRUE, FALSE, TRUE, FALSE) | c(TRUE, TRUE, FALSE, FALSE)  # OU vetorial    
TRUE || FALSE                                              # OU vetor unário 
c(FALSE, TRUE) || c(FALSE, FALSE)                          # OU vetor unário:: VER RESULTADO 
xor(TRUE, TRUE)                                            # OU exclusivo   

#' 
#' #### Algumas funções numéricas úteis
#' 
#' | Função    | Operação                     | Exemplo                           |
#' |----------:|:-----------------------------|:----------------------------------|
#' | min(x)    | Mínimo de um vetor           | `min(20:40)` = `r min(20:40)`     |
#' | max(x)    | Máximo de um vetor           | `max(20:40)` = `r max(20:40)`     |
#' | range(x)  | Mínimo e máximo de um vetor  | `range(20:40)` = `r range(20:40)` |
#' | sum(x)    | Soma dos valores de um vetor | `sum(1:5)` = `r sum(1:5)`         |
#' 

min(20:40)   # Mínimo de um vetor           
max(20:40)   # Máximo de um vetor           
range(20:40) # Mínimo e máximo de um vetor 
sum(1:5)     # Soma dos valores de um vetor

