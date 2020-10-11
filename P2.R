source('https://git.io/JUNHv')
avaliacao2(2171236)

library(ggplot2)

#####EX 1 ####
#1 - Dado que 49 % dos produtos de uma empresa nao apresentam falhas apos a produ-
#cao, em uma amostra de 12 componentes, determine:

#1A - A probabilidade de que nenhum falhe
dbinom(12, 12, 0.49)
#A probabilidade que nenhum falhe e' igual a 0,019%

#1B - A probabilidade de que no minimo 1 falhe
sum(dbinom(1:12, 12, 0.51)) #P(X<=a)
#ou
1 - pbinom(0, 12, 0.51)  #Por complemento
#ou
pbinom(0,12,0.51,lower.tail = F) #Desligando a cauda de baixo
#Portanto a probabilidade que lemo menos 1 falhe e' = 99,98%

#1C - A probabilidade de que ao menos 3 falhem
sum(dbinom(3:12, 12, 0.51)) #P(X<=a)
#ou
1 - pbinom(2, 12, 0.51)  #Por complemento
#ou
pbinom(2,12,0.51,lower.tail = F) #Desligando a cauda de baixo
#Portanto a probabilidade que lemo menos 1 falhe e' = 98,37%

#1D - Se a amostra for de 50 componentes, quantos irao falhar em media?
nfalha = 50 * 0.51
nfalha
#A media de falha com x = 50 sera de 25.5%

#1E - Construa um grafico com todas as probabilidades
x = 0:12
px = dbinom(x, 12, 0.49)
dados = data.frame(x,px)
dados
ggplot(dados, aes(x = x, y = px, fill = px)) + geom_col() + geom_text(aes(label = round(px,2))) +
  labs(title = 'Grafico com as probabilidades', x = 'X', y = 'P(X)') 
#Gráfico de barras (x,px)

#####EX 2 ####
#2 A capacidade de processamento de um componente eletronico eh,
# em media, de 2.7 Hz por segundo. Determine:

#2A - A probabilidade de processar  4 ciclos  por segundo?
#Suponha x ~ Pois, com media = 2.7 (lambida)
#Determine P(X = 4)
dpois(4, lambda = 2.7) #Distribuicao de Poison
# A probabilidade de processar 4 ciclos por segundo e' = 14.88%

#2B - A probabilidade de processar  3 ou mais ciclos  por segundo?
#Suponha x ~ Pois, com media = 3 (lambida)
#Determine P(X = 3)
1 - sum(dpois(0:3, 2.7))
#ou
1- ppois(3,2.7)
#ou
ppois(3, 2.7, lower.tail = F)
# A probabilidade de processar 4 ciclos por segundo e' = 28.59%

#2C - Apresente graficamente a maior parte das probabilidades
xx=0:10
pxx=dpois(xx,2.7)
dados=data.frame(xx,pxx)
ggplot(dados, aes(x = xx, y = pxx, fill = pxx)) + geom_col() + geom_text(aes(label = round(pxx,2))) +
  labs(title = 'Grafico com as probabilidades', x = 'X', y = 'P(X)')

#2D - A probabilidade de processar 300 ou menos ciclos em um minuto?
#Det P(x <= 300)
px300 = 60 * 2.7
px300 
ppois(300, 162)
#A probabilidade e' igual a 100%

0


#####EX 3 ####
#3 O tempo de falha de um componente eletrico segue uma distribuicao 
#exponencial com media 2.7 anos. Calcule:

#3A - A probabilidade da falha ocorrer apos 7 anos?
#Suponha x ~ Exp, com media = 2.7 (lambida = 1/2.7)
#Calcule a P(X>7)
1 - pexp(7, rate = 1/2.7)
pexp(7, rate = 1/2.7, lower.tail = F)
#P(X>7) = 7.48%

#3B - A probabilidade da falha ocorrer antes de 3 anos?
#Suponha x ~ Exp, com media = 2.7 (lambida = 1/2.7)
#Calcule a P(X<3)
pexp(4, 1/2.7)
#P(X<3) = 77.27%

#3C - A probabilidade da falha ocorrer entre 2 e 6 anos?
pexp(2:6, 1/2.7)
a = 0.5232394 + 0.6708070 + 0.7726993 + 0.8430537 + 0.8916320
a / 5 * 100 
# A probabilidade de falha entre 2 a 6 anos e' = 74.03%

#3D - Qual a variancia do tempo de falha?
#1 / lambida * lambida
1 / (1 / (2.7 * 2.7))
# A variancia do tempo de falha e' = 7.29

#3E - Apresente graficamente a distribuicao de probabilidade
xxx = seq(0, 100)
pxxx = dexp(xxx,1 / 2.7)
axxx = pexp(xxx, 1 / 2.7)
dados=data.frame(xxx,pxxx,axxx)
ggplot(dados, aes(xxx)) + geom_line(aes (xxx ,pxxx), col = 'red', lwd = 2)
#Apresente a funcao de probabilidade
ggplot(dados, aes(xxx,pxxx)) + geom_line(aes(xxx,pxxx), col = 'red', lwd = 2)

#####EX 4 ####
#4 Seja X uma variavel aleatoria que segue distribuicao normal com media
#92.3 e variancia 150. Determine:

#4A - P(X < 115)  
pnorm(115, 92.3, sqrt(150))
#P(X < 115) = 96.81%

#4B - P(85<X<110)
pnorm(85,92.3,sqrt(150),lower.tail = F)
# ou
1 - pnorm(110, 92.3, sd = sqrt(150))
#P(85<X<110) = 72.44%

#4C - P(X> 65)
pnorm(65, 92.3, sqrt(150), lower.tail = F)
#ou
1 - pnorm(65, 92.3, sqrt(150))
#P(X> 65) = 98.71%

#4D - O valor de k tal que  P(X<K)=0,45	
k = qnorm(0.45, 92.3, sqrt(150))
k
k = 90.76 para  P(X<K)=0,45	

#4E - Apresente graficamente a distribuicao normal
xxxx = rnorm(5000, 92.3, sqrt(150))
pxxxx = dnorm(xxxx, 92.3, sqrt(150))
axxxx = pnorm(xxxx, 92.3, sqrt(150))
dados = data.frame(xxxx,pxxxx,axxxx)
ggplot(dados, aes(xxxx)) + geom_histogram(aes(y = ..density..))
#Apresente a funcao de probabilidade
ggplot(dados, aes(xxxx,pxxxx)) + geom_line()

#5 - 
#####EX 5 ####
#5 Uma amostra de tamanho 60 de uma variavel que segue uma distribuicao
#normal foi obtida e se encontra salva no objeto "amostra"(digite amostra para ver!).
#Determine a probabilidade de X<25
amostra
mean(amostra)
sd(amostra)
pnorm(25,19.57059, 4.716847)
#Portanto P(X<25) = 87.51%.