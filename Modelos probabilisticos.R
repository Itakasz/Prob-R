##### Funcoes Iniciais ####

x = c(1.987,2.876,3.456,1.776)
x
#Verdadeiro / falso     ifelse()
ifelse(x<=2,1,0)
ifelse(x<=2 & x>=3,'correto', 'falso')
# (x,y,z) = (condicao, verdadeiro, falso)


#Arredondar     round()
round(x,2)
#numerador indica quantas casas decimais sera o arredondamento


#Area Grafico
geom_area()

#Linha grafico
geom_hline()

#Funcao de Probabilidade P(X)
dnome
dbinom(x = 1, size = 10, prob = 0.3)     #Probabilidade de x
sum(dbinom(0:5, 10, 0.3))                # por soma x<=6
sum(dbinom(6:10, size = 10, prob = 0.3)) #Por soma x>=6

#Funcao Acumulada P(X<=a)
pnome
1-pbinom(5,10,0.3) #Por complemento                            x>=6
pbinom(5,10,0.3,lower.tail = F) #Desligando a cauda de baixo   x>=6
pbinom(5, size = 10, prob = 0.3)                             #P(X<=a)
#O comando p e' igual o comando 0:5

#Gerar Valores Aleatorios
rnome

#Funcao Quantilica
qnome

##### Exemplos --- Distribuicao Binomial  d, p, r ####   

#Ex1: Suponha um experimento realizado 10 vezes, com probabilidade de sucesso p=0.3
# Deternube P(X=1)
# n = tentativas, p = prob de sucesso
dbinom(x = 1, size = 10, prob = 0.3)

#Ex2: Determine P(x<=5)
0:5 #sequencia de 0 a 5
sum(dbinom(0:5, 10, 0.3)) # por soma

#Ex3: Determine P(X>=6)
sum(dbinom(6:10, size = 10, prob = 0.3)) #Por soma 
1-pbinom(5,10,0.3) #Por complemento
pbinom(5,10,0.3,lower.tail = F) #Desligando a cauda de baixo

#Ex4: Gere uma amostra de tamanho 100
rbinom(n = 100,size = 10, prob = 0.3)

#Ex5: Determine as probabilidades de todos os valores de x e salve em um objeto
xx = 0:10
dbinom(xx,10, 0.3)
px = dbinom(xx,10, 0.3) #Probabilidade de X

#Ex6: Faça um gráfico de barras com os valores obtido
library(ggplot2)
dados = data.frame(xx,px)
dados

ggplot(dados, aes(x = xx, y = px)) + geom_col() + geom_text(aes(label = round(px,2))) #Gráfico de barras (xx,px)
#label para mostrar os valores de px
#round para arredondar os valores

#Ex7: Faca uma are para x<3
ggplot(dados, aes(x = xx, y = px)) + geom_col(fill = ifelse(dados$xx<=3,'blue','gray')) + 
  geom_text(aes(label = round(px,2))) 

#Ex8: Faça uma are para 1<x<5
ggplot(dados, aes(x = xx, y = px)) + geom_col(fill = ifelse(dados$xx <= 1 | dados$xx >= 5,'blue','gray')) + 
  geom_text(aes(label = round(px,2))) 

#Ex9: Faça um grafico de barras para a funcao acumulada
xx = 0:10 #Todos os valores que X pode assumir
px = dbinom(xx, 10, 0.3)        #Prob de x
ax = pbinom(xx, 10, 0.3)        #Acumulada de x
ax

dados = data.frame(xx,px,ax)
ggplot(dados, aes(xx, ax)) + geom_col() + geom_text(aes(label = round(ax, 2)))

##### Distribuicao de Poison ####

#Suponha x ~ Pois, com media = 50 (lambida)
#Determine P(X = 4)
dpois( x = 4, lambda = 2.5)

#Determine a P(X <= 45)
dpois(0:45,50)
sum(dpois(0:45,50))
ppois(45,50)

#Determine a P(X >= 50)
dpois(0:49, 50)
1 - sum(dpois(0:49, 50))
1- ppois(49,50)
ppois(49,50, lower.tail = F)

#Gere uma amostra de tamanho 100
rpois(100, 50)

#Faca um grafico das probabilidades
xx = 0:100
px = dpois(xx, 5)
dados = data.frame(xx,px)
ggplot(dados, aes(xx, px)) + geom_col()

#xlim(min, max)
ggplot(dados, aes(xx, px)) + geom_col() + geom_text(aes( label = round(px, 2))) + 
  xlim(25,75)
#Graficos
#Grafico de area para x <= 40
ggplot(dados, aes(xx,px)) + geom_col(fill = ifelse(dados$xx <= 40, 'blue','gray')) +
  geom_text(aes (label = round(px, 2)))

#Grafico de area para x>40 e x<60
ggplot(dados, aes(xx,px)) + geom_col(fill = ifelse(dados$xx > 40 & dados$xx < 60, 'blue','gray')) +
  geom_text(aes (label = round(px, 2)))

#Grafico da acumulada
xx = 25:75
px = dpois(xx, 50)
ax = ppois (xx, 50)
dados = data.frame(xx,px,ax)
ggplot(dados, aes(xx,ax)) + geom_col() + geom_text(aes (label = round(ax,2)))

#Marque a area para x < 50
ggplot(dados, aes(xx,ax)) + geom_col(fill = ifelse(dados$xx < 50,'red','gray')) + geom_text(aes (label = round(ax,2)))


##### Distribuicao Exponencial ####

#Suponha X~Exp, com media 50 (lambda=1/50)
#Calcule a P(X<=40)
pexp(40, rate = 1/50) #Rate é inversa da media

#P(X>40)
1 - pexp(40, rate = 1/50)
pexp(40, rate = 1/50, lower.tail = F)

#Gerar uma amostra de tamanho 5000 e apresentar o histograma
xx = rexp(5000, 1/50)
px = dexp(xx, 1/50)
ax = pexp(xx, 1/50)
dados = data.frame(xx,px,ax)
#Contagem
ggplot(dados, aes(xx)) + geom_histogram()
#Probabilidade
ggplot(dados, aes(xx)) + geom_histogram(aes(y =..density..))

#Adicione a funcao de probabilidade exponencial no histograma
ggplot(dados, aes(xx)) + geom_histogram(aes(y =..density..)) +
  geom_line(aes(xx,px, col = 'red', lwd = 2))

#Apresentar apenas a função de probabilidade
ggplot(dados, aes(xx)) + geom_line(aes(xx,px, col = 'red', lwd = 2))

#Apresente a area de x < 60
ggplot(dados, aes(xx)) + geom_line(aes(xx, px), col = 'red', lwd = 2) +
  geom_area(mapping = aes(x = ifelse(xx < 60, xx, 0), y = ifelse(xx < 60, px, 0)), fill = 'blue') 

#Apresente a Acumulada
ggplot(dados, aes(xx, ax)) + geom_line(aes(xx,ax))

#Determine a mediana de X e represente no grafico
pexp(20, 1 / 50)
qexp(0.32968, 1/50)  #q é o inverso de p

#Achar a mediana
qexp(0.5, 1 / 50)
md = qexp(0.5, 1 / 50)
#Area abaixo de 60
ggplot(dados,aes(xx))+geom_line(aes(xx,px),col='red',lwd=2)+
  geom_area(mapping = aes(x=ifelse(xx<md,xx,0),y=ifelse(xx<md,px,0)),fill='blue')  
#Acumulada abaixo de 60
ggplot(dados,aes(xx))+geom_line(aes(xx,ax))+
  geom_area(mapping = aes(x=ifelse(xx<md,xx,0),y=ifelse(xx<md,ax,0)),fill='blue')+
  geom_hline(yintercept = 0.5)

##### Distribuicao Normal ####

#Suponha que x~N(media = 50, var = 25)
#Ex1 = Calcule a P(X<45)
1 - pnorm(45,mean = 50, sd = sqrt(25))
pnorm(45,50,sqrt(25),lower.tail = F)

#Gere uma amostra de tamanho 5000 e apresente um histograma
xx = rnorm(5000, 50, 5)
px = dnorm(xx, 50, 5)
ax = pnorm(xx, 50, 5)
dados = data.frame(xx,px,ax)

ggplot(dados, aes(xx)) + geom_histogram(aes(y = ..density..))

#Apresente a funcao de probabilidade
ggplot(dados, aes(xx,px)) + geom_line()

#Apresente a funcao acumulada
ggplot(dados, aes(xx,ax)) + geom_line()

#Determine o valor de a, tal que a P(x<a) = 0.38
a = qnorm(0.38, 50, 5)
ggplot(dados, aes(xx,px)) + geom_line() + geom_area(mapping = aes(x = ifelse(xx<a,xx,30), y = ifelse(xx<a,px,0)), fill = 'blue')
