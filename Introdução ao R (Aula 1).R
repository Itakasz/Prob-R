#comentario

#rodar linha = ctrl + enter
1+1


#funcoes basicas 
sin(pi/2)
cos(pi)
tan(pi)

#vetor
(x=c(1,2,5,6))
(y=3:11)
x+y


#matrizes
z=matrix(x,nrow = 2, byrow = T)  #matriz linear

solve (z)                      #matriz inversa

t(z)                          #matriz transposta

determinant(z)               #determinante


#derivadas
D(expression(tan(x)),'x')
D(expression(2*tan(x)*sin(x)),'x')

#integral
library(mosaicCalc)
antiD(sin(x)~x)          #integral


#funcao
funcao=function(x){
  x+1
  }
funcao(3)


#baskara
funcao2=function(a,b,c){
  delta=b^2-4*a*c
  x1=(-b-sqrt(delta))/(2*a)
  x2=(-b+sqrt(delta))/(2*a)
  xs=c(x1,x2)
  xs
}

funcao2(4,-2,-6)


#integral definida
integrate(funcao,0,2)


#como ler dados no R
dados=read.csv2("dados.csv")
dados$nome


#estatistica
mean(dados$renda)   #media

var(dados$renda)    #variancia
  
sd(dados$renda)     #desvio padrao




