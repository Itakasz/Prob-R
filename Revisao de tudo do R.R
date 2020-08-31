#####Codigos Basicos####
#soma (1+1)
1+1

#funções algebricas sin(pi/2)
sin(pi/2)

##### Vetor #### 

#vetor (x=c(n1,n2,...
(x=c(1,2,3,4))
#para fazer sequencia de um vetor   (x=n:N)
(x=2:10)

##### Matriz ####

#matriz de coluna  matrix(x,nrow = 2)
matrix(x,nrow = 2)

#matriz de linha matrix(x,nrow = 2, byrow = T)
matrix(x,nrow = 2, byrow = T)
z=matrix(x,nrow = 2, byrow = T)

#matriz inversa    solve ()
solve(z)

#matriz transposta    t()
t(z)

#determinante       determinant()
determinant(z)


##### Derivada e Integral ####

#derivada  D(expression(...),'x')
D(expression(2*tan(x)*sin(x)),'x')

#integral     antiD(...)~x)
library(mosaicCalc)
antiD(sin(x)~x)

#integral definida  integrate(funcao,n,N)
integrate(funcao,1,2)





##### Funcao ####      

#funcao      function(x) {
funcao1=function(x){
  x+1
}

#funcao bhaskara
funcao2=function(a,b,c){ 
  delta=b^2-4*a*c
  x1=(-b-sqrt(delta))/(2*a)
  x2=(-b+sqrt(delta))/(2*a)
  xs=c(x1,x2)
  xs
}



##### Estatistica ####

#carregar dados       dados=read.csv2('nomedoarquivo.csv")
dados=read.csv2('dados.csv')

# media mean(dados $ titulo que interessa)
mean(dados$renda)

#variancia    var(dados $ titulo que interessa)
var(dados$renda)

#desvio padrão     sd(dados $ titulo que interessa)
sd(dados$renda)
