#carregar dados
dados=read.csv('dados_plot.csv',sep=',')
dados

#dados iniciais
head(dados)        

#dados por linha e coluna
dados[L,C]        
dados[2,]
dados$company

#variaveis
names(dados)

#traduzir para portugues as informacoes
names(dados)=c('linha','empresa','rendimento','tamanho','vendas','palavra','fre-palavra')
dados 


#tabela
table(dados$empresa,dados$tamanho)  #frequencia de ocorrencia

mean(dados$rendimento)             #rendimento medio               


#medidas por variaveis
tapply(dados$rendimento,dados$empresa,mean)


#graficos com ggplot
library(ggplot2)

##### grafico de coluna/barra ####

#para variaveis nao numericas

#ggplot base
ggplot(dados,aes(x=empresa))  

#adicionar colunas geom_bar
ggplot(dados,aes(x=empresa))+geom_bar()                 

#adicionar barras coord_flip
ggplot(dados,aes(x=empresa))+geom_bar()+coord_flip()

#adicionar cores fill
ggplot(dados,aes(x=empresa,fill=empresa))+geom_bar()+coord_flip() 

#nome dos eixos labs()
ggplot(dados,aes(x =empresa,fill=empresa)) +geom_bar()+           
  labs(title = 'Titulo', x = 'Empresa', y = 'Total')

#cores scale_fill_brewer
ggplot(dados,aes(x =empresa,fill=empresa)) +geom_bar()+     
  scale_fill_brewer(palette="Set3")

#tema de fundo theme_NOME()
ggplot(dados,aes(x =empresa,fill=empresa)) +geom_bar()+     
  scale_fill_brewer(palette="Set3")+theme_light()

#colunas agrupadas(adicionar uma nova info, em fill)
ggplot(dados,aes(x =empresa,fill=tamanho)) +geom_bar()+    
  scale_fill_brewer(palette="Set3")+theme_light()        
head(dados)

#dividir por variavel facet_wrap()
ggplot(dados,aes(x =empresa,fill=empresa)) +geom_bar()+    
  scale_fill_brewer(palette="Set3")+theme_light()+         
  facet_wrap(~tamanho)


##### Graficos de dispersao ####
#para variaveis numericas

#base
ggplot(dados,aes(x=rendimento,y=vendas)) 

#pontos geom_point
ggplot(dados,aes(x=rendimento,y=vendas))+geom_point()

#tendenciageom_Smooth()
ggplot(dados,aes(x=rendimento,y=vendas))+geom_point()+geom_smooth()

#colorir (col=)
ggplot(dados,aes(x=rendimento,vendas,col=empresa))+geom_point()

#tendencia por empresa 
ggplot(dados,aes(x=rendimento,vendas,col=empresa))+geom_point()+geom_smooth()

#remover desvio padrao
ggplot(dados,aes(x=rendimento,vendas,col=empresa))+geom_point()+geom_smooth(se=F)

#separar   facet_wrap
ggplot(dados,aes(x=rendimento,vendas,col=empresa))+geom_point()+geom_smooth(se=F)+
  facet_wrap(~empresa)+theme_minimal()

##### grafico de pizza ####
#criar banco de dados
dados1=as.data.frame(table(dados$empresa))
dados1
names(dados1)[1]='empresa'
dados1

dados2=dados1

#somar o total sum()
sum(dados2$Freq)

#porcentagem
dados2$Freq/sum(dados2$Freq)

#arredondar o numero round
dados2$Freq=round(dados2$Freq/sum(dados2$Freq),3)
dados2


#grafico de pizza
ggplot(dados2, aes(x = "", y = Freq, fill = empresa)) + 
  geom_bar(width = 1, stat="identity") +   coord_polar("y",start = 0) +
  scale_fill_brewer(palette="Set3")

#adicionar barra geom_bar(width = 1, stat="identity")
#entortar barra utilizando coord_polar("y",start = 0)

#grafico dunnet (x=2 e +xlim(0.5,2.5))
ggplot(dados2, aes(x=2, y=Freq, fill=empresa)) +
  geom_bar(width = 1, stat = "identity")+ coord_polar("y", start = 0)+ 
  scale_fill_brewer(palette = "Set3") + xlim(0.5,2.5)+theme_minimal()

##### histograma ####
#parecido com barra/coluna, porem para var. num. continua

#base + histograma: geom_histogram()
ggplot(dados,aes(rendimento)) + geom_histogram()

#definir numero de classes bins=c
ggplot(dados,aes(rendimento)) + geom_histogram(bins=5)
 
#colorir geom_histogram(fill='')
ggplot(dados,aes(rendimento)) + geom_histogram(bins=5,fill='purple')

#agrupando
ggplot(dados,aes(rendimento)) + geom_histogram(aes(fill=empresa),bins=5)


##### densidade ####
#base
ggplot(dados, aes(rendimento))

#densidade geom_density
ggplot(dados,aes(rendimento)) + geom_density()

#densidade por variavel geom_density(aes(fill=''))
ggplot(dados,aes(rendimento)) + geom_density(aes(fill=empresa))

#transparencia alpha
ggplot(dados,aes(rendimento)) + geom_density(aes(fill=empresa,alpha=0.3))

ggplot(dados,aes(rendimento)) + geom_density(aes(fill=empresa,alpha=0.3)) +
  facet_wrap (~empresa)


##### box-plot ####
# min = 1 Q1=1.5  2 media 4 Q3=4.5  max = 5

#base
ggplot(dados,aes(y=rendimento))

#boxplot
ggplot(dados, aes( y=rendimento))+geom_boxplot()

#linha central representa a media
#a imagem branca abaixo 25% e acima entre 75%
#as linhas representam valores abaixo de 25% e acima de 75%

#boxplot por variavel
ggplot(dados, aes( x=empresa, y=rendimento,fill=empresa))+geom_boxplot()+
  facet_wrap(~tamanho)


##### violino ####

#base
ggplot(dados, aes(x=empresa, y=rendimento,fill=empresa))+geom_violin()

##### tree map ####
library(treemapify)
dados1

#base
ggplot(dados1, aes(area=Freq,fill=empresa)) + geom_treemap()


#rendimento medio por tamanho x tipo de empresa
interaction(dados$empresa,dados$tamanho)

dados$interacao=interaction(dados$empresa,dados$tamanho)
dados

tapply(dados$rendimento,dados$interacao,mean)

#banco de dados as.data.frame
#media
as.data.frame(tapply(dados$rendimento,dados$interacao,mean))
dados3=as.data.frame(tapply(dados$rendimento,dados$interacao,mean))
dados3$nomes=labels(dados3)[[1]]
dados3
names(dados3)[1]='total'
dados3

ggplot(dados3,aes(area=total,fill=nomes))+geom_treemap()


##### nuvem de letras ####
library()

#selecionar as colunas que queremos usar
dados4=dados[,6:7]
dados4

library

