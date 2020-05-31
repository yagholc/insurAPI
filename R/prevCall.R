library(readr)

# Funções base ------------------------------------------------------------

Anuid <- function(i, idade, n , b, qx){ #para calcular a variancia... v2<-(1-(1/(1+i))^(1:n))/(1-(1/(1+i)))
  px <- 1-qx
  df<-1
  v <- (1/(1+i))^(0:(n-1))
  pxx <- c(1, cumprod( px[(idade+1):(idade+n-1)]) )
  
  ax <- (b* sum(v*pxx))
  return(ax) 
}

AnuidF <- function(i, idade, n , qx){ #pAnuidade fracionada
  px <- 1-qx
  df<-1
  v <- (1/(1+i))^(0:(n-1))
  pxx <- c(1, cumprod( px[(idade+1):(idade+n-1)]) )
  
  ax <- sum(v*pxx) - 11/24 * (1-Dotal_Puro(i, idade,  n, 1, qx))
  return(ax) 
}

# Anuidade vitalícia
# df: 0 para postecipado e 1 para antecipado
# i= taxa de juros, n= período, b = benefício
Anuidvit <- function(i, idade, b, qx){ #nv=nevermind só coloquei para padronizar a chamada de produtos
  n <- 116-idade
  px <- 1-qx
  v <- (1/(1+i))^((1):(n))
  pxx <- c(1, cumprod( px[(idade+1):(idade+n-1)]) )
  
  ax <- (b* sum(v*pxx))
  return(ax) 
}


Dotal_Puro <- function(i, idade, n, b, qx){
  px <- 1-qx
  v <- 1/(i+1)
  Ax <-  b*(v^n)*cumprod(px[(idade+1):(idade+n)])[n]
  return(Ax)
}


DiferidoTemp<- function(PROD=Anuid, i, idade, n=5, b, qx, m){
  Dx<-(PROD(i, idade+m, n , 1, qx))*(Dotal_Puro(i, idade, m, 1, qx))
  Dx <- b*(Dx - 11/24)
  return(Dx) #Corrigir variancia
}

Diferido<- function(PROD=Anuidvit, i, idade, b, qx, m){
  Dx<-(PROD(i, idade+m, 1, qx))*(Dotal_Puro(i, idade, m, 1, qx)) 
  Dx <- b*(Dx - 11/24) #Fracionando
  #Dx<-(PROD(i, idade+m, b, qx))*(1/(1+i))^m
  return(Dx) #Corrigir variancia
}

#Anuidade financeira
anuidFin <- function(i, m){
  v <- (1/(1+i))^(0:(m-1))
  return(sum(v))
}




# Produtos personalizados -------------------------------------------------



aposentadoria <- function(i=0.055, idade=20, b=2000, qx, m=40){
  #b=b*12
  #r=Diferido(i=i, idade=idade, b=b, qx=qx, m=m)/Anuid(i, idade, m, 1, qx)/12
  #return(r)
  return(Diferido(i=i, idade=idade, b=b, qx=qx, m=m)/AnuidF(i, idade, m, qx))
}

casapropria <- function(i=0.04, idade=20, vlrCasa=200000, qx, m=20){
  b = vlrCasa*0.3
  v=1/(1+i)
  #return((b*(v^n))/AnuidF(i, m, n, qx))
  return((b*(v^n))/Anuid(i, m, n, 1, qx)  * 11/24)
}

universidade <- function(i=0.07, idade=0, b=2000, qx, m=20){
  #b=b*12
  #r = DiferidoTemp(i=i, idade=idade, b=b, qx=qx, m=m)/Anuid(i, idade, m, 1, qx)/12
  #return(r)
  return(DiferidoTemp(i=i, idade=idade, b=b, qx=qx, m=m)/AnuidF(i, idade, m, qx))
}


# Calculo -----------------------------------------------------------------

#idade: Idade do beneficiário
#b : Beneficio
#m: período de contribuição

#opt: Qual a função a ser chamada
# 1: Aposentadoria
# 2: Casa própria
# 3: Universidade

prevCall <-function(idade=20, b=2000, m=40, opt=1){
  tabuas = tabua
  qx = tabuas$AT.2000_MALE #AT2000_Suavizada_10_MAS
  
  switch(opt,
         '1'={
           premio <- aposentadoria(idade=idade, b=b, m=m, qx=qx)
         },
         '2'={
           premio <- casapropria(idade=idade, vlrCasa = b, m=m, qx=qx)
         },
         '3'={
           premio <- universidade(idade=idade, b=b, m=m, qx=qx)
         })
  
  return(premio)
}

