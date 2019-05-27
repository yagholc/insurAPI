library(readr)
# Load data


# Produtos ----------------------------------------------------------------

segCall<-function(cobertura="MN", bp=50, beninput=F, idade=23){ #preparar para receber varias funcoes e UM premio para o total
  
  dados<-tabuas
  #dados <- read.csv('data/tabuas.csv', h=T)
  multi<-2.5 #definindo um pádrao para mulrtip´liocador para aproximar carregamentos e comissao
  switch(cobertura,
         MN={
           qx<-dados$AT.2000_MALE
           multi<-multi*0.55/12
         },
         MA={
           qx<-dados$AT.2000_MALE
           multi<-multi*0.45/12
         },
         DG={
           qx<-dados$GAM.71_MALE
           multi<-multi*2.2/12
         },
         IP={
           qx<-dados$INV_PERM
           multi<-multi*0.3/12
         },
         IF={
           qx<-dados$INV_PERM
           multi<-multi*0.2/12
         })
  #b<-bp
  #Ax<-bp
  #n<-1
  #i<-0.03
  #px  <- 1-qx 
  #v   <- 1/(i+1)^(1:n)
  #qxx <- c(qx[(idade+1):(idade+n)])
  #pxx <- c(1, cumprod( px[(idade+1):(idade+n-1)]) )
  #if (beninput)
  #  return(b * sum(v*pxx*qxx)*multi)
  #return(Ax / sum(v*pxx*qxx)*multi)
  V_Temp(0.03, idade, 1, bp, qx, bp, beninput, multi)
}

SV_Temp <- function(i, idade, n, b, qx, Ax=1, beninput=T, multi) {  #Função única para as duas operações
  px  <- 1-qx #beninput , o input a ser usado para calculo é o beneficio?
  v   <- 1/(i+1)^(1:n)
  qxx <- c(qx[(idade+1):(idade+n)])
  pxx <- c(1, cumprod( px[(idade+1):(idade+n-1)]) )
  if (beninput)
    return(b * sum(v*pxx*qxx)*multi)
  return(Ax / sum(v*pxx*qxx)*multi)
}
