#######################
## Theme pro ggplot2 ##
#######################

themeHyper <- function(angle=90, fonte="calibri") {
  
  theme(panel.grid.major = element_blank()) +
    theme(panel.background = element_blank())+
    theme(legend.title = element_text(family=fonte, size = 16, face = "bold")) +
    theme(legend.text = element_text(family=fonte, size = 12, face = "bold")) +
    theme(plot.title = element_text(family=fonte, size = 16, face = "bold")) +
    theme(axis.text.x = element_text(family=fonte, size = 16, face = "bold",  angle = angle, vjust = .5)) +
    theme(axis.text.y= element_text(family=fonte, size = 16, face = "bold")) +
    theme(axis.title.x= element_text(family=fonte, size = 16, face = "bold")) +
    theme(axis.title.y= element_text(family=fonte, size = 16, face = "bold"))
  
}

theme_hyper <- function(angle=0, fonte="arial", size=14) {
  
  theme(panel.grid.major = element_blank()) +
    theme(panel.background = element_blank())+
    theme (panel.grid.minor = element_blank()) +
    theme(legend.title = element_text(family=fonte, size = size, face = "bold")) +
    theme(legend.text = element_text(family=fonte, size = size, face = "bold")) +
    theme(plot.title = element_text(family=fonte, size = size, face = "bold")) +
    theme(axis.text.x = element_text(family=fonte, size = size, face = "bold", angle = angle, vjust = .5)) +
    theme(axis.text.y= element_text(family=fonte, size = size, face = "bold", hjust=.5)) +
    theme(axis.title.x= element_text(family=fonte, size = size, face = "bold")) +
    theme(axis.title.y= element_text(family=fonte, size = size, face = "bold"))
  
}

#########
# Brasil
#########

remove_acento <- function(vec, Toupper=F) {
  vec <- tolower(vec)
  vec <- gsub('aã', 'o', vec)
  vec <- gsub('ã”', 'o', vec)
  vec <- gsub('ãƒ', 'a', vec)
  vec <- gsub('á', 'a', vec)
  vec <- gsub('Ã', 'a', vec)
  vec <- gsub('ã', 'a', vec)
  vec <- gsub('â', 'a', vec)
  vec <- gsub('é', 'e', vec) 
  vec <- gsub('ê', 'e', vec)
  vec <- gsub('í', 'i', vec)
  vec <- gsub('ó', 'o', vec) 
  vec <- gsub('ô', 'o', vec)
  vec <- gsub('õ', 'o', vec)
  vec <- gsub('ú', 'u', vec)
  vec <- gsub('ç', 'c', vec)
  if ( Toupper==T) vec <- toupper(vec)
  return(vec)
}

converte_uf <- function (df, uf, para_sigla=T) {
  
  UF_extenso = c('Distrito Federal',
                 'Minas Gerais','Rio de Janeiro','São Paulo', 'Rio Grande do Norte',
                 'Rio Grande do Sul','Mato Grosso do Sul','Mato Grosso', 'Paraná', 'Santa Catarina','Roraima', 
                 'Amapá', 'Paraíba','Alagoas','Sergipe','Bahia','Pernambuco','Ceará','Pará',
                 'Maranhão','Amazonas','Acre','Rondônia','Tocantins','Goiás','Piauí','Espírito Santo')
  
  UF_sigla = c('DF','MG', 'RJ', 'SP','RN', 'RS', "MS",
               "MT", 'PR', 'SC','RR', 'AP', 'PB','AL', 'SE', 'BA',
               'PE','CE', 'PA', 'MA','AM', 'AC', 'RO', 'TO', 
               'GO', 'PI','ES')
  
  mat = matrix(c(UF_extenso, UF_sigla), nrow=27)
  mat <- remove_acento(mat, Toupper=T)
  
  df[,uf] <- toupper(remove_acento(df[,uf]))
  
  if (para_sigla==T) {
    for ( i in 1:nrow(mat)) {
      df[, uf][which(df[,uf]==mat[i,1])] <- mat[i,2]
    }
  } else {
    for ( i in 1:nrow(mat)) {
      df[, uf][which(df[,uf]==mat[i,2])] <- mat[i,1]
    }
  }
  return(df)
}

########
# Cores
########

visa_cores <- function(cor){
  if(cor == 'azul100') cor <- '#0023a0'
  if(cor == 'azul60') cor <- '#667bc6'
  if(cor == 'azul20') cor <- '#ccd3ec'
  if(cor == 'laranja100') cor <- '#f9a635'
  if(cor == 'laranja60') cor <- '#fec666'
  if(cor == 'laranja20') cor <- '#feedd7'
  if(cor == 'cinza100') cor <- '#bebec0'
  if(cor == 'cinza60') cor <- '#d8d8d9'
  if(cor == 'cinza20') cor <- '#f2f2f2'
  return(cor)
}


#Fazer fun??o para cores da Hyper

#hyper_cores <- function(cor){
#  if(cor == 'azul') cor <- '#0000FF'
#  return(cor)
#}

########
# Excel
########

cria_xlsx <- function (ldf, arquivo) {
  if (require(xlsx) == F) {install.packages('xlsx'); require('xlsx') }
  wb <- createWorkbook()
  lsheets <- as.list(1:length(ldf))
  for (i in 1:length(ldf)) {
    lsheets [[i]] <- createSheet(wb, sheetName=names(ldf)[[i]])
    addDataFrame(ldf[[i]], lsheets[[i]], row.names=F)
  }
  saveWorkbook(wb, paste(getwd(), "/", arquivo, ".xlsx", sep=""))
}

  
  

print.functions <- function (){
  print(c('remove_acento', 'converte_uf', 'visa_cores', 'cria_xlsx'))
}
  



########################################################
### Fun??es auxiliares para fazer o relat?rio Bancos ###
########################################################



CarregaBase <- function (servidor='192.168.1.22', tabela='[Relatorio].[Resultado].[7611_RelatorioPerfilBancovdV]', banco="'SANTANDER'") {
  
  if(require(RODBC)==F) {
    install.packages('RODBC')
    require(RODBC)
  } else {
    require(RODBC)
  }
  
  
  aux <- paste("driver={SQL Server};server=", servidor, "; uid='usrLoyalty', pwd='usrLoyalty', database='PromoSys_v2'; trusted_connection=False, readOnly=F", sep="")
  dbhandle <- odbcDriverConnect(aux)
  
  base <- sqlQuery(dbhandle, paste("select * from", tabela, "where [Banco] =",banco, sep=" "))
  return(base)
}
  



## fun??es


faixaIdade <- function (df, nomeVar="Idade") {
  # cria uma coluna de faixa et?ria a prtir de uma coluna de dados de idade num?rica
  dfAux <- df
  
  if (is.numeric(dfAux[, nomeVar])==F) {
    print(paste(nomeVar,"transformanda em num?rica"))
    dfAux[, nomeVar] <- as.numeric(as.character(dfAux[, nomeVar]))
  }
  
  FaixaEtaria <-  ifelse(is.na(dfAux[, nomeVar]), "N?o Informado",
                         ifelse(dfAux[, nomeVar] < 18, '< 18',
                                ifelse(dfAux[, nomeVar] < 25, 'de 18 a 24', 
                                       ifelse(dfAux[, nomeVar] < 35, 'de 25 a 34',
                                              ifelse(dfAux[, nomeVar] < 45, 'de 35 a 44', 
                                                     ifelse(dfAux[, nomeVar] < 55, 'de 45 a 54', 
                                                            ifelse(dfAux[, nomeVar] <= 70, 'de 55 a 70',
                                                                   ifelse( dfAux[, nomeVar] <= 110, 'Acima de 70',
                                                                           ifelse( dfAux[, nomeVar] > 110, 'N?o Informado', 'tem erro')))))))))
  return(FaixaEtaria)
}
  


# fun??o que computa qtde de clientes por faixas de UF, faixa et?ria, G?nero e Qtde Cart?es

PerfilDemo <- function (df, nomeVar) {
  # fun??o que computa qtde de clientes por faixas de UF, faixa et?ria, G?nero e Qtde Cart?es
  
  dfaux <- subset(df, Relevancia==1)
  
  aux <- data.frame(nomeVar=c(rownames(t(t(table(dfaux[,nomeVar]))))), Quantidade_de_Clientes=(t(t(table(dfaux[,nomeVar]))))[,1], 
                    Percentual=round((t(t(table(dfaux[,nomeVar])))[,1]/sum(t(t(table(dfaux[,nomeVar])))))*100, 2), row.names=NULL)
  aux[,1] <- as.character(aux[,1])
  aux[nrow(aux)+1,1] <- 'Total'
  aux[nrow(aux),2:3] <- c(sum(aux[,2], na.rm=T), 100)
  return(aux)
}
  


Perfil <- function (df, vecDemograficos, QtdeCardClient=T, ClientesProduto=T, CartoesProduto=T, Email=T) {
  
  # fun??o que chama PerfilDemo e coloca numa lista
  ldf <- list()
  aux <- paste('Idade', 'Faixa', sep="")
  df[,aux] <- NA
  df[,aux] <- faixaIdade (df, 'Idade')
  vecAux1 <-  c(vecDemograficos[vecDemograficos!="Idade"], aux)
  for ( i in 1:length(vecAux1)) {
    ldf[[vecAux1[i]]] <- PerfilDemo(df, vecAux1[i])   
  }
  
  if (QtdeCardClient==T ) {
    ldf$QtdeCardClientes <- CardClientes(df)
  }
  if (ClientesProduto==T ) {
    ldf$ClientesProduto <-  ClientesProduto(df) 
  }
  if (QtdeCardClient==T ) {
    ldf$CartoesProduto <-  CartoesProduto(df)
  }
  if (Email==T ) {
    ldf$Email <- PerfilEmail(df)
  }
  
  return(ldf)
  
}
  


#Calculando numero de cart?es por cliente - banco 'base'

CardClientes <- function(df ) {
  Cartoes <- data.frame(Numero_Cartoes=c('Um', 'Dois', 'Tr?s ou mais'),  
                        Quantidade_de_Clientes=c(sum(table(df$codCliente)==1), 
                                                 sum(table(df$codCliente)==2), sum(table(df$codCliente)>2)), row.names=NULL)
  
  Cartoes[,'Percentual'] <- round((Cartoes[,'Quantidade_de_Clientes']/sum(Cartoes[,'Quantidade_de_Clientes']))*100, 2)
  
  Cartoes[,1] <- as.character(Cartoes[,1])
  Cartoes[nrow(Cartoes)+1,1] <- 'Total'
  Cartoes[nrow(Cartoes),2:3] <- c(sum(Cartoes[,2], na.rm=T), 100)
  return(Cartoes)
}
  

## calculando qualidade de e-mail por cliente
PerfilEmail <- function (df) {
  
  df <- subset(df, Relevancia==1)
  
  Email <- data.frame(Qualidade_do_Email=c('Email Bom', 'Email Ruim', 'Total de clientes'),
                      Quantidade_de_Clientes=c(sum(as.numeric(df$QualidadeEmail)==1), 
                                               sum(as.numeric(df$QualidadeEmail) == 2), length(df$QualidadeEmail)), row.names=NULL)
  Email[1:2,'Percentual'] <- round((Email[,'Quantidade_de_Clientes'][1:2]/sum(Email[,'Quantidade_de_Clientes'][1:2]))*100, 2)
  Email[,'Percentual'][3] <- 100.00
  
  return(Email)
}
  


# Clientes por produto

ClientesProduto <- function (df) {
  if(require(plyr)==F) {
    install.packages('plyr')
  }
  Clientes_Produto <- ddply(df, "Produto", function (df1) length(unique(df1$codCliente)))
  Clientes_Produto[,3] <- round(Clientes_Produto[,2]/sum(Clientes_Produto[,2])*100,2)
  colnames(Clientes_Produto) <- c("Produto", "Quantidade_de_Clientes", "Percentual")
  Clientes_Produto[,1] <- as.character(Clientes_Produto[,1])
  Clientes_Produto[nrow(Clientes_Produto)+1,1] <- 'Total de Clientes*'
  Clientes_Produto[nrow(Clientes_Produto),2:3] <- c(sum(Clientes_Produto[,2], na.rm=T), 100)
  Clientes_Produto[nrow(Clientes_Produto)+1,1] <- '*Clientes com mais de um tipo de cart?o foram contabilizados mais de uma vez.'
  ## Clientes_Produto[nrow(Clientes_Produto),2:3] <- c('','')
  return(Clientes_Produto)
}
  

# Cartoes por produto

CartoesProduto <- function (df) {
  
  Cartoes_Produto <- ddply(df, "Produto", function (df1) length(unique(df1$codCartao)))
  Cartoes_Produto[,3] <- round(Cartoes_Produto[,2]/sum(Cartoes_Produto[,2])*100,2)
  colnames(Cartoes_Produto) <- c("Produto", "Quantidade_de_Cartoes", "Percentual")
  Cartoes_Produto[,1] <- as.character(Cartoes_Produto[,1])
  Cartoes_Produto[nrow(Cartoes_Produto)+1,1] <- 'Total de Cartoes*'
  Cartoes_Produto[nrow(Cartoes_Produto),2:3] <- c(sum(Cartoes_Produto[,2], na.rm=T), 100)
  return(Cartoes_Produto)
}
  

# salva em excel
GeraExcelBancos <- function (ldf, nomeArquivoSalvamento, vecDemograficos, servidor='192.168.1.22', tabela='[Relatorio].[Resultado].[7611_RelatorioPerfilBancovdV]', banco="'CEF'", 
                        QtdeCardClient=T, ClientesProduto=T, CartoesProduto=T, Email=T) {
  df <- CarregaBase(servidor, tabela, banco)
  listaDF <- Perfil(df, vecDemograficos)
  cria_xlsx(listaDF, paste(Sys.Date(), nomeArquivoSalvamento, sep='_'))
  
}
  

