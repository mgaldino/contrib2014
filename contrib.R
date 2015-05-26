#######################################
#### Manoel Galdino 25/05/2015     ####
#### Webscraping dados candidatos  ####
#### TB                            ####
#######################################

## carregando bibliotecas
library(XML)
library(data.table)

# definindo diretorio
setwd("D:\\2015\\Transparência\\dados externos\\prestacao_final_2014\\receita_candidato")

# pegando arquivos do computador
nomeFiles <- list.files()
dfList <- list()
dfList1 <- list()
for (i in 1:length(nomeFiles)) {
  dfList[[i]] <- read.table(nomeFiles[i], header=T, sep=";", colClasses= "character") ## le o arquivo no r como data.frame
  dfList[[i]]$Valor.receita <- as.numeric(gsub(",", ".", dfList[[i]]$Valor.receita)) ## converte receita pra numérico
  dfList1[[i]] <- as.data.table(dfList[[i]]) ## transforma em ata.table
  dfList1[[i]] <- dfList1[[i]][ , list(receita = sum(Valor.receita)),
                   by=c("UF", "CPF.do.candidato", "Nome.candidato", "Cargo", "Sigla..Partido") ] # agrupa por deputado, smando receitas
  
}

baseContribCandidatos <- do.call(rbind, dfList)

head(baseContribCandidatos)
baseContribCandidatos <- subset( baseContribCandidatos, Cargo != c("Governador", "Presidente"))

# contrib$Valor.receita <- as.numeric(gsub(",", ".", contrib$Valor.receita))
# teste <- contrib[ , list(receita = sum(Valor.receita)),
#                  by=c("UF", "CPF.do.candidato", "Nome.candidato", "Cargo", "Sigla..Partido") ]

## agora importar informações dos candidatos

subset( baseContribCandidatos, Nome.candidato=="MARIA DALVA ALVES DOURADO")
baseContribCandidatos[grepl("MARIA DALVA", baseContribCandidatos$Nome.candidato),]

sum(grepl("MARIA DALVA", dfList[[1]]$Nome.candidato))
sum(grepl("MARIA DALVA", contrib$Nome.candidato))

length(unique(contrib$Nome.candidato))
summary(baseContribCandidatos)
head(subset( baseContribCandidatos, receita<0.5))



## mudandocaminho
setwd("D:\\2015\\Transparência\\dados externos\\consulta_cand_2014")

nomeColuna <- c("data", "hora", "ano", "geral1", "tipo", "sigla", "sigla1", "uf", "v9", "cargo", "nome",
                "geral2", "geral3", "CPF.do.candidato", "apelido", "geral5", "status_candidato", "numero", "sigla_partido",
                "nome_partido", "geral6", "v22", "coligacao", "nome_coligcao", "v25", "ocupacao", "nascimento", 
                "v28", "idade", "v30", "genero", "v32", "escolaridade", "v34", "status_civil", "v36", "cor", "v38", "nacionalidade",
                "estado", "v41", "cidade", "v43", "v44", "status_eleito")


# pegando arquivos do computador
nomeFiles1 <- list.files( pattern="consulta")
lista <- list()
for (i in 1:length(nomeFiles1)) {
  lista[[i]] <- read.table(nomeFiles1[i], header=F, sep=";", colClasses= "character") ## le o arquivo no r como data.frame
  names(lista[[i]]) <- nomeColuna ## coloca nome nas variáveis
  lista[[i]] <- subset(lista[[i]], select= c("sigla", "cargo", "nome", "CPF.do.candidato", 
                                             "apelido", "status_candidato", "sigla_partido", 
                                             "coligacao", "ocupacao", "idade", "genero", "escolaridade",
                                             "status_civil", "cor", "status_eleito"))
}

baseCandidatos <- do.call(rbind, lista)
baseCandidatos <- subset(baseCandidatos, cargo != c("GOVERNADOR", "PRESIDENTE", "VICE-GOVERNADOR", "VICE-PRESIDENTE" ))
baseContribCandidatos <- subset( baseContribCandidatos, Cargo != c("Governador", "Presidente"))

head(baseContribCandidatos)
head(baseCandidatos)
names(baseCandidatos)[1] <-  "UF"
base <- merge( baseCandidatos, baseContribCandidatos , by.x="CPF.do.candidato", by.y="CPF.do.candidato", all.x=T)

base$receita[is.na(base$receita)] <- 0
base1 <- as.data.table(base)

base1[ , bolReceita := receita==0 ]
analiseCompleta <- base1[ , list(receiaMedia =mean(receita ), numSemReceita=sum(bolreceita), totalCandidatos = length(unique(CPF.do.candidato)), totalCandidatos1 = length(CPF.do.candidato)), by=c("genero", "cor", "status_eleito", "UF.x", "cargo")]

analiseGenero <- base1[ , list(receitaMedia =mean(receita ), totalCandidatos = length(CPF.do.candidato)), by=c("genero", "bolReceita")]
analiseGenero

analiseCor <- base1[ , list(receitaMedia =mean(receita ), totalCandidatos = length(CPF.do.candidato)), by=c("cor", "bolReceita")]
analiseCor

analiseGeneroCargo <- base1[ , list(receitaMedia =mean(receita ), totalCandidatos = length(CPF.do.candidato)), by=c("genero", "bolReceita","cargo")]
analiseGeneroCargo[order(analiseGeneroCargo$bolReceita, analiseGeneroCargo$cargo, analiseGeneroCargo$genero), ]


