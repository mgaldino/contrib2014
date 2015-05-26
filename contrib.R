#######################################
#### Manoel Galdino 25/05/2015     ####
#### Webscraping dados candidatos  ####
#### TB                            ####
#######################################

## carregando bibliotecas
library(XML)
library(data.table)
library(ggplot2)
library(scales)

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


head(baseContribCandidatos)
head(baseCandidatos)
names(baseCandidatos)[1] <-  "UF"
base <- merge( baseCandidatos, baseContribCandidatos , by.x="CPF.do.candidato", by.y="CPF.do.candidato", all.x=T)

base <- subset(base, cargo %in% c("DEPUTADO ESTADUAL", "DEPUTADO FEDERAL", "SENADOR", "DEPUTADO DISTRITAL", 
                                "1º SUPLENTE", "2º SUPLENTE"))
base$idade <- as.numeric(base$idade)
base$receita[is.na(base$receita)] <- 0
base1 <- as.data.table(base)

base1[ , bolReceita := receita==0 ]
analiseCompleta <- base1[ , list(receiaMedia =mean(receita ), numSemReceita=sum(bolReceita), totalCandidatos = length(unique(CPF.do.candidato)), totalCandidatos1 = length(CPF.do.candidato)), by=c("genero", "cor", "status_eleito", "UF.x", "cargo")]

analiseGenero <- base1[ , list(receitaMedia =mean(receita ), totalCandidatos = length(CPF.do.candidato)), by=c("genero", "bolReceita")]
analiseGenero

analiseCor <- base1[ , list(receitaMedia =mean(receita ), totalCandidatos = length(CPF.do.candidato)), by=c("cor", "bolReceita")]
analiseCor

analiseGeneroCargo <- base1[ , list(receitaMedia =mean(receita ), totalCandidatos = length(CPF.do.candidato)), by=c("genero", "bolReceita","cargo")]
analiseGeneroCargo[order(analiseGeneroCargo$bolReceita, analiseGeneroCargo$cargo, analiseGeneroCargo$genero), ]


### Gráficos
setwd("D:/2015/Transparência/gráficos")

## barra genero x cargo entre os que receberam doação
baseCargos <- subset(analiseGeneroCargo, bolReceita==F)
baseCargos$cargo <- gsub("deputado ", "", tolower(baseCargos$cargo))
baseCargos$cargo <- gsub("federal", "fed", baseCargos$cargo)
baseCargos$cargo <- gsub("estadual", "est", baseCargos$cargo)
baseCargos$cargo <- gsub("distrital", "dist", baseCargos$cargo)
baseCargos$cargo <- gsub("senador", "sen", baseCargos$cargo)

baseCargos$genero <- gsub("MASCULINO", "Masc", baseCargos$genero)
baseCargos$genero <- gsub("FEMININO", "Fem", baseCargos$genero)
                          
dodgewidth <- position_dodge(width=0.9)
p <- ggplot(baseCargos, aes(genero, y=receitaMedia, fill=cargo) ) +
  geom_bar(stat = "identity", position=dodgewidth)  + scale_fill_grey(start = 0.1, end = .9) + 
  stat_identity(geom="text", position= dodgewidth, aes(x=genero, label=cargo), vjust=-1) +
  scale_y_continuous(labels = dollar, limits=c(0, 1500000)) +theme_tb() + ylab("Receita Média") + xlab("")
p
ggsave(p, file="receita_genero_cargo_barra_V1.pdf", scale=2)

p <- ggplot(baseCargos, aes(cargo, y=receitaMedia, fill=genero) ) +
  geom_bar(stat = "identity", position=dodgewidth)  + scale_fill_grey(start = 0.1, end = .9, name="") + 
  #stat_identity(geom="text", position= dodgewidth, aes(x=cargo, label=genero, size=12), vjust=-1) +
  scale_y_continuous(labels = dollar, limits=c(0, 1500000)) +theme_tb(legenda="right") + ylab("Receita Média") + xlab("")
p

ggsave(p, file="receita_genero_cargo_barra_V2.pdf", scale=2)

baseDepFed <- subset(base1, bolReceita==F &  Cargo=="Deputado Federal")

p <- ggplot(baseDepFed, aes(idade, y=receita, label = as.character(genero))) +
     geom_text(size = 3 , aes(colour=genero )) + scale_y_continuous(labels = dollar) +theme_tb()



ggsave(p, file="receita_idade_genero.pdf", scale=2)
