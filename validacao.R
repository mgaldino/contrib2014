### Validação

names(baseCandidatos)

baseCandidatosPreto <- subset( baseCandidatos, cor=="PRETA")
head(baseCandidatosPreto)
table(baseCandidatosPreto$status_eleito)

head(subset(baseCandidatosPreto , status_eleito=="ELEITO POR QP"))

subset(baseContribCandidatos, CPF.do.candidato == "23325330282")

subset(base1, CPF.do.candidato == "23325330282")


subset( baseContribCandidatos, Nome.candidato=="MARIA DALVA ALVES DOURADO")
baseContribCandidatos[grepl("MARIA DALVA", baseContribCandidatos$Nome.candidato),]

sum(grepl("MARIA DALVA", dfList[[1]]$Nome.candidato))
sum(grepl("MARIA DALVA", contrib$Nome.candidato))

length(unique(contrib$Nome.candidato))
summary(baseContribCandidatos)
head(subset( baseContribCandidatos, receita<0.5))

## Validando info do mapa

mapaBrasil2 <- as.data.table(mapaBrasil1)
aux <- mapaBrasil2[ , list(maxLong=max(long), maxLat=max(lat.x), minLong=min(long), minLat=min(lat.x),
                    maxLongReceita=max(long), maxLatReceita=max(lat.y), receita=max(receitaMedia)),
            by=id]
aux

aux1 <- unique(mapaBrasil$id)
aux2 <- maScDepEst1$UF
aux1[3] ==aux2[24]
x <- aux1[3]
x <- gsub("A","A", x)
nchar(x)
nchar(aux1[3])
gsub("AMAPA", "AMAPA", aux1[3])
nchar(aux2[4])


head(baseContribCandidatos)
tmp <- subset(baseContribCandidatos, CPF.do.candidato == "85885177872")
tmp

x <- 0
for ( i in 1:28) {
  if ( x < length(dfList[[i]])) x <- i
}
x
length(dfList[[28]])      
dfList[[28]][[1]]

setwd("D:\\2015\\Transparência\\dados externos\\prestacao_final_2014\\receita_candidato")

sp <- read.table("receitas_candidatos_2014_SP.txt", header=T, sep=";", colClasses= "character") 
head(sp)
names(sp)
valid <- subset(sp, CPF.do.candidato == "85885177872")
valid1 <- subset( sp,  CPF.CNPJ.do.doador.originário == "85885177872")
valid2 <- subset( sp,  CPF.CNPJ.do.doador == "85885177872")

serra <- subset(sp, CPF.do.candidato == "93565968834")
serra <- subset(serra, select=c("Numero.Recibo.Eleitoral", "CPF.CNPJ.do.doador",
                                "Nome.do.doador", "Nome.do.doador..Receita.Federal.",
                                "Sigla.UE.doador", "Cod.setor.econômico.do.doador",              
                                "Setor.econômico.do.doador", "Data.da.receita",                            
                                "Valor.receita", "Tipo.receita", "Fonte.recurso",                              
                                "Especie.recurso", "Descricao.da.receita", "CPF.CNPJ.do.doador.originário",              
                                "Nome.do.doador.originário","Tipo.doador.originário"))

View(serra)

serraDT <- as.data.table(serra)
serraDT$Valor.receita <- as.numeric(gsub(",", ".", serraDT$Valor.receita))
serraDT[ , sum(Valor.receita), by=Nome.do.doador]

serra1 <- serra[grepl("IGUATEMI", serra$Nome.do.doador.originário),]
serra1$Valor.receita <- as.numeric(gsub(",", ".", serra1$Valor.receita))

View(serra1)
sum(serra1$Valor.receita)

setwd("D:\\2015\\Transparência\\dados externos\\prestacao_final_2014")

spComite <- read.table("receitas_comites_2014_SP.txt", header=T, sep=";", colClasses= "character") 

names(spComite)
colunas <- c("CNPJ.Prestador.Conta", "CPF.CNPJ.do.doador",
             "Nome.do.doador", "Nome.do.doador..Receita.Federal.",
             "Sigla.UE.doador", "Número.candidato.doador", 
             "Cod.setor.econômico.do.doador",              
             "Setor.econômico.do.doador", "Data.da.receita",                            
             "Valor.receita", "Tipo.receita", "Fonte.recurso",                              
             "Espécie.recurso", "Descrição.da.receita", "CPF.CNPJ.do.doador.originário",              
             "Nome.do.doador.originário",
             "Tipo.doador.originário")

match( colunas, names(spComite))
spComite1 <- subset(spComite, select=colunas)
dim(spComite1)
spComite$Valor.receita <- as.numeric(gsub(",", ".", spComite1$Valor.receita))
spComite2 <- spComite1[grepl("IGUATEMI", spComite1$Nome.do.doador.originário),]

spComite2 <- spComite1[grepl("IGUATEMI", spComite1$Nome.do.doador..Receita.Federal.),]

View(spComite2)
Nome.do.doador..Receita.Federal.
dim(spComite2)

tmp <- subset(spComite, CPF.CNPJ.do.doador== "51218147000193")
View(tmp)

tmp1 <- sp[grepl("7017796", sp$Numero.do.documento), ]
dim(tmp1)

despComite <- read.table("despesas_comites_2014_SP.txt", header=T, sep=";", colClasses= "character") 
names(despComite)

tmp1 <- despComite[grepl("7017796", despComite$Numero.do.documento), ]
dim(tmp1)
View(despComite)

investig <- despComite[grepl("ALCKMIN", despComite$Nome.do.fornecedor), ]
names(investig)
investig$CPF.CNPJ.do.fornecedor
View(investig)

iguatemi <- sp[grepl("IGUATEMI", sp$Nome.do.doador.originário),]
unique(iguatemi$Nome.do.doador.originário)
unique(iguatemi$CPF.CNPJ.do.doador.originário)

dim(iguatemi)
iguatemi$Valor.receita <- as.numeric(gsub(",", ".", iguatemi$Valor.receita))
sum(iguatemi$Valor.receita)
View(iguatemi)
sum(serra1$Valor.receita)

iguatemi <- as.data.table(iguatemi)
aux <- iguatemi [ , sum(Valor.receita), by=c("Nome.do.doador", "Numero.candidato")]
aux
View(aux)

iguatemi [ , sum(Valor.receita), by=c("Nome.do.doador")]

sum(iguatemi$Valor.receita)


## Despesas candidatos SP

despesas_candidatos_2014_SP

setwd("D:\\2015\\Transparência\\dados externos\\prestacao_final_2014")

spDesp <- read.table("despesas_candidatos_2014_SP.txt", header=T, sep=";", colClasses= "character") 
dim(spDesp)
head(sp)
names(spDesp)

serraD <- subset(spDesp, CPF.do.candidato == "93565968834")


View(serraD)

serraD <- as.data.table(serraD)
serraD$Valor.despesa <- as.numeric(gsub(",", ".", serraD$Valor.despesa))
serraD[ , sum(Valor.despesa)]

serra1 <- serra[grepl("IGUATEMI", serra$Nome.do.doador.originário),]
serra1$Valor.receita <- as.numeric(gsub(",", ".", serra1$Valor.receita))

View(serra1)
sum(serra1$Valor.receita)