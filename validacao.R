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