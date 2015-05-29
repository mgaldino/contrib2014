##
### Partidos
##

library(reshape2)
library(ggplot2)
library(data.table)

## Assembléia
partidos <- base1
setkey(partidos, status_eleito)

# total mulheres eleitas por partido na assembleia

partidosA <- subset(partidos, status_eleito %in% c("ELEITO POR MÉDIA", "ELEITO POR QP") & cargo %in% c("dist", "est"))
partidosCTodos <- subset(partidos, cargo %in% c("dist", "est"))

# candidaturas
mulherEleitaPartido <- partidosC[ , list(MulheresEleitas = sum(genero=="Fem")), by=Sigla..Partido]
TotCandiPartido <- partidosCTodos[ , list(totalCandidatos=.N), by=Sigla..Partido]
TotmulherCandiPartido <- partidosCTodos[ , list(totalMulherCandidatas=sum(genero=="Fem")), by=Sigla..Partido]
TotEleitosPartido <- partidosC[ , list(TotalEleitos=.N), by=Sigla..Partido]

setkey(mulherEleitaPartido, Sigla..Partido)
setkey(TotCandiPartido, Sigla..Partido)
setkey(TotCandiPartido, Sigla..Partido)
setkey(TotmulherCandiPartido, Sigla..Partido)

genPartido <- merge(mulherEleitaPartido, TotEleitosPartido)
genPartido <- merge(genPartido, TotCandiPartido)
genPartido <- merge(genPartido, TotmulherCandiPartido)

genPartido [ , percWeleitas := round(MulheresEleitas/TotalEleitos, 2)]
genPartido [ , percWcandidatas := round(totalMulherCandidatas/totalCandidatos, 2)]

genPartido1 <- subset(genPartido, select=c("Sigla..Partido", "percWeleitas", "percWcandidatas"))
meltData <- melt(genPartido1, id.vars="Sigla..Partido", value.name="perc")
### gráficos

dodgewidth <- position_dodge(width=0.9)

p <- ggplot(meltData, aes(x=Sigla..Partido, y=perc, fill=variable)) +
  geom_bar(stat = "identity", position=dodgewidth) + coord_flip() + scale_fill_grey(start = 0.1, end = .9, name="") + 
  scale_y_continuous(labels = percent, limits=c(0, .4)) +theme_tb(legenda="right") + ylab("% mulheres") + xlab("")
p

p1 <- ggplot(genPartido1, aes(x=percWcandidatas , y=percWeleitas, label=Sigla..Partido)) +
  geom_text() + scale_y_continuous(labels = percent, limits=c(0, .4)) +theme_tb() +
  scale_x_continuous(labels = percent, limits=c(0, .4)) + ylab("% mulheres eleitas") + xlab("% mulheres candidatas")
p1

with(genPartido1, cor(percWcandidatas, percWeleitas))


#### Financiamento
partidosA <- subset(partidos, status_eleito %in% c("ELEITO POR MÉDIA", "ELEITO POR QP") & cargo %in% c("dist", "est"))
partidosATodos <- subset(partidos, cargo %in% c("dist", "est"))

# eleitos
setkey(partidosA, genero)
mulherFinPartidoEleita <- partidosA[ "Fem" , list(receitaWElec = mean(receita)), by=Sigla..Partido]
totalFinPArtidoEleito <- partidosA[ , list(receitaTotalElec = mean(receita)), by=Sigla..Partido]

# todos
setkey(partidosATodos, genero)
TotFinPartido <- partidosATodos[ "Fem", list(receitaTodosW = mean(receita)), by=Sigla..Partido]
TotmulherPartido <- partidosATodos[ , list(receitaTodos = mean(receita)), by=Sigla..Partido]

setkey(mulherFinPartidoEleita, Sigla..Partido)
setkey(totalFinPArtidoEleito, Sigla..Partido)
setkey(TotFinPartido, Sigla..Partido)
setkey(TotmulherPartido, Sigla..Partido)

genPartidoFin <- merge(mulherFinPartidoEleita, totalFinPArtidoEleito)
genPartidoFin <- merge(genPartidoFin, TotFinPartido)
genPartidoFin <- merge(genPartidoFin, TotmulherPartido)

genPartidoFin [ , percFinWeleitas := round(receitaWElec/receitaTotalElec, 2)]
genPartidoFin [ , percFinWcandidatas := round(receitaTodosW/receitaTodos, 2)]


genPartidoFin1 <- subset(genPartidoFin, select=c("Sigla..Partido", "percFinWeleitas", "percFinWcandidatas"))
meltDataFin <- melt(genPartidoFin1, id.vars="Sigla..Partido", value.name="perc")
### gráficos

dodgewidth <- position_dodge(width=0.9)

p <- ggplot(meltDataFin, aes(x=Sigla..Partido, y=perc, fill=variable)) +
  geom_bar(stat = "identity", position=dodgewidth) + coord_flip() + scale_fill_grey(start = 0.1, end = .9, name="") + 
  scale_y_continuous(labels = percent, limits=c(0, 5)) +theme_tb(legenda="right") + ylab("% mulheres") + xlab("")
p

setwd("D:/2015/TB/mulheres/contrib2014/graficos")
ggsave(p, file="perc_mulheres_arrecadacao_assembleias.png", dpi=200, scale=2)

p1 <- ggplot(genPartidoFin1, aes(x=percFinWcandidatas , y=percFinWeleitas, label=Sigla..Partido)) +
  geom_text() + scale_y_continuous(labels = percent, limits=c(0, 5)) +theme_tb() +
  scale_x_continuous(labels = percent, limits=c(0, 1)) + ylab("% receita mulheres eleitas") + xlab("% receita mulheres candidatas")
p1

setwd("D:/2015/TB/mulheres/contrib2014/graficos")
ggsave(p1, file="scatter_perc_mulheres_arrecadacao_assembleias.png", dpi=200, scale=2)

with(genPartidoFin1, cor(percFinWcandidatas, percFinWeleitas))


# valid
ppl <- subset(base1, Sigla..Partido == "PPL")
head(ppl)
setkey(ppl, cargo)
ppl[c("est", "dist") , sum(receita), by=c("genero", "status_eleito")  ]
ppl[c("est", "dist") , .N, by=c("genero", "status_eleito")  ]

