## Estados

library(ggplot2)
library(ggmap)
library(reshape2)
library(maps)
library(Cairo)
library(rgdal)

library(maptools)
library(gpclib)

gpclibPermit()

setwd("D:\\2015\\TB\\mulheres\\contrib2014\\dados gerados")
load(file="lonlat_v1.RData")

base1 <- subset(base1, cargo %in% c("est", "dist"))
numcand <- base1[ , list(numCandidatos =length(CPF.do.candidato)) , by=c("sigla", "genero")]
setkey(numcand, sigla)
head(numcand)
totalUF <- numcand[ , list(totalCand=sum(numCandidatos)), by=sigla]
setkey(totalUF, sigla)
baseUF <- merge(numcand, totalUF)

baseUF[ , percCandidatos := numCandidatos/totalCand ]
baseUF

# baseUF[ , lon := lonlat[,2]] ## se rodar de novo, trocar...
# baseUF[ , lat := lonlat[,1]]

setnames(baseUF, "sigla", "UF")


baseUF <-  converte_uf(as.data.frame(baseUF), "UF", para_sigla=F)
baseUF1 <- subset(baseUF, genero=="Fem")

setwd("D:\\2015\\TB\\mulheres\\contrib2014\\dados externos")
shapeBr <- readOGR (dsn=".", layer="UFEBRASIL", encoding="utf-8")
ogrInfo(".", "UFEBRASIL")
gpclibPermit()
mapaBrasil <- fortify(shapeBr, region= "NM_ESTADO")

mapaBrasil$id <- remove_acento(mapaBrasil$id, Toupper=T )
# save(mapaBrasil, file="mapaBrasil.RData")
## mapaBrasil preparado

match(unique(mapaBrasil$id), baseUF1$UF)
# merge 
mapaBrasil1 <- merge(mapaBrasil, baseUF1, by.x = "id", by.y="UF", all.x=T)
head(mapaBrasil1)


# necessary to reorder. If not, map will not be plotted corrected
mapaBrasil1 <- mapaBrasil1[order(mapaBrasil1$order),]

## Masculino
# to have visible borders)
m0 <- ggplot(data=mapaBrasil1, aes(x=long, y=lat, group=group))
m1 <- m0 + geom_polygon(aes(fill=percCandidatos)) + coord_equal()
m2 <- m1 + geom_path(aes(group=group), color='grey', size=.1)
m3 <- m2 + theme_tb(legenda="right", dir.Legenda="vertical") + xlab("") + ylab("")
m4 <- m3 +  theme(axis.text.x = element_blank(), axis.text.y = element_blank())

m5 <- m4 + scale_fill_continuous(low="yellow", high="red", breaks = pretty_breaks(n = 10), 
                           labels=percent)
m6 <- m5+  guides(fill = guide_legend( title = "% mulheres candidatas \n nas assembléias", reverse = TRUE, override.aes = 
                                         list(alpha = 1)))
m6

setwd("D:/2015/Transparência/gráficos")
ggsave(m6, file="perc_mulheres_candidatas_assembelais_estaduais.png", dpi=200, scale=2)

## eleitos
base1 <- subset(base1, cargo %in% c("est", "dist") & status_eleito %in% c("ELEITO POR QP","ELEITO POR MÉDIA"))
numcand <- base1[ , list(numCandidatos =length(CPF.do.candidato)) , by=c("sigla", "genero")]
setkey(numcand, sigla)
head(numcand)
totalUF <- numcand[ , list(totalCand=sum(numCandidatos)), by=sigla]
setkey(totalUF, sigla)
baseUF <- merge(numcand, totalUF)

baseUF[ , percCandidatos := numCandidatos/totalCand ]
baseUF

# baseUF[ , lon := lonlat[,2]] ## se rodar de novo, trocar...
# baseUF[ , lat := lonlat[,1]]

setnames(baseUF, "sigla", "UF")


baseUF <-  converte_uf(as.data.frame(baseUF), "UF", para_sigla=F)
baseUF1 <- subset(baseUF, genero=="Fem")

mapaBrasil1 <- merge(mapaBrasil, baseUF1, by.x = "id", by.y="UF", all.x=T)


# necessary to reorder. If not, map will not be plotted corrected
mapaBrasil1 <- mapaBrasil1[order(mapaBrasil1$order),]

## Masculino
# to have visible borders)
m0 <- ggplot(data=mapaBrasil1, aes(x=long, y=lat, group=group))
m1 <- m0 + geom_polygon(aes(fill=percCandidatos)) + coord_equal()
m2 <- m1 + geom_path(aes(group=group), color='grey', size=.1)
m3 <- m2 + theme_tb(legenda="right", dir.Legenda="vertical") + xlab("") + ylab("")
m4 <- m3 +  theme(axis.text.x = element_blank(), axis.text.y = element_blank())

m5 <- m4 + scale_fill_continuous(low="yellow", high="red", breaks = pretty_breaks(n =5), 
                                 labels=percent)
m6 <- m5+  guides(fill = guide_legend( title = "% mulheres eleitas \n nas assembléias", reverse = TRUE, override.aes = 
                                         list(alpha = 1)))
m6

setwd("D:/2015/Transparência/gráficos")
ggsave(m6, file="perc_mulheres_eleitas_assembleias_estaduais.png", dpi=200, scale=2)

assem2010eleito <- c(.29, .25, .23, .19, .17, .17, .17, .17, .17, .17,.17, .15, .13, .13, .13, .13,
                     .11, .10, .08,  .08, .08, .08, .08, .07, .07, .05, .05 )
uf2010 <- c("AP", "SE", "PI", "RJ", "BA", "PA", "DF", "MA", "PB", "TO", "AC", "RS", "ES", "CE",
            "RN", "RO", "SP", "SC", "AM", "MT", "MS", "RR", "PE", "AL", "PR", "MG", "GO")
df2010 <- data.frame(uf2010, assem2010eleito)
df2010 <- converte_uf(df2010, "uf2010", para_sigla=F)
compAnos <- merge(df2010, baseUF1, by.x= "uf2010", by.y="UF")
cor(compAnos$assem2010eleito, compAnos$percCandidatos)

compAnos$sigla <- compAnos$uf2010
compAnos <- converte_uf(compAnos, "sigla")

p <- ggplot(compAnos, aes(x=assem2010eleito, y=percCandidatos, label=sigla)) +geom_text(size=3, position = "jitter") +
  theme_tb() + xlab("% mulheres em 2010") + ylab("% mulheres em 2014") +
  scale_x_continuous(breaks = pretty_breaks(n=4), labels=percent, limits=c(0, .4)) +
  scale_y_continuous(breaks = pretty_breaks(n=4), labels=percent, limits=c(0, .4))

p

setwd("D:/2015/TB/mulheres/contrib2014/graficos")
ggsave(p, file="perc_mulheres_eleitas_assembleias_estaduais_2010_14_scatter.png", dpi=200, scale=1)

### Câmara e Senado
## eleitos

# Camara
base2 <- subset(base1, cargo=="fed" & status_eleito %in% c("ELEITO POR QP","ELEITO POR MÉDIA"))
base2[ , length()]
numcand <- base2[ , list(numCandidatos =length(CPF.do.candidato)) , by=c("sigla", "genero")]
setkey(numcand, sigla)
head(numcand)
totalUF <- numcand[ , list(totalCand=sum(numCandidatos)), by=sigla]
setkey(totalUF, sigla)
baseUF <- merge(numcand, totalUF)

baseUF[ , percCandidatos := numCandidatos/totalCand ]
baseUF

setnames(baseUF, "sigla", "UF")
baseUF <-  converte_uf(as.data.frame(baseUF), "UF", para_sigla=F)
baseUF1 <- subset(baseUF, genero=="Fem")

match(unique(mapaBrasil$id),baseUF1$UF )

mapaBrasil1C <- merge(mapaBrasil, baseUF1, by.x = "id", by.y="UF", all.x=T)



# necessary to reorder. If not, map will not be plotted corrected
mapaBrasil1C <- mapaBrasil1C[order(mapaBrasil1C$order),]

mapaBrasil1C$percCandidatos[is.na(mapaBrasil1C$percCandidatos)] <- 0


## Masculino
# to have visible borders)
m0 <- ggplot(data=mapaBrasil1C, aes(x=long, y=lat, group=group))
m1 <- m0 + geom_polygon(aes(fill=percCandidatos)) + coord_equal()
m2 <- m1 + geom_path(aes(group=group), color='grey', size=.1)
m3 <- m2 + theme_tb(legenda="right", dir.Legenda="vertical") + xlab("") + ylab("")
m4 <- m3 +  theme(axis.text.x = element_blank(), axis.text.y = element_blank())

m5 <- m4 + scale_fill_continuous(low="yellow", high="red", breaks = pretty_breaks(n =5), 
                                 labels=percent)
m6 <- m5+  guides(fill = guide_legend( title = "% mulheres eleitas \n na câmara", reverse = TRUE, override.aes = 
                                         list(alpha = 1)))
m6

setwd("D:/2015/TB/mulheres/contrib2014/graficos")
ggsave(m6, file="perc_mulheres_eleitas_dep_federal.png", dpi=200, scale=2)


## Senado
base2 <- subset(base1, cargo=="sen")
setkey(base2, status_eleito)
# total mulheres eleitas
round(base2["ELEITO", sum(genero=="Fem")]/27, 2)

# perc mulheres candidatas
round(base2[, sum(genero=="Fem")] / nrow(base2), 2)


## Câmara Federal
base2 <- subset(base1, cargo=="fed")
setkey(base2, status_eleito)
# total mulheres eleitas

aux <- subset(base2, status_eleito %in% c("ELEITO POR MÉDIA", "ELEITO POR QP"))
round(aux[ , sum(genero=="Fem")]/513, 2)

# perc mulheres candidatas
round(base2[, sum(genero=="Fem")] / nrow(base2), 2)


## assembléias estaduais
base2 <- subset(base1, cargo %in% c("est", "dist"))
setkey(base2, status_eleito)
# total mulheres eleitas

aux <- subset(base2, status_eleito %in% c("ELEITO POR MÉDIA", "ELEITO POR QP"))
round(aux[ , sum(genero=="Fem")]/nrow(aux), 2)

# perc mulheres candidatas
round(base2[, sum(genero=="Fem")] / nrow(base2), 2)


