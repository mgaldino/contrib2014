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


numcand <- base1[ , list(numCandidatos =length(CPF.do.candidato)) , by=c("sigla", "genero")]
setkey(numcand, sigla)
head(numcand)
totalUF <- numcand[ , list(totalCand=sum(numCandidatos)), by=sigla]
setkey(totalUF, sigla)
baseUF <- merge(numcand, totalUF)

baseUF[ , percCandidatos := numCandidatos/totalCand ]
baseUF

baseUF[ , lon := lonlat[,2]] ## se rodar de novo, trocar...
baseUF[ , lat := lonlat[,1]]

setnames(baseUF, "sigla", "UF")

setwd("D:\\2015\\TB\\mulheres\\contrib2014\\dados externos")
shapeBr <- readOGR (dsn=".", layer="UFEBRASIL", encoding="utf-8")
ogrInfo(".", "UFEBRASIL")
gpclibPermit()
mapaBrasil <- fortify(shapeBr, region= "NM_ESTADO")

mapaBrasil$id <- remove_acento(mapaBrasil$id, Toupper=T )

## mapaBrasil preparado

# merge masc
mapaBrasil1 <- merge(mapaBrasil, maScDepEst1, by.x = "id", by.y="UF", all.x=T)

# merge fem
mapaBrasil2 <- merge(mapaBrasil, femDepEst1, by.x = "id", by.y="UF", all.x=T)


# necessary to reorder. If not, map will not be plotted corrected
mapaBrasil1 <- mapaBrasil1[order(mapaBrasil1$order),]
mapaBrasil2 <- mapaBrasil2[order(mapaBrasil2$order),]

## Masculino
# to have visible borders)
m0 <- ggplot(data=mapaBrasil1, aes(x=long, y=lat.x, group=group))
m1 <- m0 + geom_polygon(aes(fill=receitaMedia)) + coord_equal()
m2 <- m1 + geom_path(aes(x=long, y=lat.x, group=group), color='grey', size=.1)
m3 <- m2 + theme_tb(legenda="right", dir.Legenda="vertical") + xlab("") + ylab("")
m4 <- m3 +  theme(axis.text.x = element_blank(), axis.text.y = element_blank())
m5 <- m4 + scale_fill_continuous(low="#56B1F7", high="#132B43", labels=dollar)
m5

setwd("D:/2015/Transparência/gráficos")
ggsave(m5, file="mapa_receita_media_masc_dep_est_e_dist_comreceita.pdf",  scale=2)


baseUF1 <-  converte_uf(as.data.frame(baseUF), "UF", para_sigla=F)
