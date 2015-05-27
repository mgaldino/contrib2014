## Mapa

## loading libraries
## pra usargpclib, acessei
## http://tlocoh.r-forge.r-project.org/manual_install.html
if (!require(gpclib)) install.packages("gpclib", type="source")


library(ggplot2)
library(ggmap)
library(reshape2)
library(maps)
library(Cairo)
library(rgdal)
gpclibPermit()
library(maptools)
library(gpclib)




## carregando dados
setwd()
load()


lonlat <- matrix(NA, nrow=27, ncol=2)
for(i in 1:nrow(maScDepEst)){
  lonlat[i,] <- unlist(geocode(paste(maScDepEst$UF[i], ", Brazil", sep=""), output = "latlon"))
}

save(lonlat, file="lonlat_v1.RData")


analiseGeneroCargoUf <- base1[ , list(receitaMedia =mean(receita ), totalCandidatos = length(CPF.do.candidato)), by=c("genero", "bolReceita","cargo","sigla")]
analiseGeneroCargoUf <- subset(analiseGeneroCargoUf, bolReceita=F)
names(analiseGeneroCargoUf)[4] <- "UF"

maScDepEst <- subset(analiseGeneroCargoUf, genero=="MASCULINO" & cargo %in% c("DEPUTADO ESTADUAL", "DEPUTADO DISTRITAL") & bolReceita==F )
maScDepEst <- subset( maScDepEst, select=c("UF", "receitaMedia"))

maScDepEst1 <-  converte_uf(as.data.frame(maScDepEst), "UF", para_sigla=F)
head(maScDepEst1)

femDepEst <- subset(analiseGeneroCargoUf, genero=="FEMININO" & cargo %in% c("DEPUTADO ESTADUAL", "DEPUTADO DISTRITAL") & bolReceita==F )
femDepEst <- subset( femDepEst, select=c("UF", "receitaMedia"))



femDepEst[ , lon := lonlat[,2]] ## se rodar de novo, trocar...
femDepEst[ , lat := lonlat[,1]]

femDepEst1 <-  converte_uf(as.data.frame(femDepEst), "UF", para_sigla=F)
head(femDepEst1)

### choropleth
## carregando mapa brasil com shapefile
setwd("D:\\2015\\TB\\mulheres\\contrib2014\\dados externos")
shapeBr <- readOGR (dsn=".", layer="UFEBRASIL", encoding="utf-8")
ogrInfo(".", "UFEBRASIL")
names(shapeBr)
shapeBr@data
gpclibPermit()
mapaBrasil <- fortify(shapeBr, region= "NM_ESTADO")
head(mapaBrasil)
unique(mapaBrasil$id)



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

# Feminino
m0 <- ggplot(data=mapaBrasil2, aes(x=long, y=lat.x, group=group))
m1 <- m0 + geom_polygon(aes(fill=receitaMedia)) + coord_equal()
m2 <- m1 + geom_path(aes(x=long, y=lat.x, group=group), color='grey', size=.1)
m3 <- m2 + theme_tb(legenda="right", dir.Legenda="vertical") + xlab("") + ylab("")
m4 <- m3 +  theme(axis.text.x = element_blank(), axis.text.y = element_blank())
m5 <- m4 + scale_fill_continuous(low="#56B1F7", high="#132B43", labels=dollar)
m5

setwd("D:/2015/Transparência/gráficos")
ggsave(m5, file="mapa_receita_media_fem_dep_est_e_dist_comreceita.pdf",  scale=2)

