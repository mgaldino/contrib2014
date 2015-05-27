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

analiseGeneroCargoUf <- base1[ , list(receitaMedia =mean(receita ), totalCandidatos = length(CPF.do.candidato)), by=c("genero", "bolReceita","cargo","sigla")]
analiseGeneroCargoUf <- subset(analiseGeneroCargoUf, bolReceita=F)
names(analiseGeneroCargoUf)[4] <- "UF"

maScDepEst <- subset(analiseGeneroCargoUf, genero=="MASCULINO" & cargo %in% c("DEPUTADO ESTADUAL", "DEPUTADO DISTRITAL") & bolReceita==F )
maScDepEst <- subset( maScDepEst, select=c("UF", "receitaMedia"))
length(maScDepEst$UF)


lonlat <- matrix(NA, nrow=27, ncol=2)
for(i in 1:nrow(maScDepEst)){
  lonlat[i,] <- unlist(geocode(paste(maScDepEst$UF[i], ", Brazil", sep=""), output = "latlon"))
  }

save(lonlat, file="lonlat_v1.RData")

maScDepEst[ , lon := lonlat[,1]]
maScDepEst[ , lat := lonlat[,2]]
## getting map brasil
map <- get_map(location = "Brazil", zoom = 4, maptype = "terrain", source = "google")

map1 <- get_map(location = "Brazil", zoom = 4, maptype = "satellite", source = "google")
  
p <- ggmap(map)

#juntando o mapa com o pontos  
p <- p + geom_text(data=maScDepEst,aes(x = lon, y = lat, label = receitaMedia), colour="black",
                   size=4,hjust=0, vjust=0)+ theme(legend.position = "none") 

#colocando os pontos em cima do mapa
p <- p + geom_point(data=maScDepEst, aes(x=lon, y=lat, size=receitaMedia), colour="blue")

p


###Tentativa
setwd("D:\\2015\\TB\\mulheres\\contrib2014\\dados externos")
shapeBr <- readOGR (dsn=".", layer="UFEBRASIL")
names(shapeBr)
shapeBr@data
gpclibPermit()
mapaBrasil <- fortify(shapeBr, region="ID")
print(proj4string(shapeBr))

ogrInfo(".", "UFEBRASIL")

plot(shapeBr, axes=TRUE, border="gray")

# Transforma o arquivo de mapa em um grafico (ggplot2)
sp.map.2012 <- ggmap(sp.map, base_layer =
                       ggplot(aes(x = lon, y = lat), data = latlon), extent = "device")

#Plota o resultado (os pontos dos endereços)
sp.map.2012 + geom_point(size = I(4),colour="red", alpha = 2/3)



ggmap(map1, extent="device")

world  = map_data("world")

## merging with data of health
world1 <- merge (world, pseM1, by.x="region", by.y="country", all.x=T, all.y=F)

# necessary to reorder. If not, map will not be plotted corrected
world1 <- world1[order(world1$order),]

m0 <- ggplot(data=world1)
# empty map (only borders)
m1 <- m0 + geom_path(aes(x=long, y=lat.x, group=group), color='gray') + coord_equal()

# fill with health expenditure data
m2 <- m1 + geom_polygon(aes(x=long, y=lat.x, group=group, fill=value))

# inverse order (to have visible borders)
m0 <- ggplot(data=world1)
m1 <- m0 + geom_polygon(aes(x=long, y=lat.x, group=group, fill=value)) + coord_equal()
m2 <- m1 + geom_path(aes(x=long, y=lat.x, group=group), color='grey', size=.1)
m2
ggsave(m2, file="mapa_qualidade_gasto_gov_saude.png",  units="cm", h = 10, w = 20, type = "cairo-png", dpi=150)


