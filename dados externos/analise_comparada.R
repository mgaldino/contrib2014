## Análise
library("foreign")
library(ggplot2)
library(maps)
library(rworldmap)
library(rgdal)
setwd("D:\\2015\\TB\\mulheres\\contrib2014\\dados externos")

qog <- read.dta("qog_bas_cs_jan15.dta")
head(qog)
summary(qog$ipu_l_sw)

qog1 <- subset(qog, select=c("cname", "ccodealp",  "ccodecow",
                             "gol_est_spec", "ipu_l_s", "ipu_l_sw", "ipu_l_w"))
head(qog1)
qog1$ipu_l_sw <- qog1$ipu_l_sw/100

qog1$nomePais <- qog1$cname

qog1$nomePais <- gsub( " \\(-[0-9]+\\)" , "",qog1$nomePais)
qog1$nomePais <- gsub( " \\([0-9]+-\\)" , "",qog1$nomePais)
unique(qog1$nomePais)


dodgewidth <- position_dodge(width=0.9)

p <- ggplot(qog1, aes(x = cname, y = ipu_l_sw)) + geom_bar(stat = "identity", position=dodgewidth) +
     scale_fill_grey(start = 0.1, end = .9, name="") + theme_tb(legenda="right") + coord_flip() +
  ylab("% PArticipação de mulheres") + xlab("") +  scale_y_continuous(labels = percent) 
p

## Mapa
setwd("D:/2015/TB/mulheres/contrib2014/dados externos/ne_110m_admin_0_countries") 

world  = map_data("world")
mapa2 <- getMap()
mapaf <- fortify(mapa2)
head(mapaf)
countries <- readOGR(".", layer="ne_110m_admin_0_countries") 
countries_robin <- spTransform(countries, CRS("+init=ESRI:54030"))
countries_robin_df <- fortify(countries_robin)

rm(countries, countries_robin, countries_robin_df)




qog1$nomePais <- gsub("United States", "USA", qog1$nomePais)
qog1$nomePais <- gsub("Korea, North", "North Korea", qog1$nomePais)
qog1$nomePais <- gsub("Korea, South", "South Korea", qog1$nomePais)
qog1$nomePais <- gsub("United Kingdom", "UK", qog1$nomePais)
qog1$nomePais <- gsub("United Kingdom", "UK", qog1$nomePais)
qog1$nomePais <- gsub("Russia", "USSR", qog1$nomePais)


## merging with data of health
world1 <- merge (world, qog1, by.x="region", by.y="nomePais", all.x=T, all.y=F)

# necessary to reorder. If not, map will not be plotted corrected
world1 <- world1[order(world1$order),]

# inverse order (to have visible borders)
m0 <- ggplot(data=world1)
m1 <- m0 + geom_polygon(aes(x=long, y=lat, group=group, fill=ipu_l_sw)) + coord_equal()
m2 <- m1 + geom_path(aes(x=long, y=lat, group=group), color='grey', size=.1)
m3 <- m2 + theme_tb(legenda="right", dir.Legenda="vertical") + xlab("") + ylab("")
m4 <- m3 +  theme(axis.text.x = element_blank(), axis.text.y = element_blank())
m5 <- m4 + scale_fill_continuous(low="red", high="blue", labels=percent, limits=c(0, .5))
m5

