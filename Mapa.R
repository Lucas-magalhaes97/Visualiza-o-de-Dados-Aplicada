require(sf)
require(dplyr)
require(ggplot2)
mapa <- st_read("/home/est/lma23/Documentos/Visualização de Dados Aplicada/Mapas/41MUE250GC_SIR.shp")
mapa
plot(st_geometry(mapa))

cidades <- c("CURITIBA","SÃO JOSÉ DOS PINHAIS",
             "PINHAIS","BOCAIÚVA DO SUL",
             "QUATRO BARRAS",
             "ITAPERUÇU","COLOMBO",
             "ALMIRANTE TAMANDARÉ")

mapa$NM_MUNICIP <- iconv(mapa$NM_MUNICIP,
                         from="latin1", 
                         to="utf-8")


cidades <- data.frame(cidades)
names(cidades)="NM_MUNICIP"
cidades2 <- left_join(cidades,mapa,by="NM_MUNICIP")
mapa_red <- st_as_sf(cidades2)

plot(st_geometry((mapa_red)))

dados_pr <- read.csv("/home/est/lma23/Documentos/Visualização de Dados Aplicada/Mapas/dados_pr.csv",dec=",",
                     sep=";",header=T)
#View(mapa_red)
names(dados_pr)[1]="CD_GEOCODM"
dados_pr$CD_GEOCODM=as.character(dados_pr$CD_GEOCODM)
mapa_redD <- left_join(mapa_red,dados_pr,by="CD_GEOCODM")
names(mapa_redD)

mapa1 <- 
  mapa_redD %>% ggplot(aes(fill=IDH.municipal))+
  geom_sf()+
  scale_fill_gradient(low="tomato",high = "green2")+
  theme_bw()+
  theme(legend.position = "bottom")

require(plotly)
ggplotly(mapa1)

### LEAFLET
require(leaflet)
require(htmltools)


qPaleta <- colorQuantile("Blues",
                         mapa_redD$IDH.municipal,
                         10)

qPaleta <- colorQuantile(colorRampPalette(c("purple", "gold"))(10),
                         mapa_redD$IDH.municipal,
                         5)

ufpr <- data.frame(lat=-25.450052641847687,
                   lng=-49.231127407523545)
mapa_redD %>% 
  leaflet() %>% 
  addProviderTiles(providers$OpenStreetMap.France) %>% 
  addPolygons(color=NA,
              label=paste(mapa_redD$NM_MUNICIP,
                          "IDHm:",mapa_redD$IDH.municipal),
              fillOpacity = 0.6,
              fillColor = qPaleta(mapa_redD$IDH.municipal)) %>% 
  addLegend(pal=qPaleta,
            values=mapa_redD$IDH.municipal,
            position = "bottomright") %>% 
  addMarkers(lat=ufpr$lat,lng=ufpr$lng,
             popup = "Estamos Aqui!")

marcas <- data.frame(lat=runif(5,-26.45,-24.45),
                     lng=runif(5,-50.23,-48.23))

marcas %>% leaflet() %>% 
  addTiles() %>% 
  addMarkers()

LEGIcon <- makeIcon(iconUrl = "https://upload.wikimedia.org/wikipedia/commons/thumb/7/7d/Corinthianssccp2023.png/1200px-Corinthianssccp2023.png",
                    iconWidth = 25, iconHeight = 25)

marcas %>% leaflet() %>% 
  addTiles() %>% 
  addMarkers(icon=LEGIcon)
