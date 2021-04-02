#parte reutizable del script ----
#cargar paquetes
library(rgrass7)
library(sp)
library(sf)
library(raster)
gisdbase <- 'grass-data-test' #Base de datos de GRASS GIS
wd <- getwd() #Directorio de trabajo
wd
loc <- initGRASS(gisBase = "/usr/lib/grass78/",
                 home = wd,
                 gisDbase = paste(wd, gisdbase, sep = '/'),
                 location = 'cana',
                 mapset = "PERMANENT",
                 override = TRUE)

#unlink_.gislock()

#Fin de la parte reutilizable
#video 4 ----
#Definir proyección de la región de GRASS GIS, importar fuente y utilizarla para definir extensión y resolución. Cómo ver la ayuda de las funciones
#Muestra la definición de la región
gmeta()
#Definir ruta del DEM
dem <- 'datos-fuente/srtm_dem_cuenca_cana.tif'

#Definir la proyección de la región basada en DEM
execGRASS(
  cmd = 'g.proj',
  flags = c('t','c'),
  georef=dem)
#Muestra la definición de la región modificada
gmeta()

#Importar mapa raster
#r.in.gdal importa la fuente a GRASS
execGRASS(
  cmd = 'r.in.gdal',
  flags=c('overwrite','quiet'),
  parameters=list(
    input=dem,
    output='dem'
  )
)
#Actualizar la extensión de la región al DEM, sólo por precaución
execGRASS(
  cmd = 'g.region',
  parameters=list(
    raster = 'dem',
    align = 'dem'
  )
)
#Importar un mapa vectorial también
demext <- 'datos-fuente/cuenca_cana.geojson'
execGRASS(
  cmd = 'v.in.ogr',
  flags=c('overwrite','quiet'),
  parameters=list(
    input=demext,
    output='dem_extent'
  )
)
#Imprimir lista de mapas ráster y vectoriales dentro en la región/localización activa
execGRASS(
  'g.list',
  flags = 't',
  parameters = list(
    type = c('raster', 'vector')
  )
)
#Ver los addons disponibles en el repositorio oficial de GRASS GIS, incluyendo descripción
execGRASS(
  cmd = 'g.extension',
  flags = 'c'
)
#Video 5 Explorar datos espaciales básicos entre GRASS y R ----
#Cargar en R el DEM (mapa ráster)
use_sp()
dem_sp <- readRAST('dem')
op <- par()
plot(dem_sp)

#Cargar a R el mapa vectorial de una cuenca que se encuentra alojado fuera de GRASS, hacer el plot y representar la cuenca del rio cana superpuesta
rutacana <- 'datos-fuente/cuenca_cana.geojson'
cana <- st_read(rutacana)
plot(dem_sp)
plot(cana, add=T, col='transparent', border='black', lwd=5);par(op[c('mfrow','mar')])

#Analizar el DEM dentro de la cuenca del rio cana
dem_r0 <- raster(dem_sp)
dem_r1 <- crop(dem_r0, cana)
dem_cana <- mask(dem_r1, cana)
plot(dem_cana)

summary(dem_cana)
hist(dem_cana)

#Obtener variables de terreno básicas con el paquete raster dentro de R
pend_cana <- terrain(x = dem_cana, opt = 'slope', unit = 'degrees')
plot(pend_cana)

#video 6 Calcular parámetros hidrográficos con r.watershed. Visualizar con leaflet ----
# Calcular parámetros hidrográficos de interés usando `r.watershed`
execGRASS(
  "r.watershed",
  flags = c('overwrite','quiet'),
  parameters = list(
    elevation = "dem",
    accumulation = "accum-de-rwshed",
    stream = "stream-de-rwshed",
    drainage = "drainage-dir-de-rwshed",
    basin = 'basins',
    half_basin = 'half-basins',
    threshold = 80
  )
)
# Traer capas a R
#Usar Spatial*
library(sp)
use_sp()
#Paquete manejo de los raster
library(raster)
#DEM
dem <- raster(readRAST('dem'))
#Basins
basins <- raster(readRAST('basins'))
#Stream network
stream <- raster(readRAST('stream-de-rwshed'))
stream3857 <- projectRaster(stream, crs = CRS("+init=epsg:3857"), method = 'ngb')
#Generar un vectorial de extensión de capa en EPSG:4326
e <- extent(stream)
e <- as(e, 'SpatialPolygons')
proj4string(e) <- CRS("+init=epsg:32619")
e <- spTransform(e, CRSobj = CRS("+init=epsg:4326"))

# Visualizar capas con `leaflet`
library(leaflet)
library(leafem)
leaflet() %>%
  addProviderTiles(providers$Stamen.Terrain, group = 'terrain') %>%
  addRasterImage(dem, group='DEM', opacity = 0.5) %>%
  addRasterImage(
    ratify(basins),
    group='basins', opacity = 0.7,
    colors = sample(rep(RColorBrewer::brewer.pal(12, 'Set3'),1000))) %>% 
  addRasterImage(stream3857, project = F, group='str', opacity = 0.7, method = 'ngb', colors = 'blue') %>% 
  addLayersControl(
    overlayGroups = c('terrain','DEM','basins','str'),
    options = layersControlOptions(collapsed=FALSE)) %>% 
  addHomeButton(extent(e), 'Ver todo')

#Video 7 Extraer una cuenca de drenaje con r.water.outlet. Visualizar con mapview y leaflet ----
# Obtener las coordenadas de la desembocadura de la cuenca de interés
library(mapview)
mapview(
  stream3857, method='ngb', col.regions = 'blue',
  legend = FALSE, label = FALSE, maxpixels =  910425
)
# Convertir las coordenadas lat/lon a EPSG:32619
my_trans <- function(coords = NULL) {
  require(sp)
  pt <- SpatialPoints(matrix(coords, ncol = 2), CRS("+init=epsg:4326"))
  foo <- spTransform(pt, CRSobj = CRS("+init=epsg:32619"))
  bar <- as.vector(coordinates(foo))
  return(bar)
}
cana_out <- my_trans(coords = c(-71.62524,18.94026))
cana_out
## Extraer la cuenca de interés
execGRASS(
  "r.water.outlet",
  flags = c('overwrite','quiet'),
  parameters = list(
    input = 'drainage-dir-de-rwshed',
    output = 'cana-basin',
    coordinates = cana_out
  )
)
## Convertir la cuenca a vectorial en GRASS
execGRASS(
  "r.to.vect",
  flags = c('overwrite','quiet'),
  parameters = list(
    input = 'cana-basin',
    output = 'cana_basin',
    type = 'area'
  )
)
## Mostrar lista nuevamente
execGRASS(
  'g.list',
  flags = 't',
  parameters = list(
    type = c('raster', 'vector')
  )
)
## Traer a R la cuenca del rio cana
cana_bas <- readVECT('cana_basin')
cana_bas
plot(cana_bas)
cana_bas4326 <- spTransform(cana_bas, CRSobj = CRS("+init=epsg:4326"))
leaflet() %>% 
  addProviderTiles(providers$Stamen.Terrain) %>%
  addRasterImage(stream, opacity = 0.7, method = 'ngb', colors = 'blue') %>% 
  addPolygons(data = cana_bas4326) %>% 
  leafem::addHomeButton(extent(cana_bas4326), 'Ver cuenca')

#Video 8 Extraer una red drenaje con r.stream.extract. Visualizar con leaflet ----
#Usar la cuenca del rio cana como máscara
execGRASS(
  "r.mask",
  flags = c('verbose','overwrite','quiet'),
  parameters = list(
    vector = 'cana_basin'
  )
)
# Extraer la red de drenaje de la cuenca de interés
execGRASS(
  "r.stream.extract",
  flags = c('overwrite','quiet'),
  parameters = list(
    elevation = 'dem',
    threshold = 80,
    stream_raster = 'cana-stream-de-rstr',
    stream_vector = 'cana_stream_de_rstr'
  )
)
# Mostrar lista nuevamente
execGRASS(
  'g.list',
  flags = 't',
  parameters = list(
    type = c('raster', 'vector')
  )
)
## Traer a R la red de drenaje del rio cana
cana_net <- readVECT('cana_stream_de_rstr', ignore.stderr = T)
cana_net
plot(cana_net)
cana_net4326 <- spTransform(cana_net, CRSobj = CRS("+init=epsg:4326"))
cana_net4326
cana_centroid <- coordinates(rgeos::gCentroid(cana_bas4326))
cana_centroid
cana_net_r <- raster(readRAST('cana-stream-de-rstr'))
cana_net_r
cana_net_r3857 <- projectRaster(cana_net_r, crs = CRS("+init=epsg:3857"), method = 'ngb')
cana_net_r3857
leaflet() %>% 
  setView(lng = cana_centroid[1], lat = cana_centroid[2], zoom = 11) %>%
  addProviderTiles(providers$Stamen.Terrain, group = 'terrain') %>%
  addRasterImage(cana_net_r3857, opacity = 0.7, method = 'ngb', colors = 'grey20', group = 'str_raster') %>% 
  addPolylines(data = cana_net4326, weight = 3, opacity = 0.7, group = 'str_vect') %>% 
  leafem::addHomeButton(extent(cana_net4326), 'Ver todo') %>% 
  addLayersControl(
    overlayGroups = c('terrain','str_vect','str_raster'),
    options = layersControlOptions(collapsed=FALSE)) 

#Video 10 Orden de red, morfometría y análisis hortoniano usando r.stream ----
# Crear mapa de dirección de flujo a partir de r.stream
execGRASS(
  "r.stream.extract",
  flags = c('overwrite','quiet'),
  parameters = list(
    elevation = 'dem',
    threshold = 80,
    direction = 'drainage-dir-de-rstr'
  )
)
#Crear mapas de órdenes de red
execGRASS(
  "r.stream.order",
  flags = c('overwrite','quiet'),
  parameters = list(
    stream_rast = 'cana-stream-de-rstr',
    direction = 'drainage-dir-de-rstr',
    elevation = 'dem',
    accumulation = 'accum-de-rwshed',
    stream_vect = 'order_all',
    strahler = 'order-strahler',
    horton = 'order-horton',
    shreve = 'order-shreve',
    hack = 'order-hack-gravelius',
    topo = 'order-topology'
  )
)
# Mostrar lista nuevamente
execGRASS(
  'g.list',
  flags = 't',
  parameters = list(
    type = c('raster', 'vector')
  )
)
# Visualizar la red con leaflet
#Simbología única
order <- readVECT('order_all')

order4326 <- spTransform(order, CRSobj = CRS("+init=epsg:4326"))
leaflet() %>% 
  addProviderTiles(providers$Stamen.Terrain, group = 'terrain') %>%
  addPolylines(
    data = order4326, weight = 3, opacity = 0.7, group = 'order',
    label = ~as.character(strahler),
    highlightOptions = highlightOptions(color = "white",
                                        weight = 5, bringToFront = F, opacity = 1),
    labelOptions = labelOptions(noHide = T,
                                style = list(
                                  "font-size" = "8px",
                                  "background" = "rgba(255, 255, 255, 0.5)",
                                  "background-clip" = "padding-box",
                                  "padding" = "1px"))) %>% 
  leafem::addHomeButton(extent(order4326), 'Ver todo') %>% 
  addLayersControl(
    overlayGroups = c('terrain','order'),
    options = layersControlOptions(collapsed=FALSE))

#Simbología aplicando grosor según orden de red
leaflet() %>% 
  addProviderTiles(providers$Stamen.Terrain, group = 'terrain') %>%
  addPolylines(
    data = order4326, weight = order4326$strahler*1.5, opacity = 0.7, group = 'order',
    label = ~as.character(strahler),
    highlightOptions = highlightOptions(color = "white",
                                        weight = 5, bringToFront = F, opacity = 1),
    labelOptions = labelOptions(noHide = F)) %>% 
  leafem::addHomeButton(extent(order4326), 'Ver todo') %>% 
  addLayersControl(
    overlayGroups = c('terrain','order'),
    options = layersControlOptions(collapsed=FALSE))

#Delimitar cuencas según orden de red de Strahler
#Obtener órdenes de red mínimo y máximo
#Estadísticas para obtener los valores mínimo y máximo del orden de red de Strahler
rinfo.ordstra <- execGRASS(
  'r.info',
  flags = 'r',
  parameters = list(
    map = 'order-strahler'
  )
)
#Órdenes de red mínimo y máximo
minmaxord <- as.numeric(
  stringr::str_extract_all(
    attributes(rinfo.ordstra)$resOut,
    "[0-9]+"
  )
)
minmaxord

### Delimitar cuencas, convertirlas de ráster a vectorial
sapply(
  min(minmaxord):max(minmaxord),
  function(x){
    execGRASS(
      "r.stream.basins",
      flags = c('overwrite','c','quiet'),
      parameters = list(
        direction = 'drainage-dir-de-rstr',
        stream_rast = 'order-strahler',
        cats = as.character(x),
        basins = paste0('r-stream-basins-',x)
      )
    )
    execGRASS(
      "r.to.vect",
      flags=c('overwrite','quiet'),
      parameters = list(
        input = paste0('r-stream-basins-',x),
        output = paste0('r_stream_basins_',x),
        type = 'area'
      )
    )
  }
)

#Representar las cuencas con leaflet
sapply(
  min(minmaxord):max(minmaxord),
  function(x){
    assign(
      paste0('orden', x),
      spTransform(readVECT(paste0('r_stream_basins_',x)), CRSobj = CRS("+init=epsg:4326")),
      envir = .GlobalEnv)
  }
)
paleta <- RColorBrewer::brewer.pal(12, 'Set3')
leaflet() %>% 
  addProviderTiles(providers$Stamen.Terrain, group = 'terrain') %>%
  addPolygons(data = orden4, stroke = T, weight = 2,
              color = ~paleta, fillOpacity = 0.4, group = 'O4') %>% 
  addPolygons(data = orden3, stroke = T, weight = 2,
              color = ~paleta, fillOpacity = 0.4, group = 'O3') %>%
  addPolygons(data = orden2, stroke = T, weight = 2,
              color = ~paleta, fillOpacity = 0.4, group = 'O2') %>%
  addPolygons(data = orden1, stroke = T, weight = 2,
              color = ~paleta, fillOpacity = 0.4, group = 'O1') %>%
  addPolylines(
    data = order4326, weight = order4326$strahler*1.5,
    opacity = 0.7, group = 'str_order') %>%
  leafem::addHomeButton(extent(order4326), 'Ver todo') %>% 
  addLayersControl(
    overlayGroups = c('terrain','O1','O2','O3','O4','str_order'),
    options = layersControlOptions(collapsed=FALSE))

#Estadísticas de red resumidas por orden de red.
execGRASS(
  "r.stream.stats",
  flags = c('overwrite','quiet','o'),
  parameters = list(
    stream_rast = 'order-strahler',
    direction = 'drainage-dir-de-rstr',
    elevation = 'dem',
    output = 'cana_stats.txt'
  )
)
file.show('cana_stats.txt')
d <- read.csv("cana_stats.txt", skip=1, header=TRUE)
plot(num_of_streams~order, data=d, log="y")
mod <- lm(log10(num_of_streams)~order, data=d)
abline(mod)
text(2, 20, 'logN=2.064-0.544u')
rb <- 1/10^mod$coefficients[[2]]
rb

#Estadísticas de red ampliadas
execGRASS(
  "r.stream.stats",
  flags = c('overwrite','quiet'),
  parameters = list(
    stream_rast = 'order-strahler',
    direction = 'drainage-dir-de-rstr',
    elevation = 'dem',
    output = 'cana_stats_expanded.txt'
  )
)
file.show('cana_stats_expanded.txt')