#inicio parte reutilizable
#cargar paquetes
library(rgrass7)
gisdbase <- 'grass-data-test' #Base de datos de GRASS GIS
wd <- getwd() #Directorio de trabajo
wd
loc <- initGRASS(gisBase = "/usr/lib/grass78/",
                 home = wd,
                 gisDbase = paste(wd, gisdbase, sep = '/'),
                 location = 'cana',
                 mapset = "PERMANENT",
                 override = TRUE)

#fin parte reutilizable

#Definir proyección de la región de GRASS GIS, importar fuente y utilizarla para definir extensión y resolución. Cómo ver la ayuda de las funciones ----
#Muestra la definición de la región
gmeta()

#Definir ruta del DEM
dem <- 'datos-fuente/srtm_dem_cuenca_cana.tif'

#Definir la proyección de la región basada en DEM
execGRASS(
  cmd = 'g.proj',
  flags = c('t','c'),
  georef = dem)




