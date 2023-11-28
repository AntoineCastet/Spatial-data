#Montrer à quel point le fait de garder uniquement les villes français est facile.

#Je calcule les distances avec les lignes.

#Je calcule la moyenne raster de la densité de population par commune.

#Création buffer

#Création grille

#Création graph carte à la fin

# List of required packages
packages <- c("dplyr", "here", "qgisprocess", "sf", "viridis", "dplyr", "ggplot2", "terra")

# Function to install and load packages if they are not already installed
load_package <- function(package) {
  if (!requireNamespace(package, quietly = TRUE)) {
    install.packages(package)
  }
  library(package, character.only = TRUE)
}

# Apply the function to each package in the list
lapply(packages, load_package)

modif_path <- here("Modif")

#QGIS
#Je charge mes bases de données que j'ai déjà nettoyé sur QGIS.
Paris <- st_read("Paris.gpkg") 
Villes <- st_read("Villes.gpkg")
Lignes_tgv <- st_read("Lignes_tgv.gpkg")
Communes <- st_read("gadm41_FRA_3.shp")
#Je crée un id unique pour chaque ville car GADM ne fournit pas cet identifiant.
Communes$id <- seq_len(nrow(Communes))
Densité <- rast("fra_pd_2020_1km.tif")

#Je crée une fonction qui va reprojeter les données shapefile en utilisant QGIS.
reproject_and_convert <- function(input_layer, output_file) {
  reprojected <- qgis_run_algorithm("native:reprojectlayer", 
                                    INPUT = input_layer,
                                    TARGET_CRS = 'EPSG:4087',
                                    OPERATION = '+proj=pipeline +step +proj=unitconvert +xy_in=deg +xy_out=rad +step +proj=eqc +lat_ts=0 +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84',
                                    OUTPUT = output_file)
  #Je transforme ce fichier QGIS en fichier sf grâce à la commande suivante.
  sf::st_as_sf(reprojected)
}

#J'utilise la fonction créée avec l'ensemble des rasters que j'ai précédemment chargé.
Paris_4087 <- reproject_and_convert(Paris, file.path(modif_path, 'Paris_4087.gpkg'))
Villes_4087 <- reproject_and_convert(Villes, file.path(modif_path, 'Villes_4087.gpkg'))
Communes_4087 <- reproject_and_convert(Communes, file.path(modif_path, 'Communes_4087.gpkg'))
Lignes_tgv_4087 <- reproject_and_convert(Lignes_tgv, file.path(modif_path, 'Lignes_tgv_4087.gpkg'))

#Je reprojecte le fichier raster en utilisant QGIS.
Densité_4087 <- qgis_run_algorithm("gdal:warpreproject", 
                                   INPUT = 'fra_pd_2020_1km.tif',
                                   TARGET_CRS = 'EPSG:4087',
                                   RESAMPLING = 0,
                                   DATA_TYPE = 0,
                                   OUTPUT = file.path(modif_path, "Densité_4087.tif"))
#Je transforme ce fichier QGIS en fichier raster grâce à la commande suivante.
Densité_4087 <- qgis_as_terra(Densité_4087)

#J'estime la distance entre les communes et Paris.
Distance <-  qgis_run_algorithm(
  "native:shortestline",
  SOURCE = Communes_4087,
  DESTINATION = Paris_4087,
  METHOD = 0,
  NEIGHBORS = 1
)
Distance <- sf::st_as_sf(Distance)

#Je crée une grille de 100km sur 100km grâce à QGIS.
Grille <- qgis_run_algorithm("native:creategrid", 
                             TYPE = 2,
                             EXTENT = '-572599.742100000,1064260.665600000,4601252.225200000,5687245.707100000 [EPSG:4087]',
                             HSPACING = 100000,
                             VSPACING= 100000,
                             HOVERLAY = 0,
                             VOVERLAY = 0,
                             CRS = 'EPSG:4087',
                             OUTPUT = file.path(modif_path, 'Grille.gpkg'))
Grille <- sf::st_as_sf(Grille)

#Je calcule la moyenne de la valeur du raster au sein de chaque commune.
Statistiques <- qgis_run_algorithm("native:zonalstatisticsfb", 
                                   INPUT = Communes_4087,
                                   INPUT_RASTER = Densité_4087,
                                   RASTER_BAND = 1,
                                   COLUMN_PREFIX = 'QGIS_',
                                   STATISTICS = c(0,1,2)
)
Statistiques <- sf::st_as_sf(Statistiques)

#Je calcule la moyenne de la valeur du raster au sein de chaque cellule de la grille.
Statistiques_grille <- qgis_run_algorithm("native:zonalstatisticsfb", 
                                   INPUT = Grille,
                                   INPUT_RASTER = Densité_4087,
                                   RASTER_BAND = 1,
                                   COLUMN_PREFIX = 'QGIS_',
                                   STATISTICS = c(0,1,2)
)
Statistiques_grille <- sf::st_as_sf(Statistiques_grille)

#Je crée un buffer de 25km autour des villes.
Buffer <- qgis_run_algorithm("native:buffer", 
                             INPUT = Villes_4087,
                             DISTANCE = 25000,
                             SEGMENTS = 5,
                             END_CAP_STYLE = 0,
                             JOIN_STYLE = 0,
                             MITER_LIMIT = 2,
                             OUTPUT = file.path(modif_path, 'Buffer.gpkg'))
Buffer <- sf::st_as_sf(Buffer)

#Je fais en sorte d'identifier les lignes de TGV qui intersecte les buffers autour des villes. 
#J'attribue les lignes de TGV aux villes grâce à QGIS.
Villes_lignes <- qgis_run_algorithm("native:joinattributesbylocation", 
               INPUT = Buffer,
               PREDICATE = 0,
               JOIN = Lignes_tgv_4087,
               METHOD = 0,
               OUTPUT = 'Villes_lignes.gpkg')
Villes_lignes <- sf::st_as_sf(Villes_lignes)



#R
Communes <- st_read("gadm41_FRA_3.shp")
Communes$id <- seq_len(nrow(Communes))

Départements <- st_read("gadm41_FRA_2.shp")
Paris <- subset(Départements, NAME_2=="Paris")

ne_10m_populated_places <- st_read("ne_10m_populated_places_simple.shp")
Villes <- subset(ne_10m_populated_places, ADM0NAME=="France" & TIMEZONE=="Europe/Paris")

Densité <- rast("fra_pd_2020_1km.tif")

Lignes_train <- st_read("lignes-lgv-et-par-ecartement.shp")
Lignes_tgv <- subset(Lignes_train, catlig=="Ligne à grande vitesse")

Communes_4087 <- st_transform(Communes, crs=4087)
Paris_4087 <- st_transform(Paris, crs=4087)
Villes_4087 <- st_transform(Villes, crs=4087)
Lignes_tgv_4087 <- st_transform(Lignes_tgv, crs=4087)


Communes_4087 <- st_transform(Communes, crs="EPSG:4087")
crs(Densité) <- crs("+init=epsg:4326")
Densité_4087 <- project(Densité, "EPSG:4087")

Villes_4087$distance <- as.numeric(st_distance(Paris_4087, Villes_4087))

# Extract raster values to list object
Test <-  terra::extract(Densité_4087, Communes_4087, fun = mean, na.rm=TRUE, ID=FALSE) 
colnames(Test) <- "QGIS_mean"
Statistiques <- cbind(Communes_4087, Test)

# Create grid
bbox <- st_bbox(Communes_4087)
Grille <- st_make_grid(st_bbox(Communes_4087), crs = st_crs(4087),
                     cellsize = c(100000, 100000),
                     square = TRUE)
Grille <- st_sf(geometry = Grille)
Test <- terra::extract(Densité_4087, Grille, fun = mean, na.rm=TRUE, ID=FALSE) 
colnames(Test) <- "QGIS_mean"
Statistiques_grille <- cbind(Grille, Test)

Buffer <- st_buffer(Villes_4087, dist = 50000)
Villes_lignes <- st_join(Buffer, Lignes_tgv_4087, join = st_intersects)













#Je souhaite pouvoir créer des graphiques.


#######################
# Graphique 1
#######################
#Je vais fusionner les informations obtenues à l'aide des buffers aux informations (et surtout à la géométrie)
# des villes

#Je supprime la géometrie du fichier avec les buffers.
Villes_lignes <- st_drop_geometry(Villes_lignes)
#Je fusionne le fichier buffer (contenant les informations liés aux lignes TGV) avec le fichier des villes.
Villes_lignes <- left_join(Villes_4087, Villes_lignes, by = "NAME")

#Je crée deux sous-fichiers, un avec les villes traversées et un avec les villes non-traversées.
Villes_traversées <- subset(Villes_lignes, !is.na(code_ligne))
Villes_non_traversées <- subset(Villes_lignes, is.na(code_ligne))

#Je vais séparer les communes par groupes de quantiles pour mieux visualiser.
quantiles <- quantile(Statistiques$QGIS_mean, probs = seq(0, 1, by = 0.1))
Statistiques$categorical_var <- cut(Statistiques$QGIS_mean, 
                                    breaks = quantiles, 
                                    include.lowest = TRUE, 
                                    labels = paste("D", 1:10, sep=""),
                                    na.rm = FALSE)

#Je crée le graphique.
ggplot() +  
  geom_sf(data = Statistiques, aes(fill=categorical_var)) +
  geom_sf(data = Lignes_tgv_4087, color = "red", lwd = 1) +
  geom_sf(data = Villes_traversées, color = "red", fill="black", size = 2) +
  geom_sf(data = Villes_non_traversées, color = "black", size = 2) +
  scale_fill_viridis_d()


#######################
# Graphique 2
#######################
#Je vais fusionner les informations obtenues à l'aide du calcul de distance aux informations (et surtout à 
#la géométrie) des communes.
#Je supprime la géometrie du fichier avec les lignes de distance.
Distance <- st_drop_geometry(Distance)

#Je fusionne le fichier distance (contenant les informations liés à la distance à Paris) avec le fichier des communes.
Communes_distance <- left_join(Communes_4087, Distance, by = "id")

#Je crée le graphique.
ggplot() +  
  geom_sf(data = Communes_distance, aes(fill=distance)) +
  scale_fill_viridis()


#######################
# Graphique 3
#######################
#Je crée le graphique.
ggplot() +  
  geom_sf(data = Statistiques_grille, aes(fill=QGIS_mean)) +
  scale_fill_viridis() 