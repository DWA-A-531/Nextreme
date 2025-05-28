#' Extraktion der KOSTRA-DWD-2020 Parameter fuer bestimmte Standorte
#' @description
#' Die geschaetzten Parameter von KOSTRA-DWD-2020 werden aus dem DWD-Klimadatenzentrum fuer bestimmte Standorte ausgelesen. Als Eingabe werden die Koordinaten der Standorte benoetigt.
#' @param Standorte Ein Datenframe mit den Standorten, aus denen die KOSTRA-DWD-2020-Daten extrahiert werden sollen. Der Dataframe sollte drei Spalten haben: die Standort-ID - 'Stations_id', die Laengenkoordinaten - 'geoLaenge' und die Breitenkoordinaten 'geoBreite'. Die Koordinaten sollten in der crs(„+proj=longlat +datum=WGS84“) sein!
#' @param Temp_Pfad Ein Ordner-Pfad, in den die KOSTRA-Daten heruntergeladen werden koennen.
#' @details
#'  R-Funktion, die die von KOSTRA-DWD-2020 geschaetzten Parameter in eine bestimmte oder temporaere Datei (Folder) herunterlaedt und die entsprechenden Parameterwerte (Theta - Koutsoyiannis erster Parameter, Eta - Koutsoyiannis zweiter Parameter, Mu - GEV Lokationsparameter, Sigma - GEV Skalenparameter und Gamma - GEV Formparameter) fuer die gewuenschten Positionen liest und zurueckgibt.
#' @return Es wird eine Tabelle im Datenframe-Format mit den Koordinaten und den entsprechenden geschaetzten Kostra2020-Parametern (in jeder Spalte angegeben) fuer die angegebenen Standorte (in jeder Reihe angegeben) zurueckgegeben. Fuer Standorte, die ausserhalb der KOSTRA-DWD-2020 Bereiche liegen, werden NA-Werte zurueckgegeben.
#' @examples
#' Station = data.frame(Stations_id = 01684, geoBreite = 51.1621, geoLaenge = 14.9506)
#' kostraParameter = Kostra2020_Parameter(Standorte = Station )
Kostra2020_Parameter = function(Standorte, Temp_Pfad = "./"){
  if(missing(Standorte)) stop("Das Standorte Input ist nicht vorhanden! Bitte geben Sie einen data.frame mit den Standorten der Stationen ein.")
  else if(class(Standorte)!="data.frame") stop("Das Standorte Input sollte als data.frame sein! Bitte geben Sie einen data.frame mit den Standorten der Stationen ein.")
  else if(dim(Standorte)[1]<1) stop("Das Standorte Input soll mindestens eine Zeile enthalten!")
  else if(dim(Standorte)[2]<3) stop("Das Standorte Input soll mindestens 3 Spalten enthalten (bzw. fuer Stations_id, geoLaenge,geoBreite)!")
  else if(any(is.na(Standorte)==T)) stop("Das Standorte Input enthaelt mindestens einen fehlenden Wert! Entfernen Sie die fehlenden Werte")
  else if(any(c("Stations_id","geoLaenge", "geoBreite")%in%names(Standorte)==F)) stop("Das Standorte Input sollte alle der folgenden Spaltennamen enthalten: Stations_id, geoLaenge, geoBreite!")

  if(dir.exists(Temp_Pfad)==F) warning("Das Temp_Pfad Input existiert nicht! Es wird ein weiterer temporaerer Pfad erstellt.")

  KOSTRA_Link <- "https://opendata.dwd.de/climate_environment/CDC/grids_germany/return_periods/precipitation/KOSTRA/KOSTRA_DWD_2020/asc/"
  KOSTRA_htmllines <- readLines(KOSTRA_Link, warn = FALSE)
  # Find lines with "_ASC.zip"
  zip_lines <- grep("_ASC\\.zip", KOSTRA_htmllines, value = TRUE)
  # Extract the filenames
  KOSTRA_AllFiles <- gsub('.*href="([^"]+_ASC\\.zip)".*', '\\1', zip_lines)

  # das Koordinatenreferenzsystem fuer die eingegebenen Stationsdaten angeben
  station_CRS = "+proj=longlat +datum=WGS84"
  # das Koordinatenreferenzsystem fuer die Ausgabedaten angeben
  Kostra_CRS = "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs +type=crs"
  # aenderung der Koordinaten der Stationen in das Ausgabekoordinatensystem.
  station_shp = terra::vect(Standorte, geom =c("geoLaenge","geoBreite"), crs=station_CRS)
  station_shp = terra::project(station_shp, Kostra_CRS)

  temp = tempfile()
  if (dir.exists(Temp_Pfad)==T) temp = tempfile(tmpdir = Temp_Pfad)
  utils::download.file(paste0(KOSTRA_Link,"/",KOSTRA_AllFiles[grep("Parameter", KOSTRA_AllFiles, value=F)]),temp)

  utils::unzip(temp, "Parameter_KOSTRA-DWD-2020_KOUT_1_THETA.asc", exdir = "./")
  Theta = terra::rast("Parameter_KOSTRA-DWD-2020_KOUT_1_THETA.asc")
  terra::crs(Theta) = Kostra_CRS

  utils::unzip(temp, "Parameter_KOSTRA-DWD-2020_KOUT_2_ETA.asc", exdir = "./")
  Eta = terra::rast("Parameter_KOSTRA-DWD-2020_KOUT_2_ETA.asc")
  terra::crs(Eta) = Kostra_CRS

  utils::unzip(temp, "Parameter_KOSTRA-DWD-2020_LOCAL_XI.asc", exdir = "./")
  Mu = terra::rast("Parameter_KOSTRA-DWD-2020_LOCAL_XI.asc")
  terra::crs(Mu) = Kostra_CRS

  utils::unzip(temp, "Parameter_KOSTRA-DWD-2020_SCALE_ALPHA.asc", exdir = "./")
  Sigma = terra::rast("Parameter_KOSTRA-DWD-2020_SCALE_ALPHA.asc")
  terra::crs(Sigma) = Kostra_CRS

  utils::unzip(temp, "Parameter_KOSTRA-DWD-2020_SHAPE_KAPPA.asc", exdir = "./")
  Gamma = terra::rast("Parameter_KOSTRA-DWD-2020_SHAPE_KAPPA.asc")
  terra::crs(Gamma) = Kostra_CRS

  Kostra_Parameter = data.frame(ID=Standorte$Stations_id, geoBreite = Standorte$geoBreite, geoLaenge = Standorte$geoLaenge,
                                 Theta = terra::extract(Theta, station_shp)[,-1], Eta = terra::extract(Eta, station_shp)[,-1], Mu = terra::extract(Mu, station_shp)[,-1],
                                 Sigma = terra::extract(Sigma, station_shp)[,-1], Gamma = terra::extract(Gamma, station_shp)[,-1])
  return(Kostra_Parameter)
}
