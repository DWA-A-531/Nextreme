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
  require(terra)
  require(rdwd)
  require(lubridate)
  if(missing(Standorte)) stop("Das Standorte Input ist nicht vorhanden! Bitte geben Sie einen data.frame mit den Standorten der Stationen ein.")
  else if(class(Standorte)!="data.frame") stop("Das Standorte Input sollte als data.frame sein! Bitte geben Sie einen data.frame mit den Standorten der Stationen ein.")
  else if(dim(Standorte)[1]<1) stop("Das Standorte Input soll mindestens eine Zeile enthalten!")
  else if(dim(Standorte)[2]<3) stop("Das Standorte Input soll mindestens 3 Spalten enthalten (bzw. fuer Stations_id, geoLaenge,geoBreite)!")
  else if(any(is.na(Standorte)==T)) stop("Das Standorte Input enthaelt mindestens einen fehlenden Wert! Entfernen Sie die fehlenden Werte")
  else if(any(c("Stations_id","geoLaenge", "geoBreite")%in%names(Standorte)==F)) stop("Das Standorte Input sollte alle der folgenden Spaltennamen enthalten: Stations_id, geoLaenge, geoBreite!")

  if(dir.exists(Temp_Pfad)==F) warning("Das Temp_Pfad Input existiert nicht! Es wird ein weiterer temporaerer Pfad erstellt.")

  KOSTRA_Link <- "return_periods/precipitation/KOSTRA/KOSTRA_DWD_2020/asc/"
  data("gridIndex")
  KOSTRA_Files <- grep(KOSTRA_Link, gridIndex, value=TRUE)

  # das Koordinatenreferenzsystem fuer die eingegebenen Stationsdaten angeben
  station_CRS = "+proj=longlat +datum=WGS84"
  # das Koordinatenreferenzsystem fuer die Ausgabedaten angeben
  Kostra_CRS = "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs +type=crs"
  # aenderung der Koordinaten der Stationen in das Ausgabekoordinatensystem.
  station_shp = vect(Standorte, geom =c("geoLaenge","geoBreite"), crs=station_CRS)
  station_shp = project(station_shp, Kostra_CRS)

  temp = tempfile()
  if (dir.exists(Temp_Pfad)==T) temp = tempfile(tmpdir = Temp_Pfad)
  download.file(paste0(gridbase,"/",KOSTRA_Files[1]),temp)

  unzip(temp, "Parameter_KOSTRA-DWD-2020_KOUT_1_THETA.asc", exdir = "./")
  Theta = rast("Parameter_KOSTRA-DWD-2020_KOUT_1_THETA.asc")
  crs(Theta) = Kostra_CRS

  unzip(temp, "Parameter_KOSTRA-DWD-2020_KOUT_2_ETA.asc", exdir = "./")
  Eta = rast("Parameter_KOSTRA-DWD-2020_KOUT_2_ETA.asc")
  crs(Eta) = Kostra_CRS

  unzip(temp, "Parameter_KOSTRA-DWD-2020_LOCAL_XI.asc", exdir = "./")
  Mu = rast("Parameter_KOSTRA-DWD-2020_LOCAL_XI.asc")
  crs(Mu) = Kostra_CRS

  unzip(temp, "Parameter_KOSTRA-DWD-2020_SCALE_ALPHA.asc", exdir = "./")
  Sigma = rast("Parameter_KOSTRA-DWD-2020_SCALE_ALPHA.asc")
  crs(Sigma) = Kostra_CRS

  unzip(temp, "Parameter_KOSTRA-DWD-2020_SHAPE_KAPPA.asc", exdir = "./")
  Gamma = rast("Parameter_KOSTRA-DWD-2020_SHAPE_KAPPA.asc")
  crs(Gamma) = Kostra_CRS

  Kostra_Parameter = data.frame(ID=Station$Stations_id, geoBreite = Station$geoBreite, geoLaenge = Station$geoLaenge,
                                 Theta = extract(Theta, station_shp)[,-1], Eta = extract(Eta, station_shp)[,-1], Mu = extract(Mu, station_shp)[,-1],
                                 Sigma = extract(Sigma, station_shp)[,-1], Gamma = extract(Gamma, station_shp)[,-1])
  return(Kostra_Parameter)
}
