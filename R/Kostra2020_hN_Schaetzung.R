#' Extraktion der Kostra-DWD-2020 Regenhoehe (mm/Dauer) fuer bestimmte Standorte, Dauern und Wiederkehrintervalle
#' @description
#' Die geschaetzten Regenhoehe (mm/Dauer) von KOSTRA-DWD-2020 werden aus dem DWD-Klimadatenzentrum fuer bestimmte Standorte, Regendauern und Wiederkehrintervalle ausgelesen. Alternativ kann die Unsicherheitsabschaetzung fuer jeden Standort, jede Regendauern und Wiederkehrintervalle ausgelesen werden.
#' @param Standorte Ein Datenframe mit den Standorten, aus denen die KOSTRA-Daten extrahiert werden sollen. Der Dataframe sollte drei Spalten haben: die Standort-ID - 'Stations_id', die Laengenkoordinaten - 'geoLaenge' und die Breitenkoordinaten 'geoBreite'. Die Koordinaten sollten in der crs(„+proj=longlat +datum=WGS84“) sein!
#' @param Dauern die Regendauer(n), fuer die die Regenhoehe ausgelesen werden soll. Die Dauer sollte in Minuten angegeben werden!
#' @param Tn die Wiederkehrintervalle, fuer die die Regenhoehe ausgelesen werden soll. Die Wiederkehrintervalle sollten in Jahren angegeben werden!
#' @param Temp_Pfad Ein Ordner-Pfad, in den die KOSTRA-Daten heruntergeladen werden koennen.
#' @param Unsicherheit True oder False. Bestimmt, ob auch die Unsicherheitsabschaetzung gelesen werden soll. True - die Regenhoehen und die Unsicherheitsabschaetzung werden gelesen und zurueckgegeben, False - nur die Regen hoehen werden gelesen und zurueckgegeben. Standardwert ist True.
#' @details
#' R-Funktion, die die von KOSTRA-DWD-2020 geschaetzten Regenhoehen in eine bestimmte oder temporaere Datei (Folder) herunterlaedt, die entsprechenden Regenhoehe- und Unsicherheitsabschaetzungen fuer die gewuenschten Standorte, Regendauern und Wiederkehrintervalle liest und zurueckgibt.
#' @return Es wird eine Tabelle im Dataframe-Format mit den Koordinaten und den entsprechenden geschaetzten KOSTRA-DWD-2020-Regenhoehen fuer die angegebenen Standorte (in jeder Zeile angegeben), die Regendauer und die Wiederkehrintervalle (in jeder Spalte angegeben) zurueckgegeben.  Falls auch die Unsicherheit gewuenscht ist, wird eine Liste mit zwei Dataframes zurueckgegeben (eine fuer die Regenhoehe - Kostra_HN und eine fuer die Unsicherheit - Kostra_UC). Fuer Standorte ausserhalb der KOSTRA-DWD-2020 Bereiche werden NA-Werte zurueckgegeben.
#' @examples
#' Station = data.frame(Stations_id = 01684, geoBreite = 51.1621, geoLaenge = 14.9506)
#' Dauern = c(5,10,15)
#' Tn = c(50,100)
#' Kostra_Hn = Kostra2020_hN_Schaetzung(Station, Dauern, Tn, Unsicherheit=FALSE)
#' print(Kostra_Hn)
#' Kostra_Werte = Kostra2020_hN_Schaetzung(Station, Dauern, Tn, Unsicherheit=TRUE)
#' Kostra_Hn = Kostra_Werte$Kostra_HN
#' print(Kostra_Hn)
#' Kostra_UC = Kostra_Werte$Kostra_UC
#' print(Kostra_UC)
#' Kostra_UK_HN = Kostra_Hn[,-(1:3)] - Kostra_UC[,-(1:3)]*Kostra_Hn[,-(1:3)]/100
#' Kostra_OK_HN = Kostra_Hn[,-(1:3)] + Kostra_UC[,-(1:3)]*Kostra_Hn[,-(1:3)]/100
Kostra2020_hN_Schaetzung = function(Standorte,
                                    Dauern=c(5, 10, 15,30,60,120,360,720,1440, 2880, 4320, 10080),
                                    Tn=c(1,5,10,20,50,100),
                                    Temp_Pfad = "./",
                                    Unsicherheit = TRUE){
  Kostra_Dauern = c(5, 10, 15, 20, 30, 45, 60, 90, 120, 180, 240, 360, 540, 720, 1080, 1440, 2880, 4320, 5760, 7200, 8640, 10080)
  Kostra_Tn     = c(1, 2, 3, 5, 10, 20, 30, 50, 100)
  if(missing(Standorte)) stop("Das Standorte Input ist nicht vorhanden! Bitte geben Sie die Standorte als data.frame an.")
  else if(class(Standorte)!="data.frame") stop("Das Standorte Input sollte als data.frame sein! Bitte geben Sie die Standorte als data.frame an.")
  else if(dim(Standorte)[1]<1) stop("Das Standorte Input soll mindestens eine Zeile enthalten!")
  else if(dim(Standorte)[2]<3) stop("Das Standorte Input soll mindestens 3 Spalten enthalten (bzw. fuer Stations_id, geoLaenge,geoBreite)!")
  else if(any(is.na(Standorte)==T)) stop("Das Standorte Input enthaelt mindestens einen fehlenden Werte! Entfernen Sie die fehlenden Wert")
  else if(any(c("Stations_id","geoLaenge", "geoBreite")%in%names(Standorte)==F)) stop("Das Standorte Input sollte alle der folgenden Spaltennamen enthalten: Stations_id, geoLaenge, geoBreite!")

  # Bedingung 2: Das Input Dauern sollte als Zahlenvektor angegeben werden und mehr als 1 Element haben.
  if(class(Dauern)!="numeric") stop("Das Dauern Input sollte als numeric sein! Bitte einen 1d-Vektor der numerischen Regendauern angeben. Die Dauern sollten in Minuten sein.")
  else if(any(is.na(Dauern)==T)) stop("Das Dauern Input hat fehlende Werte! Bitte entfernen Sie diese.")
  else if(any(Dauern<=0)) stop("Das Dauern Input enthaelt negative oder Null Werte!")
  else if(any(Dauern%in%Kostra_Dauern==F)==T) stop(paste("Das Dauern Input enthaelt Werte die nicht innerhalb der Kostra-DWD-2020 Dauer verfuegbar sind! Die Kostra-DWD-2020 Dauer sind:", paste(Kostra_Dauern, collapse=", "), "Minute!"))

  # Bedingung 3: Das Input Tn sollte als Zahlenvektor angegeben werden und mehr als 0 Element haben.
  if(class(Tn)!="numeric") stop("Das Tn Input sollte als numeric sein! Bitte einen 1d-Vektor der numerischen Wiederkehrintervalle angeben.")
  else if(length(Tn)==0) stop("Das Tn Input ist leer! Bitte einen 1d-Vektor der numerischen Wiederkehrintervalle angeben.")
  else if(any(is.na(Tn)==T)) stop("Das Tn Input hat fehlende Werte! Bitte entfernen Sie diese.")
  else if(any(Tn<=0)) stop("Das Tn Input enthaelt negative oder Null Werte!")
  else if(any(Tn%in%Kostra_Tn==F)==T) stop(paste("Das Tn Input enthaelt Werte die nicht innerhalb der Kostra-DWD-2020 Wiederkehrintervalle verfuegbar sind! Die Kostra-DWD-2020 Wiederkehrintervalle sind:", paste(Kostra_Tn, collapse=", "), "Jahre!"))

  # Bedingung 4: Das Input Temp.Pfad soll existieren und als Charakter sein
  if(dir.exists(Temp_Pfad)==F) warning("Das Temp_Pfad Input existiert nicht! Es wird ein weiterer temporaerer Pfad erstellt.")

  # Bedingung 5: Das Unsicherheit Input kann entweder False oder True sein
  if(class(Unsicherheit)!="logical") stop("Das Unsicherheit Input sollte als logic sein: entweder False oder True.")
  else if(length(Unsicherheit)!=1) stop("Das Unsicherheit Input soll nur eine einzige Wert haben: entweder False oder True!")
  else if(Unsicherheit%in%c(TRUE, FALSE)==F) stop("Das Unsicherheit Input soll entweder False oder True sein!")

  KOSTRA_Link <- "https://opendata.dwd.de/climate_environment/CDC/grids_germany/return_periods/precipitation/KOSTRA/KOSTRA_DWD_2020/asc/"
  KOSTRA_htmllines <- readLines(KOSTRA_Link, warn = FALSE)
  # Find lines with "_ASC.zip"
  zip_lines <- grep("_ASC\\.zip", KOSTRA_htmllines, value = TRUE)
  # Extract the filenames
  KOSTRA_AllFiles <- gsub('.*href="([^"]+_ASC\\.zip)".*', '\\1', zip_lines)

  ### das Koordinatenreferenzsystem fuer die eingegebenen Stationsdaten angeben
  station_CRS = "+proj=longlat +datum=WGS84"
  ### das Koordinatenreferenzsystem fuer die Ausgabedaten angeben
  Kostra_CRS = "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs +type=crs"

  # aenderung der Koordinaten der Stationen in das Ausgabekoordinatensystem.
  station_shp = terra::vect(Standorte, geom =c("geoLaenge","geoBreite"), crs=station_CRS)
  station_shp = terra::project(station_shp, Kostra_CRS)

  temp = tempfile()
  if (dir.exists(Temp_Pfad)==T) temp = tempfile(tmpdir = Temp_Pfad)
  Kostra_HN_perDauer = do.call(cbind, lapply(Dauern, function(Dauer){
    Dauer_Datei = grep(formatC(Dauer,width=5,flag=0, format="d"),KOSTRA_AllFiles, value=TRUE)
    utils::download.file(paste0(KOSTRA_Link,"/",Dauer_Datei),temp)
    Dauer_Daten = do.call(cbind, lapply(Tn, function(tn){
      tn_Datei =  paste0("Hn_KOSTRA-DWD-2020_D",formatC(Dauer,width=5,flag=0, format="d"),"_T", formatC(tn,width=3,flag=0), ".asc")
      utils::unzip(temp,tn_Datei, exdir = "./")
      tn_Daten = terra::rast(tn_Datei)
      terra::crs(tn_Daten) = Kostra_CRS
      tn_output = terra::extract(tn_Daten, station_shp)[,-1]
      return(matrix(tn_output, ncol=1))
    }))
    return(Dauer_Daten)
  }))
  Kostra_HN_perDauer = as.data.frame(Kostra_HN_perDauer)
  names(Kostra_HN_perDauer) = unlist(lapply(Dauern, function(D) unlist(lapply(Tn, function(tn) paste0("D",formatC(D,width=5,flag=0, format="d"),"_T",formatC(tn,width=3,flag=0))))))
  Kostra_HN = data.frame(ID=Standorte$Stations_id, geoBreite = Standorte$geoBreite, geoLaenge = Standorte$geoLaenge, Kostra_HN_perDauer)

  Kostra_UC_perDauer = do.call(cbind, lapply(Dauern, function(Dauer){
    Dauer_Datei = grep(formatC(Dauer,width=5,flag=0, format="d"),KOSTRA_AllFiles, value=TRUE)
    utils::download.file(paste0(KOSTRA_Link,"/",Dauer_Datei),temp)
    Dauer_Daten = do.call(cbind, lapply(Tn, function(tn){
      tn_Datei =  paste0("UC_KOSTRA-DWD-2020_D",formatC(Dauer,width=5,flag=0, format="d"),"_T", formatC(tn,width=3,flag=0), ".asc")
      utils::unzip(temp,tn_Datei, exdir = "./")
      tn_Daten = terra::rast(tn_Datei)
      terra::crs(tn_Daten) = Kostra_CRS
      tn_output = terra::extract(tn_Daten, station_shp)[,-1]
      return(matrix(tn_output, ncol=1))
    }))
    return(Dauer_Daten)
  }))
  Kostra_UC_perDauer = as.data.frame(Kostra_UC_perDauer)
  names(Kostra_UC_perDauer) = unlist(lapply(Dauern, function(D) unlist(lapply(Tn, function(tn) paste0("D",formatC(D,width=5,flag=0, format="d"),"_T",formatC(tn,width=3,flag=0))))))
  Kostra_UC = data.frame(ID=Standorte$Stations_id, geoBreite = Standorte$geoBreite, geoLaenge = Standorte$geoLaenge, Kostra_UC_perDauer)

  if(Unsicherheit==TRUE) Kostra_hN_Schaetzng = list(Kostra_HN = Kostra_HN, Kostra_UC = Kostra_UC)
  if(Unsicherheit==FALSE) Kostra_hN_Schaetzng = Kostra_HN
  return(Kostra_hN_Schaetzng)
}
