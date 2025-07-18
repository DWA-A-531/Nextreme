% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Kostra2020_Parameter.R
\name{Kostra2020_Parameter}
\alias{Kostra2020_Parameter}
\title{Extraktion der KOSTRA-DWD-2020 Parameter fuer bestimmte Standorte}
\usage{
Kostra2020_Parameter(Standorte, Temp_Pfad = "./")
}
\arguments{
\item{Standorte}{Ein Datenframe mit den Standorten, aus denen die KOSTRA-DWD-2020-Daten extrahiert werden sollen. Der Dataframe sollte drei Spalten haben: die Standort-ID - 'Stations_id', die Laengenkoordinaten - 'geoLaenge' und die Breitenkoordinaten 'geoBreite'. Die Koordinaten sollten in der crs(„+proj=longlat +datum=WGS84“) sein!}

\item{Temp_Pfad}{Ein Ordner-Pfad, in den die KOSTRA-Daten heruntergeladen werden koennen.}
}
\value{
Es wird eine Tabelle im Datenframe-Format mit den Koordinaten und den entsprechenden geschaetzten Kostra-DWD-2020 Parametern (in jeder Spalte angegeben) fuer die angegebenen Standorte (in jeder Reihe angegeben) zurueckgegeben. Fuer Standorte, die ausserhalb der KOSTRA-DWD-2020 Bereiche liegen, werden NA-Werte zurueckgegeben.
}
\description{
Die geschaetzten Parameter von KOSTRA-DWD-2020 werden aus dem DWD-Climate Data Center fuer bestimmte Standorte ausgelesen. Als Eingabe werden die Koordinaten der Standorte benoetigt.
}
\details{
R-Funktion, die die von KOSTRA-DWD-2020 geschaetzten Parameter in eine bestimmte oder temporaere Datei (Folder) herunterlaedt und die entsprechenden Parameterwerte (Theta - Koutsoyiannis erster Parameter, Eta - Koutsoyiannis zweiter Parameter, Mu - GEV Lokationsparameter, Sigma - GEV Skalenparameter und Gamma - GEV Formparameter) fuer die gewuenschten Positionen liest und zurueckgibt.
}
\examples{
Station = data.frame(Stations_id = 01684, geoBreite = 51.1621, geoLaenge = 14.9506)
kostraParameter = Kostra2020_Parameter(Standorte = Station )
}
