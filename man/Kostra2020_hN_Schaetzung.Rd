% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Kostra2020_hN_Schaetzung.R
\name{Kostra2020_hN_Schaetzung}
\alias{Kostra2020_hN_Schaetzung}
\title{Kostra-DWD-2020 Regenhoehe (mm/Dauer) fuer bestimmte Standorte, Dauern und Wiederkehrintervalle}
\usage{
Kostra2020_hN_Schaetzung(
  Standorte,
  Dauern = c(5, 10, 15, 30, 60, 120, 360, 720, 1440, 2880, 4320, 10080),
  Tn = c(1, 5, 10, 20, 50, 100),
  Temp_Pfad = "./",
  Unsicherheit = TRUE
)
}
\arguments{
\item{Standorte}{Ein data.frame mit den Standorten, aus denen die KOSTRA-Daten extrahiert werden sollen. Der Dataframe sollte drei Spalten haben: die Standort-ID - 'Stations_id', die Laengenkoordinaten - 'geoLaenge' und die Breitenkoordinaten 'geoBreite'. Die Koordinaten sollten in der crs(„+proj=longlat +datum=WGS84“) sein!}

\item{Dauern}{die Regendauer(n), fuer die die Regenhoehe ausgelesen werden soll. Die Dauer sollte in Minuten angegeben werden!}

\item{Tn}{die Wiederkehrintervalle, fuer die die Regenhoehe ausgelesen werden soll. Für KOSTRA-DWD-2020 (Rasterdaten) sind die Wiederkehrintervalle als 1, 2, 3, 5, 10, 20, 30, 50 und 100 fest definiert. Die Wiederkehrintervalle sollten in Jahren angegeben werden!}

\item{Temp_Pfad}{Ein Ordner-Pfad, in den die KOSTRA-Daten heruntergeladen werden koennen.}

\item{Unsicherheit}{TRUE oder FALSE Bestimmt, ob auch die Unsicherheitsabschaetzung gelesen werden soll. TRUE - die Regenhoehen und die Unsicherheitsabschaetzung werden gelesen und zurueckgegeben, FALSE - nur die Regenhoehen werden gelesen und zurueckgegeben. Standardwert ist TRUE.}
}
\value{
Es wird eine Tabelle im Dataframe-Format mit den Koordinaten und den entsprechenden geschaetzten KOSTRA-DWD-2020-Regenhoehen fuer die angegebenen Standorte (in jeder Zeile angegeben), die Regendauer und die Wiederkehrintervalle (in jeder Spalte angegeben) zurueckgegeben.  Falls auch die Unsicherheit gewuenscht ist, wird eine Liste mit zwei Dataframes zurueckgegeben (eine fuer die Regenhoehe - Kostra_HN und eine fuer die Unsicherheit - Kostra_UC). Fuer Standorte ausserhalb der KOSTRA-DWD-2020 Bereiche werden NA-Werte zurueckgegeben.
}
\description{
Die geschaetzten Regenhoehe (mm/Dauer) von KOSTRA-DWD-2020 werden aus dem DWD-Climate Data Center fuer bestimmte Standorte, Regendauern und Wiederkehrintervalle ausgelesen. Alternativ kann die Unsicherheitsabschaetzung fuer jeden Standort, jede Regendauern und Wiederkehrintervalle ausgelesen werden.
}
\details{
R-Funktion, die die von KOSTRA-DWD-2020 geschaetzten Regenhoehen in eine bestimmte oder temporaere Datei (Folder) herunterlaedt, die entsprechenden Regenhoehe- und Unsicherheitsabschaetzungen fuer die gewuenschten Standorte, Regendauern und Wiederkehrintervalle liest und zurueckgibt.
}
\examples{
Station = data.frame(Stations_id = 01684, geoBreite = 51.1621, geoLaenge = 14.9506)
Dauern = c(5,10,15)
Tn = c(50,100)
Kostra_Hn = Kostra2020_hN_Schaetzung(Station, Dauern, Tn, Unsicherheit=FALSE)
print(Kostra_Hn)
Kostra_Werte = Kostra2020_hN_Schaetzung(Station, Dauern, Tn, Unsicherheit=TRUE)
Kostra_Hn = Kostra_Werte$Kostra_HN
print(Kostra_Hn)
Kostra_UC = Kostra_Werte$Kostra_UC
print(Kostra_UC)
Kostra_UK_HN = Kostra_Hn[,-(1:3)] - Kostra_UC[,-(1:3)]*Kostra_Hn[,-(1:3)]/100
Kostra_OK_HN = Kostra_Hn[,-(1:3)] + Kostra_UC[,-(1:3)]*Kostra_Hn[,-(1:3)]/100
}
