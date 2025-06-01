## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  fig.width = 6,
  fig.height = 6,
  collapse = TRUE,
  comment = "#>"
)

## ----eval=F-------------------------------------------------------------------
# install.packages("lmomco")
# install.packages("lubridate")
# install.packages("terra")
# install.packages("evd")
# install.packages("scales")

## ----eval=F-------------------------------------------------------------------
# install.packages("pak")
# pak::pak("DWA-A-531/Nextreme")

## -----------------------------------------------------------------------------
library("lubridate")
library("lmomco")
library("terra")
library("Nextreme")

## -----------------------------------------------------------------------------
data("Regendaten_01684")
# die ersten 10 Zeilen der Regendaten_01684 zeigen
head(Regendaten_01684, 10)
# die letzten 10 Zeilen der Regendaten_01684 zeigen
tail(Regendaten_01684, 10)

## -----------------------------------------------------------------------------
# Ueberpruefung der Struktur der 5-Minuten-Zeitreihe
str(Regendaten_01684)
# Ueberpruefung, ob die Datumdaten kontinuierlich sind
unique(diff(Regendaten_01684$Datum))
# Ueberpruefung des Wertebereichs der  RH
range(Regendaten_01684$RH, na.rm=T)

## -----------------------------------------------------------------------------
Goerlitz_maxIntSerie = jaehrliche_maxSerie(Regendaten_01684, 
                                        Dauern = c(5, 10,15,30,60,120,360,720,
                                                   1440, 2880,4320, 10080), 
                                        DSDmin=240, Intervall = 5,
                                        SerieTyp = "INT")
print(head(Goerlitz_maxIntSerie,10))

## -----------------------------------------------------------------------------
Goerlitz_maxIntSerie    = Intervallkorrektur(Goerlitz_maxIntSerie, Intervall = 5)
print(head(Goerlitz_maxIntSerie,10))

## -----------------------------------------------------------------------------
WechselDatum = as.Date("1991-01-01", format=c("%Y-%m-%d"))
Goerlitz_maxSerie_korrigiert    = Sprung_Korrektur(Goerlitz_maxIntSerie,
                                                   WechselDatum)
# Die Differenz vor und nach der Funktion sprungKorrektur() ueberpruefen.
print(head(round(Goerlitz_maxSerie_korrigiert-Goerlitz_maxSerie,0),10))

## -----------------------------------------------------------------------------
N_pars      = Parameter_Schaetzung(Goerlitz_maxIntSerie, 
                                   Dauern = c(5, 10, 15,30,60,120,360,720,1440, 
                                              2880,4320, 10080), 
                                   methGEV="GEV", formTyp = "FIX", Gamma = -0.1,
                                   SerieTyp="INT")
print(N_pars)

## -----------------------------------------------------------------------------
H_quas      = Quantil_Schaetzung(N_pars, 
                                 Dauern = c(5, 10, 15,30,60,120,360,720,1440, 
                                            2880, 4320, 10080),
                                 Tn =c(2,5,10,20,50,100), 
                                 methGEV="GEV",SerieTyp = "VOL")
print(H_quas)

## -----------------------------------------------------------------------------
KI          = Unsicherheit_Schaetzung(Goerlitz_maxIntSerie,
                                  Tn =c(2,5,10,20,50,100),
                                  Dauern = c(5, 10, 15,30,60,120,360,720,1440,
                                             2880, 4320, 10080),
                                  methGEV="GEV",
                                  formTyp = "FIX",
                                  Gamma=-0.1,
                                  nBoots = 100,
                                  rSeed = 1232,
                                  SerieTyp = "VOL",
                                  Konfidenzgrenzen  = c(0.025,0.975))
# Zugriff auf die Parameterinformationen
PAR_KI      = KI$PAR_INFO
# Zugriff auf die Quantils Informationen 
HN_KI       = KI$QUA_INFO

## ----echo=TRUE, eval=T--------------------------------------------------------
Dauern = c(5, 10, 15,30,60,120,360,720,1440, 2880, 4320, 10080)
Tn_Farbe    = rev(hcl.colors(6, palette = "viridis"))
plot(Dauern, H_quas["2",], type="l", lwd=2, lty=1, log="xy", col=Tn_Farbe[1],
     ylab="hN [mm]", ylim=range(H_quas), xlab="Dauer [min]", 
     main = "Station 01684")
lines(Dauern, H_quas["5",],  lwd=2, col=Tn_Farbe[2])
lines(Dauern, H_quas["10",], lwd=2, col=Tn_Farbe[3])
lines(Dauern, H_quas["20",], lwd=2, col=Tn_Farbe[4])
lines(Dauern, H_quas["50",], lwd=2, col=Tn_Farbe[5])
lines(Dauern, H_quas["100",],lwd=2, col=Tn_Farbe[6])
legend("bottomright", legend = c(2,5,10,20,50,100),lty=1, lwd=3, col=Tn_Farbe,
       cex=0.8, title="Tn", horiz=T, bty="n")

## ----echo=TRUE, eval=TRUE-----------------------------------------------------
Dauern = c(5, 10, 15,30,60,120,360,720,1440, 2880, 4320, 10080)
plot(Dauern, H_quas["100",], type="l", lwd=2, lty=2, log="xy",
     ylim=range(HN_KI$`97.5%`["100",], HN_KI$`2.5%`["100",]), col="red",
     ylab="hN [mm]", xlab="Dauer [min]", main = "Station 01684")
polygon(c(Dauern, rev(Dauern)), c(HN_KI$`2.5%`["100",],
        rev(HN_KI$`97.5%`["100",])), col="royalblue", border=NA)
lines(Dauern, HN_KI$Mittelwert["100",], type="l", col="royalblue4", lwd=2)
lines(Dauern, H_quas["100",], type="l", col="red", lwd=2, lty=2)
legend("topleft", c("95%KI", "Mittelwert", "lokale Schaetzung"),
       col=c("royalblue", "royalblue4", "red"), lty=c(1, 1, 2), lwd=c(10,2,2),
       title = "Legende", bty="n")

## ----echo=TRUE, eval=TRUE-----------------------------------------------------
KI = round(HN_KI$rel.Unsicherheit,2)
# Option 1 
barplot(as.matrix(KI), col=Tn_Farbe, ylab="KI [%]",  xlab="Dauer [min]",
        main = "Station 01684")
legend("top", legend = c(2,5,10,20,50,100), fill=Tn_Farbe, cex=0.8, 
       title="Tn", horiz=T, bty="n")
# Option 2 
barplot(as.matrix(KI),col=Tn_Farbe, beside=T, ylim=c(0,50), ylab="KI [%]",
        xlab="Dauer [min]", main = "Station 01684")
legend("top", legend = c(2,5,10,20,50,100), fill=Tn_Farbe, cex=0.8, 
       title="Tn", horiz=T, bty="n")
# Option 3 
Dauer_Farbe = hcl.colors(dim(KI)[2], "blues", rev = T)
barplot(as.matrix(t(KI)),col=Dauer_Farbe, beside=T, ylim=c(0,60), 
        ylab="KI [%]", xlab="Tn", main = "Station 01684")
legend("top", legend = Dauern, fill=Dauer_Farbe, cex=0.8, title="Dauer (min)"
       ,  bty="n", ncol=6)


## ----echo=TRUE, eval=TRUE-----------------------------------------------------
Ta_Ereignis = Tn_Schaetzung(N_pars, Dauern = 360, hN= 58.6, methGEV="GEV")
print(Ta_Ereignis)

## -----------------------------------------------------------------------------
Tn = c(2,5,10,20,50,100)
Dauern = c(5, 10, 15, 30, 60, 120, 360, 720, 1440, 2880, 4320, 10080)
Station = data.frame(Stations_id = 01684, geoBreite = 51.1621, 
                     geoLaenge = 14.9506)
H_quas_Kostra = Kostra2020_hN_Schaetzung(Standorte = Station, Dauern=Dauern,
                                         Tn =Tn, Temp_Pfad = "./",
                                         Unsicherheit=T)
# Zugang zu den Regenhoehen und formatiert sie so, dass sie der zuvor geschaetzten
# Tabelle H_quas entsprechen. 
Hn_Kostra = H_quas_Kostra$Kostra_HN
print(Hn_Kostra[1,1:5])
Hn_Kostra = matrix(unlist(Hn_Kostra[1,-(1:3)]), nrow = 6, ncol = 12, byrow=F) 
Hn_Kostra = as.data.frame(Hn_Kostra)
rownames(Hn_Kostra) = Tn
names(Hn_Kostra) = Dauern

# Zugang zu den Unsicherheiten und formatiert sie so, dass sie der zuvor  
# geschaetzten Tabelle H_quas entsprechen. 
UC_Kostra = H_quas_Kostra$Kostra_UC
print(UC_Kostra[1,1:5])
UC_Kostra = matrix(unlist(UC_Kostra[1,-(1:3)]), nrow = 6, ncol = 12, byrow=F) 
UC_Kostra = as.data.frame(UC_Kostra)
rownames(UC_Kostra) = Tn
names(UC_Kostra) = Dauern

# Da die KOSTRA-DWD-2020 Unsicherheiten in Prozent angegeben sind, kann die
# obere und untere Konfidenzgrenze wie folgt berechnet werden:
Hn_Kostra_Ku =  Hn_Kostra - round(UC_Kostra*Hn_Kostra/100,2) # untere Grenze
Hn_Kostra_Ko =  Hn_Kostra + round(UC_Kostra*Hn_Kostra/100,2) # obere Grenze

## ----echo=TRUE, eval=TRUE-----------------------------------------------------
Dauern = c(5, 10, 15,30,60,120,360,720,1440, 2880, 4320, 10080)
plot(Dauern, H_quas["100",], type="l", lwd=2, lty=2, log="xy",
     ylim=range(HN_KI$`97.5%`["100",], HN_KI$`2.5%`["100",], 
                Hn_Kostra_Ku["100",], Hn_Kostra_Ko["100",]), col="red",
     ylab="hN [mm]", xlab="Dauer [min]", main = "Station 01684, Tn=100")
polygon(c(Dauern, rev(Dauern)), c(Hn_Kostra_Ku["100",],
        rev(Hn_Kostra_Ko["100",])),col="lightgrey", border=NA)
polygon(c(Dauern, rev(Dauern)), c(HN_KI$`2.5%`["100",],
        rev(HN_KI$`97.5%`["100",])),col="royalblue", border=NA)
lines(Dauern, HN_KI$Mittelwert["100",], type="l", col="royalblue4", lwd=2)
lines(Dauern, H_quas["100",], type="l", col="red", lwd=2, lty=2)
lines(Dauern, Hn_Kostra["100",], type="l", col="black", lwd=2, lty=2)

legend("bottomright", c("lokale - 95%KI", "lokale Mittelwert", "lokale Hn",
                    "Unsicherheitbereich KOSTRA-DWD-2020",  "KOSTRA-DWD-2020"), 
       col=c("royalblue", "royalblue4", "red", "lightgrey", "black"), 
       lty=c(1, 1, 2, 1, 2), lwd=c(10,2,2, 10, 2), title = "Legende", bty="n")

## ----echo=TRUE, eval=TRUE-----------------------------------------------------
plot(Tn, H_quas[,"60"], type="l", lwd=2, lty=2, log="x", ylim=range(15, 70), 
     col="red",ylab="hN [mm]", xlab="Tn", main = "Station 01684, D=60min")
polygon(c(Tn, rev(Tn)), c(Hn_Kostra_Ku[,"60"], rev(Hn_Kostra_Ko[,"60"])),
        col="lightgrey", border=NA)
polygon(c(Tn, rev(Tn)), c(HN_KI$`2.5%`[,"60"], rev(HN_KI$`97.5%`[,"60"])),
        col="royalblue", border=NA)
lines(Tn, HN_KI$Mittelwert[,"60"], type="l", col="royalblue4", lwd=2)
lines(Tn, H_quas[,"60"], type="l", col="red", lwd=2, lty=2)
lines(Tn, Hn_Kostra[,"60"], type="l", col="black", lwd=2, lty=2)
legend("topleft", c("lokale - 95%KI", "lokale Mittelwert", "lokale Hn",
                    "Unsicherheitbereich KOSTRA-DWD-2020",  "KOSTRA-DWD-2020"), 
       col=c("royalblue", "royalblue4", "red", "lightgrey", "black"), 
       lty=c(1, 1, 2, 1, 2), lwd=c(10,2,2, 10, 2), title = "Legende", bty="n")

## ----echo=TRUE, eval=TRUE-----------------------------------------------------
kostraParameter    = Kostra2020_Parameter(Standorte = Station )
Hn_Tn_Kostra       = Tn_Schaetzung(kostraParameter, Dauern = 360, 
                            hN   = 58.6, # values have to be in mm
                            methGEV="GEV")
print(Hn_Tn_Kostra)


