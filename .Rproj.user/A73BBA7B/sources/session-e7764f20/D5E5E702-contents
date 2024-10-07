#' Sprungelimination
#' @param Serie numeric vector, Vektor der Serienwerte einer gegebenen Dauerstufe
#' @param Sensor character vector, Vektor der Sensorangaben mit identischer Laenge wie Serie
#' @param ZielSensor character, Sensor, auf dessen Serienmittelwert die Serienwerte angehoben oder abgesenkt werden sollen
#' @details
#' Das Verfahren ermittelt die sensorspezifischen Mittelwerte der Serienwerte, subtrahiert diese von den Serienwerten und addiert dann den Serienwertmittelwert des ZielSensors.
#' Ist der Zielsensor nicht in Sensor enthalten, dann wird der Eingabevektor wieder zurueckgeliefert.
#' @return data.frame mit den Spalten SenorZ und SerieNeu, Anzahl der Reihen entspricht Laenge von Serie. SensorZ ist der ZielSensor, SerieNeu die auf den ZielSensor angehobenen Serienwerte.
#' @export
#' @examples library(data.table)
#' @examples n1=50;n2=50;m1=10;m2=20
#' @examples Sensor = c(rep("analog",n1),rep("digital",n2)); Wert = c(rnorm(n1,m1),rnorm(n2,m2))                                 # synthetische Daten, ein Sensorwechsel ab dem 51. Wert
#' @examples par(mfrow=c(2,1))
#' @examples data.table(Wert,Sprung_Elimination(Wert,Sensor))[,{plot(Wert,pch=1,cex=2);points(SerieNeu,col=2,pch=15)}]
#' @examples n1=30;n2=40;n3=30;m1=30;m2=20;m3=40
#' @examples Sensor = c(rep("analog1",n1),rep("analog2",n2),rep("digital",n3)); Wert = c(rnorm(n1,m1),rnorm(n2,m2),rnorm(n3,m3)) # synthetische Daten, zwei Sensorwechsel
#' @examples data.table(Wert,Sprung_Elimination(Wert,Sensor))[,{plot(Wert,pch=1,cex=2);points(SerieNeu,col=2,pch=15)}]
Sprung_Elimination=function(Serie,
                           Sensor,
                           ZielSensor=Sensor[length(Sensor)]){

  Sensor=factor(Sensor)
  LEV=levels(Sensor)
  if(length(LEV)==1 | !(ZielSensor%in%LEV))return(data.frame(SensorZ=Sensor,SerieNeu=Serie))
  mo = lm(Serie~factor(Sensor))
  SensorZ = rep(ZielSensor,length(Sensor))
  PR0 = predict(mo)
  PR1 = predict(mo,new=data.frame(Sensor=SensorZ))
  SerieNeu=Serie-PR0+PR1 # sensorspez. mittelwert abziehen, zielsensor-mittelwert aufaddieren
  data.frame(SensorZ,SerieNeu)
}
