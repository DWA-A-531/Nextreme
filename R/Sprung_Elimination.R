#' Sprungelimination
#' @param Serie numeric vector, Vektor der Serienwerte einer gegebenen Dauerstufe
#' @param Sensor character vector, Vektor der Sensorangaben mit identischer Laenge wie Serie
#' @param ZielSensor character, Sensor, auf dessen Serienmittelwert die Serienwerte angehoben oder abgesenkt werden sollen
#' @details
#' Das Verfahren ermittelt die sensorspezifischen Mittelwerte der Serienwerte, subtrahiert diese von den Serienwerten und addiert dann den Serienwertmittelwert des ZielSensors.
#' Ist der Zielsensor nicht in Sensor enthalten, dann wird der Eingabevektor wieder zurueckgeliefert.
#' @return data.frame mit den Spalten SenorZ und SerieNeu, Anzahl der Reihen entspricht Laenge von Serie. SensorZ ist der ZielSensor, SerieNeu die auf den ZielSensor angehobenen Serienwerte.
#' @export
#' @examples n1=50;n2=50;m1=10;m2=20
#' @examples # synthetische Daten, ein Sensorwechsel ab dem 51. Wert
#' @examples Sensor = c(rep("analog",n1),rep("digital",n2)); Wert = c(rnorm(n1,m1),rnorm(n2,m2))
#' @examples plot(Wert,Sprung_Elimination(Wert,Sensor)$SerieNeu, xlab="Original", ylab="Korrektur")
Sprung_Elimination=function(Serie,
                           Sensor,
                           ZielSensor=Sensor[length(Sensor)]){

  Sensor=factor(Sensor)
  LEV=levels(Sensor)
  if(length(LEV)==1 | !(ZielSensor%in%LEV))return(data.frame(SensorZ=Sensor,SerieNeu=Serie))
  mo = stats::lm(Serie~factor(Sensor))
  SensorZ = rep(ZielSensor,length(Sensor))
  PR0 = stats::predict(mo)
  PR1 = stats::predict(mo,new=data.frame(Sensor=SensorZ))
  # Bedingung fuer die Aufnahme von Stationen, bei denen der Mittelwert des analogen PR0 groeßer ist als der Mittelwert des digitalen PR1
  if(mean(mo$model[mo$model[,2]=="analog",]$Serie, na.rm=T) > mean(mo$model[mo$model[,2]=="digital",]$Serie, na.rm=T)){
    SerieNeu=Serie
    #print("Sprungkorrektur: Analog > Digital, keine Korrektur!")
  } else {
    SerieNeu=Serie-PR0+PR1 # sensorspez. mittelwert abziehen, zielsensor-mittelwert aufaddieren
    #print("Sprungkorrektur durchgefuehrt!")
  }
  data.frame(SensorZ,SerieNeu)
}
