# Nextreme 1.2.1

28.05.2025  - implementiert von Bora Shehu
			 1. jaehrliche_maxSerie.R (vorgeschlagen am 18.03.2024 von Bora Shehu und Angelika Palarz)
			 In der neuen Version wurde die Berechnung der Jahresmaxima komplett ueberarbeitet, um folgende Punkte zu beruecksichtigen: 
			 a) fehlende Werte - so wird kein Fehler mehr angezeigt, wenn in der Zeitreihe fehlende Werte vorhanden sind
			 b) jaehrliche Minimun an verfuegbaren Messungen - Zwei Bedingungen sind eingefuegt:
			     - zwischen Maerz und Oktober sollen mindestens 172 Tage mit vollstaendigen Beobachtungen
			     - zwischen April und September sollen midnestens 127 Tage mit vollstaendigen Beobachtungen
			 c) unabhaengigkeit von Ereignissen - es wurde eine Bedingung fuer Maximalwerte (mit einer Dauer von mehr als 4 Stunden) hinzugefuegt, die am Ende des Kalenderjahres auftreten. 
			     - Wenn die Bedingung erfuellt ist, wird die Fortsetzung des Ereignisses im naechsten Jahr auf 0 gesetzt. 
			     - Die Fortsetzung des Ereignisses basiert auf einer Trockenheitsdauer (DSDmin) von 4 Stunden. 

			 2. Sprung_Elimination.R (vorgeschlagen am 16.12.2024 von Thomas Junghaenel und Winfrid Willems)
			 In der neuen Version wird eine Bedingung eingeführt, um Stationen einzubeziehen, 
			 bei denen der Mittelwert von analogem PR0 groeßer ist als der Mittelwert von digitalem PR1
			 
		   	 3. Sprung_Korrektur.R (vorgeschlagen am 16.12.2024 von Thomas Junghaenel und Winfrid Willems)
			 In der neuen Version wird geprueft, ob in Spalten mit einer Dauer <= 30 Minuten ein "Sprung" vorliegt. 
			 Falls mindestens ein "Sprung" festgestellt wird, werden alle AMS-Werte für Dauerstufen <= 30 Minuten entsprechend korrigiert. 
			 
			 4. an alle .R funtionen (vorgeschlagen am 19.09.2024 von Thomas Junghaenel)
			 In der neuen Version wird jede importierte Funktion anderer Packages (z. B. „lubridate”, „terra”) in das folgende Format aktualisiert:
			 Beispiel: lubridate::year()
			 Die Zeile require(lubridate) faellt somit weg.
			 
			 5. Die Datei ReadMe.md wurde hinzugefuegt (vorgeschlagen am 19.09.2024 von Thomas Junghaenel) und enthaelt:
			 Paketbeschreibung
			 Installationsanweisungen
			 Grundlegende Anwendungsbeispiele
			
			 6. Die Datei News.md wurde hinzugefuegt (vorgeschlagen am 23.05.2025 von Bora Shehu).
			 Enthaelt Hauptaenderung ab erste Release Nextreme_1.1.0
			 
			 7. Kostra_hN_Schaetzung.R und Kostra202_Parameter.R (vorgeschlagen am 23.05.2025 von Bora Shehu)
			Die Funktionen aus dem Paket rdwd zum Lesen der Kostra asc-Dateien wurden ersetzt. Das Paket rdwd wird nicht mehr verwendet.

# Nextreme 1.2.0	 
07.10.2024 - implementiert von Bora Shehu
			 1. kw_koupar1.R und kw_koupar2.R (vorgeschlagen am 07.10.2024 von Jennifer Oestermoeller)
			 Die Berechnung des Rangs fuer die Schaetzung des Koustoyiannis-Parameters wurde geaendert:
			 mittlere.Rang  = mean(unlist(sapply(Inten.jeDauer, function(k) mean(which(alle.sortiert==k)))))
			 
			 2. Kostra_hN_Schaetzung.R und Kostra202_Parameter.R (vorgeschlagen am 17.07.2024 von Thomas Junghaenel, Jennifer Oestermoeller und Angelika Palarz)
			 Die Projektion der KOSTRA-Daten wurde aktualisiert:
			 Kostra_CRS = "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs +type=crs"

