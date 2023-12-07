---
editor_options: 
  markdown: 
    wrap: 72
---

# Kinoklub

Script zur Abrechnung für den Kinoklub TaB

## Datensätze

Die Datensätze können von <https://www.advance-ticket.ch/admin>
heruntergeladen werden und sind unter dem Verzeichnis " /input/advance
tickets" abzuspeichern.

-   Eintritte 02.12.23.txt\
    Copy paste von html für jede Filmvorführung und Speichern unter
    "Eintritt xx.xx.xx.txt"

-   Kiosk 02.12.23.txt\
    Copy paste von html für jede Filmvorführung und Speichern unter
    "Kiosk xx.xx.xx.txt"

-   Shows.txt\
    Copy paste von html für die gewünschte Abrechnungsperiode und
    Speichern unter "Shows.txt"

## Konfigurations Dateien

Im Verzeichniss /input kann mit Hilfe von Excelfiles folgendes definiert
werden:

-   Einkauf Kiosk 01.02.23\
    Die Einkaufspreise für die Kioskverkäufe müssen gepflegt werden.
    Ändern sich die Einkaufspreise so muss das File abgepasst werden und
    und mit dem neuen Gültigkeitsdauer abgespeichert werden. Die älteren
    Dateien dürfen nicht gelöscht werden .

-   Spezialpreisekiosk.xlsx\
    Definition der Sonderangebote

-   Verleiherabgaben\
    Die Abrechnungen werden erstellt wenn das Script "Erstelle
    Abrechnung.R" ausgeführt wird. Alle erstellten files findest du im
    Verzeichniss /output das zur Laufzeit erstellt wird.

# How to use

1.  Download R und R Studio <https://posit.co/download/rstudio-desktop/>

2.  Download git <https://git-scm.com/downloads>

3.  Navigiere zu einem beliebigen Order und öffne ein Terminal

4.  Befehl: git clone <https://github.com/slvwagner/Kinoclub>

5.  Verknüpfe ".R"-files mit Rstudio

6.  Starte Rstudio mit dem Anklicken der Datei "Erstelle Abrechnung.R"
    Oder Starte Rstudio und finde mit getwd() den Standard Pfad und
    installiere mit "git clone ..." das Verzeichnis. mit setwd() kann
    auch gearbeitet werden.
