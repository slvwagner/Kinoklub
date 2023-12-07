---
editor_options: 
  markdown: 
    wrap: 72
---

# Kinoclub

Abrechnung für den Kinoklub Die Datensätze können von
<https://www.advance-ticket.ch/admin> heruntergeladen werden und sind
unter dem Verzeichnis " /input/advance tickets" abzuspeichern.

Im Verzeichniss /input kann mit hilfe von Excelfiles folgendes definiert
werden:

-   Einkaufspreise Kiosk

-   Spezialpreise Kiosk also sonderangebote

-   Verleiherabgaben\
    Die Abrechnungen werden erstellt wenn das Script "Erstelle
    Abrechnung.R" ausgeführt wird. Alle erstellten files findest du im
    Verzeichniss /output das zur Laufzeit erstellt wird.

# How to use

1.   Download R und R Studio
    <https://posit.co/download/rstudio-desktop/>

2.  Download git <https://git-scm.com/downloads>

3.  Navigiere zu einem beliebigen Order und öffne ein Terminal

4.  Befehl: git clone <https://github.com/slvwagner/Kinoclub>

5.  Verknüpfe ".R"-files mit Rstudio

6.  Starte Rstudio mit dem Anklicken der Datei "Erstelle Abrechnung.R"
