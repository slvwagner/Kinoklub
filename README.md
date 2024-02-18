
# Kinoklub

Script zur Abrechnung für den Kinoklub TaB

## Datensätze

Die Datensätze können von <https://www.advance-ticket.ch/admin> heruntergeladen werden und sind unter dem Verzeichnis
**/input/advance tickets** abzuspeichern.

-   "Eintritte 02.12.23.txt" \
    Copy paste von html für jede Vorführung und bitte speichern unter "input/advance tickets/Eintritt xx.xx.xx.txt"
    
-   "Kiosk 02.12.23.txt"" \
    Copy paste von html für jede Vorführung und Speichern unter "input/advance tickets/Kiosk xx.xx.xx.txt". \
    Im Menu auf "DecompteCaisse" <https://www.advance-ticket.ch/decomptecaisse?lang=de> navigieren.   
    Spalte 1 Das Datum muss gewählt werden, Spalte 2 "reinach", Splate 3 "Atelierkino Kasse" und Spalte 4 "..." eingestellt werden. \
    ![Kiosk](doc/Kiosk.png)
-   "Shows.txt" \
    Copy paste von html für die gewünschte Abrechnungsperiode. Bitte speichern unter "input/advance tickets/Shows.txt" \
    Im Menu auf "Shows" <https://www.advance-ticket.ch/shows?lang=de> navigieren. \
    Spalte 1 startdatum wählen 1.1.20xx, Spalte 2 Enddatum wählen 31.12.20xx
    ![Shows](doc/shows.png)
    

## Konfigurations Dateien

Im Verzeichniss **/input** kann mit Hilfe von Excelfiles folgendes definiert werden:

-   "Einkauf Kiosk xx.xx.xx.xlsx" \
    Die Einkaufspreise für die Kioskverkäufe müssen gepflegt werden. Ändern sich die Einkaufspreise so muss ein neues File erstellt werden. 
    Die älteren Dateien dürfen nicht gelöscht werden.
-   "Spezialpreisekiosk.xlsx" \
    Definition der Sonderangebote
-   "Verleiherabgaben.xlsx" \
    Definition der Umsatzabgaben die der Filmverleiher erhält.
-   "Einnahmen und Ausgaben.xlsx" \
    Einnahmen und Ausgaben aus Rechnungen und Gutschrifften. 

## Ausgabe 
Alle Dateien die erzeugt wurden finden sich im **/output** Verzeichniss.

- Für jede Vorführung respektive Datum wird ein Abrechnung erstellt.
- Die Erfolgsrechnung wird über alle Datensätze erstellt.
- Zusätzliche Daten sind in der Statistik ersichtlich.
- Alle verwendeten Datensätze werden in ein Excelfile abgespeichert.

## Installation

1.  Download and install R \
    <https://cran.r-project.org/bin/windows/base/>
2.  Download and install Rsudio \
    <https://posit.co/download/rstudio-desktop/>
3.  Download git: \
    <https://git-scm.com/downloads>
5.  Kionklub Scripts download:
    Navigate to folder you would like to install the Scripts
    ```
    git clone https://github.com/slvwagner/Kinoklub
    ```
6.  Start Rstudio from the Kinoklub folder.
7.  Install the needed packages in the R Terminal
    ```
    install.packages("rmarkdown")
    install.packages("tidyverse")
    install.packages("rebus")
    install.packages("openxlsx")
    install.packages("flextable")
    ```
8.  Run the Script:
    ```
    source("Erstelle Abrechnung.R")
    ```
