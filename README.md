
# Kinoklub

Script zur Abrechnung für den Kinoklub TaB

## Datensätze

Die Datensätze können von <https://www.advance-ticket.ch/admin> heruntergeladen werden und sind unter dem Verzeichnis
**/input/advance tickets** abzuspeichern.

-   Eintritte 02.12.23.txt
    Copy paste von html für jede Filmvorführung und Speichern unter "Eintritt xx.xx.xx.txt"

-   Kiosk 02.12.23.txt
    Copy paste von html für jede Filmvorführung und Speichern unter "Kiosk xx.xx.xx.txt"

-   Shows.txt
    Copy paste von html für die gewünschte Abrechnungsperiode und Speichern unter "Shows.txt"

## Konfigurations Dateien

Im Verzeichniss **/input** kann mit Hilfe von Excelfiles folgendes definiert werden:

-   Einkauf Kiosk 01.02.23\
    Die Einkaufspreise für die Kioskverkäufe müssen gepflegt werden. Ändern sich die Einkaufspreise so muss das File
    abgepasst werden und und mit dem Datum ab wann diese Gültig sind abgespeichert werden. Die älteren Dateien dürfen
    nicht gelöscht werden .

-   Spezialpreisekiosk.xlsx
    Definition der Sonderangebote

-   Verleiherabgaben
    Die Abrechnungen werden erstellt wenn das Script "Erstelle Abrechnung.R" ausgeführt wird. Alle erstellten files
    findest Du im Verzeichniss /output das zur Laufzeit erstellt wird.

## Installation

1.  Download and install R 
    <https://cran.r-project.org/bin/windows/base/>

2.  Download and install Rsudio 
    <https://posit.co/download/rstudio-desktop/>

3.  Download git:
    <https://git-scm.com/downloads>

5.  Kionklub Scripts download:
    Navigate to folder you would like to install the Scripts
    ```
    git clone https://github.com/slvwagner/Kinoklub
    ```

6.  Start Rstudio from the Kinoklub folder.
7.  Install the needed packages in the R Terminal
    ```
    install.packages("tidyverse")
    ```
8.  Run the Script:
    ```
    source("Erstelle Abrechnung.R")
    ```
