---
title: Dokumentation
output: 
  html_document:
    css: source/Kinoklub_dark.css
---
# Inhaltsverzeichnis<a name="Inhaltsverzeichnis"></a>
* [Kinoklub](#A_Kinoklub)
    + [Installation](#A_Installation)
    + [Datensätze](#A_Datensätze)
    + [Script Konfiguration](#A_Script Konfiguration)
        + [Abrechnung für Filmvorführungen](#A_Abrechnung für Filmvorführungen)
        + [Inhaltsverzeichnisse](#A_Inhaltsverzeichnisse)
        + [Mehrwertsteuersatz](#A_Mehrwertsteuersatz)
        + [Platzkategorien ohne Umsatz die dennoch abgerechnet werden müssen.](#A_Platzkategorien ohne Umsatz die dennoch abgerechnet werden müssen.)
        + [Ausgabeformate](#A_Ausgabeformate)
    + [Excel Dateien](#A_Excel Dateien)
        + [Einkaufspreise](#A_Einkaufspreise)
        + [Spezialpreise Kiosk](#A_Spezialpreise Kiosk)
        + [Verleiherabgaben](#A_Verleiherabgaben)
        + [Einnahmen und Ausgaben](#A_Einnahmen und Ausgaben)
    + [Berichte](#A_Berichte)
        + [Abrechnung Filmvorführung](#A_Abrechnung Filmvorführung)
        + [Jahresabrechnungen](#A_Jahresabrechnungen)
    + [Ausgabe / Output](#A_Ausgabe / Output)
* [Berchnungsdokumentation](#A_Berchnungsdokumentation)
    + [Ablauf](#A_Ablauf)
    + [Abrechnung Filmvorführung](#A_Abrechnung Filmvorführung)



# Kinoklub<a name="A_Kinoklub"></a>
[Inhaltsverzeichnis](#Inhaltsverzeichnis)

Author: Florian Wagner \
\
Script zur Abrechnung für den Kinoklub TaB. 
Um die Abrechnung für den Kinoklub zu vereinfachen respektive zu automatisieren wurde dieser Script erstellt. \
Dieser Skrip kann mit folgendem Befehl ausgeführt werden:
```
source("Erstelle Abrechnung.R")
```
\
Bei Fehlern kann ein "Issue" in Github erfasst werden. \
\
Die Datei "README.md" und die Dokumentation wird automatisch erstellt. 
```
source("doc/create Readme and Docu.R")
```

Eine Änderung  muss deshalb in der Datei **"doc/README.Rmd"** vorgenommen werden.  

\newpage

## Installation<a name="A_Installation"></a>
[Inhaltsverzeichnis](#Inhaltsverzeichnis)


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

\newpage

## Datensätze<a name="A_Datensätze"></a>
[Inhaltsverzeichnis](#Inhaltsverzeichnis)


Die Datensätze können von <https://www.advance-ticket.ch/admin> heruntergeladen werden und sind unter dem Verzeichnis
**.../Kinoklub/input/advance tickets/** abzuspeichern.

-   "Eintritte 02.12.23.txt" \
    Copy paste von html für jede Vorführung und bitte speichern unter "input/advance tickets/Eintritt xx.xx.xx.txt"
    Es muss die Kalenderwoche sowie der Film ausgewählt werden. \
    ![Eintritt](doc/eintritt.png)
-   "Kiosk 02.12.23.txt"" \
    Copy paste von html für jede Vorführung und Speichern unter "input/advance tickets/Kiosk xx.xx.xx.txt". \
    Im Menu auf "DecompteCaisse" <https://www.advance-ticket.ch/decomptecaisse?lang=de> navigieren.   
    Spalte 1 Das Datum muss gewählt werden, Spalte 2 "reinach", Splate 3 "Atelierkino Kasse" und Spalte 4 "..." eingestellt werden. \
    ![Kiosk](doc/Kiosk.png)
-   "Shows.txt" \
    Copy paste von html für die gewünschte Abrechnungsperiode. Bitte speichern unter "input/advance tickets/Shows.txt" \
    Im Menu auf "Shows" <https://www.advance-ticket.ch/shows?lang=de> navigieren. \
    Spalte 1 startdatum wählen 1.1.20xx, Spalte 2 Enddatum wählen 31.12.20xx \
    ![Shows](doc/shows.png)

\newpage
## Script Konfiguration<a name="A_Script Konfiguration"></a>
[Inhaltsverzeichnis](#Inhaltsverzeichnis)


Die Datei **"Erstelle Abrechnung.R"** enhält am Anfang die folgenden definition die abgeändert werden können um das Verhalten des Scripts zu beeinflussen.

### Abrechnung für Filmvorführungen<a name="A_Abrechnung für Filmvorführungen"></a>
[Inhaltsverzeichnis](#Inhaltsverzeichnis)

-   Für jede Filmvorführung eine Abrechnung erstellen. \
    `c_run_single` <- TRUE  
-   Keine Abrechnug für Filmvorführung  erstellen. \
    `c_run_single` <- FALSE  

### Inhaltsverzeichnisse<a name="A_Inhaltsverzeichnisse"></a>
[Inhaltsverzeichnis](#Inhaltsverzeichnis)

Sollen die erstellten Berichte mit Inhaltsverzeichniss erstellt werden?

-   Ja \
    `toc` <- TRUE Ja
-   Nein \
    `toc` <- FALSE

### Mehrwertsteuersatz<a name="A_Mehrwertsteuersatz"></a>
[Inhaltsverzeichnis](#Inhaltsverzeichnis)

c_MWST <- 8.1 #%

### Platzkategorien ohne Umsatz die dennoch abgerechnet werden müssen.<a name="A_Platzkategorien ohne Umsatz die dennoch abgerechnet werden müssen."></a>
[Inhaltsverzeichnis](#Inhaltsverzeichnis)

Für gewisse Verleiher müssen zusätzliche Platzkategorieen abgerechnet werden. Die Defintion findet sich in der Datei "Verleiherabgaben.xlsx" TAB "Kinoförderer gratis".   \
Die Variable  `c_P_kat_verechnen` definiert welche Platzkategorien ohne Umsatz zusätzlich verrechnet werden.  \
`c_P_kat_verechnen` <- c("Kinoförderer","Spezialpreis")
\ 

### Ausgabeformate<a name="A_Ausgabeformate"></a>
[Inhaltsverzeichnis](#Inhaltsverzeichnis)


-   `c_render_option` <- "1"  only html
-   `c_render_option` <- "2"  html and docx
-   `c_render_option` <- "3"  html, docx and pdf (Achtung für pdf install Latex for Windows (Miktex) for Mac (MacTex))
 
\newpage
## Excel Dateien<a name="A_Excel Dateien"></a>
[Inhaltsverzeichnis](#Inhaltsverzeichnis)

Im Verzeichniss **.../Kinoklub/input/** kann mit Hilfe von Excelfiles folgendes definiert werden:

### Einkaufspreise<a name="A_Einkaufspreise"></a>
[Inhaltsverzeichnis](#Inhaltsverzeichnis)

Die Einkaufspreise die ab einem bestimmten Datum gültig sind. "Einkauf Kiosk xx.xx.xx.xlsx" \
Die Einkaufspreise für die Kioskverkäufe müssen gepflegt werden. Ändern sich die Einkaufspreise so muss ein neues File mit neuerem gültigkeis Datum erstellt werden. \

-   Achtung!   \
    Die alte Dateien dürfen nicht gelöscht werden.


### Spezialpreise Kiosk<a name="A_Spezialpreise Kiosk"></a>
[Inhaltsverzeichnis](#Inhaltsverzeichnis)

In der Datei **"Spezialpreisekiosk.xlsx"** müssen die Sonderangebote definiert werden. \
Diese Datei wird benötigt um die Spezialpreise 

-   Spez 1  
-   Spez 2  
-   Spez 3 
-   Spez 4 

nach zuschlagen. 


### Verleiherabgaben<a name="A_Verleiherabgaben"></a>
[Inhaltsverzeichnis](#Inhaltsverzeichnis)

Die Verleiherabgaben müssen in der Datei **".../Kinoklub/input/Verleiherabgaben.xlsx"** definiert werden. \

-   Im **Tab Verleiherabgaben** muss der "minimal Abzug" sowie "Abzug %" oder nur der "Abzug fix [CHF]" definiert werden. \ 
    Beide Einträge sind nicht erlaubt. 
    
-   Im **Tab Kinoförderer gratis** muss für jeden Verleiher definiert werden, ob gewisse Platzkategorien (z.B.Kinoförderer Tickets) als gratis abgerechnet werden dürfen. \
    Wenn **nein** gewählt wird, dann werden zusätzlich die Platzkategorieen  welche in `c_P_kat_verechnen` definiert sind als Umsatz verrechnet.  \
    Die Verleiherabrechnung wird ändert sich dadurch  was die Abgeben an den Verleiher vergrössert. 

### Einnahmen und Ausgaben<a name="A_Einnahmen und Ausgaben"></a>
[Inhaltsverzeichnis](#Inhaltsverzeichnis)

**".../Kinoklub/input/Einnahmen und Ausgaben.xlsx"** \
Alle Einnahmen und Ausgaben müssen definiert werden. \

## Berichte<a name="A_Berichte"></a>
[Inhaltsverzeichnis](#Inhaltsverzeichnis)

### Abrechnung Filmvorführung<a name="A_Abrechnung Filmvorführung"></a>
[Inhaltsverzeichnis](#Inhaltsverzeichnis)

Es wird eine Filmabrechnung pro Event (Datum) erstellt. Die folgenden Kategorien  werden einer Abrechnung zugewiesen.

-   Filmvorführung
-   Event
    -   Einnahmen
        -   In der Datei **".../Kinoklub/input/Einnahmen und Ausgaben.xlsx"** wird die Kategorie **Vermietung**  wird  pro Filmabrechnung (Datum) berücksichtigt. 
    -   Ausgaben  
        -   In der Datei **".../Kinoklub/input/Einnahmen und Ausgaben.xlsx"** wird die **Eventausgaben** wird  pro Filmabrechnung (Spieldatum) berücksichtigt.
-   Kiosk
    -   Einnahmen
    -   Ausgaben

### Jahresabrechnungen<a name="A_Jahresabrechnungen"></a>
[Inhaltsverzeichnis](#Inhaltsverzeichnis)

Die Einnahmen und Ausgaben werden für die Jahresabrechnung verwendet und je nach Kategorie der Rechnung zugewiesen. Die folgenden Kategorien werden in den Jahresrechnungen separat behandelt.

-   Eventausgaben \
    Alle Ausgaben die für den Eventausgegeben wurden, z.B. Werbung, Esswaren, Spesen
-   Kiosk \
    -   Einnahmen  \
        Die Einnahmen werden mit **"Anzahl x Verkaufspeis für Verkaufsartikel"** berechnet.
    -   Ausgaben
        -   Einkauf Getränke  \
            Die Getränke werden von Theater am Bahnhof eingekauft.  \
            Falls in der Datei **.../Kinoklub/input/Einkauf Kiosk xx.xx.xx.xlsx** der Lieferant **"Schüwo"** definiert wurde wird der Verkaufsartikel als Getränk ausgegeben. \
            Der Getränkeeinkauf wird mit **"Anzahl x Einkaufspreis"** berechnet.
        -   Einkauf Kino  \
            Für alle Verkaufsartikel mit Ausnahme der Getränke wird in der Datei **".../Kinoklub/input/Einnahmen und Ausgaben.xlsx"**  \ 
            mit Kategorie **Kiosk** definiert. 
-   Vermietung  \
    -   Einnahmen  \
        Vermietung Kinosaal, Beiträge von mit Veranstallter, ...
    -   Ausgaben  \
        Mietkosten für Filme und Material, ...
-   Werbung
    -   Einnahmen  \
        Die Werbeeinnahme aus Kinowerbung druch Trailers, Dias für Sponsoren, ... 
    -   Ausgaben  \
        Inserate, Drucksachen, Homepage, ...
-   Personalaufwand  \
    Löhne
-   Sonstiges
    -   Einnahmen  \
        Sponsoen, Gönner, Kulturbeiträge, ...
    -   Ausgaben  \
        Kinomiete an Theater am Bahnhof AG, Mitgliederbeiträge, Ciné Bulletin, ... 

    
\newpage
## Ausgabe / Output<a name="A_Ausgabe / Output"></a>
[Inhaltsverzeichnis](#Inhaltsverzeichnis)

Alle Dateien die erzeugt wurden finden sich im **.../Kinoklub/output/** Verzeichniss.

-   Für jede Filmvorführung respektive Datum wird ein Abrechnung erstellt.

-   Es wird eine Jahresbarechnung  und eine detalierte Jahresabrechnung erstellt.

-   Es wird eine Statistik mit Porgnosen erstellt.

-   Alle verwendeten Datensätze werden in ein Excelfile abgespeichert.


\newpage
# Berchnungsdokumentation<a name="A_Berchnungsdokumentation"></a>
[Inhaltsverzeichnis](#Inhaltsverzeichnis)

## Ablauf<a name="A_Ablauf"></a>
[Inhaltsverzeichnis](#Inhaltsverzeichnis)

"Erstelle Abrechnung.R" script führt folgendes auf. 

1.    Konfigurations variablen erstellen.
2.    Die Daten werden mit Script "read and convert.R" eingelesen und Konvertiert. 
      Der Script "read and convert.R" benötigt "function.R" und "Kiosk.R"
5.    Erstellen Jahresbericht
8.    Erstellen Jahresbericht detailed
11.   Erstellen Abrechnung Filmvorführung pro Datum respektive Vorführung


## Abrechnung Filmvorführung<a name="A_Abrechnung Filmvorführung"></a>
[Inhaltsverzeichnis](#Inhaltsverzeichnis)

Pro Filmvorführung wirde eine Abrechnung erstellt

