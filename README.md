---
output: 
  html_document:
    css: source/Kinoklub_dark.css
---
# Inhaltsverzeichnis<a name="Inhaltsverzeichnis"></a>
* [1 Kinoklub](#A_1_Kinoklub)
    + [1.1 Installation](#A_1.1_Installation)
    + [1.2 Datensätze](#A_1.2_Datensätze)
    + [1.3 Konfigurations Dateien](#A_1.3_Konfigurations Dateien)
        + [1.3.1 Abrechnung für Filmvorführungen](#A_1.3.1_Abrechnung für Filmvorführungen)
        + [1.3.2 Inhaltsverzeichnisse](#A_1.3.2_Inhaltsverzeichnisse)
        + [1.3.3 Mehrwertsteuersatz](#A_1.3.3_Mehrwertsteuersatz)
        + [1.3.4 Platzkategorien ohne Umsatz die für gewisse Verleiher dennoch abgerechnet werden müssen.](#A_1.3.4_Platzkategorien ohne Umsatz die für gewisse Verleiher dennoch abgerechnet werden müssen.)
        + [1.3.5 Ausgabeformate](#A_1.3.5_Ausgabeformate)
    + [1.4 Excel Dateien](#A_1.4_Excel Dateien)
        + [1.4.1 Einkaufspreise](#A_1.4.1_Einkaufspreise)
        + [1.4.2 Spezialpreise Kiosk](#A_1.4.2_Spezialpreise Kiosk)
        + [1.4.3 Verleiherabgaben](#A_1.4.3_Verleiherabgaben)
        + [1.4.4 Einnahmen und Ausgaben](#A_1.4.4_Einnahmen und Ausgaben)
            + [1.4.4.1 Filmabrechnung](#A_1.4.4.1_Filmabrechnung)
    + [1.5 Ausgabe / Output](#A_1.5_Ausgabe / Output)



# 1 Kinoklub<a name="A_1_Kinoklub"></a>
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
Die Datei "README.md" wird automatisch beim erstellen der Abrechnung erstellt. Eine Änderung muss deshalb in der Datei "doc/README.Rmd" vorgenommen werden.  

## 1.1 Installation<a name="A_1.1_Installation"></a>
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

## 1.2 Datensätze<a name="A_1.2_Datensätze"></a>
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
    Spalte 1 startdatum wählen 1.1.20xx, Spalte 2 Enddatum wählen 31.12.20xx
    ![Shows](doc/shows.png)

## 1.3 Konfigurations Dateien<a name="A_1.3_Konfigurations Dateien"></a>
[Inhaltsverzeichnis](#Inhaltsverzeichnis)


Die Datei **"Erstelle Abrechnung.R"** enhält am Anfang die folgenden definition die abgeändert werden können um das Verhalten des Scripts zu beeinflussen.

### 1.3.1 Abrechnung für Filmvorführungen<a name="A_1.3.1_Abrechnung für Filmvorführungen"></a>
[Inhaltsverzeichnis](#Inhaltsverzeichnis)

-   Für jede Filmvorführung eine Abrechnung erstellen. \
    c_run_single <- TRUE  
-   Keine Abrechnug für Filmvorführung  erstellen. \
    c_run_single <- FALSE  

### 1.3.2 Inhaltsverzeichnisse<a name="A_1.3.2_Inhaltsverzeichnisse"></a>
[Inhaltsverzeichnis](#Inhaltsverzeichnis)

Sollen die erstellten Berichte mit Inhaltsverzeichniss erstellt werden?

-   Ja \
    toc <- TRUE Ja
-   Nein \
    toc <- FALSE

### 1.3.3 Mehrwertsteuersatz<a name="A_1.3.3_Mehrwertsteuersatz"></a>
[Inhaltsverzeichnis](#Inhaltsverzeichnis)

c_MWST <- 8.1 #%

### 1.3.4 Platzkategorien ohne Umsatz die für gewisse Verleiher dennoch abgerechnet werden müssen.<a name="A_1.3.4_Platzkategorien ohne Umsatz die für gewisse Verleiher dennoch abgerechnet werden müssen."></a>
[Inhaltsverzeichnis](#Inhaltsverzeichnis)

c_P_kat_verechnen <- c("Kinoförderer","Spezialpreis")
\ 
In der Datei "Verleiherabgaben.xlsx" kann die Abrechnungsvariante definiert werden

### 1.3.5 Ausgabeformate<a name="A_1.3.5_Ausgabeformate"></a>
[Inhaltsverzeichnis](#Inhaltsverzeichnis)


-   c_render_option <- "1"  only html
-   c_render_option <- "2"  html and docx
-   c_render_option <- "3"  html, docx and pdf (Achtung für pdf install Latex for Windows (Miktex) for Mac (MacTex))
 


## 1.4 Excel Dateien<a name="A_1.4_Excel Dateien"></a>
[Inhaltsverzeichnis](#Inhaltsverzeichnis)

Im Verzeichniss **.../Kinoklub/input/** kann mit Hilfe von Excelfiles folgendes definiert werden:

### 1.4.1 Einkaufspreise<a name="A_1.4.1_Einkaufspreise"></a>
[Inhaltsverzeichnis](#Inhaltsverzeichnis)

Die Einkaufspreise die ab einem bestimmten Datum gültig sind. "Einkauf Kiosk xx.xx.xx.xlsx" \
Die Einkaufspreise für die Kioskverkäufe müssen gepflegt werden. Ändern sich die Einkaufspreise so muss ein neues File mit neuerem gültigkeis Datum erstellt werden. \

-   Achtung! \ 
    Die alte Dateien dürfen nicht gelöscht werden.


### 1.4.2 Spezialpreise Kiosk<a name="A_1.4.2_Spezialpreise Kiosk"></a>
[Inhaltsverzeichnis](#Inhaltsverzeichnis)

In der Datei **"Spezialpreisekiosk.xlsx"** müssen die Sonderangebote definiert werden. \
Diese Datei wird benötigt um die Spezialpreise 

-   Spez 1  
-   Spez 2  
-   Spez 3 
-   Spez 4 

nach zuschlagen. 


### 1.4.3 Verleiherabgaben<a name="A_1.4.3_Verleiherabgaben"></a>
[Inhaltsverzeichnis](#Inhaltsverzeichnis)

Die Verleiherabgaben müssen in der Datei **"Verleiherabgaben.xlsx"** definiert werden. \

-   Im **Tab Verleiherabgaben** muss der "minimal Abzug" sowie "Abzug %" oder nur der "Abzug fix [CHF]" definiert werden. \ 
    Beide Einträge sind nicht erlaubt. 
    
-   Im **Tab Kinoförderer gratis** muss für jeden Verleiher definiert werden, ob gewisse Platzkategorien (z.B.Kinoförderer Tickets) als gratis abgerechnet werden dürfen. \
    Wenn **nein** gewählt wird, dann werden zusätzlich die Platzkategorieen  welche in `c_P_kat_verechnen` definiert sind als Umsatz verrechnet.  \
    Die Verleiherabrechnung wird ändert sich dadurch  was die Abgeben an den Verleiher vergrössert. 

### 1.4.4 Einnahmen und Ausgaben<a name="A_1.4.4_Einnahmen und Ausgaben"></a>
[Inhaltsverzeichnis](#Inhaltsverzeichnis)

"Einnahmen und Ausgaben.xlsx" \
Alle Einnahmen und Ausgaben müssen definiert werden. \
Die Einnahmen und Ausgaben werden für die Jahresabrechnung verwendet und jenach Kategorie der Rechnung zugewiesen. 


#### 1.4.4.1 Filmabrechnung<a name="A_1.4.4.1_Filmabrechnung"></a>
[Inhaltsverzeichnis](#Inhaltsverzeichnis)

Für die Filmabrechnung die folgenden Definitionen wichtig.

-   Filmabrechnung-Einahmen \
    Die Kategorie **Vermietung**  wird  pro Filmabrechnung (Datum) berücksichtigt. \
    
-   Filmabrechnung-Ausgaben \
    Die Kategorie **Eventausgaben** wird  pro Filmabrechnung (Spieldatum) berücksichtigt. \
    
-   Verleiherrechnung-Ausgaben \
    Die Kategorie **Verleiher** wird pro Filmabrechnung (Spieldatum) berücksichtigt. \
    

## 1.5 Ausgabe / Output<a name="A_1.5_Ausgabe / Output"></a>
[Inhaltsverzeichnis](#Inhaltsverzeichnis)

Alle Dateien die erzeugt wurden finden sich im **.../Kinoklub/output/** Verzeichniss.

- Für jede Vorführung respektive Datum wird ein Abrechnung erstellt.
- Die Erfolgsrechnung wird über alle Datensätze erstellt.
- Zusätzliche Daten sind in der Statistik ersichtlich.
- Alle verwendeten Datensätze werden in ein Excelfile abgespeichert.


