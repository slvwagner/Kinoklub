Script Version: 2024 V1.1
 
## Kinoklub 

Script zur Abrechnung für den Kinoklub TaB. Um die Abrechnung für den Kinoklub zu vereinfachen respektive zu automatisieren wurde dieser Script erstellt.\
Dieser Skrip kann mit folgendem Befehl ausgeführt werden:

```         
source("Erstelle Abrechnung.R")
```

\
Bei Fehlern kann ein "Issue" in Github erfasst werden.\
\
Die Datei "README.md" und die Dokumentation wird automatisch erstellt.

```         
source("doc/create Readme and Docu.R")
```

Eine Änderung muss deshalb in der Datei **"doc/README.Rmd"** vorgenommen werden.



## Installation

1.  Download and install R\
    <https://cran.r-project.org/bin/windows/base/>
2.  Download and install Rstudio\
    <https://posit.co/download/rstudio-desktop/>
3.  Download git:\
    <https://git-scm.com/downloads>
4.  Kionklub Scripts download: Navigate to folder you would like to install the Scripts

```         
    git clone https://github.com/slvwagner/Kinoklub
```

6.  Start Rstudio from the Kinoklub folder, or open the project with Rstudio "Kinoklub.Rproj".
7.  Install the needed packages in the R Terminal

```         
    install.packages("rmarkdown")
    install.packages("tidyverse")
    install.packages("rebus")
    install.packages("openxlsx")
    install.packages("flextable")
    install.packages("magick")
    install.packages("webshot")
    install.packages("xml2")
```

7.  Run this command once in R-Terminal, error MSG can be ignored

```         
    webshot::install_phantomjs()
```

8.  Run the Script:

```         
    source("Erstelle Abrechnung.R")
```

## Git Passwort / Personal Access Token (PAT)
Git Passwort gibt es seit 2021 nicht mehr. Um sich bei Git anzumelden, muss man auf der GitHub Webseite unter dem eigenen Profil in den Einstellungen auf **Developer Settings** navigieren. Dann unter **Personal access tokens** **Tokens (classic)** anwählen. Oben rechts auf **Generate new token** klicken und **classic** auswählen. Dem Token einen Namen geben und **Expiration** auf **No Expiration** setzen. Danach alle **repo** anwählen. Nach unten scrollen und Token generieren. Token kopieren und Anleitung unten im Bild folgen. \
![](doc/PAT.png)

## Datensätze

### Advance-Tickets 

Die Datensätze können von <https://www.advance-ticket.ch/admin> heruntergeladen werden und sind unter dem Verzeichnis **.../Kinoklub/input/advance tickets/** abzuspeichern.

#### Eintritte
**Eintritte 02.12.23.txt**  \
Copy paste von html für jede Vorführung und bitte speichern unter "input/advance tickets/Eintritt xx.xx.xx.txt" Es muss die Kalenderwoche sowie der Film ausgewählt werden.\
![Eintritt](doc/eintritt.png)

#### Kiosk
**Kiosk 02.12.23.txt**  \
Copy paste von html für jede Vorführung und Speichern unter "input/advance tickets/Kiosk xx.xx.xx.txt".\
Im Menu auf "DecompteCaisse" <https://www.advance-ticket.ch/decomptecaisse?lang=de> navigieren.\
Spalte 1 Das Datum muss gewählt werden, Spalte 2 "reinach", Splate 3 "Atelierkino Kasse" und Spalte 4 "..." eingestellt werden.\
![Kiosk](doc/Kiosk.png)

#### Shows
**Shows.txt**  \
Copy paste von html für die gewünschte Abrechnungsperiode. Bitte speichern unter "input/advance tickets/Shows.txt"\
Im Menu auf "Shows" <https://www.advance-ticket.ch/shows?lang=de> navigieren.\
Spalte 1 startdatum wählen 1.1.20xx, Spalte 2 Enddatum wählen 31.12.20xx\
![Shows](doc/shows.png)


### Excel Dateien

Im Verzeichniss **.../Kinoklub/input/** kann mit Hilfe von Excelfiles folgendes definiert werden:

#### Einkaufspreise

Die Einkaufspreise die ab einem bestimmten Datum gültig sind. "Einkauf Kiosk xx.xx.xx.xlsx"\
Die Einkaufspreise für die Kioskverkäufe müssen gepflegt werden. Ändern sich die Einkaufspreise so muss ein neues File mit neuerem gültigkeis Datum erstellt werden.\

-   Achtung!\
    Die alte Dateien dürfen nicht gelöscht werden.

#### Spezialpreise Kiosk

In der Datei **Spezialpreisekiosk.xlsx** müssen die Sonderangebote (Spez-Verkaufsartikel) definiert werden.\
Diese Datei wird benötigt um die Spezialpreise

-   Spez 1
-   Spez 2
-   Spez 3
-   Spez 4

nach zuschlagen.

#### Verleiherabgaben

Die Verleiherabgaben müssen in der Datei **.../Kinoklub/input/Verleiherabgaben.xlsx** definiert werden.\

-   Im **Tab Verleiherabgaben** muss der **"minimal Abzug"** sowie **"Abzug %"** oder nur der **"Abzug fix [CHF]"** definiert werden.   Beide Einträge sind nicht erlaubt.

-   Im **Tab Kinoförderer gratis** muss für jeden Verleiher definiert werden, ob gewisse Platzkategorien (z.B.Kinoförderer Tickets) als gratis abgerechnet werden dürfen.\
    Wenn **nein** gewählt wird, dann werden zusätzlich die Platzkategorieen welche in `c_P_kat_verechnen` definiert sind, auch als Umsatz verrechnet.\
    Der Rechnungsbetrag der Verleiherrechnung an den Kinoklub wird demnach grösser.

#### Einnahmen und Ausgaben

Alle Einnahmen und Ausgaben müssen in der Datei **.../Kinoklub/input/Einnahmen und Ausgaben.xlsx** definiert werden.\
Ja nach **Ausgabentyp** muss eine **Kategorie** verwendet werden. Das ist nötig um die Einnahmen und Ausgaben auf die korrekten in den verschiedenen **Berichten** auszuwerten. 

##### Einnahmen

<!-- ```{r} -->
<!-- paste("-\t",Einnahmen_und_Ausgaben$dropdown$`drop down`,"\n", collapse = "")|>writeLines() -->
<!-- ``` -->


```{r}
tibble(Kategorie = Einnahmen_und_Ausgaben$dropdown$`drop down`,
       Bezeichnung = c("Einnahmen die nicht aus Kiosk oder Tickets gemacht wurden, z.B. Eventsponsoren",
                       "Einnahmem aus Kioskverkäufen",
                       "",
                       "Sonstige Einnahmen, z.B. Sponsoren, Kinoförderer",
                       "",
                       "Vermietung Saal, Einnahmen gehen an Theater am Bahnhof AG",
                       "Werbeeinnahmen z.B. durch Kinowerbung die nicht für jede Filmvorführung abgerechnet werden kann."
                       ),
       `Bericht-Berücksichtigung` = c("Abrechnung, Jahresrechnungen, Statistik",
                              "Abrechnung, Jahresrechnungen, Statistik",
                              "",
                              "Jahresrechnungen",
                              "",
                              "Jahresrechnungen",
                              "Jahresrechnungen"
                              )
       )|>
  knitr::kable()

```

##### Ausgaben

<!-- ```{r} -->
<!-- paste("-\t",Einnahmen_und_Ausgaben$dropdown$`drop down`,"\n", collapse = "")|>writeLines() -->
<!-- ``` -->

```{r}
tibble(Kategorie = Einnahmen_und_Ausgaben$dropdown$`drop down`,
       Bezeichnung = c("Ausgaben für Spezial Verkaufsartikel, Deko, ...",
                       "Einkauf für Kiosk ohne Getränkeeinkauf Schüwo der über die TabBar abgewickelt wird. ",
                       "Lohnkosten, Sozialleistungen",
                       "Ausgaben die über das Jahr anfallen z.B. Reperaturkosten Kinoinfrastruktur, Porto, Spesen u.s.w..",
                       "Rechnungen der Verleiher die pro **Spieldatum** abgerechnet werden",
                       "Ausgaben für Vermietungen z.B. Filmmiete, weitere Kosten die der Kinoklub aufwenden musste um die Vermietung durchzuführen.",
                       "Werbeausgaben für Inserate, Drucksachen, u.s.w"
                       ),
       `Bericht-Berücksichtigung` = c("Abrechnung, Jahresrechnungen, Statistik",
                              "Abrechnung, Jahresrechnungen, Statistik",
                              "Jahresrechnungen",
                              "Jahresrechnungen",
                              "Abrechnung, Jahresrechnungen, Statistik",
                              "Jahresrechnungen",
                              "Jahresrechnungen"
                              )
       )|>
  knitr::kable()

```

## Script Konfiguration

Die Datei **"Erstelle Abrechnung.R"** enhält am Anfang die folgenden definition die abgeändert werden können um das Verhalten des Scripts zu beeinflussen.

### Abrechnung für Filmvorführungen

-   Für jede Filmvorführung eine Abrechnung erstellen.\
    `c_run_single` \<- TRUE\
-   Keine Abrechnug für Filmvorführung erstellen.\
    `c_run_single` \<- FALSE

### Inhaltsverzeichnisse

Sollen die erstellten Berichte mit Inhaltsverzeichniss erstellt werden?

-   Ja\
    `toc` \<- TRUE Ja
-   Nein\
    `toc` \<- FALSE

### Mehrwertsteuersatz

c_MWST \<- 8.1 #%

### Platzkategorien ohne Umsatz die dennoch abgerechnet werden müssen.

Für gewisse Verleiher müssen zusätzliche Platzkategorieen abgerechnet werden. Die Defintion findet sich in der Datei "Verleiherabgaben.xlsx" TAB "Kinoförderer gratis".\
Die Variable `df_P_kat_verechnen` definiert welche Platzkategorien ohne Umsatz zusätzlich verrechnet werden und zu welchem Preis.\
`df_P_kat_verechnen` \<- tibble(Kinoförderer = "Kinoförderer", Verkaufspreis = 13)  

### Ausgabeformate

-   `c_render_option` \<- "1" only html
-   `c_render_option` \<- "2" only docx
-   `c_render_option` \<- "3" only pdf (Achtung für pdf install Latex for Windows (Miktex) for Mac (MacTex))
-   `c_render_option` \<- "4" html and docx
-   `c_render_option` \<- "5" html and pdf (Achtung für pdf install Latex for Windows (Miktex) for Mac (MacTex))
-   `c_render_option` \<- "6" docx and pdf (Achtung für pdf install Latex for Windows (Miktex) for Mac (MacTex))
-   `c_render_option` \<- "7" html, docx and pdf (Achtung für pdf install Latex for Windows (Miktex) for Mac (MacTex))


## Ablauf

"Erstelle Abrechnung.R" Script führt folgendes auf.

1.    Konfigurations variablen erstellen.
2.    Die Daten werden mit Script "read and convert.R" eingelesen und Konvertiert. Der Script "read and convert.R" benötigt "function.R" und "Kiosk.R"
3.    Erstellen Statistikbericht 
3.    Erstellen Jahresbericht
4.    Erstellen Jahresbericht detailed
5.    Erstellen Abrechnung Filmvorführung pro Datum respektive Vorführung
6.    Site-Map mit Berichtvorschau wird erstellt
7.    Daten für Webserver werden erstellt. 



## Berichte

Alle Dateien die erzeugt wurden finden sich im **.../Kinoklub/output/** Verzeichniss.

-   Für jede Filmvorführung respektive Datum wird ein Abrechnung erstellt.
-   Es wird eine Jahresbarechnung und eine detalierte Jahresabrechnung erstellt.
-   Es wird eine Statistik mit Porgnosen erstellt.
-   Alle verwendeten Datensätze werden in ein Excelfile abgespeichert.

### Abrechnung Filmvorführung

Es wird eine Filmabrechnung pro Event (Datum) erstellt.

-   Übericht\
-   Filmvorführung
    -   Kino Besucherzahlen und Umsatz
        -   Filmabgaben\

        -   Verleiherrechnung\
            In der Datei **".../Kinoklub/input/Einnahmen und Ausgaben.xlsx"** in den **Ausgaben**\
            wird die Kategorie **Verleiher** berücksichtigt.

        -   Prozentualle Abgaben\
            Der Suisaabzug wird vom Umsatz berechnet.\

            In der Datei **".../Kinoklub/input/Verleiherabgaben.xlsx"** sind **Abzug %**, **Minimal Abzug** oder **Abzug fix [CHF]** definiert.\

            1.  Fall:\
                **Netto3** x **Abzug %** \> **Minimal Abzug**\
                Verleiherabzug: **Netto3** x **Abzug %**
            2.  Fall:\
                **Netto3** x **Abzug %** \< **Minimal Abzug**\
                Verleiherabzug: **Minimal Abzug**\
            3.  Fall:\
                **Abzug fix [CHF]**\
                Verleiherabzug: **Abzug fix [CHF]**

        -   Reklamematerial und Porto\
            Das **Reklamematerial und Porto** werden aus der Differenz der **Verleiherrechnung** und den **Prozentualle Abgaben** berechnet.

        -   MWST auf Verleiherrechnung\

            1.  Fall: Vereiherrechnung vorhanden\
                MWST wird mit der Verleiherrechnung berechnet.
            2.  Fall: Vereiherrechnung nicht vorhanden\
                MWST wird aus dem Umsatz berechnet.\
    -   Gewinn / Verlust aus Tickerverkauf\
        Der Gewinn/Verlust wird aus **Umsatz** - (**Suisa-Abzug**+**Verleiherabzug**+**MWST**+**Reklamematerial und Porto**)
-   Event
    -   Einnahmen
        -   In der Datei **".../Kinoklub/input/Einnahmen und Ausgaben.xlsx"** in den **Ausgaben**\
            wird die Kategorie **Vermietung** wird pro Filmabrechnung (Datum) berücksichtigt.
    -   Ausgaben
        -   In der Datei **".../Kinoklub/input/Einnahmen und Ausgaben.xlsx"** in den **Ausgaben**\
            wird die **Eventausgaben** wird pro Filmabrechnung (Spieldatum) berücksichtigt.
-   Kiosk
    -   Gewinn pro Artikel\
        In der Datei **".../Kinoklub/input/Einkauf Kiosk xx.xxx.xx.xlsx"** ist der Gewinn pro Artikel definiert.
    -   Umsatz Für den Verkaufsartikel gibte es keine Definition in der Datei **".../Kinoklub/input/Einkauf Kiosk xx.xxx.xx.xlsx"**\
    -   Einnahmen\
        Die Einnahmen werden aus Gewinn pro Artikel und dem Umsatz für Spezialpreise berechnet.
    -   Ausgaben\
        Achtung!\
        Für Verkaufsartikel ohne Lieferant müssen Eventausgaben definiert werden.
-   Kioskumsatz pro Gast
    -   Kioskumsatz aller Gäste
    -   Kioskumsatz pro zahlender Gast
-   Kasse\
    Die Kioskkasse wird auch für Barauszahlung von stornierten Tickets genutzt.
-   Gewinn / Verlust\
    Summe aus Einnahmen und Ausgaben



### Jahresabrechnungen

Die Einnahmen und Ausgaben werden für die Jahresabrechnung verwendet und je nach Kategorie der Rechnung zugewiesen. Die folgenden Kategorien werden in den Jahresrechnungen separat behandelt.

-   Filmvorführungen\
    -   Eintritt
        -   Einnahmen Ticketverkauf
        -   Abgaben Ticketverkauf
            -   Suisaabgaben
            -   Verleiherabgaben
            -   Nebenkosten
            -   MWST auf Verleiherleistungen
-   Event
    -   Eventeinnahmen\
        Einnahmen für den Event, z.B. Beiträgemitveranstalter, Eventsponsoring, ...
    -   Eventausgaben\
        Alle Ausgaben die für den Event, z.B. Werbung, Esswaren, Spesen, …
-   Kiosk
    -   Einnahmen\
        Die Einnahmen werden mit **"Anzahl x Verkaufspeis für Verkaufsartikel"** berechnet.
    -   Ausgaben
        -   Einkauf Getränke\
            Die Getränke werden von Theater am Bahnhof eingekauft.\
            Falls in der Datei **.../Kinoklub/input/Einkauf Kiosk xx.xx.xx.xlsx** der Lieferant **"Schüwo"** definiert wurde wird der Verkaufsartikel als Getränk ausgegeben.\
            Der Getränkeeinkauf wird mit **"Anzahl x Einkaufspreis"** berechnet.
        -   Einkauf Kino\
            Für alle Verkaufsartikel mit Ausnahme der Getränke wird in der Datei **".../Kinoklub/input/Einnahmen und Ausgaben.xlsx"**   mit Kategorie **Kiosk** definiert.
-   Abos / Kinogutscheine
    - Einnahmen
        - Abos
        - Kinogutscheine
        - Summe
    - Eingelöst
        - Abos
        - Kinogutscheine
    - Kurzfristiges zinsloses Fremd-Kapital
-   Vermietung\
    -   Einnahmen\
        Vermietung Kinosaal, Beiträge von mit Veranstallter, ...
    -   Ausgaben\
        Mietkosten für Filme und Material, ...
-   Werbung
    -   Einnahmen\
        Die Werbeeinnahme aus Kinowerbung druch Trailers, Dias für Sponsoren, ...
    -   Ausgaben\
        Inserate, Drucksachen, Homepage, ...
-   Personalaufwand\
    Löhne
-   Sonstiges
    -   Einnahmen\
        Sponsoen, Gönner, Kulturbeiträge, ...
    -   Ausgaben\
        Kinomiete an Theater am Bahnhof AG, Mitgliederbeiträge, Ciné Bulletin, ...



### Statistik

-   Gewinn/Verlust
    -   Prognose\
        Die Prognose wird mit der Kumuliertensumme pro Datum als lineares Model erstellt.
-   Ticketverkauf
    -   Prognose\
        Die Prognose wird mit der Kumuliertensumme pro Datum als lineares Model erstellt.
    -   Eintritte
        -   Anzahl\
            Diagramm
        -   Umsatz
            -   Prognose\
                Die Prognose wird mit der Kumuliertensumme pro Datum als lineares Model erstellt.
    -   Filmabgaben
        -   Prognose\
            Die Prognose wird mit der Kumuliertensumme pro Datum als lineares Model erstellt.
-   Abos
    -   Einnahmen
    -   Eingelöst
    -   Kredit
-   Kiosk-Gewinn pro Vorführung
    -   Prognose\
        Die Prognose wird mit der Kumuliertensumme pro Datum als lineares Model erstellt.
-   Kiosk
    -   Verkaufsartikel
    -   Ladenhüter (keine Verkäufe)
    -   Kiosk Umsatz pro Gast
        -   Prognose\
            Die Prognose wird mit der Kumuliertensumme pro Datum als lineares Model erstellt.
    -   Umsatz pro zahlender Gast
        -   Prognose\
            Die Prognose wird mit der Kumuliertensumme pro Datum als lineares Model erstellt.

## Versionshistorie

V1.0 Go Live mit Stefan Jablonski, Nadia und Florian Wagner  \
V1.1 Verkauf von Abos und Gutscheinen wird in der Jahresabarechnung berücksichtigt    \


