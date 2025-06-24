# roboT: regular operations based on Tosca

### Beschreibung

**roboT** ist ein R-Paket zur vereinfachten Durchführung und Reproduktion von Analysen auf Basis des [tosca](https://cran.r-project.org/package=tosca)-Frameworks. Es bietet eine Sammlung nützlicher Funktionen zum Korpus-Cleaning, zur LDA-Modellierung sowie zur Visualisierung und Export einfacher Analyseergebnisse. Die Funktionen sind explizit für den Einstieg in die computergestützte Inhaltsanalyse mit `tosca` konzipiert.

Der Funktionsumfang orientiert sich an typischen Arbeitsschritten in journalistischen und sozialwissenschaftlichen Textanalysen und kann je nach Forschungsfrage flexibel angepasst und erweitert werden.

---

### Installation

```r
# devtools ggf. zuerst installieren
install.packages("devtools")

# roboT von GitHub installieren
devtools::install_github("Piece-Of-Schmidt/roboT", upgrade = "never")

# Paket laden
library(roboT)
```

### Skripte

Die Skripte (hinterlegt in `/scripts`) dienen als Vorlage für eine einfache Analyse basierend auf dem tosca-Package und der hiesigen Erweiterung (roboT). Zur Verwendung:

* laden Sie die Skripte herunter
* oeffnen Sie das Skript `1_corpus_cleaning.R`, wenn Sie einen Korpus cleanen wollen, oder das Skript `2_tosca_LDA_basics.R` fuer eine beispielhafte LDA-Analyse
* gehen Sie das jew. Skript Zeile fuer Zeile durch und fuehren Sie die Funktionen aus, die fuer Ihre Analyse notwendig und sinnvoll sind
* bei Fragen melden Sie sich gerne bei Ihrem/Ihrer Betreuer:in

### Example Corpus
Der Korpus, der in diesem Repo liegt und Teil des Packages ist, umfasst Film-Beschreibungen zu Filmen aus den 1980er Jahren. Die hinterlegten Texte sind Original-Texte von Wikipedia. Die Meta-Informationen der Dokumente sind jedoch (zum Teil) frei erfunden. Der Korpus hat also keinen Anspruch auf Richtigkeit oder Vollständigkeit und dient einzig und allein Demonstrationszwecken. Sie können den Korpus nach der Paket-Installation aus R heraus aufrufen.

```r
corp <- example_corpus
```


### Quelle (tosca als Basis-Package)
*Koppers L, Rieger J, Boczek K, von Nordheim G (2021). tosca: Tools for Statistical Content Analysis. doi:10.5281/zenodo.3591068, R package version 0.3-2, https://github.com/Docma-TU/tosca.*<br>
Dokumentation: https://cran.r-project.org/web/packages/tosca/index.html


### Feedback
* Schicken Sie uns gerne Feedback zu den Skripten :)
* Feedback zum Package sind auch gerne gesehen
