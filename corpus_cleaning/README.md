# Beschreibung
Die hier zusammengestellten Skripte fassen die haeufigsten Vorverarbeitungsschritte an ein Corpus-Cleaning zusammen. Das bedeutet nicht, dass die hier eingebundenen Funktionen vollstaendig sind und eine tiefe Analyse des Korpus ueberfluessig machen. Vielmehr handelt es sich hierbei um eine Art lower boundary.

### Das Skript (teil)automatisiert
- die Kuerzung der Meta-Daten auf die wichtigsten Variablen
- eine Sortierung der Meta-Daten (und entsprechend der Text-Daten) nach Datum
- ein grobes Cleaning der Titles (u.A. Entfernen von mehreren Leerzeichen)
- das Entfernen kurzer Texte (der Wert kann angepasst werden; Default ist 750 Zeichen. Dies ist der Umfang einer halbem Normseite)
- das Entfernen ungewoehnlich langer Texte (auch dieser Wert kann angepasst werden; Default ist > 99% aller Texte)
- die Konvertierung alle Texte und Titles in UTF-8
- Das Entfernen von Duplikaten basierend auf unterschiedlichen Metriken
  - uneindeutige IDs
  - `tosca::duplist()`
  - Texte, die innerhalb derselben Woche erschienen sind und mit denselben 120 Zeichen beginnen (beide Werte (Woche und nchar) koennen angepasst werden)
  - Texte, die innerhalb desselben Tages erschienen sind und denselben Titel haben
 
### Darueber hinaus bietet das Skript einige Befehle zur weiteren Analyse des Korpus
- Quellen checken (welche Quellen sind hinterlegt und wie ist die Frequenz der Quellen im Zeitverlauf?)
- Titles checken (welche Titles kommen ungewoehnlich haeufig vor?)
- Titles loeschen aufgrund bestimmter Patterns/Words, die in den Titles vorkommen
- ...

# Verwendung
- laden Sie alle Skripte herunter
- fuehren Sie das Skript `clean_corpus.R` aus. Das Skript greift auf die anderen beiden Skripte zu. Diese muessen also nicht von Hand geoeffnet werden
- gehen Sie das Skript Zeile fuer Zeile durch. Die Skripte sind so geschrieben, dass sie fortlaufend Rueckmeldung ueber den Cleaning-Prozess geben
