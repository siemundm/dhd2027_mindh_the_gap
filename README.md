# dhd2027_mindh_the_gap
Daten und Skripte zur Vortragseinreichung 

"Mind dh gap! Analyse der Themen in den Abstracts der DHd-Konferenzen"

von Catharina Ochsner und Melanie Seltmann für die [DHd2027](https://dhd2027.digitalhumanities.de)


## Vorgehen
1. Metadaten wurden aus der [Zenodo-Community "Digital Humanities im deutschsprachigen Raum"](https://zenodo.org/communities/dhd/records?q=&l=list&p=1&s=10&sort=newest) via Skript extrahiert (In der Zwischenzeit scheinen sich Änderungen ergeben zu haben, sodass das Skript nicht mehr funktioniert) --> Datei [recordlist.csv](https://github.com/siemundm/dhd2027_mindh_the_gap/blob/main/recordlist.csv)
2. Aus ConfTool wurden manuell die deutschen und englischen TaDaRaH-Schlagwörter heruntergeladen (DE: [tadirah_dhd.csv](https://github.com/siemundm/dhd2027_mindh_the_gap/blob/main/tadirah_dhd.csv), EN: [tadirah_dhd_en.csv](https://github.com/siemundm/dhd2027_mindh_the_gap/blob/main/tadirah_dhd_en.csv))
3. Analyse-Skript und Erstellung von Grafiken via [20260717_DHd-Themen.R](https://github.com/siemundm/dhd2027_mindh_the_gap/blob/main/20260717_DHd-Themen.R) (zur Erstellung wurde teilweise das KI-Modell HU-LLM1-Text - Kimi-K2.6-NVFP4 verwendet)
