### ------------------------------------------
### CODE zu Mind dh gap! Analyse der Themen in den Abstracts der DHd-Konferenzen
### ------------------------------------------


# LADEN DER BIBLIOTHEKEN
library(dplyr)
library(forcats)
library(ggplot2)
library(ggraph)
library(gridExtra)
library(httr)
library(igraph)
library(jsonlite)
library(lubridate)
library(patchwork)
library(readr)
library(rdflib)
library(stringr)
library(tidygraph)
library(tidytext)
library(tidyr)
library(tidyverse)
library(treemap)
library(widyr)
library(wordcloud2)

# DATAFRAME EINLESEN
data <- read.csv("recordlist.csv")

# SOURCE BEREINIGEN
# Leerzeichen löschen, kleinschreiben, vdhd2021 vereinheitlichen
data <- data %>%
  mutate(
    source_backup = source %>%
      str_sub(1, 8) %>%
      str_remove_all(" ") %>%
      str_remove_all(",+$") %>%
      str_to_lower() %>%
      str_sub(1, 7) %>%
      str_replace("^(vdhd,vd|vdhd202)$", "vdhd2021")
  )

# Alle subject-Spalten zusammenfassen
subjects_long <- data %>%
  mutate(paper_id = row_number()) %>%
  select(paper_id, source_backup, starts_with("subject")) %>%
  pivot_longer(
    cols = starts_with("subject"),
    values_to = "keyword",
    names_to = "column"
  ) %>%
  filter(
    !is.na(keyword),
    keyword != "",
    keyword != "NA"
  ) %>%
  mutate(keyword = str_squish(keyword))


subjects_long <- subjects_long %>%
  filter(
    !str_detect(
      keyword,
      regex("^(Paper|Poster|Posterpräsentation|Vortrag|Metareflexion|Vortrag: Theorie|Positionspapier|vdhd2021|d?Hd(201[4-9]|202[0-6])|dhd)$",
            ignore_case = TRUE)
    )
  )

# häufigste Labels vereinheitlichen
subjects_long <- subjects_long %>%
  mutate(
    keyword = trimws(keyword),
    keyword = tolower(keyword),
    keyword = case_when(
      keyword %in% c("digital humanities", "dh") ~ "digital humanities",
      keyword %in% c("named-entity recognition (ner)", "named entity recognition","ner") ~ "named entity recognition",
      keyword %in% c("optical character recognition","ocr") ~ "optical character recognition",
      keyword %in% c("machine learning", "ml") ~ "machine learning",
      keyword %in% c("artificial intelligence", "ai", "ki", "künstliche intelligenz") ~"artificial intelligence",
      keyword %in% c("data-recognition", "data recognition") ~"data-recognition",
      keyword %in% c("visualisation", "visualization", "visualisierung") ~"visualisation",
      keyword %in% c("annotieren", "annotating") ~"annotating",
      keyword %in% c("transkription", "transcription") ~"transcription",
      keyword %in% c("modelling", "modeling", "modellierung") ~"modelling",
      keyword %in% c("metadata", "metadaten") ~"metadaten",
      keyword %in% c("mixed methods", "mixed-methods") ~"mixed methods",
      keyword %in% c("inhaltsanalyse", "content analysis") ~"content analysis",
      keyword %in% c("3d-modell", "3d-modelle") ~"3d-modell",
      keyword %in% c("daten", "data") ~"daten", 
      keyword %in% c("forschungsdaten", "research data") ~"research data",
      keyword %in% c("tei", "text encoding initiative") ~"text encoding initiative",
      keyword %in% c("literatur", "literature") ~"literatur",
      keyword %in% c("methoden", "methods") ~"methoden",
      keyword %in% c("infrastructure", "infrastruktur") ~"infrastruktur",
      keyword %in% c("kollaboration", "collaboration") ~"collaboration",
      keyword %in% c("veröffentlichung", "publication","publizieren","publishing") ~"publishing",
      keyword %in% c("sammlung", "collection") ~"collection",
      keyword %in% c("forschungsprozess", "research process") ~"forschungsprozess",
      keyword %in% c("netzwerkanalyse", "network analysis") ~"network analysis",
      keyword %in% c("werkzeuge", "tools") ~"werkzeuge",
      keyword %in% c("benannte entitäten (named entities)", "benannte entitäten", "named entities") ~ "benannte entitäten (named entities)",
      keyword %in% c("programmierung", "programming") ~"programming",
      keyword %in% c("archivierung", "archival","archiving") ~"archiving",
      keyword %in% c("forschung", "research") ~"forschung",
      keyword %in% c("bilder", "pictures") ~"bilder",
      keyword %in% c("communication", "kommunikation","kommunizieren","communicating") ~"communicating",
      keyword %in% c("kontextsetzung", "contextualisation", "kontextualisierung","kontextualisieren","contextualising") ~"contextualising",
      keyword %in% c("virtuelle forschungsumgebungen", "virtual research environments") ~"virtuelle forschungsumgebungen",
      keyword %in% c("digitale edition", "digital edition","digitale editionen","digital editions") ~"digital editions",
      keyword %in% c("lehre", "teaching","learning","lernen") ~"teaching/learning",
      keyword %in% c("entdeckung", "discovery","entdecken","discovering") ~"discovering",
      keyword %in% c("teilen", "share","sharing") ~"sharing",
      keyword %in% c("webentwicklung", "web development") ~"web-development",
      keyword %in% c("sprache", "language") ~"sprache",
      keyword %in% c("manuscript", "manuskript") ~"manuskript",
      keyword %in% c("theoretisierung", "theorisation","theorisierung","theorising") ~"theorising",
      keyword %in% c("datenerkennung", "data recognition") ~"data recognition",
      keyword %in% c("bereinigung", "cleaning","cleanup") ~"cleanup",
      keyword %in% c("bearbeitung", "processing") ~"processing",
      keyword %in% c("artefakte", "artefacts") ~"artefakte",
      keyword %in% c("projekte", "projects") ~"projekte",
      keyword %in% c("Personen", "people") ~"personen",
      keyword %in% c("strukturanalyse", "structural analysis") ~"structural analysis",
      keyword %in% c("forschungsergebnis", "research findings") ~"forschungsergebnis",
      keyword %in% c("forschungsdatenmanagement", "research data management", "fdm", "rdm") ~"research data management",
      keyword %in% c("community-building", "community building","community-bildung") ~"community-building",
      TRUE ~ keyword
    )
  ) %>%
  distinct(paper_id, keyword, .keep_all = TRUE)

# deutsche kleinschreiben
tadirah_de <- read.csv2("tadirah_dhd.csv")
tadirah_de <- tadirah_de %>%
  mutate(
    label = tolower(label),
    concept_short = tolower(concept_short)
  )

# deutsche TaDiRAH-Konzepte in Datenframe auf englisch übersetzen
subjects_long <- subjects_long %>%
  left_join(
    tadirah_de %>% select(label, concept_short),
    by = c("keyword" = "label")
  ) %>%
  mutate(keyword = coalesce(concept_short, keyword)) %>%
  select(-concept_short)


keyword_counts <- subjects_long %>%
  count(keyword, sort = TRUE)

# TaDiRaH-Abgleich
english_keywords <- read.csv2("tadirah_dhd_en.csv")

english_keywords_norm <- english_keywords %>%
  mutate(label_norm = str_to_lower(str_squish(label)))


# Zusammenfassung: Häufigkeit pro TaDiRAH-Konzept
tadirah_hits <- keyword_counts %>%
  filter(keyword %in% english_keywords_norm$label_norm)

tadirah_hits <- keyword_counts %>%
  inner_join(english_keywords_norm, by = c("keyword" = "label_norm")) %>%
  group_by(concept_short) %>%
  summarise(
    total_count = sum(n, na.rm = TRUE),
    n_variants = n_distinct(keyword),
    .groups = "drop"
  ) %>%
  right_join(
    distinct(english_keywords_norm, concept_short, label),
    by = "concept_short"
  ) %>%
  mutate(
    total_count = replace_na(total_count, 0),
    n_variants = replace_na(n_variants, 0)
  ) %>%
  select(concept_short, label, total_count, n_variants) %>%
  arrange(desc(total_count))


# TaDiRAH-Treffer als Tabelle speichern
# write_csv2(tadirah_hits, "tadirah_hits.csv")

# GESAMT-PLOTS

# Top 30 Labels aus tadirah_hits
top30 <- tadirah_hits %>%
  slice_max(total_count, n = 30, with_ties = FALSE) %>%
  mutate(
    label = fct_reorder(label, total_count)  # Für sortierte Darstellung
  )

ggplot(top30, aes(x = total_count, y = label)) +
  geom_col(fill = "#004A7F") +  
  geom_text(
    aes(label = total_count),
    hjust = -0.3,
    size = 3
  ) +
  scale_x_continuous(
    expand = expansion(mult = c(0, 0.15))  # Platz für Labels
  ) +
  labs(
    x = "Anzahl der Paper",
    y = "Konzepte",
    title = "Top 30 TaDiRAH-Konzepte",
    caption = paste("n =", nrow(data))
  ) +
  theme_minimal() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 12),
    plot.caption = element_text(hjust = 0, size = 10),  # hjust = 0: linksbündig, hjust = 1: rechtsbündig
    axis.text.y = element_text(size = 9)
  )

# Top 30 Flops: aufsteigend sortiert (seltenste oben)
flops30 <- tadirah_hits %>%
  filter(total_count > 0) %>%
  slice_min(total_count, n = 30, with_ties = FALSE) %>%
  mutate(label = fct_reorder(label, desc(total_count)))  # Absteigend für Y-Achse = aufsteigende Werte von oben

ggplot(flops30, aes(x = total_count, y = label)) +
  geom_col(fill = "#8B8B8B") +
  geom_text(
    aes(label = total_count),
    hjust = -0.3,
    size = 3
  ) +
  scale_x_continuous(
    expand = expansion(mult = c(0, 0.15))
  ) +
  labs(
    x = "Anzahl der Paper",
    y = "Konzepte",
    title = "Flop 30  TaDiRAH-Konzepte",
    caption = paste("n =", nrow(data))
  ) +
  theme_minimal() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 12),
    plot.caption = element_text(hjust = 0, size = 10),  # hjust = 0: linksbündig, hjust = 1: rechtsbündig
    axis.text.y = element_text(size = 9)
  )

# PLOTS PRO SOURCE - TOP 10 UND FLOP 10

# SOURCE-BEREINIGUNG (um Subset je DHd-Konferenz bilden zu können)
data <- data %>%
  mutate(
    source_clean = source %>%
      str_sub(1, 9) %>%           # (a) ersten 9 Zeichen
      str_remove_all(" ") %>%      # (b) Leerzeichen entfernen
      str_to_lower() %>%           # (b) kleinschreiben
      str_replace("^vdhd.*$", "dhd2021") %>%  # (c) vdhd → dhd2021
      str_sub(1, 7) %>%            # (d) nach 7 Zeichen abschneiden
      str_remove_all("dhdjahr|digital") %>%  # dhdjahr und digital entfernen
      na_if("") #entfernt leere
  )

# Eindeutige Quellen
sources <- data %>%
  distinct(source_clean) %>%
  pull(source_clean) %>%
  sort()

# Funktion: Top 10 Plot pro Quelle
plot_source_top10 <- function(src, subtitle = NULL) {
  
  paper_ids_subset <- which(data$source_clean == src)
  n <- length(paper_ids_subset)  
  
  tadirah_in_subset <- subjects_long %>%
    filter(
      paper_id %in% paper_ids_subset,
      keyword %in% english_keywords_norm$label_norm
    )
  
  if (nrow(tadirah_in_subset) == 0) {
    return(
      ggplot() +
        annotate("text", x = 1, y = 1, label = "keine TaDiRAH-Treffer") +
        labs(
          title = src,
          subtitle = subtitle,
          caption = paste("n =", n)
        ) +
        theme_void() +
        theme(
          plot.title = element_text(face = "bold", size = 10, hjust = 0.5),
          plot.subtitle = element_text(size = 9, hjust = 0.5),
          plot.caption = element_text(hjust = 0, size = 8)
        )
    )
  }
  
  top10 <- tadirah_in_subset %>%
    inner_join(
      english_keywords_norm %>% select(label_norm, concept_short),
      by = c("keyword" = "label_norm")
    ) %>%
    count(concept_short, sort = TRUE) %>%
    slice_max(n, n = 10, with_ties = FALSE) %>%
    left_join(
      distinct(tadirah_hits, concept_short, label),
      by = "concept_short"
    ) %>%
    mutate(
      display_label = coalesce(label, concept_short)
    ) %>%
    select(display_label, n) %>%
    rename(label = display_label) %>%
    mutate(label = fct_reorder(label, n))
  
  ggplot(top10, aes(x = n, y = label)) +
    geom_col(fill = "#004A7F") +
    geom_text(aes(label = n), hjust = -0.3, size = 2.5) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.2))) +
    labs(
      title = src,
      subtitle = subtitle,
      x = "Anzahl der Paper",
      y = "Konzepte",
      caption = paste("n =", n)
    ) +
    theme_minimal() +
    theme(
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      plot.title = element_text(face = "bold", size = 10),
      plot.subtitle = element_text(size = 9),
      plot.caption = element_text(hjust = 0, size = 8),
      axis.text.y = element_text(size = 7, margin = margin(r = 5)),
      axis.text.x = element_text(size = 7),
      plot.margin = margin(t = 10, r = 10, b = 10, l = 10)
    )
}

# Individuelle Subtitel definieren
subtitles <- c(
  "dhd2014" = "Digital Humanities – methodischer Brückenschlag \n oder ‚feindliche Übernahme‘? \n Chancen und Risiken der Begegnung zwischen \n Geisteswissenschaften und Informatik",
  "dhd2015" = "Von Daten zu Erkenntnissen",
  "dhd2016" = "Modellierung, Vernetzung, Visualisierung",
  "dhd2017" = "Digitale Nachhaltigkeit",
  "dhd2018" = "Kritik der digitalen Vernunft",
  "dhd2019" = "Digital Humanities: multimedial & multimodal",
  "dhd2020" = "Spielräume – Digital Humanities zwischen Modellierung und Interpretation",
  "dhd2021" = "vDHd - Experimente ",
  "dhd2022" = "Kulturen des digitalen Gedächtnisses",
  "dhd2023" = "#DHd2023 Open Humanities Open Culture",
  "dhd2024" = "#Quo Vadis DH?",
  "dhd2025" = "Under Construction. Geisteswissenschaften und Data Humanities",
  "dhd2026" = "Nicht nur Text, nicht nur Daten",
  .default = NULL
)

# Alle Plots mit Subtiteln erstellen
plot_list <- map2(sources, subtitles[sources], plot_source_top10)

# In Grid mit 3 Spalten anordnen
wrap_plots(plot_list, ncol = 3) +
  plot_annotation(
    title = "Top 10 TaDiRAH-Konzepte nach Konferenz",
    theme = theme(
      plot.title = element_text(face = "bold", size = 14, hjust = 0.5)
    )
  )


# SOURCE-BEREINIGUNG (um Subset je DHd-Konferenz bilden zu können)
data <- data %>%
  mutate(
    source_clean = source %>%
      str_sub(1, 9) %>%           # (a) ersten 9 Zeichen
      str_remove_all(" ") %>%      # (b) Leerzeichen entfernen
      str_to_lower() %>%           # (b) kleinschreiben
      str_replace("^vdhd.*$", "dhd2021") %>%  # (c) vdhd → dhd2021
      str_sub(1, 7) %>%            # (d) nach 7 Zeichen abschneiden
      str_remove_all("dhdjahr|digital") %>%  # dhdjahr und digital entfernen
      na_if("") #entfernt leere
  )

# Eindeutige Quellen
sources <- data %>%
  distinct(source_clean) %>%
  pull(source_clean) %>%
  sort()

# Funktion: Top 10 Plot pro Quelle
plot_source_top10 <- function(src, subtitle = NULL) {
  
  paper_ids_subset <- which(data$source_clean == src)
  n <- length(paper_ids_subset)  # korrekt: length, nicht nrow
  
  tadirah_in_subset <- subjects_long %>%
    filter(
      paper_id %in% paper_ids_subset,
      keyword %in% english_keywords_norm$label_norm
    )
  
  if (nrow(tadirah_in_subset) == 0) {
    return(
      ggplot() +
        annotate("text", x = 1, y = 1, label = "keine TaDiRAH-Treffer") +
        labs(
          title = src,
          subtitle = subtitle,
          caption = paste("n =", n)
        ) +
        theme_void() +
        theme(
          plot.title = element_text(face = "bold", size = 10, hjust = 0.5),
          plot.subtitle = element_text(size = 9, hjust = 0.5),
          plot.caption = element_text(hjust = 0, size = 8)
        )
    )
  }
  
  top10 <- tadirah_in_subset %>%
    inner_join(
      english_keywords_norm %>% select(label_norm, concept_short),
      by = c("keyword" = "label_norm")
    ) %>%
    count(concept_short, sort = TRUE) %>%
    slice_max(n, n = 10, with_ties = FALSE) %>%
    left_join(
      distinct(tadirah_hits, concept_short, label),
      by = "concept_short"
    ) %>%
    mutate(
      display_label = coalesce(label, concept_short)
    ) %>%
    select(display_label, n) %>%
    rename(label = display_label) %>%
    mutate(label = fct_reorder(label, n))
  
  ggplot(top10, aes(x = n, y = label)) +
    geom_col(fill = "#004A7F") +
    geom_text(aes(label = n), hjust = -0.3, size = 2.5) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.2))) +
    labs(
      title = src,
      subtitle = subtitle,
      x = "Anzahl der Paper",
      y = "Konzepte",
      caption = paste("n =", n)
    ) +
    theme_minimal() +
    theme(
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      plot.title = element_text(face = "bold", size = 10),
      plot.subtitle = element_text(size = 9),
      plot.caption = element_text(hjust = 0, size = 8),
      axis.text.y = element_text(size = 7, margin = margin(r = 5)),
      axis.text.x = element_text(size = 7),
      plot.margin = margin(t = 10, r = 10, b = 10, l = 10)
    )
}

# Individuelle Subtitel definieren
subtitles <- c(
  "dhd2014" = "Digital Humanities – methodischer Brückenschlag \n oder ‚feindliche Übernahme‘? \n Chancen und Risiken der Begegnung zwischen \n Geisteswissenschaften und Informatik",
  "dhd2015" = "Von Daten zu Erkenntnissen",
  "dhd2016" = "Modellierung, Vernetzung, Visualisierung",
  "dhd2017" = "Digitale Nachhaltigkeit",
  "dhd2018" = "Kritik der digitalen Vernunft",
  "dhd2019" = "Digital Humanities: multimedial & multimodal",
  "dhd2020" = "Spielräume – Digital Humanities zwischen Modellierung und Interpretation",
  "dhd2021" = "vDHd - Experimente ",
  "dhd2022" = "Kulturen des digitalen Gedächtnisses",
  "dhd2023" = "#DHd2023 Open Humanities Open Culture",
  "dhd2024" = "#Quo Vadis DH?",
  "dhd2025" = "Under Construction. Geisteswissenschaften und Data Humanities",
  "dhd2026" = "Nicht nur Text, nicht nur Daten",
  .default = NULL
)

# Alle Plots mit Subtiteln erstellen
plot_list <- map2(sources, subtitles[sources], plot_source_top10)

# In Grid mit 3 Spalten anordnen
wrap <- wrap_plots(plot_list, ncol = 3) +
  plot_annotation(
    title = "Top 10 TaDiRAH-Konzepte nach Konferenz",
    theme = theme(
      plot.title = element_text(face = "bold", size = 14, hjust = 0.5)
    )
  )


# Funktion: Flop 10 Plot pro Quelle
plot_source_flop10 <- function(src, subtitle = NULL) {
  
  paper_ids_subset <- which(data$source_clean == src)
  n <- length(paper_ids_subset)
  
  tadirah_in_subset <- subjects_long %>%
    filter(
      paper_id %in% paper_ids_subset,
      keyword %in% english_keywords_norm$label_norm
    )
  
  if (nrow(tadirah_in_subset) == 0) {
    return(
      ggplot() +
        annotate("text", x = 1, y = 1, label = "keine TaDiRAH-Treffer") +
        labs(
          title = src,
          subtitle = subtitle,
          caption = paste("n =", n)
        ) +
        theme_void() +
        theme(
          plot.title = element_text(face = "bold", size = 10, hjust = 0.5),
          plot.subtitle = element_text(size = 9, hjust = 0.5),
          plot.caption = element_text(hjust = 0, size = 8)
        )
    )
  }
  
  flop10 <- tadirah_in_subset %>%
    inner_join(
      english_keywords_norm %>% select(label_norm, concept_short),
      by = c("keyword" = "label_norm")
    ) %>%
    count(concept_short, sort = TRUE) %>%
    slice_min(n, n = 10, with_ties = FALSE) %>%
    left_join(
      distinct(tadirah_hits, concept_short, label),
      by = "concept_short"
    ) %>%
    mutate(
      display_label = coalesce(label, concept_short)
    ) %>%
    select(display_label, n) %>%
    rename(label = display_label) %>%
    mutate(label = fct_reorder(label, n, .desc = TRUE))  # aufsteigend: kleinste n oben
  
  ggplot(flop10, aes(x = n, y = label)) +
    geom_col(fill = "#A52A2A") +
    geom_text(aes(label = n), hjust = -0.3, size = 2.5) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.2))) +
    labs(
      title = src,
      subtitle = subtitle,
      x = "Anzahl der Paper",
      y = "Konzepte",
      caption = paste("n =", n)
    ) +
    theme_minimal() +
    theme(
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      plot.title = element_text(face = "bold", size = 10),
      plot.subtitle = element_text(size = 9),
      plot.caption = element_text(hjust = 0, size = 8),
      axis.text.y = element_text(size = 7, margin = margin(r = 5)),
      axis.text.x = element_text(size = 7),
      plot.margin = margin(t = 10, r = 10, b = 10, l = 10)
    )
}
# Flop-Plots erstellen
flop_list <- map2(sources, subtitles[sources], plot_source_flop10)

# Grid mit 3 Spalten
wrap_plots(flop_list, ncol = 3) +
  plot_annotation(
    title = "Flop 10 TaDiRAH-Konzepte nach Konferenz",
    theme = theme(
      plot.title = element_text(face = "bold", size = 14, hjust = 0.5)
    )
  )


###-------------------------------------------------------------
### NETZWERKANALYSE mit TaDiRAH-Konzepten
### ------------------------------------------------------------

# -------------------------------------------------------------------------
# 0. Daten auf TaDiRAH-Ebene aggregieren
# -------------------------------------------------------------------------

# Prüfen, welche Keywords kein TaDiRAH-Mapping haben
unmapped <- subjects_long %>%
  filter(is.na(keyword)) %>%
  distinct(keyword)

# Für Netzwerk: concept_short verwenden, falls vorhanden; sonst Original-keyword
papers_concepts <- subjects_long %>%
  mutate(
    node_label = coalesce(keyword)
  ) %>%
  distinct(paper_id, node_label)

# Häufigkeit der Knotenlabels für spätere Visualisierung
node_counts <- papers_concepts %>%
  count(node_label, name = "total", sort = TRUE)

# -------------------------------------------------------------------------
# 1. Paarweise Co-Occurrence auf Konzept-Ebene
# -------------------------------------------------------------------------

concept_pairs <- papers_concepts %>%
  pairwise_count(node_label, paper_id, sort = TRUE)

# Schwellwert anpassen (weniger Kanten erwartet auf höherer Aggregationsebene)
min_cooccur <- 20

concept_pairs_filtered <- concept_pairs %>%
  filter(n >= min_cooccur)

connected_concepts <- concept_pairs_filtered %>%
  pivot_longer(cols = c(item1, item2), values_to = "node_label") %>%
  distinct(node_label) %>%
  pull(node_label)

# -------------------------------------------------------------------------
# 2. Graph aufbauen mit TaDiRAH-Metadaten
# -------------------------------------------------------------------------

# TaDiRAH-Hierarchie einlesen für Knotenfärbung/Gruppierung
tadirah_meta <- english_keywords_norm %>%
  transmute(
    concept_short = str_to_lower(concept_short),
    label,
    tadirah_category = "TaDiRAH"  # alle gleich, Unterscheidung nur TaDiRAH/sonst
  )

concept_graph <- concept_pairs_filtered %>%
  as_tbl_graph(directed = FALSE) %>%
  activate(nodes) %>%
  left_join(node_counts, by = c("name" = "node_label")) %>%
  left_join(
    tadirah_meta %>% select(concept_short, tadirah_category),
    by = c("name" = "concept_short")
  ) %>%
  mutate(
    tadirah_category = replace_na(tadirah_category, "non-TaDiRAH"),
    is_tadirah = !is.na(tadirah_category) & tadirah_category != "non-TaDiRAH"
  ) %>%
  filter(name %in% connected_concepts) %>%
  mutate(
    degree = centrality_degree(weights = n),
    betweenness = centrality_betweenness(weights = n, normalized = TRUE),
    community = as.factor(group_louvain(weights = n)),
    
    # Größenkategorien basierend auf Häufigkeit
    size_cat = case_when(
      total >= quantile(total, 0.80, na.rm = TRUE) ~ "Large",
      total >= quantile(total, 0.45, na.rm = TRUE) ~ "Medium",
      TRUE ~ "Small"
    ),
    size_cat = factor(size_cat, levels = c("Small", "Medium", "Large"))
  )

max_cooccur <- concept_graph %>% activate(edges) %>% pull(n) %>% max()
min_edge <- concept_graph %>% activate(edges) %>% pull(n) %>% min()


# -------------------------------------------------------------------------
# 3. Plot
# -------------------------------------------------------------------------

set.seed(42)

ggraph(concept_graph, layout = "stress") +
  theme(
    plot.background = element_rect(fill = "#F5F5F5", colour = "#CCCCCC", linewidth = 0.5),
    panel.background = element_rect(fill = "#FAFAFA", colour = NA)
  ) +
  
  # KANTEN
  geom_edge_link0(
    aes(alpha = n, width = n),
    colour = "#888888",
    lineend = "round"
  ) +
  
  scale_edge_alpha(
    range = c(0.1, 0.45),
    breaks = c(min_edge, max_cooccur),
    labels = c("niedrig", "hoch"),
    name = "Co-Occurrence"
  ) +
  
  scale_edge_width(range = c(0.3, 2), guide = "none") +
  
  # KNOTEN: Halo-Effekt
  geom_node_point(aes(size = size_cat), colour = "white", alpha = 1) +
  
  # KNOTEN: Hauptpunkte
  geom_node_point(
    aes(size = size_cat),
    colour = "#333333",
    fill = "#888888",
    shape = 21,
    stroke = 0.6,
    alpha = 0.92
  ) +
  
  scale_size_manual(
    values = c("Small" = 5, "Medium" = 12, "Large" = 28),
    name = "Häufigkeit",
    breaks = c("Small", "Medium", "Large"),
    labels = c("Niedrig", "Mittel", "Hoch")
  ) +
  
  # LABELS
  geom_node_text(
    aes(label = name, size = size_cat),
    colour = "#333333",
    repel = TRUE,
    max.overlaps = Inf,
    force = 2.5,
    force_pull = 1.0,
    box.padding = 0.2,
    point.padding = 0.2,
    segment.size = 0.15,
    segment.alpha = 0.3,
    segment.colour = "#666666",
    fontface = "bold",
    family = "sans"
  ) +
  
  scale_size_manual(
    aesthetics = "size",
    values = c("Small" = 2.8, "Medium" = 3.5, "Large" = 4.5),
    guide = "none"
  ) +
  
  coord_fixed(clip = "off", expand = TRUE) +
  
  labs(
    title = "Konzept Co-Occurrence Network",
    subtitle = paste0(
      vcount(concept_graph), " Konzepte, ", 
      ecount(concept_graph), " Verbindungen (≥", min_cooccur, ")"
    ),
    edge_alpha = "Co-Occurrence"
  ) +
  
  theme_graph(base_family = "sans", base_size = 10) +
  theme(
    aspect.ratio = 1,
    plot.title = element_text(face = "bold", size = 14, hjust = 0, 
                              margin = margin(b = 2)),
    plot.subtitle = element_text(size = 9, colour = "#555555", hjust = 0,
                                 margin = margin(b = 10)),
    plot.margin = margin(20, 20, 20, 20),
    legend.position = "bottom",
    legend.box = "vertical",
    legend.margin = margin(t = 5),
    legend.key.size = unit(0.5, "cm"),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 9, face = "bold"),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )