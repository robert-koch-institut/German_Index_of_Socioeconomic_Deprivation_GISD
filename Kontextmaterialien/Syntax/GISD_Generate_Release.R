# GISD - German Index of Socio-Economic Deprivation
# Author: Lola Omar Soliman
# Citation: https://github.com/robert-koch-institut/German_Index_of_Socioeconomic_Deprivation_GISD

# Revision: 2026_v01
# Date: 2026-05-19

# 0. Setup =====================================================================

library(tidyverse)  # Tidyverse Methoden
library(readxl)     # Excel-Files lesen
library(writexl)    # Excel-Files schreiben
library(scales)     # Skalierungsfunktionen

# Skriptpfad als Arbeitspfad setzen
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Datenjahrspanne, Revisionsnummer, Jahr des Gebietsstands
year_min <- 1998
year_max <- 2023
currentrevision <- "2026_v01"
gebietsstand <- 2023

# Pfade für Input/Output
dir_input <- "../Rohdaten/"
dir_output_tsv <- "../../"
dir_output_xlsx <- "../../GISD_Release_aktuell/"

# Dezimal-Zahlendarstellung erzwingen (Wegen Gebietskennziffern)
options(scipen = 999)

# I. ID-Datensatz generieren ===================================================

# (Quelle: https://www.bbsr.bund.de/BBSR/DE/forschung/raumbeobachtung/Raumabgrenzungen/downloads/download-referenzen.html)

bundeslaender <- tibble(
  Bundesland_Kennziffer = sprintf("%02d", 1:16),
  Bundesland = c("Schleswig-Holstein", "Hamburg", "Niedersachsen", "Bremen", 
                 "Nordrhein-Westfalen", "Hessen", "Rheinland-Pfalz",
                 "Baden-Württemberg", "Bayern", "Saarland", "Berlin",
                 "Brandenburg", "Mecklenburg-Vorpommern", "Sachsen",
                 "Sachsen-Anhalt", "Thüringen"))

id_dataset <- read_excel(paste0(dir_input,"Referenz/",
                                "raumgliederungen-referenzen-",gebietsstand,".xlsx"),
                         sheet = "Gemeindereferenz (inkl. Kreise)") %>% 
  slice(-1) %>% # Erste Zeile kann raus
  # Variablen formatieren (vor allem Kennziffern als Strings mit Leading Zeros)
  mutate(
    Gemeindekennziffer = str_pad(GEM2023, width = 8, pad = "0"),
    Gemeindename = GEM_NAME,
    Bevoelkerung = as.numeric(bev23)/100, # Bevölkerung muss in 100 angegeben werden (Wichtig!)
    GVBKennziffer = str_pad(VWG2023, width = 9, pad = "0"),
    GVBName = VWG_NAME,
    Kreiskennziffer = str_pad(str_sub(KRS2023, end = -4),
                              width = 5, pad = "0"),
    Kreisname = KRS_NAME,
    ROR_Kennziffer = str_pad(KRO2023, width = 4, pad = "0"),
    ROR_Name = KRO_NAME,
    NUTS2_Kennziffer = N2D2023,
    NUTS2_Name = N2D_NAME,
    Bundesland_Kennziffer = str_sub(Gemeindekennziffer, 1, 2)) %>%
  left_join(bundeslaender, by = "Bundesland_Kennziffer") %>%
  select(Gemeindekennziffer, Gemeindename,
         Bevoelkerung,
         GVBKennziffer, GVBName,
         Kreiskennziffer, Kreisname,
         ROR_Kennziffer, ROR_Name,
         NUTS2_Kennziffer, NUTS2_Name,
         Bundesland_Kennziffer, Bundesland)

rm(bundeslaender)

# # Auf Missings prüfen
# nrow(id_dataset %>% filter(if_any(everything(), is.na))) # keine Missings

# II. Indikatoren einlesen =====================================================

## Rohdaten über Schleife einlesen
# Bilden eines Datensatz-"Skeletts" beginnend mit Kreisebene und neuestem Datenjahr
Basedata <- id_dataset %>% 
  distinct(Kreiskennziffer) %>% 
  rename(Kennziffer = Kreiskennziffer) %>% 
  mutate(Jahr = year_max)

# Inputliste der hinzuzufügenden Rohdaten
fileinputlist <- list.files(paste0(dir_input,"INKAR_1998_",year_max),
                            # Nur Excel-Dateien (".xls" und ".xlsx")
                            pattern = "\\.xls(x)?$",
                            # Inklusive Unterordner (Kreisdaten für 1998-2000)
                            recursive = TRUE, include.dirs = TRUE,
                            # Dateipfade bewahren
                            full.names = TRUE) %>%
  # Beschäftigtenabschlüsse pre-2012 ergänzen
  c(., c(paste0(dir_input, "Referenz/Agentur_fuer_Arbeit/",
                "B_BeschaeftigtemitakadAbschluss-pre2012.xlsx"),
         paste0(dir_input, "Referenz/Agentur_fuer_Arbeit/",
                "B_BeschaeftigteohneAbschluss-pre2012.xlsx")))

# Einlesen der Inputs (jedes Jahr und jede Region ebenenunabhängig als
# eigene Zeile, je nach Ebene mit den entsprechenden Indikatoren befüllt)
for (filename in fileinputlist) {
  temp_import <- read_excel(filename,
                            sheet = "Daten",
                            skip = 1) %>%
    rename(Kennziffer = 1) %>% 
    select(-(2:3)) %>%
    # Jahr von wide zu long
    pivot_longer(
      cols = -Kennziffer, # Alle Spalten außer Kennziffer (ergo die Jahre)
      names_to = "Jahr",
      names_transform = as.numeric,
      # Dateiname als Variablenname
      values_to = str_extract(filename, # Ziehe aus Filename alles:
                              paste0("(?<=_)", # Nach dem letzten Unterstrich
                                     "[^_.]+", # Keine "_" oder "."
                                     "(?=\\.)") # Vor dem ersten Punkt
      )) %>% 
    filter(Jahr %in% year_min:year_max)
  
  # Zu Basedata hinzufügen
  Basedata <- full_join(Basedata, temp_import,
                        by = c("Kennziffer", "Jahr"))
}

rm(fileinputlist, filename, temp_import)

## Anpassung:
# Da INKAR noch keine Daten zu Bruttoverdienst oder Haushaltseinkommen für 2023
# veröffentlicht hat, werden fast identische Daten der VGR für eine Regression
# herangezogen, um die "INKAR-Version" für 2023 zu schätzen.

# (Quelle: https://www.statistikportal.de/de/vgrdl/ergebnisse-kreisebene/einkommen-kreise)

# Bruttoverdienst pro Kopf
vgr_bv <- read_excel(paste0(dir_input,"Referenz/VGR/",
                            "vgrdl_r2b2_bs2024_2.xlsx"),
                     sheet = "5",
                     skip = 4) %>% 
  filter(!is.na(`NUTS 3`)) %>%
  select(Kennziffer = `Regional-schlüssel`,
         starts_with("20")) %>% 
  mutate(Kennziffer = str_pad(Kennziffer, 5, "right", pad = "0")) %>% 
  pivot_longer(cols = -Kennziffer,
               names_to = "Jahr",
               names_transform = as.numeric,
               values_to = "Bruttoverdienst_VGR") %>% 
  # Bruttoverdienst jährlich -> monatlich (wie bei INKAR)
  mutate(Bruttoverdienst_VGR = Bruttoverdienst_VGR / 12)

# Monatliches Haushaltseinkommen pro Kopf
vgr_hh <- read_excel(paste0(dir_input,"Referenz/VGR/",
                            "vgrdl_r2b3_bs2024.xlsx"),
                     sheet = "2.4",
                     skip = 4) %>% 
  filter(!is.na(`NUTS 3`)) %>%
  select(Kennziffer = `Regional-schlüssel`,
         starts_with("20")) %>% 
  mutate(Kennziffer = str_pad(Kennziffer, 5, "right", pad = "0")) %>% 
  pivot_longer(cols = -Kennziffer,
               names_to = "Jahr",
               names_transform = as.numeric,
               values_to = "Haushaltseinkommen_VGR") %>% 
  # Haushaltseinkommen jährlich -> monatlich (wie bei INKAR)
  mutate(Haushaltseinkommen_VGR = Haushaltseinkommen_VGR / 12)

# VGR-Daten anbinden
Basedata <- Basedata %>% 
  left_join(vgr_bv, by = c("Kennziffer", "Jahr")) %>% 
  left_join(vgr_hh, by = c("Kennziffer", "Jahr"))

# ## Stärke des Jahreseffekts zwischen INKAR- und VGR-Version vergleichen
# coef_df <- Basedata %>% 
#   filter(Jahr %in% 2014:2022,
#          nchar(Kennziffer) == 5) %>% 
#   select(Kennziffer, Jahr,
#          starts_with(c("Brutto", "Haushalt")))
# 
# # Liste mit Koeffizienten (nur Jahr als Prädiktor)
# coefs <- list("Bruttoverdienst (INKAR)" =
#                 lm(Bruttoverdienst ~ Jahr,
#                    data = coef_df)$coefficients["Jahr"],
#               "Bruttoverdienst (VGR)" = 
#                 lm(Bruttoverdienst_VGR ~ Jahr,
#                    data = coef_df)$coefficients["Jahr"],
#               "Haushaltseinkommen (INKAR) " = 
#                 lm(Haushaltseinkommen ~ Jahr,
#                    data = coef_df)$coefficients["Jahr"],
#               "Haushaltseinkommen (VGR)" = 
#                 lm(Haushaltseinkommen_VGR ~ Jahr,
#                    data = coef_df)$coefficients["Jahr"]) %>%
#   lapply(round, 2)
# 
# cat("Vergleich der Koeffizienten für den Jahreseffekt zwischen VGR- und INKAR-Daten")
# for (ind in names(coefs)) {
#   cat(paste0("Effektstärke Jahr für ",ind,": ",coefs[[ind]],"\n"))
# }
# 
# # Jahr und Kreis-Dummies als Prädiktoren
# coefs <- list("Bruttoverdienst (INKAR)" =
#                 lm(Bruttoverdienst ~ Jahr + as.factor(Kennziffer),
#                    data = coef_df)$coefficients["Jahr"],
#               "Bruttoverdienst (VGR)" = 
#                 lm(Bruttoverdienst_VGR ~ Jahr + as.factor(Kennziffer),
#                    data = coef_df)$coefficients["Jahr"],
#               "Haushaltseinkommen (INKAR) " = 
#                 lm(Haushaltseinkommen ~ Jahr + as.factor(Kennziffer),
#                    data = coef_df)$coefficients["Jahr"],
#               "Haushaltseinkommen (VGR)" = 
#                 lm(Haushaltseinkommen_VGR ~ Jahr + as.factor(Kennziffer),
#                    data = coef_df)$coefficients["Jahr"]) %>%
#   lapply(round, 2)
# 
# cat("Vergleich der Koeffizienten für den Jahreseffekt zwischen VGR- und INKAR-Daten (mit Kontrolle auf Kreis-Dummies")
# for (ind in names(coefs)) {
#   cat(paste0("Effektstärke Jahr für ",ind,": ",coefs[[ind]],"\n"))
# } # Fazit: Effektstärken ähnlich genug, Kontrolle auf Jahr verzichtbar
# 
# rm(coef_df, coefs, ind)

# Koeffizienten berechnen
model_bv <- lm(Bruttoverdienst ~ Bruttoverdienst_VGR,
               data = Basedata %>% filter(Jahr %in% 2022))

model_hh <- lm(Haushaltseinkommen ~ Haushaltseinkommen_VGR,
               data = Basedata %>% filter(Jahr %in% 2022))

# INKAR-Versionen der Variablen für 2023 schätzen
Basedata <- Basedata %>% 
  mutate(Bruttoverdienst =    ifelse(Jahr == 2023,
                                     predict(model_bv, newdata = .),
                                     Bruttoverdienst),
         Haushaltseinkommen = ifelse(Jahr == 2023,
                                     predict(model_hh, newdata = .),
                                     Haushaltseinkommen))

rm(vgr_bv, vgr_hh,
   model_bv, model_hh)

## Anpassung:
# INKAR gibt keine Daten zu Beschäftigtenabschlüssen vor 2012 raus. Daher wurden
# ergänzende Daten von der Statistik der Bundesagentur für Arbeit erworben und
# manuell zu Gebietsstand 2022 harmonisiert. Die Daten vor und nach 2012 werden
# hier zusammengeführt. Zusätzlich sind es vor 2012 Absolutzahlen und nach 2012
# Werte relativ zu SV-Beschäftigten, also werden die Werte vor 2012 gegen die
# Anzahl der SV-Beschäftigten gerechnet.
Basedata <- Basedata %>% 
  mutate(`BeschaeftigtemitakadAbschluss-pre2012` = `BeschaeftigtemitakadAbschluss-pre2012` / SVBeschaeftigte * 100,
         `BeschaeftigteohneAbschluss-pre2012`    = `BeschaeftigteohneAbschluss-pre2012`    / SVBeschaeftigte * 100,
         BeschaeftigtemitakadAbschluss = ifelse(Jahr <= 2011, `BeschaeftigtemitakadAbschluss-pre2012`, BeschaeftigtemitakadAbschluss),
         BeschaeftigteohneAbschluss    = ifelse(Jahr <= 2011, `BeschaeftigteohneAbschluss-pre2012`   , BeschaeftigteohneAbschluss)) %>% 
  select(-`BeschaeftigtemitakadAbschluss-pre2012`,
         -`BeschaeftigteohneAbschluss-pre2012`,
         - SVBeschaeftigte) # SV-Beschäftigte nicht mehr benötigt

## Basisdaten auf Gebietsebenen mit ihren entsprechenden Indikatoren aufteilen
# Gemeinden
Basedata_Gemeindeebene <- Basedata %>% 
  select(Gemeindekennziffer = Kennziffer,
         Jahr,
         Arbeitslosigkeit,
         Beschaeftigtenquote,
         ErwerbsfaehigeBevoelkerung,
         Einkommensteuer)

# Gemeindeverbände (Momentan keine Variablen, kann sich in Zukunft ändern)
Basedata_Gemeindeverbandsebene <- Basedata %>%
  select(GVBKennziffer = Kennziffer,
         Jahr)

# Kreise
Basedata_Kreisebene <- Basedata %>%
  select(Kreiskennziffer = Kennziffer,
         Jahr,
         Bruttoverdienst,
         BeschaeftigtemitakadAbschluss,
         BeschaeftigteohneAbschluss,
         SchulabgaengerohneAbschluss,
         Haushaltseinkommen,
         Schuldnerquote,
         # Ergänzende Kreisdaten
         ErwerbsfaehigeBevoelkerungKreis,
         BeschaeftigtenquoteKreis,
         ArbeitslosigkeitKreis)

# Regionalebenen ineinander verschachteln
Workfile <- id_dataset %>% 
  # ID-Datensatz mit Jahren long "auffächern"
  expand_grid(Jahr = year_min:year_max) %>% 
  select(Gemeindekennziffer,
         Jahr,
         Bevoelkerung,
         GVBKennziffer,
         Kreiskennziffer,
         Bundesland) %>% 
  left_join(Basedata_Gemeindeebene, by = c("Gemeindekennziffer", "Jahr")) %>% 
  left_join(Basedata_Gemeindeverbandsebene, by = c("GVBKennziffer", "Jahr")) %>%
  left_join(Basedata_Kreisebene, by = c("Kreiskennziffer", "Jahr"))

rm(Basedata_Gemeindeebene,
   Basedata_Gemeindeverbandsebene,
   Basedata_Kreisebene)

# gem_nopop <- Workfile %>% filter(Bevoelkerung == 0)

# Finale Aufbereitungsschritte
Workfile <- Workfile %>%
  filter(Bevoelkerung > 0) %>% # Entfernen bevölkerungsloser Gemeinden
  mutate(
    # Arbeitslose anteilig gegen erwerbsfähige Bevölkerung rechnen
    Arbeitslosigkeit = Arbeitslosigkeit / ErwerbsfaehigeBevoelkerung * 100,
    ArbeitslosigkeitKreis = ArbeitslosigkeitKreis / ErwerbsfaehigeBevoelkerungKreis * 100,
    
    # Ersetzen fehlender Gemeindedaten durch Kreisdaten für 1998-2000
    Arbeitslosigkeit           = ifelse(Jahr < 2001, ArbeitslosigkeitKreis, Arbeitslosigkeit),
    Beschaeftigtenquote        = ifelse(Jahr < 2001, BeschaeftigtenquoteKreis, Beschaeftigtenquote),
    ErwerbsfaehigeBevoelkerung = ifelse(Jahr < 2001, ErwerbsfaehigeBevoelkerungKreis, ErwerbsfaehigeBevoelkerung),
    
    # Implausible Arbeitslosigkeiten ersetzen
    Arbeitslosigkeit = case_when(
      ErwerbsfaehigeBevoelkerung == 0 ~ ArbeitslosigkeitKreis,
      Arbeitslosigkeit <= 0           ~ ArbeitslosigkeitKreis,
      Arbeitslosigkeit >= 100         ~ ArbeitslosigkeitKreis,
      TRUE                            ~ Arbeitslosigkeit
    ),
    
    # Implausible Beschäftigtenquoten ersetzen
    Beschaeftigtenquote = case_when(
      Beschaeftigtenquote == 0 ~ BeschaeftigtenquoteKreis,
      Beschaeftigtenquote > 80 ~ 80, # Bei 80% deckeln
      TRUE                     ~ Beschaeftigtenquote
    )
  ) %>%
  select(
    -ArbeitslosigkeitKreis,
    -BeschaeftigtenquoteKreis,
    -ErwerbsfaehigeBevoelkerungKreis,
    -ErwerbsfaehigeBevoelkerung
  ) %>%
  arrange(Gemeindekennziffer, Jahr)

# III. Anpassungen =============================================================

## 1. Verbraucherpreisindex und Logarithmierung ====
# Quelle VBP-Index (2020 = 100): <https://www-genesis.destatis.de/datenbank/online/statistic/61111/table/61111-0001>

vbp <- read_excel(paste0(dir_input,
                         "Referenz/verbraucherpreisindex.xlsx"),
                  col_names = c("Jahr", "vbp"),
                  col_types = "numeric",
                  range = "A13:B39")

Workfile <- Workfile %>%
  left_join(vbp, by = "Jahr") %>%
  mutate(Bruttoverdienst    = Bruttoverdienst    / vbp * 100,
         Einkommensteuer    = Einkommensteuer    / vbp * 100,
         Haushaltseinkommen = Haushaltseinkommen / vbp * 100,
         Bruttoverdienst_ln = log(Bruttoverdienst),
         Haushaltseinkommen_ln = log(Haushaltseinkommen),
         # log(x = 0) = NaN. Daher auf 0.75 setzen, so als wäre Einkommensteuer = 2.12
         Einkommensteuer_ln = ifelse(Einkommensteuer == 0, 0.75, log(Einkommensteuer)),
         # log(x < 0) = NaN. Daher auf 0.25 setzen, so als wäre Einkommensteuer = 1.28
         Einkommensteuer_ln = ifelse(Einkommensteuer < 0, 0.25, Einkommensteuer_ln)) %>% 
  select(-vbp)

rm(vbp)

## 2. G8/G9-Reformen ====
# Adjustment des Indikators "Schulabgänger ohne Abschluss" anhand regions-
# und periodenspezifischer bildungspolitischer Reformeffekte
# (Für mehr Kontext siehe "Input/Referenz/G8-Reform")

# Markieren der von Reformeffekten betroffenen Jahr-Bundesland-Paare
Workfile <- Workfile %>%
  mutate(
    # Doppelte Abschlüsse nach Einführung G8,
    G8_jahr = case_when(
      Bundesland == "Baden-Württemberg"       & Jahr == 2012 ~ 1,
      Bundesland == "Bayern"                  & Jahr == 2011 ~ 1,
      Bundesland == "Berlin"                  & Jahr == 2012 ~ 1,
      Bundesland == "Brandenburg"             & Jahr == 2012 ~ 1,
      Bundesland == "Bremen"                  & Jahr == 2012 ~ 1,
      Bundesland == "Hamburg"                 & Jahr == 2010 ~ 1,
      Bundesland == "Hessen"                  & Jahr == 2013 ~ 1,
      Bundesland == "Mecklenburg-Vorpommern"  & Jahr == 2008 ~ 1,
      Bundesland == "Niedersachsen"           & Jahr == 2011 ~ 1,
      Bundesland == "Nordrhein-Westfalen"     & Jahr == 2013 ~ 1,
      Bundesland == "Saarland"                & Jahr == 2009 ~ 1,
      Bundesland == "Sachsen-Anhalt"          & Jahr == 2007 ~ 1,
      Bundesland == "Schleswig-Holstein"      & Jahr == 2016 ~ 1,
      TRUE ~ 0),
    # Weniger Abschlüsse nach Rückkehr zu G9
    G9_jahr = case_when(
      Bundesland == "Baden-Württemberg"       & Jahr == 2020 ~ 1,
      Bundesland == "Bayern"                  & Jahr == 2025 ~ 1,
      Bundesland == "Niedersachsen"           & Jahr == 2023 ~ 1,
      Bundesland == "Nordrhein-Westfalen"     & Jahr == 2027 ~ 1,
      Bundesland == "Schleswig-Holstein"      & Jahr == 2027 ~ 1,
      TRUE ~ 0),
    # Sachsen-Anhalt 2001 viele Schulabgänger ohne Hauptschulabschluss durch Umstellung Schuljahre
    SN_OA = case_when(
      Bundesland == "Sachsen-Anhalt"          & Jahr == 2001 ~ 1,
      TRUE ~ 0),
    # Thüringen vor 2004 abweichende Anerkennung von Gym-Abschlüssen
    THvor2004 = case_when(
      Bundesland == "Thüringen"               & Jahr < 2004 ~ 1,
      TRUE ~ 0))

# Funktion zum Ersetzen der Werte in den von Verzerrungen betroffenen Fällen durch um Reformeffekte bereinigte Quoten
adjust_g8 <- function(data, outcome_name) {
  
  # Datensatz aufbereiten
  regdata <- data %>%
    select(Gemeindekennziffer, Jahr,
           G8_jahr, G9_jahr, SN_OA, THvor2004,
           Outcome = paste(outcome_name)) %>% 
    # Zeitreihendurchschnitt jeder Gemeinde
    mutate(.by = Gemeindekennziffer,
           MEAN = mean(Outcome, na.rm = TRUE))
  
  # Regression durchführen (Effekt der Reformen auf Schulabgängerquoten)
  reg_g8 <- lm(Outcome ~
                 I(Jahr*Jahr*MEAN) + I(Jahr*MEAN) +
                 G8_jahr + G9_jahr + SN_OA + THvor2004,
               data = regdata,
               na.action = "na.exclude")
  
  # Koeffizienten notieren
  coefs <- coef(reg_g8)[c("G8_jahr", "G9_jahr", "SN_OA", "THvor2004")]
  
  # # Werte prüfen
  # print(reg_g8)
  
  # Koeffizient des Effekts von Indikator abziehen (wenn von Reform betroffen)
  regdata %>%
    mutate(Outcome = Outcome
           # Koeffizienten abziehen aber nur wenn Markervariable == 1
           - (G8_jahr   * coefs["G8_jahr"])
           - (G9_jahr   * coefs["G9_jahr"])
           - (SN_OA     * coefs["SN_OA"])
           - (THvor2004 * coefs["THvor2004"])) %>%
    pull(Outcome) # Bereinigten Wert ausgeben
}

# Adjustment auf Indikator anwenden
Workfile <- Workfile %>% 
  mutate(SchulabgaengerohneAbschluss_adj = adjust_g8(.,"SchulabgaengerohneAbschluss"))

rm(adjust_g8)

## 3. Beschäftigtenabschlüsse in den neuen Bundesländern ====

# Markieren der Kreise
Workfile <- Workfile %>% 
  mutate(ow = ifelse(as.numeric(Kreiskennziffer) < 11000, 0, 1))

# Funktion zum Ersetzen der Werte in den betroffenen Fällen durch um Ost-West-Effekte bereinigte Werte
adjust_ostwest <- function(data, outcome_name) {
  
  data <- Workfile
  outcome_name <- "BeschaeftigteohneAbschluss"
  
  # Datensatz aufbereiten
  regdata <- data %>%
    select(Gemeindekennziffer,
           Jahr,
           ow,
           Outcome = all_of(outcome_name)) %>% 
    mutate(Jahr_Dummy = relevel(as.factor(Jahr), ref = "2012"))
  
  # Regression durchführen (Effekt der Region auf Beschäftigtenabschlüsse + Jahresdummy + Interaktion der beiden)
  reg_ow <- lm(Outcome ~ Jahr_Dummy*ow,
               data = regdata,
               na.action = "na.exclude")
  
  # # Werte prüfen
  # print(reg_ow)
  
  # Koeffizient des Effekts von Indikator abziehen (wenn Teil der neuen Länder)
  regdata %>%
    mutate(
      # Ost-West-Effekt
      coef_ow = coef(reg_ow)["ow"],
      # Effekt des Datenjahres auf den Ost-West-Effekt
      coef_jahr_ow = coef(reg_ow)[paste0("Jahr_Dummy",Jahr,":ow")],
      coef_jahr_ow = ifelse(Jahr == 2012, 0, coef_jahr_ow),
      Outcome = ifelse(ow == 1,
                       # Ost-West-Effekt (+Interaktion) abziehen
                       Outcome - (coef_ow + coef_jahr_ow),
                       Outcome)) %>%
    pull(Outcome) # Bereinigten Wert ausgeben
}

# Adjustment auf Indikator anwenden
Workfile <- Workfile %>% 
  mutate(BeschaeftigteohneAbschluss_adj = adjust_ostwest(.,"BeschaeftigteohneAbschluss"))

rm(adjust_ostwest)

## 4. Messänderung SV-Beschäftigte ====

# Werte von 2013 auf 2012 übertragen und Messänderung markieren
Workfile <- Workfile %>% 
  select(-BeschaeftigteohneAbschluss) %>% 
  rename(BeschaeftigteohneAbschluss = BeschaeftigteohneAbschluss_adj) %>% 
  arrange(Jahr) %>% 
  mutate(.by = Gemeindekennziffer,
         # Betroffene Jahre markieren (Alles vor 2012)
         Messaenderung_Besch = ifelse(Jahr < 2012, 1, 0),
         # Ohne Abschluss von 2013 zu 2012 rüberkopieren
         BeschaeftigteohneAbschluss_adj =
           ifelse(Jahr == 2012,
                  lead(BeschaeftigteohneAbschluss, 1),
                  BeschaeftigteohneAbschluss),
         # Akad. Abschluss von 2013 zu 2012 rüberkopieren
         BeschaeftigtemitakadAbschluss_adj =
           ifelse(Jahr == 2012,
                  lead(BeschaeftigtemitakadAbschluss, 1),
                  BeschaeftigtemitakadAbschluss)) %>% 
  arrange(Gemeindekennziffer, Jahr)

# Funktion zum Ersetzen der Werte in den betroffenen Jahren durch um Messänderungs-Effekte bereinigte Werte
adjust_messaenderung <- function(data, outcome_name) {
  
  # Datensatz aufbereiten
  regdata <- data %>%
    select(Gemeindekennziffer,
           Jahr,
           Messaenderung_Besch,
           "Outcome" = paste(outcome_name)) %>% 
    mutate(MEAN=mean(Outcome, na.rm=TRUE))
  
  # Regression durchführen (Effekt der Messänderung auf Beschaeftigtenabschlüsse)
  reg_messaenderung <- lm(Outcome ~
                            I(Jahr*Jahr*MEAN) + I(Jahr*MEAN) + Messaenderung_Besch,
                          data = regdata,
                          na.action = "na.exclude")
  
  # # Werte prüfen
  # print(reg_messaenderung)
  
  # Koeffizient des Effekts von Indikator abziehen (für Beobachtungen vor 2012)
  regdata %>%
    mutate(coef_mb = coef(reg_messaenderung)["Messaenderung_Besch"],
           Outcome = ifelse(Messaenderung_Besch == 1, # (entspricht Jahr < 2012)
                            Outcome - coef_mb,
                            Outcome)) %>%
    pull(Outcome) # Bereinigten Wert ausgeben
}

# Adjustment auf Indikatoren anwenden
Workfile <- Workfile %>% 
  mutate(BeschaeftigteohneAbschluss_adj = adjust_messaenderung(.,"BeschaeftigteohneAbschluss_adj"),
         BeschaeftigtemitakadAbschluss_adj = adjust_messaenderung(.,"BeschaeftigtemitakadAbschluss_adj")) %>%
  # Sämtliche Markervariablen entfernen
  select(-G8_jahr, -G9_jahr, -SN_OA, -THvor2004,
         -ow, -Messaenderung_Besch)

rm(adjust_messaenderung)

# IV. Imputation fehlender Werte ===============================================

indikatoren <- c("BeschaeftigtemitakadAbschluss_adj", 
                 "BeschaeftigteohneAbschluss_adj", 
                 "SchulabgaengerohneAbschluss_adj",
                 "Arbeitslosigkeit", 
                 "Beschaeftigtenquote", 
                 "Bruttoverdienst_ln", 
                 "Einkommensteuer_ln", 
                 "Haushaltseinkommen_ln",
                 "Schuldnerquote")

# Funktion zum Imputieren anhand des Zeitreihenmittelwerts
reg_impute <- function(data, outcome_name) {
  
  # Datensatz aufbereiten
  regdata <- data %>%
    select(Gemeindekennziffer, Jahr,
           "Outcome" = paste(outcome_name)) %>%
    # Zeitreihenmittelwert jeder Gemeinde berechnen
    mutate(.by = Gemeindekennziffer,
           MEAN = mean(Outcome, na.rm=TRUE))
  
  # Regression durchführen (Effekt des Zeitreihenmittelwerts auf den Indikator)
  reg_imp <- lm(Outcome ~
                  I(Jahr*Jahr*MEAN) + I(Jahr*MEAN),
                data = regdata,
                na.action = "na.exclude")
  
  # Predicted Value einsetzen
  regdata %>% 
    select(Outcome) %>% 
    mutate(
      # Predicted Value des Modells vermerken
      Imputed = predict(reg_imp, newdata = regdata),
      # Missings mit predicted Value ersetzen
      Outcome = ifelse(is.finite(Outcome),
                       Outcome,
                       Imputed),
      # Implausible (negative) Werte zurück auf 0 setzen
      Outcome = ifelse(Outcome < 0, 0, Outcome)) %>% 
    pull(Outcome) # Errechneten Wert ausgeben
}

# Über sämtliche Indikatoren imputieren
Workfile_imputed <- Workfile %>%
  mutate(BeschaeftigtemitakadAbschluss_adj=reg_impute(.,"BeschaeftigtemitakadAbschluss_adj"),
         BeschaeftigteohneAbschluss_adj   =reg_impute(.,"BeschaeftigteohneAbschluss_adj"),
         SchulabgaengerohneAbschluss_adj  =reg_impute(.,"SchulabgaengerohneAbschluss_adj"),
         Arbeitslosigkeit                 =reg_impute(.,"Arbeitslosigkeit"),
         Beschaeftigtenquote              =reg_impute(.,"Beschaeftigtenquote"),
         Bruttoverdienst_ln               =reg_impute(.,"Bruttoverdienst_ln"),
         Einkommensteuer_ln               =reg_impute(.,"Einkommensteuer_ln"),
         Haushaltseinkommen_ln            =reg_impute(.,"Haushaltseinkommen_ln"),
         Schuldnerquote                   =reg_impute(.,"Schuldnerquote")) %>% 
  select(Gemeindekennziffer,
         Jahr,
         Bevoelkerung,
         GVBKennziffer,
         Kreiskennziffer,
         Bundesland,
         all_of(indikatoren))

# # Ergebnis der Imputation
# cat("Übersicht über Indikatoren nach Imputation: \n\n")
# summary(Workfile_imputed %>% select(all_of(indikatoren)))
# 
# # Vergleich der Anzahl NAs vor und nach Imputation
# missings <- Workfile %>% 
#   # Nur relevante Variablen mitnehmen
#   select(Gemeindekennziffer,
#          Jahr,
#          all_of(indikatoren)) %>%
#   # Reshape auf long (Indikatoren zeilenweise)
#   pivot_longer(cols = 3:11,
#                names_to = "Indikator",
#                values_to = "value") %>%
#   # Missings aufsummieren
#  summarise(.by = Indikator,
#            PreImputation = sum(is.na(value))) %>%
#   # Selbiges für imputierten Datensatz und dann joinen
#   left_join(Workfile_imputed %>% pivot_longer(cols = 7:15,
#                                               names_to = "Indikator",
#                                               values_to = "value") %>%
#               # Missings aufsummieren
#               summarise(.by = Indikator,
#                         PostImputation = sum(is.na(value))),
#             by = "Indikator")
# 
# missings
# 
# rm(missings)

rm(reg_impute)

# V. Korrektur ausreißender Ausgangsdaten ======================================

## Daten aufbereiten
# Daten zu long-Format konvertieren und z berechnen
ausgangsdaten <- Workfile_imputed %>% 
  # Indikatoren long untereinander
  pivot_longer(cols = all_of(indikatoren),
               names_to = "Indikator",
               values_to = "roh") %>% 
  # Gemeinde-Jahr-Indikator-Schlüssel für Zuordnung
  mutate(gem_year_ind = paste0(Gemeindekennziffer,"_",Jahr,"_",Indikator)) %>% 
  # Z-Werte (pro Indikator und Gemeinde) berechnen
  mutate(.by = c(Indikator, Gemeindekennziffer),
         z_gem = as.numeric(scale(roh))) %>% 
  # Z-Werte zentrieren, indem der jeweilige Jahresdurchschnitt abgezogen wird
  mutate(.by = c(Indikator, Jahr),
         z_mean = mean(z_gem),
         z_gem = z_gem - z_mean) %>% 
  select(-z_mean) %>% 
  # Z-Werte als eigene Zeilen
  pivot_longer(cols = c(roh, z_gem),
               names_to = "Typ",
               values_to = "Wert")

## Selektion der Ausreißer
# Anhand optischer Begutachtung wurden folgende Beobachtungen ausgewählt und für die Imputation markiert:
ausr <- ausgangsdaten %>%
  # Rohwert und z-Wert wide nebeneinander
  pivot_wider(names_from = Typ,
              values_from = Wert) %>%
  mutate(
    mark = case_when(
      (Indikator == "Arbeitslosigkeit" & Jahr %in% c(2009, 2014, 2017, 2022) & z_gem >= 4) ~ 1,
      (Indikator == "Arbeitslosigkeit" & Jahr %in% c(2017, 2019) & roh >= 20) ~ 1,
      (Indikator == "Beschaeftigtenquote" & Jahr %in% c(2011:2015) & z_gem <= -3.25) ~ 1,
      (Indikator == "Beschaeftigtenquote" & Jahr %in% c(2019) & z_gem <= -5) ~ 1,
      (Indikator == "Bruttoverdienst_ln" & Jahr %in% c(2022) & z_gem >= 2) ~ 1,
      (Indikator == "Einkommensteuer_ln" & Jahr %in% c(2004, 2010) & z_gem <= -3) ~ 1,
      (Indikator == "Einkommensteuer_ln" & Jahr %in% c(2009:2010) & roh <= 0.25) ~ 1,
      (Indikator == "Einkommensteuer_ln" & Jahr %in% c(2015:2016) & !between(z_gem, -2.2, 2)) ~ 1,
      (Indikator == "Einkommensteuer_ln" & Jahr %in% c(2017) & Bundesland == "Bayern") ~ 1, # Einkommensteuer 2017 reißt nur Bayern aus
      (Indikator == "Einkommensteuer_ln" & Jahr %in% c(1998:2021) & roh <= 0.75) ~ 2, # Hier über ausreißerbereinigtes Jahresminimum imputieren (mark = 2)
      (Indikator == "Haushaltseinkommen_ln" & Jahr %in% c(2004) & z_gem >= 3) ~ 1,
      (Indikator == "Haushaltseinkommen_ln" & Jahr %in% c(2012, 2014) & z_gem >= 2) ~ 1,
      (Indikator == "Haushaltseinkommen_ln" & Jahr %in% c(2017) & z_gem >= 1.5) ~ 1,
      (Indikator == "Schuldnerquote" & Jahr %in% c(2010:2016) & !between(z_gem, -1.5, 2)) ~ 1,
      TRUE ~ 0)
  ) %>%
  # Nur markierte Fälle
  filter(mark != 0)

# Fälle für lineare Interpolation
ausr_linear <- ausr %>%
  filter(mark == 1) %>%
  pull(gem_year_ind)

# Fälle für Imputation über Jahresminimum (log. Einkommensteuer <= 2.5)
ausr_einkst <- ausr %>%
  filter(mark == 2) %>%
  pull(gem_year_ind)

## Imputation der Ausreißer
Workfile_corrected <- Workfile_imputed %>%
  # Indikatoren temporär long untereinander
  pivot_longer(cols = all_of(indikatoren),
               names_to = "Indikator",
               values_to = "roh") %>%
  mutate(
    # Zuordnung über Gemeinde-Jahr-Indikator-Schlüssel
    gem_year_ind = paste0(Gemeindekennziffer,"_",Jahr,"_",Indikator),
    # Ausreißer markieren
    impute = case_when(gem_year_ind %in% ausr_linear ~ 1, # Für lineare Interpolation
                       gem_year_ind %in% ausr_einkst ~ 2, # Für Imputation über Jahresminimum (log. Einkommensteuer <= 2.5)
                       TRUE ~ 0)
  ) %>%
  # Linear interpolieren
  mutate(.by = c(Gemeindekennziffer, Indikator),
         adjusted = ifelse(impute == 1, NA, roh),
         adjusted = zoo::na.approx(adjusted, x = Jahr, na.rm = FALSE)) %>%
  # Imputation über ausreißerbereinigtes Jahresminimum
  mutate(.by = c(Jahr, Indikator),
         adjusted = ifelse(impute == 2, NA, adjusted),
         adjusted = ifelse(is.na(adjusted),
                           min(adjusted, na.rm = TRUE),
                           adjusted)) %>% 
  # Indikatoren zurück auf wide nebeneinander
  pivot_wider(id_cols = c(Gemeindekennziffer, Jahr, Bevoelkerung,
                          GVBKennziffer, Kreiskennziffer, Bundesland),
              names_from = Indikator,
              values_from = adjusted) %>%
  arrange(Gemeindekennziffer, Jahr)

rm(ausgangsdaten,
   ausr,
   ausr_linear,
   ausr_einkst)

# VI. Faktorenanalyse und Generierung der Faktorscores =========================

# Datengrundlage
PCA_Input <- list(
  "Bildung" = Workfile_corrected %>%
    filter(Jahr >= year_max - 20) %>%
    select(BeschaeftigtemitakadAbschluss_adj,
           BeschaeftigteohneAbschluss_adj,
           SchulabgaengerohneAbschluss_adj),
  
  "Arbeitswelt" = Workfile_corrected %>%
    filter(Jahr >= year_max - 20) %>%
    select(Arbeitslosigkeit,
           Beschaeftigtenquote,
           Bruttoverdienst_ln),
  
  "Einkommen" = Workfile_corrected %>%
    filter(Jahr >= year_max - 20) %>%
    select(Einkommensteuer_ln,
           Haushaltseinkommen_ln,
           Schuldnerquote)
)

# Hauptkomponentenanalyse
PCA_Models <- list(
  "Bildung"     = prcomp(PCA_Input$Bildung,     scale. = TRUE, rank. = 1),
  "Arbeitswelt" = prcomp(PCA_Input$Arbeitswelt, scale. = TRUE, rank. = 1),
  "Einkommen"   = prcomp(PCA_Input$Einkommen,   scale. = TRUE, rank. = 1)
)

# # Schneller Check
# PCA_Models # In jeder Dimension zweite Komponente < 1

## Generierung der Faktorscores

# Prediction der rohen Scores
Results <- Workfile_corrected %>%
  mutate(TS_Bildung_raw =     c(predict(PCA_Models$Bildung, newdata = .)),
         TS_Arbeitswelt_raw = c(predict(PCA_Models$Arbeitswelt, newdata = .)),
         TS_Einkommen_raw =   c(predict(PCA_Models$Einkommen, newdata = .)))

# # Übersicht über Teildimensionen
# summary(Results %>% select(TS_Bildung_raw,
#                            TS_Arbeitswelt_raw,
#                            TS_Einkommen_raw))

# Polungen korrigieren (= höherer Wert -> mehr Deprivation)
for (dim in c("TS_Bildung_raw", "TS_Arbeitswelt_raw", "TS_Einkommen_raw")) {
  # Korreliert Teilscore negativ mit Arbeitslosigkeit?
  if (cor(Results$Arbeitslosigkeit, Results[[dim]]) < 0) {
    # Wenn Korrelation negativ, dann Vorzeichen des Teilscores tauschen
    Results[[dim]] <- -Results[[dim]]
  }
}

# # Korrelationsmatrix der Teildimensionen + Arbeitslosigkeit nach Umpolung
# cor(Results %>% select(Arbeitslosigkeit,
#                        TS_Bildung_raw,
#                        TS_Arbeitswelt_raw,
#                        TS_Einkommen_raw))

# Normieren
Results <- Results %>%
  mutate(
    .by = Jahr,
    # Teildimensionen jahresweise normieren
    TS_Bildung_nrm = rescale(TS_Bildung_raw),
    TS_Arbeitswelt_nrm = rescale(TS_Arbeitswelt_raw),
    TS_Einkommen_nrm = rescale(TS_Einkommen_raw),
    # Zu Gesamtscore aufaddieren und noch mal jahresweise normieren
    GISD_Score = rescale(TS_Bildung_nrm + TS_Arbeitswelt_nrm + TS_Einkommen_nrm)
  )

# # Übersicht über Gesamt- und Teilscores nach Normierung
# summary(Results %>% select(TS_Bildung_nrm,
#                            TS_Arbeitswelt_nrm,
#                            TS_Einkommen_nrm,
#                            GISD_Score))

rm(dim,
   PCA_Input,
   Workfile,
   Workfile_imputed,
   Workfile_corrected)

# VII. Datenexport =============================================================

## Vorbereitung PLZ-Ebene ====

ew_gem_plz_filename <- paste0(dir_input,"SHP/",
                              "EW_Gem_PLZ_Intersect_",gebietsstand,".rds")

if (file.exists(ew_gem_plz_filename)) { # Schon vorhanden?
  
  ew_gem_plz <- read_rds(ew_gem_plz_filename) # Dann laden
  rm(ew_gem_plz_filename)
  
} else { # Sonst generieren (Achtung, bisschen langsam):
  
  message("PLZ-Gemeinde-Populationsdatensatz fehlt. Versuche, zu generieren...")
  
  # PLZ-Shapefile mit Gemeinde-Shapefile intersecten und Gemeindepopulation
  # flächengewichtet den Intersects zuweisen. So können GISD-Scores
  # einwohnerproportional den PLZ-Gebieten zugeordnet werden.
  dir_plz <- paste0(dir_input,"SHP/PLZ/daten/plz/de/")
  shp_plz_filename <- paste0(dir_plz,"PLZ.shp")
  
  if (!file.exists(shp_plz_filename)) { # PLZ-Shapefile nicht vorhanden?
    stop("PLZ-Shapefile nicht gefunden!")
  } # Sonst führe weiter aus:
  
  ## PLZ-Intersect-Generierung
  library(sf)         # Geospatial Data Manipulation
  library(rmapshaper) # Shapefiles simplifizieren
  
  # Gemeinde-Shapefile inkl. Einwohnerzahlen (Gebietsstand: 31.12.2023, Projektion: GK3)
  # (Quelle: https://daten.gdz.bkg.bund.de/produkte/vg/vg250-ew_ebenen_1231/2023/vg250-ew_12-31.gk3.shape.ebenen.zip)
  shp_gem <- st_read(paste0(dir_input,"SHP/",
                            "vg250-ew_12-31.gk3.shape.ebenen/vg250-ew_ebenen_1231/",
                            "VG250_GEM.shp")) %>%
    mutate(bundesland = floor(as.numeric(as.character(AGS))/1000000)) %>% 
    select(gemeinde_id = AGS,
           gemeinde_name = GEN,
           population_gem = EWZ,
           geometry)
  
  # PLZ-Shapefile (Gebietsstand: November 2023, Projektion: UTM32)
  # (Quelle: https://gdz.bkg.bund.de/index.php/default/postleitzahlgebiete-deutschland-plz.html)
  # (Hinweis: Datensatz nur auf Anfrage verfügbar)
  shp_plz <- st_read(shp_plz_filename) %>% 
    st_transform(st_crs(shp_gem)) %>% # Projektion an Gemeinde-SHP angleichen
    mutate(PLZ_4 = str_sub(PLZ_5, 1, 4),
           PLZ_3 = str_sub(PLZ_5, 1, 3),
           PLZ_2 = str_sub(PLZ_5, 1, 2)) %>% 
    select(rev(starts_with("PLZ")),
           geometry)
  
  # Geometrie simplifizieren (Rechenleistung einsparen)
  shp_plz_small <- ms_simplify(shp_plz, keep = .1 , keep_shapes = TRUE)
  shp_plz <- shp_plz_small
  rm(shp_plz_small)
  
  # PLZ mit Gemeinden intersecten und Gemeindepopulation flächengewichtet auf Intersections aufteilen
  ew_gem_plz <- st_intersection(shp_gem, shp_plz) %>% 
    mutate(flaeche_intersect = as.numeric(st_area(.))) %>% 
    mutate(.by = gemeinde_id,
           flaeche_proportion = flaeche_intersect/sum(flaeche_intersect),
           population_intersect = round(flaeche_proportion*population_gem)) %>%
    select(contains("PLZ"),
           gemeinde_id,
           gemeinde_name,
           population_gem,
           population_intersect,
           flaeche_intersect,
           flaeche_proportion) %>% 
    arrange(PLZ_5, gemeinde_id) %>% 
    # Geometrie entfernen
    st_set_geometry(NULL)
  
  # Exportieren
  saveRDS(ew_gem_plz,
          file = ew_gem_plz_filename)
  
  rm(shp_plz_filename,
     shp_plz,
     shp_gem,
     ew_gem_plz_filename)
  
}

## Hilfsfunktionen ====

# Bevölkerungsgewichtetes und regional-jährlich gruppiertes Aggregieren der
# rohen Teilscores, optional noch andere Variablen mitnehmen
aggregate_subscores <- function(df, by, keep = character(0)) {
  df %>%
    summarise(.by = all_of(by),
              TS_Bildung_raw     = weighted.mean(TS_Bildung_raw,     population),
              TS_Arbeitswelt_raw = weighted.mean(TS_Arbeitswelt_raw, population),
              TS_Einkommen_raw   = weighted.mean(TS_Einkommen_raw,   population),
              population         = sum(population),
              across(all_of(keep), first))
}

# Jahresweise (und optional bundeslandweises) Normieren der Teilscores
# und Berechnen des Gesamtscores und der Quantile
generate_gisd <- function(df, by) {
  df %>%
    mutate(.by = all_of(by),
           TS_Bildung_nrm     = rescale(TS_Bildung_raw),
           TS_Arbeitswelt_nrm = rescale(TS_Arbeitswelt_raw),
           TS_Einkommen_nrm   = rescale(TS_Einkommen_raw),
           gisd_score = rescale(TS_Bildung_nrm + TS_Arbeitswelt_nrm + TS_Einkommen_nrm),
           gisd_5     = ntile(gisd_score, 5),
           gisd_10    = ntile(gisd_score, 10),
           gisd_k     = findInterval(gisd_5, c(1, 2, 5)),
           gisd_score = round(gisd_score, digits = 5))
}

# Variablen an Zielschema matchen und sortieren
# Optional mit Bundesland ganz vorne dabei
select_export_vars <- function(df, ebene, region_id, region_name,
                               bundesland = FALSE) {
  if (bundesland == TRUE) { bl <- "federal_state" } else { bl <- NULL }
  
  df %>%
    mutate(region_type = ebene,
           region_id   = !!sym(region_id),
           region_name = !!sym(region_name)) %>% 
    select(any_of(bl),
           starts_with("region"),
           starts_with("year"),
           starts_with("gisd"))
}

# Umlaute ersetzen damit GitHub keine Probleme macht
adjust_umlaute <- function(string) {
  string %>% 
    gsub("ä", "ae", .) %>%
    gsub("ö", "oe", .) %>%
    gsub("ü", "ue", .) %>%
    gsub("ß", "ss", .)
}

## Aggregationsschleife ====

# Ergebnisse für Export formatieren
Results_export <- Results %>%
  select(Gemeindekennziffer, Jahr,
         TS_Bildung_raw,
         TS_Arbeitswelt_raw,
         TS_Einkommen_raw,
         GISD_Score) %>% 
  left_join(id_dataset, by = "Gemeindekennziffer") %>%
  rename(gemeinde_id   = Gemeindekennziffer,
         gemeinde_name = Gemeindename,
         year          = Jahr, 
         gisd_score    = GISD_Score,
         population    = Bevoelkerung,
         gvb_id        = GVBKennziffer,
         gvb_name      = GVBName,
         kreis_id      = Kreiskennziffer,
         kreis_name    = Kreisname,
         ror_id        = ROR_Kennziffer,
         ror_name      = ROR_Name,
         nuts_2_id     = NUTS2_Kennziffer,
         nuts_2_name   = NUTS2_Name,
         bl_id         = Bundesland_Kennziffer,
         federal_state = Bundesland) %>% 
  mutate(federal_state = adjust_umlaute(federal_state))

# Ebenen aufschlüsseln
schema_labels <- tibble(
  region_type = c("Gemeinde",      "Gemeindeverband", "Kreis",      "Raumordnungsregion", "NUTS2"),
  region_id =   c("gemeinde_id",   "gvb_id",          "kreis_id",   "ror_id",             "nuts_2_id"),
  region_name = c("gemeinde_name", "gvb_name",        "kreis_name", "ror_name",           "nuts_2_name")
)

# Leere Listen initialisieren
exports_bund <- list()
exports_bl <- list()

# Über Ebenen gehen, Scores aggregieren, Datensatz-Listen füllen
for (i in seq(nrow(schema_labels))) {
  
  ebene <-    schema_labels$region_type[i]
  id_var <-   schema_labels$region_id[i]
  name_var <- schema_labels$region_name[i]
  
  # Für alle Ebenen jährlich über Bund normieren
  exports_bund[[ebene]] <- Results_export %>% 
    aggregate_subscores(by = c(id_var, "year"),
                        keep = name_var) %>% 
    generate_gisd(by = c("year")) %>% 
    select_export_vars(ebene, id_var, name_var)
  
  # Für Gemeinden und Kreise: Zusätzlich pro Bundesland normieren
  if (ebene %in% c("Gemeinde", "Kreis")) {
    exports_bl[[ebene]] <- Results_export %>% 
      # Ohne Stadtstaaten
      filter(!federal_state %in% c("Berlin", "Bremen", "Hamburg")) %>%
      aggregate_subscores(by = c(id_var, "year"),
                          keep = c(name_var, "federal_state")) %>% 
      generate_gisd(by = c("year", "federal_state")) %>% 
      select_export_vars(ebene, id_var, name_var, bundesland = TRUE)
  }
  
}

# PLZ-Ebene an Bundes-Version anhängen
for (plz_ebene in c("PLZ_5", "PLZ_4", "PLZ_3", "PLZ_2")) {
  
  exports_bund[[plz_ebene]] <- Results_export %>%
    select(gemeinde_id, year, gisd_score) %>%
    # Scores an Gemeinde-PLZ-Intersect anfügen
    left_join(ew_gem_plz, ., by = "gemeinde_id") %>%
    filter(population_intersect > 0, !is.na(year)) %>%
    # Scores populationsgewichtet den Intersects zuweisen, dann zu PLZ auflösen
    mutate(.by = c(year, gemeinde_id),
           gisd_score = weighted.mean(gisd_score, population_intersect)) %>%
    summarise(.by = c("year", plz_ebene),
              gisd_score = weighted.mean(gisd_score, population_intersect),
              population = sum(population_intersect)) %>%
    mutate(.by = "year",
           gisd_5     = ntile(gisd_score, 5),
           gisd_10    = ntile(gisd_score, 10),
           gisd_k     = findInterval(gisd_5, c(1, 2, 5)),
           gisd_score = round(gisd_score, digits = 5)) %>%
    select_export_vars(ebene = plz_ebene,
                       region_id = plz_ebene,
                       region_name = plz_ebene)
}

rm(i, ebene, id_var, name_var, plz_ebene,
   ew_gem_plz, schema_labels)

## Export ====

write_tsv(bind_rows(exports_bund), paste0(dir_output_tsv,"GISD_Bund.tsv"))
write_tsv(bind_rows(exports_bl),   paste0(dir_output_tsv,"GISD_Bundesland.tsv"))

write_xlsx(exports_bund, paste0(dir_output_xlsx,"00_GISD_Bund.xlsx"))

# Excel-Output in Bundesländer aufgetrennt (ohne Stadtstaaten)
bl_codes <- Results_export %>% 
  distinct(federal_state, bl_id) %>% 
  filter(!federal_state %in% c("Berlin", "Bremen", "Hamburg")) %>% 
  deframe()

for (bl_name in names(bl_codes)) {
  bl_id <- bl_codes[[bl_name]]
  
  export_bl_i <- list(
    Gemeinden = exports_bl[["Gemeinde"]] %>% filter(federal_state == bl_name) %>% select(-region_type, -federal_state),
    Kreise =    exports_bl[["Kreis"]]    %>% filter(federal_state == bl_name) %>% select(-region_type, -federal_state)
  )
  
  write_xlsx(export_bl_i, paste0(dir_output_xlsx, bl_id,"_GISD_", bl_name,".xlsx"))
}

rm(bl_codes, bl_name, bl_id, export_bl_i)

### ENDE ###

