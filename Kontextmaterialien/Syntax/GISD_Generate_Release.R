# GISD - German Index of Socio-Economic Deprivation
# Author: Lola Omar Soliman
# Citation: https://github.com/robert-koch-institut/German_Index_of_Socioeconomic_Deprivation_GISD

# Revision: GISD Release 2022 v1.1
# Date: 2023-12-12

# Libraries
library(tidyverse)
library(haven)
library(sf)

# Scientific Notation in der Zahlendarstellung abschalten
options(scipen = 50)

## Set working directory as script directory
# setwd("/path/to/script")
getwd()

# Path Definitions
infiles_dir  <- "../Rohdaten/"
outfiles_dir <- "../../GISD_Release_aktuell/"
dir.create(outfiles_dir)
dir.create(paste0(outfiles_dir, "Bund"))
dir.create(paste0(outfiles_dir, "Bundesland"))


## I.  Generierung eines ID-Datensatzes
load_dataset = function(sheet) {
  read_excel(paste0(infiles_dir, "Referenz/Referenz_1998_2019.xls"),
             sheet=sheet, na="NA")
}

Gemeinden_INKAR <- load_dataset("Gemeinden-GVB") %>% 
  na.omit() %>%
  mutate(Kennziffer=as.numeric(gem19),
         GVBKennziffer=gvb19,
         fl19=as.numeric(fl19)) %>% 
  select(-gem19, -gvb19)

Gemeindeverbaende_INKAR <- load_dataset("Gemeindeverbände") %>% 
  na.omit() %>% 
  select(GVBKennziffer=gvb19,
         "Name des Gemeindeverbands"=gvb19name)

Kreise_INKAR <- load_dataset("KRS") %>%
  mutate(krs19=as.numeric(krs19),
         fl19=as.numeric(fl19))

id_dataset <- Gemeinden_INKAR %>% 
  select(Gemeindekennziffer=Kennziffer,
         "Name der Gemeinde"=gem19name,
         GVBKennziffer,
         Bevoelkerung="Bevölkerung") %>% 
  mutate(Kreiskennziffer=floor(Gemeindekennziffer/1000)) %>%
  left_join(Kreise_INKAR %>% select("Kreiskennziffer"=krs19,
                                    "Name des Kreises"=krs19name,
                                    "Raumordnungsregion Nr"=ROR11,
                                    "Raumordnungsregion"=ROR11name,
                                    NUTS2,
                                    "NUTS2 Name"=NUTS2name,
                                    Bundesland),
            by="Kreiskennziffer") %>%
  left_join(Gemeindeverbaende_INKAR, by="GVBKennziffer")


## II. Erzeugen eines Datensatzes mit Kennziffern als ID unabhängig von der Ebene
Basedata <- Kreise_INKAR %>%
  select(Kennziffer=krs19) %>%
  mutate(Jahr=2019)

inputdataset <- list.files(paste0(infiles_dir, "INKAR_1998_2019/"),
                           pattern="\\.xlsx$")

inputdataset

for(file in inputdataset) {
  myimport <- read_excel(paste0(infiles_dir, "INKAR_1998_2019/", file),
                         skip=1, sheet="Daten")
  
  names(myimport)[1] <- "Kennziffer"
  myimport[2:3] <- NULL
  myimport <- myimport %>%
    gather(key=Jahr,
           value=Value,
           -Kennziffer,
           convert=TRUE,
           na.rm=TRUE) %>%
    mutate(Kennziffer=as.numeric(as.character(Kennziffer)),
           Value=as.numeric(Value))
  
  #Setze Dateinamen des jeweiligen Indikators als Variablenname ein
  names(myimport)[names(myimport)=="Value"] <- gsub("^.+_(.+)[.].+$", #Alles zw. Leerstrich und Punkt
                                                    "\\1", #Detected String einsetzen
                                                    file) #Aus current File-String
  Basedata <- Basedata %>%
    full_join(., myimport, by=c("Kennziffer", "Jahr"))
}

rm(inputdataset, myimport)

# Tabelle der Indikatoren mit regionaler Tiefe
level_table <- data.frame(Indikator=names(Basedata)[-(1:2)],
                          Tiefe_Indikator=c("Gemeindeverband", "Gemeindeverband",
                                            "Kreis", "Gemeindeverband",
                                            "Kreis", "Kreis", "Kreis", "Kreis", "Kreis",
                                            "Gemeindeverband", "Kreis", "Kreis"))

Indikatoren_Gemeindeverband <- level_table %>%
  filter(Tiefe_Indikator == "Gemeindeverband") %>%
  pull(Indikator)

Indikatoren_Kreis <- level_table %>%
  filter(Tiefe_Indikator == "Kreis") %>%
  pull(Indikator)

Basedata_Gemeindeverbandsebene <- Basedata %>%
  select(Gemeindeverband=Kennziffer,
         Jahr, all_of(Indikatoren_Gemeindeverband)) %>%   
  gather(key, value, 3:5) %>%
  filter(!is.na(value)) %>%
  spread(key, value) %>%
  filter(Jahr>=1998)

Basedata_Kreisebene <- Basedata %>%
  select(Kreis=Kennziffer,
         Jahr,
         all_of(Indikatoren_Kreis)) %>% 
  filter(Jahr>=1998)

Workfile <- expand.grid(Kennziffer=Gemeinden_INKAR$Kennziffer,
                        Jahr=sort(unique(Basedata$Jahr))) %>%
  mutate(Kreiskennziffer=floor(as.numeric(Kennziffer)/1000)) %>%
  as_tibble() %>%
  left_join(Gemeinden_INKAR, by="Kennziffer") %>%
  select(Gemeindekennziffer=Kennziffer,
         Kreis=Kreiskennziffer,
         Gemeindeverband=GVBKennziffer,
         Jahr,
         Bevoelkerung=Bevölkerung) %>%
  mutate(Gemeindeverband=as.numeric(Gemeindeverband),
         Bevoelkerung=as.numeric(Bevoelkerung)) %>% 
  arrange(Gemeindeverband, Jahr) %>% # Join Metadata
  left_join(Basedata_Kreisebene, by=c("Kreis", "Jahr")) %>%
  left_join(Basedata_Gemeindeverbandsebene, by=c("Gemeindeverband", "Jahr")) %>%
  filter(Jahr >= 1998)

inputdataset_kreis <- list.files(paste0(infiles_dir, "INKAR_1998_2019/Indikatoren_Kreisebene/"))

for(file in inputdataset_kreis){
  myimport <- read_excel(paste0(infiles_dir, "INKAR_1998_2019/Indikatoren_Kreisebene/", file),
                         skip = 1, sheet = "Daten")
  
  names(myimport)[1] <- "Kreis"
  myimport[2:3] <- NULL
  myimport <- myimport %>%
    gather(key=Jahr,
           value=Value,
           -Kreis,
           convert=TRUE,
           na.rm=TRUE) %>%
    mutate(Kreis=as.numeric(as.character(Kreis)),
           Value=as.numeric(Value))
  
  #Setze Dateinamen des jeweiligen Indikators als Variablenname ein
  names(myimport)[names(myimport) == "Value"] <- gsub("^.+_(.+)[.].+$", #Alles zw. Leerstrich und Punkt
                                                      "\\1", #Detected String einsetzen
                                                      file) #Aus current File-String
  
  Workfile <- Workfile %>%
    full_join(myimport, by=c("Kreis", "Jahr"))
}

rm(inputdataset_kreis, myimport)

Workfile <- Workfile %>%
  mutate(ErwerbsfaehigeBevoelkerung=ifelse(Jahr < 2001, ErwerbsfaehigeBevoelkerungKreis, ErwerbsfaehigeBevoelkerung),
         Arbeitslosigkeit          =ifelse(Jahr < 2001, ArbeitslosigkeitKreis,           Arbeitslosigkeit),
         Beschaeftigtenquote       =ifelse(Jahr < 2001, BeschaeftigtenquoteKreis,        Beschaeftigtenquote)) %>%
  select(-ErwerbsfaehigeBevoelkerungKreis,
         -ArbeitslosigkeitKreis,
         -BeschaeftigtenquoteKreis)

Workfile <- Workfile %>%
  filter(Bevoelkerung > 0) %>%
  mutate(BeschaeftigteohneAbschluss    =round(BeschaeftigteohneAbschluss    / SVBeschaeftigte * 100, digits=2),
         BeschaeftigtemitakadAbschluss =round(BeschaeftigtemitakadAbschluss / SVBeschaeftigte * 100, digits=2),
         Arbeitslosigkeit=round(Arbeitslosigkeit / ErwerbsfaehigeBevoelkerung * 1000, digits=2),
         Arbeitslosigkeit=ifelse(is.finite(Arbeitslosigkeit),
                                 Arbeitslosigkeit, NA)) %>%
  select(-SVBeschaeftigte, -ErwerbsfaehigeBevoelkerung)

Basedata <- Basedata %>%
  select(-SVBeschaeftigte, -ErwerbsfaehigeBevoelkerung)

Basedata_Kreisebene <- Basedata_Kreisebene %>%
  select(-SVBeschaeftigte)

Basedata_Gemeindeverbandsebene <- Basedata_Gemeindeverbandsebene %>%
  select(-ErwerbsfaehigeBevoelkerung)

## III.Imputation fehlender Werte
Verbraucherpreisindex <- data.frame(Jahr   =seq(1998, 2020),
                                    VBindex=c(78.3, 78.8, 79.9, 81.5, 82.6,
                                              83.5, 84.9, 86.2, 87.6, 89.6,
                                              91.9, 92.2, 93.2, 95.2, 97.1,
                                              98.5, 99.5,  100, 100.5, 102,
                                             103.8, 105.3, 105.8))

Workfile <- Workfile %>%
  left_join(Verbraucherpreisindex, by="Jahr") %>%
  mutate(Einkommensteuer   =Einkommensteuer    / VBindex * 100,
         Haushaltseinkommen=Haushaltseinkommen / VBindex * 100,
         Bruttoverdienst   =Bruttoverdienst    / VBindex * 100,
         Einkommensteuer_ln   =ifelse(Einkommensteuer == 0, 0.75, log(Einkommensteuer)),
         Haushaltseinkommen_ln=log(Haushaltseinkommen),
         Bruttoverdienst_ln   =log(Bruttoverdienst),
         G8_jahr=case_when(Kreis < 2000  &                 Jahr == 2016 ~ 1,
                           Kreis > 1999  & Kreis < 3000  & Jahr == 2010 ~ 1,
                           Kreis > 2999  & Kreis < 4000  & Jahr == 2011 ~ 1,
                           Kreis > 3999  & Kreis < 5000  & Jahr == 2012 ~ 1,
                           Kreis > 4999  & Kreis < 6000  & Jahr == 2013 ~ 1,
                           Kreis > 5999  & Kreis < 7000  & Jahr == 2013 ~ 1,
                           Kreis > 7999  & Kreis < 9000  & Jahr == 2012 ~ 1,
                           Kreis > 8999  & Kreis < 10000 & Jahr == 2011 ~ 1,
                           Kreis > 9999  & Kreis < 11000 & Jahr == 2009 ~ 1,
                           Kreis > 10999 & Kreis < 12000 & Jahr == 2012 ~ 1,
                           Kreis > 11999 & Kreis < 13000 & Jahr == 2012 ~ 1,
                           Kreis > 12999 & Kreis < 14000 & Jahr == 2008 ~ 1,
                           Kreis > 14999 & Kreis < 16000 & Jahr == 2007 ~ 1),
         SN_KA       =ifelse(Kreis > 14999 & Kreis < 16000 & Jahr == 2001, 1, 0),
         THvor2004   =ifelse(Kreis > 15999                 & Jahr <  2004, 1, 0),
         G8_jahr  =if_else(is.na(G8_jahr),   0, G8_jahr),
         SN_KA    =if_else(is.na(SN_KA),     0, SN_KA),
         THvor2004=if_else(is.na(THvor2004), 0, THvor2004))

adj_G8_jahr <- function(data, outcome_name) {
  mydata <- data %>%
    group_by(Gemeindekennziffer) %>% 
    select(Gemeindekennziffer, Jahr, G8_jahr, SN_KA, THvor2004,
           Outcome=all_of(outcome_name)) %>%
    mutate(MEAN=mean(Outcome, na.rm=TRUE)) %>%
    ungroup()
  
  mymodell2 <- lm(Outcome ~
                    I(Jahr*Jahr*MEAN) + I(Jahr*MEAN) + G8_jahr + SN_KA + THvor2004,
                  data=mydata, na.action="na.exclude")
  
  print(mymodell2)
  
  mydata %>%
      mutate(coef_G8=coef(mymodell2)["G8_jahr"],
             coef_SH=coef(mymodell2)["SN_KA"],
             coef_TH=coef(mymodell2)["THvor2004"],
             Outcome=ifelse(G8_jahr   == 1, Outcome - coef_G8, Outcome),
             Outcome=ifelse(SN_KA     == 1, Outcome - coef_SH, Outcome),
             Outcome=ifelse(THvor2004 == 1, Outcome - coef_TH, Outcome)) %>%
      pull(Outcome)
}

OW <- function(data,outcome_name){
  mydata <- data %>%
    select(Gemeindekennziffer, Jahr, OW,
           Outcome=all_of(outcome_name)) %>% 
    mutate(Jahr_Dummy=relevel(as.factor(Jahr), ref="2012")) %>%
    ungroup()
  
  mymodell_ow <- lm(Outcome ~ Jahr_Dummy + Jahr_Dummy*OW,
                    data=mydata, na.action="na.exclude")
  
  print(mymodell_ow)
  coef(mymodell_ow)["OW"]
  mydata %>%
    mutate(coef_ow=coef(mymodell_ow)["OW"]) %>%
    mutate(Outcome=ifelse(OW == 1, Outcome - coef_ow, Outcome)) %>%
    pull(Outcome)
}

Workfile <- Workfile %>%
  mutate(SchulabgaengermitHochschulreife_adj=adj_G8_jahr(., "SchulabgaengermitHochschulreife"),
         SchulabgaengerohneAbschluss_adj    =adj_G8_jahr(., "SchulabgaengerohneAbschluss"),
         OW=ifelse(Kreis < 11000, 0, 1)) %>%
  mutate(BeschaeftigteohneAbschluss_adj=OW(., "BeschaeftigteohneAbschluss"))

Workfile_spread <- Workfile %>%
  filter(Jahr == 2013) %>%
  spread(key=Jahr,
         value=BeschaeftigteohneAbschluss_adj) %>%
  select("2013", Gemeindekennziffer) %>%
  mutate(Jahr=2012) %>%
  rename(BeschaeftigteohneAbschluss_adj="2013")

Workfile <- Workfile %>%
  left_join(Workfile_spread, by=c("Gemeindekennziffer", "Jahr")) %>%
  mutate(BeschaeftigteohneAbschluss_adj=ifelse(Jahr == 2012,
                                               BeschaeftigteohneAbschluss_adj.y,
                                               BeschaeftigteohneAbschluss_adj.x)) %>%
  select(-BeschaeftigteohneAbschluss_adj.y,
         -BeschaeftigteohneAbschluss_adj.x)

Workfile_spread <- Workfile %>%
  filter(Jahr == 2013) %>%
  spread(key=Jahr,
         value=BeschaeftigtemitakadAbschluss) %>%
  select("2013", Gemeindekennziffer) %>%
  mutate(Jahr=2012) %>%
  rename(BeschaeftigtemitakadAbschluss="2013")

Workfile <- Workfile %>%
  left_join(Workfile_spread, by=c("Gemeindekennziffer", "Jahr")) %>%
  mutate(BeschaeftigtemitakadAbschluss=ifelse(Jahr == 2012,
                                              BeschaeftigtemitakadAbschluss.y,
                                              BeschaeftigtemitakadAbschluss.x),
         Messaenderung_Besch=ifelse(Jahr > 2012, 1, 0)) %>%
  select(-BeschaeftigtemitakadAbschluss.y,
         -BeschaeftigtemitakadAbschluss.x)

Messaenderung <- function(data,outcome_name) {
  mydata <- data %>%
    select(Gemeindekennziffer, Jahr, Messaenderung_Besch,
           Outcome=all_of(outcome_name)) %>% 
    mutate(MEAN=mean(Outcome, na.rm=TRUE)) %>%
    ungroup()
  
  mymodell_Messaenderung <- lm(Outcome ~
                                 I(Jahr*Jahr*MEAN) + I(Jahr*MEAN) + Messaenderung_Besch,
                               data=mydata, na.action="na.exclude")
  
  print(mymodell_Messaenderung) 
  coef(mymodell_Messaenderung)["Messaenderung_Besch"]
  mydata <- mydata %>% 
    mutate(coef_mb=coef(mymodell_Messaenderung)["Messaenderung_Besch"]) %>%
    mutate(Outcome=ifelse(Jahr < 2012, Outcome + coef_mb, Outcome)) %>%
    pull(Outcome)
}

Workfile <- Workfile %>% 
  mutate(BeschaeftigteohneAbschluss_adj   =Messaenderung(., "BeschaeftigteohneAbschluss_adj"),
         BeschaeftigtemitakadAbschluss_adj=Messaenderung(., "BeschaeftigtemitakadAbschluss"))

# Implausible Werte rausfiltern bevor imputiert wird
Impdata <-  Workfile %>%
  filter(Jahr >= 1998) %>% 
  gather(key,value,6:15) %>%
  mutate(value=ifelse(value<0.00001,NA,value)) %>%
  spread(key,value)

my_ts_imputer1 <- function(data,outcome_name) {
  mydata <- data %>%
    group_by(Gemeindekennziffer) %>%
    select(Gemeindekennziffer, Jahr,
           Outcome=all_of(outcome_name)) %>% 
    mutate(MEAN=mean(Outcome, na.rm=TRUE)) %>%
    ungroup()
  
  mymodell <- lm(Outcome ~
                   I(Jahr*Jahr*MEAN) + I(Jahr*MEAN),
                 data=mydata, na.action="na.exclude")
  
  mydata %>%
    mutate(Imputed=predict(mymodell, newdata=.)) %>%
    mutate(Outcome=ifelse(is.finite(Outcome), Outcome, Imputed),
           Outcome=ifelse(Outcome < 0, 0, Outcome)) %>%
    pull(Outcome)
}

Impdata.imputed <- Impdata %>%
  mutate(Arbeitslosigkeit                 =my_ts_imputer1(., "Arbeitslosigkeit"),
         SchulabgaengerohneAbschluss_adj  =my_ts_imputer1(., "SchulabgaengerohneAbschluss_adj"),
         SchulabgaengerohneAbschluss      =my_ts_imputer1(., "SchulabgaengerohneAbschluss"),
         Beschaeftigtenquote              =my_ts_imputer1(., "Beschaeftigtenquote"),
         Bruttoverdienst_ln               =my_ts_imputer1(., "Bruttoverdienst_ln"),
         Bruttoverdienst                  =my_ts_imputer1(., "Bruttoverdienst"),
         BeschaeftigtemitakadAbschluss_adj=my_ts_imputer1(., "BeschaeftigtemitakadAbschluss_adj"),
         BeschaeftigtemitakadAbschluss    =my_ts_imputer1(., "BeschaeftigtemitakadAbschluss"),
         BeschaeftigteohneAbschluss_adj   =my_ts_imputer1(., "BeschaeftigteohneAbschluss_adj"),
         BeschaeftigteohneAbschluss       =my_ts_imputer1(., "BeschaeftigteohneAbschluss"),
         Einkommensteuer_ln               =my_ts_imputer1(., "Einkommensteuer_ln"),
         Einkommensteuer                  =my_ts_imputer1(., "Einkommensteuer"),
         Haushaltseinkommen_ln            =my_ts_imputer1(., "Haushaltseinkommen_ln"),
         Haushaltseinkommen               =my_ts_imputer1(., "Haushaltseinkommen"),
         Schuldnerquote                   =my_ts_imputer1(., "Schuldnerquote"))

listofdeterminants <- c("Arbeitslosigkeit",
                        "SchulabgaengerohneAbschluss_adj",
                        "Beschaeftigtenquote",
                        "Bruttoverdienst_ln",
                        "BeschaeftigtemitakadAbschluss_adj",
                        "BeschaeftigteohneAbschluss_adj",
                        "Einkommensteuer_ln",
                        "Haushaltseinkommen_ln",
                        "Schuldnerquote",
                        "SchulabgaengermitHochschulreife_adj")

Impdata.imputed %>%
  as.data.frame() %>%
  select(all_of(listofdeterminants)) %>%
  summary()

my_ts_imputer2 <- function(data,outcome_name){
  mydata <- data %>%
    select(Gemeindekennziffer, Jahr, Arbeitslosigkeit,
           SchulabgaengerohneAbschluss_adj,
           Beschaeftigtenquote, Bruttoverdienst_ln,
           BeschaeftigtemitakadAbschluss_adj,
           BeschaeftigteohneAbschluss_adj,
           Einkommensteuer_ln,Haushaltseinkommen_ln,
           Outcome=all_of(outcome_name)) %>%
    mutate(MEAN=mean(Outcome, na.rm=TRUE))
  
  mymodell <- lm(Outcome ~
                   I(Jahr*Jahr*MEAN) + I(Jahr*MEAN) + Arbeitslosigkeit + 
                   SchulabgaengerohneAbschluss_adj + Beschaeftigtenquote + Bruttoverdienst_ln + BeschaeftigtemitakadAbschluss_adj + BeschaeftigteohneAbschluss_adj + Einkommensteuer_ln + Haushaltseinkommen_ln ,
                 data=mydata, na.action="na.exclude")
  
  mydata %>%
    mutate(Imputed=predict(mymodell, newdata=.)) %>%
    mutate(Outcome=ifelse(is.finite(Outcome), Outcome, Imputed)) %>% 
    mutate(Outcome=ifelse(Outcome < 0, 0, Outcome)) %>%
    pull(Outcome)
}

Impdata.imputed <- Impdata.imputed %>%
  mutate(SchulabgaengermitHochschulreife_adj=my_ts_imputer2(.,"SchulabgaengermitHochschulreife_adj"),
         SchulabgaengermitHochschulreife    =my_ts_imputer2(.,"SchulabgaengermitHochschulreife"))

summary(Impdata.imputed$SchulabgaengermitHochschulreife_adj)

# Amt Creuzburg wurde Ende 2019 aus drei aufgelösten Gemeinden neu gebildet.
# (Quelle: https://statistik.thueringen.de/DatenBank/gebiet3.asp?nr=63104)
# In der Referenz ist die neue Gemeinde enthalten, aber in den Rohdaten weder
# die neue, noch die alten. Die Gemeinde wird deshalb aus der Referenz entfernt.
Impdata.imputed <- Impdata.imputed %>%
  filter(Gemeindekennziffer!=16063104)


## IV. Faktorenanalyse (Hauptkomponentenanalyse) inklusive Generierung der Faktorscores
TS_Arbeitswelt_adj <- Impdata.imputed %>%
  filter(Jahr > 1999) %>%
  select(Beschaeftigtenquote, Arbeitslosigkeit, Bruttoverdienst_ln)

TS_Einkommen_adj <- Impdata.imputed %>%
  filter(Jahr > 1999) %>%
  select(Einkommensteuer_ln, Haushaltseinkommen_ln, Schuldnerquote) 

TS_Bildung_adj <- Impdata.imputed %>%
  filter(Jahr > 1999) %>%
  select(BeschaeftigtemitakadAbschluss_adj,
         BeschaeftigteohneAbschluss_adj,
         SchulabgaengerohneAbschluss_adj)

TS_Arbeitswelt_adj.pca <- prcomp(TS_Arbeitswelt_adj, center=TRUE, scale.=TRUE,
                                 retx=TRUE)

TS_Arbeitswelt_adj.pca <- prcomp(TS_Arbeitswelt_adj, center=TRUE, scale.=TRUE,
                                 retx=TRUE, rank.=1)
TS_Arbeitswelt_adj.pca

TS_Einkommen_adj.pca <- prcomp(TS_Einkommen_adj, center=TRUE, scale.=TRUE,
                               retx=TRUE)

TS_Einkommen_adj.pca <- prcomp(TS_Einkommen_adj, center=TRUE, scale.=TRUE,
                               retx=TRUE, rank.=1) 
TS_Einkommen_adj.pca

TS_Bildung_adj.pca <- prcomp(TS_Bildung_adj, center=TRUE, scale.=TRUE,
                             retx=TRUE)

TS_Bildung_adj.pca <- prcomp(TS_Bildung_adj, center=TRUE, scale.=TRUE,
                             retx=TRUE, rank.=1) 
TS_Bildung_adj.pca


# Um die Eigenvektoren in Faktorladungen umzuwandeln müssen wir die Werte, in der Scorespalte noch mit den Wurzeln der ersten Eigenwerte multiplizieren (= Standardabweichung der jeweils ersten Komponente)
GISD_Komponents1 <- data.frame(Variable=rownames(TS_Arbeitswelt_adj.pca$rotation),
                               Dimension="Arbeitswelt",
                               Anteil=unname(TS_Arbeitswelt_adj.pca$rotation^2),
                               Score=unname(TS_Arbeitswelt_adj.pca$rotation * TS_Arbeitswelt_adj.pca$sdev[1]))

GISD_Komponents2 <- data.frame(Variable=rownames(TS_Einkommen_adj.pca$rotation),
                               Dimension="Einkommen",
                               Anteil=unname(TS_Einkommen_adj.pca$rotation^2),
                               Score=unname(TS_Einkommen_adj.pca$rotation * TS_Einkommen_adj.pca$sdev[1]))

GISD_Komponents3 <- data.frame(Variable=rownames(TS_Bildung_adj.pca$rotation),
                               Dimension="Bildung (adj.)",
                               Anteil=unname(TS_Bildung_adj.pca$rotation^2),
                               Score=unname(TS_Bildung_adj.pca$rotation * TS_Bildung_adj.pca$sdev[1]))

GISD_Komponents <- rbind(GISD_Komponents1, GISD_Komponents2, GISD_Komponents3) %>%
  mutate(Proportion=round(Anteil*100, digits=1))

Resultdataset <- Impdata.imputed %>%
  mutate(TS_Arbeitswelt_adj=c(predict(TS_Arbeitswelt_adj.pca, newdata=.)),
         TS_Einkommen_adj  =c(predict(TS_Einkommen_adj.pca,   newdata=.)),
         TS_Bildung_adj    =c(predict(TS_Bildung_adj.pca,     newdata=.)))

Resultdataset %>%
  select(TS_Arbeitswelt_adj, TS_Einkommen_adj, TS_Bildung_adj) %>%
  summary()

descs <- Resultdataset %>%
  select(-Bevoelkerung) %>%
  pastecs::stat.desc()

Resultdataset %>%
  select(Arbeitslosigkeit,
         TS_Arbeitswelt_adj, TS_Einkommen_adj, TS_Bildung_adj) %>%
  cor(use="pairwise.complete.obs")

if(cor(Resultdataset$Arbeitslosigkeit, Resultdataset$TS_Bildung_adj, use="pairwise.complete.obs") < 0) {
  Resultdataset$TS_Bildung_adj <- -Resultdataset$TS_Bildung_adj
}

if(cor(Resultdataset$Arbeitslosigkeit, Resultdataset$TS_Arbeitswelt_adj, use="pairwise.complete.obs") < 0) {
  Resultdataset$TS_Arbeitswelt_adj <- -Resultdataset$TS_Arbeitswelt_adj
}

if(cor(Resultdataset$Arbeitslosigkeit, Resultdataset$TS_Einkommen_adj, use="pairwise.complete.obs") < 0) {
  Resultdataset$TS_Einkommen_adj <- -Resultdataset$TS_Einkommen_adj
}

Resultdataset %>%
  select(Arbeitslosigkeit,
         TS_Arbeitswelt_adj, TS_Einkommen_adj, TS_Bildung_adj) %>%
  cor(use="pairwise.complete.obs")

GISD_Komponents

Resultdataset <- Resultdataset %>%
  mutate(TS_Arbeitswelt_adj=(TS_Arbeitswelt_adj-min(TS_Arbeitswelt_adj))/(max(TS_Arbeitswelt_adj)-min(TS_Arbeitswelt_adj)),
         TS_Einkommen_adj  =(TS_Einkommen_adj  -min(TS_Einkommen_adj))  /(max(TS_Einkommen_adj)  -min(TS_Einkommen_adj)),
         TS_Bildung_adj    =(TS_Bildung_adj    -min(TS_Bildung_adj))    /(max(TS_Bildung_adj)    -min(TS_Bildung_adj)),
         GISD_Score=TS_Arbeitswelt_adj + TS_Einkommen_adj + TS_Bildung_adj) %>%
  group_by(Jahr) %>%
  mutate(GISD_Score=(GISD_Score-min(GISD_Score)) / (max(GISD_Score)-min(GISD_Score)),
         GISD_Score=round(GISD_Score, digits=5)) %>%
  ungroup()

Resultdataset %>%
  select(TS_Arbeitswelt_adj, TS_Einkommen_adj, TS_Bildung_adj,
         GISD_Score) %>%
  summary()

Resultdataset %>%
  select(TS_Arbeitswelt_adj, TS_Einkommen_adj, TS_Bildung_adj,
         GISD_Score) %>%
  str()

## V.  Datenexport - Erstellung der Datensätze 
RawResult <- Resultdataset %>%
  left_join(id_dataset, by=c("Gemeindekennziffer", "Bevoelkerung")) %>%
  rename(year         =Jahr, 
         population   =Bevoelkerung, 
         gisd_score   =GISD_Score, 
         gemeinde_id  =Gemeindekennziffer,
         gvb_id       =GVBKennziffer,
         kreis_id     =Kreiskennziffer,
         ror_id       =`Raumordnungsregion Nr`,
         nuts_2_id    =NUTS2,
         gemeinde_name=`Name der Gemeinde`,
         gvb_name     =`Name des Gemeindeverbands`,
         kreis_name   =`Name des Kreises`,
         ror_name     =Raumordnungsregion,
         nuts_2_name  =`NUTS2 Name`) %>%
  #IDs als String speichern wegen führenden Nullen (bitte jährlich prüfen)
  mutate(gemeinde_id=sprintf("%.8d", gemeinde_id),
         gvb_id     =sprintf("%.8d", as.numeric(gvb_id)), #Besonders hier
         kreis_id   =sprintf("%.5d", as.numeric(kreis_id)),
         ror_id     =sprintf("%.4d", as.numeric(ror_id)))

exportlist <- data.frame(Kennziffern=c("gemeinde_id",   "kreis_id",   "gvb_id",          "ror_id",             "nuts_2_id"),
                         Namen      =c("gemeinde_name", "kreis_name", "gvb_name",        "ror_name",           "nuts_2_name"),
                         Label      =c("Gemeinde",      "Kreis",      "Gemeindeverband", "Raumordnungsregion", "NUTS2"))


for(mykennziffer in exportlist$Kennziffern) {
  myname <- exportlist %>%
    filter(Kennziffern == mykennziffer) %>%
    pull(Namen)
  
  mylabel <- exportlist %>%
    filter(Kennziffern == mykennziffer) %>%
    pull(Label)
  
  print(paste("Level:", myname,"Label:", mylabel))

  outputdata <- RawResult
  
  outputdata$Group <- outputdata[[mykennziffer]]
  mergedataset  <- outputdata %>% select(ID=mykennziffer,myname,Bundesland) %>% 
    group_by(ID) %>% filter(row_number()==1) %>% ungroup() 
  names(mergedataset)[1]=mykennziffer
  
  outputdata.agg <- outputdata %>% 
    group_by(Group,year) %>% 
    select(Group,year,"population",gisd_score, TS_Bildung_adj, TS_Einkommen_adj, TS_Arbeitswelt_adj) %>% 
    summarise(gisd_score = weighted.mean(gisd_score, population), 
              TS_Bildung_adj = weighted.mean(TS_Bildung_adj, population), 
              TS_Einkommen_adj = weighted.mean(TS_Einkommen_adj, population),
              TS_Arbeitswelt_adj = weighted.mean(TS_Arbeitswelt_adj, population),
              population = sum(population))
  
  names(outputdata.agg)[1] <- mykennziffer
  outputdata.agg <- merge(outputdata.agg,mergedataset,by=mykennziffer) %>%  
    select(mykennziffer,myname,year,Bundesland,"population",gisd_score, TS_Bildung_adj, TS_Einkommen_adj, TS_Arbeitswelt_adj) %>%
    group_by(year) %>% as_tibble()
  
  for (i in 1998:2019) {
    outputdata.agg <- outputdata.agg %>% group_by(year) %>% mutate(gisd_score = ifelse(year == i,(gisd_score -min(gisd_score))/(max(gisd_score)-min(gisd_score)), gisd_score))
  }
  outputdata.agg <- outputdata.agg %>% group_by(year) %>% mutate(gisd_5 = findInterval(gisd_score, quantile(gisd_score,   probs=0:5/5 , type=9)),
                                                                 gisd_5 = findInterval(gisd_5, c(1:5)),
                                                                 gisd_10 = findInterval(gisd_score, quantile(gisd_score, probs=0:10/10 , type=9)),
                                                                 gisd_10 = findInterval(gisd_10, c(1:10)),
                                                                 gisd_k = findInterval(gisd_5, c(1,2,5))) %>% ungroup()
  
  summary(outputdata.agg %>% select(contains("gisd")))
  
  outputdata.agg <- outputdata.agg %>% mutate(gisd_score = round(gisd_score, digits = 5))
  
  mydata <- outputdata.agg %>% ungroup() %>% select(mykennziffer,
                                                    gisd_score,
                                                    gisd_5,
                                                    gisd_10,
                                                    gisd_k,
                                                    myname,
                                                    year)
  
  write.csv(mydata, paste0(outfiles_dir,"Bund/GISD_Bund_",mylabel,".csv"), row.names=FALSE, fileEncoding="UTF-8")
  
  names(mydata) <- gsub("\\.","_",make.names(names(mydata)))
  names(mydata) <- gsub("\\?","oe",names(mydata))
  names(mydata) <- gsub("\\?","ae",names(mydata))
  names(mydata) <- gsub("\\?","ue",names(mydata))
  names(mydata) <- gsub("\\?","ss",names(mydata))
  
  write_dta(mydata, paste0(outfiles_dir,"Bund/GISD_Bund_",mylabel,"_long.dta"))
  
  if (mylabel %in% c("Gemeindeverband","Kreis")) {
    
    outputdata <- RawResult 
    
    outputdata$Group <- outputdata[[mykennziffer]]
    mergedataset  <- outputdata %>% dplyr::select(ID=mykennziffer,myname,Bundesland) %>% 
      group_by(ID) %>% filter(row_number()==1) %>% ungroup() 
    names(mergedataset)[1]=mykennziffer
    
    outputdata.bula <- outputdata %>% 
      group_by(Group,year) %>% 
      dplyr::select(Group,year,"population",gisd_score, TS_Bildung_adj, TS_Einkommen_adj, TS_Arbeitswelt_adj) %>% 
      summarise(gisd_score = weighted.mean(gisd_score, population), 
                TS_Bildung_adj = weighted.mean(TS_Bildung_adj, population), 
                TS_Einkommen_adj = weighted.mean(TS_Einkommen_adj, population),
                TS_Arbeitswelt_adj = weighted.mean(TS_Arbeitswelt_adj, population),
                population = sum(population))
    
    names(outputdata.bula)[1] <- mykennziffer
    outputdata.bula <- merge(outputdata.bula,mergedataset,by=mykennziffer) %>%  
      select(mykennziffer,myname,year,Bundesland,"population",gisd_score, TS_Bildung_adj, TS_Einkommen_adj, TS_Arbeitswelt_adj) %>%
      group_by(year) %>% as_tibble()
    
    outputdata.bula <- outputdata.bula %>% ungroup() %>% filter(!(Bundesland %in% c("Bremen","Hamburg","Berlin"))) %>% group_by(year,Bundesland)
    
    for (i in 1998:2019) {
      outputdata.bula <- outputdata.bula %>% group_by(year, Bundesland) %>% mutate(gisd_score = ifelse(year == i,(gisd_score -min(gisd_score))/(max(gisd_score)-min(gisd_score)), gisd_score)) %>% ungroup()
    }
    
    summary(outputdata.bula %>% select(contains("gisd")))
    
    outputdata.bula <- outputdata.bula %>% mutate(gisd_score = round(gisd_score, digits = 5))
    
    # Ausgabe Bundesländer
    ListeBula <- unique(outputdata.bula$Bundesland)  
    for(myland in ListeBula) {
      mydata.bula <- outputdata.bula %>% filter(Bundesland==myland) %>% ungroup() %>% select(gisd_score, mykennziffer, myname, year)
      
      write.csv(mydata.bula, paste0(outfiles_dir,"Bundesland/GISD_",myland,"_",mylabel,".csv"), row.names=FALSE, fileEncoding="UTF-8")
      
      names(mydata.bula) <- gsub("\\.","_",make.names(names(mydata.bula)))
      names(mydata.bula) <- gsub("\\?","oe",names(mydata.bula))
      names(mydata.bula) <- gsub("\\?","ae",names(mydata.bula))
      names(mydata.bula) <- gsub("\\?","ue",names(mydata.bula))
      names(mydata.bula) <- gsub("\\?","ss",names(mydata.bula))
      
      write_dta(mydata.bula, paste0(outfiles_dir,"Bundesland/GISD_",myland,"_",mylabel,".dta"))
    }
  }  
}


## VI.  Datensätze für PLZ generieren
load(paste0(infiles_dir,"Referenz/GEM_Zipcode_Intersections_2015.RData")) # AGS/Postcode-Intersections-Dataset in sf format

  # Goettingen hat im Jahr 2017 neue GKZ bekommen.
  # Die PLZ-Referenz ist aus 2015, also müssen die GKZ dort aktualisiert werden
PLZ_update <- read_excel(paste0(infiles_dir, "Referenz/Gebietsstandsaenderungen_Goettingen_PLZ.xlsx"), na="NA") %>%
  mutate(gkz_alt = if_else(nchar(as.character(gkz_alt)) == 7, #Führende Null einfügen wenn GKZ nur 7-stellig
                                 paste0("0", gkz_alt),
                                 as.character(gkz_alt)),
         gkz_neu = if_else(nchar(as.character(gkz_neu)) == 7, #Führende Null einfügen wenn GKZ nur 7-stellig
                                 paste0("0", gkz_neu),
                                 as.character(gkz_neu))) %>%
  select(-gemeindename)

PLZ.df <- left_join(PLZ.df %>% ungroup() %>% mutate(AGS=as.character(AGS)),
                    PLZ_update,
                    by = c("AGS" = "gkz_alt")) %>%
  mutate(AGS = ifelse(is.na(gkz_neu), AGS, gkz_neu)) %>%
  select(-gkz_neu)

rm(PLZ_update)

for (mykennziffer in c("PLZ2","PLZ3","PLZ4","PLZ5")) {
  #Tiefe der PLZ merken
  plz_ebene <- as.numeric(substr(mykennziffer, nchar(mykennziffer), nchar(mykennziffer)))
  
  myname <- paste0(mykennziffer)
  mylabel <- paste0(mykennziffer)
  print(paste("Level:",myname,"Label:",mylabel))
  
  outputdata.plz <- Resultdataset
  
  outputdata.plz <- outputdata.plz %>% rename(gemeinde_id = Gemeindekennziffer, year = Jahr, gisd_score = GISD_Score)
  
  outputdata.plz <- outputdata.plz %>% select(AGS=gemeinde_id,year,gisd_score)
  
  outputdata.plz <- left_join(as.data.frame(PLZ.df) %>% ungroup() %>% mutate(AGS=as.numeric(as.character(AGS))),
                              outputdata.plz,
                              by=c("AGS"),
                              relationship = "many-to-many")
  
  outputdata.plz <- outputdata.plz %>% mutate(AGS = as.character(AGS),
                                      AGS = ifelse(nchar(AGS)<8,paste0("0",AGS),AGS))
  
  outputdata.plz <- outputdata.plz %>% filter(!is.na(mykennziffer) & !is.na(EW_Area) & !is.na(year) & EW_Area>0)
  mycol <- which(mykennziffer %in% names(outputdata.plz))
  outputdata.plz <- outputdata.plz %>% group_by(year,AGS) 
  outputdata.plz <- outputdata.plz %>% mutate(gisd_score = weighted.mean(gisd_score,EW_Area))
  outputdata.plz <- outputdata.plz %>% group_by_at(vars("year",mykennziffer)) %>% 
    summarise(gisd_score = weighted.mean(gisd_score,EW_Area), population = sum(EW_Area)) %>%
    group_by(year)
  
  outputdata.plz <- outputdata.plz %>%  mutate(gisd_score = round((gisd_score -min(gisd_score ))/(max(gisd_score )-min(gisd_score)), digits=6),
                                       gisd_5 = findInterval(gisd_score, quantile(gisd_score,   probs=0:5/5 , type=9)),
                                       gisd_5 = findInterval(gisd_5, c(1:5)),
                                       gisd_10 = findInterval(gisd_score, quantile(gisd_score, probs=0:10/10 , type=9)),
                                       gisd_10 = findInterval(gisd_10, c(1:10)),
                                       gisd_k = findInterval(gisd_5, c(1,2,5)))
  summary(outputdata.plz)            
  head(outputdata.plz)
  ListeJahre <- unique(outputdata.plz$year)
  mydata <- outputdata.plz %>% ungroup()
  
  #Führende Null einfügen wenn PLZ zu wenig stellen hat
  mydata <- mydata %>% 
    mutate(!!sym(mykennziffer):= if_else(!nchar(as.character(!!sym(mykennziffer))) == plz_ebene,
                                  paste0("0", !!sym(mykennziffer)),
                                  as.character(!!sym(mykennziffer))))
  
  #Speichern
  write.csv(mydata, paste0(outfiles_dir,"Bund/GISD_Bund_",mylabel,".csv"), row.names=FALSE, fileEncoding="UTF-8")
  
  mydata <- outputdata.plz %>% ungroup() 
  names(mydata) <- gsub("\\.","_",make.names(names(mydata)))
  names(mydata) <- gsub("\\?","oe",names(mydata))
  names(mydata) <- gsub("\\?","ae",names(mydata))
  names(mydata) <- gsub("\\?","ue",names(mydata))
  names(mydata) <- gsub("\\?","ss",names(mydata))
  
  write_dta(mydata, paste0(outfiles_dir,"Bund/GISD_Bund_",mylabel,"_long.dta"))
}