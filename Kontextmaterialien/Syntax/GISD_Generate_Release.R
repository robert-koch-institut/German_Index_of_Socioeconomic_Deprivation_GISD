# GISD - German Index of Socio-Economic Deprivation
# Author: 
# Citation: 

# Revision: 2022.v3
# Date: 2022-11-22

# Libraries
library("tidyverse")
library("bookdown") 
library("readxl")
library("zoo")
library("imputeTS")
library("haven")
library("sf")
library(pastecs)

# Path Definitions
home <- getwd()
outfiles_dir <- "S:/OE/FG28/205 Regionale Unterschiede/GISD/"
outfiles_daten <- "C:/projects_rstudio/GISD/Outfiles"
dir.create("Outfiles", showWarnings=T)
dir.create("Outfiles/2022_v03", showWarnings=T)
dir.create("Outfiles/2022_v03/Bund", showWarnings=T)
dir.create("Outfiles/2022_v03/Other", showWarnings=T)
dir.create("Outfiles/2022_v03/Stata", showWarnings=T)
setwd(home)
getwd()


## I.  Generierung eines ID-Datensatzes
print_missings = function(data) {
  df = data[-1,]; 
  if(sum(is.na(df))>0){print("Missing observations: "); print(df[!complete.cases(df),])}; 
  df}

load_dataset = function(sheet) {
  suppressMessages(
    read_excel("Data/Referenz/Referenz_1998_2019.xls", sheet = sheet, na = "NA")
  )
}

Gemeinden_INKAR <- load_dataset("Gemeinden-GVB") %>% 
  na.omit() %>%
  mutate(Kennziffer=as.numeric(gem19),GVBKennziffer=gvb19, fl19=as.numeric(fl19))

Gemeindeverbaende_INKAR <- load_dataset("Gemeindeverbände") %>% 
  na.omit() %>% 
  select(GVBKennziffer=gvb19,"Name des Gemeindeverbands"=gvb19name)

Kreise_INKAR <- load_dataset("KRS") %>%
  mutate(krs19= as.numeric(krs19), fl19 = as.numeric(fl19))

id_dataset <- Gemeinden_INKAR %>% 
  select(Gemeindekennziffer=Kennziffer,"Name der Gemeinde"=gem19name,GVBKennziffer, "Bevölkerung") %>% 
  mutate(Kreiskennziffer=floor(Gemeindekennziffer/1000)) %>%
  left_join(.,Kreise_INKAR %>% select("Kreiskennziffer"=krs19,
                                      "Name des Kreises"=krs19name,
                                      "Raumordnungsregion Nr"=ROR11,
                                      "Raumordnungsregion"=ROR11name,
                                      NUTS2,
                                      "NUTS2 Name"=NUTS2name,
                                      "Bundesland"=Bundesland),by="Kreiskennziffer")%>%
  left_join(.,Gemeindeverbaende_INKAR, by="GVBKennziffer")


## II. Erzeugen eines Datensatzes mit Kennziffern als ID unabhängig von der Ebene
Basedata <- Kreise_INKAR %>% select(Kennziffer=krs19) %>% mutate(Jahr=2019)

inputdataset <- list.files("Data/INKAR_1998_2019/")

inputdataset <- inputdataset[-c(13,14)];
inputdataset

for(file in inputdataset){
  suppressMessages(myimport <- read_excel(paste0("Data/INKAR_1998_2019/",file), skip = 1, sheet = "Daten"))
  names(myimport)[1] <- "Kennziffer"
  myimport[2:3] <- NULL
  myimport <- myimport %>% gather(key = "Jahr", value = "Value" , -"Kennziffer", convert=T, na.rm = T) %>%
    mutate(Kennziffer=as.numeric(as.character(Kennziffer)), Value=as.numeric(Value))
  names(myimport)[3] <- strsplit(strsplit(file,"_")[[1]][2],"[.]")[[1]][1]
  Basedata <- full_join(Basedata, myimport, by=c("Kennziffer", "Jahr"))
}

rm(inputdataset)

listofdeterminants <- names(Basedata)[3:length(Basedata)]

ind_level <- c("Gemeindeverband","Gemeindeverband","Kreis", "Gemeindeverband", "Kreis", "Kreis", "Kreis", "Kreis", "Kreis", "Gemeindeverband", "Kreis", "Kreis")
level_table <- cbind(listofdeterminants,ind_level)
# Tabelle der Indikatoren mit regionaler Tiefe
ind_col = c("Indikator","Tiefe des Indikators")

Basedata_Gemeindeverbandsebene <- Basedata %>% select(Kennziffer,Jahr,Arbeitslosigkeit,Beschaeftigtenquote,ErwerbsfaehigeBevoelkerung,Einkommensteuer) %>%   
  gather(key,value,3:5) %>% filter(!is.na(value)) %>% spread(key,value) %>% filter(Jahr>=1998) %>% rename("Gemeindeverband"=Kennziffer)

Basedata_Kreisebene <- Basedata %>% select(krs15=Kennziffer,Jahr,listofdeterminants) %>% 
  select(-Arbeitslosigkeit,-Einkommensteuer,-Beschaeftigtenquote, -ErwerbsfaehigeBevoelkerung)%>% filter(Jahr>=1998) %>% rename(Kreis=krs15)

Workfile <- as.data.frame(expand.grid("Kennziffer"=Gemeinden_INKAR %>% pull(Kennziffer),"Jahr"=seq(min(Basedata$Jahr):max(Basedata$Jahr)) + min(Basedata$Jahr)-1)) %>% mutate(Kreiskennziffer=floor(as.numeric(Kennziffer)/1000)) %>% as_tibble() %>%
  left_join(. , Gemeinden_INKAR,by=c("Kennziffer")) %>%
  select(Gemeindekennziffer="Kennziffer",Kreis="Kreiskennziffer",Gemeindeverband=GVBKennziffer,Jahr,Bevoelkerung="Bevölkerung") %>% mutate(Gemeindeverband=as.numeric(Gemeindeverband), Bevoelkerung=as.numeric(Bevoelkerung)) %>% 
  arrange("Gemeindeverband","Jahr") %>% # Join Metadata
  left_join(. , Basedata_Kreisebene,by=c("Kreis","Jahr")) %>%
  left_join(. , Basedata_Gemeindeverbandsebene,by=c("Gemeindeverband","Jahr")) %>%
  filter(Jahr>=1998)

rm(myimport)

NUTS2 <- id_dataset %>% select(NUTS2) %>% mutate(Jahr = 2019) %>% distinct()

inputdataset <- list.files("Data/INKAR_1998_2019/NUTS2/")

for(file in inputdataset){
  suppressMessages(myimport <- read_excel(paste0("Data/INKAR_1998_2019/NUTS2/",file), skip = 1, sheet = "Daten"))
  names(myimport)[1] <- "NUTS2"
  myimport[2:3] <- NULL
  myimport <- myimport %>% gather(key = "Jahr", value = "Value" , -"NUTS2", convert=T, na.rm = T) %>%
    mutate(Value=as.numeric(Value))
  names(myimport)[3] <- strsplit(strsplit(file,"_")[[1]][2],"[.]")[[1]][1]
  NUTS2 <- full_join(NUTS2, myimport, by=c("NUTS2", "Jahr"))
}

NUTS2 <- NUTS2 %>% left_join(id_dataset, by = "NUTS2") %>% mutate(Gemeindeverband = GVBKennziffer) %>% select(Jahr, BevoelkerungmitakadAbschluss, BevoelkerungohneAbschluss, Gemeindeverband)

Workfile <- Workfile %>% left_join(NUTS2, by = c("Gemeindeverband", "Jahr")) %>% distinct()

rm(myimport)

inputdataset <- list.files("Data/INKAR_1998_2019/Indikatoren_Kreisbene/")

for(file in inputdataset){
  suppressMessages(myimport <- read_excel(paste0("Data/INKAR_1998_2019/Indikatoren_Kreisbene/",file), skip = 1, sheet = "Daten"))
  names(myimport)[1] <- "Kennziffer"
  myimport[2:3] <- NULL
  myimport <- myimport %>% gather(key = "Jahr", value = "Value" , -"Kennziffer", convert=T, na.rm = T) %>%
    mutate(Kennziffer=as.numeric(as.character(Kennziffer)), Value=as.numeric(Value))
  names(myimport)[3] <- strsplit(strsplit(file,"_")[[1]][2],"[.]")[[1]][1]
  myimport <- myimport %>% mutate(Kreis = Kennziffer) %>% select(-Kennziffer)
  Workfile <- full_join(Workfile, myimport, by=c("Kreis", "Jahr"))
}

Workfile <- Workfile %>% mutate(ErwerbsfaehigeBevoelkerung = ifelse(Jahr < 2001, ErwerbsfaehigeBevoelkerungKreis, ErwerbsfaehigeBevoelkerung), Arbeitslosigkeit = ifelse(Jahr < 2001, ArbeitslosigkeitKreis, Arbeitslosigkeit), Beschaeftigtenquote = ifelse(Jahr < 2001, BeschaeftigtenquoteKreis, Beschaeftigtenquote)) %>% select(-ErwerbsfaehigeBevoelkerungKreis, -ArbeitslosigkeitKreis, -BeschaeftigtenquoteKreis)

rm(myimport)

Gemeinden_ohne_Bevoelkerung <- Workfile %>% filter(Bevoelkerung==0)
write_dta(Gemeinden_ohne_Bevoelkerung, paste0("Outfiles/Gemeinden_ohne_Bevoelkerung.dta"))
rm(Gemeinden_ohne_Bevoelkerung)

Workfile <- Workfile %>% filter(Bevoelkerung>0) %>% mutate(BeschaeftigteohneAbschluss = round(BeschaeftigteohneAbschluss / SVBeschaeftigte * 100, digits = 2), BeschaeftigtemitakadAbschluss = round(BeschaeftigtemitakadAbschluss / SVBeschaeftigte * 100, digits = 2)) %>%
  select(-SVBeschaeftigte)
Basedata <- Basedata %>% select(-SVBeschaeftigte)
Basedata_Kreisebene <- Basedata_Kreisebene %>% select(-SVBeschaeftigte)
level_table <- level_table[-9,]
listofdeterminants <- listofdeterminants[-9]

Workfile <- Workfile %>% filter(Bevoelkerung>0) %>% mutate(Arbeitslosigkeit = ifelse(is.na(ErwerbsfaehigeBevoelkerung),NA,round(Arbeitslosigkeit / ErwerbsfaehigeBevoelkerung * 1000, digits = 2)), Arbeitslosigkeit=ifelse(is.nan(Arbeitslosigkeit),NA,Arbeitslosigkeit),
                                                           Arbeitslosigkeit=ifelse(is.infinite(Arbeitslosigkeit),NA,Arbeitslosigkeit)) %>% select(-ErwerbsfaehigeBevoelkerung)
Basedata <- Basedata %>% select(-ErwerbsfaehigeBevoelkerung)
Basedata_Gemeindeverbandsebene <- Basedata_Gemeindeverbandsebene %>% select(-ErwerbsfaehigeBevoelkerung)
level_table <- level_table[-4,]
listofdeterminants <- listofdeterminants[-4]

Workfile <- Workfile %>% filter(Bevoelkerung>0)


## III.Imputation fehlender Werte
Verbraucherpreisindex <- data.frame(Jahr = c(1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020),
                                    VBindex =  c(78.3, 78.8 , 79.9, 81.5, 82.6, 83.5, 84.9, 86.2, 87.6, 89.6, 91.9, 92.2, 93.2, 95.2, 97.1, 98.5, 99.5, 100, 100.5, 102, 103.8, 105.3, 105.8))

Workfile <- Workfile %>% left_join(Verbraucherpreisindex, by = "Jahr")

Workfile <- Workfile %>% mutate(Einkommensteuer = Einkommensteuer / VBindex * 100, Haushaltseinkommen = Haushaltseinkommen / VBindex * 100, Bruttoverdienst = Bruttoverdienst / VBindex * 100)


Workfile <- Workfile %>% mutate(Einkommensteuer_ln = ifelse(Einkommensteuer==0, 0.75, log(Einkommensteuer)))

Workfile <- Workfile %>% mutate(Haushaltseinkommen_ln = log(Haushaltseinkommen))

Workfile <- Workfile %>% mutate(Bruttoverdienst_ln = log(Bruttoverdienst))

Workfile <- Workfile %>%
  mutate(G8_jahr = case_when(Kreis < 2000 & Jahr == 2016 ~ 1,
                             Kreis > 1999 & Kreis < 3000 & Jahr == 2010 ~ 1,
                             Kreis > 2999 & Kreis < 4000 & Jahr == 2011 ~ 1,
                             Kreis > 3999 & Kreis < 5000 & Jahr == 2012 ~ 1,
                             Kreis > 4999 & Kreis < 6000 & Jahr == 2013 ~ 1,
                             Kreis > 5999 & Kreis < 7000 & Jahr == 2013 ~ 1,
                             Kreis > 7999 & Kreis < 9000 & Jahr == 2012 ~ 1,
                             Kreis > 8999 & Kreis < 10000 & Jahr == 2011 ~ 1,
                             Kreis > 9999 & Kreis < 11000 & Jahr == 2009 ~ 1,
                             Kreis > 10999 & Kreis < 12000 & Jahr == 2012 ~ 1,
                             Kreis > 11999 & Kreis < 13000 & Jahr == 2012 ~ 1,
                             Kreis > 12999 & Kreis < 14000 & Jahr == 2008 ~ 1,
                             Kreis > 14999 & Kreis < 16000 & Jahr == 2007 ~ 1),
         SN_KA = ifelse(Jahr == 2001 & Kreis > 14999 & Kreis < 16000, 1, 0),
         THvor2004 = ifelse(Jahr < 2004 & Kreis > 15999, 1, 0))
Workfile$G8_jahr[is.na(Workfile$G8_jahr)] = 0
Workfile$SN_KA[is.na(Workfile$SN_KA)] = 0
Workfile$THvor2004[is.na(Workfile$THvor2004)] = 0

adj_G8_jahr <- function(data,outcome_name){
  mydata   <- data %>%
    group_by(Gemeindekennziffer) %>% 
    select(Gemeindekennziffer, Jahr, G8_jahr, SN_KA, THvor2004, "Outcome"=paste(outcome_name)) %>% 
    mutate(MEAN=mean(Outcome , na.rm=T)) %>% ungroup()
  
  mymodell2 <- lm(Outcome ~
                    I(Jahr*Jahr*MEAN) + I(Jahr*MEAN) + G8_jahr + SN_KA + THvor2004, data = mydata  , na.action="na.exclude")
  print(mymodell2)
  mydata %>% mutate(coef = summary(mymodell2)$coefficients[4,1], coef_SH = summary(mymodell2)$coefficients[5,1], coef_TH = summary(mymodell2)$coefficients[6,1]) %>%
    mutate(Outcome = ifelse(G8_jahr == 1, Outcome - coef, Outcome), Outcome = ifelse(SN_KA == 1, Outcome - coef_SH, Outcome), Outcome = ifelse(THvor2004 == 1, Outcome - coef_TH, Outcome)) %>%
    pull(Outcome)
}

Workfile <- Workfile %>% 
  mutate(SchulabgaengermitHochschulreife_adj = adj_G8_jahr(.,"SchulabgaengermitHochschulreife"),
         SchulabgaengerohneAbschluss_adj = adj_G8_jahr(.,"SchulabgaengerohneAbschluss"))

Workfile <- Workfile %>% mutate(OW = ifelse(Kreis < 11000, 0, 1))

OW <- function(data,outcome_name){
  mydata   <- data %>%
    select(Gemeindekennziffer, Jahr, OW, "Outcome"=paste(outcome_name)) %>% 
    mutate(Jahr_Dummy = as.factor(Jahr)) %>% ungroup()
  
  mymodell_ow <- lm(Outcome ~ Jahr_Dummy + relevel(Jahr_Dummy, ref = "2012") * OW, data = mydata, na.action="na.exclude")
  
  print(mymodell_ow)
  summary(mymodell_ow)$coefficients[23,1]
  mydata %>% mutate(coef = summary(mymodell_ow)$coefficients[18,1]) %>%
    mutate(Outcome = ifelse(OW == 1, Outcome - coef, Outcome)) %>%
    pull(Outcome)
}

Workfile <- Workfile %>% 
  mutate(BeschaeftigteohneAbschluss_adj = OW(.,"BeschaeftigteohneAbschluss"))

Workfile_spread <- Workfile %>% filter(Jahr == 2013) %>% spread(key = Jahr, value = BeschaeftigteohneAbschluss_adj) %>% select("2013", Gemeindekennziffer ) %>% mutate(Jahr = 2012) %>% rename(BeschaeftigteohneAbschluss_adj = "2013")

Workfile <- Workfile %>% left_join(Workfile_spread, by = c("Gemeindekennziffer", "Jahr")) %>% mutate(BeschaeftigteohneAbschluss_adj = ifelse(Jahr == 2012, BeschaeftigteohneAbschluss_adj.y, BeschaeftigteohneAbschluss_adj.x)) %>% select(-BeschaeftigteohneAbschluss_adj.y, -BeschaeftigteohneAbschluss_adj.x)

Workfile_spread <- Workfile %>% filter(Jahr == 2013) %>% spread(key = Jahr, value = BeschaeftigtemitakadAbschluss) %>% select("2013", Gemeindekennziffer ) %>% mutate(Jahr = 2012) %>% rename(BeschaeftigtemitakadAbschluss = "2013")

Workfile <- Workfile %>% left_join(Workfile_spread, by = c("Gemeindekennziffer", "Jahr")) %>% mutate(BeschaeftigtemitakadAbschluss = ifelse(Jahr == 2012, BeschaeftigtemitakadAbschluss.y, BeschaeftigtemitakadAbschluss.x)) %>% select(-BeschaeftigtemitakadAbschluss.y, -BeschaeftigtemitakadAbschluss.x)

Workfile <- Workfile %>% mutate(Messaenderung_Besch = ifelse(Jahr > 2012, 1, 0))

Messaenderung <- function(data,outcome_name){
  mydata   <- data %>%
    select(Gemeindekennziffer, Jahr, Messaenderung_Besch, "Outcome"=paste(outcome_name)) %>% 
    mutate(MEAN=mean(Outcome , na.rm=T)) %>% ungroup()
  
  mymodell_Messaenderung <- lm(Outcome ~
                                 I(Jahr*Jahr*MEAN) + I(Jahr*MEAN) + Messaenderung_Besch, data = mydata  , na.action="na.exclude")
  
  print(mymodell_Messaenderung) 
  summary(mymodell_Messaenderung)$coefficients[4,1]
  mydata <- mydata %>% 
    mutate(coef = summary(mymodell_Messaenderung)$coefficients[4,1]) %>%
    mutate(Outcome = ifelse(Jahr < 2012, Outcome + coef, Outcome)) %>%
    pull(Outcome)
}

Workfile <- Workfile %>% 
  mutate(BeschaeftigteohneAbschluss_adj = Messaenderung(.,"BeschaeftigteohneAbschluss_adj"),
         BeschaeftigtemitakadAbschluss_adj = Messaenderung(.,"BeschaeftigtemitakadAbschluss"))

Impdata <-  Workfile %>%  filter(Jahr>=1998) %>% 
  gather(key,value,6:15) %>% mutate(value=ifelse(value<0.00001,NA,value)) %>% spread(key,value)

listofdeterminants <- c("Arbeitslosigkeit", "SchulabgaengerohneAbschluss_adj", "Beschaeftigtenquote", "Bruttoverdienst_ln", "BeschaeftigtemitakadAbschluss_adj", "BeschaeftigteohneAbschluss_adj", "Einkommensteuer_ln", "Haushaltseinkommen_ln", "Schuldnerquote", "BevoelkerungohneAbschluss", "BevoelkerungmitakadAbschluss", "SchulabgaengermitHochschulreife_adj")

my_ts_imputer <- function(data,outcome_name){
  mydata   <- data %>% group_by(Gemeindekennziffer) %>%
    select(Gemeindekennziffer,Jahr,"Outcome"=paste(outcome_name)) %>% 
    mutate(MEAN=mean(Outcome , na.rm=T)) %>% ungroup()
  mymodell <- lm(Outcome ~
                   I(Jahr*Jahr*MEAN) + I(Jahr*MEAN),
                 data = mydata  , na.action="na.exclude")
  mydata %>% select(Outcome) %>% mutate(Imputed = predict(mymodell, newdata =mydata )) %>%
    mutate(Outcome=ifelse(is.na(Outcome),Imputed,Outcome)) %>% 
    mutate(Outcome=ifelse(is.nan(Outcome),Imputed,Outcome)) %>%
    mutate(Outcome=ifelse(is.infinite(Outcome),Imputed,Outcome)) %>%
    mutate(Outcome=ifelse(Outcome<0,0,Outcome)) %>% pull(Outcome)
}

Impdata.imputed <- Impdata %>%
  mutate(Arbeitslosigkeit=my_ts_imputer(.,"Arbeitslosigkeit"),
         SchulabgaengerohneAbschluss_adj=my_ts_imputer(.,"SchulabgaengerohneAbschluss_adj"),
         SchulabgaengerohneAbschluss=my_ts_imputer(.,"SchulabgaengerohneAbschluss"),
         Beschaeftigtenquote=my_ts_imputer(.,"Beschaeftigtenquote"),
         Bruttoverdienst_ln=my_ts_imputer(.,"Bruttoverdienst_ln"),
         Bruttoverdienst=my_ts_imputer(.,"Bruttoverdienst"),
         BeschaeftigtemitakadAbschluss_adj=my_ts_imputer(.,"BeschaeftigtemitakadAbschluss_adj"),
         BeschaeftigtemitakadAbschluss=my_ts_imputer(.,"BeschaeftigtemitakadAbschluss"),
         BeschaeftigteohneAbschluss_adj=my_ts_imputer(.,"BeschaeftigteohneAbschluss_adj"),
         BeschaeftigteohneAbschluss=my_ts_imputer(.,"BeschaeftigteohneAbschluss"),
         Einkommensteuer_ln=my_ts_imputer(.,"Einkommensteuer_ln"),
         Einkommensteuer=my_ts_imputer(.,"Einkommensteuer"),
         Haushaltseinkommen_ln=my_ts_imputer(.,"Haushaltseinkommen_ln"),
         Haushaltseinkommen=my_ts_imputer(.,"Haushaltseinkommen"),
         Schuldnerquote=my_ts_imputer(.,"Schuldnerquote"),
         BevoelkerungohneAbschluss=my_ts_imputer(.,"BevoelkerungohneAbschluss"),
         BevoelkerungmitakadAbschluss=my_ts_imputer(.,"BevoelkerungmitakadAbschluss"))

summary(as.data.frame(Impdata.imputed) %>% ungroup()  %>% select(listofdeterminants))

Impdata.imputed <- Impdata.imputed %>% mutate(BundeslandKZ = round(Kreis / 1000))

my_ts_imputer_NUTS2 <- function(data,outcome_name){
  mydata   <- data %>% group_by(BundeslandKZ) %>%
    select(BundeslandKZ,Jahr,"Outcome"=paste(outcome_name)) %>% 
    mutate(MEAN=mean(Outcome , na.rm=T)) %>% ungroup()
  mymodell <- lm(Outcome ~
                   I(Jahr*Jahr*MEAN) + I(Jahr*MEAN),
                 data = mydata  , na.action="na.exclude")
  mydata %>% select(Outcome) %>% mutate(Imputed = predict(mymodell, newdata =mydata )) %>%
    mutate(Outcome=ifelse(is.na(Outcome),Imputed,Outcome)) %>% 
    mutate(Outcome=ifelse(Outcome<0,0,Outcome)) %>% pull(Outcome)
}

Impdata.imputed <- Impdata.imputed %>%
  mutate(BevoelkerungohneAbschluss=my_ts_imputer_NUTS2(.,"BevoelkerungohneAbschluss"),
         BevoelkerungmitakadAbschluss=my_ts_imputer_NUTS2(.,"BevoelkerungmitakadAbschluss"))

summary(as.data.frame(Impdata.imputed) %>% ungroup()  %>% select(listofdeterminants))

Impdata.imputed <- Impdata.imputed %>% select(-BundeslandKZ)

my_ts_imputer <- function(data,outcome_name){
  mydata   <- data %>%
    select(Gemeindekennziffer,Jahr,Arbeitslosigkeit,SchulabgaengerohneAbschluss_adj,Beschaeftigtenquote,Bruttoverdienst_ln,BeschaeftigtemitakadAbschluss_adj,BeschaeftigteohneAbschluss_adj,Einkommensteuer_ln,Haushaltseinkommen_ln,"Outcome"=paste(outcome_name)) %>% mutate(MEAN=mean(Outcome , na.rm=T))
  mymodell <- lm(Outcome ~
                   I(Jahr*Jahr*MEAN) + I(Jahr*MEAN) + Arbeitslosigkeit + 
                   SchulabgaengerohneAbschluss_adj + Beschaeftigtenquote + Bruttoverdienst_ln + BeschaeftigtemitakadAbschluss_adj + BeschaeftigteohneAbschluss_adj + Einkommensteuer_ln + Haushaltseinkommen_ln ,
                 data = mydata  , na.action="na.exclude")
  mydata %>% select(Outcome) %>% mutate(Imputed = predict(mymodell, newdata =mydata )) %>%
    mutate(Outcome=ifelse(is.na(Outcome),Imputed,Outcome)) %>% 
    mutate(Outcome=ifelse(Outcome<0,0,Outcome)) %>% pull(Outcome)
}

Impdata.imputed <- Impdata.imputed %>% mutate(
  SchulabgaengermitHochschulreife_adj=my_ts_imputer(.,"SchulabgaengermitHochschulreife_adj"),
  SchulabgaengermitHochschulreife=my_ts_imputer(.,"SchulabgaengermitHochschulreife")
)

summary(Impdata.imputed$SchulabgaengermitHochschulreife_adj)

Impdata.imputed <- Impdata.imputed %>% filter(Gemeindekennziffer!=16063104)


## IV. Faktorenanalyse (Hauptkomponentenanalyse) inklusive Generierung der Faktorscores
TS_Arbeitswelt_adj <- Impdata.imputed  %>% ungroup() %>% filter(Jahr > 1999) %>% select(Beschaeftigtenquote,Arbeitslosigkeit,Bruttoverdienst_ln)

TS_Einkommen_adj   <- Impdata.imputed %>% ungroup %>% filter(Jahr > 1999) %>% select(Einkommensteuer_ln,Haushaltseinkommen_ln,Schuldnerquote) 

TS_Bildung_adj <- Impdata.imputed %>% ungroup %>% filter(Jahr > 1999) %>% select(BeschaeftigtemitakadAbschluss_adj,BeschaeftigteohneAbschluss_adj,SchulabgaengerohneAbschluss_adj)

TS_Arbeitswelt_adj.pca <- prcomp(TS_Arbeitswelt_adj, center = TRUE, scale. = TRUE, retx=TRUE)
plot(TS_Arbeitswelt_adj.pca)

TS_Arbeitswelt_adj.pca <- prcomp(TS_Arbeitswelt_adj, center = TRUE, scale. = TRUE, retx=TRUE, rank. = 1)
TS_Arbeitswelt_adj.pca

TS_Einkommen_adj.pca <- prcomp(TS_Einkommen_adj, center = TRUE, scale. = TRUE, retx=TRUE) 
plot(TS_Einkommen_adj.pca)
TS_Einkommen_adj.pca <- prcomp(TS_Einkommen_adj, center = TRUE, scale. = TRUE, retx=TRUE, rank. = 1) 
TS_Einkommen_adj.pca

TS_Bildung_adj.pca <- prcomp(TS_Bildung_adj, center = TRUE, scale. = TRUE, retx=TRUE) 
plot(TS_Bildung_adj.pca)
TS_Bildung_adj.pca <- prcomp(TS_Bildung_adj, center = TRUE, scale. = TRUE, retx=TRUE, rank. =1 ) 
TS_Bildung_adj.pca

GISD_Komponents <- cbind("Teildimension"="Arbeitswelt","Anteil"=TS_Arbeitswelt_adj.pca$rotation^2,"Score"=TS_Arbeitswelt_adj.pca$rotation) 
GISD_Komponents <- rbind(GISD_Komponents,cbind("Teildimension"="Einkommen","Anteil"=TS_Einkommen_adj.pca$rotation^2,"Score"=TS_Einkommen_adj.pca$rotation)) 
GISD_Komponents <- rbind(GISD_Komponents,cbind("Teildimension"="Bildung (adj.)","Anteil"=TS_Bildung_adj.pca$rotation^2,"Score"=TS_Bildung_adj.pca$rotation)) 
GISD_Komponents <- cbind("Variables"=as.data.frame(rownames(GISD_Komponents)),as.data.frame(GISD_Komponents))

rownames(GISD_Komponents) <- NULL
colnames(GISD_Komponents) <- c("Variable","Dimension","Anteil","Score")
GISD_Komponents$GISD <- "GISD"
GISD_Komponents$Proportion <- round(as.numeric(as.character(GISD_Komponents$Anteil))*100,digits=1)

Resultdataset <- Impdata.imputed
Resultdataset$TS_Arbeitswelt_adj <- as.numeric(predict(TS_Arbeitswelt_adj.pca, newdata = Impdata.imputed))
Resultdataset$TS_Einkommen_adj <- as.numeric(predict(TS_Einkommen_adj.pca , newdata = Impdata.imputed))
Resultdataset$TS_Bildung_adj <- as.numeric(predict(TS_Bildung_adj.pca , newdata = Impdata.imputed))

summary(Resultdataset %>% select(TS_Arbeitswelt_adj, TS_Einkommen_adj, TS_Bildung_adj))
descs <- stat.desc(Resultdataset[, -5])

Resultdataset %>% select(Arbeitslosigkeit,TS_Arbeitswelt_adj,TS_Einkommen_adj,TS_Bildung_adj)  %>% cor( use="pairwise.complete.obs")

if (cor(Resultdataset$Arbeitslosigkeit, Resultdataset$TS_Bildung_adj,use="pairwise.complete.obs")<0) {
  Resultdataset$TS_Bildung_adj <- Resultdataset$TS_Bildung_adj*-1
}
if (cor(Resultdataset$Arbeitslosigkeit, Resultdataset$TS_Arbeitswelt_adj,use="pairwise.complete.obs")<0) {
  Resultdataset$TS_Arbeitswelt_adj <- Resultdataset$TS_Arbeitswelt_adj*-1
}
if (cor(Resultdataset$Arbeitslosigkeit, Resultdataset$TS_Einkommen_adj,use="pairwise.complete.obs")<0) {
  Resultdataset$TS_Einkommen_adj <- Resultdataset$TS_Einkommen_adj*-1
}

Resultdataset %>% select(Arbeitslosigkeit,TS_Arbeitswelt_adj,TS_Einkommen_adj,TS_Bildung_adj) %>% cor( use="pairwise.complete.obs")

GISD_Komponents

Resultdataset$TS_Arbeitswelt_adj <- (Resultdataset$TS_Arbeitswelt_adj -min(Resultdataset$TS_Arbeitswelt_adj ))/(max(Resultdataset$TS_Arbeitswelt_adj )-min(Resultdataset$TS_Arbeitswelt_adj ))
Resultdataset$TS_Einkommen_adj <- (Resultdataset$TS_Einkommen_adj -min(Resultdataset$TS_Einkommen_adj ))/(max(Resultdataset$TS_Einkommen_adj )-min(Resultdataset$TS_Einkommen_adj ))
Resultdataset$TS_Bildung_adj <- (Resultdataset$TS_Bildung_adj -min(Resultdataset$TS_Bildung_adj ))/(max(Resultdataset$TS_Bildung_adj )-min(Resultdataset$TS_Bildung_adj ))

Resultdataset$GISD_Score <- Resultdataset$TS_Arbeitswelt_adj+Resultdataset$TS_Einkommen_adj+Resultdataset$TS_Bildung_adj
for (i in 1998:2019) {
  Resultdataset <- Resultdataset %>% group_by(Jahr) %>% mutate(GISD_Score = ifelse(Jahr == i,(GISD_Score -min(GISD_Score))/(max(GISD_Score)-min(GISD_Score)), GISD_Score)) %>% ungroup()
}

Resultdataset <- Resultdataset %>% mutate(GISD_Score = round(GISD_Score , digits = 5))

summary(Resultdataset %>% select(TS_Arbeitswelt_adj,TS_Einkommen_adj,TS_Bildung_adj,GISD_Score))
str(Resultdataset %>% select(TS_Arbeitswelt_adj,TS_Einkommen_adj,TS_Bildung_adj,GISD_Score))


## V.  Datenexport - Erstellung der Datensätze 
RawResult <- left_join(Resultdataset,id_dataset,by="Gemeindekennziffer")

RawResult <- RawResult %>% rename(year = Jahr, 
                                  population = Bevölkerung, 
                                  gisd_score = GISD_Score, 
                                  gemeinde_id = Gemeindekennziffer,
                                  gvb_id = GVBKennziffer,
                                  kreis_id = Kreiskennziffer,
                                  ror_id = `Raumordnungsregion Nr`,
                                  nuts_2_id = NUTS2,
                                  gemeinde_name = `Name der Gemeinde`,
                                  gvb_name = `Name des Gemeindeverbands`,
                                  kreis_name = `Name des Kreises`,
                                  ror_name = Raumordnungsregion,
                                  nuts_2_name = `NUTS2 Name`) %>%
  mutate(gemeinde_id = as.character(as.vector(gemeinde_id)),
         gemeinde_id = ifelse(nchar(gemeinde_id)==7,paste0("0",gemeinde_id),gemeinde_id),
         gvb_id = as.character(gvb_id),
         gvb_id = ifelse(nchar(gvb_id)==8,paste0("0",gvb_id),gvb_id),
         kreis_id = as.character(kreis_id),
         kreis_id = ifelse(nchar(kreis_id)==4,paste0("0",kreis_id),kreis_id),
         ror_id = as.character(ror_id),
         ror_id = ifelse(nchar(ror_id)==3,paste0("0",ror_id),ror_id))

exportlist<- NULL
exportlist$Kennziffern <- c("gemeinde_id","kreis_id","gvb_id","ror_id","nuts_2_id")
exportlist$Namen <- c("gemeinde_name","kreis_name","gvb_name","ror_name","nuts_2_name")
exportlist$Label <- c("Gemeinde","Kreis","Gemeindeverband","Raumordnungsregion","NUTS2")

for(mykennziffer in exportlist$Kennziffern) {
  myname <-  exportlist$Namen[exportlist$Kennziffern==mykennziffer]
  mylabel<-  exportlist$Label[exportlist$Kennziffern==mykennziffer]
  print(paste("Level:",myname,"Label:",mylabel))

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
  
  dir.create("Outfiles/", showWarnings=F)
  dir.create("Outfiles/2022_v03/", showWarnings=F)
  dir.create("Outfiles/2022_v03/Bund/", showWarnings=F)
  mydata <- outputdata.agg %>% ungroup() %>% select(mykennziffer, gisd_score, gisd_5, gisd_10, gisd_k, myname, year)
  write.csv(mydata, paste0("Outfiles/2022_v03/Bund/GISD_Bund_",mylabel,".csv"), row.names=FALSE, fileEncoding="UTF-8")
  
  names(mydata) <- gsub("\\.","_",make.names(names(mydata)))
  names(mydata) <- gsub("\\?","oe",names(mydata))
  names(mydata) <- gsub("\\?","ae",names(mydata))
  names(mydata) <- gsub("\\?","ue",names(mydata))
  names(mydata) <- gsub("\\?","ss",names(mydata))
  write_dta(mydata, paste0("Outfiles/2022_v03/Bund/GISD_Bund_",mylabel,"_long.dta"))
  
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
    dir.create("Outfiles/2022_v03/Bundesland", showWarnings=F)  
    for(myland in ListeBula) {
      mydata.bula <- outputdata.bula %>% filter(Bundesland==myland) %>% ungroup() %>% select(gisd_score, mykennziffer, myname, year)
      write.csv(mydata.bula, paste0("Outfiles/2022_v03/Bundesland/GISD_",myland,"_",mylabel,".csv"), row.names=FALSE, fileEncoding="UTF-8")
      
      names(mydata.bula) <- gsub("\\.","_",make.names(names(mydata.bula)))
      names(mydata.bula) <- gsub("\\?","oe",names(mydata.bula))
      names(mydata.bula) <- gsub("\\?","ae",names(mydata.bula))
      names(mydata.bula) <- gsub("\\?","ue",names(mydata.bula))
      names(mydata.bula) <- gsub("\\?","ss",names(mydata.bula))
      write_dta(mydata.bula, paste0("Outfiles/2022_v03/Bundesland/GISD_",myland,"_",mylabel,".dta"))
    }
  }  
}


## VI.  Datensätze für PLZ generieren
load("Data/SHP/GEM_Zipcode_Intersections_2015.RData") # AGS/Postcode-Intersections-Dataset in sf format


for (mykennziffer in c("PLZ2","PLZ3","PLZ4","PLZ5")) {
  myname <-  paste0(mykennziffer)
  mylabel<-  paste0(mykennziffer)
  print(paste("Level:",myname,"Label:",mylabel))
  
  outputdata <- Resultdataset
  
  outputdata <- outputdata %>% rename(gemeinde_id = Gemeindekennziffer, year = Jahr, gisd_score = GISD_Score)
  
  outputdata <- outputdata %>% select(AGS=gemeinde_id,year,gisd_score)
  outputdata <- left_join(as.data.frame(PLZ.df) %>% ungroup() %>% mutate(AGS=as.numeric(as.character(AGS))),
                          outputdata,by=c("AGS"), all.x = TRUE)
  
  outputdata <- outputdata %>% mutate(AGS = as.character(AGS),
                                      AGS = ifelse(nchar(AGS)<8,paste0("0",AGS),AGS))
  
  outputdata <- outputdata %>% filter(!is.na(mykennziffer) & !is.na(EW_Area) & !is.na(year) & EW_Area>0)
  mycol <- which(mykennziffer %in% names(outputdata))
  outputdata <- outputdata %>% group_by(year,AGS) 
  outputdata <- outputdata %>% mutate(gisd_score = weighted.mean(gisd_score,EW_Area))
  outputdata <- outputdata %>% group_by_at(vars("year",mykennziffer)) %>% 
    summarise(gisd_score = weighted.mean(gisd_score,EW_Area), population = sum(EW_Area)) %>%
    group_by(year)
  
  outputdata <- outputdata %>%  mutate(gisd_score = round((gisd_score -min(gisd_score ))/(max(gisd_score )-min(gisd_score)), digits=6),
                                       gisd_5 = findInterval(gisd_score, quantile(gisd_score,   probs=0:5/5 , type=9)),
                                       gisd_5 = findInterval(gisd_5, c(1:5)),
                                       gisd_10 = findInterval(gisd_score, quantile(gisd_score, probs=0:10/10 , type=9)),
                                       gisd_10 = findInterval(gisd_10, c(1:10)),
                                       gisd_k = findInterval(gisd_5, c(1,2,5)))
  summary(outputdata)            
  head(outputdata)
  ListeJahre <- unique(outputdata$year)
  dir.create( paste0("Outfiles"), showWarnings=F)
  dir.create( paste0("Outfiles/2022_v03"), showWarnings=F) 
  dir.create( paste0("Outfiles/2022_v03/Bund/"), showWarnings=F) 
  mydata <- outputdata %>% ungroup() 
  write.csv(mydata, paste0("Outfiles/2022_v03/Bund/GISD_Bund_",mylabel,".csv"), row.names=FALSE, fileEncoding="UTF-8")
  mydata <- outputdata %>% ungroup() 
  names(mydata) <- gsub("\\.","_",make.names(names(mydata)))
  names(mydata) <- gsub("\\?","oe",names(mydata))
  names(mydata) <- gsub("\\?","ae",names(mydata))
  names(mydata) <- gsub("\\?","ue",names(mydata))
  names(mydata) <- gsub("\\?","ss",names(mydata))
  write_dta(mydata, paste0("Outfiles/2022_v03/Bund/GISD_Bund_",mylabel,"_long.dta"))
}