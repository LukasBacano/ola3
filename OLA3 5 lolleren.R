library(httr)
library("jsonlite")
#install.packages("rmarkdown")
library(rmarkdown)


#################################### 5.2
#Vedhæft et link til den side hvor de forskellige observationer er beskrevet.
https://www.dmi.dk/friedata/guides-til-frie-data/sadan-males-data 
+
https://opendatadocs.dmi.govcloud.dk/en/Data/Meteorological_Observation_Data

beskrivURL <- "https://dmigw.govcloud.dk/v2/metObs/collections/observation/items"
besVis <- GET(beskrivURL, add_headers("X-Gravitee-Api-Key" = "be7212cb-5180-4d3d-8f8a-698eb1375546"))       









#Hvilken enhed angiver man ”visibility” i? - https://dmigw.govcloud.dk/v2/metObs/collections/observation/items
URLvis <- "https://dmigw.govcloud.dk/v2/metObs/collections/observation/items"
ResVis <- GET(URLvis, add_headers("X-Gravitee-Api-Key" = "be7212cb-5180-4d3d-8f8a-698eb1375546"))
print(status_code(ResVis))
print( ResVis[["content"]])
Visibility <- fromJSON(content(ResVis, "text", encoding = "UTF-8"))
#Visibility2 <- httr::properties(Visibility, as = "text")
Visibility2 <- Visibility$features$properties









#Sigtbarhed er et mål for den maksimale afstand, som man kan se en mørk genstand mod horisonten.
#DMI benytter en Present Weather Sensor (PWS) til at bestemme sigtbarhed. Automatmålere som denne kan måle sigtbarhed
#op til 50 km og melder om aktuelt vejr, så som tåge, finregn, slud, sne.
#Sigtbarhed måles i 2 meters højde over jordoverfladen og angives i hele meter, m.
https://www.dmi.dk/friedata/guides-til-frie-data/sadan-males-data









#Hvor ofte opdateres ”wind_max”?
                                       wind_max <- Visibility2[365,]
        #hver time, eller i nogle tilfælde^

                                       
                                       
API <- "be7212cb-5180-4d3d-8f8a-698eb1375546"
#Hvilken by gemmer sig bag stationen med id’et 05272

AlleStationer <- "https://dmigw.govcloud.dk/v2/metObs/collections/station/items?datetime=2024-11-01T00%3A00%3A00Z"
AlleStationerli <- GET(AlleStationer, add_headers("X-Gravitee-Api-Key" = "be7212cb-5180-4d3d-8f8a-698eb1375546"))
print(status_code(AlleStationerli)) #status check - alt andet end 200 fungerer ikke.
AlleStationerJSON <- fromJSON(content(AlleStationerli, "text", encoding = "UTF-8"))
STATIONERdf<- AlleStationerJSON$features$properties

#Brande


#################################### 5.3
#DMI har også et andet API, hvor man kan trække en vejrudsigt. Angiv linket til API’ets ”endpoint”.
forecastdataAPI = "f98ba9e1-49b7-442e-9827-8dafecba8844"

FCD <- "https://dmigw.govcloud.dk/v1/forecastdata/collections"
FCDres <- GET(FCD, add_headers("X-Gravitee-Api-Key" = forecastdataAPI))
FCDresJSON <- fromJSON(content(FCDres, "text"))
print(FCDresJSON$collections)
FCDdf <- FCDresJSON$collections






#Hvor mange ”collections” kan man spørge til?
max er 15







#Hvilket ID har Lille Bælt?
lillebælt <- FCDdf[5,]

lillebælturl <- "https://dmigw.govcloud.dk/v1/forecastdata/collections/dkss_lb"
lillebæltliste <- GET(lillebælturl, add_headers("X-Gravitee-Api-Key" = "f98ba9e1-49b7-442e-9827-8dafecba8844"))
lillebæltJSON <- fromJSON(content(lillebæltliste, "text"))

lillebælturlitems <- "https://dmigw.govcloud.dk/v1/forecastdata/collections/dkss_lb/items"
lillebæltlisteitems <- GET(lillebælturlitems, add_headers("X-Gravitee-Api-Key" = forecastdataAPI))
lillebæltJSONitems <- fromJSON(content(lillebæltlisteitems, "text"))
lillebæltJSONdf <- lillebæltJSONitems$features


lillebæltcont <- httr::content(lillebæltlisteitems, as = "text")
LILBcontlist <- fromJSON(content(lillebæltlisteitems, "text"))
lillebæltdf <- LILBcontlist$features

gribfil <- "https://dmigw.govcloud.dk/v1/forecastdata/download/DKSS_LB_SF_2024-10-29T120000Z_2024-10-29T120000Z.grib"
gribDL <- GET(gribfil,, add_headers("X-Gravitee-Api-Key" = "f98ba9e1-49b7-442e-9827-8dafecba8844"))
writeBin(content(gribDL, "raw"), "data.grib")

#install.packages("rNOMADS")
library(rNOMADS)

# Path to your downloaded GRIB file
grib_file <- "/Users/lukasbachcouzy/data.grib"
#grib_data <- ReadGrib(file.names = grib_file, levels = NULL, variables = NULL)
print(grib_file)

# Use NCEP.Model.Grid function to extract data from the GRIB file
# This function needs specific variable names and levels that match the GRIB file structure
# Refer to the documentation to select parameters correctly
grib_data <- ReadGrib(grib_file, levels = NULL, variables = NULL, domain = NULL)

# Explore the data
print(grib_data)

#Hvilket filformat enderman med at få ”forecasten” i?
.grib format

##############################5.4

Opgave 5.4 – En graf med mangler
Her er en graf over vindobservationer fra to stationer - Århus og Anholt - fra stormen i november ´22.
1. find stationerne århus og anholt
vindurl <- "https://dmigw.govcloud.dk/v2/metObs/collections/station/items?datetime=2018-02-12T00%3A00%3A00Z%2F2018-03-18T12%3A31%3A12Z&bbox-crs=https%3A%2F%2Fwww.opengis.net%2Fdef%2Fcrs%2FOGC%2F1.3%2FCRS84"
vindres <- GET(vindurl, add_headers("X-Gravitee-Api-Key" = "be7212cb-5180-4d3d-8f8a-698eb1375546"))
print(status_code(vindres))
vindJSON <- fromJSON(content(vindres, "text", encoding = "UTF-8"))

vinddf<- vindJSON$features$properties #omkring linje 140
århusID <- vinddf[180,c(4,11)]
anholtID <- vinddf[98,c(4,11)]

2.definer tidsperioden for stormen
14nov-21nov 22
vindurl <- "https://dmigw.govcloud.dk/v2/metObs/collections/station/items?datetime=2018-02-12T00%3A00%3A00Z%2F2018-03-18T12%3A31%3A12Z&bbox-crs=https%3A%2F%2Fwww.opengis.net%2Fdef%2Fcrs%2FOGC%2F1.3%2FCRS84"
#  stormen22URL <- "https://dmigw.govcloud.dk/v2/metObs/collections/observation/items?datetime=2018-02-12T00%3A00%3A00Z%2F2018-03-18T12%3A31%3A12Z&stationId=06079%2C%2006074&parameterId=wind_dir_past1h&bbox-crs=https%3A%2F%2Fwww.opengis.net%2Fdef%2Fcrs%2FOGC%2F1.3%2FCRS84"
# DEN HER SPILLER MEN ER FOR FORKERT DATO
#stormen22URL <- "https://dmigw.govcloud.dk/v2/metObs/collections/observation/items?datetime=2018-02-12T00%3A00%3A00Z%2F2018-03-18T12%3A31%3A12Z&stationId=06079&parameterId=wind_dir_past1h&bbox-crs=https%3A%2F%2Fwww.opengis.net%2Fdef%2Fcrs%2FOGC%2F1.3%2FCRS84"
stormen22URL <- "https://dmigw.govcloud.dk/v2/metObs/collections/observation/items?datetime=2022-11-14T00%3A00%3A00Z%2F2022-11-21T12%3A31%3A12Z&stationId=06079&parameterId=wind_dir_past1h&bbox-crs=https%3A%2F%2Fwww.opengis.net%2Fdef%2Fcrs%2FOGC%2F1.3%2FCRS84"

storm22res <- GET(stormen22URL,add_headers("X-Gravitee-Api-Key" = "be7212cb-5180-4d3d-8f8a-698eb1375546"))
print(status_code(storm22res))
storm22JSON <- fromJSON(content(storm22res, "text", encoding = "UTF-8"))
print(storm22JSON)
anholtvinddir <- as.data.frame(storm22JSON$features$properties)


#######BREAKPOInTS for vindretning i degrees
anholtvinddir$vindretning <- with(anholtvinddir,
                          ifelse(value <= 0, "Vindstille",
                                 ifelse(value > 0 & value <= 23, "N",
                                        ifelse(value > 23 & value <= 68, "NØ",
                                               ifelse(value > 68 & value <= 113, "Ø",
                                                      ifelse(value > 113 & value <= 158, "SØ",
                                                             ifelse(value > 158 & value <= 203, "S",
                                                                   ifelse(value > 203 & value <= 258, "SV",
                                                                        ifelse(value > 258 & value <= 293, "V",
                                                                             ifelse(value > 293 & value <= 338, "NV",
                                                                                  ifelse(value > 338 & value <= 360, "N", "så der fejl i koden")))))))))))
                                                                               







stormen22århusURL <- "https://dmigw.govcloud.dk/v2/metObs/collections/observation/items?datetime=2022-11-14T00%3A00%3A00Z%2F2022-11-21T12%3A31%3A12Z&stationId=06074&parameterId=wind_dir_past1h&bbox-crs=https%3A%2F%2Fwww.opengis.net%2Fdef%2Fcrs%2FOGC%2F1.3%2FCRS84"

storm22århusres <- GET(stormen22århusURL,add_headers("X-Gravitee-Api-Key" = "be7212cb-5180-4d3d-8f8a-698eb1375546"))
print(status_code(storm22århusres))
storm22århusJSON <- fromJSON(content(storm22århusres, "text", encoding = "UTF-8"))
print(storm22århusJSON)
århusvinddir <- as.data.frame(storm22århusJSON$features$properties)


århusvinddir$vindretning <- with(århusvinddir,
                                  ifelse(value <= 0, "Vindstille",
                                         ifelse(value > 0 & value <= 23, "N",
                                                ifelse(value > 23 & value <= 68, "NØ",
                                                       ifelse(value > 68 & value <= 113, "Ø",
                                                              ifelse(value > 113 & value <= 158, "SØ",
                                                                     ifelse(value > 158 & value <= 203, "S",
                                                                            ifelse(value > 203 & value <= 258, "SV",
                                                                                   ifelse(value > 258 & value <= 293, "V",
                                                                                          ifelse(value > 293 & value <= 338, "NV",
                                                                                                 ifelse(value > 338 & value <= 360, "N", "så der fejl i koden")))))))))))

colnames(århusvinddir)[colnames(århusvinddir) == "vindretning"] <- "Århus.vindretning"

colnames(anholtvinddir)[colnames(anholtvinddir) == "vindretning"] <- "Anholt.vindretning"
VINDdirDF <- århusvinddir$observed
VINDdirDF <- as.data.frame(VINDdirDF)
VINDdirDF$anholttid <- anholtvinddir$observed
VINDdirDF$Århus.vindretning <- århusvinddir$Århus.vindretning
VINDdirDF$Anholt.vindretning <- anholtvinddir$Anholt.vindretning

stormen19_11_2022 <- VINDdirDF[38:61,]



#Lav samme graf (gerne meget bedre) for stormen, der i oktober 2023 hærgede Danmark.

#samle data 1 find winddir fra århus - 06074
#breakpoints scrpit
år23Sdir <- "https://dmigw.govcloud.dk/v2/metObs/collections/observation/items?datetime=2023-10-16T00%3A00%3A00Z%2F2023-10-23T12%3A31%3A12Z&stationId=06074&parameterId=wind_dir_past1h"
storm23dirres <- GET(år23Sdir,add_headers("X-Gravitee-Api-Key" = "be7212cb-5180-4d3d-8f8a-698eb1375546"))
print(status_code(storm23dirres))
år23dirjSON <- fromJSON(content(storm23dirres, "text", encoding = "UTF-8"))
århusvinddir23 <- as.data.frame(år23dirjSON$features$properties)

århusvinddir23$århusvindretning <- with(århusvinddir23,
                                 ifelse(value <= 0, "Vindstille",
                                        ifelse(value > 0 & value <= 23, "N",
                                               ifelse(value > 23 & value <= 68, "NØ",
                                                      ifelse(value > 68 & value <= 113, "Ø",
                                                             ifelse(value > 113 & value <= 158, "SØ",
                                                                    ifelse(value > 158 & value <= 203, "S",
                                                                           ifelse(value > 203 & value <= 258, "SV",
                                                                                  ifelse(value > 258 & value <= 293, "V",
                                                                                         ifelse(value > 293 & value <= 338, "NV",
                                                                                                ifelse(value > 338 & value <= 360, "N", "så der fejl i koden")))))))))))

århusspeed23 <- "https://dmigw.govcloud.dk/v2/metObs/collections/observation/items?datetime=2023-10-16T00%3A00%3A00Z%2F2023-10-23T12%3A31%3A12Z&stationId=06074&parameterId=wind_speed_past1h"
århusspeedwind <- GET(århusspeed23,add_headers("X-Gravitee-Api-Key" = "be7212cb-5180-4d3d-8f8a-698eb1375546"))
print(status_code(århusspeedwind))
årspeedwind23 <- fromJSON(content(århusspeedwind, "text", encoding = "UTF-8"))
print(storm23dirres)
århuswindspeed23 <- as.data.frame(årspeedwind23$features$properties)





#så fra Anholt
#breakpoints scrpit
AN23Sdir <- "https://dmigw.govcloud.dk/v2/metObs/collections/observation/items?datetime=2023-10-16T00%3A00%3A00Z%2F2023-10-23T12%3A31%3A12Z&stationId=06079&parameterId=wind_dir_past1h&bbox-crs=https%3A%2F%2Fwww.opengis.net%2Fdef%2Fcrs%2FOGC%2F1.3%2FCRS84"
ANH23dirres <- GET(AN23Sdir,add_headers("X-Gravitee-Api-Key" = "be7212cb-5180-4d3d-8f8a-698eb1375546"))
print(status_code(ANH23dirres))
Ar23dirjSON <- fromJSON(content(ANH23dirres, "text", encoding = "UTF-8"))
print(Ar23dirjSON)
anholtvinddir23 <- as.data.frame(Ar23dirjSON$features$properties)

anholtvinddir23$Anholtvindretning <- with(anholtvinddir23,
                                        ifelse(value <= 0, "Vindstille",
                                               ifelse(value > 0 & value <= 23, "N",
                                                      ifelse(value > 23 & value <= 68, "NØ",
                                                             ifelse(value > 68 & value <= 113, "Ø",
                                                                    ifelse(value > 113 & value <= 158, "SØ",
                                                                           ifelse(value > 158 & value <= 203, "S",
                                                                                  ifelse(value > 203 & value <= 258, "SV",
                                                                                         ifelse(value > 258 & value <= 293, "V",
                                                                                                ifelse(value > 293 & value <= 338, "NV",
                                                                                                       ifelse(value > 338 & value <= 360, "N", "så der fejl i koden")))))))))))

Anholtspeed23 <- "https://dmigw.govcloud.dk/v2/metObs/collections/observation/items?datetime=2023-10-16T00%3A00%3A00Z%2F2023-10-23T12%3A31%3A12Z&stationId=06079&parameterId=wind_speed_past1h&bbox-crs=https%3A%2F%2Fwww.opengis.net%2Fdef%2Fcrs%2FOGC%2F1.3%2FCRS84"
Anholtspeedwind <- GET(Anholtspeed23,add_headers("X-Gravitee-Api-Key" = "be7212cb-5180-4d3d-8f8a-698eb1375546"))
print(status_code(Anholtspeedwind))
Anspeedwind23 <- fromJSON(content(Anholtspeedwind, "text", encoding = "UTF-8"))
print(storm23dirres)
ANHOLTwindspeed23 <- as.data.frame(Anspeedwind23$features$properties)



#samling af første data som vi skal bruge (vindretning)
StormenOKT23 <- as.data.frame(anholtvinddir23$observed)
StormenOKT23$ÅrhusVindRetning <- århusvinddir23$århusvindretning
StormenOKT23$AnholtVindRetning <- anholtvinddir23$Anholtvindretning
StormenOKT23$ÅrhusWindSpeed <- århuswindspeed23$value
StormenOKT23$AnholtWindSpeed <- ANHOLTwindspeed23$value
#Clean datoerne
colnames(StormenOKT23)[colnames(StormenOKT23) == "anholtvinddir23$observed"] <- "date"
StormenOKT23$date <- gsub(":00:00Z", "", StormenOKT23$date)
StormenOKT23$date <- gsub("T", ":", StormenOKT23$date)




library(ggplot2)
#Nu plottes der
# Konverter 'date' kolonnen til POSIXct format
StormenOKT23$date <- as.POSIXct(StormenOKT23$date, format = "%Y-%m-%d:%H", tz = "CET")

# Fjern rækker med NA værdier i enten wind speed kolonner eller date
StormenOKT23 <- StormenOKT23[!is.na(StormenOKT23$date) & !is.na(StormenOKT23$ÅrhusWindSpeed) & !is.na(StormenOKT23$AnholtWindSpeed), ]

# Opret plot i ggplot2
ggplot(data = StormenOKT23, aes(x = date)) +
  geom_line(aes(y = ÅrhusWindSpeed, color = "Århus")) +
  geom_line(aes(y = AnholtWindSpeed, color = "Anholt")) +
  geom_text(aes(y = ÅrhusWindSpeed, label = ÅrhusVindRetning), color = "blue", vjust = -1, size = 3) +
  geom_text(aes(y = AnholtWindSpeed, label = AnholtVindRetning), color = "red", vjust = -1, size = 3) +
  labs(x = "Oktober 2023", y = "Vindhastighed (m/s)", title = "Anholt er mindre beskyttet mod blæst end Århus") +
  scale_color_manual(name = "Vindhastigheder", values = c("Århus" = "blue", "Anholt" = "red")) +
  theme_minimal()









cosinus for at udregne vinkel på på efter degree i vinddir


5.5

https://www.dmi.dk/friedata/guides-til-frie-data/sadan-bruger-andre-data
Woodsense <- "https://www.dmi.dk/friedata/guides-til-frie-data/sadan-bruger-andre-data/saadan-bruger-woodsense-dmis-frie-data-til-at-goere-byggeri-mere-b"
