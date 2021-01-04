library(sf)
library(tmap)
library(dplyr)

# settings
tmap_mode("plot")
date_of_publication = "2020-12-23"

# create directories
directories <- list("downloads", "shapefiles", "data", "output")
for (directory in directories) {
  if (!dir.exists(file.path(directory))) {
    dir.create(file.path(directory))
  }
}

# download shapefiles
url = "https://www.cbs.nl/-/media/cbs/dossiers/nederland-regionaal/wijk-en-buurtstatistieken/wijkbuurtkaart_2020_v1.zip"
filename_zip <- sapply(strsplit(url, "/"), tail, 1)
filepath_zip <- file.path("downloads", filename_zip)
if (!file.exists(filepath_zip)){
  download.file(url, filepath_zip)
}

# extract shapefiles
filename_shp <- "gemeente_2020_v1.dbf"
filepath_shp <- file.path("shapefiles", filename_shp)
if (!file.exists(filepath_shp)){
  unzip(filepath_zip, exdir=file.path("shapefiles"))
}

# read shapefile
municipality <- st_read(filepath_shp) %>%
  filter(H2O == "NEE") %>%
  mutate(GROOTTE_HH = AANT_INW / AANTAL_HH) %>%
  select(GM_CODE, GM_NAAM, BEV_DICHTH, AANT_INW, AANTAL_HH, GROOTTE_HH, geometry)

# download covid data
url <- "https://data.rivm.nl/covid-19/COVID-19_aantallen_gemeente_per_dag.csv"
filename_covid <- sapply(strsplit(url, "/"), tail, 1)
filepath_covid <- file.path("data", filename_covid)
download.file(url, filepath_covid)

# read covid data
covid <- read.csv2(filepath_covid) %>%
  filter(Date_of_publication==date_of_publication,
         Municipality_code!=""
        ) %>%
  select(Municipality_code, Total_reported) %>%
  group_by(Municipality_code) %>%
  summarise(Total_reported = sum(Total_reported))

# download verkiezingsuitslagen 2018.03.21
url <- "https://www.verkiezingsuitslagen.nl/data/CsvZetels/10895"
filename_elec_20180321 <- "Zetelverdeling_alle_gemeenten_GR20180321.csv"
filepath_elec_20180321 <- file.path("data", filename_elec_20180321)
if (!file.exists(filepath_elec_20180321)){
  download.file(url, filepath_elec_20180321)
}

# download verkiezingsuitslagen 2018.11.21
url <- "https://www.verkiezingsuitslagen.nl/data/CsvZetels/10901"
filename_elec_20181121 <- "Zetelverdeling_alle_gemeenten_GR20181121.csv"
filepath_elec_20181121 <- file.path("data", filename_elec_20181121)
if (!file.exists(filepath_elec_20181121)){
  download.file(url, filepath_elec_20181121)
}

# read verkiezingsuitslagen: aantal zetels
elec_chairs <- rbind(read.csv2(filepath_elec_20180321), read.csv2(filepath_elec_20181121)) %>%
  select(RegioNaam, AantalZetels) %>%
  group_by(RegioNaam) %>%
  summarise(AantalZetels = sum(AantalZetels, na.rm=TRUE))

# read verkiezingsuitslagen: aantal zetels per gemeente
elec_chr <- rbind(read.csv2(filepath_elec_20180321),
                  read.csv2(filepath_elec_20181121)
                  ) %>%
  filter(Partij == "ChristenUnie" |
         Partij == "ChristenUnie - SGP" |
         Partij == "ChristenUnie-SGP" |
         Partij == "ChristenUnie/SGP" |
         Partij == "SGP-ChristenUnie" |
         Partij == "Staatkundig Gereformeerde Partij (SGP)"
         ) %>%
  select(RegioNaam, AantalZetels) %>%
  group_by(RegioNaam) %>%
  summarise(AantalZetelsChr = sum(AantalZetels, na.rm=TRUE))

# merge verkiezingsuitslagen
elec <- merge(elec_chairs, elec_chr, by="RegioNaam") %>%
  mutate(Aandeel_SGP_CU = AantalZetelsChr / AantalZetels)

munic_covid <- merge(municipality, covid, by.x="GM_CODE", by.y="Municipality_code") %>%
  mutate(Total_reported_100K = Total_reported / AANT_INW * 100000) %>%
  select(GM_CODE, GM_NAAM, GROOTTE_HH, Total_reported_100K, geometry)

# final dataset
df <- merge(munic_covid, elec, by.x="GM_NAAM", by.y="RegioNaam", all.x=TRUE)

# visualize data
figuur_1 <- tm_shape(df) +
  tm_polygons(c("Total_reported_100K", "Aandeel_SGP_CU")) +
  tm_facets(sync = TRUE, ncol = 2)

# save figure
tmap_save(figuur_1, filename = file.path("output", "figuur_1.png"))

# visualize data
figuur_2 <- tm_shape(df) +
  tm_polygons(c("Total_reported_100K", "GROOTTE_HH")) +
  tm_facets(sync = TRUE, ncol = 2)

# save figure
tmap_save(figuur_2, filename = file.path("output", "figuur_2.png"))
