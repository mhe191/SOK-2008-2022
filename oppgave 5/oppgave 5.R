library(tidyverse)
library(httr)
library(ggrepel)
library(janitor)
library(rjstat)

#Oppgave 1
# Henter data fra ssb
url <- "https://data.ssb.no/api/v0/no/table/05185/"

data <- '{"query": [{"code": "Kjonn","selection": {"filter": "item",
"values": ["1","2"]}},{"code": "Landbakgrunn","selection": {
"filter": "agg:Verdensdel2","values": ["b0","b11","b12","b13","b14","b2",
"b3","b4","b5","b6","b8","b9"]}}],"response": {"format": "json-stat2"}}'
tabell1 <- POST(url , body = data, encode = "json", verbose())
tabell1 <- fromJSONstat(content(tabell1, "text"))


tabell1 <- tabell1 %>%
  pivot_wider(names_from = kjønn, values_from = value) %>% 
  arrange(år) %>% 
  mutate("begge_kjønn" = Menn + Kvinner) %>% 
  filter(år > 2004) %>% 
  filter(!landbakgrunn == "Norge")

tabell1$år = (as.numeric(as.character(tabell1$år)))  

tabell1 %>%
  ggplot(aes(år, begge_kjønn, color=landbakgrunn )) +
  geom_line() +
  ggtitle("Utvikling i innvandring til Norge") +
  labs(x="År", y="Aldresgruppe") +
  theme_bw() +
  scale_y_continuous(labels = scales::comma)

#oppgave 2
url2 <- "https://data.ssb.no/api/v0/no/table/13215/"

data2 <- '{"query": [{"code": "Kjonn","selection": {"filter": "item","values": [
"0"]}},{"code": "Alder","selection": {"filter": "item","values": ["15-74"]}},
{"code": "InnvandrKat","selection": {"filter": "item","values": ["B"]}},
{"code": "Landbakgrunn","selection": {"filter": "item","values": ["015a","100c",
"694c","400"]}},{"code": "NACE2007","selection":{"filter": "agg:NACE260InnvGrupp2",
"values": ["SNI-00-99","SNI-01-03","SNI-05-09","SNI-10-33","SNI-35-39","SNI-41-43",
"SNI-45-47","SNI-49-53","SNI-49.3","SNI-55","SNI-56","SNI-58-63","SNI-64-66",
"SNI-68-75","SNI-77-82","SNI-78.2","SNI-81.2","SNI-84","SNI-85","SNI-86-88",
"SNI-90-99","SNI-00"]}},{"code": "Tid","selection": {"filter": "item","values":[
"2021"]}}],"response": {"format": "json-stat2"}}'
tabell2 <- POST(url2 , body = data2, encode = "json", verbose())
tabell2 <- fromJSONstat(content(tabell2, "text"))

tabell2 <- tabell2 %>% 
  clean_names()

tabell2 %>% 
  filter(landbakgrunn == "EU-land i Øst-Europa") %>% 
  filter(!naering_sn2007 == "00-99 ... Alle næringer") %>% 
  ggplot(aes(x=value, y=naering_sn2007)) +
  geom_col() +
  theme_bw() + 
  ggtitle("Hvor jobber invandrer fra Øst-Eurppa") +
  labs(x="Antall", y="Næring")



 
  
