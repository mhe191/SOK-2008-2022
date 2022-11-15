library(tidyverse)
library(httr)
library(ggrepel)
library(janitor)
library(rjstat)
library(patchwork)
#1
url <- "https://data.ssb.no/api/v0/no/table/05111/"

data <- '{"query": [{"code": "ArbStyrkStatus",
"selection": {"filter": "item","values": ["99","0","1","2","6"]}},{
"code": "Kjonn","selection": {"filter": "item","values": ["1","2"]}},{
"code": "Alder","selection": {"filter": "item","values": ["15-74","20-64",
"20-66","15-24","25-54","55-74"]}}],"response": {"format": "json-stat2"}}'
tabell1 <- POST(url , body = data, encode = "json", verbose())
tabell1 <- fromJSONstat(content(tabell1, "text"))


#2
url2 <- "https://data.ssb.no/api/v0/no/table/12441/"

data2 <- '{"query": [{"code": "Kjonn","selection": {
"filter": "item","values": ["1","2"]}},{"code": "NACE2007","selection": {
"filter": "item","values": ["00-99"]}},{"code": "Sykefraver2","selection": {
"filter": "item","values": ["Alt"]}}],"response": {"format": "json-stat2"}}'
tabell2 <- POST(url2 , body = data2, encode = "json", verbose())
tabell2 <- fromJSONstat(content(tabell2, "text"))


tabell1 <- tabell1 %>% 
  filter(alder=="15-74 år") %>% 
  filter(år > 2004) %>% 
  filter(år < 2020) %>% 
  filter(statistikkvariabel == "Personer (prosent)") %>% 
  filter(arbeidsstyrkestatus == "Arbeidsledige") %>% 
  mutate("arbeidsledige" = value)

tabell2 <- tabell2 %>% 
  filter(år > 2004) %>% 
  filter(år < 2020) %>% 
  mutate("sykefravær" = value)

tabell3 <- merge(tabell1, tabell2, by = c("kjønn","år"))

tabell3$år = (as.numeric(as.character(tabell3$år)))



cof <- 2
plt1 <- tabell3 %>% 
  filter(kjønn == "Kvinner") %>% 
  ggplot(aes(x=år, y=arbeidsledige)) +
  geom_line(color = "blue") +
  geom_line(aes(y = sykefravær/cof), color = "red") +
  scale_y_continuous("Arbeidsledighet %", sec.axis = sec_axis(~.*cof, name = "Sykefravær %")) +
  scale_x_continuous("Year") +
  theme(axis.line.y.right = element_line(color = "red"), 
        axis.ticks.y.right = element_line(color = "red"),
        axis.text.y.right = element_text(color = "red"), 
        axis.title.y.right = element_text(color = "red")) +
  theme(axis.line.y.left = element_line(color = "blue"), 
        axis.ticks.y.left = element_line(color = "blue"),
        axis.text.y.left = element_text(color = "blue"), 
        axis.title.y.left = element_text(color = "blue")) +
  ggtitle("Arbeidsledighet og sykefravær for kvinner")  

cof2 <- 1
plt2 <-tabell3 %>% 
  filter(kjønn == "Menn") %>% 
  ggplot(aes(x=år, y=arbeidsledige)) +
  geom_line(color = "blue") +
  geom_line(aes(y = sykefravær/cof2), color = "red") +
  scale_y_continuous("Arbeidsledighet %", sec.axis = sec_axis(~.*cof2, name = "Sykefravær %")) +
  scale_x_continuous("Year") +
  theme(axis.line.y.right = element_line(color = "red"), 
        axis.ticks.y.right = element_line(color = "red"),
        axis.text.y.right = element_text(color = "red"), 
        axis.title.y.right = element_text(color = "red")) +
  theme(axis.line.y.left = element_line(color = "blue"), 
        axis.ticks.y.left = element_line(color = "blue"),
        axis.text.y.left = element_text(color = "blue"), 
        axis.title.y.left = element_text(color = "blue")) +
  ggtitle("Arbeidsledighet og sykefravær for menn") 

plt1 + plt2
