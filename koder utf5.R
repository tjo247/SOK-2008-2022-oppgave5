library(PxWebApiData)
library(tidyverse)
library(ggplot2)
library(readr)
library(hrbrthemes)
library(dplyr)
library(tidyr)
library(viridis)



innvandring <- ApiData("https://data.ssb.no/api/v0/no/table/05185/", 
                       Landbakgrunn=list('agg:Verdensdel2', c("b11", "b12", "b13", "b14", "b2", "b3", "b4", "b5", "b6")), 
                       Tid=list('item', c("2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022")), 
                       Kjonn=FALSE, 
                       ContentsCode=TRUE)

innvandring <- as.tibble(innvandring[[1]])

innvandringwide <- innvandring %>%
  pivot_wider(names_from = år, values_from = value)

### Utfordring 5.1.1


innvandring %>% 
  ggplot(aes(x=år, y=value, fill=landbakgrunn)) +
  geom_col() +
  theme(axis.text.x = element_text(angle=60, vjust = 0.6)) +
  labs(title="Innvandring til Norge etter landbakgrunn",
       subtitle ="Figur 1" ,
       x ="År", y = "Personer")


innvandring_anndel <- innvandring %>%
  select(landbakgrunn,år,value) %>%
  group_by(år) %>%
  mutate(sum = sum(value),
         anndel = value/sum,
         anndel = anndel*100)

innvandring_anndel %>% 
  ggplot(aes(x=år, y=anndel, fill=landbakgrunn)) +
  geom_col() +
  theme(axis.text.x = element_text(angle=60, vjust = 0.6)) +
  labs(title="Innvandring til Norge etter landbakgrunn ",
       subtitle ="Figur 2" ,
       x ="År", y = "Prosent Andel")


### Utfordring 5.1.2 

sysselsetting_innvandring <- ApiData("https://data.ssb.no/api/v0/no/table/13215/", 
                                     Alder=list('item', c("15-74")), 
                                     InnvandrKat=list('item', c("B")), 
                                     Landbakgrunn=list('item', c("015a")), 
                                     NACE2007=list('agg:NACE260InnvGrupp2', c("SNI-00-99", "SNI-01-03", "SNI-05-09", "SNI-10-33", "SNI-35-39", "SNI-41-43", "SNI-45-47", "SNI-49-53", "SNI-49.3", "SNI-55", "SNI-56", "SNI-58-63", "SNI-64-66", "SNI-68-75", "SNI-77-82", "SNI-78.2", "SNI-81.2", "SNI-84", "SNI-85", "SNI-86-88", "SNI-90-99", "SNI-00")), 
                                     Tid=list('item', c("2021")), 
                                     Kjonn=FALSE, 
                                     ContentsCode=TRUE)

sysselsetting_innvandring <- as.tibble(sysselsetting_innvandring[[1]])



sysselsetting_innvandring$`næring (SN2007)` <- gsub(".*\\.", "", sysselsetting_innvandring$`næring (SN2007)`)


ggplot(sysselsetting_innvandring[-1,], aes(x = value, y =`næring (SN2007)`, fill = `næring (SN2007)`)) +
  geom_col(show.legend = FALSE) +
  labs(x = "",
       y = "",
       title = "Næringslivssektor blant innvandrere fra EU-land i Øst-Europa",
       subtitle ="Figur 3")

