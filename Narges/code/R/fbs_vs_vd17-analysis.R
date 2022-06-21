library(tidyverse)
library(here)
library(ggplot2)
library(gghsci)
library(tidyr)
library(dplyr)
library(DBI)
library(RMariaDB)
library(readxl)
library(patchwork)
library(cowplot)
library(ggpubr)

con <- dbConnect(
  drv = MariaDB(),
  host = "vm2505.kaj.pouta.csc.fi",
  dbname = "vd17",
  user = "vd17",
  password = "vd17",
  bigint = "integer",
  validationInterval = 10,
  load_data_local_infile = TRUE,
  autocommit = TRUE,
  reconnect = TRUE
)
dbExecute(con, "SET default_storage_engine=Aria")

vd17_c <- tbl(con, "vd17_c")

fbs <- read_csv(("C:/Users/azizi/Documents/GitHub/vd17-analysis/Narges/data/fbs_record_number.csv"), guess_max = Inf, lazy = TRUE)


df1 <- vd17_c %>%
  filter(field_code=="011@",subfield_code=="a") %>%
  filter (value != "16XX")%>%
  mutate(year=as.integer(value[!is.na(value)])) %>%
  filter(year>=1617,year<=1682) %>%
  collect()%>%
  select(record_number,year)%>%
  left_join(vd17_c %>%
              filter(field_code=="044S",subfield_code=="a") %>%
              collect() %>%
              mutate(genre=str_replace_all(value[!is.na(value)],":.*","")) %>%
              select(record_number,genre),
            by=c("record_number"))%>%
  left_join(vd17_c %>%
              filter(field_code=="010@",subfield_code=="a") %>%
              collect() %>%
              mutate(pub_lan=value[!is.na(value)]) %>%
              select(record_number,pub_lan),
            by=c("record_number"))

all_fbs <- merge(df1,fbs,by="record_number")
all_fbs <- all_fbs[!duplicated(all_fbs), ]
all_fbs <- select (all_fbs, -5)

translated <- vd17_c %>%
  filter(field_code=="010@",subfield_code=="c") %>%
  collect()%>%
  mutate(original_lan=value[!is.na(value)]) %>%
  select(record_number,original_lan)

translated_fbs <- merge(translated,all_fbs,by="record_number")
translated_fbs <- translated_fbs[!duplicated(translated_fbs), ]

nontranslated_fbs <- all_fbs[!(all_fbs$record_number %in% translated_fbs$record_number),]
  

#Top 5 popular genres per each phase for translated and nontranslated publications

t1_fbs <- translated_fbs%>%
  select(genre,year)%>%
  filter(year>=1617&year<=1650)%>%
  na.omit(year)%>%
  na.omit(genre)%>%
  group_by(year)%>%
  filter(!duplicated('genre','year'))%>%
  count(genre)%>%
  arrange(desc(n))%>%
  slice(1:5)


t2_fbs <- translated_fbs%>%
  select(genre,year)%>%
  filter(year>=1651&year<=1667)%>%
  na.omit(year)%>%
  na.omit(genre)%>%
  group_by(year)%>%
  filter(!duplicated('genre','year'))%>%
  count(genre)%>%
  arrange(desc(n))%>%
  slice(1:5)

t3_fbs <- translated_fbs%>%
  select(genre,year)%>%
  filter(year>=1668&year<=1682)%>%
  na.omit(year)%>%
  na.omit(genre)%>%
  group_by(year)%>%
  filter(!duplicated('genre','year'))%>%
  count(genre)%>%
  arrange(desc(n))%>%
  slice(1:5)

ggplot(NULL, aes(x=year, fill=genre)) + 
  geom_bar(data=t1_fbs)+
  geom_bar(data=t2_fbs)+
  geom_bar(data=t3_fbs)+
  labs(y = "Top 5 frequent genres for translated publications", x= "Three phases of fbs", lintype = "Legend")+
  scale_x_continuous(breaks = seq(1000, 2000, by = 5))+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))


nt1_fbs <- nontranslated_fbs%>%
  select(genre,year)%>%
  filter(year>=1617&year<=1650)%>%
  na.omit(year)%>%
  na.omit(genre)%>%
  group_by(year)%>%
  filter(!duplicated('genre','year'))%>%
  count(genre)%>%
  arrange(desc(n))%>%
  slice(1:5)


nt2_fbs <- nontranslated_fbs%>%
  select(genre,year)%>%
  filter(year>=1651&year<=1667)%>%
  na.omit(year)%>%
  na.omit(genre)%>%
  group_by(year)%>%
  filter(!duplicated('genre','year'))%>%
  count(genre)%>%
  arrange(desc(n))%>%
  slice(1:5)

nt3_fbs <- nontranslated_fbs%>%
  select(genre,year)%>%
  filter(year>=1668&year<=1682)%>%
  na.omit(year)%>%
  na.omit(genre)%>%
  group_by(year)%>%
  filter(!duplicated('genre','year'))%>%
  count(genre)%>%
  arrange(desc(n))%>%
  slice(1:5)

ggplot(NULL, aes(x=year, fill=genre)) + 
  geom_bar(data=nt1_fbs)+
  geom_bar(data=nt2_fbs)+
  geom_bar(data=nt3_fbs)+
  labs(y = "Top 5 frequent genres for non-translated publications", x= "Three phases of fbs", lintype = "Legend")+
  scale_x_continuous(breaks = seq(1000, 2000, by = 5))+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

