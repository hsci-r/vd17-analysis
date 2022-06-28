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
              filter(pub_lan=="ger")%>%
              select(record_number,pub_lan),
            by=c("record_number"))

all_fbs <- merge(df1,fbs,by="record_number")
all_fbs <- all_fbs[!duplicated(all_fbs), ]
all_fbs <- select (all_fbs, -5)

translated <- vd17_c %>%
  filter(field_code=="010@",subfield_code=="c") %>%
  collect()%>%
  mutate(original_language=value[!is.na(value)]) %>%
  select(record_number,original_language)

translated_fbs <- merge(translated,all_fbs,by="record_number")
translated_fbs <- translated_fbs[!duplicated(translated_fbs), ]

all_fbs$type <- all_fbs$record_number %in% c(translated_fbs$record_number)


intermediary <- vd17_c %>%
  filter(field_code=="010@",subfield_code=="b") %>%
  collect()%>%
  mutate(intermediary_lan=value[!is.na(value)]) %>%
  select(record_number,intermediary_lan)

intermediary_fbs <- merge(intermediary,all_fbs,by="record_number")
intermediary_fbs <- intermediary_fbs[!duplicated(intermediary_fbs), ] 

#intermediary_fbs is zero so in fbs there is no intermediary language

#number of translated and nontranslated publication in fbs

all_fbs$type[all_fbs$type == "FALSE"] <- "non_translated"
all_fbs$type[all_fbs$type == "TRUE"] <- "translated"

t_fbs <- all_fbs%>%
  filter(!duplicated('record_number','year'))%>%
  select(year,type)%>%
  group_by(year)%>%
  count(type)%>%
  arrange(desc(n))

t_fbs%>%
  ggplot(aes(x=year,y=n, fill=type)) + 
  geom_col()+
  labs(y = "Amount of translated and non-translated publications", x= "Years", lintype = "Legend")+
  scale_x_continuous(breaks = seq(1000, 2000, by = 2))+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

#original language of german-translated publications

o_fbs <- translated_fbs%>%
  na.omit(original_language)%>%
  na.omit(pub_lan)%>%
  filter(original_language!="ger")%>%
  filter(!duplicated('record_number','year'))%>%
  select(year,original_language)%>%
  group_by(year)

o_fbs%>%
  ggplot(aes(x=year, fill=original_language)) + 
  geom_bar()+
  labs(y = "Original language of german-translated publications", x= "Years", lintype = "Legend")+
  scale_x_continuous(breaks = seq(1000, 2000, by = 2))+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))


#genre of german-translated publications

g_fbs <- translated_fbs%>%
  select(genre,year)%>%
  na.omit(year)%>%
  na.omit(genre)%>%
  group_by(year)%>%
  filter(!duplicated('genre','year'))%>%
  count(genre)

write.csv(g_fbs, "genres_translated_german.csv")

g_fbs%>%
  ggplot(aes(x=year, y=n, fill=genre)) + 
  geom_col()+
  labs(y = "Genre of german-translated publications", x= "Years", lintype = "Legend")+
  scale_x_continuous(breaks = seq(1000, 2000, by = 2))+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

#identifying the primary authors and translators as the spreadsheet

primary1 <- vd17_c %>%
  filter(field_code=="028A",subfield_code=="P") %>%
  collect()%>%
  mutate(name_or_preferred_name=value[!is.na(value)]) %>%
  select(record_number,name_or_preferred_name)

primary2 <- vd17_c %>%
  filter(field_code=="029A",subfield_code=="a") %>%
  collect()%>%
  mutate(name_or_preferred_name=value[!is.na(value)]) %>%
  select(record_number,name_or_preferred_name)

all_primary <- rbind(primary1, primary2)
all_primary <- all_primary[!duplicated(all_primary), ]

primary_fbs <- merge(all_primary,translated_fbs,by="record_number")
primary_fbs <- primary_fbs[!duplicated(primary_fbs), ]

write.csv(primary_fbs, "primary_authors_german.csv")

translator <- vd17_c %>%
  filter(field_code=="028C",subfield_code=="P") %>%
  collect()%>%
  mutate(personal_name=value[!is.na(value)]) %>%
  select(record_number,personal_name)%>%
  left_join(vd17_c %>%
              filter(field_code=="028C",subfield_code=="4") %>%
              collect() %>%
              mutate(role=value[!is.na(value)]) %>%
              select(record_number,role),
            by=c("record_number"))

translator <- translator%>%
  filter(role=="trl")


translator_fbs <- merge(translator,translated_fbs,by="record_number")
translator_fbs <- translator_fbs[!duplicated(translator_fbs), ]

write.csv(translator_fbs, "translators_german.csv")

#identifying female primary authors

gnd_gender <- read_csv(("C:/Users/azizi/Documents/GitHub/vd17-analysis/Narges/data/gnd_gender.csv"), guess_max = Inf, lazy = TRUE)
primary_fbs <- select (primary_fbs, -7)
primary_gender_fbs <- merge(primary_fbs,gnd_gender,by="record_number")
primary_gender_fbs <- primary_gender_fbs[!duplicated(primary_gender_fbs), ]

primary_gender_fbs['gender'][primary_gender_fbs['gender'] == 'Männlich'] <- 'Male'
primary_gender_fbs['gender'][primary_gender_fbs['gender'] == 'Weiblich'] <- 'Female'
primary_gender_fbs['gender'][primary_gender_fbs['gender'] == 'Unbekannt'] <- 'Unknown'

write.csv(primary_gender_fbs, "gender_primary_german.csv")
