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

#number of publications, members and the topics covered

df_num <- vd17_c %>%
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
            by=c("record_number"))

fbs_num <- merge(df_num,fbs,by="record_number")
fbs_num <- fbs_num[!duplicated(fbs_num), ]
fbs_num <- select (fbs_num, -4)

pub1_fbs <- fbs_num%>%
  select(record_number,year)%>%
  filter(year>=1617&year<=1650)%>%
  na.omit(year)%>%
  na.omit(record_number)%>%
  group_by(year)%>%
  summarise(freq = n_distinct(record_number))

pub2_fbs <- fbs_num%>%
  select(record_number,year)%>%
  filter(year>=1651&year<=1667)%>%
  na.omit(year)%>%
  na.omit(record_number)%>%
  group_by(year)%>%
  summarise(freq = n_distinct(record_number))

pub3_fbs <- fbs_num%>%
  select(record_number,year)%>%
  filter(year>=1668&year<=1682)%>%
  na.omit(year)%>%
  na.omit(record_number)%>%
  group_by(year)%>%
  summarise(freq = n_distinct(record_number))

publications1 <- ggplot(NULL, aes(x=year, y=freq)) + 
  geom_line(data=pub1_fbs, aes(color = "Phase1"))+
  geom_line(data=pub2_fbs, aes(color = "Phase2"))+
  geom_line(data=pub3_fbs, aes(color = "Phase3"))+
  labs(y = "Number of unique publications", x= "Three phases of fbs", lintype = "Legend")+
  scale_x_continuous(breaks = seq(1000, 2000, by = 10))

frequency_counts_pub <- fbs_num%>%
  select(record_number,year)%>%
  na.omit(record_number)%>%
  na.omit(year)%>%
  group_by(year)%>%
  summarise(freq = n_distinct(record_number))

cumulative_frequencies_pub <- frequency_counts_pub %>% 
  arrange(year) %>% 
  mutate(cum_frequency=cumsum(freq))


publications2 <- ggplot(cumulative_frequencies_pub, aes(x=year, y=cum_frequency)) +
  geom_step() + xlab("Year") + ylab("Number of publications (cumulative distribution)")

ggarrange(publications1,publications2, 
          ncol = 2, nrow = 1)

m1_fbs <- fbs_num%>%
  select(year,gnd)%>%
  filter(year>=1617&year<=1650)%>%
  na.omit(year)%>%
  na.omit(gnd)%>%
  group_by(year)%>%
  summarise(freq = n_distinct(gnd))

m2_fbs <- fbs_num%>%
  select(year,gnd)%>%
  filter(year>=1651&year<=1667)%>%
  na.omit(year)%>%
  na.omit(gnd)%>%
  group_by(year)%>%
  summarise(freq = n_distinct(gnd))

m3_fbs <- fbs_num%>%
  select(year,gnd)%>%
  filter(year>=1668&year<=1682)%>%
  na.omit(year)%>%
  na.omit(gnd)%>%
  group_by(year)%>%
  summarise(freq = n_distinct(gnd))

participants1 <- ggplot(NULL, aes(x=year, y=freq)) + 
  geom_line(data=m1_fbs, aes(color = "Phase1"))+
  geom_line(data=m2_fbs, aes(color = "Phase2"))+
  geom_line(data=m3_fbs, aes(color = "Phase3"))+
  labs(y = "Number of participants", x= "Three phases of fbs", lintype = "Legend")+
  scale_x_continuous(breaks = seq(1000, 2000, by = 10))

frequency_counts_mem <- fbs_num%>%
  select(year,gnd)%>%
  na.omit(year)%>%
  na.omit(gnd)%>%
  group_by(year)%>%
  summarise(freq = n_distinct(gnd))

cumulative_frequencies_mem <- frequency_counts_mem %>% 
  arrange(year) %>% 
  mutate(cum_frequency=cumsum(freq))


participants2 <- ggplot(cumulative_frequencies_mem, aes(x=year, y=cum_frequency)) +
  geom_step() + xlab("Year") + ylab("Number of participants (cumulative distribution)")

ggarrange(participants1,participants2, 
          ncol = 2, nrow = 1)

fbs_num%>%
  select(genre,year)%>%
  na.omit(year)%>%
  na.omit(genre)%>%
  mutate(genre=fct_lump_n(genre,10)) %>%
  ggplot(aes(x=year,fill=genre)) + 
  geom_bar(width=1,position='fill') +
  labs(fill=NULL) +
  scale_x_continuous(breaks = seq(1000, 2000, by = 5)) +
  scale_y_continuous(labels=scales::percent_format(accuracy=1),breaks = seq(0, 1, by = 0.1)) +
  labs(y = "Top 10 genres of fbs", x= "Years")+
  theme_hsci_discrete() +
  theme(legend.position = "bottom")


#number of dedicatees for each phase

dedicatees1 <- vd17_c %>%
  filter(field_code=="028C",subfield_code=="4") %>%
  mutate(role=value[!is.na(value)]) %>%
  collect()%>%
  select(record_number,role)%>%
  filter(role=="dte")

dedicatees2 <- vd17_c %>%
  filter(field_code=="029G",subfield_code=="4") %>%
  mutate(role=value[!is.na(value)]) %>%
  collect()%>%
  select(record_number,role)%>%
  filter(role=="dte")


ded_all <- rbind(dedicatees1, dedicatees2)

all_dedicatees <- merge(fbs_num,ded_all,by="record_number")
all_dedicatees <- all_dedicatees[,c("record_number","year","role")]
all_dedicatees <- all_dedicatees[!duplicated(all_dedicatees), ]

d1_fbs <- all_dedicatees%>%
  select(year,role)%>%
  filter(year>=1617&year<=1650)%>%
  na.omit(year)%>%
  na.omit(role)%>%
  group_by(year)%>%
  count(role)

d2_fbs <- all_dedicatees%>%
  select(year,role)%>%
  filter(year>=1651&year<=1667)%>%
  na.omit(year)%>%
  na.omit(role)%>%
  group_by(year)%>%
  count(role)

d3_fbs <- all_dedicatees%>%
  select(year,role)%>%
  filter(year>=1668&year<=1682)%>%
  na.omit(year)%>%
  na.omit(role)%>%
  group_by(year)%>%
  count(role)

ggplot(NULL, aes(x=year, y=n)) + 
  geom_line(data=d1_fbs, aes(color = "Phase1"))+
  geom_line(data=d2_fbs, aes(color = "Phase2"))+
  geom_line(data=d3_fbs, aes(color = "Phase3"))+
  labs(y = "Number of dedicatees", x= "Three phases of fbs", lintype = "Legend")+
  scale_x_continuous(breaks = seq(1000, 2000, by = 10))


