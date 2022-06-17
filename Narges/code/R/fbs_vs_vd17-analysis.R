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

gnd_gender <- read_csv(("C:/Users/azizi/Documents/gitHub-mine/vd17-analysis/gnd_gender.csv"), guess_max = Inf, lazy = TRUE)
fbs <- read_csv(("C:/Users/azizi/Documents/gitHub-mine/vd17-analysis/fbs_record_number.csv"), guess_max = Inf, lazy = TRUE)


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
  left_join(gnd_gender %>%
              collect()%>%
              select(record_number,gender,gnd),
            by=c("record_number"))


df1['gender'][df1['gender'] == 'Männlich'] <- 'Male'
df1['gender'][df1['gender'] == 'Weiblich'] <- 'Female'
df1['gender'][df1['gender'] == 'Unbekannt'] <- 'Unknown'

no_gnd_df1 <- select(df1, -gnd)
all_fbs <- merge(no_gnd_df1,fbs,by="record_number")
all_fbs <- all_fbs[!duplicated(all_fbs), ]
all_fbs <- select (all_fbs, -5)

#number of publications per each phase

pub1_fbs <- all_fbs%>%
  select(record_number,year)%>%
  filter(year>=1617&year<=1650)%>%
  na.omit(year)%>%
  na.omit(record_number)%>%
  group_by(year)%>%
  distinct(record_number, .keep_all = TRUE)%>%
  summarise(freq = n_distinct(record_number))

pub2_fbs <- all_fbs%>%
  select(record_number,year)%>%
  filter(year>=1651&year<=1667)%>%
  na.omit(year)%>%
  na.omit(record_number)%>%
  group_by(year)%>%
  distinct(record_number, .keep_all = TRUE)%>%
  summarise(freq = n_distinct(record_number))

pub3_fbs <- all_fbs%>%
  select(record_number,year)%>%
  filter(year>=1668&year<=1682)%>%
  na.omit(year)%>%
  na.omit(record_number)%>%
  group_by(year)%>%
  distinct(record_number, .keep_all = TRUE)%>%
  summarise(freq = n_distinct(record_number))

publications1 <- ggplot(NULL, aes(x=year, y=freq)) + 
  geom_line(data=pub1_fbs, aes(color = "Phase1"))+
  geom_line(data=pub2_fbs, aes(color = "Phase2"))+
  geom_line(data=pub3_fbs, aes(color = "Phase3"))+
  labs(y = "Number of unique publications", x= "Three phases of fbs", lintype = "Legend")+
  scale_x_continuous(breaks = seq(1000, 2000, by = 10))

pub1_all <- df1%>%
  select(record_number,year)%>%
  filter(year>=1617&year<=1650)%>%
  na.omit(year)%>%
  na.omit(record_number)%>%
  group_by(year)%>%
  distinct(record_number, .keep_all = TRUE)%>%
  summarise(freq = n_distinct(record_number))

pub2_all <- df1%>%
  select(record_number,year)%>%
  filter(year>=1651&year<=1667)%>%
  na.omit(year)%>%
  na.omit(record_number)%>%
  group_by(year)%>%
  distinct(record_number, .keep_all = TRUE)%>%
  summarise(freq = n_distinct(record_number))

pub3_all <- df1%>%
  select(record_number,year)%>%
  filter(year>=1668&year<=1682)%>%
  na.omit(year)%>%
  na.omit(record_number)%>%
  group_by(year)%>%
  distinct(record_number, .keep_all = TRUE)%>%
  summarise(freq = n_distinct(record_number))

publications2 <- ggplot(NULL, aes(x=year, y=freq)) + 
  geom_line(data=pub1_all, aes(color = "Phase1"))+
  geom_line(data=pub2_all, aes(color = "Phase2"))+
  geom_line(data=pub3_all, aes(color = "Phase3"))+
  labs(y = "Number of unique publications", x= "Three phases of vd17", lintype = "Legend")+
  scale_x_continuous(breaks = seq(1000, 2000, by = 10))

ggarrange(publications1,publications2, 
          ncol = 2, nrow = 1)

#number of participants (members) per each phase

m1_fbs <- all_fbs%>%
  select(gnd,year)%>%
  filter(year>=1617&year<=1650)%>%
  na.omit(year)%>%
  na.omit(gnd)%>%
  group_by(year)%>%
  distinct(gnd, .keep_all = TRUE)%>%
  summarise(freq = n_distinct(gnd))

m2_fbs <- all_fbs%>%
  select(gnd,year)%>%
  filter(year>=1651&year<=1667)%>%
  na.omit(year)%>%
  na.omit(gnd)%>%
  group_by(year)%>%
  distinct(gnd, .keep_all = TRUE)%>%
  summarise(freq = n_distinct(gnd))

m3_fbs <- all_fbs%>%
  select(gnd,year)%>%
  filter(year>=1668&year<=1682)%>%
  na.omit(year)%>%
  na.omit(gnd)%>%
  group_by(year)%>%
  distinct(gnd, .keep_all = TRUE)%>%
  summarise(freq = n_distinct(gnd))

participants1 <- ggplot(NULL, aes(x=year, y=freq)) + 
  geom_line(data=m1_fbs, aes(color = "Phase1"))+
  geom_line(data=m2_fbs, aes(color = "Phase2"))+
  geom_line(data=m3_fbs, aes(color = "Phase3"))+
  labs(y = "Number of participants", x= "Three phases of fbs", lintype = "Legend")+
  scale_x_continuous(breaks = seq(1000, 2000, by = 10))

m1_all <- df1%>%
  select(gnd,year)%>%
  filter(year>=1617&year<=1650)%>%
  na.omit(year)%>%
  na.omit(gnd)%>%
  group_by(year)%>%
  distinct(gnd, .keep_all = TRUE)%>%
  summarise(freq = n_distinct(gnd))

m2_all <- df1%>%
  select(gnd,year)%>%
  filter(year>=1651&year<=1667)%>%
  na.omit(year)%>%
  na.omit(gnd)%>%
  group_by(year)%>%
  distinct(gnd, .keep_all = TRUE)%>%
  summarise(freq = n_distinct(gnd))

m3_all <- df1%>%
  select(gnd,year)%>%
  filter(year>=1668&year<=1682)%>%
  na.omit(year)%>%
  na.omit(gnd)%>%
  group_by(year)%>%
  distinct(gnd, .keep_all = TRUE)%>%
  summarise(freq = n_distinct(gnd))

participants2 <- ggplot(NULL, aes(x=year, y=freq)) + 
  geom_line(data=m1_all, aes(color = "Phase1"))+
  geom_line(data=m2_all, aes(color = "Phase2"))+
  geom_line(data=m3_all, aes(color = "Phase3"))+
  labs(y = "Number of participants", x= "Three phases of vd17", lintype = "Legend")+
  scale_x_continuous(breaks = seq(1000, 2000, by = 10))

ggarrange(participants1,participants2, 
          ncol = 2, nrow = 1)

#number of genres per each phase

g1_fbs <- all_fbs%>%
  select(genre,year)%>%
  filter(year>=1617&year<=1650)%>%
  na.omit(year)%>%
  na.omit(genre)%>%
  group_by(year)%>%
  filter(!duplicated('genre','year'))%>%
  summarise(freq = n_distinct(genre))

g2_fbs <- all_fbs%>%
  select(genre,year)%>%
  filter(year>=1651&year<=1667)%>%
  na.omit(year)%>%
  na.omit(genre)%>%
  group_by(year)%>%
  filter(!duplicated('genre','year'))%>%
  summarise(freq = n_distinct(genre))

g3_fbs <- all_fbs%>%
  select(genre,year)%>%
  filter(year>=1668&year<=1682)%>%
  na.omit(year)%>%
  na.omit(genre)%>%
  group_by(year)%>%
  filter(!duplicated('genre','year'))%>%
  summarise(freq = n_distinct(genre))

genre1 <- ggplot(NULL, aes(x=year, y=freq)) + 
  geom_line(data=g1_fbs, aes(color = "Phase1"))+
  geom_line(data=g2_fbs, aes(color = "Phase2"))+
  geom_line(data=g3_fbs, aes(color = "Phase3"))+
  labs(y = "Number of genres", x= "Three phases of fbs", lintype = "Legend")+
  scale_x_continuous(breaks = seq(1000, 2000, by = 10))

g1_all <- df1%>%
  select(genre,year)%>%
  filter(year>=1617&year<=1650)%>%
  na.omit(year)%>%
  na.omit(genre)%>%
  group_by(year)%>%
  filter(!duplicated('genre','year'))%>%
  summarise(freq = n_distinct(genre))

g2_all <- df1%>%
  select(genre,year)%>%
  filter(year>=1651&year<=1667)%>%
  na.omit(year)%>%
  na.omit(genre)%>%
  group_by(year)%>%
  filter(!duplicated('genre','year'))%>%
  summarise(freq = n_distinct(genre))

g3_all <- df1%>%
  select(genre,year)%>%
  filter(year>=1668&year<=1682)%>%
  na.omit(year)%>%
  na.omit(genre)%>%
  group_by(year)%>%
  filter(!duplicated('genre','year'))%>%
  summarise(freq = n_distinct(genre))

genre2 <- ggplot(NULL, aes(x=year, y=freq)) + 
  geom_line(data=g1_all, aes(color = "Phase1"))+
  geom_line(data=g2_all, aes(color = "Phase2"))+
  geom_line(data=g3_all, aes(color = "Phase3"))+
  labs(y = "Number of genres", x= "Three phases of vd17", lintype = "Legend")+
  scale_x_continuous(breaks = seq(1000, 2000, by = 10))

ggarrange(genre1,genre2, 
          ncol = 2, nrow = 1)

#Top 5 popular genres per each phase

g1_10_fbs <- all_fbs%>%
  select(genre,year)%>%
  filter(year>=1617&year<=1650)%>%
  na.omit(year)%>%
  na.omit(genre)%>%
  group_by(year)%>%
  filter(!duplicated('genre','year'))%>%
  count(genre)%>%
  arrange(desc(n))%>%
  slice(1:5)


g2_10_fbs <- all_fbs%>%
  select(genre,year)%>%
  filter(year>=1651&year<=1667)%>%
  na.omit(year)%>%
  na.omit(genre)%>%
  group_by(year)%>%
  filter(!duplicated('genre','year'))%>%
  count(genre)%>%
  arrange(desc(n))%>%
  slice(1:5)

g3_10_fbs <- all_fbs%>%
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
  geom_bar(data=g1_10_fbs)+
  geom_bar(data=g2_10_fbs)+
  geom_bar(data=g3_10_fbs)+
  labs(y = "Top 5 frequent genres per each year", x= "Three phases of fbs", lintype = "Legend")+
  scale_x_continuous(breaks = seq(1000, 2000, by = 5))+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))


g1_10_all <- df1%>%
  select(genre,year)%>%
  filter(year>=1617&year<=1650)%>%
  na.omit(year)%>%
  na.omit(genre)%>%
  group_by(year)%>%
  filter(!duplicated('genre','year'))%>%
  count(genre)%>%
  arrange(desc(n))%>%
  slice(1:5)
  

g2_10_all <- df1%>%
  select(genre,year)%>%
  filter(year>=1651&year<=1667)%>%
  na.omit(year)%>%
  na.omit(genre)%>%
  group_by(year)%>%
  filter(!duplicated('genre','year'))%>%
  count(genre)%>%
  arrange(desc(n))%>%
  slice(1:5)

g3_10_all <- df1%>%
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
  geom_bar(data=g1_10_all)+
  geom_bar(data=g2_10_all)+
  geom_bar(data=g3_10_all)+
  labs(y = "Top 5 frequent genres per each year", x= "Three phases of vd17", lintype = "Legend")+
  scale_x_continuous(breaks = seq(1000, 2000, by = 5))+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

#number of female, male and unknown gender in all three phases

gender1 <- df1%>%
  distinct(record_number, .keep_all = TRUE)%>%
  na.omit(gender)%>%
  na.omit(year)%>%
  na.omit(record_number)%>%
  group_by(year)%>%
  summarise(Male = length(gender[gender=='Male']),Female = length(gender[gender=='Female']), Unknown = length(gender[gender=='Unknown']))%>%
  ggplot() + 
  geom_line(aes(x=year, y=Male, linetype = "Male"))+
  geom_line(aes(x=year, y=Female, linetype = "Female"))+
  geom_line(aes(x=year, y=Unknown, linetype = "Unknown"))+
  scale_y_continuous(breaks=seq(0,1600,100), limits=c(0,1600))+
  labs(y = "Gender participation per year in vd17", x= "Year", lintype = "Legend")
  
gender2 <- all_fbs%>%
  distinct(record_number, .keep_all = TRUE)%>%
  na.omit(gender)%>%
  na.omit(year)%>%
  na.omit(record_number)%>%
  group_by(year)%>%
  summarise(Male = length(gender[gender=='Male']),Female = length(gender[gender=='Female']), Unknown = length(gender[gender=='Unknown']))%>%
  ggplot() + 
  geom_line(aes(x=year, y=Male, linetype = "Male"))+
  geom_line(aes(x=year, y=Female, linetype = "Female"))+
  geom_line(aes(x=year, y=Unknown, linetype = "Unknown"))+
  scale_y_continuous(breaks=seq(0,1600,100), limits=c(0,1600))+
  labs(y = "Gender participation per year in fbs", x= "Year", lintype = "Legend")

ggarrange(gender1,gender2, 
          ncol = 2, nrow = 1)

