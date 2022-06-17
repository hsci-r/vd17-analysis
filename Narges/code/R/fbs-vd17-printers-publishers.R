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
  left_join(gnd_gender %>%
              collect()%>%
              select(record_number,gnd),
            by=c("record_number"))
  

p_p1 <- vd17_c %>%
  filter(field_code=="028C",subfield_code=="9") %>%
  mutate(ppn=value[!is.na(value)]) %>%
  collect()%>%
  select(record_number,ppn)
p_p2 <- vd17_c %>%
  filter(field_code=="029F",subfield_code=="9") %>%
  mutate(ppn=value[!is.na(value)]) %>%
  collect()%>%
  select(record_number,ppn)
p_p3 <- vd17_c %>%
  filter(field_code=="033J",subfield_code=="9") %>%
  mutate(ppn=value[!is.na(value)]) %>%
  collect()%>%
  select(record_number,ppn)

df2 <- rbind(p_p1, p_p2, p_p3)

all_ppn <- merge(df1,df2,by="record_number")
all_ppn <- all_ppn[!duplicated(all_ppn), ]

fbs_ppn <- select(fbs, -1,-2)
fbs_ppn <- merge(fbs_ppn,all_ppn,by="record_number")
fbs_ppn <- fbs_ppn[!duplicated(fbs_ppn), ]

#number of unique() ppn per each year in fbs community and whole vd17

fbs_ppn_year <- fbs_ppn%>%
  na.omit(year)%>%
  na.omit(ppn)%>%
  group_by(year)%>%
  distinct(ppn, .keep_all = TRUE)%>%
  summarise(freq = n_distinct(ppn))


vd17_ppn_year <- all_ppn%>%
  na.omit(year)%>%
  na.omit(ppn)%>%
  group_by(year)%>%
  distinct(ppn, .keep_all = TRUE)%>%
  summarise(freq = n_distinct(ppn))
  
  
ggplot(NULL,aes(x = year,y=freq))+
  geom_line(data=fbs_ppn_year, aes(color = "fbs"))+
  geom_line(data=vd17_ppn_year, aes(color = "vd17"))+
  labs(y = "Number of printers/publishers", x= "Years", lintype = "Legend")+
  scale_x_continuous(breaks = seq(1000, 2000, by = 10))


#The genre of top 10 popular ppn per each phase for fbs and vd17


genre_ppn_vd17 <- all_ppn%>%
  left_join(vd17_c %>%
              filter(field_code=="044S",subfield_code=="a") %>%
              collect() %>%
              mutate(genre=str_replace_all(value[!is.na(value)],":.*","")) %>%
              select(record_number,genre),
            by=c("record_number")) 
  
  
genre_ppn_fbs <- fbs_ppn%>%
  left_join(vd17_c %>%
              filter(field_code=="044S",subfield_code=="a") %>%
              collect() %>%
              mutate(genre=str_replace_all(value[!is.na(value)],":.*","")) %>%
              select(record_number,genre),
            by=c("record_number"))


phase1_vd17 <- genre_ppn_vd17%>%
  distinct()%>%
  filter(year>=1617&year<=1650)%>%
  na.omit(year)%>%
  na.omit(genre)%>%
  na.omit(ppn)%>%
  group_by(year,genre)%>%
  count(ppn)%>%
  arrange(desc(n))%>%
  head(10)

phase2_vd17 <- genre_ppn_vd17%>%
  distinct()%>%
  filter(year>=1651&year<=1667)%>%
  na.omit(year)%>%
  na.omit(genre)%>%
  na.omit(ppn)%>%
  group_by(year,genre)%>%
  count(ppn)%>%
  arrange(desc(n))%>%
  head(10)

phase3_vd17 <- genre_ppn_vd17%>%
  distinct()%>%
  filter(year>=1668&year<=1682)%>%
  na.omit(year)%>%
  na.omit(genre)%>%
  na.omit(ppn)%>%
  group_by(year,genre)%>%
  count(ppn)%>%
  arrange(desc(n))%>%
  head(10)

phase1_fbs <- genre_ppn_fbs%>%
  distinct()%>%
  filter(year>=1617&year<=1650)%>%
  na.omit(year)%>%
  na.omit(genre)%>%
  na.omit(ppn)%>%
  group_by(year,genre)%>%
  count(ppn)%>%
  arrange(desc(n))%>%
  head(10)

phase2_fbs <- genre_ppn_fbs%>%
  distinct()%>%
  filter(year>=1651&year<=1667)%>%
  na.omit(year)%>%
  na.omit(genre)%>%
  na.omit(ppn)%>%
  group_by(year,genre)%>%
  count(ppn)%>%
  arrange(desc(n))%>%
  head(10)

phase3_fbs <- genre_ppn_fbs%>%
  distinct()%>%
  filter(year>=1668&year<=1682)%>%
  na.omit(year)%>%
  na.omit(genre)%>%
  na.omit(ppn)%>%
  group_by(year,genre)%>%
  count(ppn)%>%
  arrange(desc(n))%>%
  head(10)

vd17_all <- bind_rows(phase1_vd17, phase2_vd17, phase3_vd17, .id = "phase")
fbs_all <- bind_rows(phase1_fbs, phase2_fbs, phase3_fbs, .id = "phase")

p1 <- ggplot(vd17_all, aes(x=genre, y=n, fill = phase)) + 
  geom_col(position = position_dodge())+
  labs(y = "The genre of top 10 popular printers/publishers of vd17",x="Genre")+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

p2 <- ggplot(fbs_all, aes(x=genre, y=n, fill = phase)) + 
  geom_col(position = position_dodge())+
  labs(y = "The genre of top 10 popular printers/publishers of fbs",x="Genre")+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

ggarrange(p1,p2, 
          ncol = 2, nrow = 1)

#The language of top 10 popular ppn per each phase for fbs and vd17


lan_ppn_vd17 <- all_ppn%>%
  left_join(vd17_c %>%
              filter(field_code=="010@",subfield_code=="a") %>%
              collect() %>%
              mutate(language=value[!is.na(value)]) %>%
              select(record_number,language),
            by=c("record_number"))

lan_ppn_fbs <- fbs_ppn%>%
  left_join(vd17_c %>%
              filter(field_code=="010@",subfield_code=="a") %>%
              collect() %>%
              mutate(language=value[!is.na(value)]) %>%
              select(record_number,language),
            by=c("record_number"))


p1_vd17 <- lan_ppn_vd17%>%
  distinct()%>%
  filter(year>=1617&year<=1650)%>%
  na.omit(year)%>%
  na.omit(language)%>%
  na.omit(ppn)%>%
  group_by(year,language)%>%
  count(ppn)%>%
  arrange(desc(n))%>%
  head(10)

p2_vd17 <- lan_ppn_vd17%>%
  distinct()%>%
  filter(year>=1651&year<=1667)%>%
  na.omit(year)%>%
  na.omit(language)%>%
  na.omit(ppn)%>%
  group_by(year,language)%>%
  count(ppn)%>%
  arrange(desc(n))%>%
  head(10)

p3_vd17 <- lan_ppn_vd17%>%
  distinct()%>%
  filter(year>=1668&year<=1682)%>%
  na.omit(year)%>%
  na.omit(language)%>%
  na.omit(ppn)%>%
  group_by(year,language)%>%
  count(ppn)%>%
  arrange(desc(n))%>%
  head(10)

p1_fbs <- lan_ppn_fbs%>%
  distinct()%>%
  filter(year>=1617&year<=1650)%>%
  na.omit(year)%>%
  na.omit(language)%>%
  na.omit(ppn)%>%
  group_by(year,language)%>%
  count(ppn)%>%
  arrange(desc(n))%>%
  head(10)

p2_fbs <- lan_ppn_fbs%>%
  distinct()%>%
  filter(year>=1651&year<=1667)%>%
  na.omit(year)%>%
  na.omit(language)%>%
  na.omit(ppn)%>%
  group_by(year,language)%>%
  count(ppn)%>%
  arrange(desc(n))%>%
  head(10)

p3_fbs <- lan_ppn_fbs%>%
  distinct()%>%
  filter(year>=1668&year<=1682)%>%
  na.omit(year)%>%
  na.omit(language)%>%
  na.omit(ppn)%>%
  group_by(year,language)%>%
  count(ppn)%>%
  arrange(desc(n))%>%
  head(10)

all_vd17 <- bind_rows(p1_vd17, p2_vd17, p3_vd17, .id = "phase")
all_fbs <- bind_rows(p1_fbs, p2_fbs, p3_fbs, .id = "phase")

plot1 <- ggplot(all_vd17, aes(x=language, y=n, fill = phase)) + 
  geom_col(position = position_dodge())+
  labs(y = "The language of top 10 popular printers/publishers in vd17", x= "Language")

plot2 <- ggplot(all_fbs, aes(x=language, y=n, fill = phase)) + 
  geom_col(position = position_dodge())+
  labs(y = "The language of top 10 popular printers/publishers in fbs", x= "Language")

ggarrange(plot1,plot2, 
          ncol = 2, nrow = 1)

#The place of publications of top 10 popular ppn per each phase for fbs and vd17

place_ppn_vd17 <- all_ppn%>%
  left_join(vd17_c %>%
              filter(field_code=="033D",subfield_code=="p") %>%
              collect() %>%
              mutate(place=value[!is.na(value)]) %>%
              select(record_number,place),
              by=c("record_number"))%>%
  left_join(vd17_c %>%
              filter(field_code=="033D",subfield_code=="4") %>%
              collect() %>%
              mutate(code=value[!is.na(value)]) %>%
              select(record_number,code),
            by=c("record_number"))%>%
  filter(code=="pup")

place_ppn_fbs <- fbs_ppn%>%
  left_join(vd17_c %>%
              filter(field_code=="033D",subfield_code=="p") %>%
              collect() %>%
              mutate(place=value[!is.na(value)]) %>%
              select(record_number,place),
            by=c("record_number"))%>%
  left_join(vd17_c %>%
              filter(field_code=="033D",subfield_code=="4") %>%
              collect() %>%
              mutate(code=value[!is.na(value)]) %>%
              select(record_number,code),
            by=c("record_number"))%>%
  filter(code=="pup")


pl1_vd17 <- place_ppn_vd17%>%
  distinct()%>%
  filter(year>=1617&year<=1650)%>%
  na.omit(year)%>%
  na.omit(place)%>%
  na.omit(ppn)%>%
  group_by(year,place)%>%
  count(ppn)%>%
  arrange(desc(n))%>%
  head(10)

pl2_vd17 <- place_ppn_vd17%>%
  distinct()%>%
  filter(year>=1651&year<=1667)%>%
  na.omit(year)%>%
  na.omit(place)%>%
  na.omit(ppn)%>%
  group_by(year,place)%>%
  count(ppn)%>%
  arrange(desc(n))%>%
  head(10)

pl3_vd17 <- place_ppn_vd17%>%
  distinct()%>%
  filter(year>=1668&year<=1682)%>%
  na.omit(year)%>%
  na.omit(place)%>%
  na.omit(ppn)%>%
  group_by(year,place)%>%
  count(ppn)%>%
  arrange(desc(n))%>%
  head(10)

pl1_fbs <- place_ppn_fbs%>%
  distinct()%>%
  filter(year>=1617&year<=1650)%>%
  na.omit(year)%>%
  na.omit(place)%>%
  na.omit(ppn)%>%
  group_by(year,place)%>%
  count(ppn)%>%
  arrange(desc(n))%>%
  head(10)

pl2_fbs <- place_ppn_fbs%>%
  distinct()%>%
  filter(year>=1651&year<=1667)%>%
  na.omit(year)%>%
  na.omit(place)%>%
  na.omit(ppn)%>%
  group_by(year,place)%>%
  count(ppn)%>%
  arrange(desc(n))%>%
  head(10)

pl3_fbs <- place_ppn_fbs%>%
  distinct()%>%
  filter(year>=1668&year<=1682)%>%
  na.omit(year)%>%
  na.omit(place)%>%
  na.omit(ppn)%>%
  group_by(year,place)%>%
  count(ppn)%>%
  arrange(desc(n))%>%
  head(10)

all_place_vd17 <- bind_rows(pl1_vd17, pl2_vd17, pl3_vd17, .id = "phase")
all_place_fbs <- bind_rows(pl1_fbs, pl2_fbs, pl3_fbs, .id = "phase")

plot_place1 <- ggplot(all_place_vd17, aes(x=place, y=n, fill = phase)) + 
  geom_col(position = position_dodge())+
  labs(y = "The place of publications of top 10 popular printers/publishers in vd17", x= "Place")

plot_place2 <- ggplot(all_place_fbs, aes(x=place, y=n, fill = phase)) + 
  geom_col(position = position_dodge())+
  labs(y = "The place of publications top 10 popular printers/publishers in fbs", x= "Place")

ggarrange(plot_place1,plot_place2,
          ncol = 2, nrow = 1)
