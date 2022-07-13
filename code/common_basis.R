library(googlesheets4)
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
#<<<<<<< Updated upstream
library(ggpubr)
library(data.table)
#=======
library(ggpubr)                             
library(data.table) 
library(stringi)
#>>>>>>> Stashed changes

con <- dbConnect(
  drv = MariaDB(),
  host = "vm2505.kaj.pouta.csc.fi",
  dbname = "vd17",
  user = "vd17",
  password = "vd17",
  bigint = "integer",
  load_data_local_infile = TRUE,
  autocommit = TRUE,
  reconnect = TRUE
)
dbExecute(con, "SET default_storage_engine=Aria")

vd17_a <- tbl(con, "vd17_a")
# vd17_c <- tbl(con, "vd17_c")
vd17_id_a <- tbl(con, "vd17_id_a")
# vd17_id_c <- tbl(con, "vd17_id_c")

fbs_metadata_gs <- read_sheet(ss = "1tYSIXhoeeHk92HsP93Wul4b1mDjlsKcavc9LOi4K6RU", sheet = "FBS_master_metadata", col_types = "c") %>% relocate(Name)

try(dbExecute(con, "DROP TABLE IF EXISTS fbs_a"), silent = TRUE)

fbs_metadata_a <- fbs_metadata_gs %>%
  mutate(GND = str_c("gnd/", GND)) %>%
  copy_to(con, ., name = "fbs_a", unique_indexes = c("GND"))

fbs_links_a <- vd17_a %>%
  inner_join(fbs_metadata_a, by = c("value" = "GND")) %>%
  select(record_number, field_number) %>%
  inner_join(vd17_a) %>%
  pivot_wider(
    id_cols = c(record_number, field_number, field_code),
    values_from = value, names_from = subfield_code
  ) %>%
  compute(unique_indexes = list(c("record_number", "field_number", "field_code")), indexes = c("field_code"))

fbs_record_numbers_a <- fbs_links_a %>%
  distinct(record_number)

fbs_records_a <- vd17_a %>%
  inner_join(fbs_record_numbers_a)

vd17_044s_raw <- read_tsv(here("data/input/044s-pica3.tsv"), lazy = TRUE) %>%
  copy_to(con, ., name = "vd17_044s_raw")

vd17_normalized_years_a <- vd17_a %>%
  filter(field_code == "011@") %>%
  pivot_wider(id_cols = record_number:field_number, values_from = value, names_from = subfield_code) %>%
  mutate(normalized_year = as.integer(a)) %>%
  compute(unique_indexes = list(c("record_number", "field_number")))

vd17_genres_a <- vd17_a %>%
  filter(field_code == "044S") %>%
  pivot_wider(
    id_cols = record_number:field_number,
    values_from = value, names_from = subfield_code
  ) %>%
  mutate(genre = str_replace_all(a[!is.na(a)], ":.*", "")) %>%
  compute(unique_indexes = list(c("record_number", "field_number")))

#all contributors

cont=c("BeiträgerIn","Beiträger","MitwirkendeR")

vd17_contributors1_a <- vd17_a %>%
  filter(field_code == "028C",subfield_code == "4",value=="ctb") %>%
  select(record_number)%>%
  distinct(record_number)

vd17_contributors2_a <- vd17_a %>%
  filter(field_code == "028G",subfield_code == "4",value=="ctb") %>%
  select(record_number)%>%
  distinct(record_number)

vd17_contributors3_a <- vd17_a %>%
  filter(field_code == "028C",subfield_code == "4",value=="oth") %>%
  select(record_number)%>%
  inner_join(vd17_a %>%
               filter(field_code == "028C",subfield_code == "B",value %in% cont) %>%
               select(record_number),
             by=c("record_number"))%>%
  distinct(record_number)

vd17_contributors4_a <- vd17_a %>%
  filter(field_code == "028G",subfield_code == "4",value=="oth") %>%
  select(record_number)%>%
  inner_join(vd17_a %>%
               filter(field_code == "028G",subfield_code == "B",value %in% cont) %>%
               select(record_number),
             by=c("record_number"))%>%
  distinct(record_number)

#all dedicatees

vd17_dedicatees1_a <- vd17_a %>%
  filter(field_code == "028C",subfield_code == "4",value=="dte") %>%
  select(record_number)%>%
  distinct(record_number)

vd17_dedicatees2_a <- vd17_a %>%
  filter(field_code == "028G",subfield_code == "4",value=="dte") %>%
  select(record_number)%>%
  distinct(record_number)

vd17_dedicatees3_a <- vd17_a %>%
  filter(field_code == "029F",subfield_code == "4",value=="dte") %>%
  select(record_number)%>%
  distinct(record_number)

# vd17_normalized_years_c <- vd17_c %>%
#  filter(field_code == "011@") %>%
#  pivot_wider(id_cols = record_number:field_number, values_from = value, names_from = subfield_code) %>%
#  mutate(normalized_year = as.integer(a))

vd17_normalized_locs_a <- vd17_a %>%
  filter(field_code == "033D") %>%
  pivot_wider(id_cols = record_number:field_number, values_from = value, names_from = subfield_code) %>%
  compute(unique_indexes = list(c("record_number", "field_number")))

# vd17_normalized_locs_c <- vd17_c %>%
#  filter(field_code == "033D") %>%
#  pivot_wider(id_cols = record_number:field_number, values_from = value, names_from = subfield_code)

vd17_normalized_langs_a <- vd17_a %>%
  filter(field_code == "010@") %>%
  pivot_wider(id_cols = record_number:field_number, values_from = value, names_from = subfield_code) %>%
  rename(publication_language = a, original_language = c) %>%
  compute(unique_indexes = list(c("record_number", "field_number")))

# vd17_normalized_langs_c <- vd17_c %>%
#  filter(field_code == "010@") %>%
#  pivot_wider(id_cols = record_number:field_number, values_from = value, names_from = subfield_code)

vd17_titles_a <- vd17_a %>%
  filter(field_code == "021A") %>%
  pivot_wider(id_cols = record_number:field_number, values_from = value, names_from = subfield_code) %>%
  compute(unique_indexes = list(c("record_number", "field_number")))

# vd17_titles_c <- vd17_c %>%
#  filter(field_code == "021A") %>%
#  pivot_wider(id_cols = record_number:field_number, values_from = value, names_from = subfield_code)

# vd17_wide_a <- vd17_a %>%
#  select(record_number,field_code:value) %>%
#  mutate(value=str_replace_all(value,sql("CHR(0)"),"_")) %>%
#  pivot_wider(id_cols=record_number,names_from=field_code:subfield_code,values_from=value,values_fn=~str_flatten(.,collapse="|")) %>%
#  compute(name="vd17_wide_a")
