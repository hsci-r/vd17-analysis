library(tidyverse)
library(here)
library(DBI)
library(RMariaDB)

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

vd17_a <- tbl(con, "vd17_a")
vd17_c <- tbl(con, "vd17_c")
vd17_id_a <- tbl(con, "vd17_id_a")
vd17_id_c <- tbl(con, "vd17_id_c")

vd17_old_id_to_id_map <- vd17_a %>%
  filter(field_code == "007Y", subfield_code == "0", !str_detect(value, sql("CHR(0)"))) %>%
  inner_join(vd17_id_a, by = c("record_number")) %>%
  select(old_vd17_id = value, vd17_id) %>%
  collect()

fbs <- read_csv(here("data/input/vd17_2022-03-19.csv"), guess_max = Inf, lazy = TRUE) %>%
  left_join(vd17_old_id_to_id_map, by = c("id" = "old_vd17_id")) %>%
  mutate(vd17_id = coalesce(vd17_id, id)) %>%
  left_join(vd17_id_a %>% collect(), by = c("vd17_id")) %>%
  relocate(record_number, vd17_id, aut, title, subtitle, publicationDate, genre, workLanguage, originalLanguage, publisherPlace, normalizedPlace, publisherName)

rm(vd17_old_id_to_id_map)

in_fbs <- fbs %>%
  select(record_number) %>%
  mutate(in_fbs = TRUE) %>%
  copy_to(con, ., name = "in_fbs")

fbs_a <- vd17_a %>% inner_join(in_fbs)

vd17_044s_raw <- read_tsv(here("data/input/044s-pica3.tsv"), lazy = TRUE) %>%
  copy_to(con, ., name = "vd17_044s_raw")
