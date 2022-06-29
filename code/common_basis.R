library(tidyverse)
library(here)
library(DBI)
library(RMariaDB)
library(googlesheets4)

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
vd17_c <- tbl(con, "vd17_c")
vd17_id_a <- tbl(con, "vd17_id_a")
vd17_id_c <- tbl(con, "vd17_id_c")

fbs_gnds_gs <- read_sheet(ss = "1tYSIXhoeeHk92HsP93Wul4b1mDjlsKcavc9LOi4K6RU", sheet = "vd17_authors_GND", col_types = "c") %>% relocate(Name)

try(dbExecute(con, "DROP TABLE IF EXISTS fbs_gnds_a"))
fbs_gnds_a <- fbs_gnds_gs %>%
  select(Name, GND) %>%
  union(fbs_gnds_gs %>% filter(!is.na(`Old GND`)) %>% select(Name, GND = `Old GND`)) %>%
  mutate(GND = str_c("gnd/", GND)) %>%
  copy_to(con, ., name = "fbs_gnds_a", unique_indexes = c("GND"))

fbs_links <- vd17_a %>%
  inner_join(fbs_gnds_a, by = c("value" = "GND"))

fbs_record_numbers_a <- fbs_links %>%
  distinct(record_number)

fbs_a <- vd17_a %>% inner_join(fbs_record_numbers_a)

vd17_044s_raw <- read_tsv(here("data/input/044s-pica3.tsv"), lazy = TRUE) %>%
  copy_to(con, ., name = "vd17_044s_raw")

vd17_normalized_years_a <- vd17_a %>%
  filter(field_code == "011@") %>%
  pivot_wider(id_cols = record_number:field_number, values_from = value, names_from = subfield_code) %>%
  mutate(normalized_year = as.integer(a))

vd17_normalized_years_c <- vd17_c %>%
  filter(field_code == "011@", subfield_code == "a") %>%
  mutate(normalized_year = as.integer(value)) %>%
  select(record_number, normalized_year)

vd17_normalized_locs_a <- vd17_a %>%
  filter(field_code == "033D") %>%
  pivot_wider(id_cols = record_number:field_number, values_from = value, names_from = subfield_code)

vd17_normalized_locs_c <- vd17_c %>%
  filter(field_code == "033D") %>%
  pivot_wider(id_cols = record_number:field_number, values_from = value, names_from = subfield_code)

vd17_normalized_langs_a <- vd17_a %>%
  filter(field_code == "010@") %>%
  pivot_wider(id_cols = record_number:field_number, values_from = value, names_from = subfield_code)

vd17_normalized_langs_c <- vd17_c %>%
  filter(field_code == "010@") %>%
  pivot_wider(id_cols = record_number:field_number, values_from = value, names_from = subfield_code)

vd17_titles_a <- vd17_a %>%
  filter(field_code == "021A") %>%
  pivot_wider(id_cols = record_number:field_number, values_from = value, names_from = subfield_code)

vd17_titles_c <- vd17_c %>%
  filter(field_code == "021A") %>%
  pivot_wider(id_cols = record_number:field_number, values_from = value, names_from = subfield_code)

# vd17_wide_a <- vd17_a %>%
#  select(record_number,field_code:value) %>%
#  mutate(value=str_replace_all(value,sql("CHR(0)"),"_")) %>%
#  pivot_wider(id_cols=record_number,names_from=field_code:subfield_code,values_from=value,values_fn=~str_flatten(.,collapse="|")) %>%
#  compute(name="vd17_wide_a")
