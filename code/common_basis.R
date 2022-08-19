library(googlesheets4)
library(tidyverse)
library(here)
library(ggplot2)
library(gghsci)
library(tidyr)
library(dplyr)
library(DBI)
library(RMariaDB)
library(patchwork)
library(cowplot)
library(ggpubr)
library(data.table)

unnest_cross <- function(data, cols, ...) {
  .df_out <- data
  .cols <- tidyselect::eval_select(rlang::enquo(cols), data)
  purrr::walk(
    .cols,
    function(col) {
      .df_out <<- unnest(.df_out, {{ col }}, ...)
    }
  )
  .df_out
}

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
  copy_to(con, ., name = "fbs_metadata_a", unique_indexes = c("GND"))

fbs_gnds_a <- fbs_metadata_a %>%
  select(Member_number_new, GND) %>%
  union(
    fbs_metadata_a %>%
      select(Member_number_new, GND = alternate_GND)
  ) %>%
  filter(!is.na(GND)) %>%
  rename(member_number = Member_number_new) %>%
  mutate(GND = str_c("gnd/", GND)) %>%
  copy_to(con, ., name = "fbs_gnds_a", unique_indexes = list(c("GND", "member_number")))

fbs_links_a <- vd17_a %>%
  inner_join(fbs_gnds_a, by = c("value" = "GND")) %>%
  select(record_number, field_number) %>%
  inner_join(vd17_a) %>%
  pivot_wider(
    id_cols = c(record_number, field_number, field_code),
    values_from = value, names_from = subfield_code
  ) %>%
  rename(GND = `7`, role = `4`, role2 = B, ppn = `9`) %>%
  compute(unique_indexes = list(c("record_number", "field_number", "field_code")), indexes = c("field_code", "GND", "role"))

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
  filter(field_code == "044S", value != " ") %>%
  pivot_wider(
    id_cols = record_number:field_number,
    values_from = value, names_from = subfield_code
  ) %>%
  mutate(genre = str_replace_all(a[!is.na(a)], ":.*", "")) %>%
  compute(unique_indexes = list(c("record_number", "field_number")))

vd17_normalized_locs_a <- vd17_a %>%
  filter(field_code == "033D") %>%
  collect() %>%
  pivot_wider(id_cols = record_number:field_number, values_from = value, names_from = subfield_code, values_fn = list) %>%
  unnest_cross(p:z, keep_empty = TRUE) %>%
  copy_to(con, ., name = "vd17_normalized_locs_a", indexes = list(c("record_number", "field_number")))

vd17_normalized_langs_a <- vd17_a %>%
  filter(field_code == "010@") %>%
  collect() %>%
  pivot_wider(id_cols = record_number:field_number, values_from = value, names_from = subfield_code, values_fn = list) %>%
  rename(publication_language = a, original_language = c, intermediary_language = b) %>%
  unnest_cross(publication_language:intermediary_language, keep_empty = TRUE) %>%
  copy_to(con, ., name = "vd17_normalized_langs_a", indexes = list(c("record_number", "field_number")))

vd17_titles_a <- vd17_a %>%
  filter(field_code == "021A") %>%
  pivot_wider(id_cols = record_number:field_number, values_from = value, names_from = subfield_code) %>%
  rename(title = `a`) %>%
  compute(unique_indexes = list(c("record_number", "field_number")))

# vd17_wide_a <- vd17_a %>%
#  select(record_number,field_code:value) %>%
#  mutate(value=str_replace_all(value,sql("CHR(0)"),"_")) %>%
#  pivot_wider(id_cols=record_number,names_from=field_code:subfield_code,values_from=value,values_fn=~str_flatten(.,collapse="|")) %>%
#  compute(name="vd17_wide_a")

fbs_links_of_interest_a <- fbs_links_a %>%
  filter(
    field_code %in% c("028A", "028B", "028C"),
    is.na(role) | !role %in% c("ctb", "oth", "dte"),
    is.na(role2) | !role2 %in% c(
      "AdressatIn",
      "ErwähnteR",
      "GefeierteR",
      "Mitglied eines Ausschusses, der akademische Grade vergibt",
      "Normerlassende Gebietskörperschaft",
      "Praeses",
      "RespondentIn",
      "Sonstige Person, Familie und Körperschaft",
      "VerfasserIn eines Vorworts",
      "VerfasserIn von ergänzendem Text",
      "VerfasserIn von Zusatztexten",
      "VertragspartnerIn",
      "WidmendeR",
      "WidmungsempfängerIn",
      "ZensorIn"
    )
  )
