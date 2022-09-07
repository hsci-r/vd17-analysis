library(googlesheets4)
library(tidyverse)
library(here)
library(ggplot2)
library(gghsci)
library(tidyr)
library(dplyr)
library(DBI)
library(RMariaDB)
library(hscidbutil)
library(keyring)
library(patchwork)
library(cowplot)
library(ggpubr)
library(data.table)
library(gt)

p <- function(number) {
  return(format(number, scientific = FALSE, big.mark = ","))
}
pp <- function(percentage, accuracy = 0.01) {
  return(scales::percent(percentage, accuracy = accuracy))
}

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

is_html_output <- function() {
  is.null(knitr::pandoc_to()) || (!str_detect(knitr::pandoc_to(), "^gfm") && knitr::is_html_output())
}

while (!exists("con")) {
  tryCatch(con <- dbConnect(
    drv = MariaDB(),
    host = "vm2505.kaj.pouta.csc.fi",
    dbname = "vd17_analysis",
    user = "vd17_analysis",
    password = key_get("vd17_analysis", "vd17_analysis"),
    bigint = "integer",
    load_data_local_infile = TRUE,
    autocommit = TRUE,
    reconnect = TRUE
  ), error = function(e) {
    print(e)
    key_set("vd17_analysis", "vd17_analysis")
  })
}

try(vd17_a <- tbl(con, dbplyr::in_schema("vd17", "vd17_a")))
try(vd17_c <- tbl(con, dbplyr::in_schema("vd17", "vd17_c")))

try(vd17_auth_a <- tbl(con, dbplyr::in_schema("vd17", "vd17_auth_a")))
try(vd17_auth_c <- tbl(con, dbplyr::in_schema("vd17", "vd17_auth_c")))

try(vd17_genres_a <- tbl(con, dbplyr::in_schema("vd17", "vd17_genres_a")))
try(vd17_genres_c <- tbl(con, dbplyr::in_schema("vd17", "vd17_genres_c")))

try(vd17_id_a <- tbl(con, dbplyr::in_schema("vd17", "vd17_id_a")))
try(vd17_id_c <- tbl(con, dbplyr::in_schema("vd17", "vd17_id_a")))

try(vd17_normalized_langs_a <- tbl(con, dbplyr::in_schema("vd17", "vd17_normalized_langs_a")))
try(vd17_normalized_langs_c <- tbl(con, dbplyr::in_schema("vd17", "vd17_normalized_langs_c")))

try(vd17_normalized_locations_a <- tbl(con, dbplyr::in_schema("vd17", "vd17_normalized_locations_a")))
try(vd17_normalized_locations_c <- tbl(con, dbplyr::in_schema("vd17", "vd17_normalized_locations_c")))

try(vd17_normalized_years_a <- tbl(con, dbplyr::in_schema("vd17", "vd17_normalized_years_a")))
try(vd17_normalized_years_c <- tbl(con, dbplyr::in_schema("vd17", "vd17_normalized_years_c")))

try(vd17_titles_a <- tbl(con, dbplyr::in_schema("vd17", "vd17_titles_a")))
try(vd17_titles_c <- tbl(con, dbplyr::in_schema("vd17", "vd17_titles_c")))

try(fbs_gnds_a <- tbl(con, "fbs_gnds_a"))
try(fbs_gnds_c <- tbl(con, "fbs_gnds_c"))

try(fbs_links_a <- tbl(con, "fbs_links_a"))
try(fbs_links_c <- tbl(con, "fbs_links_c"))

try(fbs_metadata_a <- tbl(con, "fbs_metadata_a"))
try(fbs_metadata_c <- tbl(con, "fbs_metadata_c"))

try(fbs_record_numbers_a <- fbs_links_a %>%
  distinct(record_number))
try(fbs_record_numbers_c <- fbs_links_c %>%
  distinct(record_number))

try(fbs_records_a <- vd17_a %>%
  inner_join(fbs_record_numbers_a))
try(fbs_records_c <- vd17_c %>%
  inner_join(fbs_record_numbers_c))

try(fbs_links_of_interest_a <- fbs_links_a %>%
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
  ))
