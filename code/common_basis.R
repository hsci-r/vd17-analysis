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

con <- get_connection()
register_tables(con, "vd17")
register_tables(con, "vd17_analysis")

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
    field_code %in% c("028A", "028B", "028C"), # FBS GND has to appear in one of these fields
    is.na(role) | !role %in% c("ctb", "dte"), # normed role has to be unknown or not one of these
    is.na(role) | !(role == "oth" & field_code == "028C" & !is.na(role2) & str_detect(role2, "^Beiträger|^Mitwirkender")), # in field 028C, if role is other, role2 should not be one of these
    is.na(role2) | !str_detect(role2, !!!str_flatten(c(
      "^AdressatIn",
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
    ), collapse = "|^"))
  ))
