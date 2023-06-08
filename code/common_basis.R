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

hyperlink_vd17_id <- function(df) {
  df %>% mutate(vd17_id = gs4_formula(str_c('=HYPERLINK("https://kxp.k10plus.de/DB=1.28/CMD?ACT=SRCHA&IKT=8079&TRM=%27', vd17_id, '%27","', vd17_id, '")')))
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

if (!exists("con")) con <- get_connection()
register_tables(con, "vd17")
register_tables(con, "vd17_analysis")

local({
  temp_tables <- list_temporary_tables(con, "vd17", "vd17_analysis")
  if (nrow(temp_tables) > 0) warning("The following temporary tables were found in the database. Use delete_temporary_tables() to remove.\n", temp_tables)
})

# Analytical set definition

fbs_rescue_records_a <- vd17_a %>%
  filter(str_detect(value, !!!str_flatten(c(
    "Palmen Orden",
    "Palmen-Orden",
    "Palmenorden",
    "Fruchtbringend",
    "Frucht-bringend",
    "Frucht Bringend",
    "Die deutsche Akademie des 17. Jahrhunderts - Fruchtbringende Gesellschaft",
    "Akademie des 17. Jahrhunderts",
    "Akademie des Siebzehnten Jahrhunderts",
    "Deutsche Akademie des Siebzehnten Jahrhunderts",
    "Deutsche Akademie des 17. Jahrhunderts",
    "Akademie des Siebzehnten Jahrhunderts",
    "Akademie des 17. Jahrhunderts",
    "Societas Fructifera",
    "Frugtbringende Selskab",
    "Fruitbearing Society",
    "Sociedad Fructífera",
    "Société des Fructifiants",
    "Ordre du Palmier",
    "Società dei Carpofori",
    "Towarzystwo Owocodajne",
    "Komunitas Fruitbearing",
    "Плодоносное общество",
    "丰收学会",
    "palmen orden",
    "palmen-orden",
    "palmenorden",
    "fruchtbringend",
    "frucht-bringend",
    "frucht bringend",
    "die deutsche akademie des 17. jahrhunderts - fruchtbringende gesellschaft",
    "akademie des 17. jahrhunderts",
    "akademie des siebzehnten jahrhunderts",
    "deutsche akademie des siebzehnten jahrhunderts",
    "deutsche akademie des 17. jahrhunderts",
    "akademie des siebzehnten jahrhunderts",
    "akademie des 17. jahrhunderts",
    "societas fructifera",
    "frugtbringende selskab",
    "fruitbearing society",
    "sociedad fructífera",
    "société des fructifiants",
    "ordre du palmier",
    "società dei carpofori",
    "towarzystwo owocodajne",
    "komunitas fruitbearing"
  ), collapse = "|")))

# Filtering down society subset

fbs_links_of_interest_a <- fbs_links_a %>%
  inner_join(vd17_person_links_a, join_by(record_number, field_number, field_code)) %>%
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
  )
