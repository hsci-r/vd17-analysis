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
library(ggbeeswarm)
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

summarise_people_by_record <- function(pdf) {
  pdf %>%
    mutate(combined = str_c(field_code, ":", role2, ": ", combined_name)) %>%
    select(record_number, combined) %>%
    group_by(record_number) %>%
    summarise(contributors = str_flatten(combined, collapse = "|")) %>%
    compute_a(unique_indexes = list(c("record_number")))
}

add_metadata_fields <- function(df) {
  df %>%
    left_join(vd17_id_a) %>%
    left_join(vd17_titles_a %>% select(record_number, combined_title) %>% group_by(record_number) %>% summarise(combined_title = str_flatten(combined_title, collapse = "|")), join_by(record_number)) %>%
    left_join(vd17_normalized_years_a %>% group_by(record_number) %>% summarise(publication_date = str_flatten(a, collapse = "|"))) %>%
    left_join(vd17_normalized_langs_a %>% mutate(languages = str_c(publication_language, intermediary_language, original_language, sep = "<-")) %>% group_by(record_number) %>% summarize(languages = str_flatten(languages, collapse = "|")), join_by(record_number)) %>%
    left_join(vd17_genres_a %>% distinct(record_number, genre) %>% group_by(record_number) %>% summarize(genre = str_flatten(genre, collapse = "|")), join_by(record_number)) %>%
    compute_a(indexes = list(c("record_number"), c("vd17_id"))) %>%
    left_join(
      summarise_people_by_record(vd17_person_links_a %>%
        inner_join(fbs_links_of_interest_a %>%
          select(record_number, field_number, member_number)) %>%
        mutate(combined_name = str_c(combined_name, " (", member_number, ")")))
    ) %>%
    rename(fbs_contributors = contributors) %>%
    left_join(
      summarise_people_by_record(vd17_person_links_a %>%
        inner_join(fbs_links_a %>%
          anti_join(fbs_links_of_interest_a %>%
            select(record_number, field_number)) %>%
          select(record_number, field_number, member_number)) %>%
        mutate(combined_name = str_c(combined_name, " (", member_number, ")")))
    ) %>%
    rename(fbs_associated = contributors) %>%
    left_join(
      summarise_people_by_record(vd17_person_links_a %>%
        anti_join(fbs_links_a %>% select(record_number, field_number)) %>%
        select(record_number, field_code, role2, combined_name) %>%
        union_all(vd17_corporate_links_a %>%
          select(record_number, field_code, role2, combined_name)))
    ) %>%
    rename(other_contributors = contributors)
}

prepare_for_gsheets <- function(df) {
  df %>%
    relocate(vd17_id, fbs_contributors, fbs_associated, other_contributors, combined_title, languages, genre) %>%
    hyperlink_vd17_id()
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
