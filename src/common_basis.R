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
