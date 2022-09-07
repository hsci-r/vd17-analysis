-   <a href="#multi-part-exploration"
    id="toc-multi-part-exploration">Multi-part exploration</a>
-   <a href="#variant-exploration" id="toc-variant-exploration">Variant
    exploration</a>

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.1 ──

    ## ✔ ggplot2 3.3.6     ✔ purrr   0.3.4
    ## ✔ tibble  3.1.8     ✔ dplyr   1.0.9
    ## ✔ tidyr   1.2.0     ✔ stringr 1.4.0
    ## ✔ readr   2.1.2     ✔ forcats 0.5.1

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

    ## here() starts at /Users/jiemakel/tyo/vd17-analysis

    ##
    ## Attaching package: 'cowplot'

    ## The following object is masked from 'package:patchwork':
    ##
    ##     align_plots

    ##
    ## Attaching package: 'ggpubr'

    ## The following object is masked from 'package:cowplot':
    ##
    ##     get_legend

    ##
    ## Attaching package: 'data.table'

    ## The following objects are masked from 'package:dplyr':
    ##
    ##     between, first, last

    ## The following object is masked from 'package:purrr':
    ##
    ##     transpose

    ## Joining, by = "record_number"
    ## Joining, by = "record_number"

## Multi-part exploration

Field 002@ (0500) - Bibliographic type and status. The 0500 field
contains encoded information about the physical form, the appearance of
the resource present, and the status of the record. In the VD 17 only
printed resources are recorded after autopsy. Subfield: $0 (none) -
Bibliographic type and status

The following codes are possible at the respective positions. The coded
information in field 0500 are given as a line without further
separators: Position 1: Physical form

-   A - Print

Position 2: Bibliographic publication type

-   a - Individual unit \[monograph\]
-   c - Record covering an entire multi-part monograph
-   f - Unit of a multi-part monograph/part of a monographic series with
    dependent titles or without title
-   F Part of a multi-part monograph with a separate/independent title

Position 3: Status

-   u - Autopsie

For example:

-   0500 Aau - Note: Printed resource, individual unit, autopsy
-   0500 Acu - Note: Printed resource, complete recording of a
    multi-part monograph, autopsy
-   0500 AFu - Note: Printed resource, part of a multi-part
    separately/independently titled monograph, autopsy
-   0500 Afu - Note: Printed resource, part of a multi-part monograph
    with dependent title, autopsy

Actual data spread in VD17:

``` r
t <- vd17_a %>%
  filter(field_code=="002@") %>%
  count(value) %>%
  arrange(desc(n))

if (is_html_output()) {
  t %>% gt(rowname_col = "value")
} else knitr::kable(t %>% collect())
```

| value |      n |
|:------|-------:|
| Aau   | 285205 |
| Afu   |  18162 |
| Acu   |   5075 |
| Aay   |     28 |
| Aaus  |     19 |
| amy   |      8 |
| AauZ  |      5 |
| Aav   |      2 |

``` r
vd17_n_records <- vd17_a %>% distinct(record_number) %>% count() %>% pull(n)
filtered_n_records <- vd17_n_records - (vd17_a %>% filter(field_code=="002@", str_detect(value, "^A[ac]")) %>% distinct(record_number) %>% count() %>% pull(n))
```

Filtering for individual units/complete recordings (`a/c`) removes
18,170 (5.89%) out of the 308,504 original records.

## Variant exploration

``` r
extents <- vd17_a %>%
  filter(field_code=="034D") %>%
  distinct(record_number,extent=value) %>%
  group_by(record_number) %>%
  dbplyr::window_order(extent) %>%
  summarize(extents=str_flatten(extent,collapse="|"),.groups="drop") %>%
  collect() %>%
  copy_to_c(con)

form_factors <- vd17_a %>%
  filter(field_code=="034I") %>%
  distinct(record_number,form_factor=value) %>%
  group_by(record_number) %>%
  dbplyr::window_order(extent) %>%
  summarize(form_factors=str_flatten(form_factor,collapse="|"),.groups="drop") %>%
  collect() %>%
  copy_to_c(con)

places_of_publication <- vd17_normalized_locations_a %>% filter(location_type=='pup') %>% select(record_number,place_of_publication=location)
places_of_production <- vd17_normalized_locations_a %>% filter(location_type=='mfp') %>% select(record_number,place_of_production=location)
places_of_distribution <- vd17_normalized_locations_a %>% filter(location_type=='dbp') %>% select(record_number,place_of_distribution=location)

unified_places_of_publication <- vd17_a %>%
  distinct(record_number) %>%
  left_join(places_of_publication) %>%
  left_join(places_of_production) %>%
  left_join(places_of_distribution) %>%
  mutate(place_of_publication=coalesce(place_of_publication,place_of_production,place_of_distribution)) %>%
  filter(!is.na(place_of_publication)) %>%
  distinct(record_number,place_of_publication) %>%
  group_by(record_number) %>%
  dbplyr::window_order(place_of_publication) %>%
  summarize(places_of_publication=str_flatten(place_of_publication,collapse="|"),.groups="drop") %>%
  collect() %>%
  copy_to_c(con)
```

    ## Joining, by = "record_number"
    ## Joining, by = "record_number"
    ## Joining, by = "record_number"

``` r
rda_printers <- vd17_a %>%
  filter(field_code=="029F",subfield_code=="4",value=="prt") %>%
  select(record_number,field_number) %>%
  inner_join(vd17_a %>% filter(subfield_code=="7")) %>%
  select(record_number,printer_gnd=value)
```

    ## Joining, by = c("record_number", "field_number")

``` r
printers <- vd17_a %>%
  filter(field_code=="033J",subfield_code=="7") %>%
  select(record_number,printer_gnd=value)
additional_printers <- vd17_a %>%
  filter(field_code=="028C",(subfield_code=="4" & value=="prt") | (subfield_code=="B" & str_detect(value,"^Drucker|^Verleger"))) %>%
  distinct(record_number,field_number) %>%
  inner_join(vd17_a %>% filter(subfield_code=="7")) %>%
  select(record_number,printer_gnd=value)
```

    ## Joining, by = c("record_number", "field_number")

``` r
all_printers <- rda_printers %>%
  union_all(printers) %>%
  union_all(additional_printers) %>%
  distinct(record_number,printer_gnd) %>%
  group_by(record_number) %>%
  dbplyr::window_order(printer_gnd) %>%
  summarize(printer_gnds=str_flatten(printer_gnd,collapse="|"),.groups="drop") %>%
  collect() %>%
  copy_to_c(con)
```

``` r
vd17_identical_titles <- vd17_titles_c %>%
  select(record_number,title) %>%
  inner_join(vd17_titles_c %>% select(record_number,title),sql_on="LHS.title=RHS.title AND LHS.record_number<RHS.record_number") %>%
  select(record_number.x,record_number.y, title=title.x) %>%
  compute_a()

identical_title_removal_removes_n =
  vd17_identical_titles %>%
  select(record_number=record_number.x) %>%
  union_all(
    vd17_identical_titles %>%
    select(record_number=record_number.y)
  ) %>%
  distinct() %>%
  count() %>%
  pull(n)  - (vd17_identical_titles %>% distinct(title) %>% count() %>% pull(n))

t <- vd17_titles_c %>%
  select(record_number,title) %>%
  inner_join(vd17_normalized_years_c %>% select(record_number,normalized_year))
```

    ## Joining, by = "record_number"

``` r
vd17_identical_titles_and_years <- t %>%
  inner_join(t,sql_on="LHS.title=RHS.title AND LHS.normalized_year=RHS.normalized_year AND LHS.record_number<RHS.record_number") %>%
  select(record_number.x,record_number.y, title=title.x, normalized_year=normalized_year.x) %>%
  compute_a()

identical_title_and_year_removal_removes_n =
  vd17_identical_titles_and_years %>%
  select(record_number=record_number.x) %>%
  union_all(
    vd17_identical_titles_and_years %>%
    select(record_number=record_number.y)
  ) %>%
  distinct() %>%
  count() %>%
  pull(n)  - (vd17_identical_titles_and_years %>% distinct(title,normalized_year) %>% count() %>% pull(n))

t <- vd17_titles_c %>%
  select(record_number,title) %>%
  inner_join(vd17_normalized_years_c %>% select(record_number,normalized_year)) %>%
  inner_join(unified_places_of_publication)
```

    ## Joining, by = "record_number"
    ## Joining, by = "record_number"

``` r
vd17_identical_titles_and_years_and_places_of_publication <- t %>%
  inner_join(t,sql_on="LHS.title=RHS.title AND LHS.normalized_year=RHS.normalized_year AND LHS.places_of_publication=RHS.places_of_publication AND LHS.record_number<RHS.record_number") %>%
  select(record_number.x,record_number.y, title=title.x, normalized_year=normalized_year.x, places_of_publication=places_of_publication.x) %>%
  compute_a()

identical_title_and_year_and_place_of_publication_removal_removes_n =
  vd17_identical_titles_and_years_and_places_of_publication %>%
  select(record_number=record_number.x) %>%
  union_all(
    vd17_identical_titles_and_years_and_places_of_publication %>%
    select(record_number=record_number.y)
  ) %>%
  distinct() %>%
  count() %>%
  pull(n)  - (vd17_identical_titles_and_years_and_places_of_publication %>% distinct(title,normalized_year,places_of_publication) %>% count() %>% pull(n))

t <- vd17_titles_c %>%
  select(record_number,title) %>%
  inner_join(vd17_normalized_years_c %>% select(record_number,normalized_year)) %>%
  inner_join(all_printers)
```

    ## Joining, by = "record_number"
    ## Joining, by = "record_number"

``` r
vd17_identical_titles_and_years_and_printers <- t %>%
  inner_join(t,sql_on="LHS.title=RHS.title AND LHS.normalized_year=RHS.normalized_year AND LHS.printer_gnds=RHS.printer_gnds AND LHS.record_number<RHS.record_number") %>%
  select(record_number.x,record_number.y, title=title.x, normalized_year=normalized_year.x, printer_gnds=printer_gnds.x) %>%
  compute_a()

identical_title_and_year_and_printer_removal_removes_n =
  vd17_identical_titles_and_years_and_printers %>%
  select(record_number=record_number.x) %>%
  union_all(
    vd17_identical_titles_and_years_and_printers %>%
    select(record_number=record_number.y)
  ) %>%
  distinct() %>%
  count() %>%
  pull(n)  - (vd17_identical_titles_and_years_and_printers %>% distinct(title,normalized_year,printer_gnds) %>% count() %>% pull(n))

t <- vd17_titles_c %>%
  select(record_number,title) %>%
  inner_join(vd17_normalized_years_c %>% select(record_number,normalized_year)) %>%
  inner_join(unified_places_of_publication) %>%
  inner_join(all_printers)
```

    ## Joining, by = "record_number"
    ## Joining, by = "record_number"
    ## Joining, by = "record_number"

``` r
vd17_identical_titles_and_years_and_places_of_publication_and_printers <- t %>%
  inner_join(t,sql_on="LHS.title=RHS.title AND LHS.normalized_year=RHS.normalized_year AND LHS.places_of_publication=RHS.places_of_publication AND LHS.printer_gnds=RHS.printer_gnds AND LHS.record_number<RHS.record_number") %>%
  select(record_number.x,record_number.y, title=title.x, normalized_year=normalized_year.x, places_of_publication=places_of_publication.x, printer_gnds=printer_gnds.x) %>%
  compute_a()

identical_title_and_year_and_place_of_publication_and_printer_removal_removes_n =
  vd17_identical_titles_and_years_and_places_of_publication_and_printers %>%
  select(record_number=record_number.x) %>%
  union_all(
    vd17_identical_titles_and_years_and_places_of_publication_and_printers %>%
    select(record_number=record_number.y)
  ) %>%
  distinct() %>%
  count() %>%
  pull(n)  - (vd17_identical_titles_and_years_and_places_of_publication_and_printers %>% distinct(title,normalized_year,places_of_publication,printer_gnds) %>% count() %>% pull(n))

t <- vd17_titles_c %>%
  select(record_number,title) %>%
  inner_join(vd17_normalized_years_c %>% select(record_number,normalized_year)) %>%
  inner_join(unified_places_of_publication) %>%
  inner_join(all_printers) %>%
  inner_join(extents) %>%
  inner_join(form_factors)
```

    ## Joining, by = "record_number"
    ## Joining, by = "record_number"
    ## Joining, by = "record_number"
    ## Joining, by = "record_number"
    ## Joining, by = "record_number"

``` r
vd17_identical_all <- t %>%
  inner_join(t,sql_on="LHS.title=RHS.title AND LHS.normalized_year=RHS.normalized_year AND LHS.places_of_publication=RHS.places_of_publication AND LHS.printer_gnds=RHS.printer_gnds AND LHS.extents=RHS.extents AND LHS.form_factors=RHS.form_factors AND LHS.record_number<RHS.record_number") %>%
  select(record_number.x,record_number.y, title=title.x, normalized_year=normalized_year.x, places_of_publication=places_of_publication.x, printer_gnds=printer_gnds.x, extents=extents.x, form_factors=form_factors.x) %>%
  compute_a()

identical_all_removal_removes_n =
  vd17_identical_all %>%
  select(record_number=record_number.x) %>%
  union_all(
    vd17_identical_all %>%
    select(record_number=record_number.y)
  ) %>%
  distinct() %>%
  count() %>%
  pull(n)  - (vd17_identical_all %>% distinct(title,normalized_year,places_of_publication,printer_gnds,extents,form_factors) %>% count() %>% pull(n))
```

Filtering out records with identical:

-   titles would remove 36,310 (11.77%) records out of the 308,504
    original records.
-   titles and publication years would remove 20,465 (6.63%) records out
    of the 308,504 original records.
-   titles, publication years and places of publication would remove
    16,991 (5.51%) records out of the 308,504 original records.
-   titles, publication years and printers would remove 15,141 (4.91%)
    records out of the 308,504 original records.
-   titles, publication years, places of publication and printers would
    remove 14,900 (4.83%) records out of the 308,504 original records.
-   titles, publication years, places of publication, printers, extents
    and form factors would remove 7,575 (2.46%) records out of the
    308,504 original records.
