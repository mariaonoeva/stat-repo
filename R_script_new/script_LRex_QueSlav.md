R Notebook for naturalness/acceptability judgments linguistic
experiments
================
Masha Onoeva
2024-05-07

Hi! This is an R notebook created by Masha Onoeva (me). It describes the
steps that are required for the analysis of naturalness/acceptability
judgments linguistic experiments. I use the experiment that I did with
Radek Šimík testing Russian negated polar questions in different
contexts as the example data. The experiment was run on LRex, so your
raw data can be different if you use a different platform.

``` r
library(tidyverse) # THE package, it contains ggplot2, tidyr, dplyr, readr and more
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.4
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   3.4.4     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.3     ✔ tidyr     1.3.0
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

## 1. Loading data

Setting the working directory and loading data. There is an option to
download the version without abandoned trials and I load it here. Then I
remove the unnecessary example items.

``` r
# setting the dir
setwd("/Users/maria.onoeva/Desktop/new_folder/GitHub/stat-repo/R_script_new")

# loading all data
all_df <-
  read_delim("queslav_neg_mo_RESULTS_2023-03-06-0953_noaband.csv", ";",
                                             escape_double = FALSE,
                                             trim_ws = TRUE,
                                             show_col_types = FALSE)

# removing example items
main_df <- all_df[-which(all_df$materials=="1_examples"),]
```

## 2. Fillers and unreliable participants

``` r
# counts all participants 
main_df %>%
  distinct(participant) %>%
  summarize(total_part = n())
```

    ## # A tibble: 1 × 1
    ##   total_part
    ##        <int>
    ## 1         95
