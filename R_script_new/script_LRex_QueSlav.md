R Notebook for naturalness/acceptability judgment experiments
================
Masha Onoeva
2024-05-07

- [1. Loading data](#1-loading-data)
- [2. Fillers and unreliable
  participants](#2-fillers-and-unreliable-participants)
- [3. Data sets](#3-data-sets)

### Info

Hi! This R notebook describes the steps that are required for the
analysis of naturalness/acceptability judgment linguistic experiments. I
use the experiment that I did with Radek Šimík testing Russian negated
polar questions in different contexts as the example data (see [Onoeva
and Šimík
2023](https://mariaonoeva.github.io/assets/pdf/FDSL16_RuNPQs_Onoeva_Simik.pdf)).
The experiment was run on [LRex](https://www.l-rex.de/), so your raw
data can be different if you use a different platform.

### Files

The csv file is available in this repo. There are also two files with
the script – rmd and md. The first one is a raw RMarkdown script from
RStudio, the second one is a pretty version for GitHub, which is easier
to follow online.

## 1. Loading data

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

``` r
library(formattable) # for pretty tables 
```

Here I’m setting the working directory and loading data. There is an
option to download a version without abandoned trials from LRex and I
load it here. Then I remove the unnecessary example items.

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
main_df <- all_df %>%
  filter(materials != "1_examples")
```

## 2. Fillers and unreliable participants

You can see the number of participants on LRex but just to double-check
it I’ll run a couple of lines here as well.

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

``` r
# summarizes items for all participants 
main_df %>%
  group_by(participant) %>%
  summarize(items = n())
```

    ## # A tibble: 95 × 2
    ##    participant items
    ##          <dbl> <int>
    ##  1           1    82
    ##  2           2    82
    ##  3           3    82
    ##  4           4    82
    ##  5           5    82
    ##  6           6    82
    ##  7           7    82
    ##  8           8    82
    ##  9           9    82
    ## 10          10    82
    ## # ℹ 85 more rows

The next step is to extract all filler items. These items are used to
test reliability of the participants. Here I have 10 such items, people
had to assess 3 items as ‘bad’ or 1-4 and the rest as ‘good’ 5-7.

``` r
# creating a new df with the filler items only 
fillers_only <- main_df %>%
  filter(materials == "f9_filler")

# creating a new column for checking if fillers are good or not 
fillers_only$filler_answer <- 0 
fillers_only$filler_answer <- as.numeric(fillers_only$filler_answer)

# rename filler items: the first three items were bad, the rest were good
fillers_only$condition[fillers_only$item %in% c("1", "2", "3")] <- 'bad'
fillers_only$condition[fillers_only$condition != "bad"] <- 'good'
```

Now I’m assigning ‘1’ to filler_answer column if these filler items were
assessed correctly. In other words, if bad fillers have 1-4 and good
5-7. If these conditions are not met, the value remains ‘0’.

``` r
# bad fillers
fillers_only$filler_answer[which(grepl('bad', fillers_only$condition) &
                                     grepl('1|2|3', fillers_only$rating1))] <- 1

# good fillers 
fillers_only$filler_answer[which(grepl('good', fillers_only$condition) &
                                     grepl('5|6|7', fillers_only$rating1))] <- 1
```

So now I have the column which tells me how each participant assessed
each filler item. I can measure their reliability simply by counting how
good they were in the fillers. In the table below, the first participant
has mean ‘1’, so they have assessed all fillers correctly as I expected
them. If mean is lower than 80 %, a participant is unreliable.

``` r
filler_results <- fillers_only %>% 
  group_by(participant) %>%
  summarize(Mean = mean(filler_answer, na.rm=TRUE))

# how in general the participants went through fillers 
mean(filler_results$Mean)
```

    ## [1] 0.8442105

Now I need to find unreliable participants. This is done quickly, just
to find the people who have means lower than 0.8. This is quite a high
threshold but 68 participants is still fine.

``` r
unreliable_participants <- filler_results %>% 
  filter(Mean < 0.8) # I have 27 unreliable participants
```

Then I remove unreliable participants from reliable ones.

``` r
fillers_only_reliable <- anti_join(filler_results, unreliable_participants, 
                          by = "participant")

mean(fillers_only_reliable$Mean) # testing by applying mean to the reliable df
```

    ## [1] 0.9235294

## 3. Data sets

In this experiment, we had one big experiment and several smaller, see
summary of the materilas column. I’m going to separate them into several
data frames. But first, it’s necessary to remove the filler items.

``` r
main_df %>%
  group_by(materials) %>%
  summarise()
```

    ## # A tibble: 10 × 1
    ##    materials      
    ##    <chr>          
    ##  1 e1_main        
    ##  2 f1_nibud       
    ##  3 f2_razve_pos   
    ##  4 f3_razve_neg   
    ##  5 f4_slucajno_neg
    ##  6 f5_slucajno_pos
    ##  7 f6_ctoli_neg   
    ##  8 f7_ctoli_pos   
    ##  9 f8_repetitive  
    ## 10 f9_filler

``` r
main_df1 <- main_df %>%
  filter(materials != "f9_filler")

formattable(main_df1 %>% # no filler in the summary 
  group_by(materials) %>%
  summarise())
```

<table class="table table-condensed">
<thead>
<tr>
<th style="text-align:right;">
materials
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:right;">
e1_main
</td>
</tr>
<tr>
<td style="text-align:right;">
f1_nibud
</td>
</tr>
<tr>
<td style="text-align:right;">
f2_razve_pos
</td>
</tr>
<tr>
<td style="text-align:right;">
f3_razve_neg
</td>
</tr>
<tr>
<td style="text-align:right;">
f4_slucajno_neg
</td>
</tr>
<tr>
<td style="text-align:right;">
f5_slucajno_pos
</td>
</tr>
<tr>
<td style="text-align:right;">
f6_ctoli_neg
</td>
</tr>
<tr>
<td style="text-align:right;">
f7_ctoli_pos
</td>
</tr>
<tr>
<td style="text-align:right;">
f8_repetitive
</td>
</tr>
</tbody>
</table>
