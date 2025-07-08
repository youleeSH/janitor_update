
<!-- README.md is generated from README.Rmd. Please edit that file -->

# janitor <img src="man/figures/logo_small.png" align="right" />

> Data scientists, according to interviews and expert estimates, spend
> from 50 percent to 80 percent of their time mired in this more mundane
> labor of collecting and preparing unruly digital data, before it can
> be explored for useful nuggets.
>
> – [“For Big-Data Scientists, ‘Janitor Work’ Is Key Hurdle to
> Insight”](https://www.nytimes.com/2014/08/18/technology/for-big-data-scientists-hurdle-to-insights-is-janitor-work.html)
> *(New York Times, 2014)*

------------------------------------------------------------------------

<!-- badges: start -->

[![R-CMD-check](https://github.com/sfirke/janitor/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/sfirke/janitor/actions/workflows/R-CMD-check.yaml)
[![Coverage
Status](https://img.shields.io/codecov/c/github/sfirke/janitor/main.svg)](https://app.codecov.io/github/sfirke/janitor?branch=main)
[![lifecycle](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version-ago/janitor)](https://cran.r-project.org/package=janitor)
![!Monthly Downloads](https://cranlogs.r-pkg.org/badges/janitor)
![!Downloads](https://cranlogs.r-pkg.org/badges/grand-total/janitor)
<!-- badges: end -->

**janitor** has simple functions for examining and cleaning dirty data.
It was built with beginning and intermediate R users in mind and is
optimized for user-friendliness. Advanced R users can perform many of
these tasks already, but with janitor they can do it faster and save
their thinking for the fun stuff.

The main janitor functions:

- perfectly format data.frame column names;
- create and format frequency tables of one, two, or three variables -
  think an improved `table()`; and
- provide other tools for cleaning and examining data.frames.

The tabulate-and-report functions approximate popular features of SPSS
and Microsoft Excel.

janitor is a
[\#tidyverse](https://cran.r-project.org/package=tidyverse/vignettes/manifesto.html)-oriented
package. Specifically, it plays nicely with the `%>%` pipe and is
optimized for cleaning data brought in with the
[readr](https://github.com/tidyverse/readr) and
[readxl](https://github.com/tidyverse/readxl) packages.

## <i class="fa fa-cog" aria-hidden="true"></i> Installation

You can install:

- the most recent officially-released version from CRAN with

``` r
install.packages("janitor")
```

- the latest development version from GitHub with

``` r
# install.packages("remotes")
remotes::install_github("sfirke/janitor")
# or from r-universe
install.packages("janitor", repos = c("https://sfirke.r-universe.dev", "https://cloud.r-project.org"))
```

## Using janitor

A full description of each function, organized by topic, can be found in
janitor’s [catalog of functions
vignette](https://sfirke.github.io/janitor/articles/janitor.html). There
you will find functions not mentioned in this README, like
`compare_df_cols()` which provides a summary of differences in column
names and types when given a set of data.frames.

Below are quick examples of how janitor tools are commonly used.

## Full Reference Manual (CRAN)

For a complete list of functions with usage and examples, refer to the
official CRAN reference manual:  
[janitor.pdf](https://cran.r-project.org/web/packages/janitor/janitor.pdf)

### Cleaning dirty data

Take this roster of teachers at a fictional American high school, stored
in the Microsoft Excel file
[dirty_data.xlsx](https://github.com/sfirke/janitor/blob/main/dirty_data.xlsx):
![All kinds of dirty.](man/figures/dirty_data.PNG)

Dirtiness includes:

- A header at the top
- Dreadful column names
- Rows and columns containing Excel formatting but no data
- Dates in two different formats in a single column (MM/DD/YYYY and
  numbers)
- Values spread inconsistently over the “Certification” columns

Here’s that data after being read in to R:

``` r
library(readxl)
library(janitor)
library(dplyr)
library(here)

roster_raw <- read_excel(here("dirty_data.xlsx")) # available at https://github.com/sfirke/janitor
glimpse(roster_raw)
#> Rows: 14
#> Columns: 11
#> $ `Data most recently refreshed on:` <chr> "First Name", "Jason", "Jason", "Alicia", "Ada", "Desus", "Chien-…
#> $ ...2                               <chr> "Last Name", "Bourne", "Bourne", "Keys", "Lovelace", "Nice", "Wu"…
#> $ ...3                               <chr> "Employee Status", "Teacher", "Teacher", "Teacher", "Teacher", "A…
#> $ `Dec-27 2020`                      <chr> "Subject", "PE", "Drafting", "Music", NA, "Dean", "Physics", "Che…
#> $ ...5                               <chr> "Hire Date", "39690", "43479", "37118", "38572", "42791", "11037"…
#> $ ...6                               <chr> "% Allocated", "0.75", "0.25", "1", "1", "1", "0.5", "0.5", NA, "…
#> $ ...7                               <chr> "Full time?", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", NA…
#> $ ...8                               <chr> "do not edit! --->", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …
#> $ ...9                               <chr> "Certification", "Physical ed", "Physical ed", "Instr. music", "P…
#> $ ...10                              <chr> "Certification", "Theater", "Theater", "Vocal music", "Computers"…
#> $ ...11                              <chr> "Active?", "YES", "YES", "YES", "YES", "YES", "YES", "YES", NA, "…
```

Now, to clean it up, starting with the column names.

Name cleaning comes in two flavors. `make_clean_names()` operates on
character vectors and can be used during data import:

``` r
roster_raw_cleaner <- read_excel(here("dirty_data.xlsx"),
  skip = 1,
  .name_repair = make_clean_names
)
glimpse(roster_raw_cleaner)
#> Rows: 13
#> Columns: 11
#> $ first_name        <chr> "Jason", "Jason", "Alicia", "Ada", "Desus", "Chien-Shiung", "Chien-Shiung", NA, "J…
#> $ last_name         <chr> "Bourne", "Bourne", "Keys", "Lovelace", "Nice", "Wu", "Wu", NA, "Joyce", "Lamarr",…
#> $ employee_status   <chr> "Teacher", "Teacher", "Teacher", "Teacher", "Administration", "Teacher", "Teacher"…
#> $ subject           <chr> "PE", "Drafting", "Music", NA, "Dean", "Physics", "Chemistry", NA, "English", "Sci…
#> $ hire_date         <dbl> 39690, 43479, 37118, 38572, 42791, 11037, 11037, NA, 36423, 27919, 42221, 34700, 4…
#> $ percent_allocated <dbl> 0.75, 0.25, 1.00, 1.00, 1.00, 0.50, 0.50, NA, 0.50, 0.50, NA, NA, 0.80
#> $ full_time         <chr> "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", NA, "No", "No", "No", "No", "No"
#> $ do_not_edit       <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA
#> $ certification     <chr> "Physical ed", "Physical ed", "Instr. music", "PENDING", "PENDING", "Science 6-12"…
#> $ certification_2   <chr> "Theater", "Theater", "Vocal music", "Computers", NA, "Physics", "Physics", NA, "E…
#> $ active            <chr> "YES", "YES", "YES", "YES", "YES", "YES", "YES", NA, "YES", "YES", "YES", "YES", "…
```

`clean_names()` is a convenience version of `make_clean_names()` that
can be used for piped data.frame workflows. The equivalent steps with
`clean_names()` would be:

``` r
roster_raw <- roster_raw %>%
  row_to_names(row_number = 1) %>%
  clean_names()
```

The data.frame now has clean names. Let’s tidy it up further:

``` r
roster <- roster_raw %>%
  remove_empty(c("rows", "cols")) %>%
  remove_constant(na.rm = TRUE, quiet = FALSE) %>% # remove the column of all "Yes" values
  mutate(
    hire_date = convert_to_date(
      hire_date, # handle the mixed-format dates
      character_fun = lubridate::mdy
    ),
    cert = dplyr::coalesce(certification, certification_2)
  ) %>%
  select(-certification, -certification_2) # drop unwanted columns
#> Removing 1 constant columns of 10 columns total (Removed: active).

roster
#> # A tibble: 12 × 8
#>    first_name   last_name employee_status subject    hire_date  percent_allocated full_time cert          
#>    <chr>        <chr>     <chr>           <chr>      <date>     <chr>             <chr>     <chr>         
#>  1 Jason        Bourne    Teacher         PE         2008-08-30 0.75              Yes       Physical ed   
#>  2 Jason        Bourne    Teacher         Drafting   2019-01-14 0.25              Yes       Physical ed   
#>  3 Alicia       Keys      Teacher         Music      2001-08-15 1                 Yes       Instr. music  
#>  4 Ada          Lovelace  Teacher         <NA>       2005-08-08 1                 Yes       PENDING       
#>  5 Desus        Nice      Administration  Dean       2017-02-25 1                 Yes       PENDING       
#>  6 Chien-Shiung Wu        Teacher         Physics    1930-03-20 0.5               Yes       Science 6-12  
#>  7 Chien-Shiung Wu        Teacher         Chemistry  1930-03-20 0.5               Yes       Science 6-12  
#>  8 James        Joyce     Teacher         English    1999-09-20 0.5               No        English 6-12  
#>  9 Hedy         Lamarr    Teacher         Science    1976-06-08 0.5               No        PENDING       
#> 10 Carlos       Boozer    Coach           Basketball 2015-08-05 <NA>              No        Physical ed   
#> 11 Young        Boozer    Coach           <NA>       1995-01-01 <NA>              No        Political sci.
#> 12 Micheal      Larsen    Teacher         English    2009-09-15 0.8               No        Vocal music
```

### Examining dirty data

#### Finding duplicates

Use `get_dupes()` to identify and examine duplicate records during data
cleaning. Let’s see if any teachers are listed more than once:

``` r
roster %>% get_dupes(contains("name"))
#> # A tibble: 4 × 9
#>   first_name   last_name dupe_count employee_status subject   hire_date  percent_allocated full_time cert     
#>   <chr>        <chr>          <int> <chr>           <chr>     <date>     <chr>             <chr>     <chr>    
#> 1 Chien-Shiung Wu                 2 Teacher         Physics   1930-03-20 0.5               Yes       Science …
#> 2 Chien-Shiung Wu                 2 Teacher         Chemistry 1930-03-20 0.5               Yes       Science …
#> 3 Jason        Bourne             2 Teacher         PE        2008-08-30 0.75              Yes       Physical…
#> 4 Jason        Bourne             2 Teacher         Drafting  2019-01-14 0.25              Yes       Physical…
```

Yes, some teachers appear twice. We ought to address this before
counting employees.

#### Tabulating tools

A variable (or combinations of two variables) can be tabulated with
`tabyl()`. For three or more variables, use `tabyl_nway()` which returns
a tidy data.frame of counts for all variable combinations.

The resulting data.frame can be tweaked and formatted with the suite of
`adorn_` functions for quick analysis and printing of pretty results in
a report. `adorn_` functions can be helpful with non-tabyls, too.

#### `tabyl()` and `tabyl_nway()`

Like `table()`, but pipe-able, data.frame-based, and fully featured.

`tabyl()` can be called two ways:

- On a vector, when tabulating a single variable:
  `tabyl(roster$subject)`
- On a data.frame, specifying 1 or 2 variable names to tabulate:
  `roster %>% tabyl(subject, employee_status)`.
  - Here the data.frame is passed in with the `%>%` pipe; this allows
    `tabyl` to be used in an analysis pipeline

One variable:

``` r
roster %>%
  tabyl(subject)
#>     subject n    percent valid_percent
#>  Basketball 1 0.08333333           0.1
#>   Chemistry 1 0.08333333           0.1
#>        Dean 1 0.08333333           0.1
#>    Drafting 1 0.08333333           0.1
#>     English 2 0.16666667           0.2
#>       Music 1 0.08333333           0.1
#>          PE 1 0.08333333           0.1
#>     Physics 1 0.08333333           0.1
#>     Science 1 0.08333333           0.1
#>        <NA> 2 0.16666667            NA
```

Two variables:

``` r
roster %>%
  filter(hire_date > as.Date("1950-01-01")) %>%
  tabyl(employee_status, full_time)
#>  employee_status No Yes
#>   Administration  0   1
#>            Coach  2   0
#>          Teacher  3   4
```

Three or more variables (n-way):

``` r
# tabyl_nway returns a tidy data.frame of all combinations and their frequencies.
tabyl_nway(roster, full_time, subject, employee_status)
#>    full_time    subject employee_status n
#> 1         No Basketball  Administration 0
#> 2         No Basketball           Coach 1
#> 3         No Basketball         Teacher 0
#> 4         No  Chemistry  Administration 0
#> 5         No  Chemistry           Coach 0
#> 6         No  Chemistry         Teacher 0
#> 7         No       Dean  Administration 0
#> 8         No       Dean           Coach 0
#> 9         No       Dean         Teacher 0
#> 10        No   Drafting  Administration 0
#> 11        No   Drafting           Coach 0
#> 12        No   Drafting         Teacher 0
#> 13        No    English  Administration 0
#> 14        No    English           Coach 0
#> 15        No    English         Teacher 2
#> 16        No      Music  Administration 0
#> 17        No      Music           Coach 0
#> 18        No      Music         Teacher 0
#> 19        No         PE  Administration 0
#> 20        No         PE           Coach 0
#> 21        No         PE         Teacher 0
#> 22        No    Physics  Administration 0
#> 23        No    Physics           Coach 0
#> 24        No    Physics         Teacher 0
#> 25        No    Science  Administration 0
#> 26        No    Science           Coach 0
#> 27        No    Science         Teacher 1
#> 28        No       <NA>  Administration 0
#> 29        No       <NA>           Coach 1
#> 30        No       <NA>         Teacher 0
#> 31       Yes Basketball  Administration 0
#> 32       Yes Basketball           Coach 0
#> 33       Yes Basketball         Teacher 0
#> 34       Yes  Chemistry  Administration 0
#> 35       Yes  Chemistry           Coach 0
#> 36       Yes  Chemistry         Teacher 1
#> 37       Yes       Dean  Administration 1
#> 38       Yes       Dean           Coach 0
#> 39       Yes       Dean         Teacher 0
#> 40       Yes   Drafting  Administration 0
#> 41       Yes   Drafting           Coach 0
#> 42       Yes   Drafting         Teacher 1
#> 43       Yes    English  Administration 0
#> 44       Yes    English           Coach 0
#> 45       Yes    English         Teacher 0
#> 46       Yes      Music  Administration 0
#> 47       Yes      Music           Coach 0
#> 48       Yes      Music         Teacher 1
#> 49       Yes         PE  Administration 0
#> 50       Yes         PE           Coach 0
#> 51       Yes         PE         Teacher 1
#> 52       Yes    Physics  Administration 0
#> 53       Yes    Physics           Coach 0
#> 54       Yes    Physics         Teacher 1
#> 55       Yes    Science  Administration 0
#> 56       Yes    Science           Coach 0
#> 57       Yes    Science         Teacher 0
#> 58       Yes       <NA>  Administration 0
#> 59       Yes       <NA>           Coach 0
#> 60       Yes       <NA>         Teacher 1

# For four or more variables:
tabyl_nway(roster, full_time, subject, employee_status, cert)# tabyl_nway returns a tidy data.frame of all combinations and their frequencies.
#>     full_time    subject employee_status           cert n
#> 1          No Basketball  Administration   English 6-12 0
#> 2          No Basketball  Administration   Instr. music 0
#> 3          No Basketball  Administration        PENDING 0
#> 4          No Basketball  Administration    Physical ed 0
#> 5          No Basketball  Administration Political sci. 0
#> 6          No Basketball  Administration   Science 6-12 0
#> 7          No Basketball  Administration    Vocal music 0
#> 8          No Basketball           Coach   English 6-12 0
#> 9          No Basketball           Coach   Instr. music 0
#> 10         No Basketball           Coach        PENDING 0
#> 11         No Basketball           Coach    Physical ed 1
#> 12         No Basketball           Coach Political sci. 0
#> 13         No Basketball           Coach   Science 6-12 0
#> 14         No Basketball           Coach    Vocal music 0
#> 15         No Basketball         Teacher   English 6-12 0
#> 16         No Basketball         Teacher   Instr. music 0
#> 17         No Basketball         Teacher        PENDING 0
#> 18         No Basketball         Teacher    Physical ed 0
#> 19         No Basketball         Teacher Political sci. 0
#> 20         No Basketball         Teacher   Science 6-12 0
#> 21         No Basketball         Teacher    Vocal music 0
#> 22         No  Chemistry  Administration   English 6-12 0
#> 23         No  Chemistry  Administration   Instr. music 0
#> 24         No  Chemistry  Administration        PENDING 0
#> 25         No  Chemistry  Administration    Physical ed 0
#> 26         No  Chemistry  Administration Political sci. 0
#> 27         No  Chemistry  Administration   Science 6-12 0
#> 28         No  Chemistry  Administration    Vocal music 0
#> 29         No  Chemistry           Coach   English 6-12 0
#> 30         No  Chemistry           Coach   Instr. music 0
#> 31         No  Chemistry           Coach        PENDING 0
#> 32         No  Chemistry           Coach    Physical ed 0
#> 33         No  Chemistry           Coach Political sci. 0
#> 34         No  Chemistry           Coach   Science 6-12 0
#> 35         No  Chemistry           Coach    Vocal music 0
#> 36         No  Chemistry         Teacher   English 6-12 0
#> 37         No  Chemistry         Teacher   Instr. music 0
#> 38         No  Chemistry         Teacher        PENDING 0
#> 39         No  Chemistry         Teacher    Physical ed 0
#> 40         No  Chemistry         Teacher Political sci. 0
#> 41         No  Chemistry         Teacher   Science 6-12 0
#> 42         No  Chemistry         Teacher    Vocal music 0
#> 43         No       Dean  Administration   English 6-12 0
#> 44         No       Dean  Administration   Instr. music 0
#> 45         No       Dean  Administration        PENDING 0
#> 46         No       Dean  Administration    Physical ed 0
#> 47         No       Dean  Administration Political sci. 0
#> 48         No       Dean  Administration   Science 6-12 0
#> 49         No       Dean  Administration    Vocal music 0
#> 50         No       Dean           Coach   English 6-12 0
#> 51         No       Dean           Coach   Instr. music 0
#> 52         No       Dean           Coach        PENDING 0
#> 53         No       Dean           Coach    Physical ed 0
#> 54         No       Dean           Coach Political sci. 0
#> 55         No       Dean           Coach   Science 6-12 0
#> 56         No       Dean           Coach    Vocal music 0
#> 57         No       Dean         Teacher   English 6-12 0
#> 58         No       Dean         Teacher   Instr. music 0
#> 59         No       Dean         Teacher        PENDING 0
#> 60         No       Dean         Teacher    Physical ed 0
#> 61         No       Dean         Teacher Political sci. 0
#> 62         No       Dean         Teacher   Science 6-12 0
#> 63         No       Dean         Teacher    Vocal music 0
#> 64         No   Drafting  Administration   English 6-12 0
#> 65         No   Drafting  Administration   Instr. music 0
#> 66         No   Drafting  Administration        PENDING 0
#> 67         No   Drafting  Administration    Physical ed 0
#> 68         No   Drafting  Administration Political sci. 0
#> 69         No   Drafting  Administration   Science 6-12 0
#> 70         No   Drafting  Administration    Vocal music 0
#> 71         No   Drafting           Coach   English 6-12 0
#> 72         No   Drafting           Coach   Instr. music 0
#> 73         No   Drafting           Coach        PENDING 0
#> 74         No   Drafting           Coach    Physical ed 0
#> 75         No   Drafting           Coach Political sci. 0
#> 76         No   Drafting           Coach   Science 6-12 0
#> 77         No   Drafting           Coach    Vocal music 0
#> 78         No   Drafting         Teacher   English 6-12 0
#> 79         No   Drafting         Teacher   Instr. music 0
#> 80         No   Drafting         Teacher        PENDING 0
#> 81         No   Drafting         Teacher    Physical ed 0
#> 82         No   Drafting         Teacher Political sci. 0
#> 83         No   Drafting         Teacher   Science 6-12 0
#> 84         No   Drafting         Teacher    Vocal music 0
#> 85         No    English  Administration   English 6-12 0
#> 86         No    English  Administration   Instr. music 0
#> 87         No    English  Administration        PENDING 0
#> 88         No    English  Administration    Physical ed 0
#> 89         No    English  Administration Political sci. 0
#> 90         No    English  Administration   Science 6-12 0
#> 91         No    English  Administration    Vocal music 0
#> 92         No    English           Coach   English 6-12 0
#> 93         No    English           Coach   Instr. music 0
#> 94         No    English           Coach        PENDING 0
#> 95         No    English           Coach    Physical ed 0
#> 96         No    English           Coach Political sci. 0
#> 97         No    English           Coach   Science 6-12 0
#> 98         No    English           Coach    Vocal music 0
#> 99         No    English         Teacher   English 6-12 1
#> 100        No    English         Teacher   Instr. music 0
#> 101        No    English         Teacher        PENDING 0
#> 102        No    English         Teacher    Physical ed 0
#> 103        No    English         Teacher Political sci. 0
#> 104        No    English         Teacher   Science 6-12 0
#> 105        No    English         Teacher    Vocal music 1
#> 106        No      Music  Administration   English 6-12 0
#> 107        No      Music  Administration   Instr. music 0
#> 108        No      Music  Administration        PENDING 0
#> 109        No      Music  Administration    Physical ed 0
#> 110        No      Music  Administration Political sci. 0
#> 111        No      Music  Administration   Science 6-12 0
#> 112        No      Music  Administration    Vocal music 0
#> 113        No      Music           Coach   English 6-12 0
#> 114        No      Music           Coach   Instr. music 0
#> 115        No      Music           Coach        PENDING 0
#> 116        No      Music           Coach    Physical ed 0
#> 117        No      Music           Coach Political sci. 0
#> 118        No      Music           Coach   Science 6-12 0
#> 119        No      Music           Coach    Vocal music 0
#> 120        No      Music         Teacher   English 6-12 0
#> 121        No      Music         Teacher   Instr. music 0
#> 122        No      Music         Teacher        PENDING 0
#> 123        No      Music         Teacher    Physical ed 0
#> 124        No      Music         Teacher Political sci. 0
#> 125        No      Music         Teacher   Science 6-12 0
#> 126        No      Music         Teacher    Vocal music 0
#> 127        No         PE  Administration   English 6-12 0
#> 128        No         PE  Administration   Instr. music 0
#> 129        No         PE  Administration        PENDING 0
#> 130        No         PE  Administration    Physical ed 0
#> 131        No         PE  Administration Political sci. 0
#> 132        No         PE  Administration   Science 6-12 0
#> 133        No         PE  Administration    Vocal music 0
#> 134        No         PE           Coach   English 6-12 0
#> 135        No         PE           Coach   Instr. music 0
#> 136        No         PE           Coach        PENDING 0
#> 137        No         PE           Coach    Physical ed 0
#> 138        No         PE           Coach Political sci. 0
#> 139        No         PE           Coach   Science 6-12 0
#> 140        No         PE           Coach    Vocal music 0
#> 141        No         PE         Teacher   English 6-12 0
#> 142        No         PE         Teacher   Instr. music 0
#> 143        No         PE         Teacher        PENDING 0
#> 144        No         PE         Teacher    Physical ed 0
#> 145        No         PE         Teacher Political sci. 0
#> 146        No         PE         Teacher   Science 6-12 0
#> 147        No         PE         Teacher    Vocal music 0
#> 148        No    Physics  Administration   English 6-12 0
#> 149        No    Physics  Administration   Instr. music 0
#> 150        No    Physics  Administration        PENDING 0
#> 151        No    Physics  Administration    Physical ed 0
#> 152        No    Physics  Administration Political sci. 0
#> 153        No    Physics  Administration   Science 6-12 0
#> 154        No    Physics  Administration    Vocal music 0
#> 155        No    Physics           Coach   English 6-12 0
#> 156        No    Physics           Coach   Instr. music 0
#> 157        No    Physics           Coach        PENDING 0
#> 158        No    Physics           Coach    Physical ed 0
#> 159        No    Physics           Coach Political sci. 0
#> 160        No    Physics           Coach   Science 6-12 0
#> 161        No    Physics           Coach    Vocal music 0
#> 162        No    Physics         Teacher   English 6-12 0
#> 163        No    Physics         Teacher   Instr. music 0
#> 164        No    Physics         Teacher        PENDING 0
#> 165        No    Physics         Teacher    Physical ed 0
#> 166        No    Physics         Teacher Political sci. 0
#> 167        No    Physics         Teacher   Science 6-12 0
#> 168        No    Physics         Teacher    Vocal music 0
#> 169        No    Science  Administration   English 6-12 0
#> 170        No    Science  Administration   Instr. music 0
#> 171        No    Science  Administration        PENDING 0
#> 172        No    Science  Administration    Physical ed 0
#> 173        No    Science  Administration Political sci. 0
#> 174        No    Science  Administration   Science 6-12 0
#> 175        No    Science  Administration    Vocal music 0
#> 176        No    Science           Coach   English 6-12 0
#> 177        No    Science           Coach   Instr. music 0
#> 178        No    Science           Coach        PENDING 0
#> 179        No    Science           Coach    Physical ed 0
#> 180        No    Science           Coach Political sci. 0
#> 181        No    Science           Coach   Science 6-12 0
#> 182        No    Science           Coach    Vocal music 0
#> 183        No    Science         Teacher   English 6-12 0
#> 184        No    Science         Teacher   Instr. music 0
#> 185        No    Science         Teacher        PENDING 1
#> 186        No    Science         Teacher    Physical ed 0
#> 187        No    Science         Teacher Political sci. 0
#> 188        No    Science         Teacher   Science 6-12 0
#> 189        No    Science         Teacher    Vocal music 0
#> 190        No       <NA>  Administration   English 6-12 0
#> 191        No       <NA>  Administration   Instr. music 0
#> 192        No       <NA>  Administration        PENDING 0
#> 193        No       <NA>  Administration    Physical ed 0
#> 194        No       <NA>  Administration Political sci. 0
#> 195        No       <NA>  Administration   Science 6-12 0
#> 196        No       <NA>  Administration    Vocal music 0
#> 197        No       <NA>           Coach   English 6-12 0
#> 198        No       <NA>           Coach   Instr. music 0
#> 199        No       <NA>           Coach        PENDING 0
#> 200        No       <NA>           Coach    Physical ed 0
#> 201        No       <NA>           Coach Political sci. 1
#> 202        No       <NA>           Coach   Science 6-12 0
#> 203        No       <NA>           Coach    Vocal music 0
#> 204        No       <NA>         Teacher   English 6-12 0
#> 205        No       <NA>         Teacher   Instr. music 0
#> 206        No       <NA>         Teacher        PENDING 0
#> 207        No       <NA>         Teacher    Physical ed 0
#> 208        No       <NA>         Teacher Political sci. 0
#> 209        No       <NA>         Teacher   Science 6-12 0
#> 210        No       <NA>         Teacher    Vocal music 0
#> 211       Yes Basketball  Administration   English 6-12 0
#> 212       Yes Basketball  Administration   Instr. music 0
#> 213       Yes Basketball  Administration        PENDING 0
#> 214       Yes Basketball  Administration    Physical ed 0
#> 215       Yes Basketball  Administration Political sci. 0
#> 216       Yes Basketball  Administration   Science 6-12 0
#> 217       Yes Basketball  Administration    Vocal music 0
#> 218       Yes Basketball           Coach   English 6-12 0
#> 219       Yes Basketball           Coach   Instr. music 0
#> 220       Yes Basketball           Coach        PENDING 0
#> 221       Yes Basketball           Coach    Physical ed 0
#> 222       Yes Basketball           Coach Political sci. 0
#> 223       Yes Basketball           Coach   Science 6-12 0
#> 224       Yes Basketball           Coach    Vocal music 0
#> 225       Yes Basketball         Teacher   English 6-12 0
#> 226       Yes Basketball         Teacher   Instr. music 0
#> 227       Yes Basketball         Teacher        PENDING 0
#> 228       Yes Basketball         Teacher    Physical ed 0
#> 229       Yes Basketball         Teacher Political sci. 0
#> 230       Yes Basketball         Teacher   Science 6-12 0
#> 231       Yes Basketball         Teacher    Vocal music 0
#> 232       Yes  Chemistry  Administration   English 6-12 0
#> 233       Yes  Chemistry  Administration   Instr. music 0
#> 234       Yes  Chemistry  Administration        PENDING 0
#> 235       Yes  Chemistry  Administration    Physical ed 0
#> 236       Yes  Chemistry  Administration Political sci. 0
#> 237       Yes  Chemistry  Administration   Science 6-12 0
#> 238       Yes  Chemistry  Administration    Vocal music 0
#> 239       Yes  Chemistry           Coach   English 6-12 0
#> 240       Yes  Chemistry           Coach   Instr. music 0
#> 241       Yes  Chemistry           Coach        PENDING 0
#> 242       Yes  Chemistry           Coach    Physical ed 0
#> 243       Yes  Chemistry           Coach Political sci. 0
#> 244       Yes  Chemistry           Coach   Science 6-12 0
#> 245       Yes  Chemistry           Coach    Vocal music 0
#> 246       Yes  Chemistry         Teacher   English 6-12 0
#> 247       Yes  Chemistry         Teacher   Instr. music 0
#> 248       Yes  Chemistry         Teacher        PENDING 0
#> 249       Yes  Chemistry         Teacher    Physical ed 0
#> 250       Yes  Chemistry         Teacher Political sci. 0
#> 251       Yes  Chemistry         Teacher   Science 6-12 1
#> 252       Yes  Chemistry         Teacher    Vocal music 0
#> 253       Yes       Dean  Administration   English 6-12 0
#> 254       Yes       Dean  Administration   Instr. music 0
#> 255       Yes       Dean  Administration        PENDING 1
#> 256       Yes       Dean  Administration    Physical ed 0
#> 257       Yes       Dean  Administration Political sci. 0
#> 258       Yes       Dean  Administration   Science 6-12 0
#> 259       Yes       Dean  Administration    Vocal music 0
#> 260       Yes       Dean           Coach   English 6-12 0
#> 261       Yes       Dean           Coach   Instr. music 0
#> 262       Yes       Dean           Coach        PENDING 0
#> 263       Yes       Dean           Coach    Physical ed 0
#> 264       Yes       Dean           Coach Political sci. 0
#> 265       Yes       Dean           Coach   Science 6-12 0
#> 266       Yes       Dean           Coach    Vocal music 0
#> 267       Yes       Dean         Teacher   English 6-12 0
#> 268       Yes       Dean         Teacher   Instr. music 0
#> 269       Yes       Dean         Teacher        PENDING 0
#> 270       Yes       Dean         Teacher    Physical ed 0
#> 271       Yes       Dean         Teacher Political sci. 0
#> 272       Yes       Dean         Teacher   Science 6-12 0
#> 273       Yes       Dean         Teacher    Vocal music 0
#> 274       Yes   Drafting  Administration   English 6-12 0
#> 275       Yes   Drafting  Administration   Instr. music 0
#> 276       Yes   Drafting  Administration        PENDING 0
#> 277       Yes   Drafting  Administration    Physical ed 0
#> 278       Yes   Drafting  Administration Political sci. 0
#> 279       Yes   Drafting  Administration   Science 6-12 0
#> 280       Yes   Drafting  Administration    Vocal music 0
#> 281       Yes   Drafting           Coach   English 6-12 0
#> 282       Yes   Drafting           Coach   Instr. music 0
#> 283       Yes   Drafting           Coach        PENDING 0
#> 284       Yes   Drafting           Coach    Physical ed 0
#> 285       Yes   Drafting           Coach Political sci. 0
#> 286       Yes   Drafting           Coach   Science 6-12 0
#> 287       Yes   Drafting           Coach    Vocal music 0
#> 288       Yes   Drafting         Teacher   English 6-12 0
#> 289       Yes   Drafting         Teacher   Instr. music 0
#> 290       Yes   Drafting         Teacher        PENDING 0
#> 291       Yes   Drafting         Teacher    Physical ed 1
#> 292       Yes   Drafting         Teacher Political sci. 0
#> 293       Yes   Drafting         Teacher   Science 6-12 0
#> 294       Yes   Drafting         Teacher    Vocal music 0
#> 295       Yes    English  Administration   English 6-12 0
#> 296       Yes    English  Administration   Instr. music 0
#> 297       Yes    English  Administration        PENDING 0
#> 298       Yes    English  Administration    Physical ed 0
#> 299       Yes    English  Administration Political sci. 0
#> 300       Yes    English  Administration   Science 6-12 0
#> 301       Yes    English  Administration    Vocal music 0
#> 302       Yes    English           Coach   English 6-12 0
#> 303       Yes    English           Coach   Instr. music 0
#> 304       Yes    English           Coach        PENDING 0
#> 305       Yes    English           Coach    Physical ed 0
#> 306       Yes    English           Coach Political sci. 0
#> 307       Yes    English           Coach   Science 6-12 0
#> 308       Yes    English           Coach    Vocal music 0
#> 309       Yes    English         Teacher   English 6-12 0
#> 310       Yes    English         Teacher   Instr. music 0
#> 311       Yes    English         Teacher        PENDING 0
#> 312       Yes    English         Teacher    Physical ed 0
#> 313       Yes    English         Teacher Political sci. 0
#> 314       Yes    English         Teacher   Science 6-12 0
#> 315       Yes    English         Teacher    Vocal music 0
#> 316       Yes      Music  Administration   English 6-12 0
#> 317       Yes      Music  Administration   Instr. music 0
#> 318       Yes      Music  Administration        PENDING 0
#> 319       Yes      Music  Administration    Physical ed 0
#> 320       Yes      Music  Administration Political sci. 0
#> 321       Yes      Music  Administration   Science 6-12 0
#> 322       Yes      Music  Administration    Vocal music 0
#> 323       Yes      Music           Coach   English 6-12 0
#> 324       Yes      Music           Coach   Instr. music 0
#> 325       Yes      Music           Coach        PENDING 0
#> 326       Yes      Music           Coach    Physical ed 0
#> 327       Yes      Music           Coach Political sci. 0
#> 328       Yes      Music           Coach   Science 6-12 0
#> 329       Yes      Music           Coach    Vocal music 0
#> 330       Yes      Music         Teacher   English 6-12 0
#> 331       Yes      Music         Teacher   Instr. music 1
#> 332       Yes      Music         Teacher        PENDING 0
#> 333       Yes      Music         Teacher    Physical ed 0
#> 334       Yes      Music         Teacher Political sci. 0
#> 335       Yes      Music         Teacher   Science 6-12 0
#> 336       Yes      Music         Teacher    Vocal music 0
#> 337       Yes         PE  Administration   English 6-12 0
#> 338       Yes         PE  Administration   Instr. music 0
#> 339       Yes         PE  Administration        PENDING 0
#> 340       Yes         PE  Administration    Physical ed 0
#> 341       Yes         PE  Administration Political sci. 0
#> 342       Yes         PE  Administration   Science 6-12 0
#> 343       Yes         PE  Administration    Vocal music 0
#> 344       Yes         PE           Coach   English 6-12 0
#> 345       Yes         PE           Coach   Instr. music 0
#> 346       Yes         PE           Coach        PENDING 0
#> 347       Yes         PE           Coach    Physical ed 0
#> 348       Yes         PE           Coach Political sci. 0
#> 349       Yes         PE           Coach   Science 6-12 0
#> 350       Yes         PE           Coach    Vocal music 0
#> 351       Yes         PE         Teacher   English 6-12 0
#> 352       Yes         PE         Teacher   Instr. music 0
#> 353       Yes         PE         Teacher        PENDING 0
#> 354       Yes         PE         Teacher    Physical ed 1
#> 355       Yes         PE         Teacher Political sci. 0
#> 356       Yes         PE         Teacher   Science 6-12 0
#> 357       Yes         PE         Teacher    Vocal music 0
#> 358       Yes    Physics  Administration   English 6-12 0
#> 359       Yes    Physics  Administration   Instr. music 0
#> 360       Yes    Physics  Administration        PENDING 0
#> 361       Yes    Physics  Administration    Physical ed 0
#> 362       Yes    Physics  Administration Political sci. 0
#> 363       Yes    Physics  Administration   Science 6-12 0
#> 364       Yes    Physics  Administration    Vocal music 0
#> 365       Yes    Physics           Coach   English 6-12 0
#> 366       Yes    Physics           Coach   Instr. music 0
#> 367       Yes    Physics           Coach        PENDING 0
#> 368       Yes    Physics           Coach    Physical ed 0
#> 369       Yes    Physics           Coach Political sci. 0
#> 370       Yes    Physics           Coach   Science 6-12 0
#> 371       Yes    Physics           Coach    Vocal music 0
#> 372       Yes    Physics         Teacher   English 6-12 0
#> 373       Yes    Physics         Teacher   Instr. music 0
#> 374       Yes    Physics         Teacher        PENDING 0
#> 375       Yes    Physics         Teacher    Physical ed 0
#> 376       Yes    Physics         Teacher Political sci. 0
#> 377       Yes    Physics         Teacher   Science 6-12 1
#> 378       Yes    Physics         Teacher    Vocal music 0
#> 379       Yes    Science  Administration   English 6-12 0
#> 380       Yes    Science  Administration   Instr. music 0
#> 381       Yes    Science  Administration        PENDING 0
#> 382       Yes    Science  Administration    Physical ed 0
#> 383       Yes    Science  Administration Political sci. 0
#> 384       Yes    Science  Administration   Science 6-12 0
#> 385       Yes    Science  Administration    Vocal music 0
#> 386       Yes    Science           Coach   English 6-12 0
#> 387       Yes    Science           Coach   Instr. music 0
#> 388       Yes    Science           Coach        PENDING 0
#> 389       Yes    Science           Coach    Physical ed 0
#> 390       Yes    Science           Coach Political sci. 0
#> 391       Yes    Science           Coach   Science 6-12 0
#> 392       Yes    Science           Coach    Vocal music 0
#> 393       Yes    Science         Teacher   English 6-12 0
#> 394       Yes    Science         Teacher   Instr. music 0
#> 395       Yes    Science         Teacher        PENDING 0
#> 396       Yes    Science         Teacher    Physical ed 0
#> 397       Yes    Science         Teacher Political sci. 0
#> 398       Yes    Science         Teacher   Science 6-12 0
#> 399       Yes    Science         Teacher    Vocal music 0
#> 400       Yes       <NA>  Administration   English 6-12 0
#> 401       Yes       <NA>  Administration   Instr. music 0
#> 402       Yes       <NA>  Administration        PENDING 0
#> 403       Yes       <NA>  Administration    Physical ed 0
#> 404       Yes       <NA>  Administration Political sci. 0
#> 405       Yes       <NA>  Administration   Science 6-12 0
#> 406       Yes       <NA>  Administration    Vocal music 0
#> 407       Yes       <NA>           Coach   English 6-12 0
#> 408       Yes       <NA>           Coach   Instr. music 0
#> 409       Yes       <NA>           Coach        PENDING 0
#> 410       Yes       <NA>           Coach    Physical ed 0
#> 411       Yes       <NA>           Coach Political sci. 0
#> 412       Yes       <NA>           Coach   Science 6-12 0
#> 413       Yes       <NA>           Coach    Vocal music 0
#> 414       Yes       <NA>         Teacher   English 6-12 0
#> 415       Yes       <NA>         Teacher   Instr. music 0
#> 416       Yes       <NA>         Teacher        PENDING 1
#> 417       Yes       <NA>         Teacher    Physical ed 0
#> 418       Yes       <NA>         Teacher Political sci. 0
#> 419       Yes       <NA>         Teacher   Science 6-12 0
#> 420       Yes       <NA>         Teacher    Vocal music 0
tabyl_nway(roster, full_time, subject, employee_status)
#>    full_time    subject employee_status n
#> 1         No Basketball  Administration 0
#> 2         No Basketball           Coach 1
#> 3         No Basketball         Teacher 0
#> 4         No  Chemistry  Administration 0
#> 5         No  Chemistry           Coach 0
#> 6         No  Chemistry         Teacher 0
#> 7         No       Dean  Administration 0
#> 8         No       Dean           Coach 0
#> 9         No       Dean         Teacher 0
#> 10        No   Drafting  Administration 0
#> 11        No   Drafting           Coach 0
#> 12        No   Drafting         Teacher 0
#> 13        No    English  Administration 0
#> 14        No    English           Coach 0
#> 15        No    English         Teacher 2
#> 16        No      Music  Administration 0
#> 17        No      Music           Coach 0
#> 18        No      Music         Teacher 0
#> 19        No         PE  Administration 0
#> 20        No         PE           Coach 0
#> 21        No         PE         Teacher 0
#> 22        No    Physics  Administration 0
#> 23        No    Physics           Coach 0
#> 24        No    Physics         Teacher 0
#> 25        No    Science  Administration 0
#> 26        No    Science           Coach 0
#> 27        No    Science         Teacher 1
#> 28        No       <NA>  Administration 0
#> 29        No       <NA>           Coach 1
#> 30        No       <NA>         Teacher 0
#> 31       Yes Basketball  Administration 0
#> 32       Yes Basketball           Coach 0
#> 33       Yes Basketball         Teacher 0
#> 34       Yes  Chemistry  Administration 0
#> 35       Yes  Chemistry           Coach 0
#> 36       Yes  Chemistry         Teacher 1
#> 37       Yes       Dean  Administration 1
#> 38       Yes       Dean           Coach 0
#> 39       Yes       Dean         Teacher 0
#> 40       Yes   Drafting  Administration 0
#> 41       Yes   Drafting           Coach 0
#> 42       Yes   Drafting         Teacher 1
#> 43       Yes    English  Administration 0
#> 44       Yes    English           Coach 0
#> 45       Yes    English         Teacher 0
#> 46       Yes      Music  Administration 0
#> 47       Yes      Music           Coach 0
#> 48       Yes      Music         Teacher 1
#> 49       Yes         PE  Administration 0
#> 50       Yes         PE           Coach 0
#> 51       Yes         PE         Teacher 1
#> 52       Yes    Physics  Administration 0
#> 53       Yes    Physics           Coach 0
#> 54       Yes    Physics         Teacher 1
#> 55       Yes    Science  Administration 0
#> 56       Yes    Science           Coach 0
#> 57       Yes    Science         Teacher 0
#> 58       Yes       <NA>  Administration 0
#> 59       Yes       <NA>           Coach 0
#> 60       Yes       <NA>         Teacher 1

# For four or more variables:
tabyl_nway(roster, full_time, subject, employee_status, cert)
#>     full_time    subject employee_status           cert n
#> 1          No Basketball  Administration   English 6-12 0
#> 2          No Basketball  Administration   Instr. music 0
#> 3          No Basketball  Administration        PENDING 0
#> 4          No Basketball  Administration    Physical ed 0
#> 5          No Basketball  Administration Political sci. 0
#> 6          No Basketball  Administration   Science 6-12 0
#> 7          No Basketball  Administration    Vocal music 0
#> 8          No Basketball           Coach   English 6-12 0
#> 9          No Basketball           Coach   Instr. music 0
#> 10         No Basketball           Coach        PENDING 0
#> 11         No Basketball           Coach    Physical ed 1
#> 12         No Basketball           Coach Political sci. 0
#> 13         No Basketball           Coach   Science 6-12 0
#> 14         No Basketball           Coach    Vocal music 0
#> 15         No Basketball         Teacher   English 6-12 0
#> 16         No Basketball         Teacher   Instr. music 0
#> 17         No Basketball         Teacher        PENDING 0
#> 18         No Basketball         Teacher    Physical ed 0
#> 19         No Basketball         Teacher Political sci. 0
#> 20         No Basketball         Teacher   Science 6-12 0
#> 21         No Basketball         Teacher    Vocal music 0
#> 22         No  Chemistry  Administration   English 6-12 0
#> 23         No  Chemistry  Administration   Instr. music 0
#> 24         No  Chemistry  Administration        PENDING 0
#> 25         No  Chemistry  Administration    Physical ed 0
#> 26         No  Chemistry  Administration Political sci. 0
#> 27         No  Chemistry  Administration   Science 6-12 0
#> 28         No  Chemistry  Administration    Vocal music 0
#> 29         No  Chemistry           Coach   English 6-12 0
#> 30         No  Chemistry           Coach   Instr. music 0
#> 31         No  Chemistry           Coach        PENDING 0
#> 32         No  Chemistry           Coach    Physical ed 0
#> 33         No  Chemistry           Coach Political sci. 0
#> 34         No  Chemistry           Coach   Science 6-12 0
#> 35         No  Chemistry           Coach    Vocal music 0
#> 36         No  Chemistry         Teacher   English 6-12 0
#> 37         No  Chemistry         Teacher   Instr. music 0
#> 38         No  Chemistry         Teacher        PENDING 0
#> 39         No  Chemistry         Teacher    Physical ed 0
#> 40         No  Chemistry         Teacher Political sci. 0
#> 41         No  Chemistry         Teacher   Science 6-12 0
#> 42         No  Chemistry         Teacher    Vocal music 0
#> 43         No       Dean  Administration   English 6-12 0
#> 44         No       Dean  Administration   Instr. music 0
#> 45         No       Dean  Administration        PENDING 0
#> 46         No       Dean  Administration    Physical ed 0
#> 47         No       Dean  Administration Political sci. 0
#> 48         No       Dean  Administration   Science 6-12 0
#> 49         No       Dean  Administration    Vocal music 0
#> 50         No       Dean           Coach   English 6-12 0
#> 51         No       Dean           Coach   Instr. music 0
#> 52         No       Dean           Coach        PENDING 0
#> 53         No       Dean           Coach    Physical ed 0
#> 54         No       Dean           Coach Political sci. 0
#> 55         No       Dean           Coach   Science 6-12 0
#> 56         No       Dean           Coach    Vocal music 0
#> 57         No       Dean         Teacher   English 6-12 0
#> 58         No       Dean         Teacher   Instr. music 0
#> 59         No       Dean         Teacher        PENDING 0
#> 60         No       Dean         Teacher    Physical ed 0
#> 61         No       Dean         Teacher Political sci. 0
#> 62         No       Dean         Teacher   Science 6-12 0
#> 63         No       Dean         Teacher    Vocal music 0
#> 64         No   Drafting  Administration   English 6-12 0
#> 65         No   Drafting  Administration   Instr. music 0
#> 66         No   Drafting  Administration        PENDING 0
#> 67         No   Drafting  Administration    Physical ed 0
#> 68         No   Drafting  Administration Political sci. 0
#> 69         No   Drafting  Administration   Science 6-12 0
#> 70         No   Drafting  Administration    Vocal music 0
#> 71         No   Drafting           Coach   English 6-12 0
#> 72         No   Drafting           Coach   Instr. music 0
#> 73         No   Drafting           Coach        PENDING 0
#> 74         No   Drafting           Coach    Physical ed 0
#> 75         No   Drafting           Coach Political sci. 0
#> 76         No   Drafting           Coach   Science 6-12 0
#> 77         No   Drafting           Coach    Vocal music 0
#> 78         No   Drafting         Teacher   English 6-12 0
#> 79         No   Drafting         Teacher   Instr. music 0
#> 80         No   Drafting         Teacher        PENDING 0
#> 81         No   Drafting         Teacher    Physical ed 0
#> 82         No   Drafting         Teacher Political sci. 0
#> 83         No   Drafting         Teacher   Science 6-12 0
#> 84         No   Drafting         Teacher    Vocal music 0
#> 85         No    English  Administration   English 6-12 0
#> 86         No    English  Administration   Instr. music 0
#> 87         No    English  Administration        PENDING 0
#> 88         No    English  Administration    Physical ed 0
#> 89         No    English  Administration Political sci. 0
#> 90         No    English  Administration   Science 6-12 0
#> 91         No    English  Administration    Vocal music 0
#> 92         No    English           Coach   English 6-12 0
#> 93         No    English           Coach   Instr. music 0
#> 94         No    English           Coach        PENDING 0
#> 95         No    English           Coach    Physical ed 0
#> 96         No    English           Coach Political sci. 0
#> 97         No    English           Coach   Science 6-12 0
#> 98         No    English           Coach    Vocal music 0
#> 99         No    English         Teacher   English 6-12 1
#> 100        No    English         Teacher   Instr. music 0
#> 101        No    English         Teacher        PENDING 0
#> 102        No    English         Teacher    Physical ed 0
#> 103        No    English         Teacher Political sci. 0
#> 104        No    English         Teacher   Science 6-12 0
#> 105        No    English         Teacher    Vocal music 1
#> 106        No      Music  Administration   English 6-12 0
#> 107        No      Music  Administration   Instr. music 0
#> 108        No      Music  Administration        PENDING 0
#> 109        No      Music  Administration    Physical ed 0
#> 110        No      Music  Administration Political sci. 0
#> 111        No      Music  Administration   Science 6-12 0
#> 112        No      Music  Administration    Vocal music 0
#> 113        No      Music           Coach   English 6-12 0
#> 114        No      Music           Coach   Instr. music 0
#> 115        No      Music           Coach        PENDING 0
#> 116        No      Music           Coach    Physical ed 0
#> 117        No      Music           Coach Political sci. 0
#> 118        No      Music           Coach   Science 6-12 0
#> 119        No      Music           Coach    Vocal music 0
#> 120        No      Music         Teacher   English 6-12 0
#> 121        No      Music         Teacher   Instr. music 0
#> 122        No      Music         Teacher        PENDING 0
#> 123        No      Music         Teacher    Physical ed 0
#> 124        No      Music         Teacher Political sci. 0
#> 125        No      Music         Teacher   Science 6-12 0
#> 126        No      Music         Teacher    Vocal music 0
#> 127        No         PE  Administration   English 6-12 0
#> 128        No         PE  Administration   Instr. music 0
#> 129        No         PE  Administration        PENDING 0
#> 130        No         PE  Administration    Physical ed 0
#> 131        No         PE  Administration Political sci. 0
#> 132        No         PE  Administration   Science 6-12 0
#> 133        No         PE  Administration    Vocal music 0
#> 134        No         PE           Coach   English 6-12 0
#> 135        No         PE           Coach   Instr. music 0
#> 136        No         PE           Coach        PENDING 0
#> 137        No         PE           Coach    Physical ed 0
#> 138        No         PE           Coach Political sci. 0
#> 139        No         PE           Coach   Science 6-12 0
#> 140        No         PE           Coach    Vocal music 0
#> 141        No         PE         Teacher   English 6-12 0
#> 142        No         PE         Teacher   Instr. music 0
#> 143        No         PE         Teacher        PENDING 0
#> 144        No         PE         Teacher    Physical ed 0
#> 145        No         PE         Teacher Political sci. 0
#> 146        No         PE         Teacher   Science 6-12 0
#> 147        No         PE         Teacher    Vocal music 0
#> 148        No    Physics  Administration   English 6-12 0
#> 149        No    Physics  Administration   Instr. music 0
#> 150        No    Physics  Administration        PENDING 0
#> 151        No    Physics  Administration    Physical ed 0
#> 152        No    Physics  Administration Political sci. 0
#> 153        No    Physics  Administration   Science 6-12 0
#> 154        No    Physics  Administration    Vocal music 0
#> 155        No    Physics           Coach   English 6-12 0
#> 156        No    Physics           Coach   Instr. music 0
#> 157        No    Physics           Coach        PENDING 0
#> 158        No    Physics           Coach    Physical ed 0
#> 159        No    Physics           Coach Political sci. 0
#> 160        No    Physics           Coach   Science 6-12 0
#> 161        No    Physics           Coach    Vocal music 0
#> 162        No    Physics         Teacher   English 6-12 0
#> 163        No    Physics         Teacher   Instr. music 0
#> 164        No    Physics         Teacher        PENDING 0
#> 165        No    Physics         Teacher    Physical ed 0
#> 166        No    Physics         Teacher Political sci. 0
#> 167        No    Physics         Teacher   Science 6-12 0
#> 168        No    Physics         Teacher    Vocal music 0
#> 169        No    Science  Administration   English 6-12 0
#> 170        No    Science  Administration   Instr. music 0
#> 171        No    Science  Administration        PENDING 0
#> 172        No    Science  Administration    Physical ed 0
#> 173        No    Science  Administration Political sci. 0
#> 174        No    Science  Administration   Science 6-12 0
#> 175        No    Science  Administration    Vocal music 0
#> 176        No    Science           Coach   English 6-12 0
#> 177        No    Science           Coach   Instr. music 0
#> 178        No    Science           Coach        PENDING 0
#> 179        No    Science           Coach    Physical ed 0
#> 180        No    Science           Coach Political sci. 0
#> 181        No    Science           Coach   Science 6-12 0
#> 182        No    Science           Coach    Vocal music 0
#> 183        No    Science         Teacher   English 6-12 0
#> 184        No    Science         Teacher   Instr. music 0
#> 185        No    Science         Teacher        PENDING 1
#> 186        No    Science         Teacher    Physical ed 0
#> 187        No    Science         Teacher Political sci. 0
#> 188        No    Science         Teacher   Science 6-12 0
#> 189        No    Science         Teacher    Vocal music 0
#> 190        No       <NA>  Administration   English 6-12 0
#> 191        No       <NA>  Administration   Instr. music 0
#> 192        No       <NA>  Administration        PENDING 0
#> 193        No       <NA>  Administration    Physical ed 0
#> 194        No       <NA>  Administration Political sci. 0
#> 195        No       <NA>  Administration   Science 6-12 0
#> 196        No       <NA>  Administration    Vocal music 0
#> 197        No       <NA>           Coach   English 6-12 0
#> 198        No       <NA>           Coach   Instr. music 0
#> 199        No       <NA>           Coach        PENDING 0
#> 200        No       <NA>           Coach    Physical ed 0
#> 201        No       <NA>           Coach Political sci. 1
#> 202        No       <NA>           Coach   Science 6-12 0
#> 203        No       <NA>           Coach    Vocal music 0
#> 204        No       <NA>         Teacher   English 6-12 0
#> 205        No       <NA>         Teacher   Instr. music 0
#> 206        No       <NA>         Teacher        PENDING 0
#> 207        No       <NA>         Teacher    Physical ed 0
#> 208        No       <NA>         Teacher Political sci. 0
#> 209        No       <NA>         Teacher   Science 6-12 0
#> 210        No       <NA>         Teacher    Vocal music 0
#> 211       Yes Basketball  Administration   English 6-12 0
#> 212       Yes Basketball  Administration   Instr. music 0
#> 213       Yes Basketball  Administration        PENDING 0
#> 214       Yes Basketball  Administration    Physical ed 0
#> 215       Yes Basketball  Administration Political sci. 0
#> 216       Yes Basketball  Administration   Science 6-12 0
#> 217       Yes Basketball  Administration    Vocal music 0
#> 218       Yes Basketball           Coach   English 6-12 0
#> 219       Yes Basketball           Coach   Instr. music 0
#> 220       Yes Basketball           Coach        PENDING 0
#> 221       Yes Basketball           Coach    Physical ed 0
#> 222       Yes Basketball           Coach Political sci. 0
#> 223       Yes Basketball           Coach   Science 6-12 0
#> 224       Yes Basketball           Coach    Vocal music 0
#> 225       Yes Basketball         Teacher   English 6-12 0
#> 226       Yes Basketball         Teacher   Instr. music 0
#> 227       Yes Basketball         Teacher        PENDING 0
#> 228       Yes Basketball         Teacher    Physical ed 0
#> 229       Yes Basketball         Teacher Political sci. 0
#> 230       Yes Basketball         Teacher   Science 6-12 0
#> 231       Yes Basketball         Teacher    Vocal music 0
#> 232       Yes  Chemistry  Administration   English 6-12 0
#> 233       Yes  Chemistry  Administration   Instr. music 0
#> 234       Yes  Chemistry  Administration        PENDING 0
#> 235       Yes  Chemistry  Administration    Physical ed 0
#> 236       Yes  Chemistry  Administration Political sci. 0
#> 237       Yes  Chemistry  Administration   Science 6-12 0
#> 238       Yes  Chemistry  Administration    Vocal music 0
#> 239       Yes  Chemistry           Coach   English 6-12 0
#> 240       Yes  Chemistry           Coach   Instr. music 0
#> 241       Yes  Chemistry           Coach        PENDING 0
#> 242       Yes  Chemistry           Coach    Physical ed 0
#> 243       Yes  Chemistry           Coach Political sci. 0
#> 244       Yes  Chemistry           Coach   Science 6-12 0
#> 245       Yes  Chemistry           Coach    Vocal music 0
#> 246       Yes  Chemistry         Teacher   English 6-12 0
#> 247       Yes  Chemistry         Teacher   Instr. music 0
#> 248       Yes  Chemistry         Teacher        PENDING 0
#> 249       Yes  Chemistry         Teacher    Physical ed 0
#> 250       Yes  Chemistry         Teacher Political sci. 0
#> 251       Yes  Chemistry         Teacher   Science 6-12 1
#> 252       Yes  Chemistry         Teacher    Vocal music 0
#> 253       Yes       Dean  Administration   English 6-12 0
#> 254       Yes       Dean  Administration   Instr. music 0
#> 255       Yes       Dean  Administration        PENDING 1
#> 256       Yes       Dean  Administration    Physical ed 0
#> 257       Yes       Dean  Administration Political sci. 0
#> 258       Yes       Dean  Administration   Science 6-12 0
#> 259       Yes       Dean  Administration    Vocal music 0
#> 260       Yes       Dean           Coach   English 6-12 0
#> 261       Yes       Dean           Coach   Instr. music 0
#> 262       Yes       Dean           Coach        PENDING 0
#> 263       Yes       Dean           Coach    Physical ed 0
#> 264       Yes       Dean           Coach Political sci. 0
#> 265       Yes       Dean           Coach   Science 6-12 0
#> 266       Yes       Dean           Coach    Vocal music 0
#> 267       Yes       Dean         Teacher   English 6-12 0
#> 268       Yes       Dean         Teacher   Instr. music 0
#> 269       Yes       Dean         Teacher        PENDING 0
#> 270       Yes       Dean         Teacher    Physical ed 0
#> 271       Yes       Dean         Teacher Political sci. 0
#> 272       Yes       Dean         Teacher   Science 6-12 0
#> 273       Yes       Dean         Teacher    Vocal music 0
#> 274       Yes   Drafting  Administration   English 6-12 0
#> 275       Yes   Drafting  Administration   Instr. music 0
#> 276       Yes   Drafting  Administration        PENDING 0
#> 277       Yes   Drafting  Administration    Physical ed 0
#> 278       Yes   Drafting  Administration Political sci. 0
#> 279       Yes   Drafting  Administration   Science 6-12 0
#> 280       Yes   Drafting  Administration    Vocal music 0
#> 281       Yes   Drafting           Coach   English 6-12 0
#> 282       Yes   Drafting           Coach   Instr. music 0
#> 283       Yes   Drafting           Coach        PENDING 0
#> 284       Yes   Drafting           Coach    Physical ed 0
#> 285       Yes   Drafting           Coach Political sci. 0
#> 286       Yes   Drafting           Coach   Science 6-12 0
#> 287       Yes   Drafting           Coach    Vocal music 0
#> 288       Yes   Drafting         Teacher   English 6-12 0
#> 289       Yes   Drafting         Teacher   Instr. music 0
#> 290       Yes   Drafting         Teacher        PENDING 0
#> 291       Yes   Drafting         Teacher    Physical ed 1
#> 292       Yes   Drafting         Teacher Political sci. 0
#> 293       Yes   Drafting         Teacher   Science 6-12 0
#> 294       Yes   Drafting         Teacher    Vocal music 0
#> 295       Yes    English  Administration   English 6-12 0
#> 296       Yes    English  Administration   Instr. music 0
#> 297       Yes    English  Administration        PENDING 0
#> 298       Yes    English  Administration    Physical ed 0
#> 299       Yes    English  Administration Political sci. 0
#> 300       Yes    English  Administration   Science 6-12 0
#> 301       Yes    English  Administration    Vocal music 0
#> 302       Yes    English           Coach   English 6-12 0
#> 303       Yes    English           Coach   Instr. music 0
#> 304       Yes    English           Coach        PENDING 0
#> 305       Yes    English           Coach    Physical ed 0
#> 306       Yes    English           Coach Political sci. 0
#> 307       Yes    English           Coach   Science 6-12 0
#> 308       Yes    English           Coach    Vocal music 0
#> 309       Yes    English         Teacher   English 6-12 0
#> 310       Yes    English         Teacher   Instr. music 0
#> 311       Yes    English         Teacher        PENDING 0
#> 312       Yes    English         Teacher    Physical ed 0
#> 313       Yes    English         Teacher Political sci. 0
#> 314       Yes    English         Teacher   Science 6-12 0
#> 315       Yes    English         Teacher    Vocal music 0
#> 316       Yes      Music  Administration   English 6-12 0
#> 317       Yes      Music  Administration   Instr. music 0
#> 318       Yes      Music  Administration        PENDING 0
#> 319       Yes      Music  Administration    Physical ed 0
#> 320       Yes      Music  Administration Political sci. 0
#> 321       Yes      Music  Administration   Science 6-12 0
#> 322       Yes      Music  Administration    Vocal music 0
#> 323       Yes      Music           Coach   English 6-12 0
#> 324       Yes      Music           Coach   Instr. music 0
#> 325       Yes      Music           Coach        PENDING 0
#> 326       Yes      Music           Coach    Physical ed 0
#> 327       Yes      Music           Coach Political sci. 0
#> 328       Yes      Music           Coach   Science 6-12 0
#> 329       Yes      Music           Coach    Vocal music 0
#> 330       Yes      Music         Teacher   English 6-12 0
#> 331       Yes      Music         Teacher   Instr. music 1
#> 332       Yes      Music         Teacher        PENDING 0
#> 333       Yes      Music         Teacher    Physical ed 0
#> 334       Yes      Music         Teacher Political sci. 0
#> 335       Yes      Music         Teacher   Science 6-12 0
#> 336       Yes      Music         Teacher    Vocal music 0
#> 337       Yes         PE  Administration   English 6-12 0
#> 338       Yes         PE  Administration   Instr. music 0
#> 339       Yes         PE  Administration        PENDING 0
#> 340       Yes         PE  Administration    Physical ed 0
#> 341       Yes         PE  Administration Political sci. 0
#> 342       Yes         PE  Administration   Science 6-12 0
#> 343       Yes         PE  Administration    Vocal music 0
#> 344       Yes         PE           Coach   English 6-12 0
#> 345       Yes         PE           Coach   Instr. music 0
#> 346       Yes         PE           Coach        PENDING 0
#> 347       Yes         PE           Coach    Physical ed 0
#> 348       Yes         PE           Coach Political sci. 0
#> 349       Yes         PE           Coach   Science 6-12 0
#> 350       Yes         PE           Coach    Vocal music 0
#> 351       Yes         PE         Teacher   English 6-12 0
#> 352       Yes         PE         Teacher   Instr. music 0
#> 353       Yes         PE         Teacher        PENDING 0
#> 354       Yes         PE         Teacher    Physical ed 1
#> 355       Yes         PE         Teacher Political sci. 0
#> 356       Yes         PE         Teacher   Science 6-12 0
#> 357       Yes         PE         Teacher    Vocal music 0
#> 358       Yes    Physics  Administration   English 6-12 0
#> 359       Yes    Physics  Administration   Instr. music 0
#> 360       Yes    Physics  Administration        PENDING 0
#> 361       Yes    Physics  Administration    Physical ed 0
#> 362       Yes    Physics  Administration Political sci. 0
#> 363       Yes    Physics  Administration   Science 6-12 0
#> 364       Yes    Physics  Administration    Vocal music 0
#> 365       Yes    Physics           Coach   English 6-12 0
#> 366       Yes    Physics           Coach   Instr. music 0
#> 367       Yes    Physics           Coach        PENDING 0
#> 368       Yes    Physics           Coach    Physical ed 0
#> 369       Yes    Physics           Coach Political sci. 0
#> 370       Yes    Physics           Coach   Science 6-12 0
#> 371       Yes    Physics           Coach    Vocal music 0
#> 372       Yes    Physics         Teacher   English 6-12 0
#> 373       Yes    Physics         Teacher   Instr. music 0
#> 374       Yes    Physics         Teacher        PENDING 0
#> 375       Yes    Physics         Teacher    Physical ed 0
#> 376       Yes    Physics         Teacher Political sci. 0
#> 377       Yes    Physics         Teacher   Science 6-12 1
#> 378       Yes    Physics         Teacher    Vocal music 0
#> 379       Yes    Science  Administration   English 6-12 0
#> 380       Yes    Science  Administration   Instr. music 0
#> 381       Yes    Science  Administration        PENDING 0
#> 382       Yes    Science  Administration    Physical ed 0
#> 383       Yes    Science  Administration Political sci. 0
#> 384       Yes    Science  Administration   Science 6-12 0
#> 385       Yes    Science  Administration    Vocal music 0
#> 386       Yes    Science           Coach   English 6-12 0
#> 387       Yes    Science           Coach   Instr. music 0
#> 388       Yes    Science           Coach        PENDING 0
#> 389       Yes    Science           Coach    Physical ed 0
#> 390       Yes    Science           Coach Political sci. 0
#> 391       Yes    Science           Coach   Science 6-12 0
#> 392       Yes    Science           Coach    Vocal music 0
#> 393       Yes    Science         Teacher   English 6-12 0
#> 394       Yes    Science         Teacher   Instr. music 0
#> 395       Yes    Science         Teacher        PENDING 0
#> 396       Yes    Science         Teacher    Physical ed 0
#> 397       Yes    Science         Teacher Political sci. 0
#> 398       Yes    Science         Teacher   Science 6-12 0
#> 399       Yes    Science         Teacher    Vocal music 0
#> 400       Yes       <NA>  Administration   English 6-12 0
#> 401       Yes       <NA>  Administration   Instr. music 0
#> 402       Yes       <NA>  Administration        PENDING 0
#> 403       Yes       <NA>  Administration    Physical ed 0
#> 404       Yes       <NA>  Administration Political sci. 0
#> 405       Yes       <NA>  Administration   Science 6-12 0
#> 406       Yes       <NA>  Administration    Vocal music 0
#> 407       Yes       <NA>           Coach   English 6-12 0
#> 408       Yes       <NA>           Coach   Instr. music 0
#> 409       Yes       <NA>           Coach        PENDING 0
#> 410       Yes       <NA>           Coach    Physical ed 0
#> 411       Yes       <NA>           Coach Political sci. 0
#> 412       Yes       <NA>           Coach   Science 6-12 0
#> 413       Yes       <NA>           Coach    Vocal music 0
#> 414       Yes       <NA>         Teacher   English 6-12 0
#> 415       Yes       <NA>         Teacher   Instr. music 0
#> 416       Yes       <NA>         Teacher        PENDING 1
#> 417       Yes       <NA>         Teacher    Physical ed 0
#> 418       Yes       <NA>         Teacher Political sci. 0
#> 419       Yes       <NA>         Teacher   Science 6-12 0
#> 420       Yes       <NA>         Teacher    Vocal music 0
```

tabyl_nway() also supports the argument show_na = FALSE to exclude rows
with NA in any of the specified variables.

#### Adorning tabyls

The `adorn_` functions dress up the results of these tabulation calls
for fast, basic reporting. Here are some of the functions that augment a
summary table for reporting:

``` r
roster %>%
  tabyl(employee_status, full_time) %>%
  adorn_totals("row") %>%
  adorn_percentages("row") %>%
  adorn_pct_formatting() %>%
  adorn_ns() %>%
  adorn_title("combined")
#>  employee_status/full_time         No        Yes
#>             Administration   0.0% (0) 100.0% (1)
#>                      Coach 100.0% (2)   0.0% (0)
#>                    Teacher  33.3% (3)  66.7% (6)
#>                      Total  41.7% (5)  58.3% (7)
```

Pipe that right into `knitr::kable()` in your RMarkdown report.

These modular adornments can be layered to reduce R’s deficit against
Excel and SPSS when it comes to quick, informative counts. Learn more
about `tabyl()` and the `adorn_` functions from the [tabyls
vignette](https://sfirke.github.io/janitor/articles/tabyls.html).

## <i class="fa fa-bullhorn" aria-hidden="true"></i> Contact me

You are welcome to:

- submit suggestions and report bugs:
  <https://github.com/sfirke/janitor/issues>
- let me know what you think on Mastodon:
  [@samfirke@a2mi.social](https://a2mi.social/@samfirke)
- compose a friendly e-mail to:
  <img src = "https://samfirke.com/wp-content/uploads/2016/07/email_address_whitespace_top.png" alt = "samuel.firke AT gmail" width = "210"/>
