# someRpackages

A collection of R functions for creating publication-ready LaTeX and PDF
tables, cross-tabulating categorical variables, and (optionally) classifying
texts via AI APIs.

## Installation

To install from GitHub:

```r
# install.packages("remotes")
remotes::install_github("SimonLabarthe/someRpackages")
```

Or, without installing the package:

```r
source("https://raw.githubusercontent.com/SimonLabarthe/someRpackages/main/load_all.R")
load_someRpackages()
```

## What is inside

- `create_descriptive_table()` — descriptive statistics table in PDF
- `create_frequency_table_2vars()` — cross-tabulation of two variables
- `create_frequency_table_3vars()` — cross-tabulation of three variables

## The paper

The technical note describing the package is in `paper/someRpackages.Rmd`.

## Citation

Labarthe, S. (2026). someRpackages: R utility functions.
https://github.com/SimonLabarthe/someRpackages

## Licence

MIT.
