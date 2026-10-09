# myRpackages

A collection of useful R functions for data analysis, text categorization, and automated reporting.

## Quick Start

Load all functions with one line:

```{r}
source("https://raw.githubusercontent.com/SimonLabarthe/myRpackages/main/load_all.R")
load_myRpackages()
```

Available Functions:

`create_descriptive_table()` - Generate descriptive statistics with PDF/LaTeX output

`create_frequency_table_2vars()` - Create 2-variable frequency tables

`create_frequency_table_3vars()` - Create 3-variable nested frequency tables

`categorise_api()` - Classify texts using DeepSeek AI API

## Examples
### Descriptive Statistics
```{r}
library(ggplot2)
create_descriptive_table(mpg, output_dir = getwd())
```

### Frequency Table
```{r}
create_frequency_table_2vars(mpg, "cyl", "drv")
```

## Requirements
```{r}
install.packages(c("xtable", "dplyr", "tidyr", "here", "ggplot2"))
```

For PDF output, install LaTeX:

```{r}
install.packages("tinytex")
tinytex::install_tinytex()
```

### Citation
It took me a while to create all of this. I would really appreciate it if you could make my work more visible.

You are pleased to cite as follow:

Labarthe, S. (2026). myRpackages: R Utility Functions.
https://github.com/SimonLabarthe/myRpackages

Visit My Website
https://github.com/SimonLabarthe



