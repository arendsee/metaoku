[![deprecated](http://badges.github.io/stability-badges/dist/deprecated.svg)](http://github.com/badges/stability-badges)

# Metaoku

A shiny app for sharing, subsetting and automagically plotting data of any type

## Installation

``` bash
git clone https://github.com/zbwrnz/metaoku
cd shiny-lab-data
mkdir data
mkdir data/my-dataset
cp your-data-set.tsv data/my-dataset
```

## Dependencies

The standard CRAN packages should work fine. However, you will need to retrieve the DT package from github. To do you may run the following:

``` R
remove.packages('DT')
devtools::install_github('rstudio/DT')
```


## Setting up the database

 1. Add one folder per dataset to the data folder
 2. Each input files must be TAB-delimited
 3. There may be multiple data tables if they share a common first key column
 4. (optional) add a file named `COLUMN.tsv` to each table-containing folder to annotate the columns
 5. (optional) add a file named `README.md` to each table-containing folder to annotate the table

Example folder:

```
└── data
    ├── HOME.md
    ├── arabidopsis
    │   ├── arabidopsis.tab
    │   ├── COLUMN.tab
    │   └── README.md
    ├── diamonds
    │   ├── diamonds.tab
    │   └── README.md
    └── yeast
        └── yeast.tab
```

## Launching the app

You may run the app locally by calling the script `run-locally.R`, which
requires R be installed on your system.

## Example

To run the app, click [here](https://metaoku.shinyapps.io/sandbox/)
