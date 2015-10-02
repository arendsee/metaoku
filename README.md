# Metaoku

A shiny app for sharing, subsetting and automagically plotting data of any type

## Installation

``` bash
git clone https://github.com/zbwrnz/shiny-lab-data
cd shiny-lab-data
mkdir data
mkdir data/my-dataset
cp your-data-set.tsv data/my-dataset
```

## Setting up the database

 1. Add one folder per dataset to the data folder
 2. Each input files must be TAB-delimited
 3. There may be multiple data tables if they share a common first key column
 4. (optional) add a file named *data/HOME.md* to annotate the project
 5. (optional) add a file named *METADATA* to each table-containing folder to annotate the columns
 6. (optional) add a file named *README.md* to each table-containing folder to annotate the table

Example folder:

```
└── data
    ├── HOME.md
    ├── arabidopsis
    │   ├── at-prot.tab
    │   ├── confidence.tab
    │   ├── METADATA
    │   ├── README.md
    │   ├── short_description.tab
    │   ├── strata.tab
    │   └── type.tab
    ├── diamonds
    │   ├── diamonds.tab
    │   └── README
    └── yeast
        └── yeast.tab
```

## Launching the app

You may run the app locally by calling the script *run-locally.R*, which
requires R be installed on your system.

## Example

To run the app, click [here](https://arendsee.shinyapps.io/shiny-lab-data/)
