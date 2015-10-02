# Upload a dataset

**NOT YET IMPLEMENTED**

## Overview
A dataset may contain

 1. Multiple tables
 2. Files describing the columns in each data
 3. Files describing each table

## Data Directory Details

 1. Add one folder per dataset to the data folder
 2. Each input files must be TAB-delimited
 3. There may be multiple data tables if they share a common first key column
 4. (optional) add a file named `data/HOME.md` to annotate the project
 5. (optional) add a file named `METADATA` to each table-containing folder to annotate the columns
 6. (optional) add a file named `README.md` to each table-containing folder to annotate the table

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
    │   └── README.md
    └── yeast
        └── yeast.tab
```

## Upload

To upload the directory, you must package the directory with zip or tar. You
may also compress it with gzip, bunzip2 or xz.
