#!/usr/bin/env Rscript

mirror='http://cran.rstudio.com'

if(! 'packrat' %in% installed.packages()){
    install.packages('packrat', repos=mirror)
}
require(packrat)
packrat::on
packrat::restore()
