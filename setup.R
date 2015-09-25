#!/bin/Rscript

if(! 'packrat' %in% installed.packages()){
    install.packages('packrat')
}
require(packrat)
packrat::restore()
