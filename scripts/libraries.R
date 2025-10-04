# load all libraries here in one place, 
# rather than loading repeatedly for each .Rmd. 
# can run this code in an .Rmd chunk by typing > source("scripts/libraries.R")

library(tidyverse) 
# incldues dplyr, tidyr, tibble, ggplot2, stringr, etc.
library(ggpubr)
library(grid) # for function unit() in ggplot2
library(gtable) # for adding subtitle in ggplot2
library(ggthemes)
library(scales) # to add percentages to plots and pretty_breaks()
library(gridExtra) # for arranging graphs side by side w ftn grid.arrange()
library(skimr) # for summary statistics
library(reshape2)
library(readxl) # to read excel files
library(writexl) # to ultimately create a .xlsx file for export
library(ggpmisc)
library(ggpubr)

#load packages necessary for GAMS
library("mgcv")
library("scam")
library("cowplot")
library("grid")              # for unit.pmax(), unit.list()
library("schoenberg")
library("tidyr")
library("nlme")

#from Cale (delete what I don't need) 
library(tidypaleo) ### for dealing with dates and vertical plotting
library(gratia) ## for improving GAMs

#troubleshooting wd issues
library(here)