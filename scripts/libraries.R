library(tidyverse)
library(ggpubr)
library(ggplot2)
library(grid) # for function unit() in ggplot2
library(gtable) # for adding subtitle in ggplot2
library(ggthemes)
library(scales) # to add percentages to plots and pretty_breaks()
library(gridExtra) # for arranging graphs side by side w ftn grid.arrange()
library(skimr) # for summary statistics
library(reshape2)
library(readxl) # to read excel files
library(writexl) # to ultimately create a .xlsx file for export
library(stringr)

#load packages necessary for GAMS
library("mgcv")
library("scam")
library("cowplot")
library("grid")                         # for unit.pmax(), unit.list()
library("schoenberg")
library("tidyr")
library("nlme")

#from Cale (delete what I don't need) 
library(tidypaleo) ### for dealing with dates and vertical plotting
library(tidyr) ## data management
library(dplyr) ## data transformation
library(cowplot) ## for combining plots
library(mgcv) ### for GAMs 
library(gratia) ## for improving GAMs

#troubleshooting wd issues
library(here)