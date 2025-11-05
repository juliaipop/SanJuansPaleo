# load all libraries here in one place, 
# rather than loading repeatedly for each .Rmd. 
# can run this code in an .Rmd chunk by typing > source("scripts/libraries.R")


if (!require('pacman')) install.packages('pacman'); library('pacman')
# remotes::install_github("MilesMcBain/breakerofchains")

# p_update(update = FALSE)  #Tells you which packages are out of date
# p_update()  #actually updates the out of date packages

pacman::p_load("tidyverse",#incldues dplyr, tidyr, tibble, ggplot2, stringr, etc.
               "ggpubr",
               "grid",
               "gtable",# for adding subtitle in ggplot2
               "ggthemes",
               "scales",# for function unit() in ggplot2
               "scales",# to add percentages to plots and pretty_breaks()
               "gridExtra",#for arranging graphs side by side w ftn grid.arrange()
               "skimr",# for summary statistics
               "readxl",
               "writexl",
               "ggpmisc",
               "tidypaleo",
               "here",
               "snotelr", #for SNOTEL
               "geosphere", #for SNOTEL
               "sf", #for gridMET
               "AOI",#for gridMET
               "climateR",#for gridMET
               #Packages for GAMS
               "mgcv",
               "gratia",
               "cowplot",
               "nlme",# for non linear mixed effects models
               "scam",
               "grid",# for unit.pmax(), unit.list()
               "schoenberg",
               "patchwork" #arrange plots)


# loading cowplot overrides the ggplot default theme: to set it back, run
theme_set(theme_bw())