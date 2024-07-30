rm(list=ls())

# NOTE: To load data, you must download both the extract's data and the DDI
# and also set the working directory to the folder with these files (or change the path below).

# if (!require("ipumsr")) stop("Reading IPUMS data into R requires the ipumsr package. It can be installed using the following command: install.packages('ipumsr')")

library(ipumsr)
library(labelled)
library(lubridate)
library(readxl)
library(tidyverse)
library(xlsx)

# import ipums raw data set
ddi <- read_ipums_ddi("cps_00008.xml")
raw_data <- read_ipums_micro(ddi)