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

# # import ipums raw data set 
# ddi <- read_ipums_ddi("cps_00006.xml")
# raw_data <- read_ipums_micro(ddi)

# import ipums clean data set by stata
stata <- read_excel('ipums.xlsx')
stata <- stata[,c('year', 'month', 'wtfinl', 'sex_new', 'race_new', 'educ_new', 'empstat_new')]

# # general info about the ipums dataset
# info <- ipums_var_info(raw_data)
# var <- colnames(raw_data)
# var_info <- ddi$var_info

#-------------------------------------------------------------------------------#                                                                            #
# Set up date format                                                            #                                                                          #
#-------------------------------------------------------------------------------#

date <- seq(ymd("1948-01-01"),Sys.Date(),by="months")-1

df <- data.frame(date)
df <- separate(df,date,into=c('year','month','day'),sep='-')
df$date <- date
df <- df[,c('date','year','month')]
df$datesv <- as.numeric(df$year) + ((as.numeric(df$month)-1)/12)

month_map <- c('january' = '01', 'february' = '02', 'march' = '03',
               'april' = '04', 'may' = '05', 'june' = '06',
               'july' = '07', 'august' = '08', 'september' = '09',
               'october' = '10', 'november' = '11', 'december' = '12')

stata$month_new <- month_map[stata$month]
stata <- stata[, c('year', 'month_new', 
                   'wtfinl', 'sex_new', 'race_new', 
                   'educ_new', 'empstat_new')]
colnames(stata)[2] <- 'month'

#-------------------------------------------------------------------------------#                                                                            
# Creating employment variables, levels                                         #
#                                                                               #
#     sex: m = male = 0, f = female = 1                                         #
#     race: w = white = 1, b = black = 2, l = hispan = 3                        #
#     educ: hs = hs or less = 1, s = some college, a = ba+                      #
#     empstat: e = employed = 1, u = unemployed = 2, n = not in labor force = 3 #
#-------------------------------------------------------------------------------#

# employed, male
m_el <- stata[stata$sex_new==0 & stata$empstat_new==1, c('year', 'month', 'wtfinl')]
m_el <- merge(df, m_el, by = c('year','month'))
m_el <- aggregate(wtfinl ~ date, data = m_el, FUN = sum)
colnames(m_el)[2] <- 'm_el'

# employed, male hs or less
m_hs_el <- stata[stata$sex_new==0 & stata$educ_new==1 & stata$empstat_new==1, c('year', 'month', 'wtfinl')]
m_hs_el <- merge(df, m_hs_el, by = c('year','month'))
m_hs_el <- aggregate(wtfinl ~ date, data = m_hs_el, FUN = sum)
colnames(m_hs_el)[2] <- 'm_hs_el'

# employed, male some college
m_s_el <- stata[stata$sex_new==0 & stata$educ_new==2 & stata$empstat_new==1, c('year', 'month', 'wtfinl')]
m_s_el <- merge(df, m_s_el, by = c('year','month'))
m_s_el <- aggregate(wtfinl ~ date, data = m_s_el, FUN = sum)
colnames(m_s_el)[2] <- 'm_s_el'

# employed, male ba+ 
m_ba_el <- stata[stata$sex_new==0 & stata$educ_new==3 & stata$empstat_new==1, c('year', 'month', 'wtfinl')]
m_ba_el <- merge(df, m_ba_el, by = c('year','month'))
m_ba_el <- aggregate(wtfinl ~ date, data = m_ba_el, FUN = sum)
colnames(m_ba_el)[2] <- 'm_ba_el'

# employed, female
f_el <- stata[stata$sex_new==1 & stata$empstat_new==1, c('year', 'month', 'wtfinl')]
f_el <- merge(df, f_el, by = c('year','month'))
f_el <- aggregate(wtfinl ~ date, data = f_el, FUN = sum)
colnames(f_el)[2] <- 'f_el'

# employed, female hs or less
f_hs_el <- stata[stata$sex_new==1 & stata$educ_new==1 & stata$empstat_new==1, c('year', 'month', 'wtfinl')]
f_hs_el <- merge(df, f_hs_el, by = c('year','month'))
f_hs_el <- aggregate(wtfinl ~ date, data = f_hs_el, FUN = sum)
colnames(f_hs_el)[2] <- 'f_hs_el'

# employed, female some college
f_s_el <- stata[stata$sex_new==1 & stata$educ_new==2 & stata$empstat_new==1, c('year', 'month', 'wtfinl')]
f_s_el <- merge(df, f_s_el, by = c('year','month'))
f_s_el <- aggregate(wtfinl ~ date, data = f_s_el, FUN = sum)
colnames(f_s_el)[2] <- 'f_s_el'

# employed, female ba+ 
f_ba_el <- stata[stata$sex_new==1 & stata$educ_new==3 & stata$empstat_new==1, c('year', 'month', 'wtfinl')]
f_ba_el <- merge(df, f_ba_el, by = c('year','month'))
f_ba_el <- aggregate(wtfinl ~ date, data = f_ba_el, FUN = sum)
colnames(f_ba_el)[2] <- 'f_ba_el'

# employed, total (male + female)
all_el <- merge(m_el, f_el, by = 'date') 
all_el$all_el <- all_el$m_el + all_el$f_el

# employed, total hs or less
hs_el <- merge(m_hs_el, f_hs_el, by = 'date') 
hs_el$hs_el <- hs_el$m_hs_el + hs_el$f_hs_el
hs_el <- hs_el[, c('date', 'hs_el')]

# employed, total some college
s_el <- merge(m_s_el, f_s_el, by = 'date') 
s_el$s_el <- s_el$m_s_el + s_el$f_s_el
s_el <- s_el[, c('date', 's_el')]

# employed, total ba+
ba_el <- merge(m_ba_el, f_ba_el, by = 'date') 
ba_el$ba_el <- ba_el$m_ba_el + ba_el$f_ba_el
ba_el <- ba_el[, c('date', 'ba_el')]

# employed, white
w_el <- stata[stata$race_new==1 & stata$empstat_new==1, c('year', 'month', 'wtfinl')]
w_el <- merge(df, w_el, by = c('year','month'))
w_el <- aggregate(wtfinl ~ date, data = w_el, FUN = sum)
colnames(w_el)[2] <- 'w_el'

# employed, white hs or less
w_hs_el <- stata[stata$race_new==1 & stata$educ_new==1 & stata$empstat_new==1, c('year', 'month', 'wtfinl')]
w_hs_el <- merge(df, w_hs_el, by = c('year','month'))
w_hs_el <- aggregate(wtfinl ~ date, data = w_hs_el, FUN = sum)
colnames(w_hs_el)[2] <- 'w_hs_el'

# employed, white some college
w_s_el <- stata[stata$race_new==1 & stata$educ_new==2 & stata$empstat_new==1, c('year', 'month', 'wtfinl')]
w_s_el <- merge(df, w_s_el, by = c('year','month'))
w_s_el <- aggregate(wtfinl ~ date, data = w_s_el, FUN = sum)
colnames(w_s_el)[2] <- 'w_s_el'

# employed, white ba+ 
w_ba_el <- stata[stata$race_new==1 & stata$educ_new==3 & stata$empstat_new==1, c('year', 'month', 'wtfinl')]
w_ba_el <- merge(df, w_ba_el, by = c('year','month'))
w_ba_el <- aggregate(wtfinl ~ date, data = w_ba_el, FUN = sum)
colnames(w_ba_el)[2] <- 'w_ba_el'

# employed, black
b_el <- stata[stata$race_new==2 & stata$empstat_new==1, c('year', 'month', 'wtfinl')]
b_el <- merge(df, b_el, by = c('year','month'))
b_el <- aggregate(wtfinl ~ date, data = b_el, FUN = sum)
colnames(b_el)[2] <- 'b_el'

# employed, black hs or less
b_hs_el <- stata[stata$race_new==2 & stata$educ_new==1 & stata$empstat_new==1, c('year', 'month', 'wtfinl')]
b_hs_el <- merge(df, b_hs_el, by = c('year','month'))
b_hs_el <- aggregate(wtfinl ~ date, data = b_hs_el, FUN = sum)
colnames(b_hs_el)[2] <- 'b_hs_el'

# employed, black some college
b_s_el <- stata[stata$race_new==2 & stata$educ_new==2 & stata$empstat_new==1, c('year', 'month', 'wtfinl')]
b_s_el <- merge(df, b_s_el, by = c('year','month'))
b_s_el <- aggregate(wtfinl ~ date, data = b_s_el, FUN = sum)
colnames(b_s_el)[2] <- 'b_s_el'

# employed, black ba+ 
b_ba_el <- stata[stata$race_new==2 & stata$educ_new==3 & stata$empstat_new==1, c('year', 'month', 'wtfinl')]
b_ba_el <- merge(df, b_ba_el, by = c('year','month'))
b_ba_el <- aggregate(wtfinl ~ date, data = b_ba_el, FUN = sum)
colnames(b_ba_el)[2] <- 'b_ba_el'

# employed, hispan
l_el <- stata[stata$race_new==3 & stata$empstat_new==1, c('year', 'month', 'wtfinl')]
l_el <- merge(df, l_el, by = c('year','month'))
l_el <- aggregate(wtfinl ~ date, data = l_el, FUN = sum)
colnames(l_el)[2] <- 'l_el'

# employed, hispan hs or less
l_hs_el <- stata[stata$race_new==3 & stata$educ_new==1 & stata$empstat_new==1, c('year', 'month', 'wtfinl')]
l_hs_el <- merge(df, l_hs_el, by = c('year','month'))
l_hs_el <- aggregate(wtfinl ~ date, data = l_hs_el, FUN = sum)
colnames(l_hs_el)[2] <- 'l_hs_el'

# employed, hispan some college
l_s_el <- stata[stata$race_new==3 & stata$educ_new==2 & stata$empstat_new==1, c('year', 'month', 'wtfinl')]
l_s_el <- merge(df, l_s_el, by = c('year','month'))
l_s_el <- aggregate(wtfinl ~ date, data = l_s_el, FUN = sum)
colnames(l_s_el)[2] <- 'l_s_el'

# employed, hispan ba+
l_ba_el <- stata[stata$race_new==3 & stata$educ_new==3 & stata$empstat_new==1, c('year', 'month', 'wtfinl')]
l_ba_el <- merge(df, l_ba_el, by = c('year','month'))
l_ba_el <- aggregate(wtfinl ~ date, data = l_ba_el, FUN = sum)
colnames(l_ba_el)[2] <- 'l_ba_el'

# unemployed, male
m_ul <- stata[stata$sex_new==0 & stata$empstat_new==2, c('year', 'month', 'wtfinl')]
m_ul <- merge(df, m_ul, by = c('year','month'))
m_ul <- aggregate(wtfinl ~ date, data = m_ul, FUN = sum)
colnames(m_ul)[2] <- 'm_ul'

# unemployed, male hs or less
m_hs_ul <- stata[stata$sex_new==0 & stata$educ_new==1 & stata$empstat_new==2, c('year', 'month', 'wtfinl')]
m_hs_ul <- merge(df, m_hs_ul, by = c('year','month'))
m_hs_ul <- aggregate(wtfinl ~ date, data = m_hs_ul, FUN = sum)
colnames(m_hs_ul)[2] <- 'm_hs_ul'

# unemployed, male some college
m_s_ul <- stata[stata$sex_new==0 & stata$educ_new==2 & stata$empstat_new==2, c('year', 'month', 'wtfinl')]
m_s_ul <- merge(df, m_s_ul, by = c('year','month'))
m_s_ul <- aggregate(wtfinl ~ date, data = m_s_ul, FUN = sum)
colnames(m_s_ul)[2] <- 'm_s_ul'

# unemployed, male ba+ 
m_ba_ul <- stata[stata$sex_new==0 & stata$educ_new==3 & stata$empstat_new==2, c('year', 'month', 'wtfinl')]
m_ba_ul <- merge(df, m_ba_ul, by = c('year','month'))
m_ba_ul <- aggregate(wtfinl ~ date, data = m_ba_ul, FUN = sum)
colnames(m_ba_ul)[2] <- 'm_ba_ul'

# unemployed, female
f_ul <- stata[stata$sex_new==1 & stata$empstat_new==2, c('year', 'month', 'wtfinl')]
f_ul <- merge(df, f_ul, by = c('year','month'))
f_ul <- aggregate(wtfinl ~ date, data = f_ul, FUN = sum)
colnames(f_ul)[2] <- 'f_ul'

# unemployed, female hs or less
f_hs_ul <- stata[stata$sex_new==1 & stata$educ_new==1 & stata$empstat_new==2, c('year', 'month', 'wtfinl')]
f_hs_ul <- merge(df, f_hs_ul, by = c('year','month'))
f_hs_ul <- aggregate(wtfinl ~ date, data = f_hs_ul, FUN = sum)
colnames(f_hs_ul)[2] <- 'f_hs_ul'

# unemployed, female some college
f_s_ul <- stata[stata$sex_new==1 & stata$educ_new==2 & stata$empstat_new==2, c('year', 'month', 'wtfinl')]
f_s_ul <- merge(df, f_s_ul, by = c('year','month'))
f_s_ul <- aggregate(wtfinl ~ date, data = f_s_ul, FUN = sum)
colnames(f_s_ul)[2] <- 'f_s_ul'

# unemployed, female ba+ 
f_ba_ul <- stata[stata$sex_new==1 & stata$educ_new==3 & stata$empstat_new==2, c('year', 'month', 'wtfinl')]
f_ba_ul <- merge(df, f_ba_ul, by = c('year','month'))
f_ba_ul <- aggregate(wtfinl ~ date, data = f_ba_ul, FUN = sum)
colnames(f_ba_ul)[2] <- 'f_ba_ul'

# unemployed, total (male + female)
all_ul <- merge(m_ul, f_ul, by = 'date') 
all_ul$all_ul <- all_ul$m_ul + all_ul$f_ul
all_ul <- all_ul[, c('date', 'all_ul')]

# unemployed, total hs or less
hs_ul <- stata[stata$educ_new==1 & stata$empstat_new==2, c('year', 'month', 'wtfinl')]
hs_ul <- merge(df, hs_ul, by = c('year','month'))
hs_ul <- aggregate(wtfinl ~ date, data = hs_ul, FUN = sum)
colnames(hs_ul)[2] <- 'hs_ul'

# unemployed, total some college
s_ul <- stata[stata$educ_new==2 & stata$empstat_new==2, c('year', 'month', 'wtfinl')]
s_ul <- merge(df, s_ul, by = c('year','month'))
s_ul <- aggregate(wtfinl ~ date, data = s_ul, FUN = sum)
colnames(s_ul)[2] <- 's_ul'

# unemployed, ba+
ba_ul <- stata[stata$educ_new==3 & stata$empstat_new==2, c('year', 'month', 'wtfinl')]
ba_ul <- merge(df, ba_ul, by = c('year','month'))
ba_ul <- aggregate(wtfinl ~ date, data = ba_ul, FUN = sum)
colnames(ba_ul)[2] <- 'ba_ul'

# unemployed, white
w_ul <- stata[stata$race_new==1 & stata$empstat_new==2, c('year', 'month', 'wtfinl')]
w_ul <- merge(df, w_ul, by = c('year','month'))
w_ul <- aggregate(wtfinl ~ date, data = w_ul, FUN = sum)
colnames(w_ul)[2] <- 'w_ul'

# unemployed, white hs or less
w_hs_ul <- stata[stata$race_new==1 & stata$educ_new==1 & stata$empstat_new==2, c('year', 'month', 'wtfinl')]
w_hs_ul <- merge(df, w_hs_ul, by = c('year','month'))
w_hs_ul <- aggregate(wtfinl ~ date, data = w_hs_ul, FUN = sum)
colnames(w_hs_ul)[2] <- 'w_hs_ul'

# unemployed, white some college
w_s_ul <- stata[stata$race_new==1 & stata$educ_new==2 & stata$empstat_new==2, c('year', 'month', 'wtfinl')]
w_s_ul <- merge(df, w_s_ul, by = c('year','month'))
w_s_ul <- aggregate(wtfinl ~ date, data = w_s_ul, FUN = sum)
colnames(w_s_ul)[2] <- 'w_s_ul'

# unemployed, white ba+ 
w_ba_ul <- stata[stata$race_new==1 & stata$educ_new==3 & stata$empstat_new==2, c('year', 'month', 'wtfinl')]
w_ba_ul <- merge(df, w_ba_ul, by = c('year','month'))
w_ba_ul <- aggregate(wtfinl ~ date, data = w_ba_ul, FUN = sum)
colnames(w_ba_ul)[2] <- 'w_ba_ul'

# unemployed, black
b_ul <- stata[stata$race_new==2 & stata$empstat_new==2, c('year', 'month', 'wtfinl')]
b_ul <- merge(df, b_ul, by = c('year','month'))
b_ul <- aggregate(wtfinl ~ date, data = b_ul, FUN = sum)
colnames(b_ul)[2] <- 'b_ul'

# unemployed, black hs or less
b_hs_ul <- stata[stata$race_new==2 & stata$educ_new==1 & stata$empstat_new==2, c('year', 'month', 'wtfinl')]
b_hs_ul <- merge(df, b_hs_ul, by = c('year','month'))
b_hs_ul <- aggregate(wtfinl ~ date, data = b_hs_ul, FUN = sum)
colnames(b_hs_ul)[2] <- 'b_hs_ul'

# unemployed, black some college
b_s_ul <- stata[stata$race_new==2 & stata$educ_new==2 & stata$empstat_new==2, c('year', 'month', 'wtfinl')]
b_s_ul <- merge(df, b_s_ul, by = c('year','month'))
b_s_ul <- aggregate(wtfinl ~ date, data = b_s_ul, FUN = sum)
colnames(b_s_ul)[2] <- 'b_s_ul'

# unemployed, black ba+ 
b_ba_ul <- stata[stata$race_new==2 & stata$educ_new==3 & stata$empstat_new==2, c('year', 'month', 'wtfinl')]
b_ba_ul <- merge(df, b_ba_ul, by = c('year','month'))
b_ba_ul <- aggregate(wtfinl ~ date, data = b_ba_ul, FUN = sum)
colnames(b_ba_ul)[2] <- 'b_ba_ul'

# unemployed, hispan
l_ul <- stata[stata$race_new==3 & stata$empstat_new==2, c('year', 'month', 'wtfinl')]
l_ul <- merge(df, l_ul, by = c('year','month'))
l_ul <- aggregate(wtfinl ~ date, data = l_ul, FUN = sum)
colnames(l_ul)[2] <- 'l_ul'

# unemployed, hispan hs or less
l_hs_ul <- stata[stata$race_new==3 & stata$educ_new==1 & stata$empstat_new==2, c('year', 'month', 'wtfinl')]
l_hs_ul <- merge(df, l_hs_ul, by = c('year','month'))
l_hs_ul <- aggregate(wtfinl ~ date, data = l_hs_ul, FUN = sum)
colnames(l_hs_ul)[2] <- 'l_hs_ul'

# unemployed, hispan some college
l_s_ul <- stata[stata$race_new==3 & stata$educ_new==2 & stata$empstat_new==2, c('year', 'month', 'wtfinl')]
l_s_ul <- merge(df, l_s_ul, by = c('year','month'))
l_s_ul <- aggregate(wtfinl ~ date, data = l_s_ul, FUN = sum)
colnames(l_s_ul)[2] <- 'l_s_ul'

# unemployed, hispan ba+
l_ba_ul <- stata[stata$race_new==3 & stata$educ_new==3 & stata$empstat_new==2, c('year', 'month', 'wtfinl')]
l_ba_ul <- merge(df, l_ba_ul, by = c('year','month'))
l_ba_ul <- aggregate(wtfinl ~ date, data = l_ba_ul, FUN = sum)
colnames(l_ba_ul)[2] <- 'l_ba_ul'

# labor force, total
all_lf <- merge(all_el, all_ul, by = 'date')
all_lf <- all_lf[, c('date', 'all_el', 'all_ul')]
all_lf$all_lf <- all_lf$all_el + all_lf$all_ul
all_lf <- all_lf[, c('date', 'all_lf')]

# labor force, male
m_lf <- merge(m_el, m_ul, by = 'date')
m_lf$m_lf <- m_lf$m_el + m_lf$m_ul
m_lf <- m_lf[, c('date', 'm_lf')]

# labor force, male hs or less
m_hs_lf <- merge(m_hs_el, m_hs_ul, by = 'date')
m_hs_lf$m_hs_lf <- m_hs_lf$m_hs_el + m_hs_lf$m_hs_ul
m_hs_lf <- m_hs_lf[, c('date', 'm_hs_lf')]

# labor force, male some college
m_s_lf <- merge(m_s_el, m_s_ul, by = 'date')
m_s_lf$m_s_lf <- m_s_lf$m_s_el + m_s_lf$m_s_ul
m_s_lf <- m_s_lf[, c('date', 'm_s_lf')]

# labor force, male ba+
m_ba_lf <- merge(m_ba_el, m_ba_ul, by = 'date')
m_ba_lf$m_ba_lf <- m_ba_lf$m_ba_el + m_ba_lf$m_ba_ul
m_ba_lf <- m_ba_lf[, c('date', 'm_ba_lf')]

# labor force, female
f_lf <- merge(f_el, f_ul, by = 'date')
f_lf$f_lf <- f_lf$f_el + f_lf$f_ul
f_lf <- f_lf[, c('date', 'f_lf')]

# labor force, female hs or less
f_hs_lf <- merge(f_hs_el, f_hs_ul, by = 'date')
f_hs_lf$f_hs_lf <- f_hs_lf$f_hs_el + f_hs_lf$f_hs_ul
f_hs_lf <- f_hs_lf[, c('date', 'f_hs_lf')]

# labor force, female some college
f_s_lf <- merge(f_s_el, f_s_ul, by = 'date')
f_s_lf$f_s_lf <- f_s_lf$f_s_el + f_s_lf$f_s_ul
f_s_lf <- f_s_lf[, c('date', 'f_s_lf')]

# labor force, female ba+
f_ba_lf <- merge(f_ba_el, f_ba_ul, by = 'date')
f_ba_lf$f_ba_lf <- f_ba_lf$f_ba_el + f_ba_lf$f_ba_ul
f_ba_lf <- f_ba_lf[, c('date', 'f_ba_lf')]

# labor force, total hs or less
hs_lf <- merge(hs_el, hs_ul, by = 'date')
hs_lf$hs_lf <- hs_lf$hs_el + hs_lf$hs_ul
hs_lf <- hs_lf[, c('date', 'hs_lf')]

# labor force, total some college
s_lf <- merge(s_el, s_ul, by = 'date')
s_lf$s_lf <- s_lf$s_el + s_lf$s_ul
s_lf <- s_lf[, c('date', 's_lf')]

# labor force, ba+
ba_lf <- merge(ba_el, ba_ul, by = 'date')
ba_lf$ba_lf <- ba_lf$ba_el + ba_lf$ba_ul
ba_lf <- ba_lf[, c('date', 'ba_lf')]

# labor force, white
w_lf <- merge(w_el, w_ul, by = 'date')
w_lf$w_lf <- w_lf$w_el + w_lf$w_ul
w_lf <- w_lf[, c('date', 'w_lf')]

# labor force, white hs or less
w_hs_lf <- merge(w_hs_el, w_hs_ul, by = 'date')
w_hs_lf$w_hs_lf <- w_hs_lf$w_hs_el + w_hs_lf$w_hs_ul
w_hs_lf <- w_hs_lf[, c('date', 'w_hs_lf')]

# labor force, white some college
w_s_lf <- merge(w_s_el, w_s_ul, by = 'date')
w_s_lf$w_s_lf <- w_s_lf$w_s_el + w_s_lf$w_s_ul
w_s_lf <- w_s_lf[, c('date', 'w_s_lf')]

# labor force, white ba+ 
w_ba_lf <- merge(w_ba_el, w_ba_ul, by = 'date')
w_ba_lf$w_ba_lf <- w_ba_lf$w_ba_el + w_ba_lf$w_ba_ul
w_ba_lf <- w_ba_lf[, c('date', 'w_ba_lf')]

# labor force, black
b_lf <- merge(b_el, b_ul, by = 'date')
b_lf$b_lf <- b_lf$b_el + b_lf$b_ul
b_lf <- b_lf[, c('date', 'b_lf')]

# labor force, black hs or less
b_hs_lf <- merge(b_hs_el, b_hs_ul, by = 'date')
b_hs_lf$b_hs_lf <- b_hs_lf$b_hs_el + b_hs_lf$b_hs_ul
b_hs_lf <- b_hs_lf[, c('date', 'b_hs_lf')]

# labor force, black some college
b_s_lf <- merge(b_s_el, b_s_ul, by = 'date')
b_s_lf$b_s_lf <- b_s_lf$b_s_el + b_s_lf$b_s_ul
b_s_lf <- b_s_lf[, c('date', 'b_s_lf')]

# labor force, black ba+ 
b_ba_lf <- merge(b_ba_el, b_ba_ul, by = 'date')
b_ba_lf$b_ba_lf <- b_ba_lf$b_ba_el + b_ba_lf$b_ba_ul
b_ba_lf <- b_ba_lf[, c('date', 'b_ba_lf')]

# labor force, hispan
l_lf <- merge(l_el, l_ul, by = 'date')
l_lf$l_lf <- l_lf$l_el + l_lf$l_ul
l_lf <- l_lf[, c('date', 'l_lf')]

# labor force, hispan hs or less
l_hs_lf <- merge(l_hs_el, l_hs_ul, by = 'date')
l_hs_lf$l_hs_lf <- l_hs_lf$l_hs_el + l_hs_lf$l_hs_ul
l_hs_lf <- l_hs_lf[, c('date', 'l_hs_lf')]

# labor force, hispan some college
l_s_lf <- merge(l_s_el, l_s_ul, by = 'date')
l_s_lf$l_s_lf <- l_s_lf$l_s_el + l_s_lf$l_s_ul
l_s_lf <- l_s_lf[, c('date', 'l_s_lf')]

# labor force, hispan ba+ 
l_ba_lf <- merge(l_ba_el, l_ba_ul, by = 'date')
l_ba_lf$l_ba_lf <- l_ba_lf$l_ba_el + l_ba_lf$l_ba_ul
l_ba_lf <- l_ba_lf[, c('date', 'l_ba_lf')]

# not in labor force, male
m_nilf <- stata[stata$sex_new==0 & stata$empstat_new==3, c('year', 'month', 'wtfinl')]
m_nilf <- merge(df, m_nilf, by = c('year','month'))
m_nilf <- aggregate(wtfinl ~ date, data = m_nilf, FUN = sum)
colnames(m_nilf)[2] <- 'm_nilf'

# not in labor force, male hs or less
m_hs_nilf <- stata[stata$sex_new==0 & stata$educ_new==1 & stata$empstat_new==3, c('year', 'month', 'wtfinl')]
m_hs_nilf <- merge(df, m_hs_nilf, by = c('year','month'))
m_hs_nilf <- aggregate(wtfinl ~ date, data = m_hs_nilf, FUN = sum)
colnames(m_hs_nilf)[2] <- 'm_hs_nilf'

# not in labor force, male some college
m_s_nilf <- stata[stata$sex_new==0 & stata$educ_new==2 & stata$empstat_new==3, c('year', 'month', 'wtfinl')]
m_s_nilf <- merge(df, m_s_nilf, by = c('year','month'))
m_s_nilf <- aggregate(wtfinl ~ date, data = m_s_nilf, FUN = sum)
colnames(m_s_nilf)[2] <- 'm_s_nilf'

# not in labor force, male ba+ 
m_ba_nilf <- stata[stata$sex_new==0 & stata$educ_new==3 & stata$empstat_new==3, c('year', 'month', 'wtfinl')]
m_ba_nilf <- merge(df, m_ba_nilf, by = c('year','month'))
m_ba_nilf <- aggregate(wtfinl ~ date, data = m_ba_nilf, FUN = sum)
colnames(m_ba_nilf)[2] <- 'm_ba_nilf'

# not in labor force, female
f_nilf <- stata[stata$sex_new==1 & stata$empstat_new==3, c('year', 'month', 'wtfinl')]
f_nilf <- merge(df, f_nilf, by = c('year','month'))
f_nilf <- aggregate(wtfinl ~ date, data = f_nilf, FUN = sum)
colnames(f_nilf)[2] <- 'f_nilf'

# not in labor force, female hs or less
f_hs_nilf <- stata[stata$sex_new==1 & stata$educ_new==1 & stata$empstat_new==3, c('year', 'month', 'wtfinl')]
f_hs_nilf <- merge(df, f_hs_nilf, by = c('year','month'))
f_hs_nilf <- aggregate(wtfinl ~ date, data = f_hs_nilf, FUN = sum)
colnames(f_hs_nilf)[2] <- 'f_hs_nilf'

# not in labor force, female some college
f_s_nilf <- stata[stata$sex_new==1 & stata$educ_new==2 & stata$empstat_new==3, c('year', 'month', 'wtfinl')]
f_s_nilf <- merge(df, f_s_nilf, by = c('year','month'))
f_s_nilf <- aggregate(wtfinl ~ date, data = f_s_nilf, FUN = sum)
colnames(f_s_nilf)[2] <- 'f_s_nilf'

# not in labor force, female ba+ 
f_ba_nilf <- stata[stata$sex_new==1 & stata$educ_new==3 & stata$empstat_new==3, c('year', 'month', 'wtfinl')]
f_ba_nilf <- merge(df, f_ba_nilf, by = c('year','month'))
f_ba_nilf <- aggregate(wtfinl ~ date, data = f_ba_nilf, FUN = sum)
colnames(f_ba_nilf)[2] <- 'f_ba_nilf'

# not in labor force, total (male + female)
all_nilf <- merge(m_nilf, f_nilf, by = 'date') 
all_nilf$all_nilf <- all_nilf$m_nilf + all_nilf$f_nilf

# not in labor force, total hs or less
hs_nilf <- merge(m_hs_nilf, f_hs_nilf, by = 'date') 
hs_nilf$hs_nilf <- hs_nilf$m_hs_nilf + hs_nilf$f_hs_nilf
hs_nilf <- hs_nilf[, c('date', 'hs_nilf')]

# not in labor force, total some college
s_nilf <- merge(m_s_nilf, f_s_nilf, by = 'date') 
s_nilf$s_nilf <- s_nilf$m_s_nilf + s_nilf$f_s_nilf
s_nilf <- s_nilf[, c('date', 's_nilf')]

# not in labor force, total ba+
ba_nilf  <- merge(m_ba_nilf , f_ba_nilf , by = 'date') 
ba_nilf$ba_nilf <- ba_nilf$m_ba_nilf + ba_nilf$f_ba_nilf
ba_nilf <- ba_nilf[, c('date', 'ba_nilf')]

# not in labor force, white
w_nilf <- stata[stata$race_new==1 & stata$empstat_new==3, c('year', 'month', 'wtfinl')]
w_nilf <- merge(df, w_nilf, by = c('year','month'))
w_nilf <- aggregate(wtfinl ~ date, data = w_nilf, FUN = sum)
colnames(w_nilf)[2] <- 'w_nilf'

# not in labor force, white hs or less
w_hs_nilf <- stata[stata$race_new==1 & stata$educ_new==1 & stata$empstat_new==3, c('year', 'month', 'wtfinl')]
w_hs_nilf <- merge(df, w_hs_nilf, by = c('year','month'))
w_hs_nilf <- aggregate(wtfinl ~ date, data = w_hs_nilf, FUN = sum)
colnames(w_hs_nilf)[2] <- 'w_hs_nilf'

# not in labor force, white some college
w_s_nilf <- stata[stata$race_new==1 & stata$educ_new==2 & stata$empstat_new==3, c('year', 'month', 'wtfinl')]
w_s_nilf <- merge(df, w_s_nilf, by = c('year','month'))
w_s_nilf <- aggregate(wtfinl ~ date, data = w_s_nilf, FUN = sum)
colnames(w_s_nilf)[2] <- 'w_s_nilf'

# not in labor force, white ba+ 
w_ba_nilf <- stata[stata$race_new==1 & stata$educ_new==3 & stata$empstat_new==3, c('year', 'month', 'wtfinl')]
w_ba_nilf <- merge(df, w_ba_nilf, by = c('year','month'))
w_ba_nilf <- aggregate(wtfinl ~ date, data = w_ba_nilf, FUN = sum)
colnames(w_ba_nilf)[2] <- 'w_ba_nilf'

# not in labor force, black
b_nilf <- stata[stata$race_new==2 & stata$empstat_new==3, c('year', 'month', 'wtfinl')]
b_nilf <- merge(df, b_nilf, by = c('year','month'))
b_nilf <- aggregate(wtfinl ~ date, data = b_nilf, FUN = sum)
colnames(b_nilf)[2] <- 'b_nilf'

# not in labor force, black hs or less
b_hs_nilf <- stata[stata$race_new==2 & stata$educ_new==1 & stata$empstat_new==3, c('year', 'month', 'wtfinl')]
b_hs_nilf <- merge(df, b_hs_nilf, by = c('year','month'))
b_hs_nilf <- aggregate(wtfinl ~ date, data = b_hs_nilf, FUN = sum)
colnames(b_hs_nilf)[2] <- 'b_hs_nilf'

# not in labor force, black some college
b_s_nilf <- stata[stata$race_new==2 & stata$educ_new==2 & stata$empstat_new==3, c('year', 'month', 'wtfinl')]
b_s_nilf <- merge(df, b_s_nilf, by = c('year','month'))
b_s_nilf <- aggregate(wtfinl ~ date, data = b_s_nilf, FUN = sum)
colnames(b_s_nilf)[2] <- 'b_s_nilf'

# not in labor force, black ba+ 
b_ba_nilf <- stata[stata$race_new==2 & stata$educ_new==3 & stata$empstat_new==3, c('year', 'month', 'wtfinl')]
b_ba_nilf <- merge(df, b_ba_nilf, by = c('year','month'))
b_ba_nilf <- aggregate(wtfinl ~ date, data = b_ba_nilf, FUN = sum)
colnames(b_ba_nilf)[2] <- 'b_ba_nilf'

# not in labor force, hispan
l_nilf <- stata[stata$race_new==3 & stata$empstat_new==3, c('year', 'month', 'wtfinl')]
l_nilf <- merge(df, l_nilf, by = c('year','month'))
l_nilf <- aggregate(wtfinl ~ date, data = l_nilf, FUN = sum)
colnames(l_nilf)[2] <- 'l_nilf'

# not in labor force, hispan hs or less
l_hs_nilf <- stata[stata$race_new==3 & stata$educ_new==1 & stata$empstat_new==3, c('year', 'month', 'wtfinl')]
l_hs_nilf <- merge(df, l_hs_nilf, by = c('year','month'))
l_hs_nilf <- aggregate(wtfinl ~ date, data = l_hs_nilf, FUN = sum)
colnames(l_hs_nilf)[2] <- 'l_hs_nilf'

# not in labor force, hispan some college
l_s_nilf <- stata[stata$race_new==3 & stata$educ_new==2 & stata$empstat_new==3, c('year', 'month', 'wtfinl')]
l_s_nilf <- merge(df, l_s_nilf, by = c('year','month'))
l_s_nilf <- aggregate(wtfinl ~ date, data = l_s_nilf, FUN = sum)
colnames(l_s_nilf)[2] <- 'l_s_nilf'

# not in labor force, hispan ba+
l_ba_nilf  <- stata[stata$race_new==3 & stata$educ_new==3 & stata$empstat_new==3, c('year', 'month', 'wtfinl')]
l_ba_nilf <- merge(df, l_ba_nilf, by = c('year','month'))
l_ba_nilf <- aggregate(wtfinl ~ date, data = l_ba_nilf, FUN = sum)
colnames(l_ba_nilf)[2] <- 'l_ba_nilf'

# civilian noninstitutional population aged 16 and higher, total
all_p <- merge(all_lf, all_nilf, by = 'date')
all_p <- all_p[, c('date', 'all_lf', 'all_nilf')]
all_p$all_p <- all_p$all_lf + all_p$all_nilf
all_p <- all_p[, c('date', 'all_p')]

# population, male
m_p <- merge(m_lf, m_nilf, by = 'date')
m_p$m_p <- m_p$m_lf + m_p$m_nilf
m_p <- m_p[, c('date', 'm_p')]

# population, male hs or less
m_hs_p <- merge(m_hs_lf, m_hs_nilf, by = 'date')
m_hs_p$m_hs_p <- m_hs_p$m_hs_lf + m_hs_p$m_hs_nilf
m_hs_p <- m_hs_p[, c('date', 'm_hs_p')]

# population, male some college
m_s_p <- merge(m_s_lf, m_s_nilf, by = 'date')
m_s_p$m_s_p <- m_s_p$m_s_lf + m_s_p$m_s_nilf
m_s_p <- m_s_p[, c('date', 'm_s_p')]

# population, male ba+
m_ba_p <- merge(m_ba_lf, m_ba_nilf, by = 'date')
m_ba_p$m_ba_p <- m_ba_p$m_ba_lf + m_ba_p$m_ba_nilf
m_ba_p <- m_ba_p[, c('date', 'm_ba_p')]

# population, female
f_p <- merge(f_lf, f_nilf, by = 'date')
f_p$f_p <- f_p$f_lf + f_p$f_nilf
f_p <- f_p[, c('date', 'f_p')]

# population, female hs or less
f_hs_p <- merge(f_hs_lf, f_hs_nilf, by = 'date')
f_hs_p$f_hs_p <- f_hs_p$f_hs_lf + f_hs_p$f_hs_nilf
f_hs_p <- f_hs_p[, c('date', 'f_hs_p')]

# population, female some college
f_s_p <- merge(f_s_lf, f_s_nilf, by = 'date')
f_s_p$f_s_p <- f_s_p$f_s_lf + f_s_p$f_s_nilf
f_s_p <- f_s_p[, c('date', 'f_s_p')]

# population, female ba+
f_ba_p <- merge(f_ba_lf, f_ba_nilf, by = 'date')
f_ba_p$f_ba_p <- f_ba_p$f_ba_lf + f_ba_p$f_ba_nilf
f_ba_p <- f_ba_p[, c('date', 'f_ba_p')]

# population, total hs or less
hs_p <- merge(hs_lf, hs_nilf, by = 'date')
hs_p$hs_p <- hs_p$hs_lf + hs_p$hs_nilf
hs_p <- hs_p[, c('date', 'hs_p')]

# population, total some college
s_p <- merge(s_lf, s_nilf, by = 'date')
s_p$s_p <- s_p$s_lf + s_p$s_nilf
s_p <- s_p[, c('date', 's_p')]

# population, ba+
ba_p <- merge(ba_lf, ba_nilf, by = 'date')
ba_p$ba_p <- ba_p$ba_lf + ba_p$ba_nilf
ba_p <- ba_p[, c('date', 'ba_p')]

# population, white
w_p <- merge(w_lf, w_nilf, by = 'date')
w_p$w_p <- w_p$w_lf + w_p$w_nilf
w_p <- w_p[, c('date', 'w_p')]

# population, white hs or less
w_hs_p <- merge(w_hs_lf, w_hs_nilf, by = 'date')
w_hs_p$w_hs_p <- w_hs_p$w_hs_lf + w_hs_p$w_hs_nilf
w_hs_p <- w_hs_p[, c('date', 'w_hs_p')]

# population, white some college
w_s_p <- merge(w_s_lf, w_s_nilf, by = 'date')
w_s_p$w_s_p <- w_s_p$w_s_lf + w_s_p$w_s_nilf
w_s_p <- w_s_p[, c('date', 'w_s_p')]

# population, white ba+ 
w_ba_p <- merge(w_ba_lf, w_ba_nilf, by = 'date')
w_ba_p$w_ba_p <- w_ba_p$w_ba_lf + w_ba_p$w_ba_nilf
w_ba_p <- w_ba_p[, c('date', 'w_ba_p')]

# population, black
b_p <- merge(b_lf, b_nilf, by = 'date')
b_p$b_p <- b_p$b_lf + b_p$b_nilf
b_p <- b_p[, c('date', 'b_p')]

# population, black hs or less
b_hs_p <- merge(b_hs_lf, b_hs_nilf, by = 'date')
b_hs_p$b_hs_p <- b_hs_p$b_hs_lf + b_hs_p$b_hs_nilf
b_hs_p <- b_hs_p[, c('date', 'b_hs_p')]

# population, black some college
b_s_p <- merge(b_s_lf, b_s_nilf, by = 'date')
b_s_p$b_s_p <- b_s_p$b_s_lf + b_s_p$b_s_nilf
b_s_p <- b_s_p[, c('date', 'b_s_p')]

# population, black ba+ 
b_ba_p <- merge(b_ba_lf, b_ba_nilf, by = 'date')
b_ba_p$b_ba_p <- b_ba_p$b_ba_lf + b_ba_p$b_ba_nilf
b_ba_p <- b_ba_p[, c('date', 'b_ba_p')]

# population, hispan
l_p <- merge(l_lf, l_nilf, by = 'date')
l_p$l_p <- l_p$l_lf + l_p$l_nilf
l_p <- l_p[, c('date', 'l_p')]

# population, hispan hs or less
l_hs_p <- merge(l_hs_lf, l_hs_nilf, by = 'date')
l_hs_p$l_hs_p <- l_hs_p$l_hs_lf + l_hs_p$l_hs_nilf
l_hs_p <- l_hs_p[, c('date', 'l_hs_p')]

# population, hispan some college
l_s_p <- merge(l_s_lf, l_s_nilf, by = 'date')
l_s_p$l_s_p <- l_s_p$l_s_lf + l_s_p$l_s_nilf
l_s_p <- l_s_p[, c('date', 'l_s_p')]

# population, hispan ba+ 
l_ba_p <- merge(l_ba_lf, l_ba_nilf, by = 'date')
l_ba_p$l_ba_p <- l_ba_p$l_ba_lf + l_ba_p$l_ba_nilf
l_ba_p <- l_ba_p[, c('date', 'l_ba_p')]

#-------------------------------------------------------------------------------#                                                                            
# Creating unemployment rate                                                    #                                                                          
#-------------------------------------------------------------------------------#

u.series <- c(, # all, male, female 
              ) # white, black, hispan

u.df <- 

colnames(u.df) <- c('date', 
                    'all_u','m_u','f_u', # all, male, female
                    'w_u','b_u','l_u') # white, black, hispan

u.df <- merge(u.df, hs_ul, by = 'date') # all educ 
u.df$hs_u <- round((hs_ul$hs_ul/hs_lf$hs_lf)*100,1)
u.df$s_u <- round((s_ul$s_ul/s_lf$s_lf)*100,1)
u.df$ba_u <- round((ba_ul$ba_ul/ba_lf$ba_lf)*100,1)

u.df$m_hs_u <- round((m_hs_ul$m_hs_ul/m_hs_lf$m_hs_lf)*100,1) # male 
u.df$m_s_u <- round((m_s_ul$m_s_ul/m_s_lf$m_s_lf)*100,1)
u.df$m_ba_u <- round((m_ba_ul$m_ba_ul/m_ba_lf$m_ba_lf)*100,1)

u.df$f_hs_u <- round((f_hs_ul$f_hs_ul/f_hs_lf$f_hs_lf)*100,1) # female 
u.df$f_s_u <- round((f_s_ul$f_s_ul/f_s_lf$f_s_lf)*100,1)
u.df$f_ba_u <- round((f_ba_ul$f_ba_ul/f_ba_lf$f_ba_lf)*100,1)

u.df$w_hs_u <- round((w_hs_ul$w_hs_ul/w_hs_lf$w_hs_lf)*100,1) # white 
u.df$w_s_u <- round((w_s_ul$w_s_ul/w_s_lf$w_s_lf)*100,1)
u.df$w_ba_u <- round((w_ba_ul$w_ba_ul/w_ba_lf$w_ba_lf)*100,1)

u.df$b_hs_u <- round((b_hs_ul$b_hs_ul/b_hs_lf$b_hs_lf)*100,1) # black 
u.df$b_s_u <- round((b_s_ul$b_s_ul/b_s_lf$b_s_lf)*100,1)
u.df$b_ba_u <- round((b_ba_ul$b_ba_ul/b_ba_lf$b_ba_lf)*100,1)

u.df$l_hs_u <- round((l_hs_ul$l_hs_ul/l_hs_lf$l_hs_lf)*100,1) # hispan 
u.df$l_s_u <- round((l_s_ul$l_s_ul/l_s_lf$l_s_lf)*100,1)
u.df$l_ba_u <- round((l_ba_ul$l_ba_ul/l_ba_lf$l_ba_lf)*100,1)

u.df <- u.df[, c('date', 'all_u', 'hs_u', 's_u', 'ba_u', # all educ
                 'm_u', 'm_hs_u', 'm_s_u', 'm_ba_u', # male
                 'f_u', 'f_hs_u', 'f_s_u', 'f_ba_u', # female
                 'w_u', 'w_hs_u', 'w_s_u', 'w_ba_u', # white
                 'b_u', 'b_hs_u', 'b_s_u', 'b_ba_u', # black
                 'l_u', 'l_hs_u', 'l_s_u', 'l_ba_u')] # hispan

#-------------------------------------------------------------------------------#                                                                            
# Creating labor force participation rate                                       #                                                                          
#-------------------------------------------------------------------------------#

l.series <- c(, # all, male, female
              ) # white, black, hispan

l.df <- 

colnames(l.df) <- c('date', 
                    'all_lfpr','m_lfpr','f_lfpr', # all, male, female
                    'w_lfpr','b_lfpr', 'l_lfpr') # white, black, hispan

l.df <- merge(l.df, hs_lf, by = 'date') # all educ
l.df$hs_lfpr <- round((hs_lf$hs_lf/hs_p$hs_p)*100,1)
l.df$s_lfpr <- round((s_lf$s_lf/s_p$s_p)*100,1)
l.df$ba_lfpr <- round((ba_lf$ba_lf/ba_p$ba_p)*100,1)

l.df$m_hs_lfpr <- round((m_hs_lf$m_hs_lf/m_hs_p$m_hs_p)*100,1) # male 
l.df$m_s_lfpr <- round((m_s_lf$m_s_lf/m_s_p$m_s_p)*100,1)
l.df$m_ba_lfpr <- round((m_ba_lf$m_ba_lf/m_ba_p$m_ba_p)*100,1)

l.df$f_hs_lfpr <- round((f_hs_lf$f_hs_lf/f_hs_p$f_hs_p)*100,1) # female 
l.df$f_s_lfpr <- round((f_s_lf$f_s_lf/f_s_p$f_s_p)*100,1)
l.df$f_ba_lfpr <- round((f_ba_lf$f_ba_lf/f_ba_p$f_ba_p)*100,1)

l.df$w_hs_lfpr <- round((w_hs_lf$w_hs_lf/w_hs_p$w_hs_p)*100,1) # white 
l.df$w_s_lfpr <- round((w_s_lf$w_s_lf/w_s_p$w_s_p)*100,1)
l.df$w_ba_lfpr <- round((w_ba_lf$w_ba_lf/w_ba_p$w_ba_p)*100,1)

l.df$b_hs_lfpr <- round((b_hs_lf$b_hs_lf/b_hs_p$b_hs_p)*100,1) # black 
l.df$b_s_lfpr <- round((b_s_lf$b_s_lf/b_s_p$b_s_p)*100,1)
l.df$b_ba_lfpr <- round((b_ba_lf$b_ba_lf/b_ba_p$b_ba_p)*100,1)

l.df$l_hs_lfpr <- round((l_hs_lf$l_hs_lf/l_hs_p$l_hs_p)*100,1) # hispan 
l.df$l_s_lfpr <- round((l_s_lf$l_s_lf/l_s_p$l_s_p)*100,1)
l.df$l_ba_lfpr <- round((l_ba_lf$l_ba_lf/l_ba_p$l_ba_p)*100,1)

l.df <- l.df[, c('date', 'all_lfpr', 'hs_lfpr', 's_lfpr', 'ba_lfpr', # all educ
                 'm_lfpr', 'm_hs_lfpr', 'm_s_lfpr', 'm_ba_lfpr', # male
                 'f_lfpr', 'f_hs_lfpr', 'f_s_lfpr', 'f_ba_lfpr', # female
                 'w_lfpr', 'w_hs_lfpr', 'w_s_lfpr', 'w_ba_lfpr', # white
                 'b_lfpr', 'b_hs_lfpr', 'b_s_lfpr', 'b_ba_lfpr', # black
                 'l_lfpr', 'l_hs_lfpr', 'l_s_lfpr', 'l_ba_lfpr')] # hispan

#-------------------------------------------------------------------------------#                                                                            
# Creating civilian noninstitutional population aged 16 and higher              #                                                                          
#-------------------------------------------------------------------------------#

p.series <- c() # population levels 

p.df <- 
colnames(p.df) <- c('date', 'all_p', 'm_p', 'f_p' , 'w_p', 'b_p', 'l_p') # all, male, female, white, black, hispan

p.df <- merge(p.df, hs_p, by='date') # all educ
p.df <- merge(p.df, s_p, by='date')
p.df <- merge(p.df, ba_p, by='date')

p.df <- merge(p.df, m_hs_p, by='date') # male
p.df <- merge(p.df, m_s_p, by='date')
p.df <- merge(p.df, m_ba_p, by='date')

p.df <- merge(p.df, f_hs_p, by='date') # female
p.df <- merge(p.df, f_s_p, by='date')
p.df <- merge(p.df, f_ba_p, by='date')

p.df <- merge(p.df, w_hs_p, by='date') # white
p.df <- merge(p.df, w_s_p, by='date')
p.df <- merge(p.df, w_ba_p, by='date')

p.df <- merge(p.df, b_hs_p, by='date') # black
p.df <- merge(p.df, b_s_p, by='date')
p.df <- merge(p.df, b_ba_p, by='date')

p.df <- merge(p.df, l_hs_p, by='date') # hispan
p.df <- merge(p.df, l_s_p, by='date')
p.df <- merge(p.df, l_ba_p, by='date')

p.df <- p.df[, c('date', 'all_p', 'hs_p', 's_p', 'ba_p', # all educ
                 'm_p', 'm_hs_p', 'm_s_p', 'm_ba_p', # male
                 'f_p', 'f_hs_p', 'f_s_p', 'f_ba_p', # female
                 'w_p', 'w_hs_p', 'w_s_p', 'w_ba_p', # white
                 'b_p', 'b_hs_p', 'b_s_p', 'b_ba_p', # black
                 'l_p', 'l_hs_p', 'l_s_p', 'l_ba_p')] # hispan

# format dataset

df <- merge(df, u.df, by='date')
df <- merge(df, l.df, by='date')
df <- merge(df, p.df, by='date')

# create excel file 

df <- df[,c('year','month','datesv',
            'all_u', 'hs_u', 's_u', 'ba_u', # unemployment all educ
            'm_u', 'm_hs_u', 'm_s_u', 'm_ba_u', # male
            'f_u', 'f_hs_u', 'f_s_u', 'f_ba_u', # female
            'w_u', 'w_hs_u', 'w_s_u', 'w_ba_u', # white
            'b_u', 'b_hs_u', 'b_s_u', 'b_ba_u', # black
            'l_u', 'l_hs_u', 'l_s_u', 'l_ba_u', # hispan
            'all_lfpr', 'hs_lfpr', 's_lfpr', 'ba_lfpr', # labor force participation rate all educ
            'm_lfpr', 'm_hs_lfpr', 'm_s_lfpr', 'm_ba_lfpr', # male
            'f_lfpr', 'f_hs_lfpr', 'f_s_lfpr', 'f_ba_lfpr', # female
            'w_lfpr', 'w_hs_lfpr', 'w_s_lfpr', 'w_ba_lfpr', # white
            'b_lfpr', 'b_hs_lfpr', 'b_s_lfpr', 'b_ba_lfpr', # black
            'l_lfpr', 'l_hs_lfpr', 'l_s_lfpr', 'l_ba_lfpr', # hispan
            'all_p', 'hs_p', 's_p', 'ba_p', # civilian noninstitutional population 16+ all educ
            'm_p', 'm_hs_p', 'm_s_p', 'm_ba_p', # male
            'f_p', 'f_hs_p', 'f_s_p', 'f_ba_p', # female
            'w_p', 'w_hs_p', 'w_s_p', 'w_ba_p', # white
            'b_p', 'b_hs_p', 'b_s_p', 'b_ba_p', # black
            'l_p', 'l_hs_p', 'l_s_p', 'l_ba_p' # hispan
            )]

write.xlsx(df, file = "urates_awb_BLS_07_05_2024.xlsx", sheetName = "Unemployment",
           col.names = TRUE, row.names = FALSE, append = FALSE, showNA = FALSE)

