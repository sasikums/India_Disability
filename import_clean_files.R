suppressPackageStartupMessages(library(XLConnect))
library(reshape2)
suppressPackageStartupMessages(library(dplyr))
library(car)
library(XML)
suppressPackageStartupMessages(library(rvest))
suppressPackageStartupMessages(library(googleVis))
library(googlesheets)
library(ggplot2)
library(scales)

#Import 'cleaned' data used in app as .csv to eliminate the cleaning process each time the app
# is loaded

import_clean <-function(ex_file){
  return(read.csv(paste('./cleaned_data/',ex_file,'.csv',sep=""),stringsAsFactors=FALSE))
}
df_dis_cit <- import_clean('top_city_data')
df_disab_pop <- import_clean('state_age_distribtution')
df_work_comp <- import_clean('employment')
df_lit_comp <- import_clean('literacy')
df_mar_comp <- import_clean('marriage')
df_dis_count <-import_clean('country_estimates')
df_wb_est <-import_clean('worldbank_estimates')



