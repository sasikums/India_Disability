library(XLConnect)
library(reshape2)
library(dplyr)
library(car)
library(XML)
library(rvest)
suppressPackageStartupMessages(library(googleVis))
library(googlesheets)
library(ggplot2)
library(scales)
setwd('C:/Users/Shree/Google Drive/Classes/Census_2011')

######Download Raw Files

downl_census <-function(url_name,file_name){
 
    download.file(url_name,paste('./raw_files/',file_name,sep=""),mode='wb')
    
}
# Disability data (by age/state)
#downl_census(url_name ='http://www.censusindia.gov.in/2011census/C-series/C-20/DDW-C20-0000.xlsx',
#              file_name='disablity_by_age_State.xlsx')
# Disability data (top cities)
#downl_census(url_name ='http://www.censusindia.gov.in/2011census/C-series/DDWUA-0000C-21.xls',
# file_name='disabilty_topcities.xlsx')

# Disability Work data by state
#downl_census(url_name ='http://www.censusindia.gov.in/2011census/Disability_Data/DISAB04-0000.xlsx',
 #file_name='disabilty_work.xlsx')

# Disability Literacy data by state
#downl_census(url_name ='http://www.censusindia.gov.in/2011census/Disability_Data/DISAB03-0000.xlsx',
#file_name='disabilty_literacy.xlsx')

# Disability Marriage data by state
#downl_census(url_name ='http://www.censusindia.gov.in/2011census/C-series/DDW-0000C-21.xlsx',
#file_name='disabilty_marriage.xlsx')


# Population data (by age/state)
#downl_census(url_name ='http://www.censusindia.gov.in/2011census/C-series/c-13/DDW-0000C-13.xls',
 #                          file_name='agedis_bystate.xlsx')
# Population Work data (by age/state)
#downl_census(url_name ='http://www.censusindia.gov.in/2011census/Age_level_Data/C13/DDW-0000C-13A.xlsx',
#file_name='agedis_byliteracy.xlsx')

# Population Marriage data (by age/state)
#downl_census(url_name ='http://www.censusindia.gov.in/2011census/C-series/c-2/DDW-0000C-02-fer3-MDDS.xlsx',
#file_name='agedis_bymarriage.xlsx')

# Population Work data (by age/state)
#downl_census(url_name ='http://www.censusindia.gov.in/2011census/Age_level_Data/C13/DDW-C13APPENDIXB--0000.xlsx',
             #file_name='agedis_bywork.xlsx')
###age_work distribution file too large for XLConnect to read. Saved as a googlesheet and
##downloaded as a .csv
#downl_age_work <- gs_url('https://docs.google.com/spreadsheets/d/1gnnM9PFnvxhCmoCQuEOW4854V-wFYamvPyb372Hk1-E/edit?usp=sharing')
#gs_download(downl_age_work,to='./raw_files/agedis_bywork.csv',overwrite=TRUE,ws=1)

## country estimtes disability data
#downl_country_est <- gs_url('https://docs.google.com/spreadsheets/d/1I-6zPouRh4lhxZBsLkGrDWD5EbWGIzISWc5bNtYxaTw/edit?usp=sharing')
#gs_download(downl_country_est,to='./raw_files/country_disability_estimates.csv',overwrite=TRUE,ws=1)

## World bank 2011 estimates of disability
#downl_wb_est <- gs_url('https://docs.google.com/spreadsheets/d/1DaAlPCT9jLU4s9tmo5SCaH9Ko7mrrw_e8s4RoEjS29g/edit?usp=sharing')
#gs_download(downl_wb_est,to='./raw_files/worldbank_disability_estimates.csv',overwrite=TRUE,ws=1)

dis_count <- read.csv('./raw_files/country_disability_estimates.csv')
dis_count <-filter(dis_count,Country!="Pakistan")
dis_count <- arrange(dis_count,desc(Disability_Perc)) %>%
  mutate(nrow=as.numeric(rownames(dis_count)),y=5,
                    Country_Label=paste(Country,':',percent(Disability_Perc),sep=""),
                    colorlab="Other") 
  
dis_count$colorlab[which(dis_count$Country=="India")]="India"




#import excel files, set column names 
import_census <- function(url_name,file_name,st_col,end_col,st_row,end_row,var_names){
 

  imp_file = readWorksheetFromFile(paste('./raw_files/',file_name,sep=""), sheet=1,
                                        startCol=st_col,
                                        endCol=end_col,
                                        startRow=st_row,
                                        endRow=end_row,
                                   header=FALSE) 
  #set column headers
  names(imp_file)=var_names

  return(imp_file)
}

#Marriage Data
df_dis_mar <- import_census(file_name='disabilty_marriage.xlsx',
                            st_col=1,
                            end_col=25,
                            st_row=7,
                            end_row=4866,
                            var_names=c('Census_Table','State_Code','District_Code','Area_Name',
                                        'Urban_Rural','Disability_Type','Age_Group',
                                        'P_Total','M_Total','F_Total',
                                        'P_NeverMarried','M_NeverMarried','F_NeverMarried',
                                        'P_Married','M_Married','F_Married',
                                        'P_Widowed','M_Widowed','F_Widowed',
                                        'P_Seperated','M_Seperated','F_Seperated',
                                        'P_Divorced','M_Divorced','F_Divorced'))
df_dis_mar_sub <- filter(df_dis_mar,Age_Group=="Total")

melt <- melt(df_dis_mar_sub,id.vars=c('Census_Table','State_Code','District_Code',
                                      'Age_Group','Area_Name',
                                      'Urban_Rural','Disability_Type'),
             value.name='dis_mar_count')
melt <- mutate(melt,sex=substr(variable,1,1),status=substr(variable,3,nchar(as.character(variable))))

# create state name variable that strips out 'State-'prefix from area name
melt$State = toupper(melt$Area_Name)
melt$State[which(melt$State!="INDIA")] = substr(melt$State[which(melt$State!="INDIA")],9,
                                                nchar(melt$State[which(melt$State!="INDIA")]))
#cleaning for delhi named nct-delhi
melt$State[which(grepl('DELHI',toupper(melt$Area_Name)))] = "DELHI"
melt<-select(melt,State,sex,Urban_Rural,Disability_Type,status,dis_mar_count)

#Convert each statusto % of total
melt_sum <-filter(melt,status=='Total')%>%
  select(State,sex,Urban_Rural,Disability_Type,dis_mar_count)%>%
  rename(dis_mar_total=dis_mar_count)

melt <- merge(melt,melt_sum,by=c('State','sex','Urban_Rural','Disability_Type'))
df_dis_mar_sub <- mutate(melt,dis_mar_perc=dis_mar_count/dis_mar_total)

## population marriage data

df_mar <- import_census(file_name='agedis_bymarriage.xlsx',
                            st_col=1,
                            end_col=24,
                            st_row=7,
                            end_row=2166,
                            var_names=c('Census_Table','State_Code','District_Code','Area_Name',
                                        'Urban_Rural','Age_Group',
                                        'P_Total','M_Total','F_Total',
                                        'P_NeverMarried','M_NeverMarried','F_NeverMarried',
                                        'P_Married','M_Married','F_Married',
                                        'P_Widowed','M_Widowed','F_Widowed',
                                        'P_Seperated','M_Seperated','F_Seperated',
                                        'P_Divorced','M_Divorced','F_Divorced'))
df_mar_sub <- filter(df_mar,Age_Group=="All ages")

melt <- melt(df_mar_sub,id.vars=c('Census_Table','State_Code','District_Code',
                                      'Age_Group','Area_Name',
                                      'Urban_Rural'),
             value.name='mar_count')
melt <- mutate(melt,sex=substr(variable,1,1),status=substr(variable,3,nchar(as.character(variable))))

# create state name variable that strips out 'State-'prefix from area name
melt$State = toupper(melt$Area_Name)
melt$State[which(melt$State!="INDIA")] = substr(melt$State[which(melt$State!="INDIA")],9,
                                                nchar(melt$State[which(melt$State!="INDIA")])-5)
#cleaning for delhi named nct-delhi
melt$State[which(grepl('DELHI',toupper(melt$Area_Name)))] = "DELHI"
melt<-select(melt,State,sex,Urban_Rural,status,mar_count)

#Convert each statusto % of total
melt_sum <-filter(melt,status=='Total')%>%
  select(State,sex,Urban_Rural,mar_count)%>%
  rename(mar_total=mar_count)

melt <- merge(melt,melt_sum,by=c('State','sex','Urban_Rural'))
df_mar_sub <- mutate(melt,mar_perc=mar_count/mar_total)


df_mar_comp <- merge(df_dis_mar_sub,df_mar_sub,
                     by=c('State','sex','Urban_Rural','status'))

#change tag for total disabilities to conform with other datasets
df_mar_comp$Disability_Type[which(df_mar_comp$Disability_Type=="Total")]='Total disabled population'

df_mar_comp$Disability_Type=gsub("-"," ",df_mar_comp$Disability_Type)

df_mar_comp <-filter(df_mar_comp,status!="Total")
#Literacy Data
df_dis_lit <- import_census(file_name='disabilty_literacy.xlsx',
                            st_col=1,
                            end_col=11,
                            st_row=6,
                            end_row=977,
                            var_names=c('Census_Table','State_Code','Area_Name',
                                        'Disablity_Type','sex',
                                        'Total_L','R_L','U_L',
                                        'Total_I','R_I','U_I')
)

#Convert Counts to % of total
df_dis_lit <- mutate(df_dis_lit,
                      Total=Total_I/(Total_I+Total_L),
                      Rural=R_I/(R_I+R_L),
                      Urban=U_I/(U_I+U_L)
                      )%>%
  select(Area_Name,Disablity_Type,sex,Total,Urban,Rural)
melt <- melt(df_dis_lit,id.vars=c('Area_Name','Disablity_Type','sex'),
             variable.name='Urban_Rural',value.name='dis_ill_lit')

# create state name variable that strips out 'State-'prefix from area name
melt$State = toupper(melt$Area_Name)
melt$State[which(melt$State!="INDIA")] = substr(melt$State[which(melt$State!="INDIA")],9,
                                                nchar(melt$State[which(melt$State!="INDIA")])-5)
#cleaning for delhi named nct-delhi
melt$State[which(grepl('DELHI',toupper(melt$Area_Name)))] = "DELHI"

#recode sex to first letter to help merging
melt$sex <- substr(melt$sex,1,1)

df_dis_lit <- select(melt,State,sex,Urban_Rural,Disablity_Type,dis_ill_lit)

#national literacy data
df_age_lit <- import_census(file_name='agedis_byliteracy.xlsx',
                            st_col=1,
                            end_col=15,
                            st_row=8,
                            end_row=7423,
                            var_names=c('Census_Table','State_Code','Dist_Code',
                                        'Area_Name','Lit','Age',
                                        'P_Total','M_Total','F_Total',
                                        'P_Rural','M_Rural','F_Rural',
                                        'P_Urban','M_Urban','F_Urban'
                            ))
df_lit_sub <- filter(df_age_lit,Age=="All ages")

melt <- melt(df_lit_sub,id.vars=c('Census_Table','State_Code','Dist_Code',
             'Area_Name','Lit','Age'))
# create state name variable that strips out 'State-'prefix from area name
melt$State = toupper(melt$Area_Name)
melt$State[which(melt$State!="INDIA")] = substr(melt$State[which(melt$State!="INDIA")],9,
                                                nchar(melt$State[which(melt$State!="INDIA")])-5)
#cleaning for delhi named nct-delhi
melt$State[which(grepl('DELHI',toupper(melt$Area_Name)))] = "DELHI"

df_lit_sub <- mutate(melt,sex=substr(variable,1,1),
                      Urban_Rural=substr(variable,3,nchar(as.character(variable))))%>%
  select(State,Urban_Rural,sex,Lit,value)

df_lit_sum <- group_by(df_lit_sub,State,Urban_Rural,sex)%>%
  summarise(tot_pop=sum(value))

df_lit_sub <- merge(df_lit_sub,df_lit_sum,
                    by=c('State','sex','Urban_Rural'))
df_lit_sub <- filter(df_lit_sub, Lit=='Illiterate') %>%
  mutate(ill_lit=value/tot_pop)%>%
  select(State,sex,Urban_Rural,ill_lit)

df_lit_comp <- merge(df_dis_lit,df_lit_sub,
                     by=c('State','sex','Urban_Rural'))





#disability work data
df_dis_raw <- import_census(file_name='disabilty_work.xlsx',
                            st_col=1,
                            end_col=12,
                            st_row=6,
                            end_row=2921,
                            var_names=c('Census_Table','State_Code','Area_Name',
                                        'Disablity_Type','Urban_Rural','sex',
                                        'All','Cult','Agri','HHI','Others','Non_Workers')
                            )
#Convert Counts to % of total
df_dis_work <- mutate(df_dis_raw,
                      Perc_All=All/(All+Non_Workers),
                      Perc_Cult=Cult/(All+Non_Workers),
                      Perc_Agri=Agri/(All+Non_Workers),
                      Perc_HHI=HHI/(All+Non_Workers),
                      Perc_Others=Others/(All+Non_Workers),
                      Perc_Non_Workers=Non_Workers/(All+Non_Workers))%>%
  select(Area_Name,Disablity_Type,Urban_Rural,sex,starts_with('Perc'))
                        


                      
melt <-  melt(df_dis_work,id.vars=c('Area_Name','Disablity_Type',
                                    'Urban_Rural','sex'),variable.name='worker_type',
              value.name='dis_worker_dist')
# create state name variable that strips out 'State-'prefix from area name
melt$State = toupper(melt$Area_Name)
melt$State[which(melt$State!="INDIA")] = substr(melt$State[which(melt$State!="INDIA")],9,
                                                nchar(melt$State[which(melt$State!="INDIA")])-5)
#cleaning for delhi named nct-delhi
melt$State[which(grepl('DELHI',toupper(melt$Area_Name)))] = "DELHI"

#recode sex to first letter to help merging
melt$sex <- substr(melt$sex,1,1)

#Recode worker_type field to correspond with worker population file for merging
df_dis_work <- mutate(melt,worker_type=recode(worker_type,
                                              "'Perc_All'='All';
                                              'Perc_Cult'='Cultivators';
                                              'Perc_Agri'='Agricultural labourers';
                                              'Perc_HHI'='HHI workers';
                                              'Perc_Others' = 'Other workers';
                                              'Perc_Non_Workers'='Non-workers'")) %>%
  select(State,Urban_Rural,sex,worker_type,Disablity_Type,dis_worker_dist)


#age distribution of worker population
df_age_work <- read.csv('./raw_files/agedis_bywork.csv')
df_work_sub <- filter(df_age_work,Age=='All ages',
                      as.character(worker_type) %in% c('Cultivators',
                                         'Agricultural labourers',
                                         'HHI workers',
                                         'Other workers',
                                         "Non-workers"))
melt <- melt(df_work_sub,id.vars=c('state_code','Area_Name','Age','worker_type'),
             value.name='worker_count')
# create state name variable that strips out 'State-'prefix from area name
melt$State = toupper(melt$Area_Name)
melt$State[which(melt$State!="INDIA")] = substr(melt$State[which(melt$State!="INDIA")],9, 
                                                nchar(melt$State[which(melt$State!="INDIA")])-5)
#cleaning for delhi named nct-delhi
melt$State[which(grepl('DELHI',toupper(melt$Area_Name)))] = "DELHI"
df_work_sub <- mutate(melt,sex=substr(variable,9,9),
                      Urban_Rural=substr(variable,11,nchar(as.character(variable))))%>%
  select(State,Urban_Rural,sex,worker_type,worker_count)

#merge in totals and convert worker count in to worker %
df_work_tot <- group_by(df_work_sub,State,Urban_Rural,sex) %>%
  summarise(worker_total=sum(worker_count))

df_work_sub <- merge(df_work_sub,df_work_tot,
                     by=c('State','Urban_Rural','sex'))

df_work_sub <- mutate(df_work_sub,worker_dist=worker_count/worker_total)

#merge disability and population work distribtuons
df_work_comp <- merge(df_dis_work,df_work_sub,
                      by=c('State','Urban_Rural','sex','worker_type'))

                        
#Disability data by age and disability
df_disabl <- import_census(file_name='disablity_by_age_State.xlsx',
                              st_col=1,
                              end_col=33,
                              st_row=6,
                              end_row=1409,
                              var_names=c('Census_Table','State_Code','Dist_Code',
                                          'Area_Name','Urban_Rural',
                                          'Age_Group',
                                          'Disab_P_TotDisab','Disab_M_TotDisab','Disab_F_TotDisab',
                                          'Disab_P_Sight','Disab_M_Sight','Disab_F_Sight',
                                          'Disab_P_Hear','Disab_M_Hear','Disab_F_Hear',
                                          'Disab_P_Speech','Disab_M_Speech','Disab_F_Speech',
                                          'Disab_P_Move','Disab_M_Move','Disab_F_Move',
                                          'Disab_P_MenIll','Disab_M_MenIll','Disab_F_MenIll',
                                          'Disab_P_MenRet','Disab_M_MenRet','Disab_F_MenRet',
                                          'Disab_P_Other','Disab_M_Other','Disab_F_Other',
                                          'Disab_P_Mult','Disab_M_Mult','Disab_F_Mult')
)

# create state name variable that strips out 'State-'prefix from area name
df_disabl$State = toupper(df_disabl$Area_Name)
df_disabl$State[which(df_disabl$State!="INDIA")] = substr(df_disabl$State[which(df_disabl$State!="INDIA")],7,100)
#cleaning for delhi named nct-delhi
df_disabl$State[which(grepl('DELHI',toupper(df_disabl$Area_Name)))] = "DELHI"

#melt so as to shape the data in long form (for merging later)
temp <- melt(df_disabl,id.vars=c('Census_Table','State_Code','Dist_Code',
                                 'Area_Name','State','Urban_Rural',
                                 'Age_Group'),
             ,value.name='disabl_count')

#extract category variabales after melting
df_disabl <- mutate(temp,sex=substr(variable,7,7),disability=substr(variable,9,100))
#remove uneccesary variables
df_disabl <- select(df_disabl, -Census_Table,-Dist_Code,-variable,-Area_Name)


df_age_dis <- import_census(file_name='agedis_bystate.xlsx',
                               st_col=1,
                               end_col=14,
                               st_row=8,
                               end_row=3715,
                               var_names=c('Census_Table','State_Code','Dist_Code',
                                           'Area_Name','Age',
                                           'P_Total','M_Total','F_Total',
                                           'P_Rural','M_Rural','F_Rural',
                                           'P_Urban','M_Urban','F_Urban'
                                           )
)

# create state name variable that strips out 'State-'prefix from area name
df_age_dis$State = toupper(df_age_dis$Area_Name)
df_age_dis$State[which(df_age_dis$State!="INDIA")] = substr(df_age_dis$State[which(df_age_dis$State!="INDIA")],9,
                                                            nchar(df_age_dis$State[which(df_age_dis$State!="INDIA")])-5)
#cleaning for delhi named nct-delhi
df_disabl$State[which(grepl('DELHI',toupper(df_disabl$Area_Name)))] = "DELHI"


#melt data to get urban/rural and sex in long form instead of wide
temp <-melt(df_age_dis,id.vars=c('Census_Table','State_Code','Dist_Code',
                                 'Area_Name','State','Age'),
            value.name='pop_count')

#extract category variables; create age groups to match disability data
#Age_High used to create numberic version
df_age_dis <- mutate(temp,Urban_Rural=substr(variable,3,10),
                sex=substr(variable,1,1),
                num_age=as.numeric(Age),
                Age_Group=recode(num_age,"0:4='0-4';5:9='5-9';
                                 10:19='10-19';20:29='20-29';
                                 30:39='30-39';40:49='40-49';
                                 50:59='50-59';60:69='60-69';
                                 70:79='70-79';80:89='80-89';
                                 90:100='90+';
                                 else='Age Not Stated'")
                )


df_age_dis$Age_Group[which(df_age_dis$Age=='All ages')] <- 'Total'

#sum by age groups
df_age_dis_group <- summarise(group_by(df_age_dis,State_Code,State,
                                       Urban_Rural,sex,Age_Group),
                              pop_count=sum(pop_count))


df_disab_pop <- merge(df_disabl,df_age_dis_group,
                      by=c('State_Code','State','Urban_Rural','sex','Age_Group'))

#Find Disabilty as % of Population and set age high to be able to subset age group as a range
#ages above 90 and unknown ages set to 100
df_disab_pop <- mutate(df_disab_pop, pop_disabl_perc=disabl_count/pop_count,
                       age_high=as.numeric(substr(Age_Group,4,5)))
df_disab_pop$age_high[which(df_disab_pop$Age_Group %in% c('0-4'))]=4
df_disab_pop$age_high[which(df_disab_pop$Age_Group %in% c('5-9'))]=9
df_disab_pop$age_high[which(df_disab_pop$Age_Group %in% c('90+','Age Not Stated'))]=100

#change sex varaible for all people from 'P' to 'All"
df_disab_pop$sex[which(df_disab_pop$sex == 'P')]= 'All'

###download lookup of iso codes for indian states
#iso_sheet <- gs_url('https://docs.google.com/spreadsheets/d/1pX4iey8hIXwYD1AHi6tPG1y7tCkqRy7I2idGvJrBY8U/edit?usp=sharing')
#gs_download(iso_sheet,to='./lookups/indian_isocodes.csv',overwrite=TRUE,ws=1)

##import iso codes to R and prepare for merging with census data
iso_codes <- read.csv('./lookups/indian_isocodes.csv',stringsAsFactors=FALSE)
names(iso_codes) <- c('iso_code','State','state_ut')
iso_codes <- mutate(iso_codes,State=toupper(gsub(' and ', ' & ', State)))
iso_codes$State[which(as.character(iso_codes$State)=="UTTARPRADESH !UTTAR PRADESH")] <- "UTTAR PRADESH"
iso_codes$State[which(as.character(iso_codes$State)=="UTTARAKHAND[NOTE 4]")] <- "UTTARAKHAND"
iso_codes$State[which(as.character(iso_codes$State)=="CHHATTISGARH[NOTE 1]")] <- "CHHATTISGARH"
iso_codes$State[which(as.character(iso_codes$State)=="ODISHA[NOTE 2]")] <- "ODISHA"
iso_codes$State[which(as.character(iso_codes$State)=="TELANGANA[NOTE 3]")] <- "TELANGANA"
iso_codes$State[which(as.character(iso_codes$State)=="UTTARAKHAND[NOTE 4]")] <- "UTTARAKHAND"


#merge iso codes to disability data
df_disab_pop <- merge(df_disab_pop,iso_codes,
                      by=c('State'),all=TRUE)





#create lookup of state codes and clean state names
st_codes <- as.data.frame(unique(cbind(df_disabl$State,df_disabl$State_Code)))
names(st_codes) <- c('State','State_Code')
write.csv(st_codes,'./lookups/state_codes.csv',
          row.names=FALSE)

#Disability data by top cities
cit_raw <- import_census(file_name='disabilty_topcities.xlsx',
                         st_col=1,
                         end_col=10,
                         st_row=7,
                         end_row=2346,
                         var_names=c('Census_Table','State_Code','Town_Code',
                                     'Urban_Rural','Urban_Area',
                                     'Disability', 'Age_Group',
                                     'Disab_P_TotDisab','Disab_M_TotDisab','Disab_F_TotDisab'
                         ))

#add states
#colclasses=character preserves the leading 0s in State_Code
st_codes <- read.csv('./lookups/state_codes.csv',colClasses = "character")
df_disabl_cit <- merge(cit_raw,st_codes,by=c('State_Code'),all.x=TRUE)
#clean city name
df_disabl_cit <- mutate(df_disabl_cit, City=substr(Urban_Area,1,regexpr(" ", Urban_Area)-1),
                        Address=paste(City,State,"INDIA",sep=",")) %>%
  select(City,State,Age_Group,Disability,starts_with('Disab_'))
#city with no spaces
df_disabl_cit$City[which(df_disabl_cit$City=="")]="GVMC"
df_disabl_cit$City[which(df_disabl_cit$City=="Greater")]="Mumbai"
df_disabl_cit$City[which(df_disabl_cit$City=="Bruhat")]="Bengaluru"

melt <- melt(df_disabl_cit,id.vars=c('City','State','Age_Group','Disability'),
             value.name='Disabled_Count')
df_dis_cit <- mutate(melt,sex=substr(variable,7,7))

#change sex varaible for all people from 'P' to 'All"
df_dis_cit$sex[which(df_dis_cit$sex == 'P')]= 'All'


#Merge World Bank estimates with India estimates

in_temp <- filter(df_disab_pop,State=="INDIA" & disability=="TotDisab" & Urban_Rural=="Total"
                  & Age_Group != "Age Not Stated")

in_temp <- mutate(in_temp,Age_Group=recode(age_high,"0:19='0-14 years';20:59='15-59 years';
                                 60:100='60+ years';else='All ages'"))
## sum to world bank age groups and multiply % by 100 to conform with wb figures
in_sum <- group_by(in_temp,Age_Group,sex) %>%
  summarise(India=100*sum(disabl_count)/sum(pop_count))


wb_est <- read.csv('./raw_files/worldbank_disability_estimates.csv')
wb_est <- rename(wb_est,sex=Sex) %>%
  mutate(Age_Group=as.character(Age_Group)) %>%
  select(-Source)

wb_est <- merge(wb_est,in_sum,by=c('Age_Group','sex'),all=TRUE)


  

wb_est <- melt(wb_est,id.vars=c('Age_Group','sex','Disability_Type'))
wb_est$sort_var=100
wb_est$sort_var[which(wb_est$variable=="India")]=0

# convert wb figures to percentage
df_wb_est <- arrange(wb_est,desc(sort_var)) %>%
  mutate(value=value/100,value.annotation=percent(value),value.style="") 
df_wb_est$value.style[which(df_wb_est$variable=="India")]="color: #D00000"




temp1 <- filter(df_wb_est,as.character(Age_Group)=='All ages' & 
                 as.character(sex)=="All" & 
                 as.character(Disability_Type)=="Moderate & Severe Disability" &
               variable %in% c('World','India','HighIncome') ) %>%
  arrange(desc(sort_var)) 



plot(gvisColumnChart(temp1,xvar='variable',yvar=c('value','value.annotation','value.style'),
                               options=list(vAxis="{format:'#,###.#%', min:0}",
                                               title='Moderate & Severe',
                                               legend='none'))
               )

plot(gvisColumnChart(temp2,options=list(vAxis="{format:'#,###.#%'}",
                                        title='Severe',
                                        legend='none')))
#Export 'cleaned' data used in app as .csv to eliminate the cleaning process each time the app
# is loaded

export_clean <-function(ex_data,ex_file){
  write.csv(ex_data,paste('./cleaned_data/',ex_file,'.csv',sep=""),row.names=FALSE)
}
export_clean(df_dis_cit,'top_city_data')
export_clean(df_disab_pop,'state_age_distribtution')
export_clean(df_work_comp,'employment')
export_clean(df_lit_comp,'literacy')
export_clean(df_mar_comp,'marriage')
export_clean(dis_count,'country_estimates')
export_clean(df_wb_est,'worldbank_estimates')





