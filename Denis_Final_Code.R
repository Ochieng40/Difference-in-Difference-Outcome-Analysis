#SDR DID Analysis - Level 4 facilities 
#Created by: Nisha Hariharan
# Edited by: Denis Ochieng
#Started: December 2022 
#Modified: March 31, 2023 
#Goal: The goal of this file is to clean the MOH 711 data. Given the issue with the lack of recorded zeros, this file replaces low frequency data with 0's in the event of missing data. 
#See additional SDR notes for details on approach 
#Mar 31: Update - Added in 2019 data and removed pregnant people data 


#libraries to use
library(readxl)
library(tidyr)
library(dplyr)
library(googlesheets4)
library(summarytools)
library(Amelia)
library(stringr)


# --------------------------------------- UPLOADING OF DATASET FROM GOOGLE SHEET--------------------------------------------------------------------------------------------------------------------------------------------------------


# Authentication used with Google Sheets
if (!gs4_has_token()) {
  gs4_auth()
}
2

# Function to read data from our  Google Sheet
read_google_sheet <- function(google_sheets_link, sheet_name) {
  df <- read_sheet(google_sheets_link, sheet = sheet_name)
  return(df)
}

# Google Sheets links
google_sheets_links <- c(
  "https://docs.google.com/spreadsheets/d/1dxRz_GIwvLYFF1IIjED8ElfG5lJ4csWz6x1sfyMMY_E/edit?usp=sharing",
  "https://docs.google.com/spreadsheets/d/1w3-zh3oYswnYMcvhZhghBHZxvtlwA9lMb3aT4Z_6VPw/edit?usp=sharing",
  "https://docs.google.com/spreadsheets/d/1ZwITrBTGA3cIHVnXsPZY9Gwdr8zIt1Bz54_AiGUaiCM/edit?usp=sharing"
)

sheet_names <- c(
  "Q3_Data",
  "Variable_Selection",
  "Sheet1"
)

# Now we create a list to Read data directly from Google Sheets
dfs <- list()

for (i in 1:length(google_sheets_links)) {
  dfs[[sheet_names[i]]] <- read_google_sheet(google_sheets_links[i], sheet_names[i])
}

# Then we assign data frames
MOH_711 <- dfs[["Q3_Data"]]
KPI_List <- dfs[["Variable_Selection"]]
Fac_List <- dfs[["Sheet1"]]


#------------------------------------------------------------------------------Data Transformation----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#transform MOH 711 data 
Data_711 <- MOH_711 %>% pivot_longer(cols = 9:78, names_to = "indicator", values_to = "value")  # The code creates a pivot longer for columns 9 to 78 (These are columns with indicators values)


#extract the year 
Data_711$year <- substr(Data_711$periodcode,1,4) 
#extract the month
Data_711$month <- substr(Data_711$periodcode,5,6)
#Grab everything between MOH.711 and the start of the month character
Data_711$indicator_clean <- gsub("MOH 711 ", "", Data_711$indicator)  # I used the regex formula to drop the 'MOH 711' words
Data_711 <- Data_711 %>%  mutate(indicator_clean = ifelse(is.na(indicator_clean), indicator,indicator_clean))


#Rename column in fac_list for merge 
Data_711 <- rename(Data_711,c('Code'='organisationunitcode'))
#Merge data sets 
Master_Data <- merge(Data_711,Fac_List,by="Code", all.x = "TRUE")  

#Limit to Kakamega and Level4 facilities 
Clean_Data <- select(Master_Data,Code,organisationunitname,organisationunitid,value,year,month,indicator_clean,Keph.level,Facility.type, County,Sub.county)
Clean_Data <- Clean_Data %>%  filter(County =='Kakamega')
Clean_Data <- Clean_Data %>%  filter(Keph.level== 'Level 4')  


#Renamed indicator column for merge 
Clean_Data <- rename(Clean_Data,c('Indicator'='indicator_clean'))



download_path <- "C:/Users/DenisOchieng_b27gbi2/Downloads"
file_name <- "sdr_dat_before_wide.csv"

write.csv(Clean_Data, file.path(download_path, file_name), row.names = FALSE)



#generated missing map using amelia package 
Clean_Data_Wide <- 
  Clean_Data %>% 
  pivot_wider(names_from = Indicator, values_from = value, values_fill = NA)



download_path <- "C:/Users/DenisOchieng_b27gbi2/Downloads"
file_name <- "sdr_dat_wide.csv"

write.csv(Clean_Data_Wide, file.path(download_path, file_name), row.names = FALSE)


Indicators_Only_Wide <- select(Clean_Data_Wide, c(10:79))
missmap(Indicators_Only_Wide)



#drop irrelevant indicators 
Clean_Data <- merge(x = Clean_Data, y = KPI_List, by="Indicator", all.x = "TRUE") 
Clean_Data <- Clean_Data %>%  filter(SDR.Related == "Yes") 


# Renamed the 'Row Labels' column to 'Clean_Indicator'
colnames(Clean_Data)[colnames(Clean_Data) == 'Row Labels'] <- 'Clean_Indicator'

download_path <- "C:/Users/DenisOchieng_b27gbi2/Downloads"
file_name <- "sdr_dat.csv"

write.csv(Clean_Data, file.path(download_path, file_name), row.names = FALSE)


# We drop a number of columns here - I had two indicator columns so I dropped one here
Clean_Data <- select(Clean_Data,-c("Code", "SDR.Related","Indicator.Type","Measure","Indicator"))


#count number of NAs for each indicator 
Clean_Data <- 
  Clean_Data %>% 
  group_by(Clean_Indicator) %>% 
  mutate(na_count_ind = sum(!complete.cases(value))) 

Clean_Data <- 
  Clean_Data %>% 
  add_count(Clean_Indicator, name = "n")

Clean_Data <- 
  Clean_Data %>% 
  mutate(prop_incomplete_ind = na_count_ind / n) 

#explore missing data 
mean_indicator_missing <-  
  Clean_Data %>% 
  group_by(Clean_Indicator)%>% 
  summarise(prop_incomplete_mean = mean((prop_incomplete_ind)))

mean_indicator_missing <- mean_indicator_missing[order(mean_indicator_missing$prop_incomplete_mean),]  


download_path <- "C:/Users/DenisOchieng_b27gbi2/Downloads"
file_name <- "mean_indicator_missing.csv"

write.csv(Clean_Data, file.path(download_path, file_name), row.names = FALSE)



#drop indicators that are under 80% complete - this threshold can be changed if needed (see MOH 711 Ind Complete for threshold analysis )
Clean_Data <- 
  Clean_Data %>% 
  filter(prop_incomplete_ind < 0.80) 

#count missing data by facility 
Clean_Data <- 
  Clean_Data %>% 
  group_by(organisationunitname) %>% 
  mutate(na_count_fac = sum(!complete.cases(value))) 

Clean_Data <- 
  Clean_Data %>% 
  add_count(organisationunitname, name = "n_2")

Clean_Data <- 
  Clean_Data %>% 
  mutate(prop_incomplete_fac = na_count_fac / n_2) 

mean_fac_missing <-  
  Clean_Data %>% 
  group_by(organisationunitname)%>% 
  summarise(prop_incomplete_fac = mean((prop_incomplete_fac)))

mean_fac_missing <- mean_fac_missing[order(mean_fac_missing$prop_incomplete_fac),]  


download_path <- "C:/Users/DenisOchieng_b27gbi2/Downloads"
file_name <- "mean_fac_missing.csv"

write.csv(mean_fac_missing, file.path(download_path, file_name), row.names = FALSE)


#count number of NAs per month 
Clean_Data <- 
  Clean_Data %>% 
  group_by(organisationunitname,year,month) %>% 
  mutate(na_count_monthfac = sum(!complete.cases(value))) 

Clean_Data <- 
  Clean_Data %>% 
  add_count(organisationunitname,year,month, name = "n_3")

Clean_Data <- 
  Clean_Data %>% 
  mutate(prop_incomplete_monthfac = na_count_monthfac / n_3)

#explore missing data - this is used to look at the distribution of values for Option 2 (determining low frequency indicators)
mean_indicator_missing <-  
  Clean_Data %>% 
  group_by(Clean_Indicator)%>% 
  summarise(mean_value = mean(value, na.rm = TRUE)) %>%
  mutate(Low_Frequency = ifelse(mean_value < 10,1,0))

Clean_Data <- merge(x = Clean_Data, y = mean_indicator_missing, by="Clean_Indicator", all.x = "TRUE") 

#Option 1 Data Set - set replace to 0 if the proportion incomplete is less than 80% 
Clean_Data <- 
  Clean_Data %>% 
  mutate(final_value_1 = ifelse(is.na(value) & prop_incomplete_monthfac < 0.8, 0, value))



#Option 2 Data Set - set replace to 0 if there is less than 80% incomplete & low_frequency indicator
# Note - replacement is only possible if the live_birth or anc_clients is not "NA"   
# Note - adjusts the live births and anc_clients values so if an indicator value is not replaced, the live births and anc_clients values are zeroed out.  

Clean_Data <- 
  Clean_Data %>% 
  mutate(final_value_2_pre = ifelse(is.na(value) & prop_incomplete_monthfac < 0.8 & Low_Frequency == 1, 0, value))

Clean_Data <- 
  Clean_Data %>% 
  group_by(organisationunitname,year,month) %>%  
  mutate(total_births_value = final_value_2_pre[Clean_Indicator == "Live.birth"] + final_value_2_pre[Clean_Indicator == "Fresh.Still.Birth"]) %>%
  mutate(Live_Births_Associated = ifelse(total_births_value>0,1,0))



# Download the csv dataset to view progress

download_path <- "C:/Users/DenisOchieng_b27gbi2/Downloads"
file_name <- "After_Replacing_with_zero.csv"

write.csv(Clean_Data, file.path(download_path, file_name), row.names = FALSE)



# The line of code below first checks if there are any rows with the specified Clean_Indicator values before attempting the calculation.
# If the conditions are not met, it sets anc_value to NA, and this
# ensures that we don't end up with a vector of size 0. 
# I was getting initially hence modified the code "anc_value` must be size 16 or 1, not 0."


Clean_Data <- 
  Clean_Data %>% 
  group_by(organisationunitname,year,month) %>%  
  mutate(anc_value = final_value_2_pre[Clean_Indicator == "New.ANC.clients"] + final_value_2_pre[Clean_Indicator == "Re.Visit.ANC.Clients"]) %>%
  mutate(ANC_Associated = ifelse(anc_value>0,1,0))





download_path <- "C:/Users/DenisOchieng_b27gbi2/Downloads"
file_name <- "view_now.csv"

write.csv(Clean_Data, file.path(download_path, file_name), row.names = FALSE)






Clean_Data <- 
  Clean_Data %>% 
  mutate(final_value_2_post = ifelse(is.na(total_births_value) & Live_Births_Associated == 1,NA,
                                     ifelse(is.na(anc_value) & ANC_Associated == 1, NA , final_value_2_pre)))

Clean_Data <- 
  Clean_Data %>% 
  mutate(final_value_total_births = ifelse(is.na(final_value_2_post) & Live_Births_Associated == 1,NA,
                                           total_births_value))


Clean_Data <- 
  Clean_Data %>% 
  mutate(final_value_anc = ifelse(is.na(final_value_2_post) & ANC_Associated == 1,NA,
                                  anc_value))




download_path <- "C:/Users/DenisOchieng_b27gbi2/Downloads"
file_name <- "Final_Doc.csv"

write.csv(Clean_Data, file.path(download_path, file_name), row.names = FALSE)




#----------------------------------------------------------- LOADING DATA FRAMES TO OUR GOOGLE SHEETS ---------------------------------------------------------------------------------------------------
# To automate the process
# This is the link to our Google Sheet - where we want to write the data
gs_link <- "https://docs.google.com/spreadsheets/d/1dxRz_GIwvLYFF1IIjED8ElfG5lJ4csWz6x1sfyMMY_E/edit?usp=sharing"

# We start by authenticating with Google Sheets
if (!gs4_has_token()) {
  gs4_auth()
}

# Then we created or authenticate the Google Sheets client
sheets <- gs4_get(gs_link)

# Then specified the data frame to write to Google Sheets
data_to_write <- Clean_Data

# Finally, we write the data frame to the Google Sheet without adding an extra header row
write_sheet(data = data_to_write, ss = sheets, sheet = "Clean_Data")


#----------------------------------------------------explore time series and checking for seasonality -------------------------------------------------------------------------------------------------------------------------------------- 

births <- Clean_Data %>%  
  filter(Clean_Indicator == "Live.birth")

births <- births %>% mutate(time_period = as.numeric(month)) 
births <- births %>% mutate(time_period = ifelse(year == 2021, time_period + 12,ifelse(year==2022 ,time_period + 24,time_period)))
temp <- births %>% select(organisationunitname,final_value_total_births,time_period)  
temp <- na.omit(temp) 
temp <- temp %>% 
  group_by(year,month) %>% 
  summarize(final_value_total_births = sum(final_value_total_births))

total_births <- temp$final_value_total_births 
births_ts<-ts(total_births,start=c(2020,1),frequency = 12) 
births_ts_components <- decompose(births_ts)
isSeasonal(births_ts)
kw(births_ts,freq=12)

#plot                            
#library(ggplot2)
#mean_indicator_missing$indicator <- factor(mean_indicator_missing$indicator, levels = mean_indicator_missing$indicator[order(mean_indicator_missing$prop_incomplete_mean)])
#gplot(mean_indicator_missing, aes(x = indicator, y = prop_incomplete_mean)) + geom_point(aes(prop_incomplete_mean)) + geom_text(label=rownames(mean_indicator_missing))