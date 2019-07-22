#PARENT FOLDER
parent_folder <- "~/Desktop/Nigeria/Hard_to_reach/New_Organization/H2R_June"
#DEFINE RAW DATA
dataset <- "JUNE_JULY_REACH_NGA_Tool_H2RQuant_NEW_KII_23_05_2019_final_2019_07_12_16_06_21"
sheet <- "REACH_NGA_Tool_H2RQuant_NEW_KII"

hq_cleaning_checks <- function(datasett,output_folder,id_indicator_name, unnecessary_indicators ){
#CREATE INDEX IN DATASET
datasett$index  <- 1:nrow(datasett) 
#FIND ID INDICATOR COLUMN
colnames(datasett)[grep(id_indicator_name, colnames(datasett))] <- "id_indic"
#SELECT ONLY INDEX AND ID COLUMNS
data_uuid <- datasett %>% dplyr::select(index, id_indic)
#FIND OUTLIERS
outliers <- cleaninginspectoR::find_outliers(datasett)   
outlier_id <- merge(outliers,data_uuid, by="index",all.x=TRUE)
#ADD CLEANING COLUMNS
outlier_id$accept_change <- ""
outlier_id$new_value <- ""
outlier_id$dataset <-   dataset
outlier_id$sheet <-  sheet
colnames(outlier_id)[grep("^value$",colnames(outlier_id))] <- "old_value"
outlier_id <- outlier_id %>% dplyr::filter(!(variable %in% unnecessary_indicators))
#EXPORT
write.csv(outlier_id, paste0(output_folder,"/",sheet,"_","outlier_id.csv"))

###LOAD CONFIRMATIONS OF HQ CHECKS 
confirmed_outliers <- read_excel(paste0(output_folder,"/",sheet,"_","outlier_id.xlsx"), substr(paste0(sheet,"_","outlier_id"),1,31))
confirmed_outliers[1]<-NULL
#SELECT ONLY CONFIRMED CLEANS
confirmed_outliers <- confirmed_outliers %>% dplyr::filter(accept_change == "yes") %>% 
  dplyr::select(dataset,sheet, variable, old_value,new_value,issue_type,id_indic)
#MISSING COLUMNS
confirmed_outliers$folder <- ""
confirmed_outliers$modified <- "no"
confirmed_outliers$name <- "CleaningInspectoR"
#REARRANGE COLUMNS
confirmed_outliers <- confirmed_outliers[c("folder", "dataset", "sheet", "variable", "old_value", "new_value", "issue_type", "id_indic","modified","name")]

#CHANGE NAMES
cleaning_names <- c("Folder", "Dataset", "Sheet", "Question","Old Value", "New Value (if any)", "Reason (e.g. data entry error, no correction needed)", "_uuid", "Modified in cleaned dataset?", "Name")      
names(confirmed_outliers) <- cleaning_names

cleaning_log_combined <- write.csv(confirmed_outliers, paste0(output_folder,"/",sheet,"_","hq_changes.csv"))
}

#DEFINE FOLDERS
cleaning_folder <- "Raw_to_Cleaning_Process"
setwd(paste0(parent_folder,"/",cleaning_folder))
raw_data_folder <- "Raw_Data" #DO NOT CHANGE
#LOAD DATA
dataa <- read_excel(paste0(raw_data_folder,"/",dataset,".xlsx"),sheet = sheet) #DO NOT CHANGE
#RUN HQ CLEANING
unneeded_indicators <- c("B__geopoint_precision","deviceid","B__geopoint_altitude","_index")
hq_cleaning_checks(dataa, "HQ_Cleaning", "uuid",unneeded_indicators)






			