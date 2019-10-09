#SET WORKING DIRECTORY TO THE RANKING FOLDER
parent_folder <- "~/Desktop/Nigeria/Hard_to_reach/New_Organization/H2R_SEPT_SO"
#AGGREGATED H2R DATA FILE NAME (CSV)
agg_data_file <- "LGA_GLOBAL_LGA_Results_H2R_sept"
#GEOGRAPHIC AGGREGATION--"WARD" OR "LGA" FILES
geo_agg <- "LGA"
#REPORT PERCENT OR COUNT
percent_or_count <- "percent"
#CSV OF LAST MONTH LGA --- IF NONE, THEN ENTER "NA"(IN QUOTATIONS)    "LGA_GLOBAL_LGA_Results_H2R_August"
last_month_results_file <- "NA"
#GIS FILE NAME
gis_file_name <- "Wards_X_GRID3_number-settlements.xlsx"
gis_sheet_name <- "Feuil1"
##############################RANKED DELUX###################################
#indicator_to_rank == IN QUOTATIONS: name of the last portion of the indicator to rank
#rank_type == IN QUOTATIONS: "count" or "percent"
#top_howmany == Top N (3 == top 3; -3 == bottom 3)
#ranklabels == Label of these ranked indicators
#geovary == IN QUOTATIONS: column of aggregate units
rank_delux <- function(dataa,indicator_to_rank,rank_type,top_howmany,ranklabels, geovary){
  ###SUBSET RANKED INDICATORS
  #RANKED: PUSH REASONS
  first_sub <- min(grep(indicator_to_rank ,colnames(dataa))) 
  last_sub <- max(grep(indicator_to_rank ,colnames(dataa))) 
  #AGGREGATION UNIT
  geounit <- grep(paste0("^",geovary,"$"),colnames(dataa))
  to_rank <- dataa[,c(geounit,first_sub:last_sub)]
  to_rank <- dplyr:: select(to_rank,contains(indicator_to_rank))
  geoidd <- dataa[,c(geounit)]
  to_rank <- data.frame(geoidd, to_rank)
  ######PERCENT OR COUNT#########
  type <- rank_type
  if(type == "count"){
    iddd <- to_rank[1]
    to_rank[1] <- NULL
    to_rank <- to_rank[ ,!grepl("pr_", names( to_rank ))]
    to_rank <- to_rank[ ,!grepl("total_respondents", names( to_rank ))]
  } else if(type=="percent"){
    print("PERCENT")
    iddd <- to_rank[1]
    to_rank[1] <- NULL
    to_rank <- to_rank[ ,grepl("pr_", names( to_rank ))]
    to_rank <- to_rank[ ,!grepl("total_respondents", names( to_rank ))]
  }
  to_rank <- data.frame(iddd,to_rank)
  colnames(to_rank)[1] <- geovary
  to_rank[sapply(to_rank, is.na)] <- 0
  #RANK TOP 3 INDICATORS
  to_rank <- rank_money2(to_rank, geovary, top_howmany,ranklabels)
  #LABEL COLUMNS
  names(to_rank)[2:ncol(to_rank)] <- paste0(ranklabels,"_",names(to_rank)[2:ncol(to_rank)])
  colnames(to_rank)[1] <- "group"
  to_rank$group <- as.data.frame(lapply(to_rank$group, unlist))
  to_rank <-  data.frame(t(apply(to_rank, 1, unlist)))
  #REMOVE "pr_" FROM RESULT STRINGS
  if(type=="percent"){
    changed_percent <- list()
    for(j in grep("name1",colnames(to_rank)): ncol(to_rank)){
      to_rank[,j] <- gsub("^.{0,3}", "", to_rank[,j])
    }
  } else {
    to_rank <- to_rank
  }
  return(to_rank)
}
appled <- rank_delux(data_file,"N_info_source_who","percent",3,"N_info_source_who", "LGA")

############RANK VALUES: VERSION 2.0##################
#df == Dataframe of columns -- NOTE THAT IT MUST BE THE ID COLUMNS AND THE REST ARE THE COLUMNS TO BE RANKED
#aggunit == IN QUOTATIONS: Aggregation unit
#toprank == Top-n ranking (e.g., 5 produces the top 5; -5 produces bottom 5)
rank_money2 <- function(df, aggunit, toprank, ranklabels) {
  callag <- melt(df, id.vars = c(aggunit))
  id_index <- grep(paste("^",aggunit,"$", sep=""),colnames(callag))
  unique_units <- unique(callag[id_index])
  unique_units<-as.data.frame(unique_units)
  if(toprank >= 1){
    direction <- TRUE
  } else(
    direction <- FALSE
  )
  snowflakes <- vector("list")
  toprank <- abs(toprank)
  for (i in 1:nrow(unique_units)){
    snowflakes[[i]] <- subset(callag, get(aggunit) == unique_units[i,])
  }
  snowflakes<-  lapply(snowflakes, function(x) x[!duplicated(x), ])
  sorted_dataframes_list <- lapply(snowflakes, function(df){
    df[order(df$value,decreasing = direction),]
  })
  rankked <- lapply(sorted_dataframes_list,head,n=toprank)
  castedd <- lapply(rankked, function(df){
    units_variable <- as.formula(paste0(as.symbol(aggunit),"~", "factor(",as.symbol("variable"),",levels=unique(",as.symbol("variable"),"))","+",as.symbol("value")))
    dcast(df, units_variable) 
  }) 
  trimcast <- lapply(castedd, function(df){
    sub("_[^_]+$", "", names(df[2:(toprank+1)]))
  })
  for (k in 1: nrow(unique_units)){
    for (j in (toprank+2):(toprank+1+toprank)){
      castedd[[k]][j]<-NA
    }
  }
  for (k in 1: nrow(unique_units)){
    for (j in 1: toprank){
      castedd[[k]][j+toprank+1] <- trimcast[[k]][j] 
    }
  }
  named <-c()  
  for (h in 1:toprank){
    named[h] <- paste0("rank",h,sep="")
  }
  ranknamed <-c() 
  for (l in 1:toprank ){
    ranknamed[l] <- paste0("name",l,sep="")
  }
  titles <- c("geounit", named,ranknamed)
  castedd <- lapply(castedd, setNames, titles)
  locations <- df[grep(paste0("^",aggunit,"$"),colnames(df))]
  locations <- unique(locations)
  ordername <- data.frame(matrix(unlist(castedd), nrow=nrow(unique_units), byrow=T),stringsAsFactors=FALSE)
  colnames(ordername) <- titles
  for (j in 1: toprank+1){
    ordername[j]<-round(as.numeric(unlist(ordername[j])),4)
  }
  ordername$geounit<-locations
  ordername[ordername == 0] <- NA
  names(ordername)[1]<-aggunit
  for(i in 2:(1+toprank)){
    ordername[,i+toprank] <- ifelse(is.na(ordername[,i]),NA,ordername[,i+toprank])
  }
  ordername <- as.data.frame(ordername,stringsAsFactors = FALSE)
  return(ordername)
}
appled <- as.numeric(grep("E_food_no_reason1", colnames(data_file)))
terror <- data_file[,c(2,appled)]
apple <- rank_money2(terror, "LGA", 3,"jkldjf")


#########SMALL LOOP TO UPDATE RANKED NAMES#########
#old_new_names == excel file with "old" and "new" columns
update_ranked_names <- function(ranked_data,old_new_names){ 
  for(i in 1:nrow(old_new_names)){
    ranked_data <-  data.frame(lapply(ranked_data, function(x) gsub(as.character(old_new_names[i,1]), as.character(old_new_names[i,2]), x)), stringsAsFactors=F)
  }
  return(ranked_data)
}


############RANK VALUES: VERSION 2.5: NAMES AUTOMATIC##################
#df == Dataframe of columns -- NOTE THAT IT MUST BE THE ID COLUMNS AND THE REST ARE THE COLUMNS TO BE RANKED
#aggunit == IN QUOTATIONS: Aggregation unit
#toprank == Top-n ranking (e.g., 5 produces the top 5; -5 produces bottom 5)
rank_money2_5 <- function(df, aggunit, toprank,ranklabels) {
  callag <- melt(df, id.vars = c(aggunit))
  id_index <- grep(paste("^",aggunit,"$", sep=""),colnames(callag))
  unique_units <- unique(callag[id_index])
  unique_units<-as.data.frame(unique_units)
  if(toprank >= 1){
    direction <- TRUE
  } else(
    direction <- FALSE
  )
  snowflakes <- vector("list")
  toprank <- abs(toprank)
  for (i in 1:nrow(unique_units)){
    snowflakes[[i]] <- subset(callag, get(aggunit) == unique_units[i,])
  }
  snowflakes<-  lapply(snowflakes, function(x) x[!duplicated(x), ])
  sorted_dataframes_list <- lapply(snowflakes, function(df){
    df[order(df$value,decreasing = direction),]
  })
  rankked <- lapply(sorted_dataframes_list,head,n=toprank)
  castedd <- lapply(rankked, function(df){
    units_variable <- as.formula(paste0(as.symbol(aggunit),"~", "factor(",as.symbol("variable"),",levels=unique(",as.symbol("variable"),"))","+",as.symbol("value")))
    dcast(df, units_variable) 
  }) 
  trimcast <- lapply(castedd, function(df){
    sub("_[^_]+$", "", names(df[2:(toprank+1)]))
  })
  for (k in 1: nrow(unique_units)){
    for (j in (toprank+2):(toprank+1+toprank)){
      castedd[[k]][j]<-NA
    }
  }
  for (k in 1: nrow(unique_units)){
    for (j in 1: toprank){
      castedd[[k]][j+toprank+1] <- trimcast[[k]][j] 
    }
  }
  named <-c()  
  for (h in 1:toprank){
    named[h] <- paste0("rank",h,sep="")
  }
  ranknamed <-c() 
  for (l in 1:toprank ){
    ranknamed[l] <- paste0("name",l,sep="")
  }
  titles <- c("geounit", named,ranknamed)
  castedd <- lapply(castedd, setNames, titles)
  locations <- df[grep(paste0("^",aggunit,"$"),colnames(df))]
  locations <- unique(locations)
  ordername <- data.frame(matrix(unlist(castedd), nrow=nrow(unique_units), byrow=T),stringsAsFactors=FALSE)
  colnames(ordername) <- titles
  for (j in 1: toprank+1){
    ordername[j]<-round(as.numeric(unlist(ordername[j])),4)
  }
  ordername$geounit<-locations
  ordername[ordername == 0] <- NA
  names(ordername)[1]<-aggunit
  for(i in 2:(1+toprank)){
    ordername[,i+toprank] <- ifelse(is.na(ordername[,i]),NA,ordername[,i+toprank])
  }
  group_name <- ordername[1]
  #GSUB 
  namesz <- dplyr:: select(ordername, contains("name"))
  if(any(grepl('pr_', ordername)==TRUE, na.rm=FALSE)){
    print("percent")
    namesz <- namesz %>%  mutate_all(funs(gsub(paste0("pr_",ranklabels,"_"), "", .))) %>%
      mutate_all(funs(gsub("\\.", " ", .))) %>%
      mutate_all(funs(gsub("_", " ", .))) %>%
      mutate_all(funs(gsub("/", " ", .))) %>%
      mutate_all(funs(gsub("-", " ", .))) %>%
      mutate_each( funs(tools::toTitleCase(.)))
  } else{
    print("count")
    namesz <- namesz %>%  mutate_all(funs(gsub(paste0(ranklabels,"_"), "", .))) %>%
      mutate_all(funs(gsub("\\.", " ", .))) %>%
      mutate_all(funs(gsub("_", " ", .))) %>%
      mutate_all(funs(gsub("/", " ", .))) %>%
      mutate_all(funs(gsub("-", " ", .))) %>%
      mutate_each( funs(tools::toTitleCase(.))) 
  }
  rest <-  dplyr:: select(ordername, -contains("name"))
  ordername <- cbind( rest,namesz)
  ordername[1] <- group_name
  return(ordername)
}
#apple <- rank_money2_5(try_rank, "LGA", 3,"jkldjf")

#################################RANK FUNCTION######################################
#OUTPUTS CSVs OF RANKED INDICATORS FROM LIST OF [KOBO] QUESTION NAMES
#dataset == dataframe with specified indicators
#geo_level == IN QUOTATIONS: The column name of the header defining the aggregation level of "dataset"
#percent_count == IN QUOTATIONS: Define as "percent" or "count" -- whether raw counts or %s are used to rank the data
#processed_outputs_folder == IN QUOTATIONS: Name of the output folder where the ranked indicator CSVs should be stored
#indics_to_rank_df == A character vector of the names of indicators to rank
#name_replace == A dataframe with old name-option strings and their "clean" definitions
rank_indicators <- function(dataset, geo_level, percent_count, processed_outputs_folder,indics_to_rank_df,name_replace) {
  #REMOVE "NO/0" FROM THE AGGREGATED DATASET
  dataset <- dplyr:: select(dataset, -contains("_0"))
  dataset <- as.data.frame(dataset)
  #RENAME FIRST COLUMN OF LIST OF INDICATORS TO-BE-RANKED
  colnames(indics_to_rank_df)[1] <- "indic_to_rank"
  #CONVERT TO CHARACTER VECTOR
  indics_to_rank_df <- as.character(indics_to_rank_df$indic_to_rank)
  #LOOP: RANK EACH INDICATOR
  for(i in 1: length(indics_to_rank_df)){
    print("indicator")
    print(i)
    #COUNT RESPONSES PER_QUESTION
    indices_per_question <-  grep(indics_to_rank_df[i], colnames(dataset))
    question_subset <- dataset[,indices_per_question]
    #question_subset <- question_subset %>% dplyr::select(-contains("pr_"))
    num_question_subset <- length(names(question_subset))
    #DEFINE TOP-X 
    if(num_question_subset == 3 | num_question_subset == 4){
      top_x <- 3
    } else if (num_question_subset >=5){
      top_x <- 5
    } else if (num_question_subset ==2){
      top_x <- 2
    } else if (num_question_subset ==1){
      top_x <- 1
    } else {
      print("NOT ENOUGH TO RANK")
      next
    }
    #RANK
    #print(indics_to_rank_df[i])
    ranked <- rank_delux(dataset,indics_to_rank_df[i],percent_count,top_x,indics_to_rank_df[i],geo_level)
    ranked <- update_ranked_names(ranked,name_replace)
    write.csv(ranked,paste0("Analysis/Indicator_Ranking/",processed_outputs_folder,"/",indics_to_rank_df[i],".csv"),row.names=FALSE)
  }
}
ranked <- rank_delux(data_file,as.character(indics_to_rank[1,]),"percent",2,as.character(indics_to_rank[1,]),"LGA")
###MOVE COLUMNS-NOT MINE
moveme <- function (invec, movecommand) {
  movecommand <- lapply(strsplit(strsplit(movecommand, ";")[[1]], 
                                 ",|\\s+"), function(x) x[x != ""])
  movelist <- lapply(movecommand, function(x) {
    Where <- x[which(x %in% c("before", "after", "first", 
                              "last")):length(x)]
    ToMove <- setdiff(x, Where)
    list(ToMove, Where)
  })
  myVec <- invec
  for (i in seq_along(movelist)) {
    temp <- setdiff(myVec, movelist[[i]][[1]])
    A <- movelist[[i]][[2]][1]
    if (A %in% c("before", "after")) {
      ba <- movelist[[i]][[2]][2]
      if (A == "before") {
        after <- match(ba, temp) - 1
      }
      else if (A == "after") {
        after <- match(ba, temp)
      }
    }
    else if (A == "first") {
      after <- 0
    }
    else if (A == "last") {
      after <- length(myVec)
    }
    myVec <- append(temp, values = movelist[[i]][[1]], after = after)
  }
  myVec
}
#############################RUN FUNCTIONS###############################
setwd(parent_folder)
agg_data_folder <- "LGA_Results"
#GIS (INPUT) FOLDER
gis_working_directory <- paste0(parent_folder,"/","Analysis","/","GIS_Settlement_list")
names_working_directory <- paste0(parent_folder,"/","Analysis")
#NAME OF THE LAST GEO-AGGREGATION/GROUPING VARIABLE IN THE RANKED FILES
last_id_col <- "group"
####ADD AGGREGATED DATA TO STACK####
lga_global_stacked <- read_csv(paste0(parent_folder,"/","Analysis/",agg_data_folder,"/",agg_data_file,".csv"))
#RANKED FILES FOLDER
rank_filepath <- paste0(parent_folder,"/","Analysis","/","Indicator_Ranking","/","processed_ranked_outputs")
#LOAD AGGREGATED H2R DATA AND REMOVE GLOBAL "yes" & "noresponse" COLUMNS
data_file <- read_csv(paste0("Analysis/",agg_data_folder,"/",agg_data_file,".csv")) 
data_file <- data_file %>% dplyr::filter(!is.na(!!sym(geo_agg))) %>%
  dplyr:: select(-ends_with("_yes")) %>%
  dplyr:: select(-ends_with("_noresponse")) %>%
  dplyr::select(-ends_with("_no"))
#INDICATORS TO BE RANKED
indics_to_rank <- read_excel(paste0("Analysis/","Indicator_Ranking","/","INDICATORS_TO_RANK.xlsx"),   sheet = "list_to_rank",col_names = FALSE)
ranked_csv_file <- as.character(indics_to_rank[[1]])
#NEW NAMES OF RANKED DATA
old_new_name_replace <- read_excel(paste0("Analysis/","Indicator_Ranking","/","INDICATORS_TO_RANK.xlsx"),   sheet = "name_change",col_names = FALSE)
#RUN--SHOOTS CSVs TO FOLDER
rank_indicators(data_file,geo_agg,percent_or_count, "processed_ranked_outputs",indics_to_rank,old_new_name_replace)
######################START IMPORT AND ARRANGE######################
files_to_change <- list()
for(i in 1: length(ranked_csv_file)){
  one_rank_to_change <- read_csv(paste0(rank_filepath,"/",ranked_csv_file[i],".csv"))
  if( ncol(one_rank_to_change[(grep(last_id_col,colnames(one_rank_to_change))+2):grep("name1",colnames(one_rank_to_change))-1]) ==2  ){
    idx <- one_rank_to_change[1:grep(last_id_col,colnames(one_rank_to_change))]
    ranked <- one_rank_to_change[(grep(last_id_col,colnames(one_rank_to_change))+2):grep("name1",colnames(one_rank_to_change))-1]
    ranked[3] <- NA
    ranked[4] <- NA
    ranked[5] <- NA
    colnames(ranked)[3] <- paste0(ranked_csv_file[i],"_rank3")
    colnames(ranked)[4] <- paste0(ranked_csv_file[i],"_rank4")
    colnames(ranked)[5] <- paste0(ranked_csv_file[i],"_rank5")
    ranknames <- one_rank_to_change[(grep("name1",colnames(one_rank_to_change))):ncol(one_rank_to_change)]
    ranknames[3] <- NA
    ranknames[4] <- NA
    ranknames[5] <- NA
    colnames(ranknames)[3] <- paste0(ranked_csv_file[i],"_name3")
    colnames(ranknames)[4] <- paste0(ranked_csv_file[i],"_name4")
    colnames(ranknames)[5] <- paste0(ranked_csv_file[i],"_name5")
    ranked_added <- cbind( ranked,ranknames)
  } else if(ncol(one_rank_to_change[(grep("rank1",colnames(one_rank_to_change))+1):grep("name1",colnames(one_rank_to_change))-1]) ==3) {
    idx <- one_rank_to_change[1:grep(last_id_col,colnames(one_rank_to_change))]
    ranked <- one_rank_to_change[(grep("rank1",colnames(one_rank_to_change))+1):grep("name1",colnames(one_rank_to_change))-1]
    ranked[4] <- NA
    ranked[5] <- NA
    colnames(ranked)[4] <- paste0(ranked_csv_file[i],"_rank4")
    colnames(ranked)[5] <- paste0(ranked_csv_file[i],"_rank5")
    ranknames <- one_rank_to_change[(grep("name1",colnames(one_rank_to_change))):ncol(one_rank_to_change)]
    ranknames[4] <- NA
    ranknames[5] <- NA
    colnames(ranknames)[4] <- paste0(ranked_csv_file[i],"_name4")
    colnames(ranknames)[5] <- paste0(ranked_csv_file[i],"_name5")
    ranked_added <- cbind(ranked,ranknames)
  } else if(ncol(one_rank_to_change[(grep("rank1",colnames(one_rank_to_change))+1):grep("name1",colnames(one_rank_to_change))-1]) ==4){
    idx <- one_rank_to_change[1:grep(last_id_col,colnames(one_rank_to_change))]
    ranked <- one_rank_to_change[(grep("rank1",colnames(one_rank_to_change))+1):grep("name1",colnames(one_rank_to_change))-1]
    ranked[5] <- NA
    colnames(ranked)[5] <- paste0(ranked_csv_file[i],"_rank5")
    ranknames <- one_rank_to_change[(grep("name1",colnames(one_rank_to_change))):ncol(one_rank_to_change)]
    ranknames[5] <- NA
    colnames(ranknames)[5] <- paste0(ranked_csv_file[i],"_name5")
    ranked_added <- cbind(ranked,ranknames)
  } else{
    ranked_added <- one_rank_to_change[grep("rank1",colnames(one_rank_to_change)):ncol(one_rank_to_change)]
  }
  print(ranked_added)
  files_to_change[[i]] <- ranked_added
}
add_ranked  <- do.call(cbind.data.frame, files_to_change) #COMBINE COLUMNS OF RANKED INDICATORS
lga_global_stacked <- lga_global_stacked %>% dplyr::filter(!is.na(.[[1]]))  #REMOVE GLOBAL FROM ORIGINAL
lga_plus_ranks <- cbind(lga_global_stacked,add_ranked) #COMBINE ORIGINAL AGGREGATION AND RANKINGS
#write.csv(lga_plus_ranks,paste0(parent_folder,"/","Analysis","/",geo_agg,"_","Results","/",geo_agg,"_","RANKED_",agg_data_file,".csv"),row.names=FALSE,na = "")

######INCLUDE ALL INDICATOR NAMES######
#REMOVE "pr_" FROM AGGREGATED FILE COLUMN NAMES
names(lga_plus_ranks)[(grep("@prop_walkable_education",colnames(lga_plus_ranks))+1) : (grep("^gis_settlment_cnt$", colnames(lga_plus_ranks))-1)] <- substring(names(lga_plus_ranks)[(grep("@prop_walkable_education",colnames(lga_plus_ranks))+1): (grep("^gis_settlment_cnt$", colnames(lga_plus_ranks))-1)],4,1000)
#LOAD COMPLETE RESPONSES FILE
complete_responses <- read_excel(paste0("Analysis/","Indicator_Ranking","/","INDICATORS_TO_RANK.xlsx"),   sheet = "complete_response_list",col_names = FALSE)
#ADD NEW INDICATORS
for(j in 1: nrow(complete_responses)){
  print(j)
  if(length(grep(as.character(complete_responses[j,1]) ,colnames(lga_plus_ranks)))>0){
    print("TRUE")
  } else{
    print("FALSE")
    need_indic <- as.data.frame(rep(NA, nrow(lga_plus_ranks)))
    colnames(need_indic) <- as.character(complete_responses[j,1])
    lga_plus_ranks <- cbind(lga_plus_ranks,need_indic)
  }
}
geo_info <- lga_plus_ranks[1:grep("State",colnames(lga_plus_ranks))]
maps <- lga_plus_ranks[grep("@assessed_settlements", colnames(lga_plus_ranks)): grep("@prop_walkable_education", colnames(lga_plus_ranks))]
gis_ranks <- lga_plus_ranks[grep("gis_settlment_cnt",colnames(lga_plus_ranks)):max(grep("_name5",colnames(lga_plus_ranks)))]
rid_names <- c(colnames(geo_info),colnames(maps), colnames(gis_ranks))
indicators <- lga_plus_ranks[,!names(lga_plus_ranks) %in% rid_names]
#USELESS
non_essential_indic <- setdiff(colnames(indicators),complete_responses[[1]])
non_essential_indic<- lga_plus_ranks[non_essential_indic]
indicators <- indicators[,!names(indicators) %in% colnames(non_essential_indic)]
indicators <- cbind(indicators,non_essential_indic)
#REORDER DATAFRAME
indicators <- indicators[,order(names(indicators))]
#RE-JOIN ORDERED
lga_plus_ranks <- cbind(geo_info,maps,indicators,gis_ranks)

#####LOAD AND PROCESS LAST MONTH RESULTS####
if(last_month_results_file != "NA"){
  last_month_results <- read.csv(paste0("Analysis/Indicator_Ranking/",last_month_results_file,".csv"),stringsAsFactors=F)
  #MANIPUALTE
  lgas_last_month <- last_month_results[,1]
  data_last_month <- last_month_results %>% dplyr::select(contains("pr_"))
  colnames(data_last_month)[1:ncol(data_last_month)] <- paste0("last_month_",colnames(data_last_month)[1:ncol(data_last_month)])
  max_min_value <- max(apply(data_last_month,2,min),na.rm = TRUE)
  data_last_whole_num <- list()
  if(max_min_value <= 1){
    for(i in 1: ncol(data_last_month)){
      data_last_whole_num[[i]] <-round(data_last_month[i] * 100,0) 
      data_last_whole_num[sapply(data_last_whole_num, is.null)] <- NULL
      data_last_whole_num <- data.frame(data_last_whole_num)
    }
  } else{
    print("already multiplied to absolute %'s")
    data_last_whole_num <- data_last_month
  } 
  last_month_results<-data.frame(lgas_last_month,data_last_whole_num,stringsAsFactors = FALSE)
  last_month_results <- last_month_results %>% dplyr::filter(lgas_last_month !="")
  #GIS -- ADD P-CODES
  gis <- read_excel(paste0(gis_working_directory,"/",gis_file_name), sheet = gis_sheet_name)
  gis_lga <- gis %>% dplyr::distinct(ADM2_EN,ADM2_PCODE) 
  colnames(last_month_results)[1] <- "ADM2_EN"
  last_month_results <- merge(last_month_results,gis_lga,by="ADM2_EN")
  last_month_results <- last_month_results[moveme(names(last_month_results), "ADM2_PCODE after ADM2_EN")]
  colnames(last_month_results)[2] <- "LGA"
  differences_month <- merge(lga_plus_ranks,last_month_results, by="LGA", all.x=TRUE)
  #CALCUALTE MONTH/MONTH DIFFERENCES
  calc_differences <- list()
  for(j in grep("D_D1_community_leadership_groups_disabled_TRUE",colnames(differences_month)):grep("N_radio_existing_yes",colnames(differences_month)) ){
    print(j)
    if( ncol(  differences_month[colnames(differences_month[j])]) ==1 & ncol( differences_month[grep(paste0("last_month_pr_",colnames(differences_month[j])), colnames(differences_month))] ) ==1 ) {
      print("TRUE")
      calc_differences[[j]] <- differences_month[grep(paste0("^",colnames(differences_month[j]),"$"),colnames(differences_month))] -   differences_month[grep(paste0("last_month_pr_",colnames(differences_month)[j]),colnames(differences_month))]
    } else{
      next
    }
  }
  calc_differences[sapply(calc_differences, is.null)] <- NULL
  calc_differences <- data.frame(calc_differences)
  colnames(calc_differences)[1:ncol(calc_differences)] <- paste0( "differences_", colnames(calc_differences)[1:ncol(calc_differences)] )
  lgas_add <- differences_month[1]
  calc_differences <- data.frame(lgas_add,calc_differences)
  lga_plus_ranks_diff <- cbind(lga_plus_ranks, calc_differences)
  #EXPORT
  write.csv(lga_plus_ranks_diff,paste0(parent_folder,"/","Analysis","/",geo_agg,"_","Results","/",geo_agg,"_","RANKED_DIFFS_",agg_data_file,".csv"),row.names=FALSE,na = "")
} else{
  write.csv(lga_plus_ranks,paste0(parent_folder,"/","Analysis","/",geo_agg,"_","Results","/",geo_agg,"_","RANKED_NO_DIFFS_",agg_data_file,".csv"),row.names=FALSE,na = "")
}

