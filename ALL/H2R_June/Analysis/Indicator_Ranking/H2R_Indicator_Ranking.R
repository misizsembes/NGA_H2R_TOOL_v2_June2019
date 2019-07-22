#SET WORKING DIRECTORY TO THE RANKING FOLDER
parent_folder <- "~/Desktop/Nigeria/Hard_to_reach/New_Organization/H2R_June"
#AGGREGATED H2R DATA FILE NAME (CSV)
agg_data_file <- "LGA_GLOBALcleanedh2r_2019-07-22_JUNE_JULY_REACH_NGA_Tool_H2RQuant_NEW_KII_23_05_2019_final_2019_07_12_16_06_21_REACH_NGA_Tool_H2RQuant_NEW_KII"
#GEOGRAPHIC AGGREGATION
geo_agg <- "LGA"
#REPORT PERCENT OR COUNT
percent_or_count <- "percent"

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
appled <- rank_delux(data_file,"N_info_source_who","percent",2,"N_info_source_who", "LGA")

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
apple <- rank_money2(trygo, "LGA", 3,"jkldjf")

infosource_who <- grep("N_info_source_who",colnames(aggd_data))
lgaa <- aggd_data[3]
datap<- aggd_data[infosource_who]
trygo <- cbind(lgaa,datap)
appled <- rank_delux(aggd_data,"N_info_source_who","count",3,"N_info_source", "LGA")

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
  print(indics_to_rank_df)
  #LOOP: RANK EACH INDICATOR
  for(i in 1: length(indics_to_rank_df)){
     print("indicator")
    print(i)
    #COUNT RESPONSES PER_QUESTION
    indices_per_question <-  grep(indics_to_rank_df[i], colnames(dataset))
    question_subset <- dataset[,indices_per_question]
    question_subset <- question_subset %>% dplyr::select(-contains("pr_"))
    num_question_subset <- length(names(question_subset))
    #DEFINE TOP-X 
    if(num_question_subset == 3 | num_question_subset == 4){
      top_x <- 3
    } else if (num_question_subset >=5){
      top_x <- 5
    } else {
      print("NOT ENOUGH TO RANK")
    next
      }
    #RANK
    ranked <- rank_delux(dataset,indics_to_rank_df[i],percent_count,top_x,indics_to_rank_df[i],geo_level)
    ranked <- update_ranked_names(ranked,name_replace)
    write.csv(ranked,paste0("Analysis/Indicator_Ranking/",processed_outputs_folder,"/",indics_to_rank_df[i],".csv"))
  }
}

##############RUN FUNCTIONS################
setwd(parent_folder)
agg_data_folder <- "LGA_Results"
#LOAD AGGREGATED H2R DATA
data_file <- read_csv(paste0("Analysis/",agg_data_folder,"/",agg_data_file,".csv")) 
data_file <- data_file %>% dplyr::filter(!is.na(!!sym(geo_agg)))
#INDICATORS TO BE RANKED
indics_to_rank <- read_excel(paste0("Analysis/","Indicator_Ranking","/","INDICATORS_TO_RANK.xlsx"),   sheet = "list_to_rank",col_names = FALSE)
#NEW NAMES OF RANKED DATA
old_new_name_replace <- read_excel(paste0("Analysis/","Indicator_Ranking","/","INDICATORS_TO_RANK.xlsx"),   sheet = "name_change",col_names = FALSE)

#RUN--SHOOTS CSVs TO FOLDER
rank_indicators(data_file,geo_agg,percent_or_count, "processed_ranked_outputs",indics_to_rank,old_new_name_replace)

