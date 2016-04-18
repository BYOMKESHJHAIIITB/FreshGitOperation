#Read file 
#get Information
#Extract cases and visualize:
	# Divide into columns and visualize
	# Show information in a Human understable form.

library(ggplot2)
library(jsonlite)
library(rjson)
library(zoom)
library(plotly)
library(plyr)
library(lattice)
library(gridExtra)

#Function to read the file and get into a frame.
readFile <- function(){
	ans <- readline("Input the file name")
	print ("Reading File")
	input_file <- file(ans,"r")
	lns <- readLines(input_file,-1L)
	account_info <- lapply(X = lns, fromJSON)  
  	print("File Ready : Start Analysis:-----")
  	
  	return (account_info)	
}

#Function to get the number of add,dele and ret in bundles.
getOperationCount <- function(final_position){
	operation_seq <- c(0,0,0)
	for (cntr in 1:length(final_position)){
		if(final_position[cntr] == "ret"){
			operation_seq[1] = operation_seq[1] + 1
		}
		else if(final_position[cntr] == "dele"){
			operation_seq[2] = operation_seq[2] + 1
		}
		else{
			operation_seq[3] = operation_seq[3] + 1 		
	}

	return (operation_seq)
}

#Function to get all the sources, where deletion happened
getAdditionSource <- function(final_position,source_candidates){
	add_source <- c()
	for (cntr in 1:length(final_position)){
		if(final_position[cntr] == "add" & source_candidates[cntr] %in% add_source == FALSE ){
			add_source <- c(add_source,source_candidates[cntr])
		}		
	}

	return (add_source)
}


visualizeDelNotFolAdd <- function(cnt,simi_data,operation){
  
  length_of_seed <- length(simi_data)
  final_position <- head(simi_data[[cnt]])$fp
  title_candidates <- head(simi_data[[cnt]])$title
  source_candidates <- head(simi_data[[cnt]])$source
  mean_dist <- head(simi_data[[cnt]])$mean_dist
  mean_angle <- head(simi_data[[cnt]])$mean_angle  

  mod_fp <- c()
  mod_dist <- c()
  mod_angle <- c()

  for (each in 2:length(final_position)){
    mod_fp <- c(mod_fp,final_position[each])
    mod_dist <- c(mod_dist,mean_dist[each])
    mod_angle <- c(mod_angle,mean_angle[each])
  }

  #Getting operation count
  op_count <- getOperationCount(mod_fp)

  #Addition source
  add_source <- getAdditionSource(mod_fp,source_candidates)

  #Find source with no addition

  DelNotFolAdd <- TRUE 

  for (cntr in 1:length(mod_fp)){
  	if(mod_fp[cntr] == "dele" & source_candidates[cntr] %in% add_source ==  FALSE){
  		DelNotFolAdd <- TRUE  		
  	}
  }






  

	
	if(add > 1 & dele >1  & ret >1  & length(final_position) > 6 & DelNotFolAdd == TRUE){
    print ("            *****************************************")
		print ("*************PLOT EUCLIDEAN AND ANGLE RESPECT TO SEED*****************")
    print ("            *****************************************")
    print (mod_dist)
    print (mod_angle)
    		
    print ("==========================TITLE=================================")
    print (title_candidates)

    print ("===================POSITION======================")
    print (final_position)
    print ("===================SOURCE======================")
    print (source_candidates)
		d_fr  = data.frame(mean_dist,mean_angle)
		p <- plot_ly(plotly::hobbs, r = mod_dist, t = mod_angle, color = mod_fp, opacity = 1, mode = "markers")
		out <- layout(p, title = "Seeds_Candidates", plot_bgcolor = toRGB("grey90"))
		print (out)

		itrp <- readline("wah I m reading")
	}
  }



#Function to start the program and get choice of operation from user, i.e. operation specific.
main <- function(){
	ans <- readline("Which operation You want to see: || 1:RET|| --- || 2:DELE|| --- ||3:ADD||")
	operation <- ""
	if(ans == 1){
		operation <- "ret"
	}
	else if(ans == 2){
		operatiion <- "dele"
	}
	else{
		operation <- "add"
	}

	#Data frame from input file 
	d_frame <- readFile()

	#Visualization of bundles : bundle by bundle
	for (cntr in 1:300){
		visualizeDelNotFolAdd(cntr,d_frame,operation)
	}
}

main()