## Function to remove or replace 
data.Clean<- function(data_mat_in,method = "Remove"){
  glevels <- unique(data_mat_in[,1])
  group_mean_vect <- NULL
  group_mode_vect<- NULL 
  group_median_vect <- NULL
  g_vect_pure <- data_mat_in[,1]
  data_ret <-NULL
  
  if(method == "Remove"){
    data_ret <- na.omit(data_mat_in)
  }else if(method == "Mean"){
    ### perform if to see if group data is missing 
    g_check <- unique(data_mat_in[,1])
    if(any(is.na(data_mat_in[,1])) ){
      stop("A group point is missing. Must use remove method for integrity of test")
    }
    
    # collect row which is na and then check group value
    row_with_na_mat <- na.omit(as.data.table(data_mat_in), invert = TRUE)
    row_with_na_mat <- as.data.frame(row_with_na_mat)
    glevels <- unique(row_with_na_mat[,1])
    
    ## now replace
    for(g in 1:length(glevels)){
      gi <- glevels[g]
      for (j in 2:ncol(row_with_na_mat)) {
        meanGj <- mean(na.omit(data_mat_in[g_vect_pure == gi,j]) ) ## get me for group for column
        index_vect <- which(is.na(row_with_na_mat[,j]) & row_with_na_mat[,1]==gi ,arr.ind = T) ## get indeces of group
        row_with_na_mat[index_vect,j] <- meanGj ## insert mean in there
      }
    }
    
    data_ret <- rbind(na.omit(data_mat_in),row_with_na_mat)
  } else if(method == "Mode"){
    g_check <- unique(data_mat_in[,1])
    if(any(is.na(data_mat_in[,1])) ){
      stop("A group point is missing. Must use remove method for integrity of test")
    }
    
    # collect row which is na and then check group value
    row_with_na_mat <- na.omit(as.data.table(data_mat_in), invert = TRUE)
    row_with_na_mat <- as.data.frame(row_with_na_mat)
    glevels <- unique(row_with_na_mat[,1])
    
    ## now replace
    for(g in 1:length(glevels)){
      gi <- glevels[g]
      for (j in 2:ncol(row_with_na_mat)) {
        meanGj <- Mode(na.omit(data_mat_in[g_vect_pure == gi,j]) ) ## get me for group for column
        index_vect <- which(is.na(row_with_na_mat[,j]) & row_with_na_mat[,1]==gi ,arr.ind = T) ## get indeces of group
        row_with_na_mat[index_vect,j] <- meanGj ## insert mean in there
      }
    }
    
    data_ret <- rbind(na.omit(data_mat_in),row_with_na_mat)
  } else if (method == "Median"){
    g_check <- unique(data_mat_in[,1])
    if(any(is.na(data_mat_in[,1])) ){
      stop("A group point is missing. Must use remove method for integrity of test")
    }
    
    # collect row which is na and then check group value
    row_with_na_mat <- na.omit(as.data.table(data_mat_in), invert = TRUE)
    row_with_na_mat <- as.data.frame(row_with_na_mat)
    glevels <- unique(row_with_na_mat[,1])
    
    ## now replace
    for(g in 1:length(glevels)){
      gi <- glevels[g]
      for (j in 2:ncol(row_with_na_mat)) {
        meanGj <- median(na.omit(data_mat_in[g_vect_pure == gi,j]) ) ## get me for group for column
        index_vect <- which(is.na(row_with_na_mat[,j]) & row_with_na_mat[,1]==gi ,arr.ind = T) ## get indeces of group
        row_with_na_mat[index_vect,j] <- meanGj ## insert mean in there
      }
    }
    
    data_ret <- rbind(na.omit(data_mat_in),row_with_na_mat)
  } else if(method == "Average Measures"){
    g_check <- unique(data_mat_in[,1])
    if(any(is.na(data_mat_in[,1])) ){
      stop("A group point is missing. Must use remove method for integrity of test")
    }
    
    # collect row which is na and then check group value
    row_with_na_mat <- na.omit(as.data.table(data_mat_in), invert = TRUE)
    row_with_na_mat <- as.data.frame(row_with_na_mat)
    glevels <- unique(row_with_na_mat[,1])
    
    ## now replace
    for(g in 1:length(glevels)){
      gi <- glevels[g]
      for (j in 2:ncol(row_with_na_mat)) {
        meanGj <- mean (c(Mode(na.omit(data_mat_in[g_vect_pure == gi,j]) ),
                          median(na.omit(data_mat_in[g_vect_pure == gi,j]) ),
                          mean(na.omit(data_mat_in[g_vect_pure == gi,j]) ), 
                          max((na.omit(data_mat_in[g_vect_pure == gi,j]) )), 
                          min((na.omit(data_mat_in[g_vect_pure == gi,j]) )) ) )
        
        index_vect <- which(is.na(row_with_na_mat[,j]) & row_with_na_mat[,1]==gi ,arr.ind = T) ## get indeces of group
        row_with_na_mat[index_vect,j] <- meanGj ## insert mean in there
      }
    }
    
    data_ret <- rbind(na.omit(data_mat_in),row_with_na_mat)
  }
  else if(method == "Max"){
    g_check <- unique(data_mat_in[,1])
    if(any(is.na(data_mat_in[,1])) ){
      stop("A group point is missing. Must use remove method for integrity of test")
    }
    
    # collect row which is na and then check group value
    row_with_na_mat <- na.omit(as.data.table(data_mat_in), invert = TRUE)
    row_with_na_mat <- as.data.frame(row_with_na_mat)
    glevels <- unique(row_with_na_mat[,1])
    
    ## now replace
    for(g in 1:length(glevels)){
      gi <- glevels[g]
      for (j in 2:ncol(row_with_na_mat)) {
        meanGj <- max((na.omit(data_mat_in[g_vect_pure == gi,j]) )) 
        
        index_vect <- which(is.na(row_with_na_mat[,j]) & row_with_na_mat[,1]==gi ,arr.ind = T) ## get indeces of group
        row_with_na_mat[index_vect,j] <- meanGj ## insert mean in there
      }
    }
    
    data_ret <- rbind(na.omit(data_mat_in),row_with_na_mat)
  }
  else if(method == "Min"){
    g_check <- unique(data_mat_in[,1])
    if(any(is.na(data_mat_in[,1])) ){
      stop("A group point is missing. Must use remove method for integrity of test")
    }
    
    # collect row which is na and then check group value
    row_with_na_mat <- na.omit(as.data.table(data_mat_in), invert = TRUE)
    row_with_na_mat <- as.data.frame(row_with_na_mat)
    glevels <- unique(row_with_na_mat[,1])
    
    ## now replace
    for(g in 1:length(glevels)){
      gi <- glevels[g]
      for (j in 2:ncol(row_with_na_mat)) {
        meanGj <- min((na.omit(data_mat_in[g_vect_pure == gi,j]) )) 
        
        index_vect <- which(is.na(row_with_na_mat[,j]) & row_with_na_mat[,1]==gi ,arr.ind = T) ## get indeces of group
        row_with_na_mat[index_vect,j] <- meanGj ## insert mean in there
      }
    }
    
    data_ret <- rbind(na.omit(data_mat_in),row_with_na_mat)
  }
  
  return(data_ret)
}