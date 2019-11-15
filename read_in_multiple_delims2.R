# Accept vector of filepaths (fps), and dataframe names (dfNames) and return a list of dataframes
# data.table's fread is used when fast = T and installs it if already not installed
# header, row.names, and skip coorespond to parameters in read.delim when fast = N

read_in_multiple_delims2 = function(fps, dfNames, fast=TRUE, header=T, row.names = NULL, skip = 0){
  if(fast == TRUE){
    # First check if data.table installed, if not install
    if(!require(data.table, character.only = T)){
      print("Installing data.table now.")
      install.packages("data.table")
    }else{
      library(data.table)
    }
    
    # Check that file paths and dataframe names match in length
    if(length(fps) != length(dfNames)){
      stop("The length of the filepaths don't match the number of names for the dataframes.")
    }
    
    # Check that filepaths are valid
    f_exists = lapply(fps, file.exists)
    names(f_exists) = fps
    
    if(length(which(f_exists == F)) > 0){
      print(paste0(names(which(f_exists == F)), " does not exist.", collapse = " "))
      stop("Fix filepaths.")
    }
      
    # Read files in
    # Use read.delim
    if(fast == FALSE){
      print("Reading in files.")
      list4dfs = lapply(fps, function(g){
        read.delim(g, header = header, row.names = row.names, skip = skip)
      })
      
      names(list4dfs) = dfNames
      return(list4dfs)
      
    }else{
      # Use data.table fread
      print("Reading in files.")
      list4dfs_fread = lapply(fps, fread)
      
      names(list4dfs_fread) = dfNames
      return(list4dfs_fread)
      
    }
  }
}  
  
  