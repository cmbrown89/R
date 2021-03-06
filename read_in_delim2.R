#' @author Clairessa M. Brown
#' 
#' Read in delimited files by accepting a vector of file paths and a vector of desired dataframe names.
#' 
#' @param fp a vector of full file paths (that can be generated by list.files function).
#' @param dfNames a vector of desired dataframe names.
#' @param print_dfs_to_global_envir a boolean value deciding if dataframes should be printed to global environment
#' @param list_name character string that will be the variable name of the list printed to global environment
#' 
#' @return user-named dataframes printed to the global environment individually as variables with a list of named dataframes, or exclusively in a list.

read_in_delims2 = function(fps, dfNames, print_dfs_to_global_envir=TRUE, list_name=NULL){
  list4dfs = lapply(fps, read.delim) # read in list of dfs
  names(list4dfs) = dfNames # name dataframes in list
  
  if(print_dfs_to_global_envir == TRUE){
      for(n in seq_along(list4dfs)){
        assign(x = names(list4dfs[n]), value = list4dfs[[n]], envir = .GlobalEnv) # extract dfs by those names
      } 
  } else{
    for(n in seq_along(list4dfs)){
      assign(x = names(list4dfs[n]), value = list4dfs[[n]]) # extract dfs by those names
      }
  }
  
  if(is.null(list_name)){
    assign(x = "list_of_dfs",value = list4dfs, envir = .GlobalEnv) # return pre-named list of dfs
  } else{
    assign(x = list_name, value = list4dfs, envir = .GlobalEnv) # return user-named list of dfs
  }

}

# To do:
## Upload example data and provide example code
## Run the example code in a RMarkdown
