# Next next task: Code sanity checks (testing to see tot seq counts per sample, ect.)
# Next next next task: Code optimization: start timing functions and try to make it faster (lapply vs for loops), ect.
# Next next next next task: Provide option to get data back in wide format

generate.tax.summary.lists = function(asv_tab, taxa_tab, relAbund = TRUE){
  ############### Accept ASV table and ASV ID table from dada2 and return a list of taxonomic level-specific relative abundance tables ###############
  
  taxa_tab = as.data.frame(taxa_tab)
  asv_tab = as.data.frame(asv_tab) # Added November 8, 2019
  
  # Check that the taxa table includes all ASVs: # subsetted ASVs in the taxa table should == # ASVs in the ASV table
  if((dim(taxa_tab[rownames(taxa_tab) %in% names(asv_tab),])[1] == dim(asv_tab)[2]) != TRUE){
    stop("Check your taxa table. All ASVs in the ASV table must be present in the taxa table.")
  }
  
  require("tidyverse")
  
  prep_asv = data.frame("Sample_Names" = rownames(asv_tab), asv_tab)
  prep_taxa = data.frame("ASV" = rownames(taxa_tab), taxa_tab)
  
  asv.tax = prep_asv %>%
    gather("ASV","Raw_Counts",-"Sample_Names") %>%
    full_join(prep_taxa, "ASV")
  
  
  # Convert taxonomic columns from factors to character vectors
  asv.tax[,4:ncol(asv.tax)] = lapply(asv.tax[,4:ncol(asv.tax)], function(o) as.character(o))
  
  # Replace NAs with "Unassigned"
  cleanedNAs = mutate_at(asv.tax, vars(Kingdom:Species), replace_na, replace = "Unassigned")
  
  # Define function that accepts df and returns list of taxonomic-level specific dataframes
  tax_lister = function(df){ 
    
    # Extract taxonomic level names
    tax_names = names(df[,!names(df) %in% c("Sample_Names", "ASV", "Raw_Counts")])
    
    # Define empty list for filling with taxonomic level-specific abundances
    list_new_df = list()
    
    new_df = df[,names(df) %in% c("Sample_Names", "Raw_Counts")]
    
    # Select all columns with taxonomic data
    df_sub = df[,names(df) %in% tax_names] 
    
    # Build list of dataframes containing taxonomic columns 
    for(i in 1:length(tax_names)){
      if(i == 1){
        list_new_df[[i]] = data.frame("Sample_Names" = df$Sample_Names, "Raw_Counts" = df$Raw_Counts, "Taxa" = df_sub$Kingdom)
      }else{
        list_new_df[[i]] = data.frame("Sample_Names" = df$Sample_Names, "Raw_Counts" = df$Raw_Counts, 
                                      "Taxa" = apply(df_sub[,1:i, drop = F], 1, function(z) paste(z, collapse = "; ")))
      }
    }
    
    # Give dataframes names
    names(list_new_df) = tax_names
    
    return(list_new_df)
    
  }
  
  tax_list = tax_lister(cleanedNAs)
  
  # Sum (condense) all taxa that are the same at each taxonomic level
  condenser = lapply(tax_list, function(d){
    d %>%
      group_by(Sample_Names, Taxa) %>%
      summarise("Summed_Raw_Counts" = sum(Raw_Counts)) %>%
      as.data.frame()
  })
  
  #return(condenser)
  
  # Relative Abundance calculation
  if(relAbund == TRUE){
    relabund = lapply(condenser, function(cd){
      cd %>%
        group_by(Sample_Names) %>%
        mutate("RelAbund" = Summed_Raw_Counts/sum(Summed_Raw_Counts)) %>%
        as.data.frame()
    })
    
    return(relabund)
    
  }else{

    return(condenser)
  }
  
}