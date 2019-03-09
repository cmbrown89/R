generate.tax.summary.cb = function(asv_tab, taxa_tab){
  # Make sure that the taxa table includes all ASVs, or else stop 
  # The number of subsetted ASVs in the taxa table should match the number of ASVs in the ASV table
  if((dim(taxa_tab[rownames(taxa_tab) %in% names(asv_tab),])[1] == dim(asv_tab)[2]) != TRUE){
    stop("Check your taxa table. All ASVs must be present in the taxa table.")
  }
  
  # Apply taxa names to ASVs
  asv_taxa = taxa_tab[rownames(taxa_tab) %in% names(asv_tab),]
  
  if(identical(rownames(asv_taxa), names(asv_tab)) == TRUE){
    
    # Concatenate all taxonomic levels
    asv_taxa_cat = do.call(paste, asv_taxa)
    
    # Remove NAs and replace spaces with ";"s
    clean = function(x){
      v = gsub(" NA", "", x)
      c = gsub(" ", ";", v)
      c
    }
    
    cleaned_asv_taxa_cat = clean(asv_taxa_cat)
  } 
  
  
  t_asv_tab = as.data.frame(t(asv_tab))
  
  if(nrow(t_asv_tab) == length(cleaned_asv_taxa_cat)){
    t_asv_tab$Taxa = cleaned_asv_taxa_cat
    
    collasped = t_final_young_asvs.df %>%
      group_by(Taxa) %>%
      summarise_all(sum) %>%
      as.data.frame()
    
    rownames(collasped) = collasped$Taxa
    
    collasped_normalized = scale(collasped[,2:ncol(collasped)], center = FALSE, 
                                 scale = apply(collasped[,2:ncol(collasped)], 2, sum)) %>%
      as.data.frame() 
  }
  
}

