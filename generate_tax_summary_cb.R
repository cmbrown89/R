#To do:
### Add sort step before line 24's check


generate.tax.summary.cb = function(asv_tab, taxa_tab){
  # Purpose: Take ASV table and taxonomic information from dada2 and produce a dataframe with the relative abundances of different taxa
  
  # Check that the taxa table includes all ASVs: # subsetted ASVs in the taxa table should == # ASVs in the ASV table
  if((dim(taxa_tab[rownames(taxa_tab) %in% names(asv_tab),])[1] == dim(asv_tab)[2]) != TRUE){
    stop("Check your taxa table. All ASVs in the ASV table must be present in the taxa table.")
  }
  
  rarefac_check = sapply(rowSums(asv_tab), function(c) identical(c[1], c))
  if(length(rarefac_check[rarefac_check == F]) != 0){
    warning("FYI: Your data is not rarefied.")
  }
  
  require("tidyverse")
  

  # Apply taxa names to ASVs
  asv_taxa = taxa_tab[rownames(taxa_tab) %in% names(asv_tab),]
  
  if(identical(rownames(asv_taxa), names(asv_tab)) == FALSE){
    stop("ASVs in taxa table do not coorespond to ASVs in ASV table.")
  }
    
  # Concatenate all taxonomic levels
  asv_taxa_cat = do.call(paste, asv_taxa)
  
  # Remove NAs and replace spaces with ";"s
  clean = function(x){
    v = gsub(" NA", "", x)
    c = gsub(" ", ";", v)
    c
  }
  
  cleaned_asv_taxa_cat = clean(asv_taxa_cat)
  
  t_asv_tab = as.data.frame(t(asv_tab))
  
  if(nrow(t_asv_tab) != length(cleaned_asv_taxa_cat)){
    stop("Hm, something went wrong.")
  }
  
  t_asv_tab$Taxa = cleaned_asv_taxa_cat
  
  collasped = t_asv_tab %>%
    group_by(Taxa) %>%
    summarise_all(sum) %>%
    as.data.frame()
  
  rownames(collasped) = collasped$Taxa
  
  collasped_normalized = scale(collasped[,2:ncol(collasped)], center = FALSE, 
                               scale = apply(collasped[,2:ncol(collasped)], 2, sum)) %>%
    as.data.frame()
  
  
  if(sum(collasped_normalized) != length(collasped_normalized)){
    warning("Uh oh, the relative abundances of all samples do not add to 100%.")
  }
  
  unassigned = function(df){
    counts = 6-str_count(rownames(df), ";") 
    completes = paste0(rownames(df), str_dup(";Unassigned", counts))
    rownames(df) = completes
    df
  }
  
  unassigned(collasped_normalized)

}

