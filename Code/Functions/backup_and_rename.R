# The essential part of this fucntion was copied  from https://rdrr.io/github/cpauvert/psadd/man/rename_otu.html by Pedro Costa in Jul/2021

# this function loads the rename_otus function , makes a backup for dada2 IDs, and then givens new ASV names based on taxa abundance
# then it changes the names of the taxonomy from rank1, rank2 to phylum, class, etc
# it's input is a phyloseq object with taxa names such as 7f09498fb455a333197f691f08b55bc7
# it's output is a phyloseq object with taxa names such as ASV_1, ASV_2, ASV_3, according the ASV abundance
backup_and_rename<-function(phyloseq_object){ 
  
  source("./Code/Functions/rename_otus.R") # loads rename_otu fucntion from https://rdrr.io/github/cpauvert/psadd/man/rename_otu.html
  tax_table(phyloseq_object)<- cbind(tax_table(phyloseq_object), "DADA2_ID"= row.names(tax_table(physeq))) #makes a backup of the dada2 IDs
  taxa_names(phyloseq_object)<-taxa_names(rename_otu(phyloseq_object, ""))
  colnames(tax_table(phyloseq_object)) = c("Domain", "Phylum", "Class", "Order", "Family", "Genus", "Species", "DADA2_ID") # this renames the taxonomy from Rank1, Rank2 to phylum, class etc
  
  return(phyloseq_object)}