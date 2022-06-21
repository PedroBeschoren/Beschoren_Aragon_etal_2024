
# this function takes 2 phyloseq objects as 2 arguments:
  # ips_important_taxa =  a subset of imporntant taxa, such as ASVs tagged as important by differential abundance, random forest, and netowrk analysis
  # ps_all_taxa = the full phyloseq objects from where you obtained the imporntat subset (and likely your input for differential abundance, random forest, and netowork analysis)

# Then it runs a fisher test, comparing the proportions of every taxonomic level accuting more than twice in both datasets
# it's just a test to compare proportions - is 6 out of 17 a similar proportion to 193 out of 2243?
# note: using phyloseq::subset_taxa with a for loop will cause issues as your taxa is intepreted as "i"! to avoid this, use phyloseq::prune_taxa instead

fisher_all_taxa_groups<-function(ps_important_taxa, ps_all_taxa){
  
  
  
  
# first, get the taxonomic groups of the taxa defined as important

#this will get us a list of (taxa_level) that appears in the important subset more than once
imp_phylum_l<-as.character(tax_table(ps_important_taxa)[,"Phylum"]) # gets a char vector  of "families" shown as relevant
imp_phylum_l<-imp_phylum_l[imp_phylum_l != "p__uncultured"] # removes any taxonomy set as "uncultured"
imp_phylum_l<-names(which(table(imp_phylum_l)>2))%>% # get names of taxa that occur ate least 1 time NOTE: CHANGED FROM >2
  na.omit()%>% #remove NA from classifications
  unique()%>%  #dereplicates repetitive values (avoids "f__Chitinophagaceae" "f__Chitinophagaceae" "f__Oxalobacteraceae")
  as.list(c()) # save the dereplicated values as a list

# now, perfom the same as above for every taxa level... it's hard-coded, but it works well enough. Pedro tried automating this and gave up after a few hours
imp_class_l<-as.character(tax_table(ps_important_taxa)[,"Class"]) 
imp_class_l<-imp_class_l[imp_class_l != "c__uncultured"] 
imp_class_l<-names(which(table(imp_class_l)>2))%>% 
  na.omit()%>% 
  unique()%>%  
  as.list(c()) 

imp_order_l<-as.character(tax_table(ps_important_taxa)[,"Order"])
imp_order_l<-imp_order_l[imp_order_l != "o__uncultured"]
imp_order_l<-names(which(table(imp_order_l)>2))%>%
  na.omit()%>% 
  unique()%>%
  as.list(c()) 

imp_fam_l<-as.character(tax_table(ps_important_taxa)[,"Family"])
imp_fam_l<-imp_fam_l[imp_fam_l != "f__uncultured"]
imp_fam_l<-names(which(table(imp_fam_l)>2))%>%
  na.omit()%>% 
  unique()%>% 
  as.list(c()) 

imp_genus_l<-as.character(tax_table(ps_important_taxa)[,"Genus"])
imp_genus_l<-imp_genus_l[imp_genus_l != "g__uncultured"]
imp_genus_l<-names(which(table(imp_genus_l)>2))%>%
  na.omit()%>% 
  unique()%>% 
  as.list(c()) 









# second, get the number of taxa occuring in each taxonomic group, within the imporntat taxa subset

#make one empty lists to store results
target_in_important_n<-list()

# obtain the number of reads of the target taxa in the important subset
for(i in imp_phylum_l) { #for every Phylum "i" with representatives defined as important....
  target_in_important_n[i]<-ntaxa(prune_taxa(taxa = tax_table(ps_important_taxa)[,"Phylum"] %in% i, # get only the taxa that belong to that particular phylum "i", then define the number of taxa in there, then save it in the empty list we just made to store the results
                                             x = ps_important_taxa))
}

# now, perfom the same as above for every taxa level... it's hard-coded, but it works well enough. Pedro tried automating this and gave up after a few hours
for(i in imp_class_l) {
  target_in_important_n[i]<-ntaxa(prune_taxa(taxa = tax_table(ps_important_taxa)[,"Class"] %in% i,
                                             x = ps_important_taxa))
}

for(i in imp_order_l) {
  target_in_important_n[i]<-ntaxa(prune_taxa(taxa = tax_table(ps_important_taxa)[,"Order"] %in% i,
                                             x = ps_important_taxa))
}


for(i in imp_fam_l) {
  target_in_important_n[i]<-ntaxa(prune_taxa(taxa = tax_table(ps_important_taxa)[,"Family"] %in% i,
                                                     x = ps_important_taxa))
}

for(i in imp_genus_l) {
  target_in_important_n[i]<-ntaxa(prune_taxa(taxa = tax_table(ps_important_taxa)[,"Genus"] %in% i,
                                             x = ps_important_taxa))
}









# third, get the number of taxa occuring in each taxonomic group, within the full dataset 

#make one empty lists to store results
target_in_all_n<-list()

# obtain the number of reads of the target taxa in the important subset
for(i in imp_phylum_l) { #for every Phylum "i" with representatives defined as important....
  target_in_all_n[i]<-ntaxa(prune_taxa(taxa = tax_table(ps_all_taxa)[,"Phylum"] %in% i, # get only the taxa that belong to that particular phylum "i", then define the number of taxa in there, then save it in the empty list we just made to store the results
                                             x = ps_all_taxa))
}

# now, perfom the same as above for every taxa level... it's hard-coded, but it works well enough. Pedro tried automating this and gave up after a few hours
for(i in imp_class_l) {
  target_in_all_n[i]<-ntaxa(prune_taxa(taxa = tax_table(ps_all_taxa)[,"Class"] %in% i,
                                             x = ps_all_taxa))
}

for(i in imp_order_l) {
  target_in_all_n[i]<-ntaxa(prune_taxa(taxa = tax_table(ps_all_taxa)[,"Order"] %in% i,
                                             x = ps_all_taxa))
}


for(i in imp_fam_l) {
  target_in_all_n[i]<-ntaxa(prune_taxa(taxa = tax_table(ps_all_taxa)[,"Family"] %in% i,
                                             x = ps_all_taxa))
}

for(i in imp_genus_l) {
  target_in_all_n[i]<-ntaxa(prune_taxa(taxa = tax_table(ps_all_taxa)[,"Genus"] %in% i,
                                             x = ps_all_taxa))
}









# now, get the total number of taxa in the imporntat subset and in the full dataset

# all important taxa
all_taxa_in_important_n<-ntaxa(ps_important_taxa)

#all taxa 
all_taxa_in_all_n<-ntaxa(ps_all_taxa)









# now perform fisher tests over lists; check online tutorials for fisher.test() if need

# this contigency table: (summed marginal totals is equal to the total number of taxa the the full object)
fisher_result<-mapply(function (target_in_important_n,target_in_all_n)
  fisher.test(matrix(c(target_in_important_n, 
                       all_taxa_in_important_n - target_in_important_n, 
                       target_in_all_n - target_in_important_n, 
                       all_taxa_in_all_n - all_taxa_in_important_n - target_in_all_n),
                     ncol=2),alternative = "greater" ), 
  target_in_all_n = target_in_all_n,
  target_in_important_n = target_in_important_n,
  SIMPLIFY = FALSE)

return(fisher_result)

}



