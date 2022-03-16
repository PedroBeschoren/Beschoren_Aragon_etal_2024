# This script is not an idependent function, and is fully hard-coded into your global environemnt.
# pedro failed to sufficiently automate this. line 50 is the major proble: taxa were being intepreted as "i" when inside a custom function or lapply
# this code will take 2 phyloseq objects (one with important taxa, the other with all taxa) and run a fisher test on the occurences of such taxa
# it's just a test to compare proportions - is 6 out of 17 a smilar proportion to 193 out of 2243?




#  @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# working hard-coded version ##########
#  @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@




#this will get us a list of (taxa_level) that appears in the important subset more than once

imp_phylum_l<-as.character(tax_table(ps_important_taxa)[,"Phylum"]) # gets a char vector  of "families" shown as relevant
imp_phylum_l<-imp_phylum_l[imp_phylum_l != "p__uncultured"] # removes any taxonomy set as "uncultured"
imp_phylum_l<-names(which(table(imp_phylum_l)>2))%>% # get names of taxa that occur ate least 1 time NOTE: CHANGED FROM >2
  na.omit()%>% #remove NA from classifications
  unique()%>%  #dereplicates repetitive values (avoids "f__Chitinophagaceae" "f__Chitinophagaceae" "f__Oxalobacteraceae")
  as.list(c()) # save the dereplicated values as a list


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


#make 2 empty lists
target_in_important_n<-list()
target_in_all_n<-list()
# obtain the number of reads of the target taxa in the important subset

for(i in imp_phylum_l) {
  target_in_important_n[i]<-ntaxa(subset_taxa(ps_important_taxa, Phylum == i))
}

for(i in imp_class_l) {
  target_in_important_n[i]<-ntaxa(subset_taxa(ps_important_taxa, Class == i))
}

for(i in imp_order_l) {
  target_in_important_n[i]<-ntaxa(subset_taxa(ps_important_taxa, Order == i))
}

for(i in imp_fam_l) {
  target_in_important_n[i]<-ntaxa(subset_taxa(ps_important_taxa, Family == i))
}

for(i in imp_genus_l) {
  target_in_important_n[i]<-ntaxa(subset_taxa(ps_important_taxa, Genus == i))
}




# obtain the number of reads of the target taxa in the full data
for(i in imp_phylum_l) {
  target_in_all_n[i]<-ntaxa(subset_taxa(ps_all_taxa, Phylum == i))
}

for(i in imp_class_l) {
  target_in_all_n[i]<-ntaxa(subset_taxa(ps_all_taxa, Class == i))
}

for(i in imp_order_l) {
  target_in_all_n[i]<-ntaxa(subset_taxa(ps_all_taxa, Order == i))
}

for(i in imp_fam_l) {
  target_in_all_n[i]<-ntaxa(subset_taxa(ps_all_taxa, Family == i))
}

for(i in imp_genus_l) {
  target_in_all_n[i]<-ntaxa(subset_taxa(ps_all_taxa, Genus == i))
}



# all important taxa
all_taxa_in_important_n<-ntaxa(ps_important_taxa)

#all taxa 
all_taxa_in_all_n<-ntaxa(ps_all_taxa)


#now fisher over lists
# this contigency table would be correct (summed marginal totals is equal to the total number of taxa the the full object)
# however we violate the assumption of independence  between cells, as the value of one cell literally depends on the other value
# thus we cannot use the p value of these fisher tests, but might be able to use counts and oods ratio
fisher_result<-mapply(function (target_in_important_n,target_in_all_n)
  fisher.test(matrix(c(target_in_important_n, 
                       all_taxa_in_important_n - target_in_important_n, 
                       target_in_all_n - target_in_important_n, 
                       all_taxa_in_all_n - all_taxa_in_important_n - target_in_all_n),
                     ncol=2),alternative = "greater" ), 
  target_in_all_n = target_in_all_n,
  target_in_important_n = target_in_important_n,
  SIMPLIFY = FALSE)






