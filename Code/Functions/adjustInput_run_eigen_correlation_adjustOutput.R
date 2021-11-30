# these function will adjst input and output data from the eigen_correlation()  function
# it helps you correlate experimental metadata of a phyloseq object to module composition of an igraph object



# first, prepare igraph object to be be used as input to eigen_correlation() ; it is necessary to adjust weights and define categories as numerical varaibles
# NOTE: the metadata to be correlated to the modules has been hard-coded inside the function
# second: execute eigen_correlation
# third: merge module sizes with eigen_correlation() output
adjustInput_run_eigen_correlation_adjustOutput<-function(igraph_obj, phyloseq_obj){
  

#turn sample data into dataframe, with numeric variable...
test_ps_meta<-as(sample_data(phyloseq_obj),"data.frame")
test_ps_meta$leaf_dry_weight<-as.numeric(test_ps_meta$leaf_dry_weight)
test_ps_meta$MYC2_at_fold_DD<-as.numeric(test_ps_meta$MYC2_at_fold_DD)
test_ps_meta$hpc<-as.numeric(test_ps_meta$hpc)

#remove weights to calculate modularity
E(igraph_obj)$weight<-1

#calculate modularity
community_data<-cluster_fast_greedy(igraph_obj, weights = NULL)

#calculates correlations between module composition and metadata
biom_eigen<-eigen_correlation( data = as.data.frame(t(otu_table(phyloseq_obj))),
                               community = community_data,
                               metadata = test_ps_meta,
                               categories= c("leaf_dry_weight", "MYC2_at_fold_DD", "hpc"))

# this gives Freq as an integer of number of nodes in module and community.sizes as a factor with the name of the module. merge it with the main object
module_sizes<-as.data.frame(sizes(community_data))%>%
  rename(c("Community.sizes"= "module_number","Freq"="nodes_in_module" ))# we also rename the variables for clarity

# merge module sizes with module correlations to emtadata
eigencorr_result<-merge (module_sizes,  
                         biom_eigen$melt_cor,  
                         by.x = "module_number",  
                         by.y ="variable")

#reorder df according module number 
eigencorr_result<-eigencorr_result[order(eigencorr_result$module_number),]


return(eigencorr_result)
}



