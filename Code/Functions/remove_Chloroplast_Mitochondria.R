# This function was written by Pedro Costa in Jul/2021
# it will remove identified o__Chloroplast and f__Mitochondria taxa from the dataset
# it's input in'ts a phyloseq object with these plant sequences
# the output will not contain these platn sequences
# there are many options and plots you could make to check how much plant DNA you ahve amplified - something we won't explore ehre


##### Chloroplast and mitochondrial DNA removal ##########
remove_Chloroplast_Mitochondria<- function(physeq_object){

#Removes Chloroplast
#ATTENTION: if you just do >subset_taxa(physeq_object, Rank4!="o__Chloroplast") ; you will also remove NAs in the identification. thus you have to turn the ASV id into a factor before removing them, check https://hypocolypse.github.io/16s-data-prep.html

# generate a df with Chloroplast ASVs
CH1 <- subset_taxa(physeq_object, Order == "o__Chloroplast") # get all Chloroplasts...
CH1 <-  as(tax_table(CH1), "matrix")
CH1 <- row.names(CH1) # get ASV IDs...
CH1df <- as.factor(CH1) # set IDs as factors
goodTaxa <- setdiff(taxa_names(physeq_object), CH1df) #define taxa you should keep
ps_no_chloro <- prune_taxa(goodTaxa, physeq_object) # your new physeq object is now chloroplast-free, but retains NA in identification



#Removes Mitochondria
#ATTENTION: if you just do >subset_taxa(physeq_chloro_mito, Rank5!="f__Mitochondria") ; you will also remove NAs in the identification. thus you have to turn the ASV id into a factor before removing them, check https://hypocolypse.github.io/16s-data-prep.html
MT1 <- subset_taxa(physeq_object, Family == "f__Mitochondria")
MT1 <-  as(tax_table(MT1), "matrix")
MT1 <- row.names(MT1)
MT1df <- as.factor(MT1)
goodTaxa <- setdiff(taxa_names(physeq_object), MT1df)
ps_no_chloro_mito <- prune_taxa(goodTaxa, ps_no_chloro)

#excellent! let's save this a new phyloseq object
physeq_clean<-ps_no_chloro_mito
return(physeq_clean)

}