# This function will filter the OTUs of a phyloseq object by minimal frequency in a sample and minimal number of occurences (prevalence) across the samples
# it was Written by Gian Benucci (Michigan state university) by 2019, and then copied by Pedro Costa
# its input is a phyloseq object 
# it's output is a phyloseq with filtered OTUs
# the arguments are phyloseq object, the abundance and frequency of your samples


filterPhyseq <- function(physeq, abund, freq){
  require(phyloseq)
  physeq_ra = transform_sample_counts(physeq, function(x) x/sum(x))
  physeq_ra_filt = filter_taxa(physeq_ra, function(x) sum(x) > abund, TRUE)
  otu_table(physeq) <- otu_table(physeq)[which(
    rownames(otu_table(physeq))%in%rownames(otu_table(physeq_ra_filt))), ]
  physeq <- filter_taxa(physeq, function(x) sum(x >= 1) >= 
                          ncol(otu_table(physeq))/100*freq, TRUE)
  return(physeq)
}

# example: this will filter to OTUs that appear count for at least 1% of the data in a sample, these OTUs must also apear on 50% the samples
#filterPhyseq (physeq, 0.01, 50)  