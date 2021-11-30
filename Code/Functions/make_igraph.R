make_igraph<-function(spiec_obj, physeq_obj){
  # this function makes a weighted igraph object from the spiec_easi object, including the taxa names from a phyloseq object
  # obth spiec_easi and phyloseq objects should ahve the same number of samples 
  
  # creating OTU names list ----------------------------------
  names_spiec_obj <- taxa_names(physeq_obj) 
  # adding weights to the graph ---------------------------------
  # sebeta <- symBeta(getOptBeta(spiec_obj), mode='maxabs') # unsilence this to use mb instead of glasso
  secor  <- cov2cor(getOptCov(spiec_obj)) # silence this if using mb instead of glasso
  elist.gl <- summary(Matrix::triu(secor*getRefit(spiec_obj), k=1)) # for glasso
  network <- adj2igraph(Matrix::drop0(getRefit(spiec_obj)),
                        vertex.attr = list(name=names_spiec_obj),
                        edge.attr=list(weight=summary(Matrix::triu(t(secor)*getRefit(spiec_obj), k=1))[,3] ), # use this for glasso
                        # edge.attr=list(weight=Matrix::summary(t(sebeta))[,3] ), # use this for mb
                        rmEmptyNodes = FALSE) # now we can add weights to the expoted write.graph()
  
# adds positive/negative as an edge attribute
edge_attr(network,"positive_negative")<-ifelse(E(network)$weight>0,"positive", "negative")

# adds edge betweenness
edge_attr(network,"weighted_edge_betweenness")<- edge_betweenness(network,  
                                                                  weights=sqrt(E(network)$weight*E(network)$weight), # weighted edge betweenness requires positive weights 
                                                                  directed = FALSE)# needs positive weights

  
  return(network)
} 
