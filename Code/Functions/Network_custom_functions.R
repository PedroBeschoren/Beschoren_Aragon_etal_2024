

#**********************************************#
#################### filterPhyseq #############
#**********************************************#

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

# ********************************************** Done! #









#**********************************************#
#################### make_igraph ###############
#**********************************************#

# this function makes a weighted igraph object from the spiec_easi object, including the taxa names from a phyloseq object
# it's input is a phyloseq object and a spiec.easi objected generated from SpiecEasi::spiec.easi() 
# it's output is an igraph object
# obth spiec_easi and phyloseq objects should ahve the same number of samples 
# the function also clasiffy covariance edges as positive or negative according to weight
# finally, it calculates weighted edge betweness centrality (negative weights were transformed to positive)

make_igraph<-function(spiec_obj, physeq_obj){

  
  # creating OTU names list
  names_spiec_obj <- taxa_names(physeq_obj) 
  # adding weights to the graph
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

# ********************************************** Done! #








#**********************************************#
######## Generate.metrics.randomNet ############
#**********************************************#

# This function generates the metrics that are relevant for comparisons of a real network with a random network with the same number of nodes and edges
#it's input is an igraph object
# it's output is essential to the Real_VS_random_networks, where real netwroks are compared to the random networks geenrated here

Generate.metrics.randomNet<-function (igraph_object){
  
  # now, let's put the centrality values all in the same place
  # let's calculate the centrality metrics in a function
  # get node's degree, closeness centrality, and ebtweness centrality. note: negative weights not allowed, and disconnected modules not allowed
  
  # remove negative weights to calculate modularity and mean edge/node betweness
  igraph_posWeight<-igraph_object # create new igraph obejct to calculate cloness and betweness from it
  E(igraph_posWeight)$weight<-sqrt(E(igraph_posWeight)$weight*E(igraph_posWeight)$weight) # pwoer and square root the weights to destroy the signal. negative weights cannot be used to calculate node closeness and centrality
  
  igraph_NullWeight<-igraph_object # create new igraph obejct to calculate modularity from it
  E(igraph_NullWeight)$weight<-1 # all modularity calculations are based on null weights due to a bu on the ZiPi funtion
  
  # remove unconected components from the main network, so closeness centrality can be calculated
  components<-components(igraph_object) #obtain network components that are unconnected the the main graph 
  Main_component = which.max(components$csize) #define the largest compenent
  igraph_posWeight_single_component <- induced_subgraph(igraph_posWeight, which(components$membership == Main_component)) #makes a subgraph, removing unconected compenets
  igraphsingle_component <- induced_subgraph(igraph_object, which(components$membership == Main_component)) #makes a subgraph, removing unconected compenets
  
  # note that the changes above were only to calculate these metrics, and do not change the network
  # WARNING: if you have multiple large components in your network, you will have to make closeness centrality calculations for each component
  
  # now we can calculate the network metrics, focused on centrality metrics. these should be compared agasint a random entwork
  
  Centralized_betweenness<-centralization.betweenness(igraph_object)$centralization
  
  Centralized_closeness<-centralization.closeness(igraphsingle_component)$centralization #negatie edge weights won't affect this
  
  #eigenvector centrality: relevance in the network comes from being connected to relevant elements of the netwrok
  Centralized_eigenvector<-centralization.evcent(igraph_object)$centralization 
  
  # transtivity (also called clustering coeficient)is the ratio of "triangles" in the network ; by default it's a global metric considering weights
  Network_transitivity<-transitivity(igraph_object)
  
  # calculate shortest path (geodesic) distance
  Network_mean_shortest_path<-mean_distance(igraph_posWeight)
  
  # calculates modules 
  cluster_fast_greedy(igraph_NullWeight) # cannot use negative weights, but assigns modules
  
  #calculates modularity
  Network_modularity<-modularity(igraph_NullWeight,
                                 cluster_fast_greedy(igraph_NullWeight)$membership,
                                 weights = NULL) # needs positive weights
  # mean node betweeness
  Mean_node_betweenness<-mean(betweenness(igraph_posWeight,  
                                          weights=E(igraph_posWeight)$weight,
                                          directed = FALSE))# needs positive weights
  
  # mean edge betweeness
  Mean_edge_betweenness<-mean(edge_betweenness(igraph_posWeight,  
                                               weights=E(igraph_posWeight)$weight,
                                               directed = FALSE))# needs positive weights
  
  
  # metrics_dataframe
  network_metrics<-data.frame(
    Centralized_betweenness,
    Centralized_closeness,
    Centralized_eigenvector,
    Network_transitivity,
    Network_mean_shortest_path,
    Network_modularity,
    Mean_node_betweenness,
    Mean_edge_betweenness)
  
  return(network_metrics)
}

# ********************************************** Done! #


















#**********************************************#
########### Real_VS_random_networks ############
#**********************************************#

# this fucntion will compare the real and 100 random networks, returning metrics that are different form random as TRUE
# it takes an igraph object as an input
# it's output is a dataframe showing the metrics for the real network, and  mean, max and lower metrics for 100 random networks of identical number of nodes and edges
# The output also tells if the real netowrk metric differs from the random network metrics
# note that it calls the custom function Generate.metrics.randomNet() defined above
Real_VS_random_networks<- function(igraph_object){
  
  set.seed(101)
  
  # calculate 100 random netowrks and their metrics
  rand_list<-replicate(1000,Generate.metrics.randomNet(rewire(igraph_object,each_edge(1))))
  
  #put 100 random networks in a  dataframe
  rand_df<-data.frame(matrix(unlist(rand_list), nrow=1000, byrow=TRUE, dimnames = list(1:1000,colnames(rand_list))),check.names = FALSE)
  
  # get means and SD of random networks
  rand_mean<-sapply(rand_df, mean)
  rand_sd<-sapply(rand_df, sd)
  rand_lower<-rand_mean-2*rand_sd
  rand_higher<-rand_mean+2*rand_sd
  
  #get teh real network metrics
  real_metrics<-Generate.metrics.randomNet(igraph_object)
  
  #put real and random netowrk metrics together
  Real_VS_Rand<-rbind(real_metrics, rand_lower,rand_higher,rand_mean)
  rownames(Real_VS_Rand) <- c("Real_network", "Random_lower", "Random_higher", "Random_mean")
  
  # are the real values different from random values?
  Diff_from_rand_logical<-sapply(Real_VS_Rand, function (x) x[1] < x[2] | x[1] > x[3]) # x = network metric, must be lowe OR higher than random eman -/+ 2SD
  
  # the output will be a list, with the values on the first element and it the difference is significant on the second element
  names<-c("Real_VS_random_netowrk_metrics", "Is_real_different_from_random?")
  output<-list(Real_VS_Rand,Diff_from_rand_logical)
  names(output)<-names
  return (output)
}

# ********************************************** Done! #


















#**********************************************#
####### Generate_RealNetworks_metrics ##########
#**********************************************#

# this function will generate multiple network metrics
# it takes an igraph object and returns a dataframe with a single row per network

Generate_RealNetworks_metrics<-function (igraph_object){
  # get node's degree, closeness centrality, and ebtweness centrality. note: negative weights not allowed, and disconnected modules not allowed
  
  # remove negative weights to calculate modularity and mean edge/node betweness
  igraph_posWeight<-igraph_object # create new igraph obejct to calculate cloness and betweness from it
  E(igraph_posWeight)$weight<-sqrt(E(igraph_posWeight)$weight*E(igraph_posWeight)$weight) # pwoer and square root the weights to destroy the signal. negative weights cannot be used to calculate node closeness and centrality
  
  igraph_NullWeight<-igraph_object # create new igraph obejct to calculate modularity from it
  E(igraph_NullWeight)$weight<-1 # all modularity calculations are based on null weights due to a bu on the ZiPi funtion
  
  
  
  # remove unconected components from the main network, so closeness centrality can be calculated
  components<-components(igraph_object) #obtain network components that are unconnected the the main graph 
  Main_component = which.max(components$csize) #define the largest compenent
  igraph_posWeight_single_component <- induced_subgraph(igraph_posWeight, which(components$membership == Main_component)) #makes a subgraph, removing unconected compenets
  igraphsingle_component <- induced_subgraph(igraph_object, which(components$membership == Main_component)) #makes a subgraph, removing unconected compenets
  
  # note that the changes above were only to calculate these metrics, and do not change the network
  # WARNING: if you have multiple large components in your network, you will have to make closeness centrality calculations for each component
  
  # p value of the test to fir the degree to a power law. if sugnificant, it does not follow a power law (but this is a simplification!)
  fit_to_power_law<-fit_power_law(degree(igraph_object))[[6]]
  
  #total number of ndoes and edges
  Edges_total<-gsize(igraph_object)
  Nodes_total<-length(degree(igraph_object))
  
  #total number of ndoes and edges in the main component
  main_C_nodes<-length(degree(igraph_posWeight_single_component))
  main_C_edges<-gsize(igraph_posWeight_single_component)
  main_C_Mdegree<-main_C_edges/main_C_nodes
  
  
  
  # now we can calculate the network metrics, focused on centrality metrics. these should be compared agasint a random entwork
  
  Centralized_betweenness<-centralization.betweenness(igraph_object)$centralization
  
  Centralized_closeness<-centralization.closeness(igraphsingle_component)$centralization #negatie edge weights won't affect this
  
  #this will allways be identical to a random entwork with the same number of nodes and edges
  Centralized_degree<-centralization.degree(igraph_object)$centralization # needs connected graphs this will be identical to random network because it is degree-preserving
  
  #eigenvector centrality: relevance in the network comes from being connected to relevant elements of the netwrok
  Centralized_eigenvector<-centralization.evcent(igraph_object)$centralization 
  
  # transtivity (also called clustering coeficient)is the ratio of "triangles" in the network ; by default it's a global metric considering weights
  Network_transitivity<-transitivity(igraph_object)
  
  # calculate shortest path (geodesic) distance
  Network_mean_shortest_path<-mean_distance(igraph_posWeight)
  
  #calculates modularity
  Network_modularity<-modularity(igraph_NullWeight,
                                 cluster_fast_greedy(igraph_NullWeight)$membership,
                                 weights = NULL) # needs positive weights
  # mean & max node betweeness
  Mean_node_betweenness<-mean(betweenness(igraph_posWeight,  
                                          weights=E(igraph_posWeight)$weight,
                                          directed = FALSE))# needs positive weights
  Max_node_betweenness<-max(betweenness(igraph_posWeight,  
                                        weights=E(igraph_posWeight)$weight,
                                        directed = FALSE))# needs positive weights
  
  # mean & max edge betweeness
  Mean_edge_betweenness<-mean(edge_betweenness(igraph_posWeight,  
                                               weights=E(igraph_posWeight)$weight,
                                               directed = FALSE))# needs positive weights
  Max_edge_betweenness<-max(edge_betweenness(igraph_posWeight,  
                                             weights=E(igraph_posWeight)$weight,
                                             directed = FALSE))# needs positive weights
  
  #mean & max degree
  Mean_degree<-mean(degree(igraph_object))
  Max_degree<-max(degree(igraph_object))
  
  #positive & negative weight edges
  N_positive_edges<-length(which(E(igraph_object)$weight>0))
  N_negative_edges<-length(which(E(igraph_object)$weight<0))
  Positive_to_negative_ratio<-N_positive_edges/N_negative_edges
  
  # number of modules, ignoring weights
  module_data<-cluster_fast_greedy(igraph_posWeight, weights = NULL)
  
  # number of modules
  N_modules<-length(module_data)
  Median_module_size<-median(sizes(module_data))
  Max_module_size<-max(sizes(module_data))
  
  
  
  # metrics_dataframe
  network_metrics<-data.frame(
    Nodes_total,
    Edges_total,
    main_C_nodes,
    main_C_edges,
    main_C_Mdegree,
    fit_to_power_law,
    Centralized_betweenness,
    Centralized_closeness,
    Centralized_eigenvector,
    Network_transitivity,
    Network_mean_shortest_path,
    Network_modularity,
    Mean_node_betweenness,
    Max_node_betweenness,
    Mean_edge_betweenness,
    Max_edge_betweenness,
    Mean_degree,
    Max_degree,
    N_positive_edges,
    N_negative_edges,
    Positive_to_negative_ratio,
    N_modules,
    Median_module_size,
    Max_module_size
  )
  
  return(network_metrics)
}

# ********************************************** Done! #

















#**********************************************#
########## Generate_node_metrics2 ##############
#**********************************************#

# this function was wirtten by pedro Beschonre da Costa in Ago/2020, based on the script from Gian Benucci 2020
# it takes a spiec_easi object, a igaph object and the phyloseq objects used to generate the networks (all fungal and bacterial samples from the experiment)
# it calculates network metrics used to define keystones (degree, centrality, closeness), then
# it formats the data into the right shape for the KeystoneDetector2() 
# the output is a dataframe with node proprieties

Generate_node_metrics2<-function(igraph_obj, physeq_obj){
  
  # remove negative weights to calculate modularity and mean edge/node betweness
  igraph_posWeight<-igraph_obj # create new igraph obejct to calculate cloness and betweness from it
  E(igraph_posWeight)$weight<-sqrt(E(igraph_posWeight)$weight*E(igraph_posWeight)$weight) # pwoer and square root the weights to destroy the signal. negative weights cannot be used to calculate node closeness and centrality
  
  # remove unconected components from the main network, so closeness centrality can be calculated
  components<-components(igraph_obj) #obtain network components that are unconnected the the main graph 
  Main_component = which.max(components$csize) #define the largest compenent
  igraph_posWeight_single_component <- induced_subgraph(igraph_posWeight, which(components$membership == Main_component)) #makes a subgraph, removing unconected compenets
  igraphsingle_component <- induced_subgraph(igraph_obj, which(components$membership == Main_component)) #makes a subgraph, removing unconected compenets
  
  # now we can calculate the network metrics
  spiec.deg <- degree(igraph_obj)
  spiec.close <- closeness(igraph_posWeight_single_component)
  spiec.bet <- betweenness(igraph_posWeight, normalized = TRUE)
  spiec.eigen_centrality<-eigen_centrality(igraph_obj)$vector
  
  
  # his puts all node metrics into a single dataframe
  nodes<-merge(spiec.deg,spiec.close, by=0)%>% # merges vectors..
    column_to_rownames(var="Row.names")
  colnames(nodes)[1] <- "Degree" #renames columns...
  colnames(nodes)[2] <- "ClosenessCentrality"
  nodes<-merge(nodes,spiec.bet, by=0)%>%
    column_to_rownames(var="Row.names")
  colnames(nodes)[3] <- "BetwenessCentrality"
  nodes<-merge(nodes,spiec.eigen_centrality, by=0)%>%
    column_to_rownames(var="Row.names")
  colnames(nodes)[4] <- "EigenvectorCentrality"
  nodes$OTU<-rownames(nodes) # add rownames as a column...
  OTU_match<-bind_rows(as.data.frame(tax_table(physeq_obj))) # gets taxonomy into a single dataframe to match the nodes
  nodes<-merge(nodes, OTU_match, by=0)%>%
    column_to_rownames(var="Row.names") # adds taxonomy information from phyloseq obeject into the node propriety dataframe
  
  nodes # key output for the function
  return(nodes)
  
}

# ********************************************** Done! #














#**********************************************#
####################### ZiPi ###################
#**********************************************#

#Zi and Pi metrics
# the ZiPi function is from the microbiome package, but loading and installation of the package might be slightly troublesome
# it copied from https://rdrr.io/github/jtclaypool/microbiome/src/R/zipi.R

ZiPi<- function (netw = "network", modules = "modules") {
  names = V(netw)$name
  total_connections = c()
  module_connections = c()
  number_of_modules = c()
  meanZ = c()
  sdZ = c()
  Zi = c()
  Pi = c()
  for (x in 1:length(names)) {
    total_connections[x] = sum(netw[x] > 0)
    module_connections[x] = sum(netw[x][which(modules == 
                                                modules[x])] > 0)
    KitKi = c()
    for (j in 1:length(unique(modules))) {
      KitKi[j] = ((sum(netw[x][which(modules == j)]))/total_connections[x])^2
    }
    Pi[x] = 1 - sum(KitKi)
  }
  for (x in 1:length(unique(modules))) {
    meanZ[x] = mean(module_connections[which(modules == 
                                               x)])
    sdZ[x] = sd(module_connections[which(modules == x)])
  }
  for (x in 1:length(names)) {
    Zi[x] = (module_connections[x] - meanZ[modules[x]])/sdZ[modules[x]]
  }
  return(cbind.data.frame(names, module = modules, module_connections, 
                          total_connections, Zi, Pi))
}

# ********************************************** Done! #












#**********************************************#
#################### Zi_Pi_list ################
#**********************************************#

# this function will take ~40 sec to run on a network with 600 nodes and 1000 edges, mostly beause of Zi and Pi calculations
# it's input is a igraph object
# it will return a list, with Zi/Pi values for nodes as well as a count of module hubs (Zi>2.5) and connectors (Pi>0.62)

Zi_Pi_list<-function (igraph_object) {
  
    # remove weights
  igraph_posWeight<-igraph_object # create new igraph obejct to calculate cloness and betweness from it
  E(igraph_posWeight)$weight<-1 # make all weights =1 (that is, remove weights) as it skews the ZiPi function
  

  #define modules
  community_data<-cluster_fast_greedy(igraph_posWeight, weights = NULL) # using weights completely skews the Pi metric
  module_data<-community_data$membership
  
  
 # calls ZiPi function defined above
  Zipi_calculated<-ZiPi(igraph_posWeight, modules = module_data)
  Module_hubs<-length(which(Zipi_calculated$Zi>=2.5))
  Module_connectors<-length(which(Zipi_calculated$Pi>=0.62))
  
  network_metrics<-data.frame(Module_hubs, Module_connectors)
  return(list(Zipi_calculated,network_metrics))
}

# ********************************************** Done! #









#**********************************************#
############# KeystoneDetector3 ################
#**********************************************#

# This function detect keystone ataxa based on degree, closeness centrality and betweeness centrality
# instead of picking a top%, the function considers a Z distribution of the metrics, and then pinpoints otus with significantly higher metrics
# it requires a dataframe with node metrics and the 95% StrainMatch taxonomy ID
# the output is list with a chart indicating keystones and a dataframe indicating the same kesytones

KeystoneDetector3<-function(nodes_stats){
  mean_close <- mean(log10(nodes_stats$ClosenessCentrality)) # removes modules omposed only of a pair of nodes
  sd_close <- sd(log10(nodes_stats$ClosenessCentrality))
  hubline_close <- (mean_close + 1.645*sd_close) # cutoff at 2 * SD = 1.65 single tail 
  
  z_score_close = (hubline_close - mean_close)/sd_close
  pnorm(z_score_close) # line is above 95 % - equal to p = 0.05
  
  
  mean_degree <- mean(log10(nodes_stats$Degree))
  sd_degree <- sd(log10(nodes_stats$Degree))
  hubline_degree <- (mean_degree + 1.645*sd_degree)
  
  z_score_degree = (hubline_degree - mean_degree)/sd_degree
  pnorm(z_score_degree) # line is above 95 % - equal to p = 0.05
  
  
  mean_between <- mean(log10(nodes_stats$BetwenessCentrality[nodes_stats$BetwenessCentrality > 0]))
  sd_between <- sd(log10(nodes_stats$BetwenessCentrality[nodes_stats$BetwenessCentrality > 0]))
  hubline_between <- (mean_between + 1.645*sd_between)
  
  z_score_between = (hubline_between - mean_between)/sd_between
  pnorm(z_score_between) # line is above 90 % - equal to p = 0.1
  
  plot_closeness <- ggplot() +
    geom_point(data = nodes_stats, aes(x = ClosenessCentrality, y = Degree), alpha = 0.6) +
    scale_size_continuous(name = "Nodule number") +
    theme_bw() +
    geom_text_repel(data = subset(nodes_stats, ClosenessCentrality > 10^hubline_close & Degree > 10^hubline_degree), 
                    aes(x = ClosenessCentrality, y = Degree, label = OTU)) +
    xlab("Closeness Centrality") +
    ylab("Degree") +
    geom_vline(xintercept = 10^hubline_close, linetype = "dashed", colour = "red") +
    geom_hline(yintercept = 10^hubline_degree, linetype = "dashed", colour = "red")
  
  plot_closeness
  
  plot_betweenness <- ggplot() +
    geom_point(data = nodes_stats, aes(x = BetwenessCentrality, y = Degree), alpha = 0.6) +
    scale_size_continuous(name = "Nodule number") +
    theme_bw() +
    geom_text_repel(data = subset(nodes_stats, BetwenessCentrality > 10^hubline_between & Degree > 10^hubline_degree), 
                    aes(x = BetwenessCentrality, y = Degree, label = OTU)) +
    xlab("Betweeness Centrality") +
    ylab("Degree") +
    geom_vline(xintercept = 10^hubline_between,linetype = "dashed", colour = "red") +
    geom_hline(yintercept = 10^hubline_degree,linetype = "dashed", colour = "red")
  
  plot_betweenness
  
  library("ggpubr")
  dual_plot<-ggarrange(plot_closeness, plot_betweenness,
                       labels = c("A", "B"),
                       widths = c(1,1),
                       align = "h", ncol = 2, nrow = 1,
                       common.legend = TRUE,
                       legend = "bottom")
  
  
  #make new variable assining OTUs as keystone before removing all other OTUS. assigning this now avoids a bug when there are no keystones in the network
  nodes_stats$keystone_taxa<-"Keystone"
  
  # OTUs are tagged as keystones if they have "high" degree and betweness centrality OR "high" degree and closeness centrality
  keystones<-subset(nodes_stats, Degree > 10^hubline_degree & BetwenessCentrality > 10^hubline_between |  Degree > 10^hubline_degree & ClosenessCentrality > 10^hubline_close )
  output <- list(dual_plot, keystones) 
  
  return(output)
} 

# ********************************************** Done! #











#**********************************************#
############# eigen_correlation ################
#**********************************************#

# this function calculates correlations between netowkr modules and metadata
# it was copied from https://rdrr.io/github/jtclaypool/microbiome/src/R/eigenvector_correlation.R
eigen_correlation<-function(data="relative abundance",community="community detection",
                            metadata="metadata file",categories="environments to compare"){
  require(WGCNA)
  require(reshape)
  data.netw=data[,which(names(data)%in%community$names)]
  community_ord=community$membership[match(colnames(data.netw),community$names)]
  ME=moduleEigengenes(data.netw,colors=community$membership)
  corr=data.frame(matrix(0,ncol=length(unique(community_ord)),nrow = length(categories)))
  names(corr)<-sort(unique(community$membership),decreasing = F)
  rownames(corr)<-categories
  pval=corr
  for(i in 1:length(categories)){
    for(j in 1:length(unique(community_ord))){
      test=cor.test(metadata[,which(names(metadata)%in%categories[i])],ME$eigengenes[,j])
      corr[i,j]=test$estimate
      pval[i,j]=test$p.value
    }
  }
  newcorr=melt(corr)
  newpval=melt(pval)
  dat=cbind.data.frame(newcorr,"pval"=newpval$value,"category"=rep(rownames(corr),nrow(newcorr)/length(categories)))
  return(list("corr"=corr,"pval"=pval,"melt_cor"=dat))
}

# ********************************************** Done! #














#**********************************************#
############# adjustInput_run_eigen_correlation_adjustOutput ################
#**********************************************#

# these function will adjust input and output data from the eigen_correlation()  function
# it helps you correlate experimental metadata of a phyloseq object to module composition of an igraph object
# first, prepare igraph object to be be used as input to eigen_correlation() ; it is necessary to adjust weights and define categories as numerical varaibles
#NOTE: the names of the metadata variables are hard-coded within this function! If you have other metadata you have to change it by hand inside the function like a barbarian. uga-bunga!
# second: execute eigen_correlation
# third: merge module sizes with eigen_correlation() output
adjustInput_run_eigen_correlation_adjustOutput<-function(igraph_obj, phyloseq_obj){
  
  
  #turn sample data into dataframe, with numeric variable...
  test_ps_meta<-as(sample_data(phyloseq_obj),"data.frame")
  test_ps_meta$leaf_dry_weight<-as.numeric(test_ps_meta$leaf_dry_weight)
  test_ps_meta$MYC2_at_fold_DD<-as.numeric(test_ps_meta$MYC2_at_fold_DD)
  test_ps_meta$lox2_at_fold_DD <-as.numeric(test_ps_meta$lox2_at_fold_DD )
  
  #remove weights to calculate modularity
  E(igraph_obj)$weight<-1
  
  #calculate modularity
  community_data<-cluster_fast_greedy(igraph_obj, weights = NULL)
  
  #calculates correlations between module composition and metadata
  biom_eigen<-eigen_correlation( data = as.data.frame(t(otu_table(phyloseq_obj))),
                                 community = community_data,
                                 metadata = test_ps_meta,
                                 categories= c("leaf_dry_weight", "MYC2_at_fold_DD", "lox2_at_fold_DD"))
  
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

# ********************************************** Done! #