# this fucntion will compare the real and 100 random networks, returning metrics that are different form random as TRUE
# it takes an igraph object as an input
# it's output is a dataframe showing the metrics for the real network, and  mean, max and lower metrics for 100 random networks of identical number of nodes and edges
# The output also tells if the real netowrk metric differs from the random network metrics
Real_VS_random_networks<- function(igraph_object){
  
  # This function generates the metrics that are relevant for comparsions of a real netowrk with a random network with the same number of nodes and edges
  Generate.metrics.randomNet<-function (igraph_object){
    
    # now, let's put the centrality values all in the same place
    # let's calculate the centrality metrics in a function
    # get node's degree, closeness centrality, and ebtweness centrality. note: negative weights not allowed, and disconnected modules not allowed
    
    # remove negative weights to calculate modularity and mean edge/node betweness
    igraph_posWeight<-igraph_object # create new igraph obejct to calculate cloness and betweness from it
    E(igraph_posWeight)$weight<-sqrt(E(igraph_posWeight)$weight*E(igraph_posWeight)$weight) # pwoer and square root the weights to destroy the signal. negative weights cannot be used to calculate node closeness and centrality
    
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
    Network_mean_shortest_path<-mean_distance(igraph_object)
    
    # calculates modules 
    cluster_fast_greedy(igraph_posWeight) # cannot use negative weights, but assigns modules
    
    #calculates modularity
    Network_modularity<-modularity(igraph_posWeight,
                                   cluster_fast_greedy(igraph_posWeight)$membership,
                                   weights = E(igraph_posWeight)$weight) # needs positive weights
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
  
  # calculate 100 random netowrks and their metrics
  rand_list<-replicate(100,Generate.metrics.randomNet(rewire(igraph_object,each_edge(1))))
  
  #put 100 random networks in a  dataframe
  rand_df<-data.frame(matrix(unlist(rand_list), nrow=100, byrow=TRUE, dimnames = list(1:100,colnames(rand_list))),check.names = FALSE)
  
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