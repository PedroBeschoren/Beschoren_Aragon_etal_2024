# this function will generate multiple network metrics
# it takes an igraph object and returns a dataframe with a single row per network

Generate_RealNetworks_metrics<-function (igraph_object){
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
  Network_modularity<-modularity(igraph_posWeight,
                                 cluster_fast_greedy(igraph_posWeight)$membership,
                                 weights = E(igraph_posWeight)$weight) # needs positive weights
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
