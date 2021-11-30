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
