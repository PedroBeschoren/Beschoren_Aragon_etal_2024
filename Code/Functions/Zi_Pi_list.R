# this function will take ~40 sec to run on a network with 600 nodes and 1000 edges, mostly beause of Zi and Pi calculations
# it's input is a phyloseq object
# it will return a list, with Zi/Pi values for nodes as well as a count of module hubs (Zi>2.5) and connectors (Pi>0.62)

Zi_Pi_list<-function (igraph_object) {

  
  # remove negative weights to calculate modularity and mean edge/node betweness
  igraph_posWeight<-igraph_object # create new igraph obejct to calculate cloness and betweness from it
  E(igraph_posWeight)$weight<-1 # make all weights =1 (that is, remove weights) as it skews the ZiPi function
  
  
#  components<-components(igraph_object) #obtain network components that are unconnected the the main graph 
#  Main_component = which.max(components$csize) #define the largest compenent
#  igraph_posWeight_single_component <- induced_subgraph(igraph_posWeight, which(components$membership == Main_component)) #makes a subgraph, removing unconected compenets
#  igraphsingle_component <- induced_subgraph(igraph_object, which(components$membership == Main_component)) #makes a subgraph, removing unconected compenets
  
  
    #define modules
   community_data<-cluster_fast_greedy(igraph_posWeight, weights = NULL) # using weights completely skews the Pi metric
   
   #adjust module data object to contain only module membership of modules with more than 4 mebers
#   big_module<-community_data[sizes(community_data)>1] # modules above 1 members
#   big_module_names<-as.numeric(names(big_module))# names of big moduels as numeric vector
   module_data<-community_data$membership#[community_data$membership%in%big_module_names]# subsets module mebership to only include modules named as modules above 4 memebrs
   
# adjsut igraph object to remove nodes present in small modules
#   big_module_nodes<-unlist(flatten(big_module))# names of nodes in big modules
#   node_to_remove<-which(!community_data$names%in%big_module_nodes)# vector of vertices NOT in both objects
#   igraph_posWeight_single_component_big_modules<-delete_vertices(igraph_posWeight_single_component, node_to_remove)
   
   
   #community_data2<-cluster_fast_greedy(igraph_posWeight_single_component_big_modules, weights = NULL)
   
   #module_data2<-cut_at(community_data2, n = 12)
  
# module_data<-cluster_fast_greedy(igraph_object, weights = NULL)$membership
 
# to few communities warning #  module_data<-cut_at(module_data,(round(modularity(module_data)*10))) # defines the max number of modules as 10x the modularity
  
  #Zi and Pi metrics
  #the ZiPi fucntion is from the microbiome package, but loading and isntallation of the package might be slightly troublesome
  # here's the copied function to be defined in the session, if needed #
  
  ZiPi<- function (netw = "network", modules = "modules") {
    names = V(netw)$name
    total_connections = c()
    module_connections = c()
    number_of_modules = c()
    meanZ = c()
    sdZ = c()
    Z = c()
    P = c()
    for (i in 1:length(names)) {
      total_connections[i] = sum(netw[i] > 0)
      module_connections[i] = sum(netw[i][which(modules == 
                                                  modules[i])] > 0)
      KitKi = c()
      for (j in 1:length(unique(modules))) {
        KitKi[j] = ((sum(netw[i][which(modules == j)]))/total_connections[i])^2
      }
      P[i] = 1 - sum(KitKi)
    }
    for (i in 1:length(unique(modules))) {
      meanZ[i] = mean(module_connections[which(modules == 
                                                 i)])
      sdZ[i] = sd(module_connections[which(modules == i)])
    }
    for (i in 1:length(names)) {
      Z[i] = (module_connections[i] - meanZ[modules[i]])/sdZ[modules[i]]
    }
    return(cbind.data.frame(names, module = modules, module_connections, 
                            total_connections, Z, P))
  }
  
  Zipi_calculated<-ZiPi(igraph_posWeight, modules = module_data)
  Module_hubs<-length(which(Zipi_calculated$Z>=2.5))
  Module_connectors<-length(which(Zipi_calculated$P>=0.62))
  
  network_metrics<-data.frame(Module_hubs, Module_connectors)
  return(list(Zipi_calculated,network_metrics))
}
