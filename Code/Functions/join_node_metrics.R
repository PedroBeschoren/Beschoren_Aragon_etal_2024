#this function will take the output of other custom fuctions present in this script: Zi_Pi_metrics, Generate_node_metrics2 and KeystoneDetector3
# then it will join all dataframes into a single dataframe
# finally it will provide yes/no classification for keystones, modules and conectors

join_node_metrics<-function (Zi_Pi_list_product, Generate_node_metrics2_product, KeystoneDetector3_product){
  #Takes the first product of Zi_Pi_metrics(), created in section 5, and merge with product of Generate_node_metrics2()
  Node_metrics<-Zi_Pi_list_product[[1]]%>%
    column_to_rownames(var = "names")%>%
    select(-module_connections,-total_connections)%>%
    merge(Generate_node_metrics2_product, by=0)%>%
    column_to_rownames(var="Row.names")
  
  Node_metrics$module<-as.factor(Node_metrics$module)
  
  # tesll in the node df if the taxa is a keystone
  Node_metrics$Keystone_taxa<-ifelse(row.names(Node_metrics)%in%row.names(KeystoneDetector3_product[[2]]),
                                     "Yes", "No")
  
  # tesll in the node df if the taxa is a module hub
  Node_metrics$Module_Hub_taxa<-ifelse(Node_metrics$Z>=2.5,
                                       "Yes", "No")
  Node_metrics$Module_Hub_taxa[is.na(Node_metrics$Module_Hub_taxa)] <-"No" # NA will apear if it has no within-module connections
  
  # tesll in the node df if the taxa is a module connector
  Node_metrics$Module_Connector_taxa<-ifelse(Node_metrics$P>=0.62,
                                             "Yes", "No")
  return(Node_metrics)
}
