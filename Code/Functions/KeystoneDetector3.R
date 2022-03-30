# This function detect keystone ataxa based on degree, closeness centrality and betweeness centrality
# instead of picking a top%, the function considers a Z distribution of the metrics, and then pinpoints otus with significantly higher metrics
# it requires a dataframe with node metrics 
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
