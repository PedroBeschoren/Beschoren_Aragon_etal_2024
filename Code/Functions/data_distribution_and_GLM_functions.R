


continuous_distribution_plot_table <- function(x){
  
  #q-q plot 
  plot <- ggqqplot(x)
  
  # Testing different distributions to data x with fitdist function, output is a fitdist object which is a list 
  # containing ~17 different parameters, which can be accessed by fitdist$parameter
  
  n <- fitdist(x, "norm")
  #summary(n)
  
  g <- fitdist(x, "gamma") 
  #summary(g)
  
  #nb <- fitdist(x, "nbinom") 
  #summary(nb)
  
  ln <- fitdist(x, "lnorm")
  #summary(ln)
  
  #plotting distribution
  plot2 <- cdfcomp(list(n,g,ln), legendtext = c("normal", "gamma", "lnorm"))
  plot3 <- denscomp(list(n,g,ln), legendtext = c("normal", "gamma", "lnorm"))
  
  # Making summary table 
  distribution_names <- c(n$distname, g$distname, ln$distname)
  AIC <- c(n$aic, g$aic, ln$aic)
  log_likelihood <- c(n$loglik, g$loglik, ln$aic)
  BIC <- c(n$bic, g$bic, ln$bic)
  N <- c(n$n, g$n, ln$n)
  
  summary_table <- data.frame(distribution_names, AIC, BIC, log_likelihood, N)   
  
  #calculating Goodness-of-fit statistic tests
  all <- gofstat(list(n,g,ln), fitnames = c("normal", "gamma", "lnorm"))
  
  print("chisq p value")
  print(all$chisqpvalue)
  print("Anderson-Darling test")
  print(all$adtest)
  print("Cramer von  Mises test")
  print(all$cvmtest)
  print("Kolmogorov-Smirnov test")
  print(all$kstest)
  
  #Test for normal distribution 
  normality_test <- (shapiro.test(x))
  
  #Obtain:
  print("summary table")
  print(summary_table)
  print(normality_test)
  print(plot)
  #print(plot2)
  #print(plot3)
  
}

glm_gaussian_Withdf <- function(x){
  
  #This function will run and print a glm, test for assumtpions of the model, make post-hoc comparisons and has as an output a final df summarizing p values and letters for differences
  
  
  print("GLM with Gaussian family")
  
  #models
  Full <- glm (ldry_weight ~ treatment + Block,
               family = gaussian (link = "identity"),
               data = x)
  
  Optimal <- glm (ldry_weight ~ treatment,
                  family = gaussian (link = "identity"),
                  data = x)
  
  Null <- glm (ldry_weight ~ 1,
               family = gaussian (link = "identity"),
               data = x)
  
  models <- list(Full, Optimal, Null)
  model.names <- c("Full", "Optimal", "Null")  
  aic_table <- aictab(models, modnames = model.names) 
  
  print("AIC values for Full and Null models:")
  print(aic_table)
  
  
  #print("####### Summary Full #######")
  
  Summary.f<- summ(Full, digits = 3)
  #print(Summary.f)
  
  # check assumptions:
  simulateResiduals(Full, plot = T)
  plot(Full,1)
  
  
  print("####### Chisq-test Full #######")
  chisq.Full <- anova (Full,Null,test="Chisq")
  print(chisq.Full)
  
  #print("####### Summary Optimal #######")
  
  Summary.o<- summ(Optimal, digits = 3)
  #print(Summary.o)
  
  # check assumptions:
  simulateResiduals(Optimal, plot = T)
  plot(Optimal,1)
  
  print("####### Chisq-test Optimal #######")
  chisq.Optimal <- anova (Optimal,Null,test="Chisq")
  print(chisq.Optimal)
  
  # make a list with both Full and Optimal GLM's
  
  list <- list(Full, Optimal, Null)
  names(list) <- c("Full", "Optimal", "Null")
  
  return(list)
  
} 

summary_model <- function(GLM, Null){
  
  print("###### Summary table #######")
  
  print(summ(GLM, digits = 4))
  
  print("####### Chisq-test FULL #######")
  chisq.Full <- anova (GLM,Null,test="Chisq")
  print(chisq.Full)
  
  print("###### Wald-ChiSquare test #######")
  
  print(aov <- Anova(GLM, test.statistic = "Wald", type = "II"))
  
  #print("###### D2 #######")
  
  #D2 <-(1-(GLM$deviance/GLM$null.deviance))
  #print("D2 value (explanation of model) is:") #d2
  #print(D2)
  
  #print("###### pseudo-R2 #######")
  #R2 <- PseudoR2(GLM, which = "McFadden")
  #print("Pseudo-R2 McFadden is:") #pseudo-r2 
  #print(R2)
  
  
  #Making df 
  
  df_aov <- aov %>% 
            tibble::rownames_to_column("GLM_factors")  
    #mutate(D2 = D2)%>%
    #mutate(pseudo_R2 = R2)
  
  
  return(df_aov) 
  
}

