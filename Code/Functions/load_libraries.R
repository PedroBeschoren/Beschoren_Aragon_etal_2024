## 0 - Loading  R Libraries
# If a package is installed, it will be loaded. If any are not installed, the missing package(s) will be installed 
# from CRAN and then loaded.

# installation of packages in ".renv/library/R-4.1/x86_64-w64-mingw32" can be facilitated if you run renv::restore()



## First specify the packages of interest
packages <- c(
  "devtools", # needed to install some packages
  "BiocManager", # needed to install some packages
  "remotes", # needed to install some packages
  "ggplot2",
  "dplyr",
  "tibble",
  "stringr", # to wrangle string vectors
  "Boruta", # for random forest feature selection
  "mlbench", # for random forest
  "caret", # for random forest
  "randomForest", # for random forest
  "tidyr",
  "vegan", # for several essential statistical tests
  "forcats",
  "ggrepel", # to avoid legends overlapping in your plot
  "ggfortify", # to run autoplot on PCA from prcomp
  "ggpubr",
  "igraph", # calculates entowrk metrics and manipulates netowrk objects
  "metagMisc", #  lets you create lists of split phyloseq objects
  "pheatmap", # heatmaps for deseq2
  "viridis", # nice colors
  "agricolae", # includes some anova post-hoc options
  "minpack.lm", # lets you do som HSD tests, output is a nive table
  "Hmisc", # for neutral models
  "spaa", # need to install Ecoutils
  "stats4", # for neutral models
  "car", # for levene test
  "indicspecies", # runs indicator species analysis
  "metacoder", # plots heat trees
  "purrr", # has map() to select table elements
  "viridis"
) # prety colors for deseq2 heatmaps


## Now load or install & load packages
lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)

# renv dependency dection works for these packages only if you laod them like this
library(Boruta)
library(DESeq2)


biocManager_packages <- c(
  "phyloseq", # essential to produce phyloseq objects with OTU, metadata, and taxa info in one single place
  "microbiome", # has convinient data wrangling functions
  "decontam", # to make use of no-template blank DNA extractions
  "metagenomeSeq", # to normalize library sizes without rarefying them
  "DESeq2"
) # used for differential abundance analysis



## Now load or install & load packages managed by BiocManager
lapply(
  biocManager_packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      BiocManager::install(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)


# additional packages outside CRAN or bioconductor
if (!require(tyRa)) install_github("DanielSprockett/tyRa")
library("tyRa") # need for neutral models

if (!require(metagMisc)) devtools::install_github("vmikk/metagMisc")
library("metagMisc") # n lets you split a phyloseq object in a list

if (!require(EcolUtils)) devtools::install_github("GuillemSalazar/EcolUtils")
library("EcolUtils") # pairwise adonis fucntion

if (!require(ggVennDiagram)) remotes::install_github("gaospecial/ggVennDiagram")
library("ggVennDiagram") # for venn diagrams on SPSS objects

if (!require(SpiecEasi)) install_github("zdk123/SpiecEasi")
library("SpiecEasi") # builds the sparse networks

if (!require(MicEco)) install_github("Russel88/MicEco")
library("MicEco") # better venn daigrams

rm(packages, biocManager_packages) # we don't need this anymore so let's remove them from the environemnt
gc() # decluters memory usage

