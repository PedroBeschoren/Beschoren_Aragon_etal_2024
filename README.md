# Jasmonic-acid induction in Arabidopsis and Brassica shoots results in stimulation of Comamonadaceae and Rhizobiales in the rhizosphere and endosphere

This repository holds the full data and code for this manuscript (auhtor names and affiliations available upon publication)

Instructions for the use of the qiime2 pipeline used for this repository can be found [here](https://github.com/PedroBeschoren/Beschoren_Aragon_etal_2023/blob/main/Code/qiime2_preProcessing/README.MD)

Instructions for the use of the R pipeline used for this repository can be found [here](https://github.com/PedroBeschoren/Beschoren_Aragon_etal_2023/blob/main/Code/README.MD)

This research, and the public availability of the code, was funded by the Dutch Research Council (NWO, Gravitation programme, MiCRop project, 024.004.014).

### These are the main analysis methods employed
    * Permanovas, beta dispersion and NMDS ordinations on CSS normalized microbiome data
    * basic alpha diversity analysis (shannon)
    * Classic DeSeq2 analysis for differentially abundant ASVs
    * Random Forest, defining ASVs that predict treatment classes
    * Network analysis, defining keystone taxa, module connectors and module hubs
    * Summarization of ASVs classified as important by random forest, networks and deseq with fisher tests
        * visualization of ASV taxonomies with heat trees
    * Neutral models per treatment using species + compartment as the source
        * follow-up with above-expected communities permanovas and heat tree visualization
        * follow-up with diversity indexes (Shannon, simpson and obseved richness) within every single taxonomic group

### Can I just donwload all of it and re-run locally?

Yes! If you don't feel like cloning the github repository you can jsut download a .zip from the green "code" box just above

run the scripts serially as in the instructions linked above. R package isntallation is facilitated by the first chunk of the first script


### Suggested tutorials for key tools used in this repository

**R studio** has a lot of handy tools & hotkeys I keep learning about: https://www.rstudio.com/resources/webinars/programming-part-1-writing-code-in-rstudio/

**R project** files are essential to organize your files and working directories: https://www.rstudio.com/resources/webinars/managing-part-1-projects-in-rstudio/

**renv** is a R package that manages R manage packages for you: https://rstudio.github.io/renv/articles/renv.html

**R markdown** allows you to organize your code into digestible chunks of code, and let's you better organize sections within your script: https://www.rstudio.com/resources/webinars/getting-started-with-r-markdown/ 

**Git & GitHub** allow for proper version control and can be integrated with R Studio. it has a steep learning curve but it's well worth the time if you want to be proficient in computation
    * https://www.rstudio.com/resources/webinars/managing-part-2-github-and-rstudio/
    * https://www.geo.uzh.ch/microsite/reproducible_research/post/rr-rstudio-git/

**phyloseq** is an essential package for this pipeline: https://joey711.github.io/phyloseq/

**Metacoder** is a nice new tool to creat heat trees for phylogenetic-comprehesive figures of sets of ASVs: https://grunwaldlab.github.io/metacoder_documentation/

**lists** can be used to store multiple dataframes or phyloseq objects, which can then be looped in lapply and mapply: https://www.youtube.com/watch?v=Rjb7sDfq_Jg

**lapply** is a powerful and useful function to loop. say goodbye to "for" loops! learn to use it: https://www.youtube.com/watch?v=8MVgYu0y-E4

**mapply** is a multivariate version of lapply, letting you handle multiple lists simultaneously: https://www.youtube.com/watch?v=V-a7CVRo-3g

**RAM memory** can be a big problem in large datasets: http://www.bytemining.com/2010/08/taking-r-to-the-limit-part-ii-large-datasets-in-r/
