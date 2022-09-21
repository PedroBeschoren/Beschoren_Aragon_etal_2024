# MICROP's MeJA_Pilot repository @ WUR

## Welcome MICROP PhD students & other collaborators!

This repository holds the **full** data and code for the WUR-ENTO MeJA pilot experiment where we test different methyl jasmonate concentration to stress *Arabidopsis thaliana* and *Brassica oleraceae*. We then evaluate rhizosphere and endosphere microbial communities with various methods. this project is in manuscript draft stage, so no new analysis will be performed.

Are you looking for the raw sequences? they are available after publication on https://dataview.ncbi.nlm.nih.gov/object/PRJNA873942

#### These are the main analysis methods employed
    * Permanovas, beta dispersion and NMDS ordinations oc CSS normalized microbiome data
    * basic alpha diversity analysis
    * Neutral model per treatment using species + compartment as the source
        * follow-up with above-expected communities ordinations and heat tree visualization
        * follow-up with diversity indexes within every single taxonomic group
    * Classic DeSeq2 analysis for differentially abundant ASVs
    * Random Forest, defining ASVs that predict treatment classes
    * Network analysis, defining keystone taxa, module connectors and module hubs
    * Summarization of ASVs classified as important by random forest, networks and deseq with fisher tests
    * visualization of ASV taxonomies with heat trees

### How to make use of this repository & run the analysis

1 - After you clone/download all content, you can use renv::restore() to install all packages with the right versions. 

2 - The first chunk of script 1_Loading_and_pre_processing_published will load all the libraries necessary for the analysis.

3 - you could then continue running scripts from 1 to 9, executing all analysis serially. Otherwise, just load "./R output/all_output_saved.RData" to get all processed objects.


    *Script 1 loads the data and performs all normalization, rarefaction, etc. it provides the phyloseq objects accessed in other scripts.
    *Script 2-7 are rather independent from each other, as long as the phyloseq objects from script 1 are available. note that my 16GB 4-core i7 system takes ~2h to calculate the networks and ~2h to calculate random forest.
    *Script 8 requires all the objects generated in scripts 2-7.
    *script 9 requires script 8, the output is a nice summary of the whole pipeline.


#### please do not alter the files you can see in this repository

Do you have a suggestion? **good, I would love to see it!** Please fork, commit and show me your branch. do not alter the files you see here directly.

### Suggested tutorials

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
