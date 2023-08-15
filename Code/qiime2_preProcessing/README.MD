# How to use this QIIME2 pipeline and reproduce results

This bash code pipeline contains several different .sb files that should be run sequentially according their enumeration. They follow a very straightforward approached used in qiime2 

## Requirements 

This pipeline was developed to run in the Anunna HPC of Wageningen University and research. 
* trimmomatic v 0.39
* qiime2-2021.2 
* Operating System: Ubuntu 20.04 LTS
* Kernel: Linux 4.15.0-177-generic
* Architecture: x86-64
* Raw sequence data were deposited at NCBI SRA with access number PRJNA873942 (https://dataview.ncbi.nlm.nih.gov/object/PRJNA873942).
* metadata files (Mapping_file_MeJA_pilot.txt and manifest_pilot.txt) are available on the “Code
/qiime2_preProcessing/” folder

### Setup and installation

Use standard tutorials in https://qiime2.org/ to install qiime2

## Sequentially running the pipeline

All code is svailable on the “Code/qiime2_preProcessing/” folder
* The numbered sb files should be executed sequentially. Run 0_Trimmomatic before 1_Import_paired_Data, etc. all scripts are written as slurm job submission files.
* theses scripts are heavily hard-coded, but use very popular software and options without any complex custom functions
### details about each script in the pipeline
* **0_Trimmomatic.sb:** trims low quality reads based on a sliding window approach with Trimmomatic
* **2_remove_primers.sb:** remove primer sequences with the cutadapt plugin of qiime2
* **3_dada2.sb:** runs dada2 pipeline (merging, denoising, chimera removal, filtering) with the pluging in qiime2
* **4_AlphaBetaDiv.sb:** performs a handful of preliminary alpha and beta diversity analysis for basic data quality checks
* **5_taxonomy.sb:** assigns taxonomies based on a re-trained SILVA 138 database, considering primer sequence and fragment length. Reference training was performed as in the standard qiime2 tutorials. Our code for this step can be found in “Code/qiime2_preProcessing/Reference_training”
* ** 6_export_as_biom.sb:** exports qiime2 artefacts as a .biom file (much lighter, can include taxonomy and metadata)
* ** 7_join_BIOM.sb:** merge taxonomy and metadata to feature table of biom file. This output will be the key input of the R pipeline.





