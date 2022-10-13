# Wastewater Variant Caller Comparison
[//]: #wastewater_variant_caller_comparison

## Description
This is a repository for the code used in the manuscript "Evaluation of variant calling algorithms for wastewater-based epidemiology usingmixed populations of SARS-CoV-2 variants in synthetic and wastewater samples" 

Unless otherwise specified code was written by Vinoy K. Ramachandran. 

## Package installation

Set up of the tools was followed as per the tools documentions, in dedicated conda environments. 


| Tool Name | Link | Version |
|---|---|---|
| BCFtools | https://github.com/samtools/bcftools |  |
| FreeBayes | https://github.com/freebayes/freebayes| |
| GATK | https://github.com/broadinstitute/gatk ||
| iVar | https://github.com/andersen-lab/ivar| |
| LoFreq | https://github.com/CSB5/lofreq | |
| Quasimodo | https://github.com/hzi-bifo/Quasimodo | |
| Varscan |https://github.com/dkoboldt/varscan | |

## Analysis Outline

- [x] Variant calling
- [x] Quasimodo 
- [x] R scripts for enumerating stats into summary files
- [x] Plotting of figures

Example files are found in the test_example directory.


## References

Deng ZL, Dhingra A, Fritz A, Götting J, Münch PC, Steinbrück L, Schulz T, Ganzenmueller T, McHardy AC. Evaluating assembly and variant calling software for strain-resolved analysis of large DNA-viruses. Briefings in Bioinformatics. 2020:7. https://doi.org/10.1093/bib/bbaa123

*
*
`http://arxiv.org/abs/1207.3907`

[//]:freebayes paper


