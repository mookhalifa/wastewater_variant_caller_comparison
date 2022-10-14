## Bash commands and scripts

Files are organised into 4 groups, ordered by sequential order of execution: 

0{1} - samtools indexing, to be run first.

1{1:6} - variant calling, one script per tool. To allow users to change conda environemnts between usage.

2{1:2} - Quasimodo, small script for 4 or less samples, large script for analysing more than 4 samples at a time.

3{1:6} - extracting frequecies from VCFs for plots and figures.
