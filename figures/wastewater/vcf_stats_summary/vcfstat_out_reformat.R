library(tidyverse)


WWC092_vcfstats <- read.table("WWC092_vcfstats_no_line-250422.txt", sep=':', strip.white = TRUE, fill = TRUE)
head(WWC092_vcfstats)
WWC092_vcfstats_group <- WWC092_vcfstats %>% mutate(group = cumsum(V1 =="File"))
head(WWC092_vcfstats_group)
write.csv(WWC092_vcfstats_group, "WWC092_vcfstats_group_250422.txt")