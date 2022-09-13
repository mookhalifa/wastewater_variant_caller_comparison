library(tidyverse)
library(data.table)
library(ggbreak)
library(patchwork)

WWC015_vcfstats <- read.table("WWC092_vcfstats_no_line-260522.txt", sep=':', strip.white = TRUE, fill = TRUE)
head(WWC015_vcfstats)
WWC015_vcfstats_group <- WWC015_vcfstats %>% mutate(group = cumsum(V1 =="File"))
head(WWC015_vcfstats_group)
write.csv(WWC015_vcfstats_group, "WWC092_vcfstats_group.txt")


###Python_code#######

WWC105_vcfstats_reformatted <- fread("WWC105_vcfstats_pd-out.txt")
head(WWC105_vcfstats_reformatted)

WWC105_vcfstats_reformatted[, 'caller'] <- NA

temp2 <- WWC105_vcfstats_reformatted %>% mutate(caller = ifelse(str_detect(File,"VS"), "VarScan", caller))
temp3 <- temp2 %>% mutate(caller = ifelse(str_detect(File,"bcf"), "BCFtools", caller))
temp4 <- temp3 %>% mutate(caller = ifelse(str_detect(File,"lfq"), "LoFreq", caller))
temp5 <- temp4 %>% mutate(caller = ifelse(str_detect(File,"ivar"), "iVar", caller))
temp6 <- temp5 %>% mutate(caller = ifelse(str_detect(File,"GATK"), "GATK", caller))
WWC105_vcfstats_reformatted_caller <- temp6 %>% mutate(caller = ifelse(str_detect(File,"fb"), "FreeBayes", caller))
head(WWC105_vcfstats_reformatted_caller)

add_new_cols <- c('replicate', 'rep_no', 'samp_desc', 'ID')
WWC105_vcfstats_reformatted_caller[, add_new_cols] <- NA
head(WWC105_vcfstats_reformatted_caller)

temp7 <- WWC105_vcfstats_reformatted_caller %>% mutate(replicate = ifelse(str_detect(File,"^01"), "Rep1", replicate))
temp8 <- temp7 %>% mutate(replicate = ifelse(str_detect(File,"^02"), "Rep2", replicate))
temp9 <- temp8 %>% mutate(replicate = ifelse(str_detect(File,"^03"), "Rep3", replicate))
temp10 <- temp9 %>% mutate(replicate = ifelse(str_detect(File,"^04"), "Rep4", replicate))
head(temp10)

temp11 <- temp10 %>% mutate(rep_no = ifelse(str_detect(File,"^01"), "01", rep_no))
temp12 <- temp11 %>% mutate(rep_no = ifelse(str_detect(File,"^02"), "02", rep_no))
temp13 <- temp12 %>% mutate(rep_no = ifelse(str_detect(File,"^03"), "03", rep_no))
WWC105_vcfstats_reformatted_caller_rep <- temp13 %>% mutate(rep_no = ifelse(str_detect(File,"^04"), "04", rep_no))
head(WWC105_vcfstats_reformatted_caller_rep)

WWC105_vcfstats_reformatted_caller_rep_filter_blanks <- WWC105_vcfstats_reformatted_caller_rep %>% filter(!str_detect(File,"blank"))
WWC105_vcfstats_reformatted_caller_rep_filter_blanks %>% count(caller)
WWC105_vcfstats_reformatted_caller_rep_filter_blanks %>% count(replicate)


WWC105_vcfstats_reformatted_caller_rep_filter_blanks <- WWC105_vcfstats_reformatted_caller_rep_filter_blanks %>% mutate_at("File", str_replace, ".vcf", "")
head(WWC105_vcfstats_reformatted_caller_rep_filter_blanks)

WWC105_vcfstats_reformatted_caller_rep_filter_blanks$sample2 <- WWC105_vcfstats_reformatted_caller_rep_filter_blanks$File
head(WWC105_vcfstats_reformatted_caller_rep_filter_blanks)

WWC105_vcfstats_reformatted_caller_rep_filter_blanks_samp <- WWC105_vcfstats_reformatted_caller_rep_filter_blanks %>% separate(sample2, c("rep_num", "samp_no"), extra='drop', remove=FALSE)
head(WWC105_vcfstats_reformatted_caller_rep_filter_blanks_samp)


WWC105_vcfstats_reformatted_caller_rep_filter_blanks_samp$samp_desc <- str_c(WWC105_vcfstats_reformatted_caller_rep_filter_blanks_samp$samp_no, '_', WWC105_vcfstats_reformatted_caller_rep_filter_blanks_samp$caller)
head(WWC105_vcfstats_reformatted_caller_rep_filter_blanks_samp)

WWC105_vcfstats_reformatted_caller_rep_filter_blanks_samp$ID <- str_c(WWC105_vcfstats_reformatted_caller_rep_filter_blanks_samp$rep_num, '_', WWC105_vcfstats_reformatted_caller_rep_filter_blanks_samp$samp_no)
head(WWC105_vcfstats_reformatted_caller_rep_filter_blanks_samp)

Mix_ID_key <- fread("Mix_ID_key.txt")
head(Mix_ID_key)

WWC105_vcfstats_reformatted_caller_rep_filter_blanks_samp_Mix <- left_join(WWC105_vcfstats_reformatted_caller_rep_filter_blanks_samp, Mix_ID_key, by='ID')
head(WWC105_vcfstats_reformatted_caller_rep_filter_blanks_samp_Mix)

# calculate median and group the data by samp_desc
WWC105_vcfstats_reformatted_caller_rep_filter_blanks_samp_Mix_median <- WWC105_vcfstats_reformatted_caller_rep_filter_blanks_samp_Mix %>% group_by(samp_desc) %>% summarize(sample=ID, Mix=Mix, caller=caller, samp_desc=samp_desc,rep=replicate,SNPs=snps, mean_SNPs=mean(snps), SD_SNPs=sd(snps))
head(WWC105_vcfstats_reformatted_caller_rep_filter_blanks_samp_Mix_median)
write.csv(WWC105_vcfstats_reformatted_caller_rep_filter_blanks_samp_Mix_median, file="WWC105_vcfstats_reformatted_caller_rep_filter_blanks_samp_Mix_median.txt")



mix_int <- fread("Mix_interested.txt")
head(mix_int)

WWC105_vcfstats_reformatted_caller_rep_filter_blanks_samp_Mix_median_2 <- WWC105_vcfstats_reformatted_caller_rep_filter_blanks_samp_Mix_median %>% filter(Mix %in% mix_int$Mix)
head(WWC105_vcfstats_reformatted_caller_rep_filter_blanks_samp_Mix_median_2)

point_plot <- ggplot(WWC105_vcfstats_reformatted_caller_rep_filter_blanks_samp_Mix_median_2, aes(fill = caller, y=mean_SNPs, x=caller)) + 
  scale_x_discrete(guide = guide_axis(n.dodge=2)) +
  geom_bar(position ="dodge", stat="identity", width=0.5) + 
  geom_errorbar(aes(ymin=mean_SNPs-SD_SNPs, ymax=mean_SNPs+SD_SNPs), width=0.1) + 
  facet_wrap(~Mix)
point_plot + theme_bw(base_size=8) + theme(legend.position = "bottom") 
ggsave("WWC105_vcfstats_reformatted_caller_rep_filter_blanks_samp_Mix_median_2.png",units="in", width= 12, height = 6, device='png', dpi=300)

WWC105_vcfstats_reformatted_caller_rep_filter_blanks_samp_Mix_median_2_wo_lfq <- WWC105_vcfstats_reformatted_caller_rep_filter_blanks_samp_Mix_median_2 %>% filter(!str_detect(samp_desc,"LoFreq"))
head(WWC105_vcfstats_reformatted_caller_rep_filter_blanks_samp_Mix_median_2_wo_lfq)


point_plot <- ggplot(WWC105_vcfstats_reformatted_caller_rep_filter_blanks_samp_Mix_median_2_wo_lfq, aes(fill = caller, y=mean_SNPs, x=caller)) + 
  scale_x_discrete(guide = guide_axis(n.dodge=2)) +
  geom_bar(position ="dodge", stat="identity", width = 0.5) + 
  geom_errorbar(aes(ymin=mean_SNPs-SD_SNPs, ymax=mean_SNPs+SD_SNPs), width=0.1) + 
  facet_wrap(~Mix)
point_plot + theme_bw(base_size=8) + theme(legend.position = "bottom")
ggsave("WWC105_vcfstats_reformatted_caller_rep_filter_blanks_samp_Mix_median_2_wo_lfq.png",units="in", width= 12, height = 6, device='png', dpi=300)

