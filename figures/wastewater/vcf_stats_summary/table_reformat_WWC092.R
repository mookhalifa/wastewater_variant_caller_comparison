library(tidyverse)
library(data.table)
library(ggbreak)
library(patchwork)
#updated on 26 May 22
# input file, is an output file from pivot_table.py
WWC092_vcfstats_reformatted <- fread("WWC092_vcfstats_pd-out.txt")
head(WWC092_vcfstats_reformatted)

WWC092_vcfstats_reformatted[, 'caller'] <- NA
WWC092_vcfstats_reformatted[, 'samp_code'] <- NA
head(WWC092_vcfstats_reformatted)
temp2 <- WWC092_vcfstats_reformatted %>% mutate(caller = ifelse(str_detect(File,"VS"), "VarScan", caller))
temp3 <- temp2 %>% mutate(caller = ifelse(str_detect(File,"bcf"), "BCFtools", caller))
temp4 <- temp3 %>% mutate(caller = ifelse(str_detect(File,"lfq"), "LoFreq", caller))
temp5 <- temp4 %>% mutate(caller = ifelse(str_detect(File,"ivar"), "iVar", caller))
temp6 <- temp5 %>% mutate(caller = ifelse(str_detect(File,"GATK"), "GATK", caller))
temp7  <- temp6 %>% mutate(caller = ifelse(str_detect(File,"fb"), "FreeBayes", caller))
temp8 <- temp7 %>% mutate(samp_code = ifelse(str_detect(File,"S42"), "S42", samp_code))
temp9 <- temp8 %>% mutate(samp_code = ifelse(str_detect(File,"S43"), "S43", samp_code))
temp10 <- temp9 %>% mutate(samp_code = ifelse(str_detect(File,"S50"), "S50", samp_code))
temp11 <- temp10 %>% mutate(samp_code = ifelse(str_detect(File,"S58"), "S58", samp_code))
temp12 <- temp11 %>% mutate(samp_code = ifelse(str_detect(File,"S59"), "S59", samp_code))
temp13 <- temp12 %>% mutate(samp_code = ifelse(str_detect(File,"S263"), "S263", samp_code))
temp14 <- temp13 %>% mutate(samp_code = ifelse(str_detect(File,"S296"), "S296", samp_code))
temp15 <- temp14 %>% mutate(samp_code = ifelse(str_detect(File,"S292"), "S292", samp_code))
temp16 <- temp15 %>% mutate(samp_code = ifelse(str_detect(File,"S270"), "S270", samp_code))
temp17 <- temp16 %>% mutate(samp_code = ifelse(str_detect(File,"S278"), "S278", samp_code))
temp18 <- temp17 %>% mutate(samp_code = ifelse(str_detect(File,"S302"), "S302", samp_code))
temp19 <- temp18 %>% mutate(samp_code = ifelse(str_detect(File,"S305"), "S305", samp_code))
WWC092_vcfstats_reformatted_caller <- temp19 %>% mutate(samp_code = ifelse(str_detect(File,"S63"), "S63", samp_code))
head(WWC092_vcfstats_reformatted_caller)
WWC092_vcfstats_reformatted_caller %>% count(caller)

WWC092_vcfstats_reformatted_caller <- WWC092_vcfstats_reformatted_caller %>% mutate_at("File", str_replace, ".vcf", "")
head(WWC092_vcfstats_reformatted_caller)

point_plot <- ggplot(WWC092_vcfstats_reformatted_caller, aes(fill = caller, y=snps, x=caller)) + 
  scale_x_discrete(guide = guide_axis(n.dodge=2)) +
  geom_bar(position ="dodge", stat="identity", width=0.5) + 
  #geom_errorbar(aes(ymin=mean_SNPs-SD_SNPs, ymax=mean_SNPs+SD_SNPs), width=0.1) + 
  facet_wrap(~samp_code)
point_plot + theme_bw(base_size=8) + theme(legend.position = "bottom") 
ggsave("WWC092_vcfstats_reformatted_caller-snps.png",units="in", width= 12, height = 6, device='png', dpi=300)

WWC092_vcfstats_reformatted_caller_wo_lfq <- WWC092_vcfstats_reformatted_caller %>% filter(!str_detect(caller,"LoFreq"))
head(WWC092_vcfstats_reformatted_caller_wo_lfq)

point_plot <- ggplot(WWC092_vcfstats_reformatted_caller_wo_lfq, aes(fill = caller, y=snps, x=caller)) + 
  scale_x_discrete(guide = guide_axis(n.dodge=2)) +
  geom_bar(position ="dodge", stat="identity", width=0.5) + 
  #geom_errorbar(aes(ymin=mean_SNPs-SD_SNPs, ymax=mean_SNPs+SD_SNPs), width=0.1) + 
  facet_wrap(~samp_code)
point_plot + theme_bw(base_size=8) + theme(legend.position = "bottom") 
ggsave("WWC092_vcfstats_reformatted_caller_wo_lfq-snps.png",units="in", width= 12, height = 6, device='png', dpi=300)

#plotting indels

point_plot <- ggplot(WWC092_vcfstats_reformatted_caller, aes(fill = caller, y=indels, x=caller)) + 
  scale_x_discrete(guide = guide_axis(n.dodge=2)) +
  geom_bar(position ="dodge", stat="identity", width=0.5) + 
  #geom_errorbar(aes(ymin=mean_SNPs-SD_SNPs, ymax=mean_SNPs+SD_SNPs), width=0.1) + 
  facet_wrap(~samp_code)
point_plot + theme_bw(base_size=8) + theme(legend.position = "bottom") 
ggsave("WWC092_vcfstats_reformatted_caller-indels.png",units="in", width= 12, height = 6, device='png', dpi=300)


point_plot <- ggplot(WWC092_vcfstats_reformatted_caller_wo_lfq, aes(fill = caller, y=indels, x=caller)) + 
  scale_x_discrete(guide = guide_axis(n.dodge=2)) +
  geom_bar(position ="dodge", stat="identity", width=0.5) + 
  #geom_errorbar(aes(ymin=mean_SNPs-SD_SNPs, ymax=mean_SNPs+SD_SNPs), width=0.1) + 
  facet_wrap(~samp_code)
point_plot + theme_bw(base_size=8) + theme(legend.position = "bottom") 
ggsave("WWC092_vcfstats_reformatted_caller_wo_lfq-indels.png",units="in", width= 12, height = 6, device='png', dpi=300)

#plotting mnps

point_plot <- ggplot(WWC092_vcfstats_reformatted_caller, aes(fill = caller, y=mnps, x=caller)) + 
  scale_x_discrete(guide = guide_axis(n.dodge=2)) +
  geom_bar(position ="dodge", stat="identity", width=0.5) + 
  #geom_errorbar(aes(ymin=mean_SNPs-SD_SNPs, ymax=mean_SNPs+SD_SNPs), width=0.1) + 
  facet_wrap(~samp_code)
point_plot + theme_bw(base_size=8) + theme(legend.position = "bottom") 
ggsave("WWC092_vcfstats_reformatted_caller-mnps.png",units="in", width= 12, height = 6, device='png', dpi=300)


point_plot <- ggplot(WWC092_vcfstats_reformatted_caller_wo_lfq, aes(fill = caller, y=mnps, x=caller)) + 
  scale_x_discrete(guide = guide_axis(n.dodge=2)) +
  geom_bar(position ="dodge", stat="identity", width=0.5) + 
  #geom_errorbar(aes(ymin=mean_SNPs-SD_SNPs, ymax=mean_SNPs+SD_SNPs), width=0.1) + 
  facet_wrap(~samp_code)
point_plot + theme_bw(base_size=8) + theme(legend.position = "bottom") 
ggsave("WWC092_vcfstats_reformatted_caller_wo_lfq-mnps.png",units="in", width= 12, height = 6, device='png', dpi=300)

##########################################################################################################################################################################################

# add_new_cols <- c('replicate', 'rep_no', 'samp_desc', 'ID')
# WWC092_vcfstats_reformatted_caller[, add_new_cols] <- NA
# head(WWC092_vcfstats_reformatted_caller)
# 
# temp7 <- WWC092_vcfstats_reformatted_caller %>% mutate(replicate = ifelse(str_detect(File,"^01"), "Rep1", replicate))
# temp8 <- temp7 %>% mutate(replicate = ifelse(str_detect(File,"^02"), "Rep2", replicate))
# temp9 <- temp8 %>% mutate(replicate = ifelse(str_detect(File,"^03"), "Rep3", replicate))
# temp10 <- temp9 %>% mutate(replicate = ifelse(str_detect(File,"^04"), "Rep4", replicate))
# head(temp10)
# 
# temp11 <- temp10 %>% mutate(rep_no = ifelse(str_detect(File,"^01"), "01", rep_no))
# temp12 <- temp11 %>% mutate(rep_no = ifelse(str_detect(File,"^02"), "02", rep_no))
# temp13 <- temp12 %>% mutate(rep_no = ifelse(str_detect(File,"^03"), "03", rep_no))
# WWC092_vcfstats_reformatted_caller_rep <- temp13 %>% mutate(rep_no = ifelse(str_detect(File,"^04"), "04", rep_no))
# head(WWC092_vcfstats_reformatted_caller_rep)

#WWC092_vcfstats_reformatted_caller_rep_filter_blanks <- WWC092_vcfstats_reformatted_caller_rep %>% filter(!str_detect(File,"blank"))

#WWC092_vcfstats_reformatted_caller_rep %>% count(replicate)
# 
# 
# 
# 
# WWC092_vcfstats_reformatted_caller_rep_filter_blanks$sample2 <- WWC092_vcfstats_reformatted_caller_rep_filter_blanks$File
# head(WWC092_vcfstats_reformatted_caller_rep_filter_blanks)
# 
# WWC092_vcfstats_reformatted_caller_rep_filter_blanks_samp <- WWC092_vcfstats_reformatted_caller_rep_filter_blanks %>% separate(sample2, c("rep_num", "samp_no"), extra='drop', remove=FALSE)
# head(WWC092_vcfstats_reformatted_caller_rep_filter_blanks_samp)
# 
# 
# WWC092_vcfstats_reformatted_caller_rep_filter_blanks_samp$samp_desc <- str_c(WWC092_vcfstats_reformatted_caller_rep_filter_blanks_samp$samp_no, '_', WWC092_vcfstats_reformatted_caller_rep_filter_blanks_samp$caller)
# head(WWC092_vcfstats_reformatted_caller_rep_filter_blanks_samp)
# 
# WWC092_vcfstats_reformatted_caller_rep_filter_blanks_samp$ID <- str_c(WWC092_vcfstats_reformatted_caller_rep_filter_blanks_samp$rep_num, '_', WWC092_vcfstats_reformatted_caller_rep_filter_blanks_samp$samp_no)
# head(WWC092_vcfstats_reformatted_caller_rep_filter_blanks_samp)
# 
# Mix_ID_key <- fread("Mix_ID_key.txt")
# head(Mix_ID_key)
# 
# WWC092_vcfstats_reformatted_caller_rep_filter_blanks_samp_Mix <- left_join(WWC092_vcfstats_reformatted_caller_rep_filter_blanks_samp, Mix_ID_key, by='ID')
# head(WWC092_vcfstats_reformatted_caller_rep_filter_blanks_samp_Mix)
# 
# # calculate median and group the data by samp_desc
# WWC092_vcfstats_reformatted_caller_rep_filter_blanks_samp_Mix_median <- WWC092_vcfstats_reformatted_caller_rep_filter_blanks_samp_Mix %>% group_by(samp_desc) %>% summarize(sample=ID, Mix=Mix, caller=caller, samp_desc=samp_desc,rep=replicate,SNPs=snps, mean_SNPs=mean(snps), SD_SNPs=sd(snps))
# head(WWC092_vcfstats_reformatted_caller_rep_filter_blanks_samp_Mix_median)
# write.csv(WWC092_vcfstats_reformatted_caller_rep_filter_blanks_samp_Mix_median, file="WWC092_vcfstats_reformatted_caller_rep_filter_blanks_samp_Mix_median.txt")
# 
# 
# 
# mix_int <- fread("Mix_interested.txt")
# head(mix_int)
# 
# WWC092_vcfstats_reformatted_caller_rep_filter_blanks_samp_Mix_median_2 <- WWC092_vcfstats_reformatted_caller_rep_filter_blanks_samp_Mix_median %>% filter(Mix %in% mix_int$Mix)
# head(WWC092_vcfstats_reformatted_caller_rep_filter_blanks_samp_Mix_median_2)
# 
# point_plot <- ggplot(WWC092_vcfstats_reformatted_caller_rep_filter_blanks_samp_Mix_median_2, aes(fill = caller, y=mean_SNPs, x=caller)) + 
#   scale_x_discrete(guide = guide_axis(n.dodge=2)) +
#   geom_bar(position ="dodge", stat="identity", width=0.5) + 
#   geom_errorbar(aes(ymin=mean_SNPs-SD_SNPs, ymax=mean_SNPs+SD_SNPs), width=0.1) + 
#   facet_wrap(~Mix)
# point_plot + theme_bw(base_size=8) + theme(legend.position = "bottom") 
# ggsave("WWC092_vcfstats_reformatted_caller_rep_filter_blanks_samp_Mix_median_2.png",units="in", width= 12, height = 6, device='png', dpi=300)
# 
# WWC092_vcfstats_reformatted_caller_rep_filter_blanks_samp_Mix_median_2_wo_lfq <- WWC092_vcfstats_reformatted_caller_rep_filter_blanks_samp_Mix_median_2 %>% filter(!str_detect(samp_desc,"LoFreq"))
# head(WWC092_vcfstats_reformatted_caller_rep_filter_blanks_samp_Mix_median_2_wo_lfq)
# 
# 
# point_plot <- ggplot(WWC092_vcfstats_reformatted_caller_rep_filter_blanks_samp_Mix_median_2_wo_lfq, aes(fill = caller, y=mean_SNPs, x=caller)) + 
#   scale_x_discrete(guide = guide_axis(n.dodge=2)) +
#   geom_bar(position ="dodge", stat="identity", width = 0.5) + 
#   geom_errorbar(aes(ymin=mean_SNPs-SD_SNPs, ymax=mean_SNPs+SD_SNPs), width=0.1) + 
#   facet_wrap(~Mix)
# point_plot + theme_bw(base_size=8) + theme(legend.position = "bottom")
# ggsave("WWC092_vcfstats_reformatted_caller_rep_filter_blanks_samp_Mix_median_2_wo_lfq.png",units="in", width= 12, height = 6, device='png', dpi=300)
# 
