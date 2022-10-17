library(tidyverse)
library(gtools)
library(data.table)
library(stringr)
library(tidyr)
library(openxlsx)

# read in the data 

quasi_1 <- fread("snpcall_benchmark_MILK_1.txt")
quasi_2 <- fread("snpcall_benchmark_MILK_2.txt")
quasi_3 <- fread("snpcall_benchmark_MILK_3.txt")

head(quasi_1)
head(quasi_2)
head(quasi_3)

temp  <-  quasi_1 %>% filter(!str_detect(caller,"fb_def"))
temp1 <-  temp %>% filter(!str_detect(caller,"lfq"))
temp2 <-  temp1 %>% filter(!str_detect(caller,"GATK"))
quasi_1_filtered <-  temp2 %>% filter(!str_detect(caller,"VS"))
#write.csv(quasi_1_filtered, file="snpcall_benchmark_MILK_1_filtered.txt")

quasi_merged <- rbind(quasi_1_filtered, quasi_2, quasi_3)
names(quasi_merged)[1] <- "sample"
head(quasi_merged)
write.csv(quasi_merged, file="snpcall_benchmark_MILK_merged.txt")
#write.xlsx(quasi_merged, "snpcall_benchmark_MILK_merged.xlsx")


quasi_merged[, 'caller'] <- NA

temp2 <- quasi_merged %>% mutate(caller = ifelse(str_detect(sample,"VS"), "VarScan", caller))
temp3 <- temp2 %>% mutate(caller = ifelse(str_detect(sample,"bcf"), "BCFtools", caller))
temp4 <- temp3 %>% mutate(caller = ifelse(str_detect(sample,"lfq"), "LoFreq", caller))
temp5 <- temp4 %>% mutate(caller = ifelse(str_detect(sample,"ivar"), "iVar", caller))
temp6 <- temp5 %>% mutate(caller = ifelse(str_detect(sample,"GATK"), "GATK", caller))
quasi_merged_caller <- temp6 %>% mutate(caller = ifelse(str_detect(sample,"fb"), "FreeBayes", caller))
head(quasi_merged_caller)
#write.csv(quasi_merged_caller, file="snpcall_benchmark_MILK_merged_caller.txt")

add_new_cols <- c('replicate', 'rep_no', 'samp_desc', 'ID')
quasi_merged_caller[, add_new_cols] <- NA
head(quasi_merged_caller)

temp7 <- quasi_merged_caller %>% mutate(replicate = ifelse(str_detect(sample,"^01"), "Rep1", replicate))
temp8 <- temp7 %>% mutate(replicate = ifelse(str_detect(sample,"^02"), "Rep2", replicate))
temp9 <- temp8 %>% mutate(replicate = ifelse(str_detect(sample,"^03"), "Rep3", replicate))
temp10 <- temp9 %>% mutate(replicate = ifelse(str_detect(sample,"^04"), "Rep4", replicate))
head(temp10)

temp11 <- temp10 %>% mutate(rep_no = ifelse(str_detect(sample,"^01"), "01", rep_no))
temp12 <- temp11 %>% mutate(rep_no = ifelse(str_detect(sample,"^02"), "02", rep_no))
temp13 <- temp12 %>% mutate(rep_no = ifelse(str_detect(sample,"^03"), "03", rep_no))
quasi_merged_caller_rep <- temp13 %>% mutate(rep_no = ifelse(str_detect(sample,"^04"), "04", rep_no))
head(quasi_merged_caller_rep)

quasi_merged_caller_rep_filter_blanks <- quasi_merged_caller_rep %>% filter(!str_detect(sample,"blank"))
quasi_merged_caller_rep_filter_blanks %>% count(caller)
quasi_merged_caller_rep_filter_blanks %>% count(replicate)


quasi_merged_caller_rep_filter_blanks$sample2 <- quasi_merged_caller_rep_filter_blanks$sample

quasi_merged_caller_rep_filter_blanks_samp <- quasi_merged_caller_rep_filter_blanks %>% separate(sample2, c("rep_num", "samp_no"), extra='drop', remove=FALSE)
head(quasi_merged_caller_rep_filter_blanks_samp)

quasi_merged_caller_rep_filter_blanks_samp$samp_desc <- str_c(quasi_merged_caller_rep_filter_blanks_samp$samp_no, '_', quasi_merged_caller_rep_filter_blanks_samp$caller)
head(quasi_merged_caller_rep_filter_blanks_samp)


quasi_merged_caller_rep_filter_blanks_samp$ID <- str_c(quasi_merged_caller_rep_filter_blanks_samp$rep_num, '_', quasi_merged_caller_rep_filter_blanks_samp$samp_no)
head(quasi_merged_caller_rep_filter_blanks_samp)

Mix_ID_key <- fread("Mix_ID_key.txt")
head(Mix_ID_key)

quasi_merged_caller_rep_filter_blanks_samp_Mix <- left_join(quasi_merged_caller_rep_filter_blanks_samp, Mix_ID_key, by='ID')
head(quasi_merged_caller_rep_filter_blanks_samp_Mix)

snp_quasi_6VCs <- quasi_merged_caller_rep_filter_blanks_samp_Mix
head(snp_quasi_6VCs)

genome_mix_int <- fread("Mix_int_only_one_genome.txt")
# genome_mix_int <- fread("Mix_int_only_one_genome_38.txt")
head(genome_mix_int)

snp_quasi_6VCs_Mix <- snp_quasi_6VCs %>% filter(Mix %in% genome_mix_int$Mix)
head(snp_quasi_6VCs_Mix)

names(snp_quasi_6VCs_Mix)[10] <- "Replicates"
names(snp_quasi_6VCs_Mix)[16] <- "Sample"
head(snp_quasi_6VCs_Mix)

temp21 <- snp_quasi_6VCs_Mix %>% mutate(Sample = replace(Sample, Sample == 17, 2))
snp_quasi_6VCs_Mix <- temp21 %>% mutate(Sample = replace(Sample, Sample == 16, 3))

write.csv(snp_quasi_6VCs_Mix, file="snp_quasi_MILK_6VCs_mix-only_one_genome.txt")

jit <- position_jitter(seed=123)
point_plot <- ggplot(snp_quasi_6VCs_Mix, aes(precision, recall, shape=Sample, color=Replicates)) +
  geom_point(size=2.5) +
  theme(legend.position = 'right',
        legend.spacing.x = unit(0.2, 'cm'),
        text = element_text(size=8)) +
  geom_jitter(aes(shape=Sample, color=as.factor(Replicates)), size=1, position=jit) +
  #scale_shape_manual(values=rep(15:22, len=8)) + 
  facet_wrap(~caller)
point_plot + theme_bw(base_size=15)+theme(legend.position = "bottom")
ggsave("Final_MILK_Mix_Replicate_color_by_caller_600.tiff",units="in", width= 12, height = 6, device='tiff', dpi=600)
ggsave("Final_MILK_Mix_Replicate_color_by_caller_300.tiff",units="in", width= 12, height = 6, device='tiff', dpi=300)
ggsave("Final_MILK_Mix_Replicate_color_by_caller_300.png",units="in", width= 12, height = 6, device='png', dpi=300)


#box plotting with jitter only unique Mix sample by median values, colored by caller vs recall
point_plot <- ggplot(snp_quasi_6VCs_Mix, aes(recall, caller, color=caller)) +
  geom_boxplot() + geom_jitter()+
  theme(legend.spacing.x = unit(0.2, 'cm'), text = element_text(size=8)) +
  theme_bw(base_size=15) + coord_cartesian(xlim=c(0,1.0)) +
  theme(legend.position = "bottom")  
point_plot
ggsave("Final_MILK_only_color_by_caller_recall_300_box_jitterpoints-replicates.png",units="in", width= 12, height = 6, device='png', dpi=300)

#box plotting with jitter only unique Mix sample by median values, colored by caller vs precision
point_plot <- ggplot(snp_quasi_6VCs_Mix, aes(precision, caller, color=caller)) +
  geom_boxplot() + geom_jitter()+
  theme(legend.spacing.x = unit(0.2, 'cm'), text = element_text(size=8)) +
  theme_bw(base_size=15) + coord_cartesian(xlim=c(0,1.0)) +
  theme(legend.position = "bottom")  
point_plot
ggsave("Final_MILK_only_color_by_caller_pre_300_box_jitterpoints-replicates.png",units="in", width= 12, height = 6, device='png', dpi=300)
