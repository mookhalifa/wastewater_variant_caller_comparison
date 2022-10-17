library(tidyverse)
library(gtools)
library(data.table)
library(stringr)
library(tidyr)
library(openxlsx)

#working modified on 05/05/22 to replace sample number instead of Mix names in the third plot

# read in the data 

quasi_1 <- fread("snpcall_benchmark_P1_1.txt")
quasi_2 <- fread("snpcall_benchmark_P1_2.txt")
quasi_3 <- fread("snpcall_benchmark_P1_3.txt")

head(quasi_1)
head(quasi_2)
head(quasi_3)

temp  <-  quasi_1 %>% filter(!str_detect(caller,"fb_def"))
temp1 <-  temp %>% filter(!str_detect(caller,"lfq"))
temp2 <-  temp1 %>% filter(!str_detect(caller,"GATK"))
quasi_1_filtered <-  temp2 %>% filter(!str_detect(caller,"VS"))
write.csv(quasi_1_filtered, file="snpcall_benchmark_P1_1_filtered.txt")

quasi_merged <- rbind(quasi_1_filtered, quasi_2, quasi_3)
names(quasi_merged)[1] <- "sample"
head(quasi_merged)
write.csv(quasi_merged, file="snpcall_benchmark_P1_merged.txt")
write.xlsx(quasi_merged, "snpcall_benchmark_P1_merged.xlsx")

quasi_merged[, 'caller'] <- NA

temp2 <- quasi_merged %>% mutate(caller = ifelse(str_detect(sample,"VS"), "VarScan", caller))
temp3 <- temp2 %>% mutate(caller = ifelse(str_detect(sample,"bcf"), "BCFtools", caller))
temp4 <- temp3 %>% mutate(caller = ifelse(str_detect(sample,"lfq"), "LoFreq", caller))
temp5 <- temp4 %>% mutate(caller = ifelse(str_detect(sample,"ivar"), "iVar", caller))
temp6 <- temp5 %>% mutate(caller = ifelse(str_detect(sample,"GATK"), "GATK", caller))
quasi_merged_caller <- temp6 %>% mutate(caller = ifelse(str_detect(sample,"fb"), "FreeBayes", caller))
head(quasi_merged_caller)
write.csv(quasi_merged_caller, file="snpcall_benchmark_P1_merged_caller.txt")
quasi_merged_caller %>% count(caller)

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
quasi_merged_caller_rep %>% count(caller)
quasi_merged_caller_rep %>% count(replicate)

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

# rename precision and recall
names(snp_quasi_6VCs)[6] <- "pre"
names(snp_quasi_6VCs)[7] <- "re"
head(snp_quasi_6VCs)

# calculate median and group the data by samp_desc
snp_quasi_6VCs_median <- snp_quasi_6VCs %>% group_by(samp_desc) %>% summarize(med_genomediff = median(genomediff), med_calleridentify = median(calleridentify), med_TP=median(TP), med_FP=median(FP), precision = median(pre), recall = median(re), med_f1 = median(f1), Mix=Mix, caller=caller, ALPHA=ALPHA, BETA=BETA, DELTA=DELTA, DELTAAY2=DELTAAY2)
head(snp_quasi_6VCs_median)
write.csv(snp_quasi_6VCs_median, file="snp_quasi_P1_6VCs_median.txt")


################################Selected Mix for further analysis ##########################################################
ggplot(snp_quasi_6VCs_median_Mix4, aes(precision, recall, color=caller)) +
  geom_point(size=2.5) +
  theme(legend.spacing.x = unit(0.2, 'cm'), text = element_text(size=8)) +
  theme_bw(base_size=15) + coord_cartesian(xlim=c(0,1.0), ylim=c(0,1.25)) +
  theme(legend.position = "bottom") + geom_smooth(method=lm, size=0.5) + facet_wrap(~caller)

mix_int <- fread("Mix_interested.txt")
mix_int
snp_quasi_6VCs_median_Mix <- snp_quasi_6VCs_median %>% filter(Mix %in% mix_int$Mix)
head(snp_quasi_6VCs_median_Mix)
write.csv(snp_quasi_6VCs_median_Mix, file="snp_quasi_P1_6VCs_median_mix.txt")

snp_quasi_6VCs_median_Mix3 <- snp_quasi_6VCs_median_Mix %>% separate(samp_desc, c("samp_ID", "caller2"), extra='drop', remove=FALSE)
head(snp_quasi_6VCs_median_Mix3)

snp_quasi_6VCs_median_Mix4 <- left_join(snp_quasi_6VCs_median_Mix3, mix_int, by='Mix')
head(snp_quasi_6VCs_median_Mix4)

# select only the unique values in samp_desc, to use in box_plot with jitter
snp_quasi_6VCs_median_Mix4_unique <- snp_quasi_6VCs_median_Mix4[!duplicated(snp_quasi_6VCs_median_Mix4$samp_desc), ]
snp_quasi_6VCs_median_Mix4_unique %>% count(samp_desc)

write.table(snp_quasi_6VCs_median_Mix4_unique, file="P1_snp_quasi_6VCs_median_Mix4_unique.txt", sep="\t")

#plotting only the Mix sample by median values, colored by caller with trend
point_plot <- ggplot(snp_quasi_6VCs_median_Mix4, aes(precision, recall, color=caller)) +
  geom_point(size=2.5) +
  theme(legend.spacing.x = unit(0.2, 'cm'), text = element_text(size=8)) +
  theme_bw(base_size=15) + coord_cartesian(xlim=c(0,1.0), ylim=c(0,1.25)) +
  theme(legend.position = "bottom") + geom_smooth(method=lm, size=0.5) 
point_plot
ggsave("Final_P1_Mix_Median_color_by_caller_trend_600.tiff",units="in", width= 12, height = 6, device='tiff', dpi=600)
ggsave("Final_P1_Mix_Median_color_by_caller_trend_300.tiff",units="in", width= 12, height = 6, device='tiff', dpi=300)
ggsave("Final_P1_Mix_Median_color_by_caller_trend_300.png",units="in", width= 12, height = 6, device='png', dpi=300)

#plotting only the Mix sample by median values, faceted and colored by caller with trend
point_plot <- ggplot(snp_quasi_6VCs_median_Mix4, aes(precision, recall)) +
  geom_point(size=2.5, color="blue") +
  theme(legend.spacing.x = unit(0.2, 'cm'), text = element_text(size=8)) +
  theme_bw(base_size=15) + coord_cartesian(xlim=c(0,1.0), ylim=c(0,1.25)) +
  theme(legend.position = "bottom") + geom_smooth(method=lm, size=0.5) + facet_wrap(~caller)
point_plot
ggsave("Final_P1_Mix_Median_gp_by_caller_trend_600-blue.tiff",units="in", width= 12, height = 6, device='tiff', dpi=600)
ggsave("Final_P1_Mix_Median_gp_by_caller_trend_300-blue.tiff",units="in", width= 12, height = 6, device='tiff', dpi=300)
ggsave("Final_P1_Mix_Median_gp_by_caller_trend_300-blue.png",units="in", width= 12, height = 6, device='png', dpi=300)


#plotting only the Mix sample by median values, colored by caller vs recall
point_plot <- ggplot(snp_quasi_6VCs_median_Mix4_unique, aes(recall, caller, color=caller)) +
  geom_point(size=2.5) +
  theme(legend.spacing.x = unit(0.2, 'cm'), text = element_text(size=8)) +
  theme_bw(base_size=15) + coord_cartesian(xlim=c(0,1.0)) +
  theme(legend.position = "bottom")  
point_plot
ggsave("Final_P1_Mix_Median_color_by_caller_recall_600.tiff",units="in", width= 12, height = 6, device='tiff', dpi=600)
ggsave("Final_P1_Mix_Median_color_by_caller_recall_300.tiff",units="in", width= 12, height = 6, device='tiff', dpi=300)
ggsave("Final_P1_Mix_Median_color_by_caller_recall_300.png",units="in", width= 12, height = 6, device='png', dpi=300)

#plotting only the Mix sample by median values, colored by caller vs precision
point_plot <- ggplot(snp_quasi_6VCs_median_Mix4, aes(precision, caller, color=caller)) +
  geom_point(size=2.5) +
  theme(legend.spacing.x = unit(0.2, 'cm'), text = element_text(size=8)) +
  theme_bw(base_size=15) + coord_cartesian(xlim=c(0,1.0)) +
  theme(legend.position = "bottom")  
point_plot
ggsave("Final_P1_Mix_Median_color_by_caller_pre_600.tiff",units="in", width= 12, height = 6, device='tiff', dpi=600)
ggsave("Final_P1_Mix_Median_color_by_caller_pre_300.tiff",units="in", width= 12, height = 6, device='tiff', dpi=300)
ggsave("Final_P1_Mix_Median_color_by_caller_pre_300.png",units="in", width= 12, height = 6, device='png', dpi=300)

#box plotting only the Mix sample by median values, colored by caller vs recall
# point_plot <- ggplot(snp_quasi_6VCs_median_Mix4, aes(recall, caller, color=caller)) +
#   geom_boxplot() +
#   theme(legend.spacing.x = unit(0.2, 'cm'), text = element_text(size=8)) +
#   theme_bw(base_size=15) + coord_cartesian(xlim=c(0,1.0)) +
#   theme(legend.position = "bottom")  
# point_plot 
# ggsave("Final_P1_Mix_Median_color_by_caller_recall_300_box.png",units="in", width= 12, height = 6, device='png', dpi=300)

#box plotting with jitter only unique Mix sample by median values, colored by caller vs recall
point_plot <- ggplot(snp_quasi_6VCs_median_Mix4_unique, aes(recall, caller, color=caller)) +
  geom_boxplot() + geom_jitter()+
  theme(legend.spacing.x = unit(0.2, 'cm'), text = element_text(size=8)) +
  theme_bw(base_size=15) + coord_cartesian(xlim=c(0,1.0)) +
  theme(legend.position = "bottom")  
point_plot
ggsave("Final_P1_Mix_Median_color_by_caller_recall_300_box_jitterpoints.png",units="in", width= 12, height = 6, device='png', dpi=300)

#box plotting with jitter only unique Mix sample by median values, colored by caller vs precision
point_plot <- ggplot(snp_quasi_6VCs_median_Mix4_unique, aes(precision, caller, color=caller)) +
  geom_boxplot() + geom_jitter()+
  theme(legend.spacing.x = unit(0.2, 'cm'), text = element_text(size=8)) +
  theme_bw(base_size=15) + coord_cartesian(xlim=c(0,1.0)) +
  theme(legend.position = "bottom")  
point_plot
ggsave("Final_P1_Mix_Median_color_by_caller_pre_300_box_jitterpoints.png",units="in", width= 12, height = 6, device='png', dpi=300)

#plotting only the Mix samples by median values colour gradient by alpha concentration in multi by caller with geom trend line
jit <- position_jitter(seed=123)
point_plot <- ggplot(snp_quasi_6VCs_median_Mix4, aes(precision, recall, color=caller)) + 
  # scale_color_gradient(low="blue", high="red") +
  geom_point(size=2.5) + 
  theme(legend.spacing.x = unit(0.2, 'cm'), text = element_text(size=8)) + 
  theme_bw(base_size=15) +
  #geom_jitter(aes(color=ALPHA), size=2.5, position=jit) + 
  # scale_shape_manual(values=rep(15:22, len=6)) + 
  geom_smooth(method=lm, size=0.5) + facet_wrap(~caller) + 
  coord_cartesian(xlim=c(0,1.0), ylim=c(0,1.0)) + theme(legend.position = "bottom")
point_plot
ggsave("Final_P1_Mix_Median_BR_grad_color_by_caller_trend_group_by_color_300.png",units="in", width= 12, height = 6, device='png', dpi=300)
ggsave("Final_P1_Mix_Median_BR_grad_color_by_caller_trend_group_by_color_600.tiff",units="in", width= 12, height = 6, device='tiff', dpi=600)
ggsave("Final_P1_Mix_Median_BR_grad_color_by_caller_trend_group_by_color_300.tiff",units="in", width= 12, height = 6, device='tiff', dpi=300)


#plotting only the Mix samples by median values colour caller by alpha concentration in multi by Mix
point_plot <- ggplot(snp_quasi_6VCs_median_Mix4, aes(precision, recall, color=caller)) + 
  geom_point(size=2.5) + 
  theme(legend.spacing.x = unit(0.2, 'cm'), text = element_text(size=12), axis.title=element_text(size=16)) + 
  geom_jitter(aes(color=caller), size=2.5, position=jit) + 
  scale_shape_manual(values=rep(15:22, len=6)) + facet_wrap(facets = ~fct_reorder(as.factor(ID2), -(ALPHA), .desc=TRUE)) + coord_cartesian(xlim=c(0,1.0), ylim=c(0,1.0)) +
  theme_bw(base_size=10)
point_plot + theme(legend.position = "bottom")
ggsave("Final_P1_Mix_Median_color_by_caller_group_by_Mix_ID2_sorted_300.png",units="in", width= 12, height = 6, device='png', dpi=300)
ggsave("Final_P1_Mix_Median_color_by_caller_group_by_Mix_ID2_sorted_600.tiff",units="in", width= 12, height = 6, device='tiff', dpi=600)
ggsave("Final_P1_Mix_Median_color_by_caller_group_by_Mix_ID2_sorted_300.tiff",units="in", width= 12, height = 6, device='tiff', dpi=300)
