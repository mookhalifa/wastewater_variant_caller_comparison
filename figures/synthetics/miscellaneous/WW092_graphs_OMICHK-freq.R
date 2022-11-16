library(tidyverse)
library(gtools)
library(data.table)
library(stringr)
library(tidyr)
library(ggrepel)
library(openxlsx)

rm(list=ls())

# read in the data 
ww_quasi <- fread("snpcall_benchmark_OMICHK.txt")
ww_quasi_1 <- fread("snpcall_benchmark_OMICHK_1.txt")
head(ww_quasi)
head(ww_quasi_1)
ww_quasi_filtered <-  ww_quasi %>% filter(!str_detect(caller,"GATK"))

write.csv(ww_quasi_filtered, file="snpcall_benchmark_OMICHK_1_filtered.txt")

quasi_merged <- rbind(ww_quasi_filtered, ww_quasi_1)
names(quasi_merged)[1] <- "sample"
head(quasi_merged)
write.csv(quasi_merged, file="snpcall_benchmark_OMICHK_merged.txt")
write.xlsx(quasi_merged, "snpcall_benchmark_OMICHK_merged.xlsx")

# # rename column "caller" to "sample"
# names(ww_quasi)[1] <- "sample"
# head(ww_quasi)

#Add two new columns for caller and samp_code and fill with NA
quasi_merged[, 'caller'] <- NA
quasi_merged[, 'samp_code'] <- NA

#populate the two new columns based on information from the sample (type of caller and sample code)

temp2 <- quasi_merged %>% mutate(caller = ifelse(str_detect(sample,"VS"), "VarScan", caller))
temp3 <- temp2 %>% mutate(caller = ifelse(str_detect(sample,"bcf"), "BCFtools", caller))
temp4 <- temp3 %>% mutate(caller = ifelse(str_detect(sample,"lfq"), "LoFreq", caller))
temp5 <- temp4 %>% mutate(caller = ifelse(str_detect(sample,"ivar"), "iVar", caller))
temp6 <- temp5 %>% mutate(caller = ifelse(str_detect(sample,"GATK"), "GATK", caller))
temp7 <- temp6 %>% mutate(caller = ifelse(str_detect(sample,"fb"), "FreeBayes", caller))
temp8 <- temp7 %>% mutate(samp_code = ifelse(str_detect(sample,"S42"), "S42", samp_code))
temp9 <- temp8 %>% mutate(samp_code = ifelse(str_detect(sample,"S43"), "S43", samp_code))
temp10 <- temp9 %>% mutate(samp_code = ifelse(str_detect(sample,"S50"), "S50", samp_code))
temp11 <- temp10 %>% mutate(samp_code = ifelse(str_detect(sample,"S58"), "S58", samp_code))
temp12 <- temp11 %>% mutate(samp_code = ifelse(str_detect(sample,"S59"), "S59", samp_code))
temp13 <- temp12 %>% mutate(samp_code = ifelse(str_detect(sample,"S263"), "S263", samp_code))
temp14 <- temp13 %>% mutate(samp_code = ifelse(str_detect(sample,"S296"), "S296", samp_code))
temp15 <- temp14 %>% mutate(samp_code = ifelse(str_detect(sample,"S292"), "S292", samp_code))
temp16 <- temp15 %>% mutate(samp_code = ifelse(str_detect(sample,"S270"), "S270", samp_code))
temp17 <- temp16 %>% mutate(samp_code = ifelse(str_detect(sample,"S278"), "S278", samp_code))
temp18 <- temp17 %>% mutate(samp_code = ifelse(str_detect(sample,"S302"), "S302", samp_code))
temp19 <- temp18 %>% mutate(samp_code = ifelse(str_detect(sample,"S305"), "S305", samp_code))
ww_quasi_caller <- temp19 %>% mutate(samp_code = ifelse(str_detect(sample,"S63"), "S63", samp_code))
head(ww_quasi_caller)


avg_freq <- fread("WWC092_samp_code_mean_frequency.txt")


ww_quasi_caller[, 'samp_desc'] <- NA

ww_quasi_caller$samp_desc <- str_c(ww_quasi_caller$samp_code, '_', ww_quasi_caller$caller)
head(ww_quasi_caller)

avg_freq_unique <- avg_freq[!duplicated(avg_freq$samp_desc), ]
avg_freq_unique %>% count(samp_desc)

avg_freq_unique_selected <- avg_freq_unique %>% select (c(samp_desc, Avg_Freq))

ww_quasi_caller_1 <- left_join(ww_quasi_caller, avg_freq_unique_selected, by='samp_desc')






#plotting all the samples, colored by caller
point_plot <- ggplot(ww_quasi_caller, aes(precision, recall, color=caller)) +
  geom_point(size=2.5) +
  theme(legend.spacing.x = unit(0.2, 'cm'),
        text = element_text(size=8)) +
  theme_bw(base_size=15) + coord_cartesian(xlim=c(0,1.0), ylim=c(0,1.0)) + theme(legend.position = "bottom")
point_plot 
ggsave("WW092_OMICHK_color_by_caller_600_1.tiff",units="in", width= 12, height = 6, device='tiff', dpi=600)
ggsave("WW092_OMICHK_color_by_caller_300_1.tiff",units="in", width= 12, height = 6, device='tiff', dpi=300)
ggsave("WW092_OMICHK_color_by_caller_600_1.pdf",units="in", width= 12, height = 6, device='pdf', dpi=600)
ggsave("WW092_OMICHK_color_by_caller_300_1.png",units="in", width= 12, height = 6, device='png', dpi=300)

#plotting all the samples, grouped by caller with trend line
point_plot <- ggplot(ww_quasi_caller_1, aes(precision, recall, color=Avg_Freq)) +
  geom_point(size=2.5) +
  scale_color_gradient(low="blue", high="red") +
  theme(legend.spacing.x = unit(0.2, 'cm'), text = element_text(size=8)) +
  theme_bw(base_size=15) + coord_cartesian(xlim=c(0,1.0), ylim=c(0,1.0)) +
  theme(legend.position = "bottom") + facet_wrap(~caller) 
point_plot
ggsave("WW092_OMICHK_color_by_caller_multi_600-freq.tiff",units="in", width= 12, height = 6, device='tiff', dpi=600)
ggsave("WW092_OMICHK_color_by_caller_multi_300-freq.tiff",units="in", width= 12, height = 6, device='tiff', dpi=300)
ggsave("WW092_OMICHK_color_by_caller_multi_600-freq.pdf",units="in", width= 12, height = 6, device='pdf', dpi=600)
ggsave("WW092_OMICHK_color_by_caller_multi_300-freq.png",units="in", width= 12, height = 6, device='png', dpi=300)

#plotting all the samples, grouped by caller with trend with label() sample_code)
point_plot <- ggplot(ww_quasi_caller_1, aes(precision, recall, color=Avg_Freq, label=samp_code)) +
  geom_point(size=2.5) +
  scale_color_gradient(low="blue", high="red") +
  theme(legend.spacing.x = unit(0.2, 'cm'), text = element_text(size=8)) +
  theme_bw(base_size=15) + coord_cartesian(xlim=c(0,1.0), ylim=c(0,1.0)) +
  theme(legend.position = "bottom") + facet_wrap(~caller) 
point_plot + geom_text_repel()
ggsave("WW092_OMICHK_color_by_caller_multi_label_600-freq.tiff",units="in", width= 12, height = 6, device='tiff', dpi=600)
ggsave("WW092_OMICHK_color_by_caller_multi_label_300-freq.tiff",units="in", width= 12, height = 6, device='tiff', dpi=300)
ggsave("WW092_OMICHK_color_by_caller_multi_label_600-freq.pdf",units="in", width= 12, height = 6, device='pdf', dpi=600)
ggsave("WW092_OMICHK_color_by_caller_multi_label_300-freq.png",units="in", width= 12, height = 6, device='png', dpi=300)

#box plotting with jitter only unique Mix sample by median values, colored by caller vs recall
point_plot <- ggplot(ww_quasi_caller, aes(recall, caller, color=caller)) +
  geom_boxplot() + geom_jitter()+
  theme(legend.spacing.x = unit(0.2, 'cm'), text = element_text(size=8)) +
  theme_bw(base_size=15) + coord_cartesian(xlim=c(0,1.0)) +
  theme(legend.position = "bottom")  
point_plot
ggsave("WW092_OMICHK_color_by_caller_recall_300_box_jitterpoints.png",units="in", width= 12, height = 6, device='png', dpi=300)

#box plotting with jitter only unique Mix sample by median values, colored by caller vs precision
point_plot <- ggplot(ww_quasi_caller, aes(precision, caller, color=caller)) +
  geom_boxplot() + geom_jitter()+
  theme(legend.spacing.x = unit(0.2, 'cm'), text = element_text(size=8)) +
  theme_bw(base_size=15) + coord_cartesian(xlim=c(0,1.0)) +
  theme(legend.position = "bottom")  
point_plot
ggsave("WW092_OMICHK_color_by_caller_pre_300_box_jitterpoints.png",units="in", width= 12, height = 6, device='png', dpi=300)













#first tried with OMIC_Hongkong smaples--these plot are not needed
# #plotting all the samples, colored by caller with altered coordinates
# point_plot <- ggplot(ww_quasi_caller, aes(precision, recall, color=caller)) +
#   geom_point(size=2.5) +
#   theme(legend.spacing.x = unit(0.2, 'cm'),
#         text = element_text(size=8)) +
#   theme_bw(base_size=15) + coord_cartesian(xlim=c(0,0.5), ylim=c(0,0.75)) + theme(legend.position = "bottom")
# point_plot 
# ggsave("WW092_omic-hk_color_by_caller_600_5.tiff",units="in", width= 12, height = 6, device='tiff', dpi=600)
# ggsave("WW092_omic-hk_color_by_caller_300_5.tiff",units="in", width= 12, height = 6, device='tiff', dpi=300)
# ggsave("WW092_omic-hk_color_by_caller_600_5.pdf",units="in", width= 12, height = 6, device='pdf', dpi=600)
# ggsave("WW092_omic-hk_color_by_caller_300_5.png",units="in", width= 12, height = 6, device='png', dpi=300)

# #plotting all the samples, colored by caller with altered coordinates
# point_plot <- ggplot(ww_quasi_caller, aes(precision, recall, color=caller)) +
#   geom_point(size=2.5) +
#   theme(legend.spacing.x = unit(0.2, 'cm'),
#         text = element_text(size=8)) +
#   theme_bw(base_size=15) + coord_cartesian(xlim=c(0,0.2), ylim=c(0,0.4)) + theme(legend.position = "bottom")
# point_plot 
# ggsave("WW092_omic-hk_color_by_caller_600_4.tiff",units="in", width= 12, height = 6, device='tiff', dpi=600)
# ggsave("WW092_omic-hk_color_by_caller_300_4.tiff",units="in", width= 12, height = 6, device='tiff', dpi=300)
# ggsave("WW092_omic-hk_color_by_caller_600_4.pdf",units="in", width= 12, height = 6, device='pdf', dpi=600)
# ggsave("WW092_omic-hk_color_by_caller_300_4.png",units="in", width= 12, height = 6, device='png', dpi=300)
# #plotting only the Mix sample by median values, colored by caller with trend
# point_plot <- ggplot(ww_quasi_caller, aes(precision, recall, color=caller)) +
#   geom_point(size=2.5) +
#   theme(legend.spacing.x = unit(0.2, 'cm'),
#         text = element_text(size=8)) +
#   theme_bw(base_size=15) + coord_cartesian(xlim=c(0,0.2), ylim=c(0,0.4)) + theme(legend.position = "bottom")
# point_plot 
# ggsave("WW092_omic-hk_color_by_caller_600_4.tiff",units="in", width= 12, height = 6, device='tiff', dpi=600)
# ggsave("WW092_omic-hk_color_by_caller_300_4.tiff",units="in", width= 12, height = 6, device='tiff', dpi=300)
# ggsave("WW092_omic-hk_color_by_caller_600_4.pdf",units="in", width= 12, height = 6, device='pdf', dpi=600)
# ggsave("WW092_omic-hk_color_by_caller_300_4.png",units="in", width= 12, height = 6, device='png', dpi=300)

