library(data.table)
library(purrr)
library(dplyr)
library(ggplot2)
library(reshape2)
library(viridis)
library(stringr)
library(gtools)
library(stringr)
library(tidyr)
library(openxlsx)
library(wesanderson)
library(ggrepel)

{freq_data <- read.table("WWC105_ALL_vcfs_final.txt", sep='\t', strip.white = TRUE, fill = TRUE)
freq_data_group <- freq_data %>% mutate(group = cumsum(V1 =="Name"))
#head(freq_data_group)

File_name_list <- freq_data_group %>% filter(V1=="Name")
File_name_list <- subset(File_name_list, select=-c(V1))


#names(File_name_list)[1] <- "File_Name"
#'freq_data_group[, 'File_Name'] <- NA
# temp2 <- freq_data_group %>% mutate(File_Name = if_else(group == File_name_list$group), File_name_list$File_Name)

freq_data_group_2 <- left_join(freq_data_group, File_name_list, by='group')

names(freq_data_group_2)[1] <- "POS"
names(freq_data_group_2)[2] <- "Frequency"
names(freq_data_group_2)[4] <- "File_Name"

freq_data_group_3 <- freq_data_group_2 %>% filter(POS!="Name")
#head(freq_data_group_3)

freq_data_group_3[, 'caller'] <- NA


temp2 <- freq_data_group_3 %>% mutate(caller = ifelse(str_detect(File_Name,"VS"), "VarScan", caller))
temp3 <- temp2 %>% mutate(caller = ifelse(str_detect(File_Name,"bcf"), "BCFtools", caller))
temp4 <- temp3 %>% mutate(caller = ifelse(str_detect(File_Name,"lfq"), "LoFreq", caller))
temp5 <- temp4 %>% mutate(caller = ifelse(str_detect(File_Name,"ivar"), "iVar", caller))
temp6 <- temp5 %>% mutate(caller = ifelse(str_detect(File_Name,"GATK"), "GATK", caller))
freq_data_group_4 <- temp6 %>% mutate(caller = ifelse(str_detect(File_Name,"fb"), "FreeBayes", caller))
#head(freq_data_group_4)
#write.csv(quasi_merged_caller, file="snpcall_benchmark_DELTA_merged_caller.txt")
#freq_data_group_4 %>% count(caller)
freq_data_group_4[, 'Replicate'] <- NA
temp7 <- freq_data_group_4 %>% mutate(Replicate = ifelse(str_detect(File_Name,"^01"), "Rep1", Replicate))
temp8 <- temp7 %>% mutate(Replicate = ifelse(str_detect(File_Name,"^02"), "Rep2", Replicate))
temp9 <- temp8 %>% mutate(Replicate = ifelse(str_detect(File_Name,"^03"), "Rep3", Replicate))
temp10 <- temp9 %>% mutate(Replicate = ifelse(str_detect(File_Name,"^04"), "Rep4", Replicate))
#head(temp10)
temp10[, 'Rep_No'] <- NA
temp11 <- temp10 %>% mutate(Rep_No = ifelse(str_detect(File_Name,"^01"), "01", Rep_No))
temp12 <- temp11 %>% mutate(Rep_No = ifelse(str_detect(File_Name,"^02"), "02", Rep_No))
temp13 <- temp12 %>% mutate(Rep_No = ifelse(str_detect(File_Name,"^03"), "03", Rep_No))
temp14 <- temp13 %>% mutate(Rep_No = ifelse(str_detect(File_Name,"^04"), "04", Rep_No))
#head(temp14)

#temp14 %>% count(caller)
temp14[, 'Rep_Num'] <- NA
temp14[, 'Sample_No'] <- NA
temp15 <- temp14 %>% separate(File_Name, c("Rep_Num", "Sample_No"), extra='drop', remove=FALSE)

temp15[, 'Percent'] <- NA
#head(temp15)

freq_data_group_4 <- temp15 %>% mutate(Percent = as.numeric(Frequency)*100)
#head(freq_data_group_4)


freq_data_group_5 <- transform(freq_data_group_4, Sample_No = as.numeric(Sample_No))

Mix_ID <- fread("Match_Mix_ID.txt")
#head(Mix_ID)

freq_data_group_6 <- left_join(freq_data_group_5, Mix_ID, by='Sample_No')
#head(freq_data_group_6)


alpha <- fread("alpha.txt", header=TRUE)
beta <- fread("beta.txt", header=TRUE)
delta <- fread("delta.txt", header=TRUE)

alpha_aa <- fread("alpha_aa_change.txt", header=TRUE)
beta_aa <- fread("beta_aa_change.txt", header=TRUE)
delta_aa <- fread("delta_aa_change.txt", header=TRUE)

freq_data_group_6 <- transform(freq_data_group_6, POS = as.numeric(POS))

freq_data_group_7 <- left_join(freq_data_group_6, alpha_aa, by='POS')
temp20 <- freq_data_group_7 %>% filter(POS %in% alpha_aa$POS)

freq_data_group_8 <- left_join(freq_data_group_6, beta_aa, by='POS')
temp21 <- freq_data_group_8 %>% filter(POS %in% beta_aa$POS)

freq_data_group_9 <- left_join(freq_data_group_6, delta_aa, by='POS')
temp22 <- freq_data_group_9 %>% filter(POS %in% delta_aa$POS)
}

#jit <- position_jitter(seed=123)
point_plot <- ggplot(temp20, aes(x=factor(ID_manuscript, level =c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19)), y=factor(AA_change,level=c("T183I","A890D", "I1412T", "H69_del", "Y144_del", "N501Y", "A570D", "P681H", "T716I", "S982A", "D1118H", "Q27*", "R52I", "Y73C", "D3L")),color=Percent)) + 
  geom_point(size=2) +
  #scale_color_viridis("Allele Frequency (%)", direction=-1)+
  scale_colour_gradientn("ALT Allele Frequency (%)", colours=rainbow(4)) +
  #scale_color_gradient("Allele Frequency (%)", low="blue", high="red") +
  #geom_jitter(aes(color=as.numeric(Depth)), size=1) +
  #scale_shape_manual(values=rep(15:22, len=6))  +
  #theme(legend.spacing.x = unit(0.2, 'cm'), text = element_text(size=8)) +
  #theme_bw(base_size=15) + theme(legend.position = "none") + 
  labs(x='Synthetic Samples', y='Alpha mutations (amino acid)') + facet_wrap(~caller) + theme(legend.position = "bottom",axis.title=element_text(size = 14), axis.text = element_text(size=8), axis.text.x.bottom = element_text(vjust=0.5))
#+ scale_x_discrete(guide=guide_axis(n.dodge=3))
point_plot 
ggsave("WWC105_Mix_Alpha_freq_plot-gradient-Rainbow-facet-1.png",units="in", width= 12, height = 6, device='png', dpi=300)


point_plot <- ggplot(temp21, aes(x=factor(ID_manuscript, level =c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19)), y=factor(AA_change,level=c("T85I","K837N","K90R","11287-del","D80A","D215G","22280-del","K417N","E484K","N501Y","A701V","Q57H","S171L","P71L","T205I")),color=Percent)) +
  geom_point(size=2) +
  #scale_color_viridis("Allele Frequency (%)", direction=-1)+
  scale_colour_gradientn("ALT Allele Frequency (%)", colours=rainbow(4))  +
  #scale_color_gradient(low="light blue", high="red") +
  #geom_jitter(aes(color=as.numeric(Depth), shape=caller), size=2, position=jit) +
  #scale_shape_manual(values=rep(0:5, len=6))  +
  #theme(legend.spacing.x = unit(0.2, 'cm'), text = element_text(size=8)) +
  #theme_bw(base_size=15) + theme(legend.position = "none") +
  labs(x='Synthetic Samples', y='Beta mutations (amino acid)') + facet_wrap(~caller) + theme(legend.position = "bottom", axis.title=element_text(size = 14), axis.text = element_text(size=8), axis.text.x.bottom = element_text(vjust=0.5))
point_plot 
#+ scale_x_discrete(guide=guide_axis(n.dodge=3)) 
ggsave("WWC105_Mix_Beta_freq_plot-gradient-Rainbow-facet-1.png",units="in", width= 12, height = 6, device='png', dpi=300)

point_plot <- ggplot(temp22, aes(x=factor(ID_manuscript, level =c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19)), y=factor(AA_change,level=c("T19R","L452R","T478K","P681R","D950N","T77A","S26L","I82T","V82A","T120I","D63G","R203M","D377Y")),color=Percent)) +
  geom_point(size=2) +
  #scale_color_viridis("Allele Frequency (%)", direction=-1)+
  scale_colour_gradientn("ALT Allele Frequency (%)", colours=rainbow(4))  +
  #scale_color_gradient(low="light blue", high="red") +
  #geom_jitter(aes(color=as.numeric(Depth), shape=caller), size=2, position=jit) +
  #scale_shape_manual(values=rep(0:5, len=6))  +
  #theme(legend.spacing.x = unit(0.2, 'cm'), text = element_text(size=8)) +
  #theme_bw(base_size=15) + theme(legend.position = "none") +
  labs(x='Synthetic Samples', y='Delta mutations (amino acid)') + facet_wrap(~caller) + theme(legend.position = "bottom", axis.title=element_text(size = 14), axis.text = element_text(size=8), axis.text.x.bottom = element_text(vjust=0.5)) #+ scale_x_discrete(guide=guide_axis(n.dodge=3))
point_plot 
ggsave("WWC105_Mix_Delta_freq_plot-gradient-Rainbow-facet-1.png",units="in", width= 12, height = 6, device='png', dpi=300)
