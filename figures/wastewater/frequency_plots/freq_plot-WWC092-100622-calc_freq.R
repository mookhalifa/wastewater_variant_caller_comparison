library(data.table)
library(purrr)
library(dplyr)
library(ggplot2)
library(reshape2)
library(viridis)
library(stringr)
library(gtools)
library(gridExtra)
library(tidyr)
library(ggrepel)

freq_data <- read.table("WWC092_ALL_vcfs_final.txt", sep='\t', strip.white = TRUE, fill = TRUE)
freq_data_group <- freq_data %>% mutate(group = cumsum(V1 =="Name"))
head(freq_data_group)

File_name_list <- freq_data_group %>% filter(V1=="Name")
File_name_list <- subset(File_name_list, select=-c(V1))


#names(File_name_list)[1] <- "File_Name"
#'freq_data_group[, 'File_Name'] <- NA
# temp2 <- freq_data_group %>% mutate(File_Name = if_else(group == File_name_list$group), File_name_list$File_Name)

freq_data_group_2 <- left_join(freq_data_group, File_name_list, by='group')
head(freq_data_group_2)

names(freq_data_group_2)[1] <- "POS"
names(freq_data_group_2)[2] <- "Frequency"
names(freq_data_group_2)[4] <- "sample"
head(freq_data_group_2)

freq_data_group_3 <- freq_data_group_2 %>% filter(POS!="Name")
head(freq_data_group_3)

freq_data_group_3[, 'caller'] <- NA
freq_data_group_3[, 'samp_code'] <- NA

temp2 <- freq_data_group_3 %>% mutate(caller = ifelse(str_detect(sample,"VS"), "VarScan", caller))
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
freq_data_group_4 <- temp19 %>% mutate(samp_code = ifelse(str_detect(sample,"S63"), "S63", samp_code))
head(freq_data_group_4)

ww_code <- fread("ww_code.txt", header=TRUE)

ww_code

freq_data_group_4 <- left_join(freq_data_group_4, ww_code, by='samp_code')

freq_data_group_4[, 'samp_desc'] <- NA

freq_data_group_4$samp_desc <- str_c(freq_data_group_4$samp_code, '_', freq_data_group_4$caller)
head(freq_data_group_4)

freq_data_group_42 <- freq_data_group_4 %>% group_by(samp_desc) %>% summarise(sample=sample, samp_code=samp_code, caller=caller, Wastewater=Wastewater, group=group, samp_desc=samp_desc, Avg_Freq = mean(as.numeric(Frequency)))
head(freq_data_group_42)
write.csv(freq_data_group_42, file="WWC092_samp_code_mean_frequency.txt")


BA1_aa <- fread("OmicronBA1_aa_change.txt", header=TRUE)
BA2_aa <- fread("OmicronBA2_aa_change.txt", header=TRUE)
delta_aa <- fread("delta_aa_change.txt", header=TRUE)





freq_data_group_6 <- transform(freq_data_group_4, POS = as.numeric(POS))

freq_data_group_7 <- left_join(freq_data_group_6, BA1_aa, by='POS')
temp20 <- freq_data_group_7 %>% filter(POS %in% BA1_aa$POS)





freq_data_group_8 <- left_join(freq_data_group_6, BA2_aa, by='POS')
temp21 <- freq_data_group_8 %>% filter(POS %in% BA2_aa$POS)

freq_data_group_9 <- left_join(freq_data_group_6, delta_aa, by='POS')
temp22 <- freq_data_group_9 %>% filter(POS %in% delta_aa$POS)


point_plot <- ggplot(temp20, aes(x=factor(samp_code, level =c("S59","S42","S63","S50","S58","S43","S296","S263","S302","S292","S278","S305","S270")), y=factor(AA_change,level=c("K38R","5386_syn","A1892T","I189V","13195_syn","I42V","S373P","G446S","G496S","T547K","N679K","N856K","N969K","L981F","D3G","Q19E","27259_syn")),color=as.numeric(Frequency)))+ 
  geom_point(size=2) + scale_y_discrete(drop=FALSE) +
  #scale_color_viridis("ALT Allele Frequency (%)", direction=-1)+
  scale_colour_gradientn("Allele Frequency (%)", colours=rainbow(4)) +
  #scale_color_gradient("Allele Frequency (%)", low="blue", high="red") +
  #geom_jitter(aes(color=as.numeric(Depth)), size=1) +
  #scale_shape_manual(values=rep(15:22, len=6))  +
  #theme(legend.spacing.x = unit(0.2, 'cm'), text = element_text(size=8)) +
  #theme_bw(base_size=15) + theme(legend.position = "none") + 
  labs(x='Wastewater Samples', y='Omicron BA.1 mutations') + facet_wrap(~caller) +
  theme(legend.position = "bottom", axis.title=element_text(size = 14), axis.text = element_text(size = 8), axis.text.x.bottom = element_text(vjust=0.5,angle = 45))
point_plot
#+ scale_x_discrete(guide=guide_axis(n.dodge=3))
ggsave("WWC092_BA1_freq_plot-gradient-Rainbow-aa_change-facet-1.png",units="in", width= 12, height = 6, device='png', dpi=300)

point_plot <- ggplot(temp21, aes(x=factor(samp_code, level =c("S59","S42","S63","S50","S58","S43","S296","S263","S302","S292","S278","S305","S270")), y=factor(AA_change,level=c("S135R","T24I","G489S","9424_syn","T327I","L438F","10198_syn","10447_syn","I42V","T19I","V213G","S373P","T376A","D405N","R408S","N679K","N969K","Q19E","27259_syn","S413R")),color=as.numeric(Frequency)))+ 
  geom_point(size=2) + scale_y_discrete(drop=FALSE) +
  #scale_color_viridis("Allele Frequency (%)", direction=-1)+
  scale_colour_gradientn("ALT Allele Frequency (%)", colours=rainbow(4)) +
  #scale_color_gradient("Allele Frequency (%)", low="blue", high="red") +
  #geom_jitter(aes(color=as.numeric(Depth)), size=1) +
  #scale_shape_manual(values=rep(15:22, len=6))  +
  #theme(legend.spacing.x = unit(0.2, 'cm'), text = element_text(size=8)) +
  #theme_bw(base_size=15) + theme(legend.position = "none") + 
  labs(x='Wastewater Samples', y='Omicron BA.2 mutations')
point_plot + facet_wrap(~caller) + theme(legend.position = "bottom", axis.title=element_text(size = 14), axis.text = element_text(size = 8), axis.text.x.bottom = element_text(vjust=0.5, angle = 45))
#+ scale_x_discrete(guide=guide_axis(n.dodge=3))
ggsave("WWC092_BA2_freq_plot-gradient-Rainbow-aa_change-facet-1.png",units="in", width= 12, height = 6, device='png', dpi=300)

point_plot <- ggplot(temp22, aes(x=factor(samp_code, level =c("S59","S42","S63","S50","S58","S43","S296","S263","S302","S292","S278","S305","S270")), y=factor(AA_change,level=c("T19R","L452R","T478K","P681R","D950N","T77A","S26L","I82T","V82A","T120I","D63G","R203M","D377Y")),color=as.numeric(Frequency)))+ 
  geom_point(size=2) + scale_y_discrete(drop=FALSE) +
  #scale_color_viridis("Allele Frequency (%)", direction=-1)+
  scale_colour_gradientn("ALT Allele Frequency (%)", colours=rainbow(4)) +
  #scale_color_gradient("Allele Frequency (%)", low="blue", high="red") +
  #geom_jitter(aes(color=as.numeric(Depth)), size=1) +
  #scale_shape_manual(values=rep(15:22, len=6))  +
  #theme(legend.spacing.x = unit(0.2, 'cm'), text = element_text(size=8)) +
  #theme_bw(base_size=15) + theme(legend.position = "none") + 
  labs(x='Wastewater Samples', y='\nDelta mutations (amino acid)') +
  facet_wrap(~caller) + theme(legend.position = "bottom", axis.title=element_text(size = 14), axis.text = element_text(size=8), axis.text.x.bottom = element_text(vjust = 0.5, angle = 45))
point_plot
#+ scale_x_discrete(guide=guide_axis(n.dodge=3))
ggsave("WWC092_Delta_freq_plot-gradient-Rainbow-aa_change-facet-1.png",units="in", width= 12, height = 6, device='png', dpi=300)