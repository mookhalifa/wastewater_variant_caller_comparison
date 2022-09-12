library(data.table)
library(purrr)
library(dplyr)
library(qpcR)
library(UpSetR)
library(ggplot2)
library(openxlsx)

# read all the filenames 
filenames <- Sys.glob("01_35_*.vcf")
filenames

# copy the 2nd column POS from each of VCF file and copy to POS_comb
do.call(qpcR:::cbind.na, lapply(filenames, function(x) {
  setNames(data.frame(read.table(x)[[2]]), tools::file_path_sans_ext(basename(x)))
})) -> POS_comb
head(POS_comb)

#rename column names
names(POS_comb)[1] <- "GATK"
names(POS_comb)[2] <- "VarScan"
names(POS_comb)[3] <- "BCFtools"
names(POS_comb)[4] <- "FreeBayes"
names(POS_comb)[5] <- "LoFreq"
names(POS_comb)[6] <- "iVar"

head(POS_comb)

write.table(POS_comb, "POS_comb_01_35.txt",na="", sep="\t")
#write.table(POS_comb, "POS_comb_01_35_added.txt",na="", sep="\t")

#alpha columne and values added to written file

POS_comb_added <- fread("POS_comb_01_35_added.txt")

#remove column V1
POS_comb_added <- subset(POS_comb_added, select=-c(V1))
head(POS_comb_added)

text_list <- fromList(POS_comb_added)
write.xlsx(text_list, "01_35_Alpha-Beta-Delta_binary.xlsx")

Alpha <- sum(text_list['Alpha']==1)
Beta <- sum(text_list['Beta']==1)
Delta <- sum(text_list['Delta']==1)
BCFtools <- sum(text_list['BCFtools']==1)
GATK <- sum(text_list['GATK']==1)
VarScan <- sum(text_list['VarScan']==1)
FreeBayes <- sum(text_list['FreeBayes']==1)
LoFreq <- sum(text_list['LoFreq']==1)
iVar <- sum(text_list['iVar']==1)

BCFtools_Alpha <- sum(text_list['BCFtools'] == 1 & text_list['Alpha']==1)
GATK_Alpha <- sum(text_list['GATK'] == 1 & text_list['Alpha']==1)
VarScan_Alpha <- sum(text_list['VarScan'] == 1 & text_list['Alpha']==1)
FreeBayes_Alpha <- sum(text_list['FreeBayes'] == 1 & text_list['Alpha']==1)
LoFreq_Alpha <- sum(text_list['LoFreq'] == 1 & text_list['Alpha']==1)
iVar_Alpha <- sum(text_list['iVar'] == 1 & text_list['Alpha']==1)

BCFtools_Beta <- sum(text_list['BCFtools'] == 1 & text_list['Beta']==1)
GATK_Beta <- sum(text_list['GATK'] == 1 & text_list['Beta']==1)
VarScan_Beta <- sum(text_list['VarScan'] == 1 & text_list['Beta']==1)
FreeBayes_Beta <- sum(text_list['FreeBayes'] == 1 & text_list['Beta']==1)
LoFreq_Beta <- sum(text_list['LoFreq'] == 1 & text_list['Beta']==1)
iVar_Beta <- sum(text_list['iVar'] == 1 & text_list['Beta']==1)

BCFtools_Delta <- sum(text_list['BCFtools'] == 1 & text_list['Delta']==1)
GATK_Delta <- sum(text_list['GATK'] == 1 & text_list['Delta']==1)
VarScan_Delta <- sum(text_list['VarScan'] == 1 & text_list['Delta']==1)
FreeBayes_Delta <- sum(text_list['FreeBayes'] == 1 & text_list['Delta']==1)
LoFreq_Delta <- sum(text_list['LoFreq'] == 1 & text_list['Delta']==1)
iVar_Delta <- sum(text_list['iVar'] == 1 & text_list['Delta']==1)

count <- data.frame(Caller = c("LoFreq","iVar","GATK","FreeBayes","BCFtools","VarScan"), 
                    Total = c(LoFreq,iVar,GATK,FreeBayes,BCFtools,VarScan), 
                    Found_in_Alpha = c(LoFreq_Alpha,iVar_Alpha,GATK_Alpha,FreeBayes_Alpha,BCFtools_Alpha,VarScan_Alpha),
                    Found_in_Beta = c(LoFreq_Beta,iVar_Beta,GATK_Beta,FreeBayes_Beta,BCFtools_Beta,VarScan_Beta),
                    Found_in_Delta = c(LoFreq_Delta,iVar_Delta,GATK_Delta,FreeBayes_Delta,BCFtools_Delta,VarScan_Delta))
print(count)
write.xlsx(count, "01_35_mix_Found_A-B-D_count.xlsx")

#Plot upset plot with parameters
upset(fromList(POS_comb_added), nintersects=70, cutoff=7, keep.order=T, sets = c("LoFreq","iVar","GATK","FreeBayes","BCFtools","VarScan","Alpha"), nsets=7, point.size=2.5, line.size=1, order.by = "degree", number.angles= 0, mainbar.y.label= "No of SNPs/Indels intersections", sets.x.label = "Mix - No of SNPs/Indels", text.scale = 1.75, sets.bar.color = "blue", main.bar.color = "dark green", matrix.color = "red")
ggsave("Upset_plot_WWC105_01_35_only_Alpha.png",units="in", width= 12, height = 6, device='png', dpi=300)

upset(fromList(POS_comb_added), nintersects=70, cutoff=7, keep.order=T, sets = c("LoFreq","iVar","GATK","FreeBayes","BCFtools","VarScan","Beta"), nsets=7, point.size=2.5, line.size=1, order.by = "degree", number.angles= 0, mainbar.y.label= "No of SNPs/Indels intersections", sets.x.label = "Mix - No of SNPs/Indels", text.scale = 1.75, sets.bar.color = "blue", main.bar.color = "dark green", matrix.color = "red")
ggsave("Upset_plot_WWC105_01_35_only_Beta.png",units="in", width= 12, height = 6, device='png', dpi=300)

upset(fromList(POS_comb_added), nintersects=70, cutoff=7, keep.order=T, sets = c("LoFreq","iVar","GATK","FreeBayes","BCFtools","VarScan","Delta"), nsets=7, point.size=2.5, line.size=1, order.by = "degree", number.angles= 0, mainbar.y.label= "No of SNPs/Indels intersections", sets.x.label = "Mix - No of SNPs/Indels", text.scale = 1.75, sets.bar.color = "blue", main.bar.color = "dark green", matrix.color = "red")
ggsave("Upset_plot_WWC105_01_35_only_Delta.png",units="in", width= 12, height = 6, device='png', dpi=300)




#without Lofreq
POS_comb_Alpha_lfq <- subset(POS_comb_Alpha, select=-c(LoFreq))
head(POS_comb_Alpha_lfq)

upset(fromList(POS_comb_Alpha_lfq), keep.order=T, sets = c ("iVar","GATK","FreeBayes","BCFtools","VarScan","Alpha"), nsets=6, point.size=2.5, line.size=1, order.by = "degree", number.angles= 0, mainbar.y.label= "No of SNPs/Indels intersections", sets.x.label = "Mix Alpha - No of SNPs/Indels", text.scale = 1.75, sets.bar.color = "blue", main.bar.color = "dark green", matrix.color = "red")
ggsave("Upset_plot_WWC105_01_1_only_Alpha_wo_lfq.png",units="in", width= 12, height = 6, device='png', dpi=300)

