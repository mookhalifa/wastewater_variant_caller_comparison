library(data.table)
library(purrr)
library(dplyr)
library(qpcR)
library(UpSetR)
library(ggplot2)
library(openxlsx)

# read all the filenames 
filenames <- Sys.glob("*S63*.vcf")
filenames

# copy the 2nd column POS from each of VCF file and copy to POS_comb
do.call(qpcR:::cbind.na, lapply(filenames, function(x) {
  setNames(data.frame(read.table(x)[[2]]), tools::file_path_sans_ext(basename(x)))
})) -> POS_comb
head(POS_comb)

#rename column names
names(POS_comb)[1] <- "GATK"
names(POS_comb)[2] <- "BCFtools"
names(POS_comb)[3] <- "FreeBayes"
names(POS_comb)[4] <- "iVar"
names(POS_comb)[5] <- "LoFreq"
names(POS_comb)[6] <- "VarScan"

# #only for S296, as GATK didnt work for S296
# names(POS_comb)[1] <- "BCFtools"
# names(POS_comb)[2] <- "FreeBayes"
# names(POS_comb)[3] <- "iVar"
# names(POS_comb)[4] <- "LoFreq"
# names(POS_comb)[5] <- "VarScan"
head(POS_comb)

write.table(POS_comb, "POS_comb_S63.txt",na="", sep="\t")

write.table(POS_comb, "POS_comb_S63_added.txt",na="", sep="\t")

#Delta columne and values added to written file

POS_comb_added <- fread("POS_comb_S63_added.txt")

#remove column V1
POS_comb_added <- subset(POS_comb_added, select=-c(V1))
head(POS_comb_added)

text_list <- fromList(POS_comb_added)
write.xlsx(text_list, "S63_binary.xlsx")

Delta <- sum(text_list['Delta']==1)
OmicronBA.1 <- sum(text_list['Omicron BA.1']==1)
OmicronBA.2 <- sum(text_list['Omicron BA.2']==1)
BCFtools <- sum(text_list['BCFtools']==1)
GATK <- sum(text_list['GATK']==1)
VarScan <- sum(text_list['VarScan']==1)
FreeBayes <- sum(text_list['FreeBayes']==1)
LoFreq <- sum(text_list['LoFreq']==1)
iVar <- sum(text_list['iVar']==1)

BCFtools_Delta <- sum(text_list['BCFtools'] == 1 & text_list['Delta']==1)
GATK_Delta <- sum(text_list['GATK'] == 1 & text_list['Delta']==1)
VarScan_Delta <- sum(text_list['VarScan'] == 1 & text_list['Delta']==1)
FreeBayes_Delta <- sum(text_list['FreeBayes'] == 1 & text_list['Delta']==1)
LoFreq_Delta <- sum(text_list['LoFreq'] == 1 & text_list['Delta']==1)
iVar_Delta <- sum(text_list['iVar'] == 1 & text_list['Delta']==1)

BCFtools_OmicronBA.1 <- sum(text_list['BCFtools'] == 1 & text_list['Omicron BA.1']==1)
GATK_OmicronBA.1 <- sum(text_list['GATK'] == 1 & text_list['Omicron BA.1']==1)
VarScan_OmicronBA.1 <- sum(text_list['VarScan'] == 1 & text_list['Omicron BA.1']==1)
FreeBayes_OmicronBA.1 <- sum(text_list['FreeBayes'] == 1 & text_list['Omicron BA.1']==1)
LoFreq_OmicronBA.1 <- sum(text_list['LoFreq'] == 1 & text_list['Omicron BA.1']==1)
iVar_OmicronBA.1 <- sum(text_list['iVar'] == 1 & text_list['Omicron BA.1']==1)

BCFtools_OmicronBA.2 <- sum(text_list['BCFtools'] == 1 & text_list['Omicron BA.2']==1)
GATK_OmicronBA.2 <- sum(text_list['GATK'] == 1 & text_list['Omicron BA.2']==1)
VarScan_OmicronBA.2 <- sum(text_list['VarScan'] == 1 & text_list['Omicron BA.2']==1)
FreeBayes_OmicronBA.2 <- sum(text_list['FreeBayes'] == 1 & text_list['Omicron BA.2']==1)
LoFreq_OmicronBA.2 <- sum(text_list['LoFreq'] == 1 & text_list['Omicron BA.2']==1)
iVar_OmicronBA.2 <- sum(text_list['iVar'] == 1 & text_list['Omicron BA.2']==1)

count <- data.frame(Caller = c("LoFreq","iVar","GATK","FreeBayes","BCFtools","VarScan"), 
                    Total = c(LoFreq,iVar,GATK,FreeBayes,BCFtools,VarScan),
                    Found_in_S63_OmicronBA.1 = c(LoFreq_OmicronBA.1,iVar_OmicronBA.1,GATK_OmicronBA.1,FreeBayes_OmicronBA.1,BCFtools_OmicronBA.1,VarScan_OmicronBA.1), 
                    Found_in_S63_OmicronBA.2 = c(LoFreq_OmicronBA.2,iVar_OmicronBA.2,GATK_OmicronBA.2,FreeBayes_OmicronBA.2,BCFtools_OmicronBA.2,VarScan_OmicronBA.2), 
                    Found_in_S63_Delta = c(LoFreq_Delta, iVar_Delta, GATK_Delta, FreeBayes_Delta, BCFtools_Delta, VarScan_Delta))
print(count)
write.xlsx(count, "Found_S63_count.xlsx")

#Plot upset plot with parameters
upset(fromList(POS_comb_added), nintersects=99, cutoff=9, keep.order=T, sets = c("LoFreq","iVar","GATK","FreeBayes","BCFtools","VarScan","Delta","Omicron BA.2","Omicron BA.1"), nsets=9, point.size=2.5, line.size=1, order.by = "degree", number.angles= 0, mainbar.y.label= "No of SNPs/Indels intersections", sets.x.label = "S63 - No of SNPs/Indels", text.scale = 1.75, sets.bar.color = "blue", main.bar.color = "dark green", matrix.color = "red")


ggsave("Upset_plot_WWC092_S63.png",units="in", width= 12, height = 6, device='png', dpi=300)

#without Lofreq
POS_comb_wo_lfq <- subset(POS_comb_added, select=-c(LoFreq))
head(POS_comb_wo_lfq)

upset(fromList(POS_comb_wo_lfq),  nintersects=99, keep.order=T, sets = c ("iVar","GATK","FreeBayes","BCFtools","VarScan","Delta","Omicron BA.2","Omicron BA.1"), nsets=8, point.size=2.5, line.size=1, order.by = "degree", number.angles= 0, mainbar.y.label= "No of SNPs/Indels intersections", sets.x.label = "S63 - No of SNPs/Indels", text.scale = 1.75, sets.bar.color = "blue", main.bar.color = "dark green", matrix.color = "red")
ggsave("Upset_plot_WWC092_S63_wo_lfq.png",units="in", width= 12, height = 6, device='png', dpi=300)

