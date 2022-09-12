library(data.table)
library(purrr)
library(dplyr)
library(qpcR)
library(UpSetR)
library(ggplot2)
library(janitor)
library(openxlsx)

old_base_dir <- getwd()
setwd(dir = "../WWC092_vcfs/")
# read all the filenames 
filenames <- Sys.glob("**.vcf")
filenames

do.call(qpcR:::cbind.na, lapply(filenames, function(x) {
  setNames(data.frame(read.table(x)[[2]]), tools::file_path_sans_ext(basename(x)))
})) -> POS_comb
head(POS_comb)
# #rename column names
# names(POS_comb)[1] <- "GATK"
# names(POS_comb)[2] <- "BCFtools"
# names(POS_comb)[3] <- "FreeBayes"
# names(POS_comb)[4] <- "iVar"
# names(POS_comb)[5] <- "LoFreq"
# names(POS_comb)[6] <- "VarScan"

#only for S296, as GATK didnt work for S296
names(POS_comb)[1] <- "BCFtools"
names(POS_comb)[2] <- "FreeBayes"
names(POS_comb)[3] <- "iVar"
names(POS_comb)[4] <- "LoFreq"
names(POS_comb)[5] <- "VarScan"
head(POS_comb)

write.table(POS_comb, "POS_comb_S296.txt",na="", sep="\t")

POS_comb_S296_added <- fread("POS_comb_S296_added.txt")
head(POS_comb_S296_added)
# 
POS_comb_S296_added_1 <- subset(POS_comb_S296_added, select=-c(V1))
head(POS_comb_S296_added_1)

POS_comb_S296_added_1 %>% remove_empty("rows")

text_list <- fromList(POS_comb_S296_added_1)
write.xlsx(text_list, "S296_binary.xlsx")

upset(fromList(POS_comb_S296_added_1), nintersects=99, keep.order=T, sets = c("LoFreq","iVar","FreeBayes","BCFtools","VarScan","Delta","Omicron BA.2","Omicron BA.1"), nsets=8, point.size=2.5, line.size=1, order.by = "degree", number.angles= 0, mainbar.y.label= "No of SNPs/Indels intersections", sets.x.label = "S296 - No of SNPs/Indels", text.scale = 1.75, sets.bar.color = "blue", main.bar.color = "dark green", matrix.color = "red")
#upset(fromList(POS_comb), keep.order=T, sets = c("LoFreq","iVar","FreeBayes","BCFtools","VarScan"), nsets=5, point.size=2.5, line.size=1, order.by = "degree", number.angles= 0, mainbar.y.label= "No of SNPs/Indels intersections", sets.x.label = "No of SNPs/Indels", text.scale = 1.75, sets.bar.color = "blue", main.bar.color = "dark green", matrix.color = "red")

#keep.order=T, sets = c("LoFreq","iVar","GATK","FreeBayes","BCFtools","VarScan","Omicron ENG", "Omicron AUS", "Omicron HK"),
#ggsave("Upset_plot_WWC105_01_1_only_S42_OMIC.png",units="in", width= 12, height = 6, device='png', dpi=300)

#without Lofreq
POS_comb_S42_OMIC_added_1_lfq <- subset(POS_comb_S42_OMIC_added_1, select=-c(LoFreq))
head(POS_comb_S42_OMIC_added_1_lfq)

upset(fromList(POS_comb_S42_OMIC_added_1_lfq), keep.order=T, sets = c("iVar","GATK","FreeBayes","BCFtools","VarScan","Delta"), nsets=6, point.size=2.5, line.size=1, order.by = "degree", number.angles= 0, mainbar.y.label= "No of SNPs/Indels intersections", sets.x.label = "100% Delta - No of SNPs/Indels", text.scale = 1.75, sets.bar.color = "blue", main.bar.color = "dark green", matrix.color = "red")

BCF_Delta_count <- sum(text_list['BCFtools'] == 1 & text_list['Delta']==1)
BCF_Delta_count

