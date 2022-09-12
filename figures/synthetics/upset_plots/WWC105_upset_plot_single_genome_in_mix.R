library(data.table)
library(purrr)
library(dplyr)
library(qpcR)
library(UpSetR)
library(ggplot2)

# read all the filenames 
filenames <- Sys.glob("01_1_*.vcf")
filenames

do.call(qpcR:::cbind.na, lapply(filenames, function(x) {
  setNames(data.frame(read.table(x)[[2]]), tools::file_path_sans_ext(basename(x)))
})) -> POS_comb

#rename column names
names(POS_comb)[1] <- "GATK"
names(POS_comb)[2] <- "BCFtools"
names(POS_comb)[3] <- "FreeBayes"
names(POS_comb)[4] <- "iVar"
names(POS_comb)[5] <- "LoFreq"
names(POS_comb)[6] <- "VarScan"

head(POS_comb)
write.table(POS_comb, "POS_comb.txt",na="", sep="\t")

alpha <- fread("alpha.txt")
head(alpha)
head(POS_comb)

POS_comb_alpha2 <- append(POS_comb, alpha)
# 
# POS_comb_alpha2 <- subset(POS_comb_alpha2, select=-c(V1))
head(POS_comb_alpha2)
POS_comb_alpha3 <- na.omit(POS_comb_alpha2)
head(POS_comb_alpha3)

upset(fromList(POS_comb_alpha2), keep.order=T, sets = c ("LoFreq","iVar","GATK","FreeBayes","BCFtools","VarScan","Alpha"), nsets=7, point.size=2.5, line.size=1, order.by = "degree", number.angles= 0, mainbar.y.label= "No of SNPs/Indels intersections", sets.x.label = "100% Alpha - No of SNPs/Indels", text.scale = 1.75, sets.bar.color = "blue", main.bar.color = "dark green", matrix.color = "red")


#ggsave("Upset_plot_WWC105_01_1_only_alpha.png",units="in", width= 12, height = 6, device='png', dpi=300)

#without Lofreq

POS_comb_alpha2 <- subset(POS_comb, select=-c(LoFreq))

head(POS_comb_alpha_wo_lfq)

upset(fromList(POS_comb_alpha_wo_lfq), keep.order=T, sets = c ("iVar","GATK","FreeBayes","BCFtools","VarScan","Alpha"), nsets=6, point.size=2.5, line.size=1, order.by = "degree", number.angles= 0, mainbar.y.label= "No of SNPs/Indels intersections", sets.x.label = "100% Alpha - No of SNPs/Indels", text.scale = 1.75, sets.bar.color = "blue", main.bar.color = "dark green", matrix.color = "red")
ggsave("Upset_plot_WWC105_01_1_only_alpha_wo_lfq.png",units="in", width= 12, height = 6, device='png', dpi=300)




head(Alpha)
# fromList(result)
# 
# 
# # upsetlist1 <- subset(upsetlist, select=-c(Alpha))
# # head(upsetlist1)
# 
# 
# 
# upsetlist <- fread("01_1_POS2.txt", header=TRUE)
# head(upsetlist)
# 
# upsetlist1 <- subset(upsetlist, select=-c(Alpha))
# head(upsetlist1)
# 
# upset(fromList(upsetlist), nsets=7, point.size=2.5, line.size=1, order.by = "degree", number.angles= 0, mainbar.y.label= "No of SNPs/Indels intersections", sets.x.label = "No of SNPs/Indels", text.scale = 1.75, sets.bar.color = "blue", main.bar.color = "dark green", matrix.color = "red")
# fromList(upsetlist)


# Alpha = c(241,3037,14408,23403,28274,913,3267,5388,5986,6954,11288,14676,15279,16176,21766,21994,23063,23271,23604,23709,24506,24914,27972,28048,28111,28280,28881,28977)
# 
# result_alpha <- cbind(result, Alpha)
