library(stringr)
library(devtools)
library(plyr)
library(RBioinf)
library(ggplot2)
options(scipen=999)

N_SVcall = 2400
N_SVTYPE = 6
LEN <- c(50,100,500,1000,2000,5000,10000,15000,100000,1000000)
source("VCF_functions.R")

################################################
### Foreign insertion sequence simulation ###
################################################
Foreign_INS_stat <- data.frame(cbind(LEN,rep(N_SVcall/N_SVTYPE/length(LEN),length(LEN))))
colnames(Foreign_INS_stat) <- c("INS_LEN","Num")
FINS_seq <- c()
FINS_name <- c()
FINS_LEN <- c()
ID <- c()
set.seed(123)
for (i in 1: nrow(Foreign_INS_stat)){
  for(j in 1:Foreign_INS_stat$Num[i]){
    FINS_name <- c(FINS_name,paste(">","Foreign_INS","_",j,"_",Foreign_INS_stat$INS_LEN[i],sep=""))
    FINS_seq <- c(FINS_seq,randDNA(Foreign_INS_stat[i,1]))
    FINS_LEN <- c(FINS_LEN,Foreign_INS_stat$INS_LEN[i])
  }
}
FINS_df <- as.data.frame(cbind(FINS_name,FINS_seq,FINS_LEN),stringAsFactor = FALSE)
FINS_bed <- data.frame(
  name = substring(FINS_df$FINS_name,2),
  start = 1,
  end = FINS_df$FINS_LEN,
  stringsAsFactors = FALSE
)
write.table(FINS_bed, "FINS.bed", quote=FALSE, sep='\t', row.names=FALSE, col.names=TRUE)
write.table(FINS_df, "FINS_seq.bed", quote=FALSE, sep='\t', row.names=FALSE, col.names=TRUE)

### Generate random FINS fasta file
FINS_df_tmp <- FINS_df[rep(c(1:nrow(FINS_df)),each=2),]
index <- rep(c(TRUE,FALSE),nrow(FINS_df))
FINS_fas <- index
FINS_fas[index] <- paste(FINS_df_tmp[index,1],FINS_df_tmp[index,3])
FINS_fas[!index] <- as.character(FINS_df_tmp[!index,2])
write.table(FINS_fas, "FINS.fas", quote=FALSE, sep='\t', row.names=FALSE, col.names=FALSE)

################################################
### from random SV POS bed file to var file ####
################################################

random_POS.bed <- read.table("random_POS_noG_noC_noT.bed",header = FALSE, sep="\t",stringsAsFactors=FALSE, quote="")
colnames(random_POS.bed) <- c("chrom","start","end","ID","length","strand")
random_POS.bed$ID <- c(1:nrow(random_POS.bed))
write.table(random_POS.bed,"random_POS_noG_noC_noT.bed", quote=FALSE, sep='\t', row.names=FALSE, col.names=FALSE)

####### Generage Somatic SV var file##########
random_POS.bed <- read.table("random_POS_noG_noC_noT.bed",header = FALSE, sep="\t",stringsAsFactors=FALSE, quote="")
colnames(random_POS.bed) <- c("chrom","start","end","ID","length","strand")

VARSSEQ_1 <- "Homo_sapiens_assembly38_chromosome.fasta" ### Reference genome to spike SVs in
FINS_VARSSEQ_1 <- "FINS.fas" ### Simulated sequence for foreign insertions
SVTYPE_N_each = N_SVcall/N_SVTYPE
include_FINS = TRUE
dir_FINS_bed = "FINS.bed"
randomSV2.var <- random_POS_to_var(random_POS.bed,LEN,include_FINS,dir_FINS_bed,SVTYPE_N_each,VARSSEQ_1,FINS_VARSSEQ_1)
write.table(randomSV2.var,"randomSV2_noG_noC_noT.var", quote=FALSE, sep='\t', row.names=FALSE, col.names=FALSE)

####### Generage germline SV var file##########
randomSV2.var <- read.table("randomSV2_noG_noC_noT.var",header = FALSE, sep="\t",stringsAsFactors=FALSE, quote="")
colnames(randomSV2.var) <- c("VID","MID","VARFREQ","VARHAP","VARCHR",
                             "VARPOS",	"VARDEL",	"VARDELSIZE",	"VARINS",	"VARINSSEQ(HAP/SEQFILE,CHR:START-END,COPY,REVCOMP")

### Simple method, but may not suitable for all cases, e.g. Not same number of SV in each type
set.seed(456)
SVTYPE_N <- 400
tmp <- sample(1:SVTYPE_N, SVTYPE_N/2, replace=F) #The number of each SVTYPE is 200 for SV2

SV1_ID <- c(paste("DEL",tmp,sep="_"),
            paste("DUP",tmp,sep="_"),
            paste("INV",tmp,sep="_"),
            paste("DINS",tmp,sep="_"),
            paste("TRA",tmp,sep="_"),
            paste("FINS",tmp,sep="_"))
randomSV1.var <- randomSV2.var[randomSV2.var$MID %in% SV1_ID,]
write.table(randomSV1.var, "randomSV1_noG_noC_noT.var", quote=FALSE, sep='\t', row.names=FALSE, col.names=FALSE)



