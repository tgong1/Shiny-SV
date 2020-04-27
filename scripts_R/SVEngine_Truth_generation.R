source("VCF_functions.R")
directory <- "./SVEngine"
setwd(directory)

####### Generate somatic SV calls ##########
randomSV2.out.bed <- read.table("randomSV2_noG_noC_noT.out.bed",header = FALSE, sep="\t",stringsAsFactors=FALSE, quote="")
randomSV1.out.bed <- read.table("randomSV1_noG_noC_noT.out.bed",header = FALSE, sep="\t",stringsAsFactors=FALSE, quote="")

colnames(randomSV2.out.bed) <- c("CHROM","START","END","VID","MID",
                                 "VARFREQ","VARHAP","VARCHR",
                                 "VARPOS","VARDEL","VARDELSIZE","VARINS","VARINSSEQ(HAP/SEQFILE,CHR:START-END,COPY,REVCOMP)")
colnames(randomSV1.out.bed) <- c("CHROM","START","END","VID","MID",
                                 "VARFREQ","VARHAP","VARCHR",
                                 "VARPOS","VARDEL","VARDELSIZE","VARINS","VARINSSEQ(HAP/SEQFILE,CHR:START-END,COPY,REVCOMP)")

SomaticSV.bed <- randomSV2.out.bed[!(randomSV2.out.bed$VID %in% randomSV1.out.bed$VID),]
write.table(SomaticSV.bed, "SomaticSV.bed", quote=FALSE, sep='\t', row.names=FALSE, col.names=FALSE)

####### Generate SVEngine Truth SV calls in standard format ##########
SomaticSV.bed <- read.table("SomaticSV.bed",header = FALSE, sep="\t",stringsAsFactors=FALSE, quote="")
colnames(SomaticSV.bed) <- c("CHROM","START","END","VID","MID",
                             "VARFREQ","VARHAP","VARCHR",
                             "VARPOS","VARDEL","VARDELSIZE","VARINS","VARINSSEQ(HAP/SEQFILE,CHR:START-END,COPY,REVCOMP)")
SomaticSV.bed <- SomaticSV.bed[order(as.numeric(gsub("VAR_","",SomaticSV.bed$VID))),]

SVEngine_truth_bed <- SVEngine_bed_generate(SomaticSV.bed)
colnames(SVEngine_truth_bed) <- c("chrom","start","end","SVTYPE","SVLEN","ID","ID_mate","VID.1","VID.2","INS_CHROM","INS_START","INS_END","INSLEN")
write.table(SVEngine_truth_bed, "SVEngine_truth.bed", quote=FALSE, sep='\t', row.names=FALSE, col.names=FALSE)


