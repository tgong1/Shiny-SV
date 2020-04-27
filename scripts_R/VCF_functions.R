library(VariantAnnotation)
library(stringr)
library(devtools)
library(StructuralVariantAnnotation)

SVEngine_segdup_generate <- function(Truth_file, bedtools_dir, directory){
  setwd(directory)
  setwd("./SVEngine")
  SVEngine_truth.bed <- read.table("SVEngine_truth.bed",header = FALSE, sep="\t",stringsAsFactors=FALSE, quote="")
  colnames(SVEngine_truth.bed) <- c("chrom","start","end","SVTYPE","SVLEN","ID","ID_mate","VID.1","VID.2","INS_CHROM","INS_START","INS_END","INSLEN")
  
  tmp_bed <- SVEngine_truth.bed
  tmp_bed[tmp_bed$SVTYPE == "INS_BND",c(1:3,5)] <- tmp_bed[tmp_bed$SVTYPE == "INS_BND",c(10:13)]
  tmp_bed[tmp_bed$SVTYPE == "TRA_BND",]$end =
    tmp_bed[tmp_bed$SVTYPE == "TRA_BND",]$start + 1
  tmp_bed <- tmp_bed[!(tmp_bed$SVTYPE == "TRA_BND" & tmp_bed$start %in% c(tmp_bed$INS_START,tmp_bed$INS_END)),]
  
  #tmp_bed[tmp_bed$SVTYPE == "TRA_BND",]$end =
  #  tmp_bed[tmp_bed$SVTYPE == "TRA_BND",]$start + 1
  #tmp_bed[tmp_bed$SVTYPE == "INS_BND",]$end =
  #  tmp_bed[tmp_bed$SVTYPE == "INS_BND",]$start + 1
  #tmp_bed[tmp_bed$SVTYPE == "INS_BND",][seq(2,800,2),c(1:3,5)] <- tmp_bed[tmp_bed$SVTYPE == "INS_BND",][seq(2,800,2),c(10:13)]
  
  write.table(tmp_bed, "tmp.bed", quote=FALSE, sep='\t', row.names=FALSE, col.names=FALSE)
  system(paste0(bedtools_dir, " intersect -a tmp.bed -b ../../hg38_genomicSuperDups.sorted.merged.bed -wo > SVEngine_segdup_intersect.bed"))
  SV2_segdup_intersect.bed <- read.table("SVEngine_segdup_intersect.bed",header = FALSE, sep="\t",stringsAsFactors=FALSE, quote="")
  colnames(SV2_segdup_intersect.bed) <- c("chrom","start","end","SVTYPE","SVLEN","ID","ID_mate","VID.1","VID.2","INS_CHROM","INS_START","INS_END","INSLEN",
                                          "CHROM","START","END","overlap")
  SVEngine_segdup.bed <- 
    SVEngine_truth.bed[str_split_fixed(SVEngine_truth.bed$ID,"_",4)[,4] %in% str_split_fixed(SV2_segdup_intersect.bed$ID,"_",4)[,4],]
  return(SVEngine_segdup.bed)
}

SVCaller_union_intersect_generate <- function(Truth_file,vcf_file,SVCaller_name,BND_diff,bkpt_T_callers,coverage,VAF,directory,SVTYPE_ignore,bedtools_dir){
  setwd(directory)
  setwd("./SVEngine")
  SVEngine_truth_bed <- read.table(Truth_file,header = FALSE, sep="\t",stringsAsFactors=FALSE, quote="")
  colnames(SVEngine_truth_bed) <- c("chrom","start","end","SVTYPE","SVLEN","ID","ID_mate","VID.1","VID.2","INS_CHROM","INS_START","INS_END","INSLEN")

  sub_directory <- paste0(SVCaller_name,collapse = "")
  ### Each vcf_file, convert to object SVCaller_vcf, then bed and bed_tmp and written to bed_tmp file
  for (i in 1:length(SVCaller_name)){
    setwd(directory)
    setwd(paste0("./",SVCaller_name[i]))
    if(SVCaller_name[i] == "BreakDancer"){
      assign(paste0(SVCaller_name[i],"_vcf"),readLines(vcf_file[i]))
      nrow_in_vcf <- length(eval(parse(text=paste0(SVCaller_name[i],"_vcf"))))
    }else{
      assign(paste0(SVCaller_name[i],"_vcf"),readVcf(vcf_file[i]))
      nrow_in_vcf <- nrow(info(eval(parse(text=paste0(SVCaller_name[i],"_vcf")))))
    }
    
    if(nrow_in_vcf == 0){
      assign(paste0(SVCaller_name[i],"_bed"), data.frame(matrix(ncol=0,nrow=0)))
      assign(paste0(SVCaller_name[i],"_bed_tmp"), data.frame(matrix(ncol=0,nrow=0)))
    }else{
      assign(paste0(SVCaller_name[i],"_bed"),SVCaller_bed_newID_generate(eval(parse(text=paste0(SVCaller_name[i],"_vcf"))),SVCaller_name[i]))
      assign(paste0(SVCaller_name[i],"_bed_tmp"),Standard_bedtool_prepare_bkpt(eval(parse(text=paste0(SVCaller_name[i],"_bed"))),BND_diff))
    }

    #assign(paste0(SVCaller_name[i],"_bed"),SVCaller_bed_newID_generate(eval(parse(text=paste0(SVCaller_name[i],"_vcf"))),SVCaller_name[i]))
    #assign(paste0(SVCaller_name[i],"_bed_tmp"),Standard_bedtool_prepare_bkpt(eval(parse(text=paste0(SVCaller_name[i],"_bed"))),BND_diff))
    setwd(directory)
    setwd(paste0("./",sub_directory))
    write.table(eval(parse(text=paste0(SVCaller_name[i],"_bed_tmp"))), paste0(SVCaller_name[i],"_tmp.bed"), quote=FALSE, sep='\t', row.names=FALSE, col.names=FALSE)
  }

  ### Union set
  # generate bed and bed with new name, bed with new name bed_tmp and written to bed_tmp file
  SVCaller_name_all <- paste0(paste0(SVCaller_name,collapse = ""),"ALL")
  SVCaller_bed_all <- do.call("rbind", lapply(paste0(SVCaller_name,"_bed"),function(s) eval(parse(text=s))))
  SVCaller_bed_all_newID <- SVCaller_bed_newID_generate2(SVCaller_bed_all,SVCaller_name_all)
  SVCaller_bed_all_newID_tmp <- Standard_bedtool_prepare_bkpt(SVCaller_bed_all_newID,BND_diff)
  write.table(SVCaller_bed_all_newID_tmp, paste0(SVCaller_name_all,"_tmp.bed"), quote=FALSE, sep='\t', row.names=FALSE, col.names=FALSE)

  ### Intersect set
  # bedtools intersect union set bed_tmp with all SV caller bed_tmp
  intersect_file <- paste0("all_",paste0(SVCaller_name,collapse = "_"),"_intersect.bed")
  overlap_f <- (BND_diff - bkpt_T_callers)/BND_diff
  system(paste(bedtools_dir,"intersect -a", paste0(SVCaller_name_all,"_tmp.bed"),
               "-b", paste(paste0(SVCaller_name,"_tmp.bed"), collapse = " "),
               "-names",paste(SVCaller_name,collapse = " "), "-f",overlap_f, "-wo >", intersect_file))

  intersect_filter <- TypePosfilter(intersect_file, SVTYPE_ignore)
  ### remove ID in tmp bed, only the ID in original bed
  intersect_filter <- intersect_filter[intersect_filter$Caller1_ID %in% SVCaller_bed_all_newID$ID,]

  ############### work for three sv caller overlapping
  #########################################################
  data <- NULL
  for(i in 1: length(unique(intersect_filter$Caller1_ID))){
    tmp <- paste(intersect_filter[intersect_filter$Caller1_ID == unique(intersect_filter$Caller1_ID)[i],]$Caller2_ID)
    data <- rbind(data,
                  c(unique(intersect_filter$Caller1_ID)[i],
                    unlist(lapply(SVCaller_name,function(s)paste(tmp[grepl(s,tmp)],collapse = ",")))))
  }
  overlap_bed <- data.frame(data[,-1], stringsAsFactors = FALSE)
  colnames(overlap_bed) <- SVCaller_name
  overlap_bed <- unique(overlap_bed)
  overlap_index <- overlap_bed !=""


  ### pick the first SV caller
  union1 <- paste(c(overlap_bed[overlap_index[,1],1]),collapse = ",")
  union2 <- paste(c(overlap_bed[overlap_index[,2] & (rowSums(overlap_index) != ncol(overlap_index)),2]),collapse = ",")
 # SVCaller_name_union <- paste0(paste0(SVCaller_name,collapse = ""),"Union")
  SVCaller_bed_union1 <- eval(parse(text=paste0(SVCaller_name[1],"_bed")))[as.character(eval(parse(text=paste0(SVCaller_name[1],"_bed$ID")))) %in% strsplit(union1,split=",")[[1]],]
  SVCaller_bed_union2 <- eval(parse(text=paste0(SVCaller_name[2],"_bed")))[as.character(eval(parse(text=paste0(SVCaller_name[2],"_bed$ID")))) %in% strsplit(union2,split=",")[[1]],]
  SVCaller_bed_union <- rbind(SVCaller_bed_union1,SVCaller_bed_union2)

  ### pick the first SV caller
  intersect <- paste(c(overlap_bed[rowSums(overlap_index) == ncol(overlap_index),1]),collapse = ",")
  SVCaller_name_intersect <- paste0(paste0(SVCaller_name,collapse = ""),"Intersect")
  SVCaller_bed_intersect <- eval(parse(text=paste0(SVCaller_name[1],"_bed")))[as.character(eval(parse(text=paste0(SVCaller_name[1],"_bed$ID")))) %in% strsplit(intersect,split=",")[[1]],]


  #############################################
  ### ony work for two SV caller overlapping ########################
  #SVCaller_bed_intersect <- eval(parse(text=paste0(SVCaller_name[1],"_bed")))[eval(parse(text=paste0(SVCaller_name[1],"_bed")))$ID %in% intersect_filter$Caller1_ID,]
  ############################################################

  List <- list(SVCaller_bed_union, SVCaller_bed_intersect)
  return(List)
}

SVCaller_bed_newID_generate2 <- function(SVCaller_bed,SVCaller_name){
  if(nrow(SVCaller_bed) == 0){return( data.frame(matrix(ncol=0,nrow=0)))}
  colnames(SVCaller_bed) <- c("chrom","start","end","SVTYPE","SVLEN","SVCallerID","SVCallerID_mate","event")
  SV_index_tmp <- c(1:length(SVCaller_bed$SVCallerID))
  ID_tmp <- SVCaller_bed$SVCallerID
  SV_mate_index_tmp <- ifelse(is.na(match(SVCaller_bed$SVCallerID_mate,ID_tmp)),SV_index_tmp,match(SVCaller_bed$SVCallerID_mate,ID_tmp))
  SV_index <- ifelse(SV_index_tmp <= SV_mate_index_tmp,SV_index_tmp,SV_mate_index_tmp)
  mate1_index <- ifelse(duplicated(SV_index),"2","1")
  mate2_index <- ifelse(mate1_index=="1","2","1")
  event_index <- SV_index
  ID <- paste(SVCaller_name,"_",SV_index,"_",mate1_index,"_",event_index,sep="")
  ID_mate <- paste(SVCaller_name,"_",SV_index,"_",mate2_index,"_",event_index,sep="")
  SVCaller_bed_newID <- data.frame(cbind(SVCaller_bed[,c(1:5)],ID,ID_mate,SVCaller_bed[,c(6:8)]),stringsAsFactors = FALSE)
  return(SVCaller_bed_newID)
}

SVCaller_union_intersect_stat <- function(SVCaller_bed,SVCaller_name,directory,Truth_file,BND_diff,BND_threshold,SVTYPE_ignore,test_No,bedtools_dir){
  ### Read the truth file
  setwd(directory)
  setwd("./SVEngine")
  SVEngine_truth_bed <- read.table(Truth_file,header = FALSE, sep="\t",stringsAsFactors=FALSE, quote="")
  colnames(SVEngine_truth_bed) <- c("chrom","start","end","SVTYPE","SVLEN","ID","ID_mate","VID.1","VID.2","INS_CHROM","INS_START","INS_END","INSLEN")
  Truth_SV <- SVEngine_STAT_generate(SVEngine_truth_bed)
  Truth_SVLEN <- SVEngine_SVLEN_stat(SVEngine_truth_bed,NA,test_No)
  SVEngine_truth_tmp <- Standard_bedtool_prepare_bkpt(SVEngine_truth_bed,BND_diff)

  ### check if the SVCaller bed file is empty
  if(nrow(SVCaller_bed) == 0){
    SVCaller_SVTYPE <- c(0,0,0,0,0,NA,0,NA,NA,NA,NA)
    SVCaller_TP <- rep(0,16)
    SVCaller_Sensitivity <- rep(0,16)
    SVCaller_TP_tmp <- SVCaller_SVTYPE
    SVCaller_FP <- SVCaller_SVTYPE
    SVCaller_Precision <- rep(NA,11)
    SVCaller_FDR <- rep(NA,11)
    SVCaller_SVLEN <- rep(0,12)
    SVCaller_TP_SVLEN <- rep(0,12)
    SVCaller_SVLEN_Sensitivity <- rep(0,12)
    SVCaller_FP_SVLEN <- rep(0,12)
    SVCaller_SVLEN_Precision <- rep(NA,12)
    SVCaller_SVLEN_FDR <- rep(NA,12)
    List <- list(SVCaller_SVTYPE,SVCaller_TP,SVCaller_Sensitivity,
                 SVCaller_TP_tmp,SVCaller_FP,SVCaller_Precision,SVCaller_FDR,
                 SVCaller_SVLEN,SVCaller_TP_SVLEN,SVCaller_SVLEN_Sensitivity,
                 SVCaller_FP_SVLEN,SVCaller_SVLEN_Precision,SVCaller_SVLEN_FDR)
    names(List) <- c(paste0(SVCaller_name,"_SVTYPE"),paste0(SVCaller_name,"_TP") ,paste0(SVCaller_name,"_Sensitivity"),
                     paste0(SVCaller_name,"_TP_tmp"),paste0(SVCaller_name,"_FP"),paste0(SVCaller_name,"_Precision"),paste0(SVCaller_name,"_FDR"),
                     paste0(SVCaller_name,"_SVLEN"),paste0(SVCaller_name,"_TP_SVLEN"),paste0(SVCaller_name,"_SVLEN_Sensitivity"),
                     paste0(SVCaller_name,"_FP_SVLEN"),paste0(SVCaller_name,"_SVLEN_Precision"),paste0(SVCaller_name,"_SVLEN_FDR"))
    return(List)
  }


  SVCaller_bed_tmp <- Standard_bedtool_prepare_bkpt(SVCaller_bed,BND_diff)
  sub_directory <- sub("(Intersect|Union)","",SVCaller_name)
  setwd(directory)
  setwd(paste0("./",sub_directory))
  write.table(SVEngine_truth_tmp, "SVEngine_TumorSV2_NormalSV1.bed", quote=FALSE, sep='\t', row.names=FALSE, col.names=FALSE)
  write.table(SVCaller_bed_tmp, paste0(SVCaller_name,"_tmp.bed"), quote=FALSE, sep='\t', row.names=FALSE, col.names=FALSE)

  ### Bedtools intersect truth with tmp bed
  intersect_file <- paste0(SVCaller_name,"_SVEngine","_intersect.bed")
  overlap_f <- (BND_diff-BND_threshold)/BND_diff
  system(paste(bedtools_dir,"intersect -a SVEngine_TumorSV2_NormalSV1.bed -b",paste0(SVCaller_name,"_tmp.bed"),
               "-f",overlap_f,
               "-wo >",
               intersect_file))

  Truth_SVCaller_overlap <- Truth_Intersect_to_overlap(intersect_file,SVEngine_truth_bed,SVTYPE_ignore,is_CNVKit=FALSE)
  overlap_tmp <- Truth_SVCaller_overlap[Truth_SVCaller_overlap$Overlap,]

  TP_STAT <- SVEngine_STAT_generate(overlap_tmp)
  SVCaller_TP <- c(sum(TP_STAT[c(1,12,13)]), TP_STAT[c(2:5)], sum(TP_STAT[c(6,12)]), TP_STAT[c(7)], sum(TP_STAT[c(8,13)]),TP_STAT[c(9:16)])
  names(SVCaller_TP) <- c("SUM", "DEL", "DEL_TRA","DUP","INV","DINS", "FINS","TRA",
                          "BND","INS_BND","TRA_BND",
                          "INS_2BND_only", "TRA_2BND_only", "INS_2BND", "TRA_2BND",
                          "TRA_andDEL")
  SVCaller_Sensitivity <- SVCaller_TP/Truth_SV

  SVCaller_STAT <- Standard_STAT_generate(SVCaller_bed)
  ## same for all callers, not use for Precision calculation; more info see SVCaller_stat function
  SVCaller_SVTYPE <- SVCaller_STAT 

  intersect_filter <- TypePosfilter(intersect_file,SVTYPE_ignore)
  ## same for all SV callers, only measure SUM; for more info see SVCaller_stat function
  SVCaller_TP_tmp <- c(sum(TP_STAT[c(2:9)]),sum(TP_STAT[c(2,3)]),TP_STAT[c(4:5)],sum(TP_STAT[c(6,7,8)]),NA, TP_STAT[c(9)],rep(NA,4)) 
  names(SVCaller_TP_tmp) <- c("SUM","DEL","DUP","INV","INS","TRA","BND","RPL","CTX","ITX","DUP/INS") 
  
  FP_bed <- SVCaller_bed[!(SVCaller_bed$ID %in% intersect_filter$Caller2_ID),]
  SVCaller_FP <- Standard_STAT_generate(FP_bed)
  SVCaller_Precision <- SVCaller_TP_tmp/(SVCaller_TP_tmp+SVCaller_FP)
  SVCaller_Precision[is.na(SVCaller_SVTYPE)] = NA
  SVCaller_Precision[SVCaller_SVTYPE==0] = 0
  SVCaller_FDR <- SVCaller_FP/SVCaller_SVTYPE

  ### SVLEN
  SVCaller_SVLEN <- SVLEN_stat(SVCaller_bed,NA,test_No)
  
  SVCaller_TP_SVLEN <- SVEngine_SVLEN_stat(overlap_tmp,NA,test_No)
  SVCaller_SVLEN_Sensitivity <- SVCaller_TP_SVLEN/Truth_SVLEN

  SVCaller_FP_SVLEN <- SVLEN_stat(FP_bed,NA,test_No)
  SVCaller_SVLEN_Precision <- SVCaller_TP_SVLEN/(SVCaller_TP_SVLEN+SVCaller_FP_SVLEN)
  SVCaller_SVLEN_Precision[SVCaller_SVLEN==0] = NA
  SVCaller_SVLEN_FDR <- SVCaller_FP_SVLEN/SVCaller_SVLEN
  List <- list("SVCaller_SVTYPE" = SVCaller_SVTYPE,
               "SVCaller_TP" = SVCaller_TP,
               "SVCaller_Sensitivity" = SVCaller_Sensitivity,
               "SVCaller_TP_tmp" = SVCaller_TP_tmp,
               "SVCaller_FP" = SVCaller_FP,
               "SVCaller_Precision" = SVCaller_Precision,
               "SVCaller_FDR" = SVCaller_FDR,
               "SVCaller_SVLEN" = SVCaller_SVLEN,
               "SVCaller_TP_SVLEN" = SVCaller_TP_SVLEN,
               "SVCaller_SVLEN_Sensitivity" = SVCaller_SVLEN_Sensitivity,
               "SVCaller_FP_SVLEN" = SVCaller_FP_SVLEN,
               "SVCaller_SVLEN_Precision" = SVCaller_SVLEN_Precision,
               "SVCaller_SVLEN_FDR" = SVCaller_SVLEN_FDR)
  names(List) <- c(paste0(SVCaller_name,"_SVTYPE"),
                   paste0(SVCaller_name,"_TP"),
                   paste0(SVCaller_name,"_Sensitivity"),
                   paste0(SVCaller_name,"_TP_tmp"),
                   paste0(SVCaller_name,"_FP"),
                   paste0(SVCaller_name,"_Precision"),
                   paste0(SVCaller_name,"_FDR"),
                   paste0(SVCaller_name,"_SVLEN"),
                   paste0(SVCaller_name,"_TP_SVLEN"),
                   paste0(SVCaller_name,"_SVLEN_Sensitivity"),
                   paste0(SVCaller_name,"_FP_SVLEN"),
                   paste0(SVCaller_name,"_SVLEN_Precision"),
                   paste0(SVCaller_name,"_SVLEN_FDR"))
  return(List)
}



SVCaller_stat_generate <- function(coverage_all,VAF_all,BND_threshold_all,SVCaller_name, directory, Truth_file, test_No, segdup){
  setwd(directory)
  setwd("./SVEngine")
  SVEngine_truth_bed <- read.table(Truth_file,header = FALSE, sep="\t",stringsAsFactors=FALSE, quote="")
  colnames(SVEngine_truth_bed) <- c("chrom","start","end","SVTYPE","SVLEN","ID","ID_mate","VID.1","VID.2","INS_CHROM","INS_START","INS_END","INSLEN")
  SVEngine_truth_stat <- SVEngine_STAT_generate(SVEngine_truth_bed) 
  Truth_SVTYPE <- c(sum(SVEngine_truth_stat[c(1,3,7,8)]),sum(SVEngine_truth_stat[c(2,3)]),
                  SVEngine_truth_stat[c(4,5)],sum(SVEngine_truth_stat[c(6,7,8)]),SVEngine_truth_stat[8],SVEngine_truth_stat[9],
                  NA,NA,NA,NA) ### for precision file
  #Truth_SV <- SVEngine_STAT_generate(SVEngine_truth_bed)[c(1:11)] ### for sensitivity file
  Truth_SV <- SVEngine_STAT_generate(SVEngine_truth_bed) ### for sensitivity file
  Truth_SVLEN <- SVEngine_SVLEN_stat(SVEngine_truth_bed,NA,test_No)

  SV_Call_STAT <- c()
  for (i in c(1:length(coverage_all))){
    coverage <- paste0(segdup,coverage_all[i])
    for(j in c(1:length(VAF_all))){
      VAF <- VAF_all[j]
      for (m in c(1:length(BND_threshold_all))){
        BND_threshold <- BND_threshold_all[m]
        if(is.null(SV_Call_STAT)){
          SV_Call_STAT <- data.frame(Truth_SVTYPE,
                                     eval(parse(text=paste0(SVCaller_name,"_stat_list_",coverage,"_",as.character(VAF),"_",as.character(BND_threshold),"bp","$",SVCaller_name,"_SVTYPE"))),
                                     row.names = c("SUM","DEL","DUP","INV","INS","TRA","BND","RPL","CTX","ITX","DUP/INS") ,stringsAsFactors = FALSE)
        }else{
          SV_Call_STAT <- data.frame(SV_Call_STAT,
                                     eval(parse(text=paste0(SVCaller_name,"_stat_list_",coverage,"_",as.character(VAF),"_",as.character(BND_threshold),"bp","$",SVCaller_name,"_SVTYPE"))),
                                     row.names = c("SUM","DEL","DUP","INV","INS","TRA","BND","RPL","CTX","ITX","DUP/INS") ,stringsAsFactors = FALSE)
        }
        
        colnames(SV_Call_STAT)[ncol(SV_Call_STAT)] <- 
          paste0(SVCaller_name,"_SVTYPE_",coverage,"_",as.character(VAF),"_",as.character(BND_threshold),"bp")
      }
    }
  }
  
  SV_TP_Sensitivity <- c() 
  for (i in c(1:length(coverage_all))){
    coverage <- paste0(segdup,coverage_all[i])
    for(j in c(1:length(VAF_all))){
      VAF <- VAF_all[j]
      for (m in c(1:length(BND_threshold_all))){
        BND_threshold <- BND_threshold_all[m]
        if(is.null(SV_TP_Sensitivity)){
          SV_TP_Sensitivity <- data.frame(Truth_SV,
                                          eval(parse(text=paste0(SVCaller_name,"_stat_list_",coverage,"_",as.character(VAF),"_",as.character(BND_threshold),"bp","$",SVCaller_name,"_TP"))),
                                          eval(parse(text=paste0(SVCaller_name,"_stat_list_",coverage,"_",as.character(VAF),"_",as.character(BND_threshold),"bp","$",SVCaller_name,"_Sensitivity"))),
                                          #row.names = c("SUM","DEL","DEL_TRA","DUP","INV","DINS","FINS","TRA","BND","INS_BND","TRA_BND") ,stringsAsFactors = FALSE)
                                          row.names = c("SUM", "DEL", "DEL_TRA","DUP","INV","DINS", "FINS","TRA",
                                                        "BND","INS_BND","TRA_BND",
                                                        "INS_2BND_only", "TRA_2BND_only", "INS_2BND", "TRA_2BND",
                                                        "TRA_andDEL"), stringsAsFactors = FALSE)
        }else{
          SV_TP_Sensitivity <- data.frame(SV_TP_Sensitivity,
                                          eval(parse(text=paste0(SVCaller_name,"_stat_list_",coverage,"_",as.character(VAF),"_",as.character(BND_threshold),"bp","$",SVCaller_name,"_TP"))),
                                          eval(parse(text=paste0(SVCaller_name,"_stat_list_",coverage,"_",as.character(VAF),"_",as.character(BND_threshold),"bp","$",SVCaller_name,"_Sensitivity"))),
                                          #row.names = c("SUM","DEL","DEL_TRA","DUP","INV","DINS","FINS","TRA","BND","INS_BND","TRA_BND") ,stringsAsFactors = FALSE)
                                          row.names = c("SUM", "DEL", "DEL_TRA","DUP","INV","DINS", "FINS","TRA",
                                                        "BND","INS_BND","TRA_BND",
                                                        "INS_2BND_only", "TRA_2BND_only", "INS_2BND", "TRA_2BND",
                                                        "TRA_andDEL"), stringsAsFactors = FALSE)
        }
        
        colnames(SV_TP_Sensitivity)[(ncol(SV_TP_Sensitivity)-1):ncol(SV_TP_Sensitivity)] <- 
          c(paste0(SVCaller_name,"_TP_",coverage,"_",as.character(VAF),"_",as.character(BND_threshold),"bp"),
            paste0(SVCaller_name,"_Sensitivity_",coverage,"_",as.character(VAF),"_",as.character(BND_threshold),"bp"))
      }
    }
  }
  
  SV_TP_Precision <- c()
  for (i in c(1:length(coverage_all))){
    coverage <- paste0(segdup,coverage_all[i])
    for(j in c(1:length(VAF_all))){
      VAF <- VAF_all[j]
      for (m in c(1:length(BND_threshold_all))){
        BND_threshold <- BND_threshold_all[m]
        if(is.null(SV_TP_Precision)){
          SV_TP_Precision <- data.frame(eval(parse(text=paste0(SVCaller_name,"_stat_list_",coverage,"_",as.character(VAF),"_",as.character(BND_threshold),"bp","$",SVCaller_name,"_SVTYPE"))),
                                        eval(parse(text=paste0(SVCaller_name,"_stat_list_",coverage,"_",as.character(VAF),"_",as.character(BND_threshold),"bp","$",SVCaller_name,"_TP_tmp"))),
                                        eval(parse(text=paste0(SVCaller_name,"_stat_list_",coverage,"_",as.character(VAF),"_",as.character(BND_threshold),"bp","$",SVCaller_name,"_FP"))),
                                        eval(parse(text=paste0(SVCaller_name,"_stat_list_",coverage,"_",as.character(VAF),"_",as.character(BND_threshold),"bp","$",SVCaller_name,"_Precision"))),
                                        eval(parse(text=paste0(SVCaller_name,"_stat_list_",coverage,"_",as.character(VAF),"_",as.character(BND_threshold),"bp","$",SVCaller_name,"_FDR"))),
                                        row.names = c("SUM","DEL","DUP","INV","INS","TRA","BND","RPL","CTX","ITX","DUP/INS") ,stringsAsFactors = FALSE)
        }else{
          SV_TP_Precision <- data.frame(SV_TP_Precision,
                                        eval(parse(text=paste0(SVCaller_name,"_stat_list_",coverage,"_",as.character(VAF),"_",as.character(BND_threshold),"bp","$",SVCaller_name,"_SVTYPE"))),
                                        eval(parse(text=paste0(SVCaller_name,"_stat_list_",coverage,"_",as.character(VAF),"_",as.character(BND_threshold),"bp","$",SVCaller_name,"_TP_tmp"))),
                                        eval(parse(text=paste0(SVCaller_name,"_stat_list_",coverage,"_",as.character(VAF),"_",as.character(BND_threshold),"bp","$",SVCaller_name,"_FP"))),
                                        eval(parse(text=paste0(SVCaller_name,"_stat_list_",coverage,"_",as.character(VAF),"_",as.character(BND_threshold),"bp","$",SVCaller_name,"_Precision"))),
                                        eval(parse(text=paste0(SVCaller_name,"_stat_list_",coverage,"_",as.character(VAF),"_",as.character(BND_threshold),"bp","$",SVCaller_name,"_FDR"))),
                                        row.names = c("SUM","DEL","DUP","INV","INS","TRA","BND","RPL","CTX","ITX","DUP/INS") ,stringsAsFactors = FALSE)
        }
        colnames(SV_TP_Precision)[(ncol(SV_TP_Precision)-4):ncol(SV_TP_Precision)] <- 
          c(paste0(SVCaller_name,"_SVTYPE_",coverage,"_",as.character(VAF),"_",as.character(BND_threshold),"bp"),
            paste0(SVCaller_name,"_TPtmp_",coverage,"_",as.character(VAF),"_",as.character(BND_threshold),"bp"),
            paste0(SVCaller_name,"_FP_",coverage,"_",as.character(VAF),"_",as.character(BND_threshold),"bp"),
            paste0(SVCaller_name,"_Precision_",coverage,"_",as.character(VAF),"_",as.character(BND_threshold),"bp"),
            paste0(SVCaller_name,"_FDR_",coverage,"_",as.character(VAF),"_",as.character(BND_threshold),"bp"))
      }
    }
  }
  
  SVLEN_Call_STAT <- c()
  for (i in c(1:length(coverage_all))){
    coverage <-  paste0(segdup,coverage_all[i])
    for(j in c(1:length(VAF_all))){
      VAF <- VAF_all[j]
      for (m in c(1:length(BND_threshold_all))){
        BND_threshold <- BND_threshold_all[m]
        if(is.null(SVLEN_Call_STAT)){
          SVLEN_Call_STAT <- data.frame(Truth_SVLEN,
                                        eval(parse(text=paste0(SVCaller_name,"_stat_list_",coverage,"_",as.character(VAF),"_",as.character(BND_threshold),"bp","$",SVCaller_name,"_SVLEN"))),
                                        stringsAsFactors = FALSE)
        }else{
          SVLEN_Call_STAT <- data.frame(SVLEN_Call_STAT,
                                        eval(parse(text=paste0(SVCaller_name,"_stat_list_",coverage,"_",as.character(VAF),"_",as.character(BND_threshold),"bp","$",SVCaller_name,"_SVLEN"))),
                                        stringsAsFactors = FALSE)
        }
        colnames(SVLEN_Call_STAT)[ncol(SVLEN_Call_STAT)] <- 
          paste0(SVCaller_name,"_SVLEN_",coverage,"_",as.character(VAF),"_",as.character(BND_threshold),"bp")
      }
    }
  }
  
  SVLEN_TP_Sensitivity <- c()
  for (i in c(1:length(coverage_all))){
    coverage <- paste0(segdup,coverage_all[i])
    for(j in c(1:length(VAF_all))){
      VAF <- VAF_all[j]
      for (m in c(1:length(BND_threshold_all))){
        BND_threshold <- BND_threshold_all[m]
        if(is.null(SVLEN_TP_Sensitivity)){
          SVLEN_TP_Sensitivity <- data.frame(eval(parse(text=paste0(SVCaller_name,"_stat_list_",coverage,"_",as.character(VAF),"_",as.character(BND_threshold),"bp","$",SVCaller_name,"_TP_SVLEN"))),
                                             eval(parse(text=paste0(SVCaller_name,"_stat_list_",coverage,"_",as.character(VAF),"_",as.character(BND_threshold),"bp","$",SVCaller_name,"_SVLEN_Sensitivity"))),
                                             stringsAsFactors = FALSE)
        }else{
          SVLEN_TP_Sensitivity <- data.frame(SVLEN_TP_Sensitivity,
                                             eval(parse(text=paste0(SVCaller_name,"_stat_list_",coverage,"_",as.character(VAF),"_",as.character(BND_threshold),"bp","$",SVCaller_name,"_TP_SVLEN"))),
                                             eval(parse(text=paste0(SVCaller_name,"_stat_list_",coverage,"_",as.character(VAF),"_",as.character(BND_threshold),"bp","$",SVCaller_name,"_SVLEN_Sensitivity"))),
                                             stringsAsFactors = FALSE)
        }
        colnames(SVLEN_TP_Sensitivity)[(ncol(SVLEN_TP_Sensitivity)-1):ncol(SVLEN_TP_Sensitivity)] <- 
          c(paste0(SVCaller_name,"_SVLEN_TP_",coverage,"_",as.character(VAF),"_",as.character(BND_threshold),"bp"),
            paste0(SVCaller_name,"_SVLEN_Sensitivity_",coverage,"_",as.character(VAF),"_",as.character(BND_threshold),"bp"))
      }
    }
  }
  
  SVLEN_TP_Precision <- c()
  for (i in c(1:length(coverage_all))){
    coverage <- paste0(segdup,coverage_all[i])
    for(j in c(1:length(VAF_all))){
      VAF <- VAF_all[j]
      for (m in c(1:length(BND_threshold_all))){
        BND_threshold <- BND_threshold_all[m]
        if(is.null(SVLEN_TP_Precision)){
          SVLEN_TP_Precision <- data.frame(eval(parse(text=paste0(SVCaller_name,"_stat_list_",coverage,"_",as.character(VAF),"_",as.character(BND_threshold),"bp","$",SVCaller_name,"_SVLEN"))),
                                           eval(parse(text=paste0(SVCaller_name,"_stat_list_",coverage,"_",as.character(VAF),"_",as.character(BND_threshold),"bp","$",SVCaller_name,"_TP_SVLEN"))),
                                           eval(parse(text=paste0(SVCaller_name,"_stat_list_",coverage,"_",as.character(VAF),"_",as.character(BND_threshold),"bp","$",SVCaller_name,"_FP_SVLEN"))),
                                           eval(parse(text=paste0(SVCaller_name,"_stat_list_",coverage,"_",as.character(VAF),"_",as.character(BND_threshold),"bp","$",SVCaller_name,"_SVLEN_Precision"))),
                                           eval(parse(text=paste0(SVCaller_name,"_stat_list_",coverage,"_",as.character(VAF),"_",as.character(BND_threshold),"bp","$",SVCaller_name,"_SVLEN_FDR"))),
                                           stringsAsFactors = FALSE)
        }else{
          SVLEN_TP_Precision <- data.frame(SVLEN_TP_Precision,
                                           eval(parse(text=paste0(SVCaller_name,"_stat_list_",coverage,"_",as.character(VAF),"_",as.character(BND_threshold),"bp","$",SVCaller_name,"_SVLEN"))),
                                           eval(parse(text=paste0(SVCaller_name,"_stat_list_",coverage,"_",as.character(VAF),"_",as.character(BND_threshold),"bp","$",SVCaller_name,"_TP_SVLEN"))),
                                           eval(parse(text=paste0(SVCaller_name,"_stat_list_",coverage,"_",as.character(VAF),"_",as.character(BND_threshold),"bp","$",SVCaller_name,"_FP_SVLEN"))),
                                           eval(parse(text=paste0(SVCaller_name,"_stat_list_",coverage,"_",as.character(VAF),"_",as.character(BND_threshold),"bp","$",SVCaller_name,"_SVLEN_Precision"))),
                                           eval(parse(text=paste0(SVCaller_name,"_stat_list_",coverage,"_",as.character(VAF),"_",as.character(BND_threshold),"bp","$",SVCaller_name,"_SVLEN_FDR"))),
                                           stringsAsFactors = FALSE)
        }
        colnames(SVLEN_TP_Precision)[(ncol(SVLEN_TP_Precision)-4):ncol(SVLEN_TP_Precision)] <- 
          c(paste0(SVCaller_name,"_SVLEN_COUNT_",coverage,"_",as.character(VAF),"_",as.character(BND_threshold),"bp"),
            paste0(SVCaller_name,"_SVLEN_TP_",coverage,"_",as.character(VAF),"_",as.character(BND_threshold),"bp"),
            paste0(SVCaller_name,"_SVLEN_FP_",coverage,"_",as.character(VAF),"_",as.character(BND_threshold),"bp"),
            paste0(SVCaller_name,"_SVLEN_Precision_",coverage,"_",as.character(VAF),"_",as.character(BND_threshold),"bp"),
            paste0(SVCaller_name,"_SVLEN_FDR_",coverage,"_",as.character(VAF),"_",as.character(BND_threshold),"bp"))
      }
    }
  }
  
  # write.csv(SV_Call_STAT, paste0(SVCaller_name,"_SV_Call_STAT.csv"),row.names = TRUE)
  # write.csv(SV_TP_Sensitivity, paste0(SVCaller_name,"_SV_TP_Sensitivity.csv"),row.names = TRUE)
  # write.csv(SV_TP_Precision, paste0(SVCaller_name,"_SV_TP_Precision.csv"),row.names = TRUE)
  # write.csv(SVLEN_Call_STAT,paste0(SVCaller_name,"_SVLEN_Call_STAT.csv"),row.names = TRUE)
  # write.csv(SVLEN_TP_Sensitivity,paste0(SVCaller_name,"_SVLEN_TP_Sensitivity.csv"),row.names = TRUE)
  # write.csv(SVLEN_TP_Precision,paste0(SVCaller_name,"_SVLEN_TP_Precision.csv"),row.names = TRUE)
  
  list <- list(SV_Call_STAT=SV_Call_STAT,
               SV_TP_Sensitivity=SV_TP_Sensitivity,
               SV_TP_Precision=SV_TP_Precision,
               SVLEN_Call_STAT=SVLEN_Call_STAT,
               SVLEN_TP_Sensitivity=SVLEN_TP_Sensitivity,
               SVLEN_TP_Precision=SVLEN_TP_Precision)
  return(list)
}


summary_stat_generate <- function(coverage_all,VAF_all,BND_threshold_all,SVCaller_name,T_coverage,N_coverage,
                                  SVTYPE_Call_STAT,SVTYPE_TP_Sensitivity,SVTYPE_TP_Precision){
  summary_stat <- c()
  for (i in c(1:length(coverage_all))){
    coverage <- coverage_all[i]
    for(j in c(1:length(VAF_all))){
      VAF <- VAF_all[j]
      for (m in c(1:length(BND_threshold_all))){
        BND_threshold <- BND_threshold_all[m]
        tmp <- c(Sensitivity = eval(parse(text=paste0("SVTYPE_TP_Sensitivity$",SVCaller_name,"_Sensitivity_",coverage,"_",as.character(VAF),"_",as.character(BND_threshold),"bp[1]"))),
                 Precision = eval(parse(text=paste0("SVTYPE_TP_Precision$",SVCaller_name,"_Precision_",coverage,"_",as.character(VAF),"_",as.character(BND_threshold),"bp[1]"))),
                 TP = eval(parse(text=paste0("SVTYPE_TP_Sensitivity$",SVCaller_name,"_TP_",coverage,"_",as.character(VAF),"_",as.character(BND_threshold),"bp[1]"))),
                 Truth = eval(parse(text=paste0("SVTYPE_TP_Sensitivity$Truth_SV[1]"))),
                 TP_tmp = eval(parse(text=paste0("SVTYPE_TP_Precision$",SVCaller_name,"_TPtmp_",coverage,"_",as.character(VAF),"_",as.character(BND_threshold),"bp[1]"))),
                 FP = eval(parse(text=paste0("SVTYPE_TP_Precision$",SVCaller_name,"_FP_",coverage,"_",as.character(VAF),"_",as.character(BND_threshold),"bp[1]"))),
                 T_coverage = T_coverage[i],
                 N_coverage = N_coverage[i],
                 VAF = VAF,
                 BND_threshold = BND_threshold)
        summary_stat <- as.data.frame(rbind(summary_stat,tmp))
        colnames(summary_stat) <- c("Sensitivity","Precision","TP","Truth","TP_tmp","FP","T_coverage","N_coverage","VAF","BND_threshold")
      }
    }
  }
  return(summary_stat)
}

SVCaller_stat <- function(vcf_file,overlap_file,intersect_file,directory,Truth_file,SVTYPE_ignore,test_No,is_SegDup, SVCaller_name,bedtools_dir){
  setwd(directory)
  setwd("./SVEngine")
  SVEngine_truth_bed <- read.table(Truth_file,header = FALSE, sep="\t",stringsAsFactors=FALSE, quote="")
  colnames(SVEngine_truth_bed) <- c("chrom","start","end","SVTYPE","SVLEN","ID","ID_mate","VID.1","VID.2","INS_CHROM","INS_START","INS_END","INSLEN")
  Truth_SV <- SVEngine_STAT_generate(SVEngine_truth_bed)
  #Truth_SV <- SVEngine_STAT_generate(SVEngine_truth_bed)[c(1:11)]
  Truth_SVLEN <- SVEngine_SVLEN_stat(SVEngine_truth_bed,NA,test_No)
  
  setwd(directory)
  setwd(paste0("./",SVCaller_name))
  
  if(SVCaller_name == "BreakDancer"){
    assign(paste0(SVCaller_name,"_vcf"),readLines(vcf_file))
    nrow_in_vcf <- length(eval(parse(text=paste0(SVCaller_name,"_vcf"))))
  }else{
    assign(paste0(SVCaller_name,"_vcf"),readVcf(vcf_file))
    nrow_in_vcf <- nrow(info(eval(parse(text=paste0(SVCaller_name,"_vcf")))))
  }
  ### check if the vcf file is empty
  if(nrow_in_vcf == 0){
    SVCaller_SVTYPE <- c(0,0,0,0,0,NA,0,NA,NA,NA,NA)
    SVCaller_TP <- rep(0,16)
    SVCaller_Sensitivity <- rep(0,16)
    SVCaller_TP_tmp <- SVCaller_SVTYPE
    SVCaller_FP <- SVCaller_SVTYPE
    SVCaller_Precision <- rep(NA,11)
    SVCaller_FDR <- rep(NA,11)
    SVCaller_SVLEN <- rep(0,12)
    SVCaller_TP_SVLEN <- rep(0,12)
    SVCaller_SVLEN_Sensitivity <- rep(0,12)
    SVCaller_FP_SVLEN <- rep(0,12)
    SVCaller_SVLEN_Precision <- rep(NA,12)
    SVCaller_SVLEN_FDR <- rep(NA,12)
    List <- list(SVCaller_SVTYPE,SVCaller_TP,SVCaller_Sensitivity,
                 SVCaller_TP_tmp,SVCaller_FP,SVCaller_Precision,SVCaller_FDR,
                 SVCaller_SVLEN,SVCaller_TP_SVLEN,SVCaller_SVLEN_Sensitivity,
                 SVCaller_FP_SVLEN,SVCaller_SVLEN_Precision,SVCaller_SVLEN_FDR)
    names(List) <- c(paste0(SVCaller_name,"_SVTYPE"),paste0(SVCaller_name,"_TP") ,paste0(SVCaller_name,"_Sensitivity"),
                     paste0(SVCaller_name,"_TP_tmp"),paste0(SVCaller_name,"_FP"),paste0(SVCaller_name,"_Precision"),paste0(SVCaller_name,"_FDR"),
                     paste0(SVCaller_name,"_SVLEN"),paste0(SVCaller_name,"_TP_SVLEN"),paste0(SVCaller_name,"_SVLEN_Sensitivity"),
                     paste0(SVCaller_name,"_FP_SVLEN"),paste0(SVCaller_name,"_SVLEN_Precision"),paste0(SVCaller_name,"_SVLEN_FDR"))
    return(List)
  }
  
  ### compute SVCaller SV in segdup regions
  if(is_SegDup){
    SVCaller_bed = SVCaller_bed_newID_generate(eval(parse(text=paste0(SVCaller_name,"_vcf"))),SVCaller_name)
    SVCaller_bed_tmp <- SVCaller_bed[,c(1:9)]
    SVCaller_bed_tmp[is.na(SVCaller_bed_tmp$end),]$end <- SVCaller_bed_tmp[is.na(SVCaller_bed_tmp$end),]$start + 1
    write.table(SVCaller_bed_tmp, "SVCaller_bed_tmp.bed", quote=FALSE, sep='\t', row.names=FALSE, col.names=FALSE)
    system(paste0(bedtools_dir, " intersect -a SVCaller_bed_tmp.bed -b ../../hg38_genomicSuperDups.sorted.merged.bed -wo > SVCaller_segdup_intersect.bed"))
    if(file.info("SVCaller_segdup_intersect.bed")$size == 0){
      SVCaller_SVTYPE <- c(0,0,0,0,0,NA,0,NA,NA,NA,NA)
      SVCaller_TP <- rep(0,16)
      SVCaller_Sensitivity <- rep(0,16)
      SVCaller_TP_tmp <- SVCaller_SVTYPE
      SVCaller_FP <- SVCaller_SVTYPE
      SVCaller_Precision <- rep(NA,11)
      SVCaller_FDR <- rep(NA,11)
      SVCaller_SVLEN <- rep(0,12)
      SVCaller_TP_SVLEN <- rep(0,12)
      SVCaller_SVLEN_Sensitivity <- rep(0,12)
      SVCaller_FP_SVLEN <- rep(0,12)
      SVCaller_SVLEN_Precision <- rep(NA,12)
      SVCaller_SVLEN_FDR <- rep(NA,12)
      List <- list(SVCaller_SVTYPE,SVCaller_TP,SVCaller_Sensitivity,
                   SVCaller_TP_tmp,SVCaller_FP,SVCaller_Precision,SVCaller_FDR,
                   SVCaller_SVLEN,SVCaller_TP_SVLEN,SVCaller_SVLEN_Sensitivity,
                   SVCaller_FP_SVLEN,SVCaller_SVLEN_Precision,SVCaller_SVLEN_FDR)
      names(List) <- c(paste0(SVCaller_name,"_SVTYPE"),paste0(SVCaller_name,"_TP") ,paste0(SVCaller_name,"_Sensitivity"),
                       paste0(SVCaller_name,"_TP_tmp"),paste0(SVCaller_name,"_FP"),paste0(SVCaller_name,"_Precision"),paste0(SVCaller_name,"_FDR"),
                       paste0(SVCaller_name,"_SVLEN"),paste0(SVCaller_name,"_TP_SVLEN"),paste0(SVCaller_name,"_SVLEN_Sensitivity"),
                       paste0(SVCaller_name,"_FP_SVLEN"),paste0(SVCaller_name,"_SVLEN_Precision"),paste0(SVCaller_name,"_SVLEN_FDR"))
      return(List)
    }
  }
  
  ### SV TYPE COUNT for SVCaller
  SVCaller_bed = SVCaller_bed_newID_generate(eval(parse(text=paste0(SVCaller_name,"_vcf"))),SVCaller_name)
  if(is_SegDup){
    SVCaller_segdup_intersect.bed <- read.table("SVCaller_segdup_intersect.bed",header = FALSE, sep="\t",stringsAsFactors=FALSE, quote="")
    colnames(SVCaller_segdup_intersect.bed) <- c("chrom","start","end","SVTYPE","SVLEN","ID","ID_mate","SVCallerID","SVCallerID_mate",
                                                 "CHROM","START","END","overlap")
    index <- (SVCaller_segdup_intersect.bed$START > SVCaller_segdup_intersect.bed$start) & (SVCaller_segdup_intersect.bed$END < SVCaller_segdup_intersect.bed$end)
    SVCaller_bkpt_segdup_intersect.bed <- SVCaller_segdup_intersect.bed[!index,]
    SVCaller_segdup.bed <-
      SVCaller_bed[str_split_fixed(SVCaller_bed$ID,"_",4)[,4] %in% str_split_fixed(SVCaller_bkpt_segdup_intersect.bed$ID,"_",4)[,4],]
    SVCaller_bed <- SVCaller_segdup.bed
  }
  SVCaller_STAT <- Standard_STAT_generate(SVCaller_bed)
  if (SVCaller_name == "Lumpy"){
    SVCaller_SVTYPE <- c(SVCaller_STAT[c(1:4)],NA,NA,SVCaller_STAT[7],NA,NA,NA,NA)
  }else if (SVCaller_name == "BreakDancer"){
    SVCaller_SVTYPE <- c(SVCaller_STAT[c(1,2)],NA,SVCaller_STAT[c(4,5)],rep(NA,3),SVCaller_STAT[c(9,10)],NA)
  }else if (SVCaller_name == "CNVKit"){
    SVCaller_SVTYPE <- c(SVCaller_STAT[c(1:3)],rep(NA,8))
  }else if (SVCaller_name == "Pindel"){
    SVCaller_SVTYPE <- c(SVCaller_STAT[c(1:5)],NA,NA,SVCaller_STAT[8],NA,NA,NA)
  }else if (SVCaller_name == "SvABA"){
    SVCaller_SVTYPE <- c(SVCaller_STAT[c(1:2)],NA,SVCaller_STAT[4],NA,NA,SVCaller_STAT[7],NA,NA,NA,SVCaller_STAT[11])
  }else{
    SVCaller_SVTYPE <- c(SVCaller_STAT[c(1:5)],NA,SVCaller_STAT[7],NA,NA,NA,NA)
  }
  
  ### SV TYPE Sensitivity
  Truth_SVCaller_overlap <- read.table(overlap_file,header = FALSE, sep="\t",stringsAsFactors=FALSE, quote="")
  colnames(Truth_SVCaller_overlap) <- c("chrom","start","end","SVTYPE","SVLEN","ID","ID_mate","INSLEN","Overlap","Caller_ID","Caller_SVTYPE","Caller_SVLEN","Freq_Truth")
  if(sum(is.na(Truth_SVCaller_overlap$Caller_ID)) == nrow(Truth_SVCaller_overlap)){
    overlap_tmp <- Truth_SVCaller_overlap[Truth_SVCaller_overlap$Overlap,]
    TP_STAT <- rep(0,16)
    SVCaller_TP <- rep(0,16)
    SVCaller_Sensitivity <- rep(0,16)
  }else{
    overlap_tmp <- Truth_SVCaller_overlap[Truth_SVCaller_overlap$Overlap,]
    #overlap_tmp <- overlap_tmp[!duplicated(overlap_tmp$Caller_ID),]  # Check if SV caller unique & remove duplicated ones
    TP_STAT <- SVEngine_STAT_generate(overlap_tmp)
    SVCaller_TP <- c(sum(TP_STAT[c(1,12,13)]), TP_STAT[c(2:5)], sum(TP_STAT[c(6,12)]), TP_STAT[c(7)], sum(TP_STAT[c(8,13)]),TP_STAT[c(9:16)])
    SVCaller_Sensitivity <- SVCaller_TP/Truth_SV
  }
  names(SVCaller_TP) <- c("SUM", "DEL", "DEL_TRA","DUP","INV","DINS", "FINS","TRA",
                            "BND","INS_BND","TRA_BND",
                            "INS_2BND_only", "TRA_2BND_only", "INS_2BND", "TRA_2BND",
                            "TRA_andDEL")
  ### SV TYPE precision
  if (SVCaller_name == "BreakDancer"){
    SVCaller_TP_tmp <- c(sum(TP_STAT[c(2:8,10,11)]),sum(TP_STAT[c(2,3)]),NA,TP_STAT[5],sum(TP_STAT[c(6,7,8)]),rep(NA,3),sum(TP_STAT[c(10,11)]),TP_STAT[4],NA)
  }else if(SVCaller_name == "CNVKit"){ ##not use
    SVCaller_TP_tmp <- c(sum(TP_STAT[c(2:4)]),sum(TP_STAT[c(2,3)]),TP_STAT[c(4:5)],sum(TP_STAT[c(6,7)]),TP_STAT[c(8:9)],rep(NA,4))  ## not use
  }else if(SVCaller_name == "Pindel"){
    N_DUP <- sum(overlap_tmp$SVTYPE=="DUP" & overlap_tmp$Caller_SVTYPE == "DUP")
    N_INV <- sum(overlap_tmp$SVTYPE=="INV" & overlap_tmp$Caller_SVTYPE == "INV")
    N_INS <- sum(overlap_tmp$SVTYPE=="INS" & overlap_tmp$Caller_SVTYPE == "INS")
    
    N_RPL <- sum(overlap_tmp$SVTYPE=="DUP" & overlap_tmp$Caller_SVTYPE == "RPL")+
            sum(overlap_tmp$SVTYPE=="INV" & overlap_tmp$Caller_SVTYPE == "RPL")+
            sum(overlap_tmp$SVTYPE=="INS" & overlap_tmp$Caller_SVTYPE == "RPL")
    
    SVCaller_TP_tmp <- c(sum(TP_STAT[c(2:8)]),sum(TP_STAT[c(2,3)]),N_DUP,N_INV,N_INS,rep(NA,2),N_RPL,rep(NA,3))
  }else if (SVCaller_name == "SvABA"){
    SVCaller_TP_tmp <- c(sum(TP_STAT[c(2:9)]),sum(TP_STAT[c(2,3)]),NA,TP_STAT[5],NA, NA, TP_STAT[c(9)],NA,NA,NA,sum(TP_STAT[c(4,6,7,8)]))
  #}else if (SVCaller_name == "Lumpy"){
    #SVCaller_TP_tmp <-  c(sum(TP_STAT[c(2:9)]),sum(TP_STAT[c(2,3)]),TP_STAT[c(4:5)],NA,NA,TP_STAT[c(9)],rep(NA,4))
  }else{
    SVCaller_TP_tmp <- c(sum(TP_STAT[c(2:9)]),sum(TP_STAT[c(2,3)]),TP_STAT[c(4:5)],sum(TP_STAT[c(6,7,8)]),NA,TP_STAT[c(9)],rep(NA,4))
  }
  names(SVCaller_TP_tmp) <- c("SUM","DEL","DUP","INV","INS","TRA","BND","RPL","CTX","ITX","DUP/INS") 
  
  if(file.info(intersect_file)$size == 0){
    FP_bed <- SVCaller_bed
  }else{
    intersect_filter <- TypePosfilter(intersect_file,SVTYPE_ignore)
    FP_bed <- SVCaller_bed[!(SVCaller_bed$ID %in% intersect_filter$Caller2_ID),]
  }
  SVCaller_FP <- Standard_STAT_generate(FP_bed)
  
  SVCaller_Precision <- SVCaller_TP_tmp/(SVCaller_TP_tmp+SVCaller_FP)
  SVCaller_Precision[is.na(SVCaller_SVTYPE)] = NA
  SVCaller_Precision[SVCaller_SVTYPE==0] = 0
  SVCaller_FDR <- SVCaller_FP/SVCaller_SVTYPE
  
  ### SVLEN sensitivity and precision
  SVCaller_SVLEN <- SVLEN_stat(SVCaller_bed,NA,test_No)
  if(sum(is.na(Truth_SVCaller_overlap$Caller_ID)) == nrow(Truth_SVCaller_overlap)){
    SVCaller_TP_SVLEN <- rep(0,length(SVCaller_SVLEN))
  }else{
    SVCaller_TP_SVLEN <- SVEngine_SVLEN_stat(overlap_tmp,NA,test_No)
  }
  SVCaller_SVLEN_Sensitivity <- SVCaller_TP_SVLEN/Truth_SVLEN
  
  SVCaller_FP_SVLEN <- SVLEN_stat(FP_bed,NA,test_No)
  SVCaller_SVLEN_Precision <- SVCaller_TP_SVLEN/(SVCaller_TP_SVLEN+SVCaller_FP_SVLEN)
  SVCaller_SVLEN_Precision[SVCaller_SVLEN==0] = NA
  SVCaller_SVLEN_FDR <- SVCaller_FP_SVLEN/SVCaller_SVLEN
  
  List <- list(SVCaller_SVTYPE,SVCaller_TP,SVCaller_Sensitivity,
               SVCaller_TP_tmp,SVCaller_FP,SVCaller_Precision,SVCaller_FDR,
               SVCaller_SVLEN,SVCaller_TP_SVLEN,SVCaller_SVLEN_Sensitivity,
               SVCaller_FP_SVLEN,SVCaller_SVLEN_Precision,SVCaller_SVLEN_FDR)
  
  names(List) <- c(paste0(SVCaller_name,"_SVTYPE"),paste0(SVCaller_name,"_TP") ,paste0(SVCaller_name,"_Sensitivity"),
                   paste0(SVCaller_name,"_TP_tmp"),paste0(SVCaller_name,"_FP"),paste0(SVCaller_name,"_Precision"),paste0(SVCaller_name,"_FDR"),
                   paste0(SVCaller_name,"_SVLEN"),paste0(SVCaller_name,"_TP_SVLEN"),paste0(SVCaller_name,"_SVLEN_Sensitivity"),
                   paste0(SVCaller_name,"_FP_SVLEN"),paste0(SVCaller_name,"_SVLEN_Precision"),paste0(SVCaller_name,"_SVLEN_FDR"))
  
  return(List)
}


SVCaller_evaluation <- function(Truth_file,vcf_file,SVCaller_name,BND_diff,BND_threshold,coverage,VAF,directory,bedtools_dir){
  setwd(directory)
  setwd(paste0("./SVEngine"))
  SVEngine_truth_bed <- read.table(Truth_file,header = FALSE, sep="\t",stringsAsFactors=FALSE, quote="")
  colnames(SVEngine_truth_bed) <- c("chrom","start","end","SVTYPE","SVLEN","ID","ID_mate","VID.1","VID.2","INS_CHROM","INS_START","INS_END","INSLEN")
  
  setwd(directory)
  setwd(paste0("./",SVCaller_name))
  if(SVCaller_name == "BreakDancer"){
    assign(paste0(SVCaller_name,"_vcf"),readLines(vcf_file))
    nrow_in_vcf <- length(eval(parse(text=paste0(SVCaller_name,"_vcf"))))
  }else{
    assign(paste0(SVCaller_name,"_vcf"),readVcf(vcf_file))
    nrow_in_vcf <- nrow(info(eval(parse(text=paste0(SVCaller_name,"_vcf")))))
  }
  if(nrow_in_vcf == 0){
    assign(paste0("Truth_",SVCaller_name,"_overlap"), cbind(SVEngine_truth_bed[,c(1:7,13)],NA,NA,NA,NA,NA))
  }else{
    assign(paste0("Truth_",SVCaller_name,"_overlap"),
           SVEngine_SVCaller_overlap(eval(parse(text=paste0(SVCaller_name,"_vcf"))),SVCaller_name,SVEngine_truth_bed,BND_diff,BND_threshold,coverage,VAF,bedtools_dir))
  } 
  write.table(eval(parse(text=paste0("Truth_",SVCaller_name,"_overlap"))), 
              paste0("Truth_",coverage,"_",as.character(VAF),"_",as.character(BND_threshold),"bp","_",SVCaller_name,"_overlap.bed"), 
              quote=FALSE, sep='\t', row.names=FALSE, col.names=FALSE)
  return(eval(parse(text=paste0("Truth_",SVCaller_name,"_overlap"))))
}

SVEngine_SVCaller_overlap <- function(SVCaller_vcf,SVCaller_name,SVEngine_truth_bed,BND_diff,BND_threshold,coverage,VAF,bedtools_dir){
  SVCaller_bed <- SVCaller_bed_newID_generate(SVCaller_vcf,SVCaller_name)
  SVCaller_bed_tmp <- Standard_bedtool_prepare_bkpt(SVCaller_bed,BND_diff)
  write.table(SVCaller_bed_tmp, paste0(SVCaller_name,"_SVEngine_TumorSV2_NormalSV1.bed"), quote=FALSE, sep='\t', row.names=FALSE, col.names=FALSE)
  
  SVEngine_truth_tmp <- Standard_bedtool_prepare_bkpt(SVEngine_truth_bed,BND_diff)
  write.table(SVEngine_truth_tmp, "SVEngine_TumorSV2_NormalSV1.bed", quote=FALSE, sep='\t', row.names=FALSE, col.names=FALSE)
  #d <- as.character(BND_diff-BND_diff*overlap_f)
  overlap_f <- (BND_diff-BND_threshold)/BND_diff
  intersect_file <- paste0(SVCaller_name,"_SVEngine_",coverage,"_",as.character(VAF),"_",as.character(BND_threshold),"bp","_intersect.bed")
  
  system(paste(bedtools_dir,"intersect -a SVEngine_TumorSV2_NormalSV1.bed -b",paste0(SVCaller_name,"_SVEngine_TumorSV2_NormalSV1.bed"),
               "-f",overlap_f,
               "-wo >",
               intersect_file))
  
  Truth_SVCaller_overlap <- Truth_Intersect_to_overlap(intersect_file,SVEngine_truth_bed,SVTYPE_ignore,is_CNVKit=FALSE)
  return(Truth_SVCaller_overlap)
}

TypePosfilter <- function(intersect_file, SVTYPE_ignore){
  intersect <- read.table(intersect_file,header = FALSE, sep="\t",stringsAsFactors=FALSE, quote="")
  if(ncol(intersect) == 16){
    colnames(intersect) <- c("Caller1_CHROM", "Caller1_POS","Caller1_END","Caller1_SVTYPE","Caller1_SVLEN","Caller1_ID","Caller1_ID_mate","Caller2",
                             "Caller2_CHROM", "Caller2_POS","Caller2_END","Caller2_SVTYPE","Caller2_SVLEN","Caller2_ID","Caller2_ID_mate","overlap")
  }else{
    colnames(intersect) <- c("Caller1_CHROM", "Caller1_POS","Caller1_END","Caller1_SVTYPE","Caller1_SVLEN","Caller1_ID","Caller1_ID_mate",
                             "Caller2_CHROM", "Caller2_POS","Caller2_END","Caller2_SVTYPE","Caller2_SVLEN","Caller2_ID","Caller2_ID_mate","overlap")
  }
  
  if(SVTYPE_ignore){
    intersect_Typefilter <- intersect
  }else{
    intersect_Typefilter <- intersect[(intersect$Caller1_SVTYPE == intersect$Caller2_SVTYPE)| 
                                        (grepl("BND",intersect$Caller1_SVTYPE) & grepl("BND",intersect$Caller2_SVTYPE))|
                                        (grepl("DEL",intersect$Caller1_SVTYPE) & grepl("DEL",intersect$Caller2_SVTYPE))|
                                        (grepl("INS",intersect$Caller1_SVTYPE) & grepl("INS",intersect$Caller2_SVTYPE))|
                                        (grepl("DUP",intersect$Caller1_SVTYPE) & grepl("DUP",intersect$Caller2_SVTYPE))|
                                        grepl("RPL",intersect$Caller2_SVTYPE)|
                                        (grepl("DUP",intersect$Caller1_SVTYPE) & grepl("ITX",intersect$Caller2_SVTYPE))
                                      #grepl("CTX",intersect$Caller2_SVTYPE)
                                      ,]
  }
  if(nrow(intersect_Typefilter)!=0){
    ### Check Caller_1 ID_mate also listed in Caller_1 ID
    tmp_index <- lapply(intersect_Typefilter$Caller1_ID_mate,function(x) which(x==intersect_Typefilter$Caller1_ID))
    BND_ID_match <- vector(length=nrow(intersect_Typefilter))
    ### Check Caller_2 ID_mate also listed in Caller_2 ID, which matched with Caller_1 ID
    for (i in 1: nrow(intersect_Typefilter)){
      BND_ID_match[i] <- intersect_Typefilter$Caller2_ID_mate[i] %in% intersect_Typefilter$Caller2_ID[tmp_index[[i]]]
    }
    intersect_TypePosBNDfilter <- intersect_Typefilter[BND_ID_match,]
  }else{
    intersect_TypePosBNDfilter <- intersect_Typefilter
  }
  return(intersect_TypePosBNDfilter)
}


Truth_Intersect_to_overlap <- function(intersect_file, Truth_bed, SVTYPE_ignore,is_CNVKit){
  if(file.info(intersect_file)$size != 0){
    if(is_CNVKit){
      intersect_filter <- CNVKit_TypePosfilter(intersect_file,SVTYPE_ignore)
    }else{
      intersect_filter <- TypePosfilter(intersect_file,SVTYPE_ignore)
    }
    Freq <- data.frame(table(intersect_filter$Caller1_ID))$Freq
    ID <- data.frame(table(intersect_filter$Caller1_ID))$Var1
    Freq_Truth <- Freq[match(intersect_filter$Caller1_ID, ID)]
    
    intersect_filter <- cbind(intersect_filter,Freq_Truth)
    intersect_filter_sort <- intersect_filter[order(intersect_filter$overlap, decreasing=TRUE),]
    intersect_filter_unique <- intersect_filter_sort[!duplicated(intersect_filter_sort$Caller1_ID),]
    intersect_ID <- intersect_filter_unique$Caller1_ID
    Overlap <- Truth_bed$ID %in% intersect_ID
    
    Caller_ID <- intersect_filter_unique$Caller2_ID[match(Truth_bed$ID,intersect_ID)]
    Caller_SVTYPE <- intersect_filter_unique$Caller2_SVTYPE[match(Truth_bed$ID,intersect_ID)]
    Caller_SVLEN <- intersect_filter_unique$Caller2_SVLEN[match(Truth_bed$ID,intersect_ID)]
    
    Freq_SVEngine <- rep(0,nrow(Truth_bed))
    Freq_SVEngine[Overlap] <- intersect_filter_unique$Freq_Truth[match(Truth_bed$ID,intersect_ID)[Overlap]]
    
    Truth_overlap_bed <- cbind(Truth_bed[,c(1:7,13)],Overlap,Caller_ID,Caller_SVTYPE,Caller_SVLEN,Freq_SVEngine)
  }else{
    Truth_overlap_bed <- cbind(Truth_bed[,c(1:7,13)],NA,NA,NA,NA,NA)
  } 
  return(Truth_overlap_bed)
}

Standard_bed_generate <- function(vcf){
  info = info(vcf)
  gr <- rowRanges(vcf)
  
  idx <- !(sapply(info$SVLEN, length))
  info$SVLEN[idx] <- NA
  info$SVLEN <- unlist(info$SVLEN)
  
  idx <- !(sapply(info$END, length))
  info$END[idx] <- NA
  info$END <- unlist(info$END)
  
  idx <- !(sapply(info$MATEID, length))
  if (isEmpty(idx)){
    info$MATEID <- NA
  }else{
    info$MATEID[idx] <- NA
    info$MATEID <- unlist(info$MATEID)
  }
  
  if(is.null(info$EVENT)){
    info$EVENT <- NA
  }
  if(is.null(info$SVINSLEN)){
    info$SVINSLEN <- NA
  }
  
  #idx <- !(sapply(info$NTLEN, length))
  #info$NTLEN[idx] <- NA
  #info$NTLEN <- unlist(info$NTLEN)
  
  bed <- data.frame(
    chrom=seqnames(gr),
    start=start(gr),
    end=info$END,
    SVTYPE=ifelse(info$SVTYPE=="DUP:TANDEM","DUP",info$SVTYPE),
    SVLEN=info$SVLEN,
    MantaID = names(gr),
    MantaID_mate = info$MATEID,
    event = info$EVENT,
    REF = gr$REF,
    ALT = unlist(gr$ALT),
    #INSLEN=info$SVINSLEN,
    #INSLEN=info$NTLEN@partitioning@end,
    stringsAsFactors = FALSE
  )
  
  return(bed)
}

Delly_bedpe_generate <- function(vcf){
  info = info(vcf)
  gr <- rowRanges(vcf)
  bedpe <- data.frame(
    CHROM1=as.character(seqnames(gr)),
    POS1=start(gr),
    CHROM2=info$CHR2,
    POS2=info$END,
    ID=names(gr),
    ID_mate=NA,
    SVTYPE=ifelse(info$SVTYPE=="DUP:TANDEM","DUP",info$SVTYPE),
    SVLEN=ifelse(info$SVTYPE=="BND",NA,info$END-start(gr)),
    INSLEN=info$INSLEN,
    REF=as.character(gr$REF),
    ALT=unlist(gr$ALT),
    stringsAsFactors = FALSE
  )
  if(sum(bedpe$SVTYPE=="BND")!=0){ bedpe[bedpe$SVTYPE=="BND",]$ID_mate <- paste0(bedpe[bedpe$SVTYPE=="BND",]$ID,"_mate")}
  return(bedpe)
}


Delly_simplebed_generate <- function(bedpe){
  bed_non_TRA <- data.frame(
    chrom = bedpe$CHROM1,
    start = bedpe$POS1,
    end = bedpe$POS2,
    SVTYPE = bedpe$SVTYPE,
    SVLEN = ifelse(bedpe$SVTYPE=="INS",bedpe$INSLEN,bedpe$SVLEN),
    ID = bedpe$ID,
    ID_mate = bedpe$ID_mate,
    ALT = bedpe$ALT
  )
  return(bed_non_TRA)
}

Delly_BNDbed_generate <- function(bedpe){
  bed_TRA <- data.frame(
    chrom = c(bedpe$CHROM1,bedpe$CHROM2),
    start = c(bedpe$POS1,bedpe$POS2),
    end = NA,
    SVTYPE = c(bedpe$SVTYPE,bedpe$SVTYPE),
    SVLEN = NA,
    ID = c(bedpe$ID,bedpe$ID_mate),
    ID_mate = c(bedpe$ID_mate,bedpe$ID),
    ALT = c(bedpe$ALT,bedpe$ALT)
  )
  return(bed_TRA)
}


Delly_bed_generate <- function(vcf){
  bedpe <- Delly_bedpe_generate(vcf)
  bedpe_non_TRA <- bedpe[bedpe$SVTYPE!="BND",]
  bed_non_TRA <- Delly_simplebed_generate(bedpe_non_TRA)
  bedpe_TRA <- bedpe[bedpe$SVTYPE=="BND",]
  ifelse(nrow(bedpe_TRA)!=0,bed_TRA <- Delly_BNDbed_generate(bedpe_TRA),bed_TRA <- data.frame())
  Delly_bed <- rbind(bed_non_TRA,bed_TRA)
  return(Delly_bed)
}

### Simple SV type classifier function
simpleEventType <- function(gr) {
  return(ifelse(seqnames(gr) != seqnames(partner(gr)), "BND", # inter-chromosomosal if chromosome No. not match
                ifelse(gr$insLen >= abs(gr$svLen) * 0.7, "INS", # insert length greater than 0.7*svlen
                       ifelse(strand(gr) == strand(partner(gr)), "INV", #check paired reads with same direction
                              ifelse(xor(start(gr) < start(partner(gr)), strand(gr) == "-"), "DEL",
                                     "DUP")))))
}

bedpe_generate <- function(vcf){
  gr <- breakpointRanges(vcf)
  info <- info(vcf)
  bedpe <- data.frame(
    chrom1=seqnames(gr),
    start1=start(gr) - 1,
    end1=end(gr),
    chrom2=seqnames(partner(gr)),
    start2=start(partner(gr)) - 1,
    end2=end(partner(gr)),
    name=names(gr),
    score=gr$QUAL,
    strand1=strand(gr),
    strand2=strand(partner(gr)),
    SVTYPE=simpleEventType(gr),
    svlen=gr$svLen,
    inslen=gr$insLen,
    mate_name=names(partner(gr)),
    event=info$EVENT[rownames(info) %in% names(gr)],
    REF=gr$REF,
    ALT=gr$ALT)
  return(bedpe)
}

simplebed_generate <- function(bedpe){
  simplebed <- data.frame(
    chrom = bedpe$chrom1,
    start = as.integer((bedpe$start1+bedpe$end1)/2),
    end = as.integer((bedpe$start2+bedpe$end2)/2),
    SVTYPE = bedpe$SVTYPE,
    SVLEN = bedpe$svlen,
    ID = bedpe$name,
    ID_mate = bedpe$mate_name,
    event=bedpe$event,
    REF=bedpe$REF,
    ALT=bedpe$ALT,
    INSLEN = bedpe$inslen
  )
  return(simplebed)
}

ITXbed_generate <- function(bedpe){
  ITXbed1 <- data.frame(
    chrom = bedpe$chrom1,
    start = as.integer((bedpe$start1+bedpe$end1)/2),
    end = NA,
    SVTYPE = bedpe$SVTYPE,
    SVLEN = bedpe$svlen,
    ID = bedpe$name,
    ID_mate = bedpe$mate_name,
    event=bedpe$event,
    REF=bedpe$REF,
    ALT=bedpe$ALT,
    INSLEN = bedpe$inslen
  )
  return(ITXbed1)
}


GRIDSS_bed_generate <- function(vcf){
  # gr <- breakpointRanges(vcf) 
  bedpe <- bedpe_generate(vcf)
  # Only ID with gridss.+o for non-ITX event and Both for ITX event
  bedpe_tmp <- bedpe[(str_detect(bedpe$name, "gridss.+o") & bedpe$SVTYPE!="BND")|bedpe$SVTYPE=="BND",]
  bedpe_non_ITX <- bedpe_tmp[bedpe_tmp$SVTYPE!="BND",]
  simplebed <- simplebed_generate(bedpe_non_ITX)
  bedpe_ITX <- bedpe_tmp[bedpe_tmp$SVTYPE=="BND",]
  ITXbed <- ITXbed_generate(bedpe_ITX)
  GRIDSS_bed <- rbind(simplebed,ITXbed)
  return(GRIDSS_bed)
}

SvABA_bedpe_generate <- function(vcf){
  SvABA_info = info(vcf)
  gr <- rowRanges(vcf)
  
  tmp <- gsub("\\:.*",'', unlist(rowRanges(vcf)$ALT))
  tmp1 <- gsub(".*\\[",'', tmp)
  tmp2 <- gsub(".*\\]",'', tmp1)
  CHROM2 <- tmp2
  
  SvABA_tmp <- data.frame(
    CHROM1 = as.character(seqnames(rowRanges(vcf))),
    POS1 = start(rowRanges(vcf)),
    CHROM2 = CHROM2,
    POS2 = NA,
    ALT1 = as.character(unlist(rowRanges(vcf)$ALT)),
    ALT2 = NA,
    ID = names(gr),
    ID_mate = SvABA_info$MATEID,
    ID_event = gsub(":.*",'', names(gr)),
    ID_POS = gsub(".*:",'', names(gr)),
    SPAN = SvABA_info$SPAN,
    SVTYPE = SvABA_info$SVTYPE,
    REF=gr$REF,
    stringsAsFactors = FALSE
  ) 
  
  SvABA_df <- SvABA_tmp[SvABA_tmp$ID_POS==1,]
  SvABA_df[match(SvABA_tmp[SvABA_tmp$ID_POS==2,]$ID_event,SvABA_df$ID_event),]$POS2 <- SvABA_tmp[SvABA_tmp$ID_POS==2,]$POS1
  SvABA_df[match(SvABA_tmp[SvABA_tmp$ID_POS==2,]$ID_event,SvABA_df$ID_event),]$ALT2 <- SvABA_tmp[SvABA_tmp$ID_POS==2,]$ALT1
  
  SvABA_df$SVTYPE[(grepl('T\\[|A\\[|G\\[|C\\[|N\\[',SvABA_df$ALT1) & grepl('\\]T|\\]A|\\]G|\\]C|\\]N',SvABA_df$ALT2)) & SvABA_df$SPAN != -1] <- "DEL"
  SvABA_df$SVTYPE[(grepl('\\]T|\\]A|\\]G|\\]C|\\]N',SvABA_df$ALT1) & grepl('T\\[|A\\[|G\\[|C\\[|N\\[',SvABA_df$ALT2)) & SvABA_df$SPAN != -1] <- "DUP/INS"
  SvABA_df$SVTYPE[((grepl('T\\]|A\\]|G\\]|C\\]|N\\]',SvABA_df$ALT1) & grepl('T\\]|A\\]|G\\]|C\\]|N\\]',SvABA_df$ALT2))|
                     (grepl('\\[T|\\[A|\\[G|\\[C|\\[N',SvABA_df$ALT2) & grepl('\\[T|\\[A|\\[G|\\[C|\\[N',SvABA_df$ALT1))) &
                    SvABA_df$SPAN != -1] <- "INV"
  return(SvABA_df)
}

SvABA_simplebed_generate <- function(bedpe){
  bed_non_TRA <- data.frame(
    chrom = bedpe$CHROM1,
    start = bedpe$POS1,
    end = bedpe$POS2,
    SVTYPE = bedpe$SVTYPE,
    SVLEN = bedpe$SPAN,
    ID = bedpe$ID,
    ID_mate = bedpe$ID_mate,
    ALT = bedpe$ALT1
  )
  return(bed_non_TRA)
}

SvABA_BNDbed_generate <- function(bedpe){
  bed_TRA <- data.frame(
    chrom = c(bedpe$CHROM1,bedpe$CHROM2),
    start = c(bedpe$POS1,bedpe$POS2),
    end = NA,
    SVTYPE = c(bedpe$SVTYPE,bedpe$SVTYPE),
    SVLEN = NA,
    ID = c(bedpe$ID,bedpe$ID_mate),
    ID_mate = c(bedpe$ID_mate,bedpe$ID),
    ALT = c(bedpe$ALT1,bedpe$ALT1)
  )
  return(bed_TRA)
}


SvABA_bed_generate <- function(vcf){
  bedpe <- SvABA_bedpe_generate(vcf)
  bedpe_non_TRA <- bedpe[bedpe$SVTYPE!="BND",]
  bed_non_TRA <- SvABA_simplebed_generate(bedpe_non_TRA)
  bedpe_TRA <- bedpe[bedpe$SVTYPE=="BND",]
  ifelse(nrow(bedpe_TRA)!=0,bed_TRA <- SvABA_BNDbed_generate(bedpe_TRA),bed_TRA <- data.frame())
  SvABA_bed <- rbind(bed_non_TRA,bed_TRA)
  return(SvABA_bed)
}

BreakDancer_bed_generate <- function(BreakDancer_out){
  BreakDancer_out_tmp <- BreakDancer_out[-c(1,2)]
  bed_tmp <- data.frame(str_split_fixed(BreakDancer_out_tmp,pattern="\t",n=14),stringsAsFactors = FALSE)
  colnames(bed_tmp) <- c(str_split_fixed(substring(BreakDancer_out[2],2),pattern="\t",n=14))
  bed_tmp$Pos1 <- as.numeric(bed_tmp$Pos1)
  bed_tmp$Pos2 <- as.numeric(bed_tmp$Pos2)
  bed_tmp$Size <- as.numeric(bed_tmp$Size)
  
  ID = paste("BreakDancer","_",c(1:nrow(bed_tmp)),"_","1","_",c(1:nrow(bed_tmp)),sep="")
  ID_mate = paste("BreakDancer","_",c(1:nrow(bed_tmp)),"_","2","_",c(1:nrow(bed_tmp)),sep="")
  bed_tmp <- cbind(bed_tmp,ID,ID_mate)
  
  bed_tmp_nonCTX <- bed_tmp[bed_tmp$Type!="CTX",]
  bed_tmp_TRA <- bed_tmp[bed_tmp$Type %in% c("CTX","ITX"),]
  bed_tmp_TRA <- bed_tmp_TRA[rep(c(1:nrow(bed_tmp_TRA)),each=2),]
  
  bed_nonCTX <- data.frame(
    chrom=bed_tmp_nonCTX$Chr1,
    start=bed_tmp_nonCTX$Pos1,
    end=bed_tmp_nonCTX$Pos2,
    SVTYPE=bed_tmp_nonCTX$Type,
    SVLEN=bed_tmp_nonCTX$Size,
    ID = bed_tmp_nonCTX$ID,
    ID_mate = bed_tmp_nonCTX$ID_mate,
    SVCallerID = NA,
    SVCallerID_mate = NA,
    event = NA,
   # numReads = bed_tmp_nonCTX$num_Reads,
   # VAF = bed_tmp_nonCTX$Allele_frequency,
    stringsAsFactors = FALSE
  )
  
  index <- rep(c(TRUE,FALSE),nrow(bed_tmp_TRA)/2)
  bed_TRA <- data.frame(
    chrom=ifelse(index, bed_tmp_TRA$Chr1, bed_tmp_TRA$Chr2),
    start=ifelse(index,bed_tmp_TRA$Pos1,bed_tmp_TRA$Pos2),
    end=NA,
    SVTYPE=paste0(bed_tmp_TRA$Type,"_BND"),
    SVLEN=bed_tmp_TRA$Size,
    ID = ifelse(index,as.character(bed_tmp_TRA$ID),as.character(bed_tmp_TRA$ID_mate)),
    ID_mate = ifelse(index,as.character(bed_tmp_TRA$ID_mate),as.character(bed_tmp_TRA$ID)),
    SVCallerID = NA,
    SVCallerID_mate = NA,
    event = NA,
    #numReads = bed_tmp_TRA$num_Reads,
   # VAF = bed_tmp_TRA$Allele_frequency,
    stringsAsFactors = FALSE
  )
  bed <- rbind(bed_nonCTX,bed_TRA)
}

SVCaller_bed_newID_generate <- function(SVCaller_vcf,SVCaller_name){
  # if(nrow(info(SVCaller_vcf)) == 0){
  #   SVCaller_bed_newID <- data.frame(matrix(ncol=10,nrow=0))  
  # }else{
  if(SVCaller_name == "BreakDancer"){
    SVCaller_bed_newID <- BreakDancer_bed_generate(SVCaller_vcf)
    return(SVCaller_bed_newID)
  }else if (SVCaller_name %in% c("GRIDSS","SvABA","Delly")){
    SVCaller_bed <- eval(parse(text=paste0(SVCaller_name,"_bed_generate(SVCaller_vcf)")))
  }else{
    SVCaller_bed <- Standard_bed_generate(SVCaller_vcf)
  }
  colnames(SVCaller_bed) <- c("chrom","start","end","SVTYPE","SVLEN","SVCallerID","SVCallerID_mate","event")
  SV_index_tmp <- c(1:length(SVCaller_bed$SVCallerID))
  
  if(SVCaller_name %in% c("GRIDSS","SvABA","Delly")){
    ID_tmp <- SVCaller_bed$SVCallerID
    SV_mate_index_tmp <- ifelse(is.na(match(SVCaller_bed$SVCallerID_mate,ID_tmp)),SV_index_tmp,match(SVCaller_bed$SVCallerID_mate,ID_tmp))
  }else{
    ID_tmp <- ifelse(is.na(SVCaller_bed$SVCallerID_mate),SVCaller_bed$SVCallerID,SVCaller_bed$SVCallerID_mate)
    SV_mate_index_tmp <- match(ID_tmp, SVCaller_bed$SVCallerID)
  }
  
  SV_index <- ifelse(SV_index_tmp <= SV_mate_index_tmp,SV_index_tmp,SV_mate_index_tmp)
  mate1_index <- ifelse(duplicated(SV_index),"2","1")
  mate2_index <- ifelse(mate1_index=="1","2","1")
  
  if(SVCaller_name == "Manta"){
    ID_tmp <- SVCaller_bed$event
    ID_tmp[is.na(SVCaller_bed$event)] <- SV_index[is.na(SVCaller_bed$event)]
    event_index <- match(ID_tmp,unique(ID_tmp))
  }else{
    event_index <- SV_index
  }
  
  ID <- paste(SVCaller_name,"_",SV_index,"_",mate1_index,"_",event_index,sep="")
  ID_mate <- paste(SVCaller_name,"_",SV_index,"_",mate2_index,"_",event_index,sep="")
  
  SVCaller_bed_newID <- cbind(SVCaller_bed[,c(1:5)],ID,ID_mate,SVCaller_bed[,c(6:8)])
  #  }
  return(SVCaller_bed_newID)
}

### Prepare bed file to compare breakpoint position
Standard_bedtool_prepare_bkpt <- function(Standard_bed,BND_diff){
  Standard_bed_tmp <- Standard_bed[,c(1:7)]
  diff <- BND_diff/2
  bed_non_BND <- Standard_bed_tmp[!grepl("BND",Standard_bed$SVTYPE),]
  bed_non_BND.expanded <- bed_non_BND[rep(seq_len(nrow(bed_non_BND)),each=2),]
  bed_non_BND.expanded$ID <- as.character(bed_non_BND.expanded$ID)
  bed_non_BND.expanded$ID_mate <- as.character(bed_non_BND.expanded$ID_mate)
  
  bed_non_BND_tmp <- bed_non_BND.expanded
  bed_non_BND_tmp$ID <- as.character(bed_non_BND_tmp$ID)
  bed_non_BND_tmp$ID_mate <- as.character(bed_non_BND_tmp$ID_mate)
  
  index <- !duplicated(bed_non_BND.expanded$ID)
  bed_non_BND_tmp[!index,]$ID <- bed_non_BND.expanded$ID_mate[!index]
  bed_non_BND_tmp[!index,]$ID_mate <- bed_non_BND.expanded$ID[!index]
  
  ### start and end
  bed_non_BND_tmp[index,]$start <- bed_non_BND.expanded$start[index]-diff
  bed_non_BND_tmp[index,]$end <- bed_non_BND.expanded$start[index]+diff
  bed_non_BND_tmp[!index,]$start <- bed_non_BND.expanded$end[!index]-diff
  bed_non_BND_tmp[!index,]$end <- bed_non_BND.expanded$end[!index]+diff
  
  bed_BND <- Standard_bed_tmp[grepl("BND",Standard_bed$SVTYPE),]
  bed_BND_tmp <- bed_BND
  bed_BND_tmp$start <- bed_BND$start-diff
  bed_BND_tmp$end <- bed_BND$start+diff
  
  Standard_bed_tmp <- rbind(bed_non_BND_tmp,bed_BND_tmp)
  
  if (sum(Standard_bed_tmp$start<0)!=0){Standard_bed_tmp[Standard_bed_tmp$start<0,]$start <- 0}
  return(Standard_bed_tmp)
}

Standard_STAT_generate <- function(bed){
  bed <- bed[,c(1:7)]
  colnames(bed) <- c("chrom","start","end","SVTYPE","SVLEN","ID","ID_mate")
  
  SV_index <- as.numeric(str_split_fixed(as.character(bed$ID),"_",n=4)[,2])
  N_sum <- sum(!duplicated(SV_index))
  N_DEL <- sum(grepl("DEL",bed$SVTYPE) & !duplicated(SV_index))
  N_DUP <- sum(bed$SVTYPE=="DUP" & !duplicated(SV_index))
  N_INV <- sum(bed$SVTYPE=="INV" & !duplicated(SV_index))
  N_INS <- sum(bed$SVTYPE=="INS" & !grepl("BND",bed$SVTYPE) & !duplicated(SV_index))
  N_TRA <- sum(bed$SVTYPE=="TRA" & !duplicated(SV_index))
  N_BND <- sum((grepl("BND",bed$SVTYPE)|bed$SVTYPE=="ITX"|bed$SVTYPE=="CTX") & !duplicated(SV_index))
  N_RPL <- sum(bed$SVTYPE=="RPL" & !duplicated(SV_index))
  N_CTX <- sum(grepl("CTX",bed$SVTYPE) & !duplicated(SV_index))
  N_ITX <- sum(bed$SVTYPE=="ITX" & !duplicated(SV_index))
  N_DUPorINS <- sum(bed$SVTYPE=="DUP/INS" & !duplicated(SV_index))
  
  Standard_STAT_bed <- c(N_sum,N_DEL,N_DUP,N_INV,N_INS,N_TRA,N_BND,N_RPL,N_CTX,N_ITX,N_DUPorINS)
  names(Standard_STAT_bed) <- c("N_sum","N_DEL","N_DUP","N_INV","N_INS","N_TRA","N_BND","N_RPL","N_CTX","N_ITX","N_DUP/INS")
  
  return(Standard_STAT_bed)
}

SVLEN_stat <- function(bed,SVTYPE,test_No){
  SV_index <- as.numeric(str_split_fixed(as.character(bed$ID),"_",n=4)[,2])
  bed <- bed[!duplicated(SV_index),]
  if(!is.na(SVTYPE)){
    bed <- bed[grepl(SVTYPE,bed$SVTYPE),]
  }
  SVLEN <- abs(bed$SVLEN[!is.na(bed$SVLEN)])
  if(test_No == "Test1"){
    stat <- c(sum(SVLEN <= 0),
              sum(SVLEN > 0 & SVLEN <= 50),
              sum(SVLEN > 50 & SVLEN <= 100),
              sum(SVLEN > 100 & SVLEN <= 500),
              sum(SVLEN > 500 & SVLEN <= 1000),
              sum(SVLEN > 1000 & SVLEN <= 2000),
              sum(SVLEN > 2000 & SVLEN <= 5000),
              sum(SVLEN > 5000 & SVLEN <= 10000),
              sum(SVLEN > 10000 & SVLEN <= 15000),
              sum(SVLEN > 15000 & SVLEN <= 100000),
              sum(SVLEN > 100000 & SVLEN <= 1000000),
              sum(SVLEN > 1000000),
              sum(is.na(bed$SVLEN)))
    names(stat) <- c("0","50","100","500","1000","2000","5000","10000","15000","100000","1000000",">1000000","NO_SVLEN")
  }else if(test_No == "Test2"){
    stat <- c(sum(SVLEN < 50),
              sum(SVLEN >= 50 & SVLEN < 100),
              sum(SVLEN >= 100 & SVLEN < 500),
              sum(SVLEN >= 500 & SVLEN < 1000),
              sum(SVLEN >= 1000 & SVLEN < 2000),
              sum(SVLEN >= 2000 & SVLEN < 5000),
              sum(SVLEN >= 5000 & SVLEN < 10000),
              sum(SVLEN >= 10000 & SVLEN < 15000),
              sum(SVLEN >= 15000 & SVLEN < 100000),
              sum(SVLEN >= 100000 & SVLEN < 1000000),
              sum(SVLEN >= 1000000),
              sum(is.na(bed$SVLEN)))
    names(stat) <- c("<50","50","100","500","1000","2000","5000","10000","15000","100000","1000000","NO_SVLEN")
  }
  
  return(stat)
}

random_POS_to_var <- function(random_POS.bed,LEN,FINS,dir_FINS_bed,SVTYPE_N,VARSSEQ_1,FINS_VARSSEQ_1){
  DEL_index <- c()
  DUP_index <- c()
  INV_index <- c()
  DINS_index <- c()
  TRA_index <- c()
  for(i in 1: length(LEN)){
    split <- split(random_POS.bed[random_POS.bed$length==LEN[i],]$ID,factor(1:5))
    DEL_index <- c(DEL_index,split$`1`)
    DUP_index <- c(DUP_index,split$`2`)
    INV_index <- c(INV_index,split$`3`)
    DINS_index <- c(DINS_index,split$`4`)
    TRA_index <- c(TRA_index,split$`5`)
  }
  POS_factor <- ifelse(FINS,3,2)
  split <- split(random_POS.bed[random_POS.bed$length==1,]$ID,factor(1:POS_factor))
  DINS_index2 <- split$`1`
  TRA_index2 <- split$`2`
  FINS_index <- split$`3`
  random_bed <- random_POS.bed[c(DEL_index,DUP_index,INV_index,DINS_index,TRA_index,FINS_index),]
  SVTYPE <- random_bed$ID
  SVTYPE[random_bed$ID %in% DEL_index] <- "DEL"
  SVTYPE[random_bed$ID %in% DUP_index] <- "DUP"
  SVTYPE[random_bed$ID %in% INV_index] <- "INV"
  SVTYPE[random_bed$ID %in% DINS_index] <- "DINS"
  SVTYPE[random_bed$ID %in% TRA_index] <- "TRA"
  SVTYPE[random_bed$ID %in% FINS_index] <- "FINS"
  N_TRA <- sum(SVTYPE=="TRA")
  
  SVTYPE_ID <- random_bed$ID
  SVTYPE_ID[random_bed$ID %in% DEL_index] <- c(1:SVTYPE_N)
  SVTYPE_ID[random_bed$ID %in% DUP_index] <- c(1:SVTYPE_N)
  SVTYPE_ID[random_bed$ID %in% INV_index] <- c(1:SVTYPE_N)
  SVTYPE_ID[random_bed$ID %in% DINS_index] <- c(1:SVTYPE_N)
  SVTYPE_ID[random_bed$ID %in% TRA_index] <- c(1:SVTYPE_N)
  SVTYPE_ID[random_bed$ID %in% FINS_index] <- c(1:SVTYPE_N)
  tmp_bed <- cbind(random_bed,SVTYPE,SVTYPE_ID)
  
  random_N1_bed <- random_POS.bed[c(DINS_index2,TRA_index2,FINS_index),]
  
  VARPOS <- tmp_bed$start
  VARPOS[tmp_bed$SVTYPE %in% c("DINS","TRA","FINS")] <- random_N1_bed$start
  
  VARCHR <- tmp_bed$chrom
  VARCHR[tmp_bed$SVTYPE %in% c("DINS","TRA","FINS")] <- random_N1_bed$chrom
  
  ID2 <- tmp_bed$ID
  ID2[tmp_bed$SVTYPE %in% c("DINS","TRA","FINS")] <- random_N1_bed$ID
  
  tmp_bed <- cbind(random_bed,SVTYPE,SVTYPE_ID,VARPOS,VARCHR,ID2)
  tmp_bed$SVTYPE <- as.character(tmp_bed$SVTYPE)
  tmp_bed$VARCHR <- as.character(tmp_bed$VARCHR)
  sum(tmp_bed[tmp_bed$SVTYPE=="DINS",]$chrom == tmp_bed[tmp_bed$SVTYPE=="DINS",]$VARCHR) 
  sum(tmp_bed[tmp_bed$SVTYPE=="TRA",]$chrom == tmp_bed[tmp_bed$SVTYPE=="TRA",]$VARCHR) ### check how many inter- and intra-chromosomal translocation
  
  ### add duplicated rows for TRA, prepare for var file
  new_index <- c(tmp_bed[tmp_bed$SVTYPE != "TRA",]$ID,rep(tmp_bed[tmp_bed$SVTYPE == "TRA",]$ID,each=2))
  new_tmp_bed <- tmp_bed[match(new_index,tmp_bed$ID),]
  
  index <- rep(c(TRUE,FALSE),N_TRA)
  new_tmp_bed[new_tmp_bed$SVTYPE=="TRA",][index,]$VARCHR <- new_tmp_bed[new_tmp_bed$SVTYPE=="TRA",][index,]$chrom
  new_tmp_bed[new_tmp_bed$SVTYPE=="TRA",][index,]$VARPOS <- new_tmp_bed[new_tmp_bed$SVTYPE=="TRA",][index,]$start
  new_tmp_bed[new_tmp_bed$SVTYPE=="TRA",][index,]$ID2 <- new_tmp_bed[new_tmp_bed$SVTYPE=="TRA",][index,]$ID
  
  VARDEL <- vector(length=nrow(new_tmp_bed))
  VARDEL[new_tmp_bed$SVTYPE %in% c("DEL","DUP","INV")] <- "True"
  VARDEL[new_tmp_bed$SVTYPE %in% c("DINS","FINS")] <- "False"
  VARDEL[new_tmp_bed$SVTYPE == "TRA"] <- rep(c("True","False"),N_TRA)
  
  VARDELSIZE <- new_tmp_bed$length
  VARDELSIZE[new_tmp_bed$SVTYPE %in% c("DINS","FINS")] <- 0
  VARDELSIZE[new_tmp_bed$SVTYPE == "TRA"][VARDEL[new_tmp_bed$SVTYPE == "TRA"]=="False"] <- 0
  
  VARINS <- vector(length=nrow(new_tmp_bed))
  VARINS[new_tmp_bed$SVTYPE == "DEL"] <- "False"
  VARINS[new_tmp_bed$SVTYPE %in% c("DINS","DUP","INV","FINS")] <- "True"
  VARINS[new_tmp_bed$SVTYPE == "TRA"] <- rep(c("False","True"),N_TRA)
  
  VARSSEQ <-  vector(length=nrow(new_tmp_bed))
  VARSSEQ[new_tmp_bed$SVTYPE == "DEL"] <- "None"
  VARSSEQ[(new_tmp_bed$SVTYPE == "TRA") & (VARDEL=="True")] <- "None"
  
  VARSSEQ_2 <- paste(new_tmp_bed$chrom,":",
                     new_tmp_bed$start+1,"-",
                     new_tmp_bed$end,sep="")
  VARSSEQ_3 <- ifelse(new_tmp_bed$SVTYPE=="DUP",2,1)
  VARSSEQ_4 <- ifelse(new_tmp_bed$SVTYPE=="INV","r","f")
  VARSSEQ[new_tmp_bed$SVTYPE!="FINS" & VARSSEQ!="None"] <- paste(VARSSEQ_1,VARSSEQ_2,VARSSEQ_3,VARSSEQ_4,sep=",")[new_tmp_bed$SVTYPE!="FINS" & VARSSEQ!="None"]
  
  if(FINS){
    FINS_VARSSEQ_1 <- FINS_VARSSEQ_1
    FINS_bed <- read.table(dir_FINS_bed,header = TRUE, sep="\t",stringsAsFactors=FALSE, quote="")
    VARSSEQ_2 <- paste(FINS_bed$name,":",
                       FINS_bed$start,"-",
                       FINS_bed$end,sep="")
    VARSSEQ_3 <- 1
    VARSSEQ_4 <- "f"
    VARSSEQ[new_tmp_bed$SVTYPE=="FINS"] <- paste(FINS_VARSSEQ_1,VARSSEQ_2,VARSSEQ_3,VARSSEQ_4,sep=",")
  }
  
  ### generate var file
  randomSV2.var <- data.frame(
    VID = paste("VAR",c(1:(2*nrow(new_tmp_bed))),sep="_"),
    MID = rep(paste(new_tmp_bed$SVTYPE,"_",new_tmp_bed$SVTYPE_ID,sep=""),each=2),
    VARFREQ = 1,
    VARHAP = rep(c(0,1),nrow(new_tmp_bed)),
    VARCHR = rep(new_tmp_bed$VARCHR,each=2),
    VARPOS = rep(new_tmp_bed$VARPOS,each=2),
    VARDEL = rep(VARDEL,each=2),
    VARDELSIZE = rep(VARDELSIZE,each=2),
    VARINS = rep(VARINS,each=2),
    VARINSSEQ = rep(VARSSEQ,each=2)
  )
  return(randomSV2.var)
}

SVEngine_bed_generate <- function(SomaticSV.bed){
  ### Convert the VARINSSEQ string to dataframe
  index <- SomaticSV.bed$`VARINSSEQ(HAP/SEQFILE,CHR:START-END,COPY,REVCOMP)`!="None"
  VARINSSEQ_ins <- str_split_fixed(SomaticSV.bed[index,]$`VARINSSEQ(HAP/SEQFILE,CHR:START-END,COPY,REVCOMP)`,",",4)
  VARINSESEQ_tmp1 <- str_split_fixed(VARINSSEQ_ins[,2], ":", 2)
  INS_CHROM = VARINSESEQ_tmp1[,1]
  INS_START = as.numeric(str_split_fixed(VARINSESEQ_tmp1[,2],"-",2)[,1])-1
  INS_END = as.numeric(str_split_fixed(VARINSESEQ_tmp1[,2],"-",2)[,2])
  INSLEN = INS_END - INS_START
  
  VARINSSEQ <- data.frame(
    "INS_CHROM"= rep(NA,nrow(SomaticSV.bed)),
    "INS_START" = rep(NA,nrow(SomaticSV.bed)),
    "INS_END" = rep(NA,nrow(SomaticSV.bed)),
    "INSLEN"= rep(NA,nrow(SomaticSV.bed)),
    stringsAsFactors=FALSE)
  
  VARINSSEQ$INS_CHROM[index] <- INS_CHROM
  VARINSSEQ$INS_START[index] <- INS_START
  VARINSSEQ$INS_END[index] <- INS_END
  VARINSSEQ$INSLEN[index] <- INSLEN
  
  event_ID <- match(SomaticSV.bed$MID,unique(SomaticSV.bed$MID))
  
  SomaticSV.bed.tmp <- cbind(SomaticSV.bed[,c(1:12)],VARINSSEQ,event_ID)
  SomaticSV.bed.tmp[grepl("INS",SomaticSV.bed.tmp$MID),]$END <- SomaticSV.bed.tmp[grepl("INS",SomaticSV.bed.tmp$MID),]$END+1
  
  ### DEL,DUP,INV,INS
  SomaticSV.bed_nonTRA <- SomaticSV.bed.tmp[!(grepl("TRA",SomaticSV.bed.tmp$MID) & SomaticSV.bed.tmp$VARDELSIZE==0),]
  
  Standard_SVTYPE <- ifelse(grepl("DEL",SomaticSV.bed_nonTRA$MID), "DEL",
                            ifelse(grepl("DUP",SomaticSV.bed_nonTRA$MID), "DUP", 
                                   ifelse(grepl("DINS",SomaticSV.bed_nonTRA$MID), "DINS",
                                          ifelse(grepl("FINS",SomaticSV.bed_nonTRA$MID), "FINS",
                                                 ifelse(grepl("INV",SomaticSV.bed_nonTRA$MID), "INV",
                                                        "DEL_TRA")))))
  
  ### index for non_TRA
  index <- rep(c(TRUE,FALSE),nrow(SomaticSV.bed_nonTRA)/2)  
  SV_ID_nonTRA <- c(1:(length(index)/2))
  event_ID_nonTRA <- SomaticSV.bed_nonTRA$event_ID[index]
  bed <- data.frame(
    chrom = SomaticSV.bed_nonTRA[index,]$CHROM,
    start = SomaticSV.bed_nonTRA[index,]$START,
    end = SomaticSV.bed_nonTRA[index,]$END,
    SVTYPE = Standard_SVTYPE[index],
    SVLEN = SomaticSV.bed_nonTRA[index,]$VARDELSIZE,
    ID = paste("SVEngine","_",SV_ID_nonTRA,"_","1","_",event_ID_nonTRA,sep=""),
    ID_mate = paste("SVEngine","_",SV_ID_nonTRA,"_","2","_",event_ID_nonTRA,sep=""),
    VID.1 = SomaticSV.bed_nonTRA[index,]$VID,
    VID.2 = SomaticSV.bed_nonTRA[!index,]$VID,
    INS_CHROM = SomaticSV.bed_nonTRA[index,]$INS_CHROM,
    INS_START = SomaticSV.bed_nonTRA[index,]$INS_START,
    INS_END = SomaticSV.bed_nonTRA[index,]$INS_END,
    INSLEN = SomaticSV.bed_nonTRA[index,]$INSLEN
  )
  
  ### BND of INSERTION & TRANSLOCATION
  SomaticSV.bed_INS_TRA <- SomaticSV.bed.tmp[grepl("DINS",SomaticSV.bed.tmp$MID)|(grepl("TRA",SomaticSV.bed.tmp$MID) & SomaticSV.bed.tmp$VARDELSIZE==0),]
  if (nrow(SomaticSV.bed_INS_TRA) != 0){
    VID.2 <- SomaticSV.bed_INS_TRA$VID
    VID.2[seq(1,nrow(SomaticSV.bed_INS_TRA),2)] <- SomaticSV.bed_INS_TRA$VID[seq(2,nrow(SomaticSV.bed_INS_TRA),2)]
    VID.2[seq(2,nrow(SomaticSV.bed_INS_TRA),2)] <- SomaticSV.bed_INS_TRA$VID[seq(1,nrow(SomaticSV.bed_INS_TRA),2)]
    
    SomaticSV.bed_INS_TRA <- cbind(SomaticSV.bed_INS_TRA,VID.2)
    
    INSESEQ_tmp <- data.frame(
      CHROM = SomaticSV.bed_INS_TRA$INS_CHROM,
      START = SomaticSV.bed_INS_TRA$INS_START,
      END = SomaticSV.bed_INS_TRA$INS_END,
      VID = SomaticSV.bed_INS_TRA$VID,
      MID = SomaticSV.bed_INS_TRA$MID,
      event_ID = SomaticSV.bed_INS_TRA$event_ID,
      VID.2 = SomaticSV.bed_INS_TRA$VID.2
    )
    
    SomaticSV.bed_BND <- rbind(SomaticSV.bed_INS_TRA[,c(1:5,17,18)],INSESEQ_tmp)
    SomaticSV.bed_BND <- SomaticSV.bed_BND[order(SomaticSV.bed_BND$VID),]
    
    index <- rep(c(TRUE,TRUE,TRUE,FALSE),nrow(SomaticSV.bed_BND)/4) 
    SV_ID_BND <- rep(c((nrow(bed)+1): (nrow(bed)+ nrow(SomaticSV.bed_BND)/2)),each=2)
    event_ID_BND <- SomaticSV.bed_BND$event_ID
    
    bed_BND <- data.frame(
      chrom = SomaticSV.bed_BND$CHROM,
      start = ifelse(index,SomaticSV.bed_BND$START,SomaticSV.bed_BND$END),
      end = NA,
      SVTYPE = ifelse(grepl("INS",SomaticSV.bed_BND$MID), "INS_BND", "TRA_BND"),
      SVLEN = NA,
      ID = paste("SVEngine","_",SV_ID_BND,"_",rep(c(1,2),nrow(SomaticSV.bed_INS_TRA)/2),"_",event_ID_BND,sep=""),
      ID_mate = paste("SVEngine","_",SV_ID_BND,"_",rep(c(2,1),nrow(SomaticSV.bed_INS_TRA)/2),"_",event_ID_BND,sep=""),
      VID.1 = SomaticSV.bed_BND$VID,
      VID.2 = SomaticSV.bed_BND$VID.2,
      INS_CHROM = rep(SomaticSV.bed_INS_TRA$INS_CHROM,each=2),
      INS_START = rep(SomaticSV.bed_INS_TRA$INS_START,each=2),
      INS_END = rep(SomaticSV.bed_INS_TRA$INS_END,each=2),
      INSLEN = rep(SomaticSV.bed_INS_TRA$INSLEN,each=2)
    )
  }else{
    bed_BND <- c()
  }
  ### added for INS signature of TRA
  SomaticSV.bed_TRA <- SomaticSV.bed.tmp[(grepl("TRA",SomaticSV.bed.tmp$MID) & SomaticSV.bed.tmp$VARDELSIZE==0),]
  index <- rep(c(TRUE,FALSE),nrow(SomaticSV.bed_TRA)/2)
  SV_ID_TRA <- c((nrow(bed)+ (nrow(bed_BND)/2) +1):(nrow(bed) + (nrow(bed_BND)/2) + nrow(SomaticSV.bed_TRA)/2))
  event_ID_TRA <- SomaticSV.bed_TRA$event_ID[index]
  bed_INS_TRA <- data.frame(
      chrom = SomaticSV.bed_TRA[index,]$CHROM,
      start = SomaticSV.bed_TRA[index,]$START,
      end = SomaticSV.bed_TRA[index,]$END,
      SVTYPE = "INS_TRA",
      SVLEN = SomaticSV.bed_TRA[index,]$VARDELSIZE,
      ID = paste("SVEngine","_",SV_ID_TRA,"_","1","_",event_ID_TRA,sep=""),
      ID_mate = paste("SVEngine","_",SV_ID_TRA,"_","2","_",event_ID_TRA,sep=""),
      VID.1 = SomaticSV.bed_TRA[index,]$VID,
      VID.2 = SomaticSV.bed_TRA[!index,]$VID,
      INS_CHROM = SomaticSV.bed_TRA[index,]$INS_CHROM,
      INS_START = SomaticSV.bed_TRA[index,]$INS_START,
      INS_END = SomaticSV.bed_TRA[index,]$INS_END,
      INSLEN = SomaticSV.bed_TRA[index,]$INSLEN
  )
  
  bed_new <- rbind(bed,bed_BND,bed_INS_TRA)
  
  return(bed_new)
}

SVEngine_STAT_generate <- function(bed){
  event_index <- as.numeric(str_split_fixed(as.character(bed$ID),"_",n=4)[,4])
  N_DEL <- sum(bed$SVTYPE=="DEL" & !duplicated(event_index))
  N_DEL_TRA <- sum(bed$SVTYPE=="DEL_TRA" & !duplicated(event_index))
  N_DUP <- sum(bed$SVTYPE=="DUP" & !duplicated(event_index))
  N_INV <- sum(bed$SVTYPE=="INV" & !duplicated(event_index))
  N_DINS <- sum(bed$SVTYPE=="DINS" & !duplicated(event_index))
  N_FINS <- sum(bed$SVTYPE=="FINS" & !duplicated(event_index))
  N_INS_TRA <- sum(bed$SVTYPE=="INS_TRA")
  
  SV_index <- as.numeric(str_split_fixed(as.character(bed$ID),"_",n=4)[,2])
  tmp <- data.frame(table(SV_index[grepl("BND",bed$SVTYPE)]))$Freq
  N_BND <- sum(tmp>=2)
  tmp <- data.frame(table(SV_index[bed$SVTYPE=="INS_BND"]))$Freq
  N_INS_BND <- sum(tmp>=2)
  tmp <- data.frame(table(SV_index[bed$SVTYPE=="TRA_BND"]))$Freq
  N_TRA_BND <- sum(tmp>=2)
  
  tmp_df <- data.frame(table(event_index[bed$SVTYPE=="TRA_BND"]))
  if(nrow(tmp_df)==0){
    N_TRA_2BND <- 0
    N_TRA_2BND_only <- 0
  }else{
    N_TRA_2BND <- sum(tmp_df$Freq >= 4)
    tmp <- tmp_df[!(tmp_df$Var1 %in% event_index[bed$SVTYPE=="INS_TRA"]),]$Freq
    N_TRA_2BND_only <- sum(tmp >= 4)
  }
  
  if((N_INS_TRA == 0) & N_TRA_2BND == 0){
    N_TRA_andDEL <- 0
  }else if (N_TRA_2BND == 0 & N_INS_TRA != 0){
    event_index_TRA <- unique(event_index[bed$SVTYPE=="INS_TRA"])
    N_TRA_andDEL <- sum(event_index_TRA %in% event_index[bed$SVTYPE=="DEL_TRA"]) ### (either TRA_2BND or INS_TRA) and DEL_TRA
  }else if (N_TRA_2BND != 0 & N_INS_TRA == 0){
    event_index_TRA <- unique(c(as.numeric(as.character(tmp_df[tmp_df$Freq>=4,]$Var1))))
    N_TRA_andDEL <- sum(event_index_TRA %in% event_index[bed$SVTYPE=="DEL_TRA"]) ### (either TRA_2BND or INS_TRA) and DEL_TRA
  }else{
    event_index_TRA <- unique(c(as.numeric(as.character(tmp_df[tmp_df$Freq>=4,]$Var1)), event_index[bed$SVTYPE=="INS_TRA"]))
    N_TRA_andDEL <- sum(event_index_TRA %in% event_index[bed$SVTYPE=="DEL_TRA"]) ### (either TRA_2BND or INS_TRA) and DEL_TRA
  }
  
  tmp_df <- data.frame(table(event_index[bed$SVTYPE=="INS_BND"]))
  if(nrow(tmp_df)==0){
    N_INS_2BND <- 0
    N_INS_2BND_only <- 0
  }else{
   N_INS_2BND <- sum(tmp_df$Freq >= 4)
   tmp <- tmp_df[!(tmp_df$Var1 %in% event_index[bed$SVTYPE=="DINS"]),]$Freq
   N_INS_2BND_only <- sum(tmp>=4)
   }
  N_sum <- N_DEL + N_DEL_TRA + N_DUP + N_INV + N_DINS + N_FINS+ N_INS_TRA
  
  Standard_STAT_bed <- c(N_sum,N_DEL,N_DEL_TRA,N_DUP,N_INV,N_DINS,N_FINS,N_INS_TRA,
                         N_BND,N_INS_BND,N_TRA_BND,
                         N_INS_2BND_only, N_TRA_2BND_only, N_INS_2BND, N_TRA_2BND,
                         N_TRA_andDEL)
  names(Standard_STAT_bed) <- c("N_SUM", "N_DEL", "N_DEL_TRA","N_DUP","N_INV","N_DINS", "N_FINS","N_INS_TRA",
                                                       "N_BND","N_INS_BND","N_TRA_BND",
                                                       "N_INS_2BND_only", "N_TRA_2BND_only", "N_INS_2BND", "N_TRA_2BND",
                                                       "N_TRA_andDEL")
  
  return(Standard_STAT_bed)
}


SVEngine_SVLEN_stat <- function(bed,SVTYPE,test_No){
  if(!is.na(SVTYPE)){
    bed <- bed[grepl(SVTYPE,bed$SVTYPE),]
    if(SVTYPE == "TRA"){
      bed <- bed[bed$SVTYPE == "TRA_BND",]
    }
  }
  SVLEN_nonINS_nonBND <- bed[!(grepl("BND",bed$SVTYPE) | grepl("INS",bed$SVTYPE)| bed$SVTYPE == "DEL_TRA"),]$SVLEN  ## EXCLUDE DEL_TRA
  SVLEN_nonINS_nonBND <- bed[!(grepl("BND",bed$SVTYPE) | grepl("INS",bed$SVTYPE)),]$SVLEN
  
  if(sum(grepl("FINS",bed$SVTYPE))!=0){
    SVLEN_FINS <- bed[grepl("FINS",bed$SVTYPE),]$INSLEN
  }else{SVLEN_FINS <- c()}
  
  if(sum(grepl("INS",bed$SVTYPE))!=0){
    bed_INS <- bed[grepl("INS",bed$SVTYPE) & (bed$SVTYPE!="INS_TRA") & (bed$SVTYPE!="FINS"),]  ### DINS and INS_BND
    event_index <- as.numeric(str_split_fixed(as.character(bed_INS$ID),"_",n=4)[,4])
    tmp1 <- event_index[bed_INS$SVTYPE!="INS_BND"]  ### event index of DINS deteted as INS
    
    if (sum(bed_INS$SVTYPE=="INS_BND")!=0){
      df <- data.frame(table(event_index[bed_INS$SVTYPE=="INS_BND"]),stringsAsFactors = FALSE)
      tmp2 <- as.numeric(as.character(df[df$Freq == 4,]$Var1)) ### event index of DINS detected as BND (INS_BND with count = 4)
    }else{tmp2 <- c()}
    
    bed_INS_tmp <- bed_INS[event_index %in% unique(c(tmp1,tmp2)),] ### event index, from DINS and (INS_BND with count 4)
    event_index <- as.numeric(str_split_fixed(as.character(bed_INS_tmp$ID),"_",n=4)[,4])
    SVLEN_INS <- bed_INS_tmp[!duplicated(event_index),]$INSLEN
  }else{
    SVLEN_INS <- c()
  }
  
  if(sum(grepl("TRA",bed$SVTYPE))!=0){
    bed_TRA <- bed[grepl("TRA",bed$SVTYPE) & (bed$SVTYPE!="DEL_TRA"), ]  ### INS_TRA and TRA_BND
    event_index <- as.numeric(str_split_fixed(as.character(bed_TRA$ID),"_",n=4)[,4])
    tmp1 <- event_index[bed_TRA$SVTYPE!="TRA_BND"]  ### event index of TRA deteted as INS
    
    if (sum(bed_TRA$SVTYPE=="TRA_BND")!=0){
      df <- data.frame(table(event_index[bed_TRA$SVTYPE=="TRA_BND"]),stringsAsFactors = FALSE)
      tmp2 <- as.numeric(as.character(df[df$Freq == 4,]$Var1)) ### event index of TRA detected as BND (TRA_BND with count = 4)
    }else{tmp2 <- c()}
    
    bed_TRA_tmp <- bed_TRA[event_index %in% unique(c(tmp1,tmp2)),] ### event index, from TRA and (TRA_BND with count 4)
    event_index <- as.numeric(str_split_fixed(as.character(bed_TRA_tmp$ID),"_",n=4)[,4])
    SVLEN_TRA <- bed_TRA_tmp[!duplicated(event_index),]$INSLEN
  }else{
    SVLEN_TRA <- c()
  }
  
  SVLEN <- c(SVLEN_nonINS_nonBND, SVLEN_FINS ,SVLEN_INS,SVLEN_TRA)
  
  SV_index <- as.numeric(str_split_fixed(as.character(bed$ID),"_",n=4)[,2])
  bed <- bed[!duplicated(SV_index),]
  
  if(test_No == "Test1"){
    stat <- c(sum(SVLEN <= 0),
              sum(SVLEN > 0 & SVLEN <= 50),
              sum(SVLEN > 50 & SVLEN <= 100),
              sum(SVLEN > 100 & SVLEN <= 500),
              sum(SVLEN > 500 & SVLEN <= 1000),
              sum(SVLEN > 1000 & SVLEN <= 2000),
              sum(SVLEN > 2000 & SVLEN <= 5000),
              sum(SVLEN > 5000 & SVLEN <= 10000),
              sum(SVLEN > 10000 & SVLEN <= 15000),
              sum(SVLEN > 15000 & SVLEN <= 100000),
              sum(SVLEN > 100000 & SVLEN <= 1000000),
              sum(SVLEN > 1000000),
              sum(is.na(bed$SVLEN)))
    names(stat) <- c("0","50","100","500","1000","2000","5000","10000","15000","100000","1000000",">1000000","NO_SVLEN")
  }else if(test_No == "Test2"){
    stat <- c(sum(SVLEN < 50),
              sum(SVLEN >= 50 & SVLEN < 100),
              sum(SVLEN >= 100 & SVLEN < 500),
              sum(SVLEN >= 500 & SVLEN < 1000),
              sum(SVLEN >= 1000 & SVLEN < 2000),
              sum(SVLEN >= 2000 & SVLEN < 5000),
              sum(SVLEN >= 5000 & SVLEN < 10000),
              sum(SVLEN >= 10000 & SVLEN < 15000),
              sum(SVLEN >= 15000 & SVLEN < 100000),
              sum(SVLEN >= 100000 & SVLEN < 1000000),
              sum(SVLEN >= 1000000),
              sum(is.na(bed$SVLEN)))
    names(stat) <- c("<50","50","100","500","1000","2000","5000","10000","15000","100000","1000000","NO_SVLEN")
  }
  return(stat)
}

# SVEngine_SVLEN_stat <- function(bed,SVTYPE,test_No){
#   if(!is.na(SVTYPE)){
#     bed <- bed[grepl(SVTYPE,bed$SVTYPE),]
#     if(SVTYPE == "TRA"){
#       bed <- bed[bed$SVTYPE == "TRA_BND",]
#     }
#   }
#   if(sum(grepl("INS",bed$SVTYPE))!=0){
#     bed_INS <- bed[grepl("INS",bed$SVTYPE) & !(bed$SVTYPE!="INS_TRA"),]  ### INS and INS_BND
#     #bed_INS <- bed[grepl("INS",bed$SVTYPE),]  ### INS and INS_BND
#     event_index <- as.numeric(str_split_fixed(as.character(bed_INS$ID),"_",n=4)[,4])
#     tmp1 <- event_index[bed_INS$SVTYPE!="INS_BND"]  ### event index of INS
#     if (sum(bed_INS$SVTYPE=="INS_BND")!=0){
#       df <- data.frame(table(event_index[bed_INS$SVTYPE=="INS_BND"]),stringsAsFactors = FALSE)
#       tmp2 <- as.numeric(as.character(df[df$Freq == 4,]$Var1)) ### event index of INS_BND with count = 4
#     }else{tmp2 <- c()}
# 
#     bed_INS_tmp <- bed_INS[event_index %in% unique(c(tmp1,tmp2)),] ### event index, from INS and INS_BND with count 4
#     event_index <- as.numeric(str_split_fixed(as.character(bed_INS_tmp$ID),"_",n=4)[,4])
#     SVLEN_INS <- bed_INS_tmp[!duplicated(event_index),]$INSLEN
#   }else{
#     SVLEN_INS <- c()
#   }
# 
#   if (sum(bed$SVTYPE=="TRA_BND")!=0){
#     bed_TRA_BND <- bed[bed$SVTYPE == "TRA_BND",]
#     event_index <- as.numeric(str_split_fixed(as.character(bed_TRA_BND$ID),"_",n=4)[,4])
#     df <- data.frame(table(event_index[bed_TRA_BND$SVTYPE=="TRA_BND"]),stringsAsFactors = FALSE)
#     tmp2 <- as.numeric(as.character(df[df$Freq == 4,]$Var1))
#     bed_TRA_BND_tmp <- bed_TRA_BND[event_index %in% unique(tmp2),] ### event index, from INS and INS_BND with count 4
#     event_index <- as.numeric(str_split_fixed(as.character(bed_TRA_BND_tmp$ID),"_",n=4)[,4])
#     SVLEN_TRA <- bed_TRA_BND_tmp[(!duplicated(event_index)),]$INSLEN
#   }else{
#     SVLEN_TRA <- c()
#   }
# 
#   SVLEN <- c(bed[!(grepl("INS",bed$SVTYPE) | bed$SVTYPE== "TRA_BND"),]$SVLEN,SVLEN_INS,SVLEN_TRA)
# 
#   SV_index <- as.numeric(str_split_fixed(as.character(bed$ID),"_",n=4)[,2])
#   bed <- bed[!duplicated(SV_index),]
# 
#   if(test_No == "Test1"){
#     stat <- c(sum(SVLEN <= 0),
#               sum(SVLEN > 0 & SVLEN <= 50),
#               sum(SVLEN > 50 & SVLEN <= 100),
#               sum(SVLEN > 100 & SVLEN <= 500),
#               sum(SVLEN > 500 & SVLEN <= 1000),
#               sum(SVLEN > 1000 & SVLEN <= 2000),
#               sum(SVLEN > 2000 & SVLEN <= 5000),
#               sum(SVLEN > 5000 & SVLEN <= 10000),
#               sum(SVLEN > 10000 & SVLEN <= 15000),
#               sum(SVLEN > 15000 & SVLEN <= 100000),
#               sum(SVLEN > 100000 & SVLEN <= 1000000),
#               sum(SVLEN > 1000000),
#               sum(is.na(bed$SVLEN)))
#     names(stat) <- c("0","50","100","500","1000","2000","5000","10000","15000","100000","1000000",">1000000","NO_SVLEN")
#   }else if(test_No == "Test2"){
#     stat <- c(sum(SVLEN < 50),
#               sum(SVLEN >= 50 & SVLEN < 100),
#               sum(SVLEN >= 100 & SVLEN < 500),
#               sum(SVLEN >= 500 & SVLEN < 1000),
#               sum(SVLEN >= 1000 & SVLEN < 2000),
#               sum(SVLEN >= 2000 & SVLEN < 5000),
#               sum(SVLEN >= 5000 & SVLEN < 10000),
#               sum(SVLEN >= 10000 & SVLEN < 15000),
#               sum(SVLEN >= 15000 & SVLEN < 100000),
#               sum(SVLEN >= 100000 & SVLEN < 1000000),
#               sum(SVLEN >= 1000000),
#               sum(is.na(bed$SVLEN)))
#     names(stat) <- c("<50","50","100","500","1000","2000","5000","10000","15000","100000","1000000","NO_SVLEN")
#   }
#   return(stat)
# }

