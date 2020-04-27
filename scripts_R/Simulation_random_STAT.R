source("VCF_functions.R")
directory <- "./VCF_results" ### directory of VCF files, each SV caller in separate folder
bedtools_dir <- "./bedtools2/bin/bedtools" ### directory of bedtools
BND_diff <- 2000
SVTYPE_ignore = FALSE
test_No <- "Test2"
is_SegDup = FALSE

ifelse(is_SegDup,Truth_file <- "SVEngine_segdup.bed",Truth_file <- "SVEngine_truth.bed" )
ifelse(is_SegDup,segdup <- "segdup_",segdup <- "")

SV_caller <- c("Manta","Lumpy","GRIDSS","BreakDancer","CNVKit","Pindel","SvABA","Delly") ### BreakDancer, CNVKit, Pindel are not included anymore

### Evaluated parameter values (e.g. Set3)
VAF_all <- c(0.05,0.15,0.3,0.5,0.7,0.9)
BND_threshold_all <- c(2,10,60,100,130,190)
T_coverage_all <- c(20,35,40,60,80,100)
N_coverage_all <- c(15,35,40,60,80,100)
coverage_all <- c()
for(i in c(1:length(T_coverage_all))){
	for (j in c(1:length(N_coverage_all))){
		coverage_all <- c(coverage_all, paste0("T",T_coverage_all[i],"x","N",N_coverage_all[j],"x"))
	}
}

N_coverage_seq <- rep(rep(N_coverage_all,each=length(VAF_all)),length(T_coverage_all))
T_coverage_seq <- rep(T_coverage_all,each=length(VAF_all)*length(N_coverage_all))
VAF_seq <- rep(VAF_all,length(T_coverage_all)*length(N_coverage_all))

for (SVCaller_name in c(SV_caller[c(1:3,7,8)])){
  if (SVCaller_name == "Lumpy"){
    SVCaller_vcf_file <- paste0("lumpy_SVEngine_TumorSV2.",as.character(T_coverage_seq),"x",
                                "_NormalSV1.",as.character(N_coverage_seq),"x",
                                "_",as.character(VAF_seq),".BLDsu0.minSU4.vcf")
  }else if (SVCaller_name == "Manta"){
    SVCaller_vcf_file <- paste0("Manta_SVEngine_TumorSV2.",as.character(T_coverage_seq),"x",
                                "_NormalSV1.",as.character(N_coverage_seq),"x",
                                "_",as.character(VAF_seq),".T.PASS.recode.vcf")
  }else if (SVCaller_name == "GRIDSS"){
    SVCaller_vcf_file <- paste0("GRIDSS_SVEngine_TumorSV2.",as.character(T_coverage_seq),"x",
                                "_NormalSV1.",as.character(N_coverage_seq),"x",
                                "_",as.character(VAF_seq),"_somatic_PASS_annotated.vcf")
  }else if (SVCaller_name == "Delly"){
    SVCaller_vcf_file <- paste0("delly_SVEngine_TumorSV2.",as.character(T_coverage_seq),"x",
                                "_NormalSV1.",as.character(N_coverage_seq),"x",
                                "_",as.character(VAF_seq),".somatic.PASS.vcf")
  }else if (SVCaller_name == "SvABA"){
    SVCaller_vcf_file <- paste0("svaba_SVEngine_TumorSV2.",as.character(T_coverage_seq),"x",
                                "_NormalSV1.",as.character(N_coverage_seq),"x",
                                "_",as.character(VAF_seq),".svaba.somatic.sv.vcf")
  }

  k=1
  for (i in c(1:length(coverage_all))){
    coverage <- paste0(segdup,coverage_all[i])
    for(j in c(1:length(VAF_all))){
      VAF <- VAF_all[j]
      vcf_file <- SVCaller_vcf_file[k]
      for (m in c(1:length(BND_threshold_all))){
        BND_threshold <- BND_threshold_all[m]
        assign(paste0("Truth_",SVCaller_name,"_overlap"),SVCaller_evaluation(Truth_file,vcf_file,SVCaller_name,BND_diff,BND_threshold,coverage,VAF,directory,bedtools_dir))
      }
      k=k+1
    }
  }
}

for (SVCaller_name in c(SV_caller[c(1:3,7,8)])){
  if (SVCaller_name == "Lumpy"){
    SVCaller_vcf_file <- paste0("lumpy_SVEngine_TumorSV2.",as.character(T_coverage_seq),"x",
                                "_NormalSV1.",as.character(N_coverage_seq),"x",
                                "_",as.character(VAF_seq),".BLDsu0.minSU4.vcf")
  }else if (SVCaller_name == "Manta"){
    SVCaller_vcf_file <- paste0("Manta_SVEngine_TumorSV2.",as.character(T_coverage_seq),"x",
                                "_NormalSV1.",as.character(N_coverage_seq),"x",
                                "_",as.character(VAF_seq),".T.PASS.recode.vcf")
  }else if (SVCaller_name == "GRIDSS"){
    SVCaller_vcf_file <- paste0("GRIDSS_SVEngine_TumorSV2.",as.character(T_coverage_seq),"x",
                                "_NormalSV1.",as.character(N_coverage_seq),"x",
                                "_",as.character(VAF_seq),"_somatic_PASS_annotated.vcf")
  }else if (SVCaller_name == "Delly"){
    SVCaller_vcf_file <- paste0("delly_SVEngine_TumorSV2.",as.character(T_coverage_seq),"x",
                                "_NormalSV1.",as.character(N_coverage_seq),"x",
                                "_",as.character(VAF_seq),".somatic.PASS.vcf")
  }else if (SVCaller_name == "SvABA"){
    SVCaller_vcf_file <- paste0("svaba_SVEngine_TumorSV2.",as.character(T_coverage_seq),"x",
                                "_NormalSV1.",as.character(N_coverage_seq),"x",
                                "_",as.character(VAF_seq),".svaba.somatic.sv.vcf")
  }

  k=1
  for (i in c(1:length(coverage_all))){
    coverage <- paste0(segdup,coverage_all[i])
    for(j in c(1:length(VAF_all))){
      VAF <- VAF_all[j]
      vcf_file <- SVCaller_vcf_file[k]
      for (m in c(1:length(BND_threshold_all))){
        BND_threshold <- BND_threshold_all[m]
        overlap_file <- paste0('Truth_',coverage,"_",as.character(VAF),'_',as.character(BND_threshold),'bp_',SVCaller_name,'_overlap.bed')
        intersect_file <- paste0(SVCaller_name,'_SVEngine_',coverage,"_",as.character(VAF),'_',as.character(BND_threshold),'bp_intersect.bed')
        stat_list_name <- paste0(SVCaller_name,"_stat_list_",coverage,"_",as.character(VAF),"_",as.character(BND_threshold),"bp")
        assign(stat_list_name, SVCaller_stat(vcf_file,overlap_file,intersect_file,directory,Truth_file,SVTYPE_ignore,test_No,is_SegDup,SVCaller_name, bedtools_dir))
      }
      k=k+1
    }
  }
  SVCaller_stat_all <- SVCaller_stat_generate(coverage_all,VAF_all,BND_threshold_all,SVCaller_name, directory, Truth_file,test_No, segdup)
  setwd(directory)
  setwd(paste0("./",SVCaller_name))
  
  #### Write results to files
  write.csv(SVCaller_stat_all[[1]], paste0(SVCaller_name,"_",segdup,"SVTYPE_Call_STAT.csv"),row.names = TRUE)
  write.csv(SVCaller_stat_all[[2]], paste0(SVCaller_name,"_",segdup,"SVTYPE_TP_Sensitivity.csv"),row.names = TRUE)
  write.csv(SVCaller_stat_all[[3]], paste0(SVCaller_name,"_",segdup,"SVTYPE_TP_Precision.csv"),row.names = TRUE)
  write.csv(SVCaller_stat_all[[4]], paste0(SVCaller_name,"_",segdup,"SVLEN_Call_STAT.csv"),row.names = TRUE)
  write.csv(SVCaller_stat_all[[5]], paste0(SVCaller_name,"_",segdup,"SVLEN_TP_Sensitivity.csv"),row.names = TRUE)
  write.csv(SVCaller_stat_all[[6]], paste0(SVCaller_name,"_",segdup,"SVLEN_TP_Precision.csv"),row.names = TRUE)
  T_coverage <- rep(T_coverage_all,each=length(N_coverage_all))
  N_coverage <- rep(N_coverage_all,length(T_coverage_all))
  summary_stat <- summary_stat_generate(coverage_all,VAF_all,BND_threshold_all,SVCaller_name,T_coverage,N_coverage,
                                        SVCaller_stat_all[[1]],SVCaller_stat_all[[2]],SVCaller_stat_all[[3]])
  write.csv(summary_stat,paste0(SVCaller_name,"_summary_stat.csv"),row.names = FALSE)
}

