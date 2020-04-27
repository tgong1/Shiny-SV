####### hg38.genome for bedtools random ##########
hg38.df <- read.table("Homo_sapiens_assembly38_chromosome.fasta.fai",header = FALSE, sep="\t",stringsAsFactors=FALSE, quote="")
colnames(hg38.df) <- c("name","N_bases","byte-index","N_bases_line","N_byte_line")
barplot(hg38.df$N_bases,name = hg38.df$name, main= "hg38_genome")

hg38.genome <- hg38.df[c(1:24),c(1:2)]
write.table(hg38.genome,"hg38.genome", quote=FALSE, sep='\t', row.names=FALSE, col.names=FALSE)

####### Telomeres as 1,000,000bp at both end ##########
Telomere_ignore <- 1000000
index <- rep(c(TRUE,FALSE),24)
Telomere.bed <- data.frame(
  chrom = rep(hg38.df$name[1:24],each=2),
  start = 0,
  end = Telomere_ignore
)

Telomere.bed[!index,]$start <- hg38.df$N_bases[1:24]-Telomere_ignore
Telomere.bed[!index,]$end <- hg38.df$N_bases[1:24]
Telomere.bed$start <- as.integer(Telomere.bed$start)
Telomere.bed$end <- as.integer(Telomere.bed$end)

write.table(Telomere.bed,"Telomere.bed", quote=FALSE, sep='\t', row.names=FALSE, col.names=FALSE)

