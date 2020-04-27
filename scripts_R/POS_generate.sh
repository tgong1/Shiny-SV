#!/bin/bash
#
#Randomly select positions of SVs on human reference genome Hg38, excluding gap, centromere and telomere regions.
#
### DEL, DUP, INV, DINS, TRA need interval positions
### DEL, DUP, INV, DINS, TRA with 400 each SV type, in total 2000
### 10 SV length ranges, so 200 intervals needed for each SV length range
N=200
LEN1=50  ### previously use 25
LEN2=100
LEN3=500
LEN4=1000
LEN5=2000
LEN6=5000
LEN7=10000
LEN8=15000
LEN9=100000
LEN10=1000000
seed=234 ###seed=123 for simulation Set1, seed=234 for simulation Set2 and seed=456 for simulation Set3


### generate random intervals among hg38 genome, specified the length and the number of intervals
bedtools random -l ${LEN1} -n $N -seed $seed -g hg38.genome > random_POS_LEN${LEN1}_N${N}.bed
bedtools random -l ${LEN2} -n $N -seed $seed -g hg38.genome > random_POS_LEN${LEN2}_N${N}.bed
bedtools random -l ${LEN3} -n $N -seed $seed -g hg38.genome > random_POS_LEN${LEN3}_N${N}.bed
bedtools random -l ${LEN4} -n $N -seed $seed -g hg38.genome > random_POS_LEN${LEN4}_N${N}.bed
bedtools random -l ${LEN5} -n $N -seed $seed -g hg38.genome > random_POS_LEN${LEN5}_N${N}.bed
bedtools random -l ${LEN6} -n $N -seed $seed -g hg38.genome > random_POS_LEN${LEN6}_N${N}.bed
bedtools random -l ${LEN7} -n $N -seed $seed -g hg38.genome > random_POS_LEN${LEN7}_N${N}.bed
bedtools random -l ${LEN8} -n $N -seed $seed -g hg38.genome > random_POS_LEN${LEN8}_N${N}.bed
bedtools random -l ${LEN9} -n $N -seed $seed -g hg38.genome > random_POS_LEN${LEN9}_N${N}.bed
bedtools random -l ${LEN10} -n $N -seed $seed -g hg38.genome > random_POS_LEN${LEN10}_N${N}.bed

### 
bedtools shuffle -excl random_POS_LEN${LEN1}_N${N}.bed -i random_POS_LEN${LEN2}_N${N}.bed -seed $seed -noOverlapping \
-g hg38.genome > random_POS_LEN${LEN2}_N${N}_s
cat random_POS_LEN${LEN1}_N${N}.bed random_POS_LEN${LEN2}_N${N}_s > random_POS_Size2_N${N}.bed

bedtools shuffle -excl random_POS_Size2_N${N}.bed -i random_POS_LEN${LEN3}_N${N}.bed -seed $seed -noOverlapping \
-g hg38.genome > random_POS_LEN${LEN3}_N${N}_s
cat random_POS_Size2_N${N}.bed random_POS_LEN${LEN3}_N${N}_s > random_POS_Size3_N$N.bed

bedtools shuffle -excl random_POS_Size3_N${N}.bed -i random_POS_LEN${LEN4}_N${N}.bed -seed $seed -noOverlapping \
-g hg38.genome > random_POS_LEN${LEN4}_N${N}_s
cat random_POS_Size3_N${N}.bed random_POS_LEN${LEN4}_N${N}_s > random_POS_Size4_N${N}.bed

bedtools shuffle -excl random_POS_Size4_N${N}.bed -i random_POS_LEN${LEN5}_N$N.bed -seed $seed -noOverlapping \
-g hg38.genome > random_POS_LEN${LEN5}_N${N}_s
cat random_POS_Size4_N${N}.bed random_POS_LEN${LEN5}_N${N}_s > random_POS_Size5_N${N}.bed

bedtools shuffle -excl random_POS_Size5_N${N}.bed -i random_POS_LEN${LEN6}_N$N.bed -seed $seed -noOverlapping \
-g hg38.genome > random_POS_LEN${LEN6}_N${N}_s
cat random_POS_Size5_N${N}.bed random_POS_LEN${LEN6}_N${N}_s > random_POS_Size6_N${N}.bed

bedtools shuffle -excl random_POS_Size6_N${N}.bed -i random_POS_LEN${LEN7}_N$N.bed -seed $seed -noOverlapping \
-g hg38.genome > random_POS_LEN${LEN7}_N${N}_s
cat random_POS_Size6_N${N}.bed random_POS_LEN${LEN7}_N${N}_s > random_POS_Size7_N${N}.bed

bedtools shuffle -excl random_POS_Size7_N${N}.bed -i random_POS_LEN${LEN8}_N$N.bed -seed $seed -noOverlapping \
-g hg38.genome > random_POS_LEN${LEN8}_N${N}_s
cat random_POS_Size7_N${N}.bed random_POS_LEN${LEN8}_N${N}_s > random_POS_Size8_N${N}.bed

bedtools shuffle -excl random_POS_Size8_N${N}.bed -i random_POS_LEN${LEN9}_N$N.bed -seed $seed -noOverlapping \
-g hg38.genome > random_POS_LEN${LEN9}_N${N}_s
cat random_POS_Size8_N${N}.bed random_POS_LEN${LEN9}_N${N}_s > random_POS_Size9_N${N}.bed

bedtools shuffle -excl random_POS_Size9_N${N}.bed -i random_POS_LEN${LEN10}_N$N.bed -seed $seed -noOverlapping \
-g hg38.genome > random_POS_LEN${LEN10}_N${N}_s
cat random_POS_Size9_N${N}.bed random_POS_LEN${LEN10}_N${N}_s > random_POS_Size10_N${N}.bed

N_insert=1200 ###DINS,FINS,TRA need insertion position as interval length = 1
bedtools random -l 1 -n $N_insert -seed $seed -g hg38.genome > random_POS_LEN1_N${N_insert}.bed
bedtools shuffle -excl random_POS_Size10_N${N}.bed -i random_POS_LEN1_N${N_insert}.bed -seed $seed -noOverlapping \
-g hg38.genome > random_POS_LEN1_N${N_insert}.s.bed
cat random_POS_Size10_N${N}.bed random_POS_LEN1_N${N_insert}.s.bed > random_POS_all.bed

bedtools shuffle -excl hg38_gaps_centromeres_Telomeres.bed -i random_POS_all.bed -seed $seed -noOverlapping \
-g hg38.genome > random_POS_noG_noC_noT.bed









