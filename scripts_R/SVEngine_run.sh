### Normal sample simulation with germline SVs
PYTHONPATH=$SVENGINEPATH python -m mf.mutforge -n 16 -f 10000 -e 10000000 -b 1000000 -v randomSV1_noG_noC_noT.var -d $TMP_DIR --debug \
-o randomSV1_noG_noC_noT \
Homo_sapiens_assembly38_chromosome.fasta \
mySeq2.par Homo_sapiens_assembly38_chromosome.fasta \

### Tumour sample simulation with somatic and germline SVs
PYTHONPATH=$SVENGINEPATH python -m mf.mutforge -n 16 -f 10000 -e 10000000 -b 1000000 -v randomSV2_noG_noC_noT.var -d $TMP_DIR --debug \
-o randomSV2_noG_noC_noT \
Homo_sapiens_assembly38_chromosome.fasta \
mySeq2.par Homo_sapiens_assembly38_chromosome.fasta \
