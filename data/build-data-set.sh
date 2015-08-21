#!/bin/bash

desc=description.tab
wget -O $desc 'ftp://ftp.arabidopsis.org/home/tair/Genes/TAIR10_genome_release/TAIR10_functional_descriptions_20140331.txt'

dtype='type'
dshort='short-description'
dcurator='curator-description'
dautomatic='automatic-description'
echo -e "model\t$dtype"      > ${dtype}.tab
echo -e "model\t$dshort"     > ${dshort}.tab
echo -e "model\t$dcurator"   > ${dcurator}.tab
echo -e "model\t$dautomatic" > ${dautomatic}.tab
cut -f 1,2 $desc | grep -P '^AT' >> ${dtype}.tab
cut -f 1,3 $desc | grep -P '^AT' >> ${dshort}.tab
cut -f 1,4 $desc | grep -P '^AT' >> ${dcurator}.tab
cut -f 1,5 $desc | grep -P '^AT' | sed -r 's/; Has [0-9].*//' >> ${dautomatic}.tab

rm $desc



gff=TAIR10_GFF3_genes.gff
wget -O $gff 'ftp://ftp.arabidopsis.org/home/tair/Genes/TAIR10_genome_release/TAIR10_gff3/TAIR10_GFF3_genes.gff'

sed -ri "/gene\t/s/ID=([^;]+);.*/\1/"    $gff
sed -ri "/exon\t/s/Parent=([^;]+).*/\1/" $gff
sed -ri "/UTR\t/s/Parent=([^;]+).*/\1/"  $gff

awk '
    BEGIN {
        OFS="\t"
        print "locus",
              "gene_length",
              "gene_start",
              "gene_end",
              "gene_strand"
    }
    $3 ~ /gene/  {
        locus=$9
        start=$4
        stop=$5
        len=stop - start + 1
        strand=$7
        print locus, len, start, stop, strand
    }
' $gff > locus-data.tab

awk '
    BEGIN {
        OFS="\t"
        print "model",
              "model_exons",
              "model_5UTR_length",
              "model_3UTR-length"
    }
    $3 ~ /exon$/            { exons[$9]++ }
    $3 ~ /five_prime_UTR$/  { utr5[$9] = $5 - $4 + 1 }
    $3 ~ /three_prime_UTR$/ { utr3[$9] = $5 - $4 + 1 }
    END {
        for (k in exons){
            print k, exons[k], utr5[k], utr3[k]
        }
    }
' $gff > model-data.tab
rm $gff



conf=conf.tab
wget -O $conf 'ftp://ftp.arabidopsis.org/home/tair/Genes/TAIR10_genome_release/TAIR10_gene_confidence_ranking/confidenceranking_gene'
awk '
    BEGIN{OFS="\t"}
    NR == 1 {
        print "model", 
              "confidence_rating",
              "confidence_overall",
              "confidence_transcript",
              "confidence_proteomic",
              "condifdence_species",
              "confidence_vista"
    }
    NR > 1 {
        print $1,         # model
              length($5), # number of stars
              $7,         # overall
              $9,         # transcript
              $11,        # proteomic
              $13,        # species
              $15         # vista
    }
    ' $conf > confidence.tab
rm $conf



# House data
datadir=~/research/DATASETS
awk '
    BEGIN{FS="\t"; OFS="\t"}
    NR == 1 {print "model", "model_stratum_level", "model_stratum_name"}
    NR > 1  {print $2, $1, $3}
    ' $datadir/strata-models.tab > strata-models.tab

awk '
    BEGIN{FS="\t"; OFS="\t"}
    NR == 1 {print "locus", "stratum_level", "stratum_name"}
    NR > 1  {print $2, $1, $3}
    ' $datadir/strata-loci.tab > strata-loci.tab

# 1   gb
# 2   length
# 3   model
# 4   locus
# 5   gi
# 6   orphan
# 7   exon.count
# 8   MW
# 9   pI
# 10  location
# 11  transmembrane.domains
# 12  structural.class
# 13  GO.terms
# 14  PO.terms
# 15  regulon.number
# 16  regulon.function
# 17  desc
# 18  stratum
# 19  ps
# 20  masked
# 21  GC
# 22  ncomp
# 23  nvar
# 24  pcomp
# 25  pvar
# 26  cds.length
# 27  emboss.comp
# 28  coils
# 29  rem465
# 30  hotloops
awk 'BEGIN{FS="\t"; OFS="\t"}
     {print $3, $1, $5, $9, $10, $11, $15, $16, $20, $21, $28, $29, $30}
    ' ~/research/DATASETS/all-tips-data.tab |
    tr -d '"' > protein-data.tab
