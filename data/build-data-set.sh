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

sed -ri "/gene\t/s/ID=([^;]+);.*/\1/" $gff
sed -ri "/exon\t/s/Parent=([^;]+).*/\1/" $gff
sed -ri "/UTR\t/s/Parent=([^;]+).*/\1/" $gff

echo -e "locus\tgene_length" > gene_length.tab
awk '
    BEGIN{OFS="\t"}
    $3 ~ /gene/ {
        seqid=$9
        len=$5 - $4 + 1
        print seqid, len
    }
    ' $gff >> gene_length.tab


echo -e "locus\tstart" > gene_start.tab
awk '
    BEGIN{OFS="\t"}
    $3 ~ /gene/ {
        seqid=$9
        start=$4
        print seqid, start
    }
    ' $gff >> gene_start.tab 


echo -e "locus\tend" > gene_end.tab
awk '
    BEGIN{OFS="\t"}
    $3 ~ /gene/ {
        seqid=$9
        end=$5
        print seqid, end
    }
    ' $gff >> gene_end.tab 


echo -e "locus\tstrand" > strand.tab
awk '
    BEGIN{OFS="\t"}
    $3 ~ /gene$/ {
        seqid=$9
        strand=$7
        print seqid, strand
    }
    ' $gff >> strand.tab 

echo -e "model\tnexons" > nexons.tab
awk '
    BEGIN{OFS="\t"}
    $3 ~ /exon$/ { a[$9]++ }
    END{ for(k in a) {print k, a[k]} }
    ' $gff >> nexons.tab

echo -e "model\tfive-prime_UTR-length" > five-UTR.tab
awk '
    BEGIN{OFS="\t"}
    $3 ~ /five_prime_UTR$/ {
        model=$9
        len=$5-$4+1
        print model, len
    }
' $gff >> five-UTR.tab

echo -e "model\tthree-prime_UTR-length" > three-UTR.tab
awk '
    BEGIN{OFS="\t"}
    $3 ~ /three_prime_UTR$/ {
        model=$9
        len=$5-$4+1
        print model, len
    }
    ' $gff >> three-UTR.tab

rm $gff



conf=conf.tab
wget -O $conf 'ftp://ftp.arabidopsis.org/home/tair/Genes/TAIR10_genome_release/TAIR10_gene_confidence_ranking/confidenceranking_gene'

echo -e "model\tconfidence_rating" > confidence_rating.tab
awk '
    BEGIN{OFS="\t"}
    NR > 1 {print $1, length($5)}
    ' $conf >> confidence_rating.tab 

echo -e "model\toverall_confidence" > overall_confidence.tab
awk '
    BEGIN{OFS="\t"}
    NR > 1 {print $1, $7}
    ' $conf >> overall_confidence.tab

echo -e "model\ttranscript_confidence" > transcript_confidence.tab
awk '
    BEGIN{OFS="\t"}
    NR > 1 {print $1, $9}
    ' $conf >> transcript_confidence.tab

echo -e "model\tproteomic_confidence" > proteomic_confidence.tab
awk '
    BEGIN{OFS="\t"}
    NR > 1 {print $1, $11}
    ' $conf >> proteomic_confidence.tab

echo -e "model\tspecies_confidence" > species_confidence.tab
awk '
    BEGIN{OFS="\t"}
    NR > 1 {print $1, $13}
    ' $conf >> species_confidence.tab

echo -e "model\tvista_confidence" > vista_confidence.tab
awk '
    BEGIN{OFS="\t"}
    NR > 1 {print $1, $15}
    ' $conf >> vista_confidence.tab

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
