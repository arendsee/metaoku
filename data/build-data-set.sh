#!/bin/bash
set -u

# This is a tab-delimited file with the following columns:
# 1. name        - column name in datafile header, shinyapp datatable header, and the
#                  internal variable name. MUST BE UNIQUE.
# 2. group       - a name for a group of columns, allowing them to be ordered together in the column selection table
# 3. description - a (preferably) short description of the field
# 4. source      - the name of the entity providing the data
# 5. link        - a link to the original data
# 6. references  - a reference for the dataset (preferably a DOI)
METADATA=METADATA
echo -e "column_name\tgroup\tdescription\tsource\tlink\treference" > $METADATA
load-meta (){
    awk -v nam="$1" \
        -v grp="$2" \
        -v des="$3" \
        -v src="$4" \
        -v lnk="$5" \
        -v ref="$6" \
        'BEGIN{OFS="\t"; print nam, grp, des, src, lnk, ref}' >> $METADATA
}



# ========================
# --- description data ---
# ========================

desc_file=description.tab
desc_grp=Descriptions
desc_src=TAIR
desc_link='ftp://ftp.arabidopsis.org/home/tair/Genes/TAIR10_genome_release/TAIR10_functional_descriptions_20140331.txt'
desc_ref='10.1093/nar/gkr1090'

dtype='type'
dshort='short_description'
dcurator='curator_description'
dautomatic='automatic_description'

wget -O $desc_file $desc_link
echo -e "model\t$dtype"      > ${dtype}.tab
echo -e "model\t$dshort"     > ${dshort}.tab
echo -e "model\t$dcurator"   > ${dcurator}.tab
echo -e "model\t$dautomatic" > ${dautomatic}.tab
cut -f 1,2 $desc_file | grep -P '^AT' >> ${dtype}.tab
cut -f 1,3 $desc_file | grep -P '^AT' >> ${dshort}.tab
cut -f 1,4 $desc_file | grep -P '^AT' >> ${dcurator}.tab
cut -f 1,5 $desc_file | grep -P '^AT' | sed -r 's/; Has [0-9].*//' >> ${dautomatic}.tab

rm $desc_file


load-meta $dtype $desc_grp \
          'gene type (protein_coding, mirnda, etc.)' \
          $desc_src $desc_link $desc_ref

load-meta $dshort $desc_grp \
          'short description of the gene' \
          $desc_src $desc_link $desc_ref

load-meta $dcurator $desc_grp \
          'full curators description of the gene' \
          $desc_src $desc_link $desc_ref

load-meta $dautomatic $desc_grp \
          'automatically generated description of the gene' \
          $desc_src $desc_link $desc_ref



# ================
# --- gff data ---
# ================

gff_file=TAIR10_GFF3_genes.gff
gff_src=TAIR
gff_grp='Gene model'
gff_link='ftp://ftp.arabidopsis.org/home/tair/Genes/TAIR10_genome_release/TAIR10_gff3/TAIR10_GFF3_genes.gff'
gff_ref='10.1093/nar/gkr1090'
wget -O $gff_file $gff_link
 
sed -ri "/gene\t/s/ID=([^;]+);.*/\1/"    $gff_file
sed -ri "/exon\t/s/Parent=([^;]+).*/\1/" $gff_file
sed -ri "/UTR\t/s/Parent=([^;]+).*/\1/"  $gff_file

awk '
    BEGIN {
        OFS="\t"
        print "locus",
              "chromosome",
              "gene_length",
              "gene_start",
              "gene_end",
              "gene_strand"
    }
    $3 ~ /gene/  {
        chr=$1
        locus=$9
        start=$4
        stop=$5
        len=stop - start + 1
        strand=$7
        print locus, chr, len, start, stop, strand
    }
' $gff_file > locus_data.tab

awk '
    BEGIN {
        OFS="\t"
        print "model",
              "model_exons",
              "model_5UTR_length",
              "model_3UTR_length"
    }
    $3 ~ /exon$/            { exons[$9]++ }
    $3 ~ /five_prime_UTR$/  { utr5[$9] = $5 - $4 + 1 }
    $3 ~ /three_prime_UTR$/ { utr3[$9] = $5 - $4 + 1 }
    END {
        for (k in exons){
            print k, exons[k], utr5[k], utr3[k]
        }
    }
' $gff_file > model_data.tab
rm $gff_file

load-meta chromosome "$gff_grp" \
          'the chromosome containing the gene' \
          $gff_src $gff_link $gff_ref

load-meta model_exons "$gff_grp" \
          'number of exons in gene model' \
          $gff_src $gff_link $gff_ref

load-meta model_5UTR_length "$gff_grp" \
          "length of gene model's 5-prime UTR" \
          $gff_src $gff_link $gff_ref

load-meta model_3UTR_length "$gff_grp" \
          "length of gene model's 3-prime UTR" \
          $gff_src $gff_link $gff_ref

load-meta gene_length "$gff_grp" \
          "total length of the gene" \
          $gff_src $gff_link $gff_ref

load-meta gene_start "$gff_grp" \
          'chromosomal starting position' \
          $gff_src $gff_link $gff_ref

load-meta gene_end "$gff_grp" \
          'chromosomal ending position' \
          $gff_src $gff_link $gff_ref

load-meta gene_strand "$gff_grp" \
          "the orientation of the gene ('+' or '-')" \
          $gff_src $gff_link $gff_ref



# =============================
# --- gene model confidense ---
# =============================

conf_link='ftp://ftp.arabidopsis.org/home/tair/Genes/TAIR10_genome_release/TAIR10_gene_confidence_ranking/confidenceranking_gene'
conf_grp='Gene model confidence'
conf_src=TAIR
conf_file=conf.tab
conf_ref='10.1093/nar/gkr1090, ftp://ftp.arabidopsis.org/home/tair/Genes/TAIR_gene_confidence_ranking/DOCUMENTATION_TAIR_Gene_Confidence.pdf'
wget -O $conf_file  $conf_link

sed -r 's/ *\| */\t/g' $conf_file |
    awk '
        BEGIN{FS="\t"; OFS="\t"}
        NR == 1 {
            print "model", 
                  "confidence_overall",
                  "confidence_transcript",
                  "confidence_proteomic",
                  "confidence_species",
                  "confidence_vista"
        }
        NR > 1 {
            print $1, # model
                  $4, # overall
                  $5, # transcript
                  $6, # proteomic
                  $7, # species
                  $8  # vista
        }
        ' > confidence.tab
rm $conf_file
 
load-meta \
    confidence_overall "$gff_grp" \
    "Level of evidence supporting gene model (where 10 means no experimental support), accounts for transcript, proteomic, X-species, and VISTA evidence." \
    $gff_src $gff_link $gff_ref

load-meta \
    confidence_transcript "$gff_grp" \
    "Level of transcript support for intron/exon pattern (1 means very strong support)" \
    $gff_src $gff_link $gff_ref

load-meta \
    confidence_proteomic "$gff_grp" \
    "Level of proteomic support (Castellana 2008), 10 means no support" \
    $gff_src $gff_link "$gff_ref;10.1073/pnas.0811066106"

load-meta \
    confidence_species "$gff_grp" \
    "Level of homology to 5 other dicots, 10 means no support" \
    $gff_src $gff_link $gff_ref

load-meta \
    confidence_vista "$gff_grp" \
    "Vista genomic conservation" \
    $gff_src $gff_link "$gff_ref;10.1093/nar/gkh458"


# ==================
# --- House Data ---
# ==================

datadir=~/research/DATASETS
# awk '
#     BEGIN{FS="\t"; OFS="\t"}
#     NR == 1 {print "model", "model_stratum_level", "model_stratum_name"}
#     NR > 1  {print $2, $1, $3}
#     ' $datadir/strata-models.tab > strata_models.tab
# 
awk '
    BEGIN{FS="\t"; OFS="\t"}
    NR == 1 {print "locus", "stratum_level", "stratum_name"}
    NR > 1  {print $2, $1, $3}
    ' $datadir/strata-loci.tab > strata_loci.tab

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
    tr -d '"' |
    sed '1 s/\./_/g' |
    sed '1 s/gb/accession/' > protein_data.tab


load-meta \
    stratum_level "phylostrata" \
    "phylostratum level (1 common to all cellular organisms, 21 is unique to A. thaliana)" \
    "house" "arendsee@iastate.edu" "doi:10.1016/j.tplants.2014.07.003"

load-meta \
    stratum_name "phylostrata" \
    "phylostratum name, i.e. the clade into which all homologs of the gene can be placed" \
    "house" "arendsee@iastate.edu" "doi:10.1016/j.tplants.2014.07.003"

load-meta \
    accession "identifiers" \
    "RefSeq accession number" \
    "RefSeq" "http://www.ncbi.nlm.nih.gov/refseq/about/" ""

load-meta \
    gi "identifiers" \
    "GeneInfo Identifier (gi)" \
    "Genebank" "http://www.ncbi.nlm.nih.gov/Sitemap/sequenceIDs.html" ""

load-meta \
    pI "protein properties" \
    "predicted protein isoelectric point" \
    "EMBOSS pepstats" "" ""

load-meta \
    location "protein properties" \
    "predicted location" \
    "TAIR10/AtSubP" "ftp://ftp.arabidopsis.org/home/tair/Genes/TAIR10_genome_release/TAIR10-Subcellular_Predictions.xlsx" "http://dx.doi.org/10.1104/pp.110.156851"

load-meta \
    regulon_number "regulon" \
    "regulon cluster id" \
    "" "" ""

load-meta \
    regulon_function "regulon" \
    "regulon function" \
    "" "" ""

load-meta \
    masked "protein properties" \
    "the fraction of the protein that is masked by SEG" \
    "" "" ""

load-meta \
    GC \
    "genomic properties" \
    "the fraction of bases in the gene that are G or C." \
    "" "" ""

disembl_src=DisEMBL
disembl_lnk='http://dis.embl.de/html/help.html'
disembl_ref='http://dis.embl.de/html/paper.html'
load-meta \
    coils \
    "disorder" \
    "predicted coiled structure, a necessary, but not sufficient, condition for disorder" \
    $disembl_src $disembl_lnk $disembl_ref

load-meta \
    hotloops \
    "disorder" \
    "a subset of coil that is predicted to be highly dynamic" \
    $disembl_src $disembl_lnk $disembl_ref

load-meta \
    rem465 \
    "disorder" \
    "predicted regions of unassignable electron densities (see the paper)" \
    $disembl_src $disembl_lnk $disembl_ref


cp $datadir/Arabidopsis-thaliana_SS.tab ./ss.tab
load-meta \
    protein_length \
    "protein properties" \
    "protein length" \
    "TAIR10" "" ""

ss_src='predicted with psipred'
ss_lnk='arendsee@iastate.edu'
ss_ref='10.1093/bioinformatics/16.4.404'
load-meta \
    helix \
    "protein secondary structure" \
    "predicted proportion of protein helical content" \
    "$ss_src" "$ss_lnk" "$ss_ref"

load-meta \
    sheet \
    "protein secondary structure" \
    "predicted proportion of protein beta-sheet content" \
    "$ss_src" "$ss_lnk" "$ss_ref"

load-meta \
    coil \
    "protein secondary structure" \
    "predicted proportion of protein coil content" \
    "$ss_src" "$ss_lnk" "$ss_ref"
