require(data.table)

# All datasets are
# 1. TAB-delimited
# 2. Have headers
# 3. The first column is either 'model' or 'locus'
LoadData <- function(){

    d <- list()

    # ==> gc.tab <==
    #       model     cds-gc
    # ATCG00500.1   0.463064
    # ATCG00510.1   0.477064
    # ATCG00280.1   0.422724
    # ATCG00890.1   0.473820
    d$gc <- data.table(read.table(file='data/gc.tab', header=TRUE), key='model')

     
    # ==> gene-names.tab <==
    #     locus   gene_name
    # AT1G01010     ANAC001
    # AT1G01010      NAC001
    # AT1G01020        ARV1
    # AT1G01030        NGA3
    d$gene.names <- data.table(read.table(file='data/gene-names.tab', header=TRUE), key='locus')
     
    # ==> lengths.tab <==
    #       model   length
    # ATCG00500.1      489
    # ATCG00510.1       38
    # ATCG00280.1      474
    # ATCG00890.1      390
    d$len <- data.table(read.table(file='data/lengths.tab', header=TRUE), key='model')
     
    # ==> secondary-structure.tab <==
    #       model   coil   beta-sheet   alpha-helix
    # AT4G14350.3    324           44           180
    # AT5G39970.1    475          137            78
    # AT1G08220.2     91           43            68
    # AT1G71020.1    238            7           383
    d$ss <- data.table(read.table(file='data/secondary-structure.tab', header=TRUE), key='model')
     
    # ==> strata-loci.tab <==
    # locus       phylostratum_level    phylostratum_name
    # AT2G01830                    1   cellular_organisms
    # AT1G08520                    1   cellular_organisms
    # AT1G76370                    1   cellular_organisms
    # AT1G09620                    1   cellular_organisms
    d$strata.loci <- data.table(read.table(file='data/strata-loci.tab', header=TRUE), key='locus')
     
    # ==> strata-models.tab <==
    #       model   phylostratum_level    phylostratum_name
    # AT1G61810.1                    1   cellular_organisms
    # AT3G56900.1                    1   cellular_organisms
    # AT1G18080.1                    1   cellular_organisms
    # AT1G05620.1                    1   cellular_organisms
    d$strata.models <- data.table(read.table(file='data/strata-models.tab', header=TRUE), key='model')
    d$strata.models$phylostratum_level <- factor(d$strata.models$phylostratum_level)

    return(d)
}

MergeData <- function(d){
    by.model <- d$strata.models[d$len[d$gc[d$ss]]]
    by.model$locus <- sub('\\.\\d+$', '', by.model$model)
    return(by.model)
}

d <- LoadData()
model.data <- MergeData(d)
save(model.data, file='model.Rdat')
