library(gtools)
library(optparse)

GAMMAS = list()
GAMMAS$HHH = c(5, 9, 13)^(-1)
GAMMAS$HHL = c(9, 13, -1)^(-1)
GAMMAS$LLL = c(-1, -0.5^(-1), 0) # TODO: Make sure that the extreme value index for Beta distribution is correct.

help="
    
    ARGUMENTS:
	
	x   -	A vector of list names.

    OUTPUT: A vector of list names where the elements are ordered so that [ICA-METHOD] and none are always next to each other.
"
plot_order = function(x){
    y = x[-which(grepl("none",x))]
    z = x[which(grepl("none",x))]
}

help="
    
    ARGUMENTS:
	
	x   -	Vector to be optimized
	y   -	Optimization criteria

    OUTPUT: A permutation of x that minimizes the L^2 distance to y

"
optimize_vector = function(x, y){

    perms = permutations(length(x), length(x), x)
    dsts  = apply(perms, 1, function(x) sum((x-y)^2))
    return(perms[which(dsts==min(dsts))[1], ])

}

help="
    
    ARGUMENTS:
	
	A   -	Array to be optimized
	y   -	Optimization criteria

    OUTPUT: An array where each row is permuted so that the L^2 distance of each
	    to y is minimized.

"
optimize_array = function(A, y){

    return(t(apply(A, 1, function(x) optimize_vector(x, y))))

}


help="
    
    ARGUMENTS:
	
	str   -	  A filename of the form out_[distribution_type]_[ev_estimator]_[sample_size]_[threshold]_[ica_method]

    OUTPUT: A list L with entries L[[ str_G ]] where is iterated over the gamma-values.

"
process_file = function(str){

    tmp       =   strsplit(str, "_")[[1]]
    gammas    =   get(tmp[2], GAMMAS)
    tmp       =   read.table(str, sep=";", header=T)
    tmp       =   tmp[complete.cases(tmp),]
    raw_data  =   optimize_array(tmp, gammas)
    #dffs      =   t(apply(raw_data, 1, function(x) x-gammas))
    dffs      =   raw_data # Instead of differences, plot the raw data.

    out     = list()
    gammas  = apply(matrix(gammas), 1, function(x) if(x==0){return(x);}else{return(1/x);}); 

    out[[ paste( sub("data/out_", "", str ), as.character(gammas[1]), sep="_") ]] = dffs[,1]
    out[[ paste( sub("data/out_", "", str ), as.character(gammas[2]), sep="_") ]] = dffs[,2]
    out[[ paste( sub("data/out_", "", str ), as.character(gammas[3]), sep="_") ]] = dffs[,3]

    return(out)

}


help="
    
    ARGUMENTS:
	
	fnames   -    A list of files to process.

    OUTPUT: As in process_file, but with more values.

"

process_files = function(fnames){
    out = list()
    for(f in fnames){
        out = c(out, process_file(f))
    }
    return(out)
}

# Parse arguments
option_list = list(
  make_option("--files", type="character", default=NA,
                help="Which files to process in form [fname1],[fname2],....,[fnameN]"),
  make_option("--op", type="character", default=NA,
                help="What to do with the data. Choices: histogram, barplot"),
  make_option("--pauliina", type="character", default=NA,
		help="Which sample size and threshold to use with op pauliina in form [sample size]_[threshold]")
);

opt_parser  =   OptionParser(option_list=option_list);
OPT         =   parse_args(opt_parser);

if(OPT$op == "pauliina"){
    fs = list.files("data")
    files = paste("data", fs[which(grepl(OPT$pauliina, fs))], sep="/")
} else{
    OPT$pauliina_params = 0
}

if(NA %in% OPT && OPT$op != "pauliina"){ 
    stop("Missing arguments. See --help.");
}

if(OPT$op != "pauliina"){
    files   =   strsplit(OPT$files, split=",")[[1]]
}

if(OPT$op == "boxplot_with_control"){
    files = c(files, sub("fICA|FOBI","none",files))
}

data 	    =   process_files(files)

if(OPT$op == "histogram"){
    
    # The number of files has to be exactly 2.
    if(length(files)!=2){stop("The histogram plotter requires exactly 2 files.");}
    
    # Six histograms in one plot.
    par(mfrow=c(2,3))

    for(f in names(data)){
        hist(get(f, data), main=f)
    }

}

if(OPT$op == "boxplot"){
    boxplot(data);
}


if(OPT$op == "boxplot_with_control"){
    boxplot(data);
}

if(OPT$op == "pauliina"){
    evs = c("hill", "moment")
    icas = c("fICA", "FOBI", "none")
    dists = c("HHH", "HHL", "LLL")
    for(d in dists){
	str0 = "DST_EV_ARG_ICA_GAMMA"
	str0 = gsub("ARG",OPT$pauliina, str0)
	str0 = gsub("DST", d, str0)
	for(g in get(d, GAMMAS)){
	    if(g!=0){g=1/g}
	    str = gsub("GAMMA", g, str0)
	    inds = c(gsub("EV", "hill", str),gsub("EV", "moment",  str))
	    inds = c(rep(inds[1], 3), rep(inds[2],3))
	    inds[1] = gsub("ICA", "fICA", inds[1])
	    inds[2] = gsub("ICA", "FOBI", inds[2])
	    inds[3] = gsub("ICA", "none", inds[3])
	    inds[4] = gsub("ICA", "fICA", inds[4])
	    inds[5] = gsub("ICA", "FOBI", inds[5])
	    inds[6] = gsub("ICA", "none", inds[6])
	    boxplot(data[inds])
	}
    }
}
