library(gtools)

GAMMAS = list()
GAMMAS$HHH = c(5, 9, 13)^(-1)
GAMMAS$HHL = c(9, 13, -1)^(-1)
GAMMAS$LLL = c(-1, -0.5, 0)^(-1) # TODO: Make sure that the extreme value index for Beta distribution is correct.

# TODO: Make sure that everything actually works.

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
    dffs      =   t(apply(raw_data, 1, function(x) x-gammas))

    out = list()
    out[[ paste( sub("data/out_", "", str ), as.character(1/gammas[1]), sep="_") ]] = dffs[,1]
    out[[ paste( sub("data/out_", "", str ), as.character(1/gammas[2]), sep="_") ]] = dffs[,2]
    out[[ paste( sub("data/out_", "", str ), as.character(1/gammas[3]), sep="_") ]] = dffs[,3]

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

