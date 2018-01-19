library(permute)

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

    OUTPUT: A list L with the following data

	* S$distribution_type
	* S$ev_estimator
	* S$sample_size
	* S$threshold
	* S$ica_method
"
params_from_filename = function(str){

    out = list()
    tmp = strsplit(str, "_")[[1]]
    print(tmp)
    out$distribution_type =   tmp[2]
    out$ev_estimator	  =   tmp[3] 
    out$sample_size	  =   tmp[4]
    out$threshold	  =   tmp[5]
    out$ica_method	  =   tmp[6]
    return(out)

}
