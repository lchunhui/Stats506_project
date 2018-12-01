// data preparation
import delimited data.csv

// view the first three rows of data
list in 1/3

// do the canonical correlation analysis
canon (v2 benzene oxylene mpxylene ethylbenzene mtbe toluene chloroform tetrachloroethene trichloroethene) (albumin alt alp ast ggt ldh tb)

// test the significance of sets of canonical covariates
canon, test(1 2 3 4 5 6 7)

// inspect the significance of raw coefficients for each sets of the canonical variates
canon (v2 benzene oxylene mpxylene ethylbenzene mtbe toluene chloroform tetrachloroethene trichloroethene) (albumin alt alp ast ggt ldh tb), first(2) stderr

// display the loadings of the variables on the canonical variates
estat loadings


