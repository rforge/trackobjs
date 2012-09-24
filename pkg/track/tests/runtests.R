if (Sys.getenv('_R_CHECK_TIMINGS_')!='') {
    cat('Not running tests because appear to running on CRAN.\n')
} else if (require(scriptests)) {
    runScripTests()
} else {
    cat("Not running tests because cannot find 'scriptests' package.\n")
}
