NOTE:

(1) All test files must use a different tracking database
    because the tests can be run in parallel.  This database
    is created in the 'tests' directory.
    One option would be to do
    > setwd(tmpdir())
    at the top of each test.  The option used here is to just
    have different name for each tracking database (i.e., "tmp1",
    "tmp2", etc.)

(2) Many of the tests do not run successfully when called
    interactively in scriptests with full=FALSE because the
    tests rely on task callbacks (where the 'auto' feature
    of track is used.)  Scriptests does not (cannot)
    implement callbacks in its fast interactive mode because
    they require information scriptests doesn't have.  The
    tests run fine under scriptests when called as part of
    "R CMD check" or when called with full=TRUE.
