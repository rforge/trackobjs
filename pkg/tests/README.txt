All test files must use a different tracking database
because the tests can be run in parallel.  This database
is created in the 'tests' directory.
One option would be to do
> setwd(tmpdir())
at the top of each test.  The option used here is to just
have different name for each tracking database (i.e., "tmp1",
"tmp2", etc.)
