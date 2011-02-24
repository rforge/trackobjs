This directory contains some scripts that do timed tests on
tracking with large objects.  Some transcripts with timings
of tests on several machines are also in this directory.

The transcripts were created by pasting the various test
files into and R session and then copying out the
transcript.

funsForTesting.R: (support) functions to create test objects
    of various classes (vector, matrix, list, etc.) and of
    various sizes.  Note that the test objects are tuned to
    of the same size on a 32bit system (Windows) - sizes do
    not match as well on a 64bit system.

testObjSizes.R: (support) show how large the various test objects are

memoryTests.R: create a tracking directory with 33 large
    objects in it, and test retrieval times with and without
    caching.

speedTests.R: try saving and loading 11 medium size objects
    of different classes

memoryLimits.R: limit memory to 128Mb, and then create and
    save 40 objects of about 16Mb each (total about 640Mb)

The HP XW4300 system used for tests is running Ubuntu 6.06
64 bit.  It has fast disks (10K), 4GB of memory, and dual
3.2G0GHz processors.

