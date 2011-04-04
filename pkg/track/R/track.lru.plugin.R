track.lru.plugin <- function(objs, inmem, envname) {
    # A simple least-recently-used discard policy
    cumsum.ordered <- function(x, order) return(replace(x, order, cumsum(x[order])))
    # just work with the objects that are in memory
    imobjs <- objs[inmem,]
    keep <- inmem
    max.size <- getOption("track.cache.bytes", default=200e6)
    # get the order, youngest first (though ones with cache='yes' or 'fixedyes'
    by.age <- order(imobjs[,"cache"]=="yes" | imobjs[,"cache"]=="fixedyes", imobjs[,"accessed"], decreasing=TRUE)
    # which ones can we definitely keep?
    keep.by.age <- cumsum.ordered(imobjs[,"size"], by.age) <= max.size
    # now, order the potential deletion candidates by size to see if we can keep some
    # smaller ones one we get rid of the bigs ones
    by.size <- order(imobjs[!keep.by.age,"size"])
    keep.by.size <- cumsum.ordered(imobjs[!keep.by.age,"size"], by.size) <= (max.size - sum(imobjs[keep.by.age,"size"]))
    keep.by.age[which(!keep.by.age)] <- keep.by.size
    return(replace(inmem, which(inmem), keep.by.age))
}
