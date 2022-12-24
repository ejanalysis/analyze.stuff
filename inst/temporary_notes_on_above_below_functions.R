
# decide if names of functions make sense...  cols.above ? is that related to rowMins or colMins?
# how many cols are above cutoff gives one summary answer per row, so its analogous to rowMins not colMins.
# So my cols.above.count is like rowCountsOfColsAbove, rowPctsColsAbove, etc., or rowCountsOfColsExceeding,
# or rowCountsOfHowManyColsExceed
# and rows.above.count = count.above

# decide if keeping multiple names for a func,
# or if dropping count.above, pct.above, & below to just keep rows... and cols...
# or rename all to be count.above but where you must specify which dimension, like in apply ??
# search for code that relies on those functions if they get changed!!!


#  RESOLVE WHICH PARAMETERS TO USE IN cols... count... rows... pct...  -- cutoffs, benchmarks, of.what
# what did of.what even do?

# allow parameter benchmarks or cutoffs to be a vector of cutoffs as long as relevant dim
# (i.e., allow a different row-specific cutoff for each row, or 1 per col)

#  make cols.below... functions like rows.below...

#  check docs for all

#  call this 0.2.1 ?

#  remake pdf etc. and post new one.
