# ### e.g., GET WEIGHTED MEAN OF EACH COLUMN, BY REGION, FAST
# # examples of weighted mean using data.table package
#
# # require(data.table)
# # n=1e6
# # mydf <- data.frame(pop=1000 + rnorm(n, 1000, 100), v1= runif(n, 0, 1), v2= rnorm(n, 100, 15), REGION=sample(c('R1', 'R2', 'R3'), n, replace=TRUE))
# # dt <- data.table::data.table(mydf)
#
#
# # http://stackoverflow.com/questions/16642087/calculating-a-weighted-mean-using-data-table-in-r-with-weights-in-one-of-the-tab?lq=1
# # shows this:
#
#  # one param of weighted.mean inside lapply + shows .SDcols to select only some results:
#
# dt[ , lapply(.SD, weighted.mean, w=a), by=key,
#     .SDcols=letters[1:5] ]
#
#
# # http://stackoverflow.com/questions/13441868/data-table-and-stratified-means?lq=1
# # shows these:
#
#  # multiple params of weighted.mean inside lapply + shows .SDcols to select only some results::
#
# dt[ , lapply(.SD, weighted.mean, swt, na.rm=TRUE), by=key(dt),
#     .SDcols=c('bmi','tc','swt')]
#
#  # using function(x):
#
# dt[ , lapply(.SD, function(x) weighted.mean(x, swt, na.rm=TRUE)), by=key(dt),
#    .SDcols=c('bmi','tc','swt') ]
#
# # not sure where I found this example. uses sum(x*wts)/sum(wts) and function(x, wts):
# # and not sure it works to use by = quoted colname???:
#
# dt[ , lapply(.SD, function(x, wts = pop) {sum(x*wts)/sum(wts)} ), by = "REGION"]
#
