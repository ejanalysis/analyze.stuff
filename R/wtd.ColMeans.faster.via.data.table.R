#########################################
# ### e.g., GET WEIGHTED MEAN OF EACH COLUMN, BY REGION, FAST
# # examples of weighted mean using data.table package
#########################################

#############################################################
if (1==0) {
# Note Hmisc::wtd.mean is not exactly same as stats::weighted.mean since na.rm defaults differ
# Hmisc::wtd.mean(x, weights=NULL, normwt="ignored", na.rm = TRUE ) # Note na.rm defaults differ.
# weighted.mean(x, w,            ...,              na.rm = FALSE)

require(data.table)
mydata <- data.table::data.table(bg, key="ST")

#########################################
# if you want to manually write out each formula with specific fields:
#########################################

z= mydata[, list(
            pctlowinc = sum(pctlowinc * pop) / sum(pop),
            pctmin    = sum(pctmin    * pop) / sum(pop)
         ),
     by = "REGION"
]

#########################################
# to specify function once and run it for all the fields
#########################################

z = mydata[ , lapply(.SD, weighted.mean, pop, na.rm=TRUE), by=key(dt),
     .SDcols=c('bmi','tc','pop')]

# or

z= mydata[, lapply(.SD,
                function(x, wts = pop) sum(x * wts) / sum(wts)
              ),
        by = "REGION"
]

#########################################
# same, but remove extra column at end
#########################################

z= mydata[, lapply(.SD,
                function(x, wts) { sum(x * wts) / sum(wts)}, wts = pop), by = REGION][ , setdiff(names(mydata), 'pop'), with = F]
# Appending [,setdiff(names(datDT), 'rate'), with = F] will remove the rate column - this column is not particularly meaningful

##################################################################################

# MORE EXAMPLES OF VARIANTS OF THIS APPROACH:

# # require(data.table)
# n=1e6
# mydf <- data.frame(pop=1000 + rnorm(n, 1000, 100), v1= runif(n, 0, 1), v2= rnorm(n, 100, 15), REGION=sample(c('R1', 'R2', 'R3'), n, replace=TRUE))
# dt <- data.table::data.table(mydf)

#########################################
#  # one param of weighted.mean inside lapply + shows .SDcols to select only some results:
# # http://stackoverflow.com/questions/16642087/calculating-a-weighted-mean-using-data-table-in-r-with-weights-in-one-of-the-tab?lq=1
#########################################
#
# dt[ , lapply(.SD, weighted.mean, w=a), by=key,
#     .SDcols=letters[1:5] ]
#

#########################################
#  # multiple params of weighted.mean inside lapply + shows .SDcols to select only some results::
# # http://stackoverflow.com/questions/13441868/data-table-and-stratified-means?lq=1
#########################################
#
# dt[ , lapply(.SD, weighted.mean, swt, na.rm=TRUE), by=key(dt),
#     .SDcols=c('bmi','tc','swt')]
#
#########################################
#  # using function(x):
# # http://stackoverflow.com/questions/13441868/data-table-and-stratified-means?lq=1
#########################################
#
# dt[ , lapply(.SD, function(x) weighted.mean(x, swt, na.rm=TRUE)), by=key(dt),
#    .SDcols=c('bmi','tc','swt') ]
#
#########################################
# # not sure where I found this example. uses sum(x*wts)/sum(wts) and function(x, wts):
# # and not sure it works to use by = quoted colname???:
#########################################
#
# dt[ , lapply(.SD, function(x, wts = pop) {sum(x*wts)/sum(wts)} ), by = "REGION"]


#############################################################
#############################################################

}
