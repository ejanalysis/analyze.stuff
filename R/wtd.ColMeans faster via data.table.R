# require(data.table)

# mydata <- data.table(mydf)

# GET WEIGHTED MEAN OF EACH COLUMN, BY REGION, FAST

# x = mydata[, lapply(.SD, function(x, y = pop) sum(y * x)/sum(y)), by = "REGION"]
