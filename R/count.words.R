#' @title Word Frequency in a Text File
#' @description Simple way to count how many times each word appears in a text file.
#'
#' @param file Character string filename, with or without path, for text file to be analyzed. Words assumed to be separated by spaces.
#' @param wordclump  number of words per clump, so if wordclump=2, it counts how often each 2-word phrase appears.
#' @param ... Any other parameters used by [scan()] may be passed through. See <http://stat.ethz.ch/R-manual/R-devel/library/base/html/scan.html>
#' @param ignore.case Logical, default TRUE which means not case-sensitive.
#' @param stopwords Vector of words to ignore and not count. Default is none, optional.
#' @param numbers.keep Not yet implemented. Would ignore numbers.
#' @param string A single character string containing text to analyze. Not yet implemented.
#' @return Returns a data.frame with term (term) and frequencies (freq) sorted by frequency,
#' showing the number of times a given word appears in the file. The rownames are also the words found.
#' @examples
#' \dontrun{
#'   counts <- count.words('speech.txt'); tail(counts, 15)
#' counts <- count.words('speech.txt', ignore.case=FALSE); head(counts[order(counts$term), ], 15)
#' counts <- count.words('speech.txt', stopwords=c('The', 'the', 'And', 'and', 'A', 'a'))
#' tail(counts, 15)
#' counts <- count.words('speech.txt', 3); tail(counts, 30)
#' #
#' counts['the', ]
#' counts[c('the', 'and', 'notfoundxxxxx'), ] # works only if you are sure all are found
#' counts[rownames(counts) %in% c('the', 'and', 'notfoundxxxxx'), ]
#'   # that works even if specified word wasn't found
#' counts[counts$term %in% c('the', 'and', 'notfoundxxxxx'), ]
#'   # that works even if specified word wasn't found
#' counts <- count.words('C:/mypath/speech.txt')
#' counts <- count.words('speech.txt', sep='.')
#'   # that is for whole sentences (sort of - splits up at decimal places as well)
#' }
#' @export
count.words <- function(file, wordclump=1, ignore.case=TRUE, stopwords='', string, numbers.keep=TRUE, ...) {

  x <- scan(file, 'character', ...)
  x <- gsub('[[:punct:]]', '', x)  # remove all punctuation, but that removes even the hyphen in middle of hyphenated words like effort-free? (which is not ideal)
  x <- gsub('\u2013', '', x)  # remove this type of hyphen
  x <- gsub('\u201C', '', x)  # remove this type of quote
  x <- gsub('\u201D', '', x)  # remove this type of quote
  x <- gsub('\u2019', '', x)  # remove this type of apostraphe
  x <- gsub(' {2,}', ' ', x) # convert 2+ spaces into a single space
  # remove leading and trailing blank spaces, in case those are present now
  x <- gsub('^\\s+|\\s+$', '', x)
  x <- x[!is.na(x)]
  x <- x[nchar(x) > 0]
  x <- x[!(x %in% stopwords)]
  if (ignore.case) {x <- tolower(x)}

  # create n-wordclump phrases of wordclump words each
  n <- length(x)
  x2 <- rep('', n - wordclump)
  for (ii in 1:wordclump) {
    # vector of all the 1st words, then all the second words of phrase, etc. (all ii-th words)
    # If memory/speed is an issue, could avoid creating xii step
    xii <- x[ii:(n - 1 + ii - wordclump)]
    if (ii==1) {x2 <- xii} else {x2 <- paste(x2, xii)} # assembles into phrases
  }
  x <- x2

  unique.word.count <- length(unique(x))
  counts <- matrix(nrow=unique.word.count, ncol=1)
  # tried to use preallocated matrix and assign into it for speed, but it fails to assign the rownames then
  # counts[1:unique.word.count, 1] <- cbind(sort(table(x)))
  counts <- cbind(sort(table(x)))
  colnames(counts) <- 'freq'
  counts.df <- data.frame(term=rownames(counts), freq=counts, stringsAsFactors=FALSE)
  # return(counts)
  return(counts.df)
}

