
# loads required packages
library(RTextTools);
library(topicmodels);

chunk.text <- function(novel.text, chunk.size = 2000, chunk.res = 2) {

	# finds where equally-spaced text chunks would start
	novel.size <- nchar(novel.text);
	chunk.starts <- round(seq(from = 1, to = novel.size, length.out = ceiling(novel.size / chunk.size)));
	chunk.lengths <- diff(chunk.starts);

	# adds overlapping chunk starts
	chunk.starts <- c(1, setdiff(sort(as.numeric(
				mapply(
					function(st, len) st - (round(len/chunk.res) * 0:(chunk.res-1)),
					chunk.starts[-1],
					chunk.lengths))),
				nchar(novel.text))
				);
	chunk.ends <- chunk.starts + chunk.size - 1;
	chunk.count <- length(chunk.starts);

	# retrieves the text corresponding to each chunk and returns it
	text.chunks <- mapply(function(st, en) substr(novel.text, st, en), chunk.starts[-chunk.count], chunk.ends[-chunk.count]);
	return(text.chunks);
	}

