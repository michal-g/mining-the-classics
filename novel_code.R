
# loads required packages
library(RTextTools);
library(topicmodels);
library(tm);
library(slam);
library(qdap);
library(SnowballC);

chunk.text <- function(novel.text, chunk.size = 200, chunk.res = 2) {

	# finds where equally-spaced text chunks would start
	novel.words <- strsplit(novel.text, ' ')[[1]]
	novel.size <- length(novel.words);
	chunk.starts <- round(seq(from = 1, to = novel.size, length.out = ceiling(novel.size / chunk.size)));
	chunk.lengths <- diff(chunk.starts);

	# adds overlapping chunk starts
	chunk.starts <- c(1, sort(as.numeric(
				mapply(
					function(st, len) st - (round(len/chunk.res) * 0:(chunk.res-1)),
					chunk.starts[-c(1,length(chunk.starts))],
					chunk.lengths[-length(chunk.lengths)]))
					)
				);
	chunk.ends <- chunk.starts + chunk.size - 1;
	chunk.ends[length(chunk.ends)] <- novel.size;

	# retrieves the text corresponding to each chunk and returns it
	text.chunks <- mapply(function(st, en) paste(novel.words[st:en], collapse = ' '), chunk.starts, chunk.ends);
	return(text.chunks);
	}

