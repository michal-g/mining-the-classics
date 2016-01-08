
# the folder where converted novel files are stored
novel.folder <- '~/Dropbox/CODEX Hackathon Data/converted_files/Classics/';
source('novel_code.R');

# gets a keyword corresponding to a novel title from the command line
args <- commandArgs(TRUE);
novel.title <- as.character(args[1]);
chunk.size <- 250;
chunk.res <- 2;

# finds the converted file matching the keyword, makes sure there's exactly one such file
novel.file <- list.files(path = novel.folder, pattern = "Time Machine", full.names = TRUE);
if (length(novel.file) == 0) {
	stop("No converted novel files matching this title keyword!");
} else if (length(novel.file) > 1) {
	stop("Multiple novel files matching this title keyword!");
	}

# reads in the novel text, removes empty lines and chapter headings
novel.text <- readLines(novel.file);
novel.text <- novel.text[sapply(novel.text, nchar) > 0];
chapter.index <- grep("^CHAPTER", novel.text);
if (length(chapter.index)) novel.text <- novel.text[-chapter.index];
novel.text <- paste(novel.text, collapse = ' ');

# breaks the novel text into overlapping chunks and creates a document term matrix
text.chunks <- chunk.text(novel.text, chunk.size = chunk.size, chunk.res = chunk.res);
novel.corpus <- Corpus(VectorSource(text.chunks));
term.mat <- DocumentTermMatrix(
		x = novel.corpus,
		control = list(
				stemming = TRUE,
				stopwords = TRUE,
				wordLengths = c(3,Inf),
				removeNumbers = TRUE,
				removePunctuation = TRUE
				)
		);

# removes words that only appear once in the text from the term matrix
term.mat <- term.mat[ ,col_sums(term.mat) > chunk.res];

