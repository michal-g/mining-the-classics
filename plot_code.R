
# load required packages
library(ggplot2);
library(reshape2);
library(grid);
library(plotly);
library(stringdist);


plot.topic.posterior <- function(novel.lda, choose.terms = NULL, term.cutoff = 3) {

	# gets model posterior posterior data
	topic.post <- posterior(novel.lda)$topics;
	term.post <- posterior(novel.lda)$terms;

	# culls topics to those corresponding to chosen terms, if necessary
	if (!is.null(choose.terms)) {
		chosen.topics <- apply(
					apply(
						terms(novel.lda, term.cutoff), 2,
						stringdistmatrix,
						tolower(choose.terms), "cosine"
						), 2,
				       	function(x) any(x < 0.1)
					);
		}

	# finds the most important topic at each location in the novel
	colnames(topic.post) <- as.character(1:nrow(term.post));
	post.df <- melt(t(topic.post));
	colnames(post.df) <- c('Topic', 'Location', 'Posterior');
	post.df$Status <- factor(
				x = as.character(unlist(tapply(
					post.df$Posterior,
					post.df$Location,
					function(x) rep(which.max(x), nrow(term.post)))) == post.df$Topic),
				levels = c('FALSE', 'TRUE'),
				labels = c('Other', 'Top')
				);
	post.df <- post.df[as.numeric(post.df$Topic) %in% which(chosen.topics), ];
	term.post <- term.post[chosen.topics, ];

	post.df$Location <- as.numeric(post.df$Location);
	post.df$Topic <- as.factor(paste("Topic", post.df$Topic));

	theme.size <- 160;
	p <- ggplot(post.df) + geom_line(aes(x = Location, y = Posterior, group = Topic, colour = Topic, alpha = Status, size = Status));
	p <- p + theme_bw();
	p <- p + scale_x_continuous('Novel Location') + scale_y_continuous('Topic Importance');

	p <- p + scale_colour_hue(
			guide = guide_legend(
					'', keywidth = theme.size * 0.1, keyheight = theme.size * 0.08, nrow = 1,
					label.theme = element_text(size = theme.size * 0.9, angle = 0, face = 'bold'),
					override.aes = list(alpha = 0.9, size = theme.size * 0.11)
				)
			);
	p <- p + scale_alpha_manual(
			values = c("Other" = 0.2, "Top" = 0.8),
			guide = FALSE
			);
	p <- p + scale_size_manual(
			values = c("Other" = theme.size * 0.02, "Top" = theme.size * 0.05),
			guide = FALSE
			);

	p <- p + theme(
		panel.margin = unit(theme.size * 0.025, 'cm'), panel.border = element_rect(colour = 'black', size = theme.size * 0.06),
		panel.grid.major = element_line(size = theme.size * 0.006, colour = 'gray45'),
		axis.title.x = element_text(size = theme.size, face = 'bold', vjust = theme.size * -0.05),
		axis.title.y = element_text(size = theme.size, face = 'bold', vjust = theme.size * 0.05),
		axis.text.x = element_text(size = theme.size * 0.8, face = 'bold'),
		axis.text.y = element_text(size = theme.size * 0.55, face = 'bold'),
		axis.ticks = element_line(size = theme.size * 0.04),
		axis.ticks.length = unit(theme.size * 0.01, 'cm'), axis.ticks.margin = unit(theme.size * 0.01, 'cm'),
		plot.margin = unit(c(0.025,0.025,0.03,0.03) * theme.size, 'cm'), legend.key = element_blank(), legend.position = 'top'
		);

	plot.file = "test.png";
	png(filename = plot.file, height = 4500, width = 8500);
	p;
	}


