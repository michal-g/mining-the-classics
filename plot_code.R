
# load required packages
library(ggplot2);
library(reshape);
library(grid);
library(plotly);


plot.topic.posterior <- function(lda.model) {

	theme.size <- 160;
	p <- ggplot(post.df) + geom_line(aes(x = Location, y = Posterior, group = Topic, colour = Topic, alpha = Status, size = Status), alpha = 0.8);
	
	p <- p + theme_bw();
	p <- p + scale_x_continuous('Novel Location') + scale_y_continuous('Topic Importance');
	p <- p + scale_colour_hue(
			guide = guide_legend(
					'', keywidth = theme.size * 0.1, keyheight = theme.size * 0.08,											                                            nrow = 1, label.theme = element_text(size = theme.size * 0.9, angle = 0, face = 'bold')
				)
			);

	p <- p + theme(
		panel.margin = unit(theme.size * 0.025, 'cm'), panel.border = element_rect(colour = 'black', size = theme.size * 0.06),
		panel.grid.major = element_line(size = theme.size * 0.006, colour = 'gray45'),
		axis.title.x = element_text(size = theme.size, face = 'bold', vjust = theme.size * -0.05),
		axis.title.y = element_text(size = theme.size, face = 'bold', vjust = theme.size * 0.001),
		axis.text.x = element_text(size = theme.size * 0.8, face = 'bold'),
		axis.text.y = element_text(size = theme.size * 0.55, face = 'bold'),
		axis.ticks = element_line(size = theme.size * 0.04), axis.ticks.length = unit(theme.size * 0.01, 'cm'), axis.ticks.margin = unit(theme.size * 0.01, 'cm'),
		plot.margin = unit(c(0.025,0.025,0.03,0.03) * theme.size, 'cm'), legend.key = element_blank(), legend.position = 'top'
		);

	plot.file = paste(plot.dir, 'rf_error-rate.png', sep = '');
	png(filename = plot.file, height = 4500, width = 6200);
	p;
	}


