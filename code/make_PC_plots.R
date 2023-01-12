setwd("/home/sam/Dropbox/Documents/PhD/PhD/analyses/ChromoMatcher/finestructure/data")

chunkcounts = sort(list.files(pattern="chunkcounts.out"))

make_pca = function(x, n_pcs = 10) {
	x_dat = fread(x, header=T)
	x_dat = melt(x_dat, id.vars="Recipient"
			)[, value := fifelse(Recipient == variable, mean(value), value), Recipient
				][, dcast(.SD, Recipient~variable)]

	pca_res = irlba::prcomp_irlba(x_dat[,-1], n=10)
	pca = data.table(ind = x_dat$Recipient, irlba::prcomp_irlba(x_dat[,-1], n=10)$x)
	pca$pop = str_remove_all(pca$ind, "[0-9]")

	n_pcs_vec = 1:n_pcs
	combinations = Map(c, n_pcs_vec[-length(n_pcs_vec)], n_pcs_vec[-1])

	plot_res_list = vector(length = length(n_pcs_vec), mode='list')

	plot_res_list = lapply(combinations, function(x) {
		pca %>%
			ggplot(aes(x=get(paste0("PC",x[[1]])), y=get(paste0("PC",x[[2]])))) +
			geom_point(alpha=0.6, aes(colour=pop)) +
			theme_light() + 
			theme(legend.position="none") +
			xlab(paste0("PC", x[[1]])) +
			ylab(paste0("PC", x[[2]])) +
			theme(
				axis.text = element_text(size=16),
				axis.title = element_text(size=20)
				)
	})

	n = length(plot_res_list)
	nCol = floor(sqrt(n))
	a = do.call(gridExtra::grid.arrange, c(plot_res_list, ncol=nCol))
	out = paste0(str_remove_all(x, ".chunkcounts.out"), ".pdf")
	ggsave(filename=out, a, width=25.9, height=25.9, units="in")
	return(0)
}

sapply(chunkcounts, make_pca)

###### correlation in PCs #########

return_PCS = function(x) {
	x = fread(x, header=T)
	pca = data.table(ind = x$Recipient, irlba::prcomp_irlba(x[,-1], n=10)$x)
}

PC_list = lapply(chunkcounts, return_PCS)

linked = PC_list[[1]]
PC_list = PC_list[-1]

PC_corr = data.table(sapply(PC_list, function(x) {
	sapply(1:10, function(y) {
		index = y+1
		abs(cor(x[,..index], linked[,..index]))
	})
}))

PC_corr %>%
	rename("PBWT" = V1, "unlinked" = V2, "ChromoMatcher" = V3) %>%
	mutate(PC = 1:n()) %>%
	pivot_longer(-PC) %>%
	mutate(PC = as.factor(PC)) %>%
	ggplot(aes(x=PC, y=value)) +
	geom_col(position = "dodge", aes(fill=name)) +
	theme(
		axis.text = element_text(size=16),
		axis.title = element_text(size=20)
	) +
	theme_light()
