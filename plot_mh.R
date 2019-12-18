#!/usr/bin/env Rscript
library(qqman)

file_names = commandArgs(trailingOnly = TRUE)
base_name = basename(tools::file_path_sans_ext(file_names))

gwas_result = read.table(file_names, sep = "", header = T)

color=c("red2","green","orange1","royalblue","yellow3","darkslategrey","purple3",
        "turquoise","hotpink","lightgreen","salmon",
        "skyblue1","goldenrod","slategrey","purple1",
        "maroon","darkgreen","orange4","darkblue",
        "brown","gray","mediumpurple", "turquoise","black")

pdf(file = paste0(base_name, ".pdf"), width = 22.752/2.54, height = 14.568/2.54, family="serif")
manhattan(gwas_result,  cex=0.3, col=color, suggestiveline =-log10(5e-4))
dev.off()

jpeg(paste0(base_name, '.jpg'), width=22.752/2.54, height=14.568/2.54, units="in", res=800, family="serif")
manhattan(gwas_result,  cex=0.3, col=color, suggestiveline =-log10(5e-4))
dev.off()
