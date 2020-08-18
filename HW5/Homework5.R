#pasilla experiment path
fn = system.file("extdata", "pasilla_gene_counts.tsv",
                 package = "pasilla", mustWork = TRUE)

#expression counts for each gene and treatment combination
counts = as.matrix(read.csv(fn, sep = "\t", row.names = "gene_id"))

#meta info file path
annotationFile = system.file("extdata",
                             "pasilla_sample_annotation.csv",
                             package = "pasilla", mustWork = TRUE)

#meta info for the count data above
pasillaSampleAnno = readr::read_csv(annotationFile)

#clean up
library("dplyr")
pasillaSampleAnno = mutate(pasillaSampleAnno,
                           condition = factor(condition, levels = c("untreated", "treated")),
                           type = factor(sub("-.*", "", type), levels = c("single", "paired")))
mt = match(colnames(counts), sub("fb$", "", pasillaSampleAnno$file))
stopifnot(!any(is.na(mt)))

library("DESeq2")
pasilla = DESeqDataSetFromMatrix(countData = counts,colData = pasillaSampleAnno[mt, ], design = ~condition)

#run mutiple -t- tests
pasilla = DESeq(pasilla)
res = results(pasilla)

#p value histogram 
library(ggplot2)
ggplot(as(res, "data.frame"), aes(x = pvalue)) +
  geom_histogram(binwidth = 0.01, fill = "Royalblue", boundary = 0)+geom_hline(yintercept=110, linetype="dashed", color = "red")

