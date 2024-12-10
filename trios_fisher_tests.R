library(stats)
library(epitools)
# Fisher tests to compare subgroup of amitriptyline discontinuation phenotypes

# PM vs IM/NM ----
# 1_1 vs 0_0 where offspring_parent and c(PM, NIM)
zero_zero <- c(4, 34)
one_one <- c(6, 10)
tbl <- matrix(c(4, 6, 34, 10), nrow = 2, byrow = TRUE)
or_results <- oddsratio(tbl)
fisher_pval <- or_results$p.value[[4]]
or <- or_results$measure[[2]]
ci_lower <- or_results$measure[[4]]
ci_upper <- or_results$measure[[6]]
fisher_pval
or
ci_lower
ci_upper

# 1_1 vs 1_0
one_zero <- c(8, 24)
one_one <- c(6, 10)
tbl <- matrix(c(8, 6, 24, 10), nrow = 2, byrow = TRUE)
or_results <- oddsratio(tbl)
fisher_pval <- or_results$p.value[[4]]
or <- or_results$measure[[2]]
ci_lower <- or_results$measure[[4]]
ci_upper <- or_results$measure[[6]]
fisher_pval
or
ci_lower
ci_upper

# 1_1 vs 0_1
zero_one <- c(0, 10)
one_one <- c(6, 10)
tbl <- matrix(c(0.5, 6, 10, 10), nrow = 2, byrow = TRUE)
or_results <- oddsratio(tbl)
fisher_pval <- or_results$p.value[[4]]
or <- or_results$measure[[2]]
ci_lower <- or_results$measure[[4]]
ci_upper <- or_results$measure[[6]]
fisher_pval
or
ci_lower
ci_upper

# 0_0, 1_0
zero_zero <- c(4, 34)
one_zero <- c(8, 24)
tbl <- matrix(c(4, 8, 34, 24), nrow = 2, byrow = TRUE)
or_results <- oddsratio(tbl)
fisher_pval <- or_results$p.value[[4]]
or <- or_results$measure[[2]]
ci_lower <- or_results$measure[[4]]
ci_upper <- or_results$measure[[6]]
fisher_pval
or
ci_lower
ci_upper

# 0_0, 0_1
zero_zero <- c(4, 34)
zero_one <- c(0, 10)
tbl <- matrix(c(4, 0, 34, 10), nrow = 2, byrow = TRUE)
or_results <- oddsratio(tbl)
fisher_pval <- or_results$p.value[[4]]
or <- or_results$measure[[2]]
ci_lower <- or_results$measure[[4]]
ci_upper <- or_results$measure[[6]]
fisher_pval
or
ci_lower
ci_upper
# continuity correction
tbl_corrected <- tbl + 0.5
fisher.test(tbl_corrected) 

# PM/IM vs PM ----
# 1_1 vs 0_0 where offspring_parent and c(PIM, NM)
zero_zero <- c(21, 17)
one_one <- c(10, 6)
tbl <- matrix(c(21, 10, 17, 6), nrow = 2, byrow = TRUE)
or_results <- oddsratio(tbl)
fisher_pval <- or_results$p.value[[4]]
or <- or_results$measure[[2]]
ci_lower <- or_results$measure[[4]]
ci_upper <- or_results$measure[[6]]
fisher_pval
or
ci_lower
ci_upper

# 1_1 vs 1_0
one_zero <- c(18, 14)
one_one <- c(10, 6)
tbl <- matrix(c(18, 10, 14, 6), nrow = 2, byrow = TRUE)
or_results <- oddsratio(tbl)
fisher_pval <- or_results$p.value[[4]]
or <- or_results$measure[[2]]
ci_lower <- or_results$measure[[4]]
ci_upper <- or_results$measure[[6]]
fisher_pval
or
ci_lower
ci_upper

# 1_1 vs 0_1
zero_one <- c(4, 6)
one_one <- c(10, 6)
tbl <- matrix(c(4, 10, 6, 6), nrow = 2, byrow = TRUE)
or_results <- oddsratio(tbl)
fisher_pval <- or_results$p.value[[4]]
or <- or_results$measure[[2]]
ci_lower <- or_results$measure[[4]]
ci_upper <- or_results$measure[[6]]
fisher_pval
or
ci_lower
ci_upper

# 0_0, 1_0
zero_zero <- c(21, 17)
one_zero <- c(18, 14)
tbl <- matrix(c(21, 18, 17, 14), nrow = 2, byrow = TRUE)
or_results <- oddsratio(tbl)
fisher_pval <- or_results$p.value[[4]]
or <- or_results$measure[[2]]
ci_lower <- or_results$measure[[4]]
ci_upper <- or_results$measure[[6]]
fisher_pval
or
ci_lower
ci_upper

# 0_0, 0_1
zero_zero <- c(21, 17)
zero_one <- c(4, 6)
tbl <- matrix(c(21, 4, 17, 6), nrow = 2, byrow = TRUE)
or_results <- oddsratio(tbl)
fisher_pval <- or_results$p.value[[4]]
or <- or_results$measure[[2]]
ci_lower <- or_results$measure[[4]]
ci_upper <- or_results$measure[[6]]
fisher_pval
or
ci_lower
ci_upper

