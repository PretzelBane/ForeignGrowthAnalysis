# Foreign Growth Analysis
# December 2022
# Filter Response Analysis


## Predictor variable correlation
# select variables to include in the correlation - exclude response variables and non-numeric (which are on-hot encoded, anyway)
LIST_vars = colnames(DF_Lot_cut)
LIST_vars_oa = colnames(DF_Lot_cut[, grep(pattern="*_oa_", x=colnames(DF_Lot_cut))])
LIST_vars_header = colnames(DF_Lot_cut[, grep(pattern="*_header_", x=colnames(DF_Lot_cut))])
LIST_vars_branch = colnames(DF_Lot_cut[, grep(pattern="*_branch_", x=colnames(DF_Lot_cut))])
LIST_vars_comp = colnames(DF_Lot_cut[, grep(pattern="*c*_flow", x=colnames(DF_Lot_cut))])
LIST_vars_pflt = colnames(DF_Lot_cut[, grep(pattern="*onehot_prefilt", x=colnames(DF_Lot_cut))])
LIST_vars_nflt = colnames(DF_Lot_cut[, grep(pattern="*onehot_nearfilt", x=colnames(DF_Lot_cut))])
LIST_vars_ferm = colnames(DF_Lot_cut[, grep(pattern="*onehot_data_fermenter_", x=colnames(DF_Lot_cut))])
LIST_vars_prod = colnames(DF_Lot_cut[, grep(pattern="*onehot_data_product_", x=colnames(DF_Lot_cut))])
LIST_vars <- c(LIST_vars_oa, LIST_vars_header, LIST_vars_branch, LIST_vars_comp, LIST_vars_pflt, LIST_vars_nflt, LIST_vars_ferm, LIST_vars_prod)
# build correlation
DF_corr_predictors = DF_Lot_cut[LIST_vars]
# convert any non-numeric data
DF_corr_predictors <- DF_corr_predictors %>% mutate_if(is.character, as.factor)
DF_corr_predictors <- DF_corr_predictors %>% mutate_if(is.factor, as.numeric)
# create the correlation table and shape it into a matrix
TAB_corr_predictors <- cor(DF_corr_predictors)
# drop insignificant & perfect correlations
TAB_corr_predictors[lower.tri(TAB_corr_predictors,diag=TRUE)] <- NA
TAB_corr_predictors[TAB_corr_predictors == 1] <- NA
# clean up
TAB_corr_predictors <- as.data.frame(as.table(TAB_corr_predictors))
TAB_corr_predictors <- na.omit(TAB_corr_predictors)
# select significant correlations & sort
TAB_corr_predictors <- subset(TAB_corr_predictors, abs(Freq) > 0.5)
MATRX_cor_predictors <- reshape2::acast(TAB_corr_predictors, Var1~Var2, value.var="Freq")
# plot
PLOT_corr_predictors <- corrplot(MATRX_cor_predictors, is.corr=FALSE, tl.col="black", na.label=" ", type='lower', diag=FALSE, mar = c(0,0,0,0), tl.cex=.5, col.lim = c(-3,3))
mtext(expression(paste(bold("Predictor Variable Correlation"))), side=3, line=-1, adj=0)
PLOT_corr_predictors




## Response variable correlation
# select variables to include in the correlation - exclude response variables and non-numeric (which are on-hot encoded, anyway)
LIST_vars = colnames(DF_Lot_cut)
LIST_vars_bug = colnames(DF_Lot_cut[, grep(pattern="*onehot_data_bug_", x=colnames(DF_Lot_cut))])
LIST_vars <- c("days_detect", LIST_vars_bug)

# build correlation
DF_corr_responses = DF_Lot_cut[LIST_vars]
DF_corr_responses <- subset(DF_corr_responses, DF_corr_responses$days_detect != "na")

# convert any non-numeric data
DF_corr_responses <- DF_corr_responses %>% mutate_if(is.character, as.factor)
DF_corr_responses <- DF_corr_responses %>% mutate_if(is.factor, as.numeric)

# create the correlation table and shape it into a matrix
TAB_corr_responses <- cor(DF_corr_responses)
# drop insignificant & perfect correlations
TAB_corr_responses[lower.tri(TAB_corr_responses,diag=TRUE)] <- NA
TAB_corr_responses[TAB_corr_responses == 1] <- NA
# clean up
TAB_corr_responses <- as.data.frame(as.table(TAB_corr_responses))
TAB_corr_responses <- na.omit(TAB_corr_responses)
# select significant correlations & sort
TAB_corr_responses <- subset(TAB_corr_responses, abs(Freq) > 0.1)
MATRX_corr_responses <- reshape2::acast(TAB_corr_responses, Var1~Var2, value.var="Freq")
# plot
PLOT_corr_responses <- corrplot(MATRX_corr_responses, type='lower', mar = c(0,0,0,0))
mtext(expression(paste(bold("Response Variable Correlation"))), side=3, line=-1, adj=0)
PLOT_corr_responses










## Pareto charts of the filtering variables
# product Code
TAB_counts <- DF_Lot_cut %>% count(data_product_code, sort = TRUE, name = "prodcount")
VEC_prodcount <- TAB_counts$prodcount
names(VEC_prodcount) <- TAB_counts$data_product_code
PLOT_pareto_prods <- pareto.chart(VEC_prodcount, main="Pareto Product Code", ylab="count", ylab2="cum%", cumperc=seq(0,100, by=10))
# fermenter number
TAB_counts <- DF_Lot_cut %>% count(data_fermenter_num, sort = TRUE, name = "fermcount")
VEC_fermcount <- TAB_counts$fermcount
names(VEC_fermcount) <- TAB_counts$data_fermenter_num
PLOT_pareto_ferms <- pareto.chart(VEC_fermcount, main="Pareto Fermenter Number", ylab="count", ylab2="cum%", cumperc=seq(0,100, by=10))
