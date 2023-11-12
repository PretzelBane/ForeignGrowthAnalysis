# Foreign Growth Analysis
# December 2022
# General Correlation Analysis


### ACTION
# run the R Code F Setup first to build out the DF_Lot_cut
# Adjust Filter and Focus in Setup code as desired prior to running or re-running it


## General predictor multi-variable correlation
# select variables to include in the correlation
LIST_vars_data = c(colnames(DF_Lot_cut[,20:27]), colnames(DF_Lot_cut[,30:50]))
LIST_vars_1h = colnames(DF_Lot_cut[, grep(pattern="^onehot", x=colnames(DF_Lot_cut))])
LIST_vars <- c(LIST_vars_data, LIST_vars_1h)
# build the correlation dataset
DF_corr_gen = DF_Lot_cut[LIST_vars]
# convert any non-numeric data
DF_corr_gen <- DF_corr_gen %>% mutate_if(is.character, as.factor)
DF_corr_gen <- DF_corr_gen %>% mutate_if(is.factor, as.numeric)
TAB_corr_gen <- cor(DF_corr_gen)
# drop insignificant & perfect correlations
TAB_corr_gen[lower.tri(TAB_corr_gen,diag=TRUE)] <- NA
TAB_corr_gen[TAB_corr_gen == 1] <- NA
# clean up
TAB_corr_gen <- as.data.frame(as.table(TAB_corr_gen))
TAB_corr_gen <- na.omit(TAB_corr_gen)
# select significant correlations & sort
TAB_corr_gen <- subset(TAB_corr_gen, abs(Freq) > 0.5)
TAB_corr_gen <- TAB_corr_gen[order(-abs(TAB_corr_gen$Freq)),]
MATRX_corr_gen <- reshape2::acast(TAB_corr_gen, Var1~Var2, value.var="Freq")
PLOT_corr_gen <- corrplot(MATRX_corr_gen, is.corr=FALSE, tl.col="black", na.label=" ")
  
  
## Targeted predictor multi-variable correlation
# select variables to include in the correlation
LIST_vars_data = c("avg_header_temp", "avg_header_dewpt", "avg_header_flow", "avg_header_pres", "total_branch_flow", "percent_c6_flow", "percent_c8_flow", "percent_c9_flow", "percent_c13_flow", "percent_c15_flow", "avg_oa_temp", "avg_oa_dewpt", "avg_oa_precip")
#LIST_vars_1h = colnames(DF_Lot_cut[, grep(pattern="^onehot", x=colnames(DF_Lot_cut))])
#LIST_vars <- c(LIST_vars_data, LIST_vars_1h)
LIST_vars <- c(LIST_vars_data, "response_infocus")
# build the correlation dataset
DF_corr_tar = DF_Lot_cut[LIST_vars]
# convert any non-numeric data
DF_corr_tar <- DF_corr_tar %>% mutate_if(is.character, as.factor)
DF_corr_tar <- DF_corr_tar %>% mutate_if(is.factor, as.numeric)
# create the correlation table
TAB_corr_tar <- cor(DF_corr_tar)
# drop insignificant & perfect correlations
#TAB_corr_tar[lower.tri(TAB_corr_tar,diag=TRUE)] <- NA
TAB_corr_tar[TAB_corr_tar == 1] <- NA
# clean up
TAB_corr_tar <- as.data.frame(as.table(TAB_corr_tar))
TAB_corr_tar <- na.omit(TAB_corr_tar)
# select significant correlations & sort
#TAB_corr_tar <- subset(TAB_corr_tar, abs(Freq) > 0.5)
#TAB_corr_tar <- TAB_corr_tar[order(-abs(TAB_corr_tar$Freq)),]
MATRX_corr_tar <- reshape2::acast(TAB_corr_tar, Var1~Var2, value.var="Freq")
PLOT_corr_tar <- corrplot(MATRX_corr_tar, is.corr=FALSE, tl.col="black", na.label=" ", type="lower", col.lim = c(-1,1), mar = c(2,1,0,0))
mtext(expression(paste(bold("LPA-Related Correlations"))), side=3, line=1, adj=0)
mtext(VAR_subtitleFilter, side=3, line=2, adj=0, cex=0.7)
mtext(VAR_subtitleFocus, side=3, line=3, adj=0, cex=0.7)



## Targeted response multi-variable correlation
# select variables to include in the correlation - exclude categorical variables (separately one-hot encoded) and non-numeric
LIST_vars = colnames(DF_Lot_cut)
LIST_vars <- LIST_vars[-c(1,2,3,4,5,6,7,8,9,10,11,13,14,15,16,17,18,19,28,29,52,53,69)]
LIST_vars <- c("response_infocus",LIST_vars)
# build the correlation dataset
DF_corr_resp = DF_Lot_cut[LIST_vars]
# convert any non-numeric data
DF_corr_resp <- DF_corr_resp %>% mutate_if(is.character, as.factor)
DF_corr_resp <- DF_corr_resp %>% mutate_if(is.factor, as.numeric)
# create the correlation table
TAB_corr_resp <- cor(DF_corr_resp)
TAB_corr_resp <- as.data.frame(as.table(TAB_corr_resp))
MATRX_corr_resp <- reshape2::acast(TAB_corr_resp, Var1~Var2, value.var="Freq")
# drop all but the in-focus response
MATRX_corr_resp <- MATRX_corr_resp[,1,drop=FALSE]
# convert to dataframe, remove least significant variables, sort, return to matrix
# change the subsetting criteria to get more or less correlations included.  Ideally, it would be 0.5 to only see significant ones
DF_corr_resp <- as.data.frame(MATRX_corr_resp)
DF_corr_resp_up <- subset(DF_corr_resp, DF_corr_resp$response_infocus > 0.1)
DF_corr_resp_dn <- subset(DF_corr_resp, DF_corr_resp$response_infocus < -0.1)
DF_corr_resp <- rbind(DF_corr_resp_up, DF_corr_resp_dn)
MATRX_corr_resp <- as.matrix(DF_corr_resp)
# plot
PLOT_corr_resp <- corrplot(MATRX_corr_resp, method='number', is.corr=FALSE, tl.col="black", na.label=" ", diag=FALSE, mar = c(0,0,3,0), tl.cex=.6, cl.pos='r', cl.ratio=3, col.lim = c(-1,1))
par(las=1)
mtext(expression(paste(bold("Targeted Correlation"))), side=3, line=0, adj=0)
mtext(VAR_subtitleFilter, side=3, line=-1, adj=0, cex=0.7)
mtext(VAR_subtitleFocus, side=3, line=-2, adj=0, cex=0.7)
PLOT_corr_resp







