# Foreign Growth Analysis
# December 2022
# Dataframe filtering, focus, and constructions

# FG Setup: this code  Run first.  Builds dataframe based on elections, one-hot encodes, checks normality
# FG Filter Response: Looks for correlations among the filterable variables (products, fermenters)
# FG Focus Response: Builds month-bucket data frame from filter selection with response from focus selection; shows trends & paretos
# FG Correlate: Looks for correlation of the focus response against all variables


## load and install packages as necessary
LST_packages <- c("readxl", "writexl", "tidyverse", "ggplot2", "ggmosaic", "dplyr", "ggpubr", "psych", "DescTools", "corrplot", "PerformanceAnalytics", "gridExtra", "qcc", "moments", "tidyquant", "naniar", "reshape2")
installed_packages <- LST_packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(LST_packages[!installed_packages])
}
invisible(lapply(LST_packages, library, character.only = TRUE))



### ACTION
# set the version of the hard data to be used
# must be exact text from the file name.  Number & month only
# this relies on maintaining the same path, most of filename, and worksheet name
VAR_version = "19DEC"



## Import the lot data
DF_Lot <- read_excel(paste("C:/Users/rapelak/OneDrive - Pfizer/Work Files/Reliability/30 Cases/2022 Cases/11 Foreign Growth/Data Analysis/Dataframe Hard Master ", VAR_version, ".xlsx", sep = ""), sheet = "Lot DataFrame (Hard)")
DF_Lot <- data.frame(DF_Lot)


## Remove any duplicate lots by lot ID
# necessary to perform one-hot operations or an error will be produced
# arbitrarily retains the first duplicate, removing the following one(s) - it would be better to clean the spreadsheet intentionally before import
DF_Lot <- DF_Lot %>% distinct(name_run_id, .keep_all = TRUE)


## Fix date formats (just in case they are integer)
DF_Lot$date_inoc <- as.Date(DF_Lot$date_inoc)
DF_Lot$date_harvest <- as.Date(DF_Lot$date_harvest)
DF_Lot$date_detect <- as.Date(DF_Lot$date_detect)


## Duplicate columns to be widened via one-hot
DF_Lot$data_prefilt_exist_1h <- DF_Lot$data_prefilt_exist
DF_Lot$data_prefilt_type_1h <- DF_Lot$data_prefilt_type
DF_Lot$data_nearfilt_qty_1h <- DF_Lot$data_nearfilt_qty
DF_Lot$data_nearfilt_orient_1h <- DF_Lot$data_nearfilt_orient
DF_Lot$data_nearfilt_fitting_1h <- DF_Lot$data_nearfilt_fitting
DF_Lot$calc_mint_lt_dewpt_1h <- DF_Lot$calc_mint_lt_dewpt
DF_Lot$calc_avgt_lt_dewpt_1h <- DF_Lot$calc_avgt_lt_dewpt
DF_Lot$data_bug_genus_1h <- DF_Lot$data_bug_genus
DF_Lot$data_fermenter_num_1h <- DF_Lot$data_fermenter_num
DF_Lot$data_product_code_1h <- DF_Lot$data_product_code


## Remove seed tank findings and findings where inoc sample was positive
DF_Lot <- subset(DF_Lot, DF_Lot$data_fg_loc_type != "Seed")
DF_Lot <- subset(DF_Lot, DF_Lot$data_fg_pre_inoc == "No")


## Build Filter Sets
# filters are used to narrow the dataset, removing records that do no comply
# product sets
VAR_name_prodset00 = "All Products"
LIST_prodset00 = unique(as.list(DF_Lot$data_product_code))
LIST_prodset00 = LIST_prodset00[!(LIST_prodset00 %in% c("na"))]
VAR_name_prodset01 = "Spectinomyacin Only"
LIST_prodset01 = c('SM')
VAR_name_prodset02 = "Spectino-Similar"
LIST_prodset02 = c('SM', "LM", "NB")
VAR_name_prodset03 = "Non-Spectinomyacin"
LIST_prodset03 = LIST_prodset00[!(LIST_prodset00 %in% c("SM"))]
VAR_name_prodset04 = "Non-Spectino-Similar"
LIST_prodset04 = LIST_prodset00[!(LIST_prodset00 %in% c("SM", "LM", "NB"))]
VAR_name_prodset05 = "Spectino & Dicheto"
LIST_prodset05 = LIST_prodset00[(LIST_prodset00 %in% c("SM", "DA"))]
# fermenter sets
VAR_name_fermset00 = "All Fermenters"
LIST_fermset00 = unique(as.list(DF_Lot$data_fermenter_num))
LIST_fermset00 = LIST_fermset00[!(LIST_fermset00 %in% c("na"))]
VAR_name_fermset01 = "Fermenters 33-36"
LIST_fermset01 = c('33', '34', '35', '36')
VAR_name_fermset02 = "Fermenters 33 Only"
LIST_fermset02 = c('33')

## Build View Sets
# views are used to indicate that is to be included in result, non-compliant records remain
# bug sets
VAR_name_bugset00 = "All Bugs"
LIST_bugset00 = unique(as.list(DF_Lot$data_bug_genus))
LIST_bugset00 = LIST_bugset00[!(LIST_bugset00 %in% c("na"))]
VAR_name_bugset01 = "Gram Neg Bugs"
LIST_bugset01 = c("Achromobacter", "Acinetobacter", "Aeromonas", "Brevundimonas", "Chryseobacteri", "Chryseobacterium", "Citrobacter", "Elizabethkingia", "Enterobacter", "Enterobacteriaceae", "Escherichia", "Klebsiella", "Lelliottia", "Micrococcus", "Pseudomonas", "Raoultella", "Serratia", "Stenotrophomonas")
VAR_name_bugset02 = "LPA-Pravelent Bugs"
LIST_bugset02 = c("Stenotrophomonas", "Pseudomonas", "Enterobacter", "Enterobacteriaceae", "Acinetobacter")
VAR_name_bugset03 = "LPA-Pravelent Bugs less Acinetobacter"
LIST_bugset03 = c("Stenotrophomonas", "Pseudomonas", "Enterobacter", "Enterobacteriaceae")
VAR_name_bugset04 = "Acinobacter Only"
LIST_bugset04 = c("Acinetobacter")
VAR_name_bugset05 = "Non-LPA-Pravelent Gram Neg Bugs"
LIST_bugset05 = LIST_bugset01[!(LIST_bugset01 %in% c("Stenotrophomonas", "Pseudomonas", "Enterobacter", "Enterobacteriaceae", "Acinetobacter"))]
VAR_name_bugset06 = "Non-LPA-Pravelent Gram Neg Bugs plus Acinetobacter"
LIST_bugset06 = LIST_bugset01[!(LIST_bugset01 %in% c("Stenotrophomonas", "Pseudomonas", "Enterobacter", "Enterobacteriaceae"))]
VAR_name_bugset07 = "Gram Pos Bugs"
LIST_bugset07 = c("Bacillus", "Brevibacillus", "Cytobacillus", "Enterococcus", "Lysinibacillus", "Microbacterium", "Paenibacillus", "Priestia")
VAR_name_bugset08 = "Bacillus Family Bugs"
LIST_bugset08 = c("Bacillus", "Brevibacillus", "Cytobacillus", "Lysinibacillus", "Paenibacillus")
VAR_name_bugset09 = "Bacillus Only"
LIST_bugset09 = c("Bacillus")
VAR_name_bugset10 = "Non-Bacillus Family Gram Pos Bugs"
LIST_bugset10 = LIST_bugset07[!(LIST_bugset07 %in% c("Bacillus", "Brevibacillus", "Cytobacillus", "Lysinibacillus", "Paenibacillus"))]
VAR_name_bugset10 = "Non-Bacillus Gram Pos Bugs"
LIST_bugset11 = LIST_bugset07[!(LIST_bugset07 %in% c("Bacillus"))]
# detection days sets
VAR_name_ddayset00 = "All Detection Days"
LIST_ddayset00 = unique(as.list(DF_Lot$days_detect))
LIST_ddayset00 = LIST_ddayset00[!(LIST_ddayset00 %in% c("na"))]
VAR_name_ddayset01 = "Early Detection Days (1-3)"
LIST_ddayset01 = c("1", "2", "3")
VAR_name_ddayset02 = "Mid Detection Days (4-6)"
LIST_ddayset02 = c("4", "5", "6")
VAR_name_ddayset03 = "Mid to Late Detection Days ( > 4)"
LIST_ddayset03 = LIST_ddayset00[!(LIST_ddayset00 %in% c("1", "2", "3"))]
VAR_name_ddayset04 = "Late Detection Days ( > 6)"
LIST_ddayset04 = LIST_ddayset00[!(LIST_ddayset00 %in% c("1", "2", "3", "4", "5", "6"))]
VAR_name_ddayset05 = "Exactly 3 Detection Days (3)"
LIST_ddayset05 = LIST_ddayset00[(LIST_ddayset00 %in% c("3"))]



### ACTION

## CHOOSE FILTER
#VAR_datefrom = "2017-01-01"
VAR_datefrom = "2021-01-01"
VAR_dateto = "2022-10-31"
VAR_products = "01"
VAR_fermenters = "00"

## CHOOSE FOCUS
VAR_bugs = "02"
VAR_detectionDays = "00"



## Build choice-related lists and variables
VAR_datefrom <- as.Date(VAR_datefrom, "%Y-%m-%d")
VAR_dateto <- as.Date(VAR_dateto, "%Y-%m-%d")
eval(parse(text=paste("LIST_prodsetXX",paste("LIST_prodset",VAR_products,sep=""),sep="=")))
eval(parse(text=paste("VAR_name_prodsetXX",paste("VAR_name_prodset",VAR_products,sep=""),sep="=")))
eval(parse(text=paste("LIST_fermsetXX",paste("LIST_fermset",VAR_fermenters,sep=""),sep="=")))
eval(parse(text=paste("VAR_name_fermsetXX",paste("VAR_name_fermset",VAR_fermenters,sep=""),sep="=")))
eval(parse(text=paste("LIST_bugsetXX",paste("LIST_bugset",VAR_bugs,sep=""),sep="=")))
eval(parse(text=paste("VAR_name_bugsetXX",paste("VAR_name_bugset",VAR_bugs,sep=""),sep="=")))
eval(parse(text=paste("LIST_ddaysetXX",paste("LIST_ddayset",VAR_detectionDays,sep=""),sep="=")))
eval(parse(text=paste("VAR_name_ddaysetXX",paste("VAR_name_ddayset",VAR_detectionDays,sep=""),sep="=")))
VAR_subtitleFilter <- paste("FILTER: Dates= ", VAR_datefrom, " to ", VAR_dateto, ";  Products= ",VAR_name_prodsetXX, ";  Fermenters= ", VAR_name_fermsetXX, sep="")
VAR_subtitleFocus <- paste("FOCUS:  Bugs= ",VAR_name_bugsetXX, "  ; Detection Days= ", VAR_name_ddaysetXX, sep="")


## Create a cut of the original dataset per filter
DF_Lot_cut <- DF_Lot
DF_Lot_cut <- subset(DF_Lot_cut, DF_Lot_cut$date_inoc >= VAR_datefrom)
DF_Lot_cut <- subset(DF_Lot_cut, DF_Lot_cut$date_harvest <= VAR_dateto)
DF_Lot_cut <- subset(DF_Lot_cut, DF_Lot_cut$data_product_code %in% LIST_prodsetXX)
DF_Lot_cut <- subset(DF_Lot_cut, DF_Lot_cut$data_fermenter_num %in% LIST_fermsetXX)


## build in-focus and out-of-focus response variables
DF_Lot_cut$response_infocus <- with(DF_Lot_cut, ifelse(result_fg_present == "Yes" & data_bug_genus %in% LIST_bugsetXX & days_detect %in% LIST_ddaysetXX, 1, 0)) 
DF_Lot_cut$response_outfocus <- with(DF_Lot_cut, ifelse(result_fg_present == "Yes" & response_infocus == 0, 1, 0))


## One-hot encode the categorical variables
# remember: can't have any duplicate runs in the data
DF_Lot_cut <- DF_Lot_cut %>% mutate(value = 1) %>% pivot_wider(names_from = data_prefilt_exist_1h, names_prefix = "onehot_prefilt_exist_", values_from = value, values_fill = 0)
DF_Lot_cut <- DF_Lot_cut %>% mutate(value = 1) %>% pivot_wider(names_from = data_prefilt_type_1h, names_prefix = "onehot_prefilt_type_", values_from = value, values_fill = 0)
DF_Lot_cut <- DF_Lot_cut %>% mutate(value = 1) %>% pivot_wider(names_from = data_nearfilt_qty_1h, names_prefix = "onehot_nearfilt_qty_", values_from = value, values_fill = 0)
DF_Lot_cut <- DF_Lot_cut %>% mutate(value = 1) %>% pivot_wider(names_from = data_nearfilt_orient_1h, names_prefix = "onehot_nearfilt_orient_", values_from = value, values_fill = 0)
DF_Lot_cut <- DF_Lot_cut %>% mutate(value = 1) %>% pivot_wider(names_from = data_nearfilt_fitting_1h, names_prefix = "onehot_nearfilt_fitting_", values_from = value, values_fill = 0)
DF_Lot_cut <- DF_Lot_cut %>% mutate(value = 1) %>% pivot_wider(names_from = calc_mint_lt_dewpt_1h, names_prefix = "onehot_mint_lt_dewpt_", values_from = value, values_fill = 0)
DF_Lot_cut <- DF_Lot_cut %>% mutate(value = 1) %>% pivot_wider(names_from = calc_avgt_lt_dewpt_1h, names_prefix = "onehot_avgt_lt_dewpt_", values_from = value, values_fill = 0)
DF_Lot_cut <- DF_Lot_cut %>% mutate(value = 1) %>% pivot_wider(names_from = data_bug_genus_1h, names_prefix = "onehot_data_bug_genus_", values_from = value, values_fill = 0)
DF_Lot_cut <- DF_Lot_cut %>% mutate(value = 1) %>% pivot_wider(names_from = data_fermenter_num_1h, names_prefix = "onehot_data_fermenter_num_", values_from = value, values_fill = 0)
DF_Lot_cut <- DF_Lot_cut %>% mutate(value = 1) %>% pivot_wider(names_from = data_product_code_1h, names_prefix = "onehot_data_product_code_", values_from = value, values_fill = 0)


## Check continuous variables for normality
# shapiro test p-value > 0.05 indicates normality
MODEL_normtest_avg_header_temp <- shapiro.test(DF_Lot_cut$avg_header_temp)
MODEL_normtest_avg_header_dewpt <- shapiro.test(DF_Lot_cut$avg_header_dewpt)
MODEL_normtest_avg_header_flow <- shapiro.test(DF_Lot_cut$avg_header_flow)
MODEL_normtest_avg_header_pres <- shapiro.test(DF_Lot_cut$avg_header_pres)
MODEL_normtest_avg_branch_flow <- shapiro.test(DF_Lot_cut$avg_branch_flow)
# skewness between -0.5 and 0.5 is fairly normal
MODEL_normskew_avg_header_temp <- skewness(DF_Lot_cut$avg_header_temp)
MODEL_normskew_avg_header_dewpt <- skewness(DF_Lot_cut$avg_header_dewpt)
MODEL_normskew_avg_header_flow <- skewness(DF_Lot_cut$avg_header_flow)
MODEL_normskew_avg_header_pres <- skewness(DF_Lot_cut$avg_header_pres)
MODEL_normskew_avg_branch_flow <- skewness(DF_Lot_cut$avg_branch_flow)
# kurtosis between -6 and 6 is fairly normal
MODEL_normkurt_avg_header_temp <- kurtosis(DF_Lot_cut$avg_header_temp)
MODEL_normkurt_avg_header_dewpt <- kurtosis(DF_Lot_cut$avg_header_dewpt)
MODEL_normkurt_avg_header_flow <- kurtosis(DF_Lot_cut$avg_header_flow)
MODEL_normkurt_avg_header_pres <- kurtosis(DF_Lot_cut$avg_header_pres)
MODEL_normkurt_avg_branch_flow <- kurtosis(DF_Lot_cut$avg_branch_flow)
# normality plot for visual check
PLOT_norm_avg_header_temp <- ggqqplot(DF_Lot_cut$avg_header_temp) + labs(title = paste ("skew: ", format (round(MODEL_normskew_avg_header_temp, 2), nsmall = 2), "    kurt: ", format (round(MODEL_normkurt_avg_header_temp, 2)), "   shapiro p: ", signif(MODEL_normtest_avg_header_temp$p.value, 2) )) + theme(plot.title = element_text(color = "blue"))
PLOT_norm_avg_header_dewpt <- ggqqplot(DF_Lot_cut$avg_header_dewpt) + labs(title = paste ("skew: ", format (round(MODEL_normskew_avg_header_temp, 2), nsmall = 2), "    kurt: ", format (round(MODEL_normkurt_avg_header_temp, 2)), "   shapiro p: ", signif(MODEL_normtest_avg_header_temp$p.value, 2) )) + theme(plot.title = element_text(color = "blue"))
PLOT_norm_avg_header_flow <- ggqqplot(DF_Lot_cut$avg_header_flow) + labs(title = paste ("skew: ", format (round(MODEL_normskew_avg_header_temp, 2), nsmall = 2), "    kurt: ", format (round(MODEL_normkurt_avg_header_temp, 2)), "   shapiro p: ", signif(MODEL_normtest_avg_header_temp$p.value, 2) )) + theme(plot.title = element_text(color = "blue"))
PLOT_norm_avg_header_pres <- ggqqplot(DF_Lot_cut$avg_header_pres) + labs(title = paste ("skew: ", format (round(MODEL_normskew_avg_header_temp, 2), nsmall = 2), "    kurt: ", format (round(MODEL_normkurt_avg_header_temp, 2)), "   shapiro p: ", signif(MODEL_normtest_avg_header_temp$p.value, 2) )) + theme(plot.title = element_text(color = "blue"))
PLOT_norm_avg_branch_flow <- ggqqplot(DF_Lot_cut$avg_branch_flow) + labs(title = paste ("skew: ", format (round(MODEL_normskew_avg_header_temp, 2), nsmall = 2), "    kurt: ", format (round(MODEL_normkurt_avg_header_temp, 2)), "   shapiro p: ", signif(MODEL_normtest_avg_header_temp$p.value, 2) )) + theme(plot.title = element_text(color = "blue"))

