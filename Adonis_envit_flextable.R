#Creating a model selection and variable selection table using flextable and Adonis and envfit
#will need a envfit/adonis df input and the flextable as a dependency
Adonis_model_selection_table <- function(df, column){
  column1 <- substitute(column)
  temp_df <- df[order(df[[column1]]),]
  temp_df1 <- temp_df[1:10,]
  flextable(temp_df1)
}

envfit_vars_selection_table <- function(envfit_df, titles, sigcodes){
  #column2 <- substitute(column)
  temp_df2 <- as.data.frame(envfit_df[["vectors"]][["arrows"]])
  temp_df2$r2 <- as.vector(envfit_df[["vectors"]][["r"]])
  temp_df2[,4] <- as.vector(envfit_df[["vectors"]][["pvals"]])
  temp_df2[,5] <- ifelse(temp_df2$V4>0.1, "", 
                         ifelse(temp_df2$V4<=0.1 & temp_df2$V4>0.05, ".", 
                                 ifelse(temp_df2$V4<=0.05 & temp_df2$V4 >0.01, "*",
                                        ifelse(temp_df2$V4<=0.01 & temp_df2$V4 >0.001, "**",
                                               ifelse(temp_df2$V4<= 0.001, "***", "")))))
  temp_df2[,6] <- rownames(as.data.frame(envfit_df[["vectors"]][["arrows"]]))
  colnames(temp_df2)[colnames(temp_df2) == "V4"] <- "Pr(>r)"
  colnames(temp_df2)[colnames(temp_df2) == "V5"] <- "Sig star"
  colnames(temp_df2)[colnames(temp_df2) == "V6"] <- "Variable"
  temp_df2 <- temp_df2[, c(6,1,2,3,4,5)]
  #temp_df3 <- temp_df2[order(temp_df2[,4]),]
  #temp_df4 <- temp_df3[1:10,]
  flextable(temp_df2) %>% add_header_lines( values = titles) %>% add_footer_lines(values = sigcodes)
}


#To make the envfit tables I needed to decompose the envfit outputs. They are default a vectorfit 
#class which cannot be coerced into a dataframe for flextable. So I make the table from scratch 
#as well as the footnotes and sig star column. Needs to be a named column or flextable will not 
#plot it. Thus I made a working column name but will look to replace with nothing in flextable
#call itself. need to read more about the package first.

#Now I need to order based on the magnitude on the PC2 axis AND r2 values...making new function
#for this

envfit_vars_selection_table_PC2impact <- function(envfit_df, titles, sigcodes){
  #column2 <- substitute(column)
  temp_df2 <- as.data.frame(envfit_df[["vectors"]][["arrows"]])
  temp_df2$r2 <- as.vector(envfit_df[["vectors"]][["r"]])
  temp_df2[,4] <- as.vector(envfit_df[["vectors"]][["pvals"]])
  temp_df2[,5] <- ifelse(temp_df2$V4>0.1, "", 
                         ifelse(temp_df2$V4<=0.1 & temp_df2$V4>0.05, ".", 
                                ifelse(temp_df2$V4<=0.05 & temp_df2$V4 >0.01, "*",
                                       ifelse(temp_df2$V4<=0.01 & temp_df2$V4 >0.001, "**",
                                              ifelse(temp_df2$V4<= 0.001, "***", "")))))
  temp_df2[,6] <- rownames(as.data.frame(envfit_df[["vectors"]][["arrows"]]))
  temp_df2[,7] <- abs(temp_df2[,2])
  colnames(temp_df2)[colnames(temp_df2) == "V4"] <- "Pr(>r)"
  colnames(temp_df2)[colnames(temp_df2) == "V5"] <- "Sig star"
  colnames(temp_df2)[colnames(temp_df2) == "V6"] <- "Variable"
  temp_df2[order(-temp_df2[,7], -temp_df2[,3], temp_df2[,4]),]
  temp_df2 <- temp_df2[, c(6,1,2,3,4,5)]
  #temp_df3 <- temp_df2[order(temp_df2[,4]),]
  #temp_df4 <- temp_df3[1:10,]
  flextable(temp_df2) %>% add_header_lines( values = titles) %>% add_footer_lines(values = sigcodes)
}