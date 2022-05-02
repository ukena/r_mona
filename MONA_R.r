library(cluster)
library(glue) 

calc_association <- function(dff) {
  
  for (vars in colnames(dff)) {  # alle variablen (x1, ..., x6)
    
    counter = 0
    
    for (other_vars in colnames(dff[, -which(names(dff) == vars)])) {  # alle variablen außer derzeitige
      
      print(glue("{vars} with {other_vars}"))
      
      beide_0 <- nrow(dff[dff[vars] == 0 & dff[other_vars] == 0, c(vars, other_vars)])
      beide_1 <- nrow(dff[dff[vars] == 1 & dff[other_vars] == 1, c(vars, other_vars)])

      
      erste_var_0_zweite_var_1 <- nrow(dff[dff[vars] == 0 & dff[other_vars] == 1, c(vars, other_vars)])
      erste_var_1_zweite_var_0 <- nrow(dff[dff[vars] == 1 & dff[other_vars] == 0, c(vars, other_vars)])
      
      erstes_produkt <- (beide_1 * beide_0)
      zweites_produkt <- (erste_var_0_zweite_var_1 * erste_var_1_zweite_var_0)
      
      result <- abs(erstes_produkt - zweites_produkt)
      print(glue("{result}"))
      
      counter = counter + result
      
    }
    print(glue("{vars} gesamt = {counter}"))
  }
}

### 1. ITERATION ###

df_1 <- data.frame(
    x1 = c(1, 1, 1, 1, 0, 0, 0, 0),
    x2 = c(1 ,1 ,1 ,1 ,0 ,0 ,0, 0),
    x3 = c(0, 0, 1, 1, 0, 0, 1, 1),
    x4 = c(1, 0, 1, 1, 1, 0, 1, 1),
    x5 = c(1, 0, 1, 1, 0, 0, 0, 1),
    x6 = c(0, 1, 0, 0, 1, 0, 1, 0)
  )

row.names(df_1) <- c("AAA", "BBB", "CCC", "DDD", "EEE", "FFF", "GGG", "HHH")
ma_1 <- mona(df_1)
plot(ma_1)

calc_association(df_1)

### 2. ITERATION ###

df_21 <- data.frame(
  x1 = c(1, 1, 1, 0),
  x2 = c(1, 1, 1, 0),
  x3 = c(0, 1, 1, 1),
  x4 = c(1, 1, 1, 1),
  x5 = c(1, 1, 1, 1),
  x6 = c(0, 0, 0, 0)
)

row.names(df_21) <- c("AAA", "CCC", "DDD", "HHH")

ma_21 <- mona(df_21)

plot(ma_21)

calc_association(df_21)

# ----- #

df_22 <- data.frame(
  x1 = c(1, 0, 0, 0),
  x2 = c(1, 0, 0, 0),
  x3 = c(0, 0, 0, 1),
  x4 = c(0, 1, 0, 1),
  x5 = c(0, 0, 0, 0),
  x6 = c(1, 1, 0, 1)
)
row.names(df_22) <- c("BBB", "EEE", "FFF", "GGG")

ma_22 <- mona(df_22)

plot(ma_22)

calc_association(df_22)

