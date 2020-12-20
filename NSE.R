
 

df <- tibble(a = c(1,2), b = c(2,3))

# ************************************************

aa <- function(df, var1, var2){

df %>% 
  ggplot() + 
  geom_col(aes({{var1}}, {{var2}}), fill = "darkorange")   
  
}

aa(df, a, b)


# ************************************************

var11 <- "a"
var22 <- "b"

bb <- function(df, var1, var2){
  
  df %>% 
    ggplot() + 
    geom_col(aes(!!(var1), !!(var2)), fill = "darkorange")   
  
}

bb(df, ensym(var11), ensym(var22))

# ************************************************

cc <- function(df, var1, var2){
  
  df %>% 
    ggplot() + 
    aes(x = a) + 
    geom_col(aes_string(y = var2), fill = "darkorange") + 
    theme_minimal()
  
}

cc(df, var11, var22)


# ************************************************

df %>% 
  ggplot() + 
  geom_col(aes_string(var11, var22), fill = "darkorange")   


# ************************************************






