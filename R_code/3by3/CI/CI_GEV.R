library(evd)


set.seed(123)
n = 300
p = 100


make_na = function(Xj, Mj){
  Xj[ Mj==0 ] = NA
  Xj
}


setwd ("C:\\Users\\xiaoming\\Desktop\\makr he\\GRWAIN\\Simulations\\3by3\\CI_GEV")



for(k in 1:50){
  
  
  namek = paste('CI_', k, '.csv', sep = '') 
  namek_true = paste('CI_', k, '_True.csv', sep = '') 
  
  multi_missings1 = lapply(1:p, function(x) {
    
    z = rexp(n, rate = 1)
    
    y = rgev(n, loc = 1, scale = 1, shape = 1)
    
    y = abs(y)
    
    p_ob=  1 - (z+1)/ max(z+1)
    
    
    d = rbinom(n,1,prob=p_ob)  
    
    list(y  = y, pm = p_ob, d =d)
  }   )
  
  
  X_true  = data.frame(do.call( cbind ,  lapply(multi_missings1, function(x)  x$y)) ) 
  M   = data.frame(do.call( cbind ,  lapply(multi_missings1, function(x)  x$d)) )
  true_Pm =  data.frame(do.call( cbind ,  lapply(multi_missings1, function(x)  x$pm)) )
  
  
  
  
  X_msg = sapply(1:p, function(j)  make_na(X_true[,j], M[,j]  ))
  X_true_norm = sapply(X_true, function(x) (x-min(x, na.rm = T))/(max(x, na.rm = T)-min(x ,na.rm = T)))
  X_export = X_true_norm
  X_export[is.na(X_msg)] = NA
  X_export[is.na(X_export) ] =  - 99
  
  
  write.table( X_export ,   namek , sep = ',', col.names = F,row.names = F)
  write.table( X_true_norm ,  namek_true , sep = ',', col.names = F,row.names = F)
}
