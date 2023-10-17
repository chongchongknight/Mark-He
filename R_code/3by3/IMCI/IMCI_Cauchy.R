set.seed(123)
n = 300
p = 100
p1 = 50

make_na = function(Xj, Mj){
  Xj[ Mj==0 ] = NA
  Xj
}


setwd ("C:\\Users\\xiaoming\\Desktop\\makr he\\GRWAIN\\Simulations\\3by3\\IMCI_Cauchy")



for(k in 1:50){
  
  
  namek = paste('IMCI_', k, '.csv', sep = '') 
  namek_true = paste('IMCI_', k, '_True.csv', sep = '') 
  
  multi_missings1 = lapply(1:p1, function(x) {
    
    z = rexp(n, rate = 1)
    
    u = rcauchy(n)
    
    u = abs(u)
    
    
    y = 1 + z + u
    
    
    p_ob=  1 - (z+1)/ max(z+1)
    
    
    d = rbinom(n,1,prob=p_ob)  
    
    list(z = z, y  = y, pm = p_ob, d =d)
  }   )
  
  
  X_true  = data.frame( cbind(do.call( cbind ,  lapply(multi_missings1, function(x)  x$y)),
                              do.call( cbind ,  lapply(multi_missings1, function(x)  x$z)) ) )
  M   = data.frame( cbind(do.call( cbind ,  lapply(multi_missings1, function(x)  x$d)),
                          do.call( cbind ,  lapply(multi_missings1, function(x)  x$d)) ))
  true_Pm =  data.frame(cbind(do.call( cbind ,  lapply(multi_missings1, function(x)  x$pm)),
                              do.call( cbind ,  lapply(multi_missings1, function(x)  x$pm)) ))
  
  
  
  
  X_msg = sapply(1:p, function(j)  make_na(X_true[,j], M[,j]  ))
  X_true_norm = sapply(X_true, function(x) (x-min(x, na.rm = T))/(max(x, na.rm = T)-min(x ,na.rm = T)))
  X_export = X_true_norm
  X_export[is.na(X_msg)] = NA
  X_export[is.na(X_export) ] =  - 99
  
  
  write.table( X_export ,   namek , sep = ',', col.names = F,row.names = F)
  write.table( X_true_norm ,  namek_true , sep = ',', col.names = F,row.names = F)
}




