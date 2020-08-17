file_list <- list.files(path=
                          #"~/PG-400/data2/Split A/32"
                          "~/PG-400/data2/Freqs/delta/32"
                        )

for (file in file_list) {
  tensor <- readRDS(paste("~/PG-400/data2/Freqs/delta/32/", file, sep =""))
  
for ( matrix in 1:dim(tensor)[1]) {
  whiteNoise <- NULL
  for (channel in 1:32) {
    
    x=NULL
    x[1]=0
    
    for (i in 2:1025) {
      x[i] = x[i-1] + rnorm(1,0,25)
    }
    
    x <- x[2:1025]
    
    whiteNoise <- rbind(whiteNoise, x)
    
    if (channel == 32 && i == 1025) {rm(x, channel, i)
      whiteNoise <- t(whiteNoise)
    }
  }
  
  tensor[matrix,,] <- whiteNoise + tensor[matrix,,]
}
  saveRDS(tensor, file = paste("~/PG-400/data2/white/delta/", file, sep = ""))
  
}