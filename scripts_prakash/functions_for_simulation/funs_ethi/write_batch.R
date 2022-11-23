## paste0(name), 1, 1,0,1,0) in which name is name of expt file. 
## 1st position 1 is for treatment number. 2nd 1 for RP. 3rd 0 is for SQ.## 4th 1 is for OP  5th 1 is for CO.

CSMbatch45 <- function(crop, name, xs, filename, lth) {
  
  outbatch <- rbind(rbind(rbind(rbind(rbind(rbind(rbind(rbind(rbind(rbind(rbind(rbind(rbind(rbind(rbind(rbind(rbind(
        rbind(rbind(rbind(rbind(rbind(rbind(rbind(rbind(rbind(rbind(rbind(rbind(rbind(rbind(rbind(rbind(rbind(rbind(rbind(
          rbind(rbind(rbind(rbind(rbind(rbind(rbind(rbind(rbind(rbind(
      # Batchfile headers            
      paste0("$BATCH(", crop, ")"),            
      "!",            
          cbind(sprintf("%6s %92s %6s %6s %6s %6s", "@FILEX", "TRTNO", "RP", "SQ", "OP","CO"))), 
          cbind(sprintf("%6s %86s %6i %6i %6i %6i",paste0(name), 1, 1,0,1,0))),
          cbind(sprintf("%6s %86s %6i %6i %6i %6i",paste0(name), 2, 1,0,1,0))),
          cbind(sprintf("%6s %86s %6i %6i %6i %6i",paste0(name), 3, 1,0,1,0))),
          cbind(sprintf("%6s %86s %6i %6i %6i %6i",paste0(name), 4, 1,0,1,0))),
          cbind(sprintf("%6s %86s %6i %6i %6i %6i",paste0(name), 5, 1,0,1,0))),
          cbind(sprintf("%6s %86s %6i %6i %6i %6i",paste0(name), 6, 1,0,1,0))),
          cbind(sprintf("%6s %86s %6i %6i %6i %6i",paste0(name), 7, 1,0,1,0))),
          cbind(sprintf("%6s %86s %6i %6i %6i %6i",paste0(name), 8, 1,0,1,0))),
          cbind(sprintf("%6s %86s %6i %6i %6i %6i",paste0(name), 9, 1,0,1,0))),
          cbind(sprintf("%6s %86s %6i %6i %6i %6i",paste0(name), 10, 1,0,1,0))),                         
          cbind(sprintf("%6s %86s %6i %6i %6i %6i",paste0(name), 11, 1,0,1,0))), 
          cbind(sprintf("%6s %86s %6i %6i %6i %6i",paste0(name), 12, 1,0,1,0))), 
          cbind(sprintf("%6s %86s %6i %6i %6i %6i",paste0(name), 13, 1,0,1,0))), 
          cbind(sprintf("%6s %86s %6i %6i %6i %6i",paste0(name), 14, 1,0,1,0))), 
          cbind(sprintf("%6s %86s %6i %6i %6i %6i",paste0(name), 15, 1,0,1,0))), 
          cbind(sprintf("%6s %86s %6i %6i %6i %6i",paste0(name), 16, 1,0,1,0))),
          cbind(sprintf("%6s %86s %6i %6i %6i %6i",paste0(name), 17, 1,0,1,0))),
          cbind(sprintf("%6s %86s %6i %6i %6i %6i",paste0(name), 18, 1,0,1,0))),
          cbind(sprintf("%6s %86s %6i %6i %6i %6i",paste0(name), 19, 1,0,1,0))),
          cbind(sprintf("%6s %86s %6i %6i %6i %6i",paste0(name), 20, 1,0,1,0))),
          cbind(sprintf("%6s %86s %6i %6i %6i %6i",paste0(name), 21, 1,0,1,0))),
          cbind(sprintf("%6s %86s %6i %6i %6i %6i",paste0(name), 22, 1,0,1,0))),
          cbind(sprintf("%6s %86s %6i %6i %6i %6i",paste0(name), 23, 1,0,1,0))),
          cbind(sprintf("%6s %86s %6i %6i %6i %6i",paste0(name), 24, 1,0,1,0))),
          cbind(sprintf("%6s %86s %6i %6i %6i %6i",paste0(name), 25, 1,0,1,0))),
          cbind(sprintf("%6s %86s %6i %6i %6i %6i",paste0(name), 26, 1,0,1,0))),
          cbind(sprintf("%6s %86s %6i %6i %6i %6i",paste0(name), 27, 1,0,1,0))),
          cbind(sprintf("%6s %86s %6i %6i %6i %6i",paste0(name), 28, 1,0,1,0))),
          cbind(sprintf("%6s %86s %6i %6i %6i %6i",paste0(name), 29, 1,0,1,0))),
          cbind(sprintf("%6s %86s %6i %6i %6i %6i",paste0(name), 30, 1,0,1,0))),
          cbind(sprintf("%6s %86s %6i %6i %6i %6i",paste0(name), 31, 1,0,1,0))),
          cbind(sprintf("%6s %86s %6i %6i %6i %6i",paste0(name), 32, 1,0,1,0))),
          cbind(sprintf("%6s %86s %6i %6i %6i %6i",paste0(name), 33, 1,0,1,0))),
          cbind(sprintf("%6s %86s %6i %6i %6i %6i",paste0(name), 34, 1,0,1,0))),
          cbind(sprintf("%6s %86s %6i %6i %6i %6i",paste0(name), 35, 1,0,1,0))),
          cbind(sprintf("%6s %86s %6i %6i %6i %6i",paste0(name), 36, 1,0,1,0))),
          cbind(sprintf("%6s %86s %6i %6i %6i %6i",paste0(name), 37, 1,0,1,0))),
          cbind(sprintf("%6s %86s %6i %6i %6i %6i",paste0(name), 38, 1,0,1,0))),
          cbind(sprintf("%6s %86s %6i %6i %6i %6i",paste0(name), 39, 1,0,1,0))),
          cbind(sprintf("%6s %86s %6i %6i %6i %6i",paste0(name), 40, 1,0,1,0))),
          cbind(sprintf("%6s %86s %6i %6i %6i %6i",paste0(name), 41, 1,0,1,0))),
          cbind(sprintf("%6s %86s %6i %6i %6i %6i",paste0(name), 42, 1,0,1,0))),
         cbind(sprintf("%6s %86s %6i %6i %6i %6i",paste0(name), 43, 1,0,1,0))),
         cbind(sprintf("%6s %86s %6i %6i %6i %6i",paste0(name), 44, 1,0,1,0))),
         cbind(sprintf("%6s %86s %6i %6i %6i %6i",paste0(name), 45, 1,0,1,0)))
	  
   
	outbatch<-outbatch[1:((lth*5)+3),] ## +3 is coz, first 3 rows are headers
	
  # Write the batch file to the selected folder  
  write(outbatch, file = filename, append = F)
  
}
