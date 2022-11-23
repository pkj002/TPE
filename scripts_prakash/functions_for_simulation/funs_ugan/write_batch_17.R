## paste0(name), 1, 1,0,1,0) in which name is name of expt file. 
## 1st position 1 is for treatment number. 2nd 1 for RP. 3rd 0 is for SQ.## 4th 1 is for OP  5th 1 is for CO.

CSMbatch <- function(crop, name, x, filename) {
  
  outbatch <- rbind(rbind(rbind(rbind(rbind(rbind(rbind(rbind(rbind(rbind(rbind(rbind(rbind(rbind(rbind(rbind(rbind(
    rbind(
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
	  cbind(sprintf("%6s %86s %6i %6i %6i %6i",paste0(name), 17, 1,0,1,0))) 
	  
   
  # Write the batch file to the selected folder  
  write(outbatch, file = filename, append = F)
  
}
