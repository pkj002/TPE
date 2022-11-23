write_soil <- function(x, out_dir){
   
  write_lines(x, file = file.path(out_dir,"SOIL.SOL"))
  
}