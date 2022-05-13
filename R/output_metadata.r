#' create text file with metadata for r output written to disk
#'
#' This function creates a txt file with metadata for any r output written to disk.
#' The output should help reproducing the analysis as it lists r script file info
#' along with info workstation info.
#' 
#'
#' @param output_file_nm filepath of r outputs (place it as variable on top of r script)
#' @param script.dir directory of code repository (place it as variable on top of r script as script.dir.info)
#' @param script.name name of script used to generate it (place it on top of r script as script.name.info)
#' @param get.script.line.number If true, try to find the line number in the existing script that the output is created at. This will likely not work in parallel environments.
#' @param notes any text string to be added to the text file
#' @return text file with same name as output file with a lot of metadata info
#' @examples
#' \donttest{ 
#' #place these two at top of script
#' script.dir.info="D:/code/reproducibility/example_use/example_repo/"
#' script.name.info="example_script.r"
#' 
#' output_file_nm="test_csv_output3.csv" #output name (this must be unique line in this script)
#' output_meta_data_fx(output_file_nm) #this will generate "test_csv_output3.csv.info.txt" metadata file
#' write.csv(data.frame(index=c(1:10)), file = output_file_nm, row.names = F)
#' }
#' @export
output_meta_data_fx=function(output_file_nm, script.dir=script.dir.info, script.name=script.name.info, get.script.line.number=T, notes=""){
  if (get.script.line.number){
    tmp_hist_file="temp.Rhistory"
    savehistory(file = tmp_hist_file)
    jnk2=readLines(tmp_hist_file)
    unlink(tmp_hist_file)
    last_line=jnk2[length(jnk2)-1]
    last_line=trimws(last_line, which = c("both"))
    
    r_file_lines=readLines(paste0(script.dir.info, script.name.info))
    r_file_lines=trimws(r_file_lines, which = c("both")) #remove left white space
    #r_file_lines[289]
    line_number=which(r_file_lines==last_line)
    
  }else{
    line_number=NA
  }
  
  wd.info=getwd()
  date.info=Sys.time()
  user.info=Sys.info() ["user"][[1]]
  R.version.info=R.version.string
  computer.name=Sys.info() ["nodename"][[1]]
  computer.sysname.info=Sys.info() ["sysname"][[1]]
  computer.release.info=Sys.info() ["release"][[1]]
  computer.info=paste(computer.sysname.info, computer.release.info)
  
  ####################################
  #get git commit version!
  #script.dir="D:/projects/Invasives_modeling/IS_V2_repo/"
  if (git2r::in_repository(path = script.dir)){ #is script part of repository?
    r = git2r::revparse_single(script.dir,"HEAD")
    gt_txt="Code part of git repository"
    gt_repo=r$repo #branch and latest comit
    gt_sha=sha(r) #sha id
    gt_author=r$author #comit author info
    gt_summary=r$summary #comit summary
    gt_message=r$message #comit message
  }else{
    gt_txt="Code not in git repository"
    gt_repo=NA
    gt_sha=NA
    gt_author=NA
    gt_summary=NA
    gt_message=NA
    
  }
  
  fileConn<-file(paste0(output_file_nm, ".info.txt"))
  writeLines(c(
    paste0("output produced on ", as.character(date.info)),
    paste0("at working directory ", as.character(wd.info)),
    paste0("saved to file-path ", output_file_nm),
    paste0("by ", as.character(user.info)),
    paste0("using script ", as.character(script.name)),
    paste0("line number ", as.character(line_number)),
    paste0("located at ", as.character(script.dir)),
    paste0("using ", as.character(R.version.info)),
    paste0("on computer ", as.character(computer.name)),
    paste0("computer info:", as.character(computer.info)),  
    #paste0("based on here package, project location is: ", as.character(location_here)),  
    paste0("Notes: ", as.character(notes)), 
    paste0("#####################################"), 
    paste0("#####################################"), 
    as.character(gt_txt), 
    paste0("branch and latest comit"), #
    capture.output(gt_repo), #
    paste0("sha comit id"),
    capture.output(gt_sha), #
    paste0("comit author info"),
    capture.output(gt_author), #
    paste0("comit summary"),
    capture.output(gt_summary),#
    paste0("comit message"),
    capture.output(gt_message), #
    
    paste0("#####################################"), 
    paste0("#####################################"), 
    paste0("Session info: "), #, "\n", sessionInfo() 
    capture.output(sessionInfo()) #for ones loads #https://stackoverflow.com/questions/21967254/how-to-write-a-reader-friendly-sessioninfo-to-text-file
  ), fileConn)
  close(fileConn)  
}