#'  This function cleans the COPS casts, includings those
#'  obtained in shallow waters
#'
#'  The function prompts the user to click on the begining and the end of the cast.
#'  In shallow waters, the user should click at the depth where the cops
#'  it the bottom. The measurements taken between that depth and the
#'  depth specified by distance.above.bottom will be remove.
#'
#'
#'@param in.file is the file name of the raw COPS cast to process
#'@param out.file is the file name of the cleaned COPS file.
#'  If NA, the input file will be overwrite. The default is "cleaned.cops.csv"
#'@param distance.above.bottom is the distance above the bottom at which the user want to cut the data.
#'  This is for shallow water casts for which the last COPS measurement are affected by the instrument shadow.
#'  If NA, we assume the profile in deep open water and all data are kept.
#'  The default is 0.15, so the last 15 cm of the profile is removed.
#'
#'@author Simon Belanger
#'@export

clean.cops.file <- function (in.file="", out.file="cleaned.cops.csv", distance.above.bottom=0.15) {

  if (!file.exists(in.file)) {
    print(paste("File ", in.file, " does not exist"))
    print("exit function")
    return(0)
  }

  if (is.na(out.file)) out.file = in.file

  index_ext = length(unlist(strsplit(in.file, "[.]")))	# for station names with periods, ex. G604.5
  ext = unlist(strsplit(in.file, "[.]"))[index_ext]



  con <- file(in.file, "r")
  line = readLines(con,1)
  if (line == "Start of Header") {
    nhead = 1
    while(line != "End of Header") {
      line = readLines(con,1)
      nhead = nhead + 1
    }
    close(con)
    print("Header detected")
    print(nhead)

    if (ext == "tsv" || ext =="txt") {
      df = read.table(file = in.file,
                      header = TRUE, as.is = TRUE, sep = "\t",
                      check.names = FALSE,
                      skip = nhead)
    }  else {
      df = read.table(file = in.file,
                      header = TRUE, as.is = TRUE, sep = ",",
                      check.names = FALSE,
                      skip = nhead)
    }

  } else {
    close(con)
    if (ext == "tsv" || ext =="txt") {
      df = read.table(file = in.file,
                      header = TRUE, as.is = TRUE, sep = "\t",
                      check.names = FALSE)
    }  else {
      df = read.table(file = in.file,
                      header = TRUE, as.is = TRUE, sep = ",",
                      check.names = FALSE)
    }
  }


  plot(df[,5], ylab = "Depth in m",
       ylim = rev(range(df[,5])),
       main = in.file)
  print("    <-  Click on starting point and then ESC")
  text(0,min(df[,5]), "    <-  Click on starting point and then ESC", pos=4)
  ix.min=identify(df[,5])

  print("Click  when COPS hit bottom or at the end of the cast and ESC")
  text(length(df[,5]),max(df[,5]), "Click on ending point and then ESC ->    ", pos=2)
  ix.max=identify(df[,5])
  depthMax=(df[ix.max,5])
  if (is.na(distance.above.bottom)) {
    ix.cut=which(df[,5] == depthCut)
  } else {
    depthCut = depthMax - distance.above.bottom
    ix.cut=which.min(abs(df[,5]-depthCut))
  }

  print("Cut at Index:")
  print(c(ix.min,ix.cut))
  if (ext == "tsv" || ext =="txt") {
    write.table(df[ix.min:ix.cut,],file = out.file,
                quote = F, row.names = F, sep="\t")
    return(1)
  }  else {
    write.table(df[ix.min:ix.cut,],file = out.file,
                quote = F, row.names = F, sep=",")
    return(1)
  }
}
