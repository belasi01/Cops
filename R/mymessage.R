mymessage <- function(texte, quit = FALSE, head = NULL, tail = NULL) {
	if(!is.null(head))
		cat(paste(rep(head,nchar(texte)),sep="",collapse = ""),"\n")
	cat(texte,"\n")
	if(!is.null(tail))
		cat(paste(rep(tail,nchar(texte)),sep="",collapse = ""),"\n")
	if(quit) stop()
}
