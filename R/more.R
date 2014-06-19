#########################################################################/**
# @RdocDefault more
#
# @title "Prints one or more objects to the standard output"
#
# \description{
#  Prints one or more objects or the contents of a file to the standard
#  output. If not all of the output fits the console window, a prompt will
#  be displayed. Valid entries in this prompt are: 
#   (1) \code{B} and \code{[ENTER]} for previous and next page, respectively;
#   (2) \code{P} and \code{N} for previous and next line, respectively; 
#   (3) \code{Q} for quit; and (4) \code{H} or \code{?} for help.
#  In addition to these for options entering an integer value will print
#  the contents starting from the line with the entered number.
# }
#
# @synopsis
#
# \arguments{
#   \item{...}{Comma-separated @list of things to be displayed.}
#   \item{file}{File to be displayed.}
#   \item{height}{The height of the console window.}
#   \item{width}{The width of the console window. Default value is 
#      \code{options("width")}. Currently this argument has no effect on
#      the result.}
#   \item{line.numbers}{If @TRUE, each line is preceeded with a line
#      number, otherwise just the contents is printed.}
#   \item{auto.refresh}{If @TRUE, the display is refreshed after prompting
#      the user, otherwise not.}
#
#   Either \code{...} or \code{file} must be specified.
# }
#
# @author
#
# \examples{
#  \dontrun{
#   more(more.default)
#   more(system("ls -l", intern=TRUE))
#   more(file=paste(.path.package("base"), "/CONTENTS", sep=""))
#  }
# }
#
# \seealso{
#   @see "head", @see "tail", @see "base::sink".
# }
#
# @keyword "utilities"
#*/#########################################################################
setMethodS3("more", "default", function(..., file, height=getTerminalInfo(rows=getOption("height"))$rows, width=getTerminalInfo(columns=getOption("width"))$columns, line.numbers=FALSE, auto.refresh=TRUE) {
  help <- function() {
    cat("more() by Henrik Bengtsson, henrikb@braju.com, 2001-2004\n");
    cat("\n");
    cat("  h H ?        - Display this help.\n");
    cat("  q Q :q :Q ZZ - Exit.\n");
    cat("\n");
    cat("  e j          - Forward one line.\n");
    cat("  y k          - Backward one line.\n");
    cat("  f ENTER      - Forward one window.\n");
    cat("  b            - Backward one window.\n");
    cat("  d            - Forward one half-window.\n");
    cat("  u            - Backward one half-window.\n");
    cat("  r            - Repaint screen.\n");
    cat("  g <          - Go to first line.\n");
    cat("  G >          - Go to last line.\n");
    cat("\n");
    cat("  l            - Turn", ifelse(line.numbers, "off", "on"), "line numbering.\n");
    cat("  a            - Turn", ifelse(auto.refresh, "off", "on"), "auto refreshing.\n");
    cat("\n");
  }

  height <- unlist(height);
  width <- unlist(width);

  height <- height-1;

  len <- length(list(...));
  if (len > 0 && !missing(file))
    stop("Only one of the arguments '...' and 'file' can be given.");

  if (len > 0) {
    expr <- substitute(list(...));
  
    # Write the output to a temporary file using sink().
    file <- tempfile("more");
    sink(file);
    for (k in 1:length(expr))
      print(eval(expr[[k]]));
    sink();
    on.exit(unlink(file));
  } else if (missing(file)) {
    stop("Either the argument '...' or 'file' must be given.");
  }

  lines <- readLines(con=file);

  nbrOfLines <- length(lines);
  prefixWidth <- floor(log(nbrOfLines, base=10)+1);

  currentLine <- 0;
  write <- TRUE;
 
  displayNRows <- height;
  ready <- FALSE; 
  while(!ready) {
    if (currentLine < 0) currentLine <- 0;
    if (currentLine > nbrOfLines) currentLine <- nbrOfLines;

    if (write) {
      while(displayNRows > 0) {
        displayNRows <- displayNRows - 1;
  	if (currentLine < nbrOfLines) {
  	  currentLine <- currentLine+1;
  	  if (line.numbers) 
  	    s <- paste(formatC(currentLine, format="d", width=prefixWidth), ": ", sep="")
  	  else
  	    s <- "";
  	  s <- paste(s, lines[currentLine], "\n", sep="");
          displayNRows <- displayNRows - ((nchar(s)-1) %/% width);
          if (displayNRows >= 0) 
            cat(s)
          else
            currentLine <- currentLine-1;
  	}
      } # for (k in 1:displayNRows)
    }

    displayNRows <- 0;
    write <- TRUE;
    ok <- FALSE;
    while(!ok) {
      prompt <- paste("more [", currentLine, "/", nbrOfLines, "=", formatC(100*currentLine/nbrOfLines, format="d"), "%]> ", sep="");
      cat(prompt);
      answer <- readline();
      ok <- TRUE;
      if (answer %in% c("q", "Q", ":q", ":Q", "ZZ")) {
        ready <- TRUE;
      } else if (answer %in% c("h", "H", "?")) {
        help();
        write <- FALSE;
      } else if (answer %in% c("y", "k")) {
        displayNRows <- 1;
        currentLine <- currentLine-2;
        if (auto.refresh) {
          displayNRows <- height;
          currentLine <- currentLine-height+1;
        }
      } else if (answer %in% c("e", "j")) {
        displayNRows <- 1;
        if (auto.refresh) {
          displayNRows <- height;
          currentLine <- currentLine-height+1;
        }
      } else if (answer %in% c("b")) {
        displayNRows <- height;
        currentLine <- currentLine-2*height-1;
      } else if (answer %in% c("f", "")) {
        displayNRows <- height;
        if (auto.refresh && currentLine >= nbrOfLines) {
          displayNRows <- height;
          currentLine <- currentLine-height+1;
        }
      } else if (answer %in% c("u")) {
        displayNRows <- height %/% 2;
        currentLine <- currentLine-(height %/% 2)+1;
        if (auto.refresh) {
          currentLine <- currentLine-height-1;
          displayNRows <- height;
        }
      } else if (answer %in% c("d")) {
        displayNRows <- height %/% 2;
        if (auto.refresh && currentLine >= nbrOfLines) {
          displayNRows <- height;
          currentLine <- currentLine+(height %/% 2)+1;
        }
      } else if (answer %in% c("r")) {
        displayNRows <- height;
        currentLine <- currentLine-height;
      } else if (answer %in% c("g", "<")) {
        displayNRows <- height;
        currentLine <- 0;
      } else if (answer %in% c("G", ">")) {
        displayNRows <- 2*height;
        currentLine <- nbrOfLines-height;
      } else if (regexpr("^[ \t\v]*[0-9]+[ \t\v]*$", answer) != -1) {
        displayNRows <- height;
        currentLine <- as.integer(answer)-1;
      } else if (answer %in% c("l")) {
        line.numbers <- !line.numbers;
        if (auto.refresh) {
          displayNRows <- height;
          currentLine <- currentLine-height;
        }
      } else if (answer %in% c("a")) {
        auto.refresh <- !auto.refresh;
      } else {
        cat("Unknown reply.\n");
        ok <- FALSE;
      }
    } # while(!ok)
  } # if ((!ready && ...))

  invisible(paste(lines, collapse="\n", sep=""));
})


if ( is.null(unlist(options("height"))) ) {
  options(height=25);
}


############################################################################
# HISTORY:
# 2005-02-20
# o Now using setMethodS3().
# 2002-04-04
# * Make use of new getTerminalInfo() to retrieve information about
#   the number of rows and columns the current terminal has.
# 2002-04-02
# * Made the Rdoc example \dontrun{} to make it pass Rcmd check.
# 2001-07-28
# * Added options("height").
# * Added a lot more prompt commands to more(). Need still to Rdoc comment
#   them, though.
# * Now more waits ONCE at 100% iff the user has been prompted before. 
# * Added the argument width=options("width").
# 2001-07-24
# * Created.
############################################################################
