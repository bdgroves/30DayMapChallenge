library(googlesheets4)

gs4_deauth()

# imagine this is the URL or ID of a Sheet readable by anyone (with a link)
ss <- "1_Cx6idGMjOi29W57ZeInISsbhGFKxfbqNnYRcGheCEA"
dat <- read_sheet(ss)