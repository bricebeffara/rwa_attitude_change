files[1]$ip

files <- as.list(files)
read.csv(files[1], header=F, stringsAsFactors=F)$V8[2]
read.csv(files[1], header=F, stringsAsFactors=F)$V10[2]

mapply(
  write.table,
  x=files, file=paste(names(files), "csv", sep="."),
  MoreArgs=list(row.names=FALSE, sep=",")
)


files_read <- lapply(files, function(i){read.csv(i, header=FALSE, stringsAsFactors=F)})
files_read <- lapply(files_read, pseudon_ip)




fix_data_frame(files_read[[1]])

lapply(files_read, function(i){write.table(i, file=paste("i", "csv", sep="."))})

list.search(files_read, "ip")

files_read[1:length(files_read)][[1]][2,V8]

[2,V8]
