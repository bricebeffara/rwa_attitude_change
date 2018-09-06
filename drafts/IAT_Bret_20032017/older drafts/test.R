dft <- data.frame(rep(NA,4))
dft$EC <- c(rep("EC1",4), rep("EC2",4))
dft$CE <- rep(c(rep("CE1",2), rep("CE2",2)),2)
dft$FA <- rep(c("B","M"),4)

dft$ECr <- ifelse (dft$EC == "EC1",-1,1)
dft$CEr <- ifelse (dft$CE == "CE1",-1,1)
dft$FAr <- ifelse (dft$FA == "B",-1,1)

dft$ord <- dft$ECr * dft$CEr * dft$FAr

#CE1 -> famille B = Neg  famille M = Pos
#CE2 -> famille B = Pos famille M = Neg
#EC1 -> CE1 puis CE2
#EC2 -> CE2 puis CE1

