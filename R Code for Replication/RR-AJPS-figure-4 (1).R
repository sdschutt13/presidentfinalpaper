## Effect = the differences in predicted probability
## Low and High = the low and high values of the 95% confidence intervals
## These estimates are all obtained from Stata (see RR-AJPS-analyzeYouGov.do)

names <- c("Research gun violence  ","Deferred deportation","Steel seizure  ","Established WPA  ","Japanese internment  ","Desegregation  ","Freed slaves  ","Enhanced interrogation  ","Abortion restrictions  ")
effect <- c(-.54,-.46,-.42,-.34,-.28,-.25,-.20,-.19,-.10)
low <- c(-.672,-.600,-.555,-.490,-.413,-.394,-.337,-.368,-.269)
high <- c(-.414,-.328,-.288,-.186,-.150,-.114,-.060,-.022,.062)

group <- c(9,8,7,6,5,4,3,2,1)
pdf(file = "youGov-revised4.pdf", family = "Times")
par(mar=c(3.5,10,1,1), mgp=c(2,.75,0))
plot(effect,group,xlim=c(-.7,.1),ylim=c(0.5,9.5),cex.lab=1,cex.axis=1,pch=20,cex=2,ylab="",yaxt="n",xlab="Difference in predicted probability of support")
abline(v=0,lty=2)
segments(-.8,group,.2,group,col="LIGHTGREY")
segments(low,group,high,group)
par(mgp=c(2,.1,0))
axis(2,at=c(9,8,7,6,5,4,3,2,1),las=2,cex.axis=1,labels=names,tck=0)
text(effect, 9:1 + .25, round(effect,2))
dev.off()





