## start of 3.2.1
set.seed(100)
gene1=rnorm(30,mean=4,sd=2)
gene2=rnorm(30,mean=2,sd=2)
org.diiff=mean(gene1)-mean(gene2)
gene.df=data.frame(exp=c(gene1,gene2),group=c(rep("test",30),rep("control",30)))
exp.null<-do(1000)*diff(mosaic::mean(exp~shuffle(group),data=gene.df))
hist(exp.null[,1],xlab="null distribution |no difference in samples",main=expression(paste(H[0],":no difference in means")),xlim=c(-2,2),col="cornflowerblue",border="white")
abline(v=quantile(exp.null[,1],0.95),col="red")
abline(v=org.diiff,col="blue")
text(x=quantile(exp.null[,1],0.95),y=200,"0.05",adj=c(1,0),col="red")
text(x=org.diiff,y=200,"org.diff",adj = c(1,0),col="blue")
p.val=sum(exp.null[,1]>org.diiff)/length(exp.null[,1])
p.val
##p.val = 0.001
#start of 3.2.2
stats::t.test(gene1,gene2)
stats::t.test(gene1,gene2,var.equal=TRUE)
##data:  gene1 and gene2
##t = 3.7653, df = 58, p-value = 0.0003905
##alternative hypothesis: true difference in means is not equal to 0
##95 percent confidence interval:
##  0.8770753 2.8680832
##sample estimates:
##  mean of x mean of y 
##4.057728  2.185149 

##start of 3.2.3
##start of 3.2.4
set.seed(100)
gset=rnorm(3000,mean=200,sd=70)
data=matrix(gset,ncol=6)
group1=1:3
group2=4:6
n1=3
n2=3
dx=rowMeans(data[,group1])-rowMeans(data[,group2])
require(matrixstats)


##start 3.2.3
##quvalue package installation
if(!require("BiocManager",quietly=TRUE))install.packages("BiocManager")
BiocManager::install("qvalue")
library(qvalue)
data("hedenfalk")
qvalues<-qvalue(hedenfalk$p)$q
qresult<-qvalue(hedenfalk$p)$q
bonf.pval<-p.adjust(hedenfalk$p,method="bonferroni")
fdr.adj.pval<-p.adjust(hedenfalk$p,method="fdr")
plot(hedenfalk$p,qresult,pch=19,ylim=c(0,1),xlab="raw p-values",ylab="adjusted p-values")
points(hedenfalk$p,bonf.pval,pch=19,col="red")
points(hedenfalk$p,fdr.adj.pval,pch=19,col="blue")
legend("bottomright",legend=c("q-value","FDR (BH)","Bonferroni"),fill=c("black","blue","red"))
