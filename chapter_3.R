set.seed(100)
gene1=rnorm(30,mean=4,sd=2)
gene2=rnorm(30,mean=2,sd=2)
org.diiff=mean(gene1)-mean(gene2)
gene.df=data.fram(exp=c(gene1,gene2),group=c(rep("test",30),rep("control",30)))
