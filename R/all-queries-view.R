
bothFull <- read.csv("all-queries-view.csv")

howmany <- 1:25

ViewLViewR <- data.frame(full=bothFull$c2
                        ,semi=bothFull$c8
                        ,seq=bothFull$c14
                        ,top=bothFull$c20
                        ,Sprint=howmany
                        ,stringsAsFactors=F);

cols <- c('magenta','blue','green','purple');
ylim <- c(0,max(ViewLViewR[c('full','semi','seq','top')])*1.8);
par(lwd=2);
barplot(
    main = 'all-FTs: Performance of applying (viewl . viewr) over a Forest of 50-node trees',
    t(ViewLViewR[c('full','semi','seq','top')]),
    beside=T,
    ylim=ylim,
    border=cols,
    col=cols, #'white',
    names.arg=ViewLViewR$Sprint,
    xlab='Number of view operations (x10,000)',
    ylab=expression(paste("Time (", mu, "s)")),
    legend.text=c('Full','Semi','Seq','Top'),
    args.legend=list(text.col='black',col=cols,border=cols,bty='n')
);
box();