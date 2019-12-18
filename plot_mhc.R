library(tidyverse)
library(RColorBrewer)

display.brewer.all()
c1<-brewer.pal(n=7,name="Set1")

data1 = read.table('./HLA-chs-chm-chn.meta.sorted', header = T, sep = '', stringsAsFactors = F)
data1$batch = 'A'
data = data1

data$tranp = -log10(data$P)
data$tranpos = data$BP/(1000*1000)
data = filter(data, (BP<34*1000*1000) & (BP>29*1000*1000))

shape_level = c()
shape_1 = c(21, 23, 24)
for (i  in data$SNP) {
  if(startsWith(i, 'rs')){
    shape_level = c(shape_level, 'SNP')
  }else if(startsWith(i, 'AA')){
    shape_level = c(shape_level, 'AA')
  }else{
    shape_level =c(shape_level, 'HLA')
  }
}
markers = c('SNP', 'AA', 'HLA')
data$shape = shape_1[factor(shape_level, levels = markers)]
colors = c1[c(3,1,6)]
data$col = colors[factor(shape_level, levels = markers)]

sig_p = -log10(5e-8)

# classic HLA information
classic_pos = c(29911991, 31323293, 31238192, 33037086, 33049368, 32608306, 32631061, 32552064)/(1000*1000)
classic_name = c('A', 'B', 'C', 'DPA1', 'DPB1', 'DQA1', 'DQB1', 'DRB1')

new_pos = classic_pos[c(1,2,3,6,7,8)]
new_name = classic_name[c(1,2,3,6,7,8)]
old_pos = classic_pos[c(4,5)]
old_name = classic_name[c(4,5)]


pdf('mhc-fine-mapping-all.pdf', family = 'serif')
# A
plot(tranp~tranpos, filter(data, batch=='A'), bg=col, pch=shape,ylim=c(y0, y1),
     xlab='Chromosome 6 position (mega base pairs)', ylab=expression('-log' ['10'] * '(p)'),
     cex.axis=1.25, cex.lab=1.25, cex.main=1.25, main='Unconditioned analysis')

from.y1 = c(0,0,0,0,0,0)
to.y1 = c(10,12,11,19.5,20.5,18.5)
from.y2 = c(0,0)
to.y2=c(20,19)
text(x=new_pos, y=to.y1, labels = new_name, adj=c(1,0), cex=1.2, font=3)
segments(x0 = new_pos, y0 = from.y1, x1 =  new_pos, y1 = to.y1, lty = "dashed")
text(x=old_pos, y=to.y2, labels = old_name, adj=c(0), cex=1.2, font=3)
segments(x0 = old_pos, y0 = from.y2, x1 =  old_pos, y1 = to.y2, lty = "dashed")
legend(legend=markers, pt.bg= colors, pch=shape_1, xpd=T, title="Marker") # share bar
dev.off()
