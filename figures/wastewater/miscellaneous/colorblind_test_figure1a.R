pdf("Colorblind_color_pallate_testing_Figure_1a.pdf", width = 10, height=8)
colorBlindBlack8  <- c("#000000", "#E69F00", "#56B4E9", "#009E73", 
                       "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
pie(rep(1, 8), col = colorBlindBlack8)
figure_1A <- ggplot(snp_quasi_6VCs_median_Mix4, aes(precision, recall, color=ALPHA)) + 
  #  scale_color_gradient(low=colorBlindBlack8[5], high=colorBlindBlack8[1]) +##low="blue", high="red") +
  #  scale_color_gradient(low=colorBlindBlack8[4], high=colorBlindBlack8[1]) +##low="blue", high="red") +
  #  scale_color_gradient(low=colorBlindBlack8[4], high=colorBlindBlack8[5]) +##low="blue", high="red") +
  scale_color_gradient(low=colorBlindBlack8[6], high=colorBlindBlack8[5]) +##low="blue", high="red") +
  geom_point(size=2.5) + 
  theme(legend.spacing.x = unit(0.2, 'cm'), text = element_text(size=8)) + 
  theme_bw(base_size=15) +
  geom_jitter(aes(color=DELTA), size=2.5, position=jit) + 
  scale_shape_manual(values=rep(15:22, len=6)) + geom_smooth(method=lm, size=0.5) + facet_wrap(~caller) + 
  coord_cartesian(xlim=c(0,1.0), ylim=c(0,1.0)) + theme(legend.position = "bottom")
figure_1A
figure_1A <- ggplot(snp_quasi_6VCs_median_Mix4, aes(precision, recall, color=ALPHA)) + 
  #  scale_color_gradient(low=colorBlindBlack8[5], high=colorBlindBlack8[1]) +##low="blue", high="red") +
  #  scale_color_gradient(low=colorBlindBlack8[4], high=colorBlindBlack8[1]) +##low="blue", high="red") +
    scale_color_gradient(low=colorBlindBlack8[4], high=colorBlindBlack8[5]) +##low="blue", high="red") +
  #scale_color_gradient(low=colorBlindBlack8[6], high=colorBlindBlack8[5]) +##low="blue", high="red") +
  geom_point(size=2.5) + 
  theme(legend.spacing.x = unit(0.2, 'cm'), text = element_text(size=8)) + 
  theme_bw(base_size=15) +
  geom_jitter(aes(color=DELTA), size=2.5, position=jit) + 
  scale_shape_manual(values=rep(15:22, len=6)) + geom_smooth(method=lm, size=0.5) + facet_wrap(~caller) + 
  coord_cartesian(xlim=c(0,1.0), ylim=c(0,1.0)) + theme(legend.position = "bottom")
figure_1A
figure_1A <- ggplot(snp_quasi_6VCs_median_Mix4, aes(precision, recall, color=ALPHA)) + 
  #  scale_color_gradient(low=colorBlindBlack8[5], high=colorBlindBlack8[1]) +##low="blue", high="red") +
    scale_color_gradient(low=colorBlindBlack8[4], high=colorBlindBlack8[1]) +##low="blue", high="red") +
  #  scale_color_gradient(low=colorBlindBlack8[4], high=colorBlindBlack8[5]) +##low="blue", high="red") +
  #scale_color_gradient(low=colorBlindBlack8[6], high=colorBlindBlack8[5]) +##low="blue", high="red") +
  geom_point(size=2.5) + 
  theme(legend.spacing.x = unit(0.2, 'cm'), text = element_text(size=8)) + 
  theme_bw(base_size=15) +
  geom_jitter(aes(color=DELTA), size=2.5, position=jit) + 
  scale_shape_manual(values=rep(15:22, len=6)) + geom_smooth(method=lm, size=0.5) + facet_wrap(~caller) + 
  coord_cartesian(xlim=c(0,1.0), ylim=c(0,1.0)) + theme(legend.position = "bottom")
figure_1A
figure_1A <- ggplot(snp_quasi_6VCs_median_Mix4, aes(precision, recall, color=ALPHA)) + 
    scale_color_gradient(low=colorBlindBlack8[5], high=colorBlindBlack8[1]) +##low="blue", high="red") +
  #  scale_color_gradient(low=colorBlindBlack8[4], high=colorBlindBlack8[1]) +##low="blue", high="red") +
  #  scale_color_gradient(low=colorBlindBlack8[4], high=colorBlindBlack8[5]) +##low="blue", high="red") +
  #scale_color_gradient(low=colorBlindBlack8[6], high=colorBlindBlack8[5]) +##low="blue", high="red") +
  geom_point(size=2.5) + 
  theme(legend.spacing.x = unit(0.2, 'cm'), text = element_text(size=8)) + 
  theme_bw(base_size=15) +
  geom_jitter(aes(color=DELTA), size=2.5, position=jit) + 
  scale_shape_manual(values=rep(15:22, len=6)) + geom_smooth(method=lm, size=0.5) + facet_wrap(~caller) + 
  coord_cartesian(xlim=c(0,1.0), ylim=c(0,1.0)) + theme(legend.position = "bottom")
figure_1A
figure_1A <- ggplot(snp_quasi_6VCs_median_Mix4, aes(precision, recall, color=ALPHA)) + 
  scale_color_gradient(low=colorBlindBlack8[3], high=colorBlindBlack8[7]) +##low="blue", high="red") +
  #  scale_color_gradient(low=colorBlindBlack8[4], high=colorBlindBlack8[1]) +##low="blue", high="red") +
  #  scale_color_gradient(low=colorBlindBlack8[4], high=colorBlindBlack8[5]) +##low="blue", high="red") +
  #scale_color_gradient(low=colorBlindBlack8[6], high=colorBlindBlack8[5]) +##low="blue", high="red") +
  geom_point(size=2.5) + 
  theme(legend.spacing.x = unit(0.2, 'cm'), text = element_text(size=8)) + 
  theme_bw(base_size=15) +
  geom_jitter(aes(color=DELTA), size=2.5, position=jit) + 
  scale_shape_manual(values=rep(15:22, len=6)) + geom_smooth(method=lm, size=0.5) + facet_wrap(~caller) + 
  coord_cartesian(xlim=c(0,1.0), ylim=c(0,1.0)) + theme(legend.position = "bottom")
figure_1A
figure_1A <- ggplot(snp_quasi_6VCs_median_Mix4, aes(precision, recall, color=ALPHA)) + 
  scale_color_gradient(low=colorBlindBlack8[4], high=colorBlindBlack8[8]) +##low="blue", high="red") +
  #  scale_color_gradient(low=colorBlindBlack8[4], high=colorBlindBlack8[1]) +##low="blue", high="red") +
  #  scale_color_gradient(low=colorBlindBlack8[4], high=colorBlindBlack8[5]) +##low="blue", high="red") +
  #scale_color_gradient(low=colorBlindBlack8[6], high=colorBlindBlack8[5]) +##low="blue", high="red") +
  geom_point(size=2.5) + 
  theme(legend.spacing.x = unit(0.2, 'cm'), text = element_text(size=8)) + 
  theme_bw(base_size=15) +
  geom_jitter(aes(color=DELTA), size=2.5, position=jit) + 
  scale_shape_manual(values=rep(15:22, len=6)) + geom_smooth(method=lm, size=0.5) + facet_wrap(~caller) + 
  coord_cartesian(xlim=c(0,1.0), ylim=c(0,1.0)) + theme(legend.position = "bottom")
figure_1A
figure_1A <- ggplot(snp_quasi_6VCs_median_Mix4, aes(precision, recall, color=ALPHA)) + 
  scale_color_gradient(low=colorBlindBlack8[2], high=colorBlindBlack8[6]) +##low="blue", high="red") +
  #  scale_color_gradient(low=colorBlindBlack8[4], high=colorBlindBlack8[1]) +##low="blue", high="red") +
  #  scale_color_gradient(low=colorBlindBlack8[4], high=colorBlindBlack8[5]) +##low="blue", high="red") +
  #scale_color_gradient(low=colorBlindBlack8[6], high=colorBlindBlack8[5]) +##low="blue", high="red") +
  geom_point(size=2.5) + 
  theme(legend.spacing.x = unit(0.2, 'cm'), text = element_text(size=8)) + 
  theme_bw(base_size=15) +
  geom_jitter(aes(color=DELTA), size=2.5, position=jit) + 
  scale_shape_manual(values=rep(15:22, len=6)) + geom_smooth(method=lm, size=0.5) + facet_wrap(~caller) + 
  coord_cartesian(xlim=c(0,1.0), ylim=c(0,1.0)) + theme(legend.position = "bottom")
figure_1A
figure_1A <- ggplot(snp_quasi_6VCs_median_Mix4, aes(precision, recall, color=ALPHA)) + 
  scale_color_gradient(low=colorBlindBlack8[6], high=colorBlindBlack8[2]) +##low="blue", high="red") +
  #  scale_color_gradient(low=colorBlindBlack8[4], high=colorBlindBlack8[1]) +##low="blue", high="red") +
  #  scale_color_gradient(low=colorBlindBlack8[4], high=colorBlindBlack8[5]) +##low="blue", high="red") +
  #scale_color_gradient(low=colorBlindBlack8[6], high=colorBlindBlack8[5]) +##low="blue", high="red") +
  geom_point(size=2.5) + 
  theme(legend.spacing.x = unit(0.2, 'cm'), text = element_text(size=8)) + 
  theme_bw(base_size=15) +
  geom_jitter(aes(color=DELTA), size=2.5, position=jit) + 
  scale_shape_manual(values=rep(15:22, len=6)) + geom_smooth(method=lm, size=0.5) + facet_wrap(~caller) + 
  coord_cartesian(xlim=c(0,1.0), ylim=c(0,1.0)) + theme(legend.position = "bottom")
figure_1A
dev.off()
