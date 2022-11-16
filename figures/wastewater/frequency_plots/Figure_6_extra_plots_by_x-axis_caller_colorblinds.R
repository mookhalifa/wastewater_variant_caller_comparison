##Figure_6

Figure_6A_extra <- ggplot(temp20, 
                          aes(x=caller,#factor(samp_code, level =c("S59","S42","S63","S50","S58","S43","S296","S263","S302","S292","S278","S305","S270")), 
                              y=factor(AA_change, level=c("K38R","5386_syn","A1892T","I189V","13195_syn","I42V","S373P","G446S","G496S","T547K","N679K","N856K","N969K","L981F","D3G","Q19E","27259_syn")),
                              color=as.numeric(Frequency))) + 
  geom_point(size=2) + 
  scale_y_discrete(drop=FALSE) +
  scale_colour_gradientn("Allele Frequency (%)", colours=rainbow(4)) +
  labs(x='Wastewater Samples', y='Omicron BA.1 mutations') + facet_wrap(~samp_code) + #facet_wrap(~caller) +
  theme(legend.position = "bottom", 
        axis.title=element_text(size = 14), 
        axis.text = element_text(size = 8), 
        axis.text.x.bottom = element_text(vjust=0.5,hjust=0.5,angle = 45)) + 
  scale_color_gradient(low=colorBlindBlack8[5], high=colorBlindBlack8[1])
Figure_6A_extra




Figure_6B_extra <- ggplot(temp21, aes(x=caller, #factor(samp_code, level =c("S59","S42","S63","S50","S58","S43","S296","S263","S302","S292","S278","S305","S270")),
                                      y=factor(AA_change,level=c("S135R","T24I","G489S","9424_syn","T327I","L438F","10198_syn","10447_syn","I42V","T19I","V213G","S373P","T376A","D405N","R408S","N679K","N969K","Q19E","27259_syn","S413R")),color=as.numeric(Frequency)))+ 
  geom_point(size=2) + scale_y_discrete(drop=FALSE) +
  #scale_color_viridis("Allele Frequency (%)", direction=-1)+
  scale_colour_gradientn("ALT Allele Frequency (%)", colours=rainbow(4)) +
  #scale_color_gradient("Allele Frequency (%)", low="blue", high="red") +
  #geom_jitter(aes(color=as.numeric(Depth)), size=1) +
  #scale_shape_manual(values=rep(15:22, len=6))  +
  #theme(legend.spacing.x = unit(0.2, 'cm'), text = element_text(size=8)) +
  #theme_bw(base_size=15) + theme(legend.position = "none") + 
  labs(x='Wastewater Samples', y='Omicron BA.2 mutations') + facet_wrap(~samp_code) +#facet_wrap(~caller) 
 theme(legend.position = "bottom", axis.title=element_text(size = 14), axis.text = element_text(size = 8), axis.text.x.bottom = element_text(vjust=0.5, angle = 45))
#+ scale_x_discrete(guide=guide_axis(n.dodge=3))
#ggsave("WWC092_BA2_freq_plot-gradient-Rainbow-aa_change-facet-1.png",units="in", width= 12, height = 6, device='png', dpi=300)
Figure_6B_extra

Figure_6C_extra <- ggplot(temp22, aes(x=caller,#factor(samp_code, level =c("S59","S42","S63","S50","S58","S43","S296","S263","S302","S292","S278","S305","S270")), 
                                      y=factor(AA_change,level=c("T19R","L452R","T478K","P681R","D950N","T77A","S26L","I82T","V82A","T120I","D63G","R203M","D377Y")),color=as.numeric(Frequency)))+ 
  geom_point(size=2) + scale_y_discrete(drop=FALSE) +
  #scale_color_viridis("Allele Frequency (%)", direction=-1)+
  scale_colour_gradientn("ALT Allele Frequency (%)", colours=rainbow(4)) +
  #scale_color_gradient("Allele Frequency (%)", low="blue", high="red") +
  #geom_jitter(aes(color=as.numeric(Depth)), size=1) +
  #scale_shape_manual(values=rep(15:22, len=6))  +
  #theme(legend.spacing.x = unit(0.2, 'cm'), text = element_text(size=8)) +
  #theme_bw(base_size=15) + theme(legend.position = "none") + 
  labs(x='Wastewater Samples', y='\nDelta mutations (amino acid)') +
  facet_wrap(~samp_code) +#facet_wrap(~caller) + 
  theme(legend.position = "bottom", axis.title=element_text(size = 14), axis.text = element_text(size=8), axis.text.x.bottom = element_text(vjust = 0.5, angle = 45))
Figure_6C_extra
#+ scale_x_discrete(guide=guide_axis(n.dodge=3))
#ggsave("WWC092_Delta_freq_plot-gradient-Rainbow-aa_change-facet-1.png",units="in", width= 12, height = 6, device='png', dpi=300)


#jit <- position_jitter(seed=123)
Figure_6D_extra <- ggplot(temp20, aes(x=caller,#factor(ID_manuscript, level =c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19)), 
                                      y=factor(AA_change,level=c("T183I","A890D", "I1412T", "H69_del", "Y144_del", "N501Y", "A570D", "P681H", "T716I", "S982A", "D1118H", "Q27*", "R52I", "Y73C", "D3L")),color=Percent)) + 
  geom_point(size=2) +
  #scale_color_viridis("Allele Frequency (%)", direction=-1)+
  scale_colour_gradientn("ALT Allele Frequency (%)", colours=rainbow(4)) +
  #scale_color_gradient("Allele Frequency (%)", low="blue", high="red") +
  #geom_jitter(aes(color=as.numeric(Depth)), size=1) +
  #scale_shape_manual(values=rep(15:22, len=6))  +
  #theme(legend.spacing.x = unit(0.2, 'cm'), text = element_text(size=8)) +
  #theme_bw(base_size=15) + theme(legend.position = "none") + 
  labs(x='Synthetic Samples', y='Alpha mutations (amino acid)') + facet_wrap(~ID_manuscript) + theme(legend.position = "bottom",axis.title=element_text(size = 14), axis.text = element_text(size=8), axis.text.x.bottom = element_text(vjust=0.5, angle=45))
#+ scale_x_discrete(guide=guide_axis(n.dodge=3))
Figure_6D_extra
#ggsave("WWC105_Mix_Alpha_freq_plot-gradient-Rainbow-facet-1.png",units="in", width= 12, height = 6, device='png', dpi=300)


Figure_6E_extra <- ggplot(temp21, aes(x=caller,#factor(ID_manuscript, level =c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19)), 
                                      y=factor(AA_change,level=c("T85I","K837N","K90R","11287-del","D80A","D215G","22280-del","K417N","E484K","N501Y","A701V","Q57H","S171L","P71L","T205I")),color=Percent)) +
  geom_point(size=2) +
  #scale_color_viridis("Allele Frequency (%)", direction=-1)+
  scale_colour_gradientn("ALT Allele Frequency (%)", colours=rainbow(4))  +
  #scale_color_gradient(low="light blue", high="red") +
  #geom_jitter(aes(color=as.numeric(Depth), shape=caller), size=2, position=jit) +
  #scale_shape_manual(values=rep(0:5, len=6))  +
  #theme(legend.spacing.x = unit(0.2, 'cm'), text = element_text(size=8)) +
  #theme_bw(base_size=15) + theme(legend.position = "none") +
  labs(x='Synthetic Samples', y='Beta mutations (amino acid)') + facet_wrap(~ID_manuscript) + theme(legend.position = "bottom", axis.title=element_text(size = 14), axis.text = element_text(size=8), axis.text.x.bottom = element_text(vjust=0.5, angle=45))
Figure_6E_extra#point_plot 
#+ scale_x_discrete(guide=guide_axis(n.dodge=3)) #
#ggsave("WWC105_Mix_Beta_freq_plot-gradient-Rainbow-facet-1.png",units="in", width= 12, height = 6, device='png', dpi=300)

Figure_6F_extra <- ggplot(temp22, aes(x=caller,#factor(ID_manuscript, level =c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19)), 
                                      y=factor(AA_change,level=c("T19R","L452R","T478K","P681R","D950N","T77A","S26L","I82T","V82A","T120I","D63G","R203M","D377Y")),color=Percent)) +
  geom_point(size=2) +
  scale_colour_gradientn("ALT Allele Frequency (%)", colours=rainbow(4))  +
  #scale_color_gradient(low="light blue", high="red") +
  #scale_color_viridis("Allele Frequency (%)", direction=-1)+
  #geom_jitter(aes(color=as.numeric(Depth), shape=caller), size=2, position=jit) +
  #scale_shape_manual(values=rep(0:5, len=6))  +
  #theme(legend.spacing.x = unit(0.2, 'cm'), text = element_text(size=8)) +
  #theme_bw(base_size=15) + theme(legend.position = "none") +
  labs(x='Synthetic Samples', y='Delta mutations (amino acid)') + facet_wrap(~ID_manuscript) + theme(legend.position = "bottom", axis.title=element_text(size = 14), axis.text = element_text(size=8), axis.text.x.bottom = element_text(vjust=0.5,hjust=0.5, angle=45)) #+ scale_x_discrete(guide=guide_axis(n.dodge=3))
Figure_6F_extra
#ggsave("WWC105_Mix_Delta_freq_plot-gradient-Rainbow-facet-1.png",units="in", width= 12, height = 6, device='png', dpi=300)



Figure_6_extra <- cowplot::plot_grid(Figure_6A_extra+scale_color_gradient(low=colorBlindBlack8[5], high=colorBlindBlack8[1]) + theme(legend.position="none", axis.title.x.bottom = element_blank() , axis.text = element_text(size=6)),
                               Figure_6B_extra+scale_color_gradient(low=colorBlindBlack8[5], high=colorBlindBlack8[1]) + theme(legend.position="none" , axis.title.x.bottom = element_blank() , axis.text = element_text(size=5)),
                               Figure_6C_extra+scale_color_gradient(low=colorBlindBlack8[5], high=colorBlindBlack8[1]) + theme(legend.position="none", axis.title.x.bottom = element_blank()  ),
                               Figure_6D_extra+scale_color_gradient(low=colorBlindBlack8[5], high=colorBlindBlack8[1]) + theme(legend.position="none", axis.title.x.bottom = element_blank()  ),
                               Figure_6E_extra+scale_color_gradient(low=colorBlindBlack8[5], high=colorBlindBlack8[1]) + theme(legend.position="none", axis.title.x.bottom = element_blank()  ),
                               Figure_6F_extra+scale_color_gradient(low=colorBlindBlack8[5], high=colorBlindBlack8[1]) + theme( axis.title.x.bottom = element_blank() ),#+ scale_color_discrete("black"),
                               #figure_1D+scale_color_gradient(low=colorBlindBlack8[5], high=colorBlindBlack8[1]),
                               ncol = 1,
                               labels = c("A.","B.","C.","D.","E.","F.")
)


ggsave2(Figure_6_extra,filename = "Figure_6__extra_for_supplemental_colourblind_#1.png", width = 10, height = 24, units = "in")



Figure_6_extra1 <- cowplot::plot_grid(Figure_6A_extra+scale_color_gradient(low=colorBlindBlack8[5], high=colorBlindBlack8[1]) + theme(legend.position="none", axis.title.x.bottom = element_blank() , axis.text = element_text(size=6)),
                                     Figure_6B_extra+scale_color_gradient(low=colorBlindBlack8[5], high=colorBlindBlack8[1]) + theme(legend.position="none" , axis.title.x.bottom = element_blank() , axis.text = element_text(size=5)),
                                     Figure_6C_extra+scale_color_gradient(low=colorBlindBlack8[5], high=colorBlindBlack8[1]) + guides(color= guide_colourbar(title = "Frequency") ),#+ theme(legend.position="none", axis.title.x.bottom = element_blank()  ),
                                    # Figure_6D_extra+scale_color_gradient(low=colorBlindBlack8[5], high=colorBlindBlack8[1]) + theme(legend.position="none", axis.title.x.bottom = element_blank()  ),
                                     #Figure_6E_extra+scale_color_gradient(low=colorBlindBlack8[5], high=colorBlindBlack8[1]) + theme(legend.position="none", axis.title.x.bottom = element_blank()  ),
                                     #Figure_6F_extra+scale_color_gradient(low=colorBlindBlack8[5], high=colorBlindBlack8[1]) + theme( axis.title.x.bottom = element_blank() ),#+ scale_color_discrete("black"),
                                     #figure_1D+scale_color_gradient(low=colorBlindBlack8[5], high=colorBlindBlack8[1]),
                                     ncol = 1,
                                     labels = c("A.","B.","C.","D.","E.","F.")
)


Figure_6_extra2 <- cowplot::plot_grid(#Figure_6A_extra+scale_color_gradient(low=colorBlindBlack8[5], high=colorBlindBlack8[1]) + theme(legend.position="none", axis.title.x.bottom = element_blank() , axis.text = element_text(size=6)),
                                     #Figure_6B_extra+scale_color_gradient(low=colorBlindBlack8[5], high=colorBlindBlack8[1]) + theme(legend.position="none" , axis.title.x.bottom = element_blank() , axis.text = element_text(size=5)),
                                     #Figure_6C_extra+scale_color_gradient(low=colorBlindBlack8[5], high=colorBlindBlack8[1]) + theme(legend.position="none", axis.title.x.bottom = element_blank()  ),
                                     Figure_6D_extra+scale_color_gradient(low=colorBlindBlack8[5], high=colorBlindBlack8[1]) + theme(legend.position="none", axis.title.x.bottom = element_blank()  ),
                                     Figure_6E_extra+scale_color_gradient(low=colorBlindBlack8[5], high=colorBlindBlack8[1]) + theme(legend.position="none", axis.title.x.bottom = element_blank()  ),
                                     Figure_6F_extra+scale_color_gradient(low=colorBlindBlack8[5], high=colorBlindBlack8[1]) + theme( axis.title.x.bottom = element_blank() ) + guides(color= guide_colourbar(title = "Frequency") ),#+ scale_color_discrete("black"),
                                     #figure_1D+scale_color_gradient(low=colorBlindBlack8[5], high=colorBlindBlack8[1]),
                                     ncol = 1,
                                     labels = c("A.","B.","C.","D.","E.","F.")
)

ggsave2(Figure_6_extra1,filename = "Figure_6__extra_part1_for_supplemental_colourblind_#1.png", width = 10, height = 24, units = "in")
ggsave2(Figure_6_extra2,filename = "Figure_6__extra_part2_for_supplemental_colourblind_#1.png", width = 10, height = 24, units = "in")



#scale_color_viridis("ALT Allele Frequency (%)", direction=-1)+
#scale_color_gradient("Allele Frequency (%)", low="blue", high="red") +
#geom_jitter(aes(color=as.numeric(Depth)), size=1) +
#scale_shape_manual(values=rep(15:22, len=6))  +
#theme(legend.spacing.x = unit(0.2, 'cm'), text = element_text(size=8)) +
#theme_bw(base_size=15) + theme(legend.position = "none") + 
