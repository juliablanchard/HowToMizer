#' file that should be sourced at the beginning of every .Rmd
#' Contains libraries and functions
#' 

if(!require('devtools', quietly = TRUE, warn.conflicts = FALSE)) install.packages("devtools")
if(!require('mizer', quietly = TRUE, warn.conflicts = FALSE)) devtools::install_github("sizespectrum/mizer")
if(!require('mizerExperimental', quietly = TRUE, warn.conflicts = FALSE)) devtools::install_github("sizespectrum/mizerExperimental")
if(!require('tidyverse', quietly = TRUE, warn.conflicts = FALSE)) install.packages("tidyverse")
if(!require('plotly', quietly = TRUE, warn.conflicts = FALSE)) install.packages("plotly")
# if(!require('tictoc', quietly = TRUE, warn.conflicts = FALSE)) install.packages("tictoc")
if(!require('shiny', quietly = TRUE, warn.conflicts = FALSE)) install.packages("shiny")
if(!require('shinyWidgets', quietly = TRUE, warn.conflicts = FALSE)) install.packages("shinyWidgets")
# if(!require('parallel', quietly = TRUE, warn.conflicts = FALSE)) install.packages("parallel")
if(!require('optimParallel', quietly = TRUE, warn.conflicts = FALSE)) install.packages("optimParallel")
if(!require('ggrepel', quietly = TRUE, warn.conflicts = FALSE)) install.packages("ggrepel")
if(!require('cowplot', quietly = TRUE, warn.conflicts = FALSE)) install.packages("cowplot")
if(!require('gridExtra', quietly = TRUE, warn.conflicts = FALSE)) install.packages("gridExtra")
if(!require('viridis', quietly = TRUE, warn.conflicts = FALSE)) install.packages("viridis")
if(!require("knitr", quietly = TRUE, warn.conflicts = FALSE)) install.packages("knitr")


g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

# arrow to show asymptotic size

plotSummary <- function (x, y, power = 1, wlim = c(.001,NA), short = F, save_it = FALSE, name_save = NULL, ...) 
{
  xlim = c(wlim[1],10^log10(max(x@params@species_params$w_inf)))
  font_size = 7
  
  # need to display the legend at the bottom and only p1 has the background so using that one
  
  plot_dat <- plotSpectra(x, power = power, wlim = wlim, return_data = TRUE, total = TRUE,)#, ...)
  
  p1 <- ggplot(plot_dat[[1]]) +
    geom_line(aes(x = w, y = value, colour = Species, group = Species)) +
    scale_x_continuous(limits = xlim, trans = "log10", name = "Individual size [g]")+#, breaks = log_breaks()) +
    scale_y_continuous(name = "Biomass density" ,trans = "log10", breaks = log_breaks()) +
    # scale_y_continuous(name = expression(paste("Biomass density (ind.", m^{-3},")", sep="")) ,trans = "log10", breaks = log_breaks()) +
    scale_colour_manual(values = x@params@linecolour) +
    scale_linetype_manual(values = x@params@linetype) +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          text = element_text(size=font_size),
          panel.background = element_blank(),
          panel.grid.minor = element_line(color = "gray"),
          panel.border = element_rect(colour = "gray", fill=NA, size=.5),
          legend.position = "right", legend.key = element_rect(fill = "white")) +
    guides(color = guide_legend(nrow=1))
  
  mylegend<-g_legend(p1) # save the legend
  p1 <- p1 + theme(legend.position = "none") # now remove it from the plot itself
  
  p7 <- plotBiomass(x)
  p7 <- p7 + theme(legend.position = "none",
                   text = element_text(size=font_size),
                   panel.background = element_blank(),
                   panel.grid.minor = element_line(color = "gray"),
                   panel.border = element_rect(colour = "gray", fill=NA, size=.5))
  # theme_bw()
  
  if(short)
  {
    p1 <- p1 +theme(axis.title.x=element_text(),
                    axis.text.x=element_text(),
                    axis.ticks.x=element_line())
    
    leftCol <- plot_grid(p1,p7,
                         ncol = 1, align = "v", axis = "l")
    p10 <- plot_grid(leftCol, mylegend,
                     rel_widths = c(6,1),
                     ncol = 2)
    
  } else
  {
    
    
    dat2 <- plotFeedingLevel2(x, include_critical = T, return_data = T)#,...)
    
    p2 <- ggplot(dat2[[1]]) + 
      geom_line(aes(x = w, y = value, colour = Species, alpha = "actual")) +
      geom_line(data = dat2[[2]], aes(x = w, y = value, colour = Species, alpha = "critical")) +
      scale_discrete_manual("alpha", name = "Feeding Level", values = c(actual = 1, critical = 0.5)) +
      scale_x_continuous(name = "Size [g]", trans = "log10", limits = xlim) + 
      scale_y_continuous(name = "Feeding Level", limits = c(0, 1)) + 
      scale_colour_manual(values = x@params@linecolour) + 
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            panel.background = element_blank(),
            panel.grid.minor = element_line(color = "gray"),
            panel.border = element_rect(colour = "gray", fill=NA, size=.5),
            text = element_text(size=font_size),
            legend.position = "none")
    
    
    dat3 <- plotPredMort(x, return_data = T)#, ...)
    dat3$mortality <- "predation"
    dat4 <- plotFMort(x, return_data = T)#, ...)
    dat4$mortality <- "fisheries"
    plot_dat <- rbind(dat3,dat4)
    
    linesize <- rep(0.8, length(x@params@linetype))
    names(linesize) <- names(x@params@linetype)
    
    p3 <-   ggplot(plot_dat) +
      geom_line(aes(x = w, y = value, colour = Species, linetype = mortality, size = Species)) +
      scale_x_continuous(name = "Size [g]", trans = "log10", limits = xlim) +
      scale_y_continuous(name = "Predation and fisheries mortality [1/year]", limits = c(0, 2)) +
      scale_colour_manual(values = x@params@linecolour) +
      scale_size_manual(values = linesize) +
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            # axis.text.y = element_text(family = "mono"),
            panel.background = element_blank(),
            panel.grid.minor = element_line(color = "gray"),
            panel.border = element_rect(colour = "gray", fill=NA, size=.5),
            text = element_text(size=font_size),
            legend.position = "none")
    
    p4 <-   ggplot(plot_dat) +
      geom_line(aes(x = w, y = value, colour = Species, linetype = mortality, size = Species)) +
      scale_x_continuous(name = "Individual size [g]", trans = "log10", limits = xlim) +
      scale_y_continuous(name = "Predation and fisheries mortality [1/year]", limits = c(.01, max(plot_dat$value)), trans = "log10") +
      scale_colour_manual(values = x@params@linecolour) +
      scale_size_manual(values = linesize) +
      theme(panel.background = element_blank(),
            panel.grid.minor = element_line(color = "gray"),
            panel.border = element_rect(colour = "gray", fill=NA, size=.5),
            text = element_text(size=font_size),
            legend.position = "none")
    
    
    
    
    
    
    
    
    
    
    # yeild and ssb |
    
    
    # plot_dat <- data.frame(catchAvg,ssbAvg)
    # plot_dat$species.1 <- NULL
    # colnames(plot_dat) <- c("Species", "average catch", "average SSB")
    # plot_dat$Species <- factor(as.character(plot_dat$Species),levels = c(as.character(nsParams$species)))
    # plot_dat <- reshape2::melt(plot_dat,"Species")
    # plot_dat$w_inf <- rep(nsParams$w_inf,2)
    
    
    
    
    # bm <- getBiomassFrame2(x,min_w = x@params@species_params$w_mat)
    bm <- getBiomassFrame2(x)
    plot_dat <- filter(bm, Year == max(unique(bm$Year)))
    # plot_dat$w_inf <- x@params@species_params$w_inf
    yieldDat <- getYield(x)
    plot_dat$yield <- yieldDat[dim(yieldDat)[1],]
    plot_dat$Year <- NULL
    colnames(plot_dat) <- c("Species", "average SSB", "average catch")
    plot_dat$Species <- factor(as.character(plot_dat$Species),levels = c(as.character(x@params@species_params$species)))
    plot_dat <- reshape2::melt(plot_dat,"Species")
    plot_dat$w_inf <- rep(x@params@species_params$w_inf,2)
    
    # don't use ssb but total biomass
    p5 <- ggplot(plot_dat)+
      geom_point(aes(x = w_inf, y = value, color = Species, shape = variable), size = 6, alpha = .8) +
      # geom_point(data = plot_dat2, aes(x = w_inf, y = value*1562500, color = Species, shape = "averaged fishing mortality"), size = 6, alpha = .8)+
      geom_text_repel(data = filter(plot_dat,variable == "average SSB"), aes(x = w_inf, y = value, label = Species), hjust = 0, nudge_x = 0.05)+
      geom_line(aes(x = w_inf, y = value, color = Species)) +
      scale_y_continuous(name = "Catch and Biomass", limits = c(NA,NA), trans = "log10") +#,sec.axis = sec_axis(trans = ~./1562500)) +
      scale_x_continuous(name = "Asymptotic size (g)", trans = "log10") +
      scale_colour_manual(values = x@params@linecolour) +
      scale_shape_manual(name = "Data", values = c(16,17)) + # add 4 if fisheries mortality present
      theme(panel.background = element_blank(),
            panel.grid.minor = element_line(color = "gray"),
            panel.border = element_rect(colour = "gray", fill=NA, size=.5),
            axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            text = element_text(size=font_size),
            legend.position = "none",legend.key = element_rect(fill = "white"))+
      guides(color = FALSE)
    
    # mylegend<-g_legend(p5) # save the legend
    # p5 <- p5 + theme(legend.position = "none")
    
    # r0
    
    # RDD/RDI
    #   plot_dat <- as.data.frame(getRDD(x@params)/getRDI(x@params))
    #   plot_dat$species <- factor(rownames(plot_dat),x@params@species_params$species)
    # colnames(plot_dat)[1] <- "ratio"
    # plot_dat$w_inf <- sim_guessed@params@species_params$w_inf
    # 
    # 
    # p6 <- ggplot(plot_dat)+
    #   geom_point(aes(x = w_inf, y = ratio, color = species), size = 6, alpha = .8) +
    #   geom_text_repel(aes(x = w_inf, y = ratio, label = species), hjust = 0, nudge_x = 0.05)+
    #   # geom_line(aes(x = w_inf, y = value, color = Species)) +
    #   scale_y_continuous(name = "density-dependent / density-independent reproduction rate", limits = c(0,1)) +
    #   scale_x_continuous(name = "Asymptotic size (g)", trans = "log10") +
    #   scale_color_manual(name = "Species", values = params_uncalibrated@linecolour) +
    #   theme(panel.background = element_blank(), 
    #                   panel.border = element_rect(colour = "gray", fill=NA, size=.5),
    #         text = element_text(size=font_size),
    #         panel.grid.minor = element_line(color = "gray"),
    #         legend.position = "bottom",legend.key = element_rect(fill = "white"))
    
    #RDI / RDD
    plot_dat <- as.data.frame(getRDI(x@params)/getRDD(x@params))
    plot_dat$species <- factor(rownames(plot_dat),x@params@species_params$species)
    colnames(plot_dat)[1] <- "ratio"
    plot_dat$w_inf <- x@params@species_params$w_inf
    
    
    p6 <- ggplot(plot_dat)+
      geom_point(aes(x = w_inf, y = ratio, color = species), size = 6, alpha = .8) +
      geom_text_repel(aes(x = w_inf, y = ratio, label = species), hjust = 0, nudge_x = 0.05)+
      # geom_line(aes(x = w_inf, y = value, color = Species)) +
      scale_y_continuous(name = "Density-independent / density-dependent reproduction rate", trans = "log10") +
      scale_x_continuous(name = "Asymptotic size (g)", trans = "log10") +
      scale_colour_manual(values = x@params@linecolour) +
      theme(panel.background = element_blank(), 
            panel.border = element_rect(colour = "gray", fill=NA, size=.5),
            text = element_text(size=font_size),
            panel.grid.minor = element_line(color = "gray"),
            legend.position = "bottom",legend.key = element_rect(fill = "white"))
    
    
    # mylegend<-g_legend(p6) # save the legend
    p6 <- p6 + theme(legend.position = "none")
    
    
    # predator / prey mass comparison    
    
    diet_dat <- getDietComp(x)
    SpIdx <- x@params@species_params$species
    tempSimDf <- NULL
    
    for(iSpecies in SpIdx) # for each species
    {
      diet_dat_sp <- diet_dat[iSpecies,,,] 
      diet_dat_sp<- apply(diet_dat_sp,c(1,3),sum) # sum prey identity, keep size class
      speciesPPMR <- NULL
      size_name_vec <- NULL
      size_preferred <- NULL
      for(iSize in dimnames(diet_dat_sp)$pred_size) # for each size class need PPMR value
      {
        if(sum(diet_dat_sp[iSize,])) # if there is at least one diet data
        {
          size_name_vec <- c(size_name_vec,iSize)
          sizeDat <- diet_dat_sp[iSize,] # select the size
          densityDat <- sizeDat / as.numeric(as.character(names(sizeDat)))# adjust biomass > density
          
          # calculating realised PPMR
          PreferredSizeClass <- which(densityDat == max(densityDat)) # which size class is most feed upon
          sizePPMR <- as.numeric(iSize)/as.numeric(names(PreferredSizeClass)) # calculate PPMR
          speciesPPMR <- c(speciesPPMR,sizePPMR)
          
          # what's the favorite mass? (taking the name of the size class)
          size_preferred <- c(size_preferred,as.numeric(names(PreferredSizeClass)))
          
          # what's the mean mass? converting from biomass in bin to mass I guess 
          # how to calculate the average from a set of discrete values? I would need to duplicate the discrete classes by the biomass number (or individual whatever)
          # and then calculate the mean from that, is it legit?
          # for now, simple soluttion, mean mass is most eaten mass (assuming normal distribution)
          # temp <- sizeDat / sim@params@w / sim@params@dw
          # f1n <- MASS::fitdistr(sizeDat,"normal")
          # mean(temp)
          # c <- hist(sizeDat)
          # size_mean <- c(size_mean,mean(sizeDat[sizeDat != 0]))
          
        }
      }
      tempSpeciesDf <- data.frame("species" = rep(iSpecies,length(speciesPPMR)), "w" = as.numeric(size_name_vec), "rPPMR" = speciesPPMR, "prey_mass" = size_preferred)
      tempSimDf <- rbind(tempSimDf,tempSpeciesDf) # create a df of species
    }
    
    # plottin the data
    plot_dat <- filter(tempSimDf)
    
    p8 <- ggplot(plot_dat) +
      geom_line(aes(x = w, y = prey_mass, color = species)) +
      scale_x_continuous(name = "Predator mass (g)", trans = "log10",limits = xlim) +
      scale_y_continuous(name = "Mean prey mass (g)", trans = "log10") +
      scale_colour_manual(values = x@params@linecolour) +
      theme(panel.background = element_blank(),
            panel.grid.minor = element_line(color = "gray"),
            panel.border = element_rect(colour = "gray", fill=NA, size=.5),
            legend.position = "none", legend.key = element_rect(fill = "white"),
            text = element_text(size=font_size)
      ) 
    
    
    # require(grid)
    # require(gridExtra)
    # p10 <- arrangeGrob(p1,p2,p3,p4)
    # grid.draw(p10) # interactive device
    # ggsave("saving.png", p10) # need to specify what to save explicitly
    
    
    # grid.newpage()
    # p10 <- plot_grid(grid.draw(rbind(ggplotGrob(p1), ggplotGrob(p2), ggplotGrob(p3), ggplotGrob(p4))))#, size = "last"))
    #     
    # p10 <- plot_grid(p1,p2,p3,p4, p6, p7, mylegend, byrow = F,
    #                  # rel_heights = c(1,1,1,2),
    #                  rel_widths = c(3,3), nrow = 4,
    #                   align = "v", axis = "l")
    
    plots_arranged <- plot_grid(p1,p2,p3,p4,p5, p6, p7,p8, byrow = F,
                                # rel_heights = c(1,1,1,2),
                                rel_widths = c(3,3), nrow = 4,
                                align = "v")#, axis = "l")
    
    
    p10 <- plot_grid(plots_arranged, mylegend,
                     rel_heights = c(10,1),
                     ncol = 1)
    
    # grid.newpage()
    # grid.draw(p10)
  }
  # p <- grid.arrange(p10,mylegend, nrow=2,heights=c(9.5,0.5))
  
  if(save_it & !is.null(name_save)) ggsave(p10, filename = paste(name_save,".png",sep=""), units = "cm", width = 21, height = 29)
  else if (save_it & is.null(name_save)) ggsave(p10, filename = "tempSummary.png", units = "cm", width = 21, height = 29)
  
  return(p10)
}

# hopefully all of this will go on sizespectrum/mizer, in the meantime

plotPredObsYield <-function(sim, dat, returnData = FALSE){
  ## check obs vs. predicted yield
  plot_dat <-melt(getYield(sim)[100,]/1e6)
  plot_dat$obs <- log10(dat)
  plot_dat$value <- log10(plot_dat$value)
  plot_dat$Species <-row.names(plot_dat)
  
  w_inf <- log10(sim@params@species_params$w_inf)
  names(w_inf) <- sim@params@species_params$species
  
  # window size
  winLim <- c(min(plot_dat$obs,plot_dat$value), max(plot_dat$obs,plot_dat$value))
  
  p <- ggplot(plot_dat) + # plot predicted and observed yields
    geom_point(aes(x = value, y = obs, color = Species, size = Species)) +
    geom_text_repel(aes(x = value, y = obs, label = Species), hjust = 0, nudge_x = 0.05)+
    scale_size_manual(values = w_inf) +
    scale_color_manual(values = sim@params@linecolour) +
    geom_abline(color = "black", slope = 1, intercept = 0, linetype = "dashed", alpha = .5) + 
    scale_x_continuous(name = "log10 Predicted Yield", limits = winLim) + 
    scale_y_continuous(name = "log10 Observed Yield", limits = winLim) +
    theme(legend.position = "none", legend.key = element_rect(fill = "white"),
          panel.background = element_blank(), panel.grid.minor = element_line(color = "gray"))
  
  if(returnData) return(plot_dat) else return(p)
} 


plotDiet2 <- function (sim, species = NULL, xlim = c(1,NA), returnData = F) 
{
  params <- sim@params
  # if (is.integer(species)) {
  #     species <- params@species_params$species[species]
  # }
  
  
  # diet <- getDiet(params)[params@species_params$species == 
  #     species, , ]
  # prey <- dimnames(diet)$prey
  # prey <- factor(prey, levels = rev(prey))
  # plot_dat <- data.frame(Proportion = c(diet), w = params@w, 
  #     Prey = rep(prey, each = length(params@w)))
  # plot_dat <- plot_dat[plot_dat$Proportion > 0, ]
  # 
  # ggplot(plot_dat) + geom_area(aes(x = w, y = Proportion, fill = Prey)) + 
  #     scale_x_log10(limits = xlim) + labs(x = "Size [g]") + 
  #   scale_fill_manual(values = sim@params@linecolour) +
  #   ggtitle(species)
  
  
  diet <- getDiet(params)
  plot_dat <- melt(diet)
  plot_dat <- plot_dat[plot_dat$value > 0, ]
  colnames(plot_dat) <- c("Predator", "size", "Prey", "Proportion")
  
  if(is.null(species)) p <- ggplot(plot_dat) + facet_wrap(.~Predator, scales = "free") else p <- ggplot(filter(plot_dat, Predator == species))
  
  p <- p +
    geom_area(aes(x = size, y = Proportion, fill = Prey))+
    scale_x_continuous(limits = c(1,NA), name = "Size [g]", trans = "log10") + 
    scale_fill_manual(values = sim@params@linecolour)+
    theme(legend.position = "right", legend.key = element_rect(fill = "white"),
          panel.background = element_blank(), panel.grid.minor = element_line(color = "gray"),
          strip.background = element_blank())
  
  if(returnData) return(plot_dat) else return(p)
  
}
# Try facets

plotFmsy <- function(params, effortRes = 20, returnData = F, speciesData = NULL)
{
  # make one gear per species so we can vary the effort per species
  gear <- gear_params(params)
  gear$gear <- params@species_params$species
  gear_params(params) <- gear
  
  catchability <- params@species_params$catchability
  
  xlim <- 1.5 # maximum effort* catchability / xaxis limit
  
  # we want to vary effort value so we get a scale from 0 to 1 of effort * catchability per species
  
  # the "species" arg allows to run the function for only one species, which should be faster but it means "species" must also contain the result of every other species (so it's a two object list)
  
  if(!is.null(speciesData))
  {
    speciesName <- speciesData[[1]] # which species are we changing?
    plot_dat <- speciesData[[2]] # plot_dat of all species
    plot_dat <- filter(plot_dat, species!= speciesName) # remove previous result of the concerned species
    iSpecies <- which(params@species_params$species == speciesName)
    counter = 0 # sim counter
    # determine effort range
    effortMax <- round(xlim/catchability[iSpecies],1)+.1
    SpDat <- NULL
    
    effortSeq <- exp(seq(0,log(effortMax+1), length.out =  effortRes)) -1 
    effortSeq <- effortSeq[effortSeq<effortMax] # creating an exponentially increasing effort sequence
    for(iEffort in effortSeq)
    {
      effort_vec <- rep(1,dim(params@species_params)[1]) # all effort set to one
      effort_vec[iSpecies] <- iEffort # except that one which varies
      
      if(!counter )
      {
        tempSim <- project(params, effort = effort_vec, t_max = 20)
        counter <- 1
      } else  tempSim <- project(params, effort = effort_vec, t_max = 10, initial_n = tempSim@n[dim(tempSim@n)[1],,], 
                                 initial_npp = tempSim@n_pp[dim(tempSim@n_pp)[1],])
      #catch
      yieldDat <- getYield(tempSim)
      SpDat <- rbind(SpDat,c(yieldDat[dim(yieldDat)[1],iSpecies],iEffort))
    }
    SpDat <- as.data.frame(SpDat)
    SpDat$species <- params@species_params$species[iSpecies]
    SpDat$V2 <- SpDat$V2*catchability[iSpecies] # so V2 is effort * catchability
    colnames(SpDat) <- c("yield","effort","species")
    plot_dat <- rbind(plot_dat,SpDat)
    
    
    
  } else {
    
    plot_dat <- NULL
    for(iSpecies in 1:dim(params@species_params)[1])
    {
      counter = 0 # sim counter
      # determine effort range
      effortMax <- round(xlim/catchability[iSpecies],1)+.1
      SpDat <- NULL
      
      effortSeq <- exp(seq(0,log(effortMax+1), length.out =  effortRes)) -1 # every .1 takes 2 min to run, evry .2 takes 1 min but lesser resolution
      effortSeq <- effortSeq[effortSeq<effortMax] # creating an exponentially increasing effort sequence
      for(iEffort in effortSeq)
      {
        effort_vec <- rep(1,dim(params@species_params)[1]) # all effort set to one
        effort_vec[iSpecies] <- iEffort # except that one which varies
        
        if(!counter )
        {
          tempSim <- project(params, effort = effort_vec, t_max = 20)
          counter <- 1
        } else  tempSim <- project(params, effort = effort_vec, t_max = 10, initial_n = tempSim@n[dim(tempSim@n)[1],,], 
                                   initial_npp = tempSim@n_pp[dim(tempSim@n_pp)[1],])
        #catch
        yieldDat <- getYield(tempSim)
        SpDat <- rbind(SpDat,c(yieldDat[dim(yieldDat)[1],iSpecies],iEffort))
      }
      SpDat <- as.data.frame(SpDat)
      SpDat$species <- params@species_params$species[iSpecies]
      SpDat$V2 <- SpDat$V2*catchability[iSpecies] # so V2 is effort * catchability
      colnames(SpDat) <- c("yield","effort","species")
      plot_dat <- rbind(plot_dat,SpDat)
      
    }
  }
  
  plot_dat$species <- factor(plot_dat$species, levels = params@species_params$species)
  # colnames(plot_dat) <- c("yield","effort","species")
  if(!is.null(speciesData)) p <- ggplot(filter(plot_dat, species == speciesName)) else p <- ggplot(plot_dat)
  
  p <- p + geom_line(aes(x = effort , y = yield, color = species))+
    facet_wrap(species~., scales = "free") +
    scale_x_continuous(limits= c(0,xlim),name = "fishing mortality rate")+#, limits = c(1e10,NA))+
    scale_y_continuous(trans = "log10") +
    scale_color_manual(name = "Species", values = params@linecolour) +
    theme(legend.position = "none", legend.key = element_rect(fill = "white"),
          panel.background = element_blank(), panel.grid.minor = element_line(color = "gray"),
          strip.background = element_blank())
  
  if(returnData) return(plot_dat) else return(p)
  
}


plotGrowthCurves2 <- function (object, 
                               species = NULL, 
                               max_age = 20, 
                               percentage = FALSE, 
                               species_panel = FALSE, 
                               highlight = NULL,
                               returnData = F) 
{
  if (is(object, "MizerSim")) {
    params <- object@params
    t <- dim(object@n)[1]
    params@initial_n[] <- object@n[t, , ]
    params@initial_n_pp <- object@n_pp[t, ]
  }
  else if (is(object, "MizerParams")) {
    params <- validParams(object)
  }
  species <- valid_species_arg(params, species)
  ws <- getGrowthCurves(params, species, max_age, percentage)
  plot_dat <- reshape2::melt(ws)
  plot_dat$Species <- factor(plot_dat$Species, params@species_params$species)
  plot_dat$legend <- "model"
  if (all(c("a", "b", "k_vb") %in% names(params@species_params))) {
    if ("t0" %in% names(params@species_params)) {
      t0 <- params@species_params$t0
    }
    else {
      t0 <- 0
    }
    VBdf <- data.frame(species = params@species_params$species, 
                       w_inf = params@species_params$w_inf, a = params@species_params$a, 
                       b = params@species_params$b, k_vb = params@species_params$k_vb, 
                       t0 = t0)
    VBdf$L_inf <- (VBdf$w_inf/VBdf$a)^(1/VBdf$b)
    plot_dat2 <- plot_dat
    plot_dat2$value <- apply(plot_dat, 1, function(x) {
      sel <- VBdf$species == x[1]
      length <- VBdf$L_inf[sel] * (1 - exp(-VBdf$k_vb[sel] * 
                                             (as.numeric(x[2]) - VBdf$t0[sel])))
      VBdf$a[sel] * length^VBdf$b[sel]
    })
    plot_dat2$legend <- "von Bertalanffy"
    plot_dat <- rbind(plot_dat, plot_dat2)
  }
  p <- ggplot(filter(plot_dat, legend == "model")) + geom_line(aes(x = Age, 
                                                                   y = value, colour = Species, linetype = Species, size = Species))
  y_label <- if (percentage) "Percent of maximum size" else "Size [g]"
  linesize <- rep(0.8, length(params@linetype))
  names(linesize) <- names(params@linetype)
  linesize[highlight] <- 1.6
  p <- p + scale_x_continuous(name = "Age [Years]") + scale_y_continuous(name = y_label) + 
    scale_colour_manual(values = params@linecolour) + scale_linetype_manual(values = params@linetype) + 
    scale_size_manual(values = linesize)
  if (!percentage) {
    if (length(species) == 1) {
      idx <- which(params@species_params$species == species)
      w_inf <- params@species_params$w_inf[idx]
      p <- p + geom_hline(yintercept = w_inf, colour = "grey") + 
        annotate("text", 0, w_inf, vjust = -1, label = "Maximum")
      w_mat <- params@species_params$w_mat[idx]
      p <- p + geom_hline(yintercept = w_mat, linetype = "dashed", 
                          colour = "grey") + annotate("text", 0, w_mat, 
                                                      vjust = -1, label = "Maturity")
      if ("von Bertalanffy" %in% plot_dat$legend) 
        p <- p + geom_line(data = filter(plot_dat, legend == 
                                           "von Bertalanffy"), aes(x = Age, y = value))
    }
    else if (species_panel) {
      p <- ggplot(plot_dat) + 
        geom_line(aes(x = Age, y = value, colour = legend)) + 
        scale_x_continuous(name = "Age [years]") +
        scale_y_continuous(name = "Size [g]") +
        facet_wrap(.~Species, scales = "free") +
        geom_hline(aes(yintercept = w_mat),
                   data = tibble(Species = as.factor(object@params@species_params$species[]),
                                 w_mat = object@params@species_params$w_mat[]),
                   linetype = "dashed", colour = "grey") +
        geom_hline(aes(yintercept = w_inf),
                   data = tibble(Species = as.factor(object@params@species_params$species[]),
                                 w_inf = object@params@species_params$w_inf[]),
                   linetype = "solid", colour = "grey") +
        theme(panel.background = element_blank(), panel.grid.minor = element_line(color = "gray"),
              strip.background = element_blank(), legend.key = element_blank())+
        scale_color_discrete(name = "Growth", labels = c("Modelled","von Bertalanffy"))
    }
  }
  if(returnData) return(plot_dat) else return(p)
}

getBiomassFrame2 <- function (sim, species = dimnames(sim@n)$sp[!is.na(sim@params@A)], min_w = NULL,
          start_time = as.numeric(dimnames(sim@n)[[1]][1]), end_time = as.numeric(dimnames(sim@n)[[1]][dim(sim@n)[1]]), 
          ylim = c(NA, NA), total = FALSE, ...) 
{
  if(is.null(min_w)) b <- getBiomass(sim, ...) 
  else {
    biom_per_size <- sim@n
    
    if(length(min_w) == 1) # can probably condense both cases in one
    {
      # find which size class is right after user-inputed w_min
      min_w_cell <- which(as.numeric(dimnames(biom_per_size)$w) >= min_w)[1]
      b <- apply(biom_per_size[,,min_w_cell:dim(biom_per_size)[3]],c(1,2),sum)
    } else if (length(min_w) == dim(biom_per_size)[2]){
      # find which size class is right after user-inputed w_min for each species
      min_w_cell <- NULL
      for(iW in min_w) min_w_cell <- c(min_w_cell,which(as.numeric(dimnames(biom_per_size)$w) >= iW)[1])
# remove size before w_min
      for(iSpecies in 1:dim(biom_per_size)[2]) biom_per_size[,iSpecies,1: (min_w_cell[iSpecies]-1)] <- 0
      
      b <- apply(biom_per_size,c(1,2),sum)
  }
  
  }
  
  
  if (start_time >= end_time) {
    stop("start_time must be less than end_time")
  }
  b <- b[(as.numeric(dimnames(b)[[1]]) >= start_time) & (as.numeric(dimnames(b)[[1]]) <= 
                                                           end_time), , drop = FALSE]
  b_total <- rowSums(b)
  if (total) {
    b <- cbind(b, Total = b_total)
    species <- c("Total", species)
  }
  bm <- mizer::melt(b)
  min_value <- 1e-20
  bm <- bm[bm$value >= min_value & (is.na(ylim[1]) | bm$value >= 
                                      ylim[1]) & (is.na(ylim[2]) | bm$value <= ylim[2]), ]
  names(bm) <- c("Year", "Species", "Biomass")
  species_levels <- c(dimnames(sim@n)$sp, "Background", "Resource", 
                      "Total")
  bm$Species <- factor(bm$Species, levels = species_levels)
  bm <- bm[bm$Species %in% species, ]
  return(bm)
}


plotCalibration <- function(sim, catch_dat = NULL, stage = 1, wlim = c(.1,NA), power = 1, effortRes = 10)
{
  # dat = catchAvg$Catch_1419_tonnes
  font_size = 8
  xlim = c(NA,10^log10(max(sim@params@species_params$w_inf)))
  
  switch (stage,
          "1" = {
            plot_dat <- plotSpectra(sim, power = power, wlim = wlim, return_data = TRUE, total = TRUE)
            
            p1 <- ggplot(plot_dat[[1]]) +
              geom_line(aes(x = w, y = value, colour = Species, group = Species)) +
              scale_x_continuous(limits = xlim, trans = "log10", name = "Individual size [g]")+
              scale_y_continuous(name = "Biomass density" ,trans = "log10", breaks = log_breaks()) +
              scale_colour_manual(values = sim@params@linecolour) +
              scale_linetype_manual(values = sim@params@linetype) +
              theme(axis.title.x=element_blank(),
                    axis.text.x=element_blank(),
                    axis.ticks.x=element_blank(),
                    text = element_text(size=font_size),
                    panel.background = element_blank(),
                    panel.grid.minor = element_line(color = "gray"),
                    panel.border = element_rect(colour = "gray", fill=NA, size=.5),
                    legend.position = "right", legend.key = element_rect(fill = "white"))

            mylegend<-g_legend(p1) # save the legend
            p1 <- p1 + theme(legend.position = "none") # now remove it from the plot itself
            
            # p1 <- plotSpectra(sim, power = power, wlim = wlim)#, ...)
            # p1 <- p1  + scale_x_continuous(limits = xlim, trans = "log10", name = "Individual size [g]") +
            #   theme(
            #     text = element_text(size=font_size),
            #     panel.background = element_blank(),
            #     panel.grid.minor = element_line(color = "gray"),
            #     legend.position = "right", legend.key = element_rect(fill = "white"))
            # # guides(color = guide_legend(nrow=2))
            # 
            # mylegend<-g_legend(p1) # save the legend
            # p1 <- p1 + theme(legend.position = "none") # now remove it from the plot itself
            
            p2 <- plotBiomass(sim)
            p2 <- p2 + theme(legend.position = "none",
                             text = element_text(size=font_size),
                             panel.background = element_blank(),
                             panel.grid.minor = element_line(color = "gray"))
            
            if(is.null(catch_dat))
            {
              leftCol <- plot_grid(p1,p2,
                                   ncol = 1, align = "v")
              p <- plot_grid(leftCol, mylegend,
                             rel_widths = c(6,1),
                             ncol = 2) 
            } else {
            
            p3 <- plotPredObsYield(sim,catch_dat) 
            p3 <- p3 + theme(text = element_text(size=font_size))
            # change tick marks to undersandble ones
            
            
            leftCol <- plot_grid(p1,p2,p3,
                                 ncol = 1, align = "v")
            p <- plot_grid(leftCol, mylegend,
                           rel_widths = c(6,1),
                           ncol = 2)
            }
          },
          "3" = {
            p <- plotFmsy(sim@params,effortRes = effortRes)
          },
          "2" = {
            p <- plotGrowthCurves2(sim, species_panel = T)
            
          },
          {print("Unknow stage selected.")
            p <- NULL}
  )
  return(p)
}

#' Get the diet composition
#' 
#' The diet \eqn{D_{ij}(w, w_p)} is the prey biomass density rate for a predator of
#' species \eqn{i} and weight \eqn{w}, resolved by prey species \eqn{j} and prey
#' size \eqn{w_p}. It is calculated from the predation kernel \eqn{\phi(w, w_p)},
#' the search volume \eqn{\gamma_i(w)}, the feeding level \eqn{f_i(w)}, the
#' species interaction matrix \eqn{\theta_{ij}} and the prey abundance density
#' \eqn{N_j(w)}:
#' \deqn{
#' D_{ij}(w, w_p) = (1-f_i(w)) \gamma_i(w) \theta_{ij} N_j(w_p) 
#' \phi_i(w, w_p) w_p.
#' }
#' The prey index \eqn{j} can run over all species and the resource. The returned 
#' values have units of 1/year. 
#'
#' The total rate \eqn{D_{ij}(w)} at which a predator of species \eqn{i} 
#' and size \eqn{w} consumes biomass from prey species \eqn{j} is
#' obtained by integrating over prey sizes:
#' \deqn{
#' D_{ij}(w) = \int D_{ij}(w, w_p) dw_p.
#' }
#' This aggregated diet can also be obtained directly from the `getDiet()` function.
#' 
#' @param sim An object of class \linkS4class{MizerSim}
#' @return An array (predator species x predator size x 
#'    (prey species + resource) x prey size)


getDietComp<- function(sim)
{
  # initialisation
  object <- sim@params
  feedinglevel=getFeedingLevel(object)
  pred_kernel <- getPredKernel(object)
  n = sim@n[dim(sim@n)[1],,]
  n_pp = sim@n_pp[dim(sim@n_pp)[1],]
  no_sp <- dim(object@species_params)[1]
  no_w <- length(object@w)
  no_w_full <- length(object@w_full)
  
  diet_comp<-array(0, c(no_sp, no_w, no_sp + 1, no_w_full),
                   dimnames=list( predator=as.character(object@species_params$species), pred_size = object@w,
                                  prey = c(as.character(object@species_params$species), "background"),
                                  prey_size = object@w_full))
  
  # Biomass by species
  n_total_in_size_bins<- sweep(n, 2, object@dw , "*")
  b_tot <- sweep(n_total_in_size_bins, 2, object@w , "*")
  
  # Index of predator size classes 
  idx_sp<- object@w_full %in% object@w
  
  
  
  #  pred_kernel * interaction matrix
  for(iW in 1:no_w){
    for(iSpecies in 1:no_sp){
      diet_comp[iSpecies,iW,1:no_sp,idx_sp]<- sweep(sweep( b_tot, c(1), object@interaction[iSpecies, 1:no_sp], "*"), c(2),
                                                    pred_kernel[iSpecies,iW,idx_sp], "*")
    }
  }
  # Search rate *  feeding level * prey biomass
  diet_comp[,,1:no_sp,]<- sweep(sweep(sweep(diet_comp[,,1:no_sp,], c(1,2), object@search_vol,"*"),
                                      c(1,2),1-feedinglevel,"*"),
                                c(1,2),b_tot,"*")  # Prey eaten: total g prey/ year  (given predator biomass density)
  
  # no interaction matrix for background spectrum
  b_background <- (sweep(pred_kernel[,,], c(3), object@dw_full*object@w_full*n_pp, "*")) 
  #Search rate *  feeding level * predator biomass
  b_background<- sweep(b_background, c(1,2), object@search_vol,"*") #Scale up by search volume
  b_background<- sweep(b_background, c(1,2), feedinglevel,"*") # Scale according to feeding level. Prey eaten: g prey / year / g predator
  b_background_tot<-sweep(b_background,c(1,2), b_tot, "*") # Prey eaten: total g prey/ year  (given predator biomass density)
  
  # Store background eaten 
  diet_comp[,,no_sp+1,]<- b_background_tot
  
  return(diet_comp)
} 


getDietMizer <-
function (params, n = initialN(params), n_pp = initialNResource(params), 
          n_other = initialNOther(params), proportion = TRUE) 
{
  params <- validParams(params)
  species <- params@species_params$species
  no_sp <- length(species)
  no_w <- length(params@w)
  no_w_full <- length(params@w_full)
  no_other <- length(params@other_encounter)
  other_names <- names(params@other_encounter)
  # assert_that(identical(dim(n), c(no_sp, no_w)), length(n_pp) == 
  #               no_w_full)
  diet <- array(0, dim = c(no_sp, no_w, no_sp + 1 + no_other), 
                dimnames = list(predator = species, w = dimnames(params@initial_n)$w, 
                                prey = c(as.character(species), "Resource", other_names)))
  idx_sp <- (no_w_full - no_w + 1):no_w_full
  if (length(params@ft_pred_kernel_e) == 1) {
    ae <- matrix(params@pred_kernel[, , idx_sp, drop = FALSE], 
                 ncol = no_w) %*% t(sweep(n, 2, params@w * params@dw, 
                                          "*"))
    diet[, , 1:no_sp] <- ae
    diet[, , no_sp + 1] <- rowSums(sweep(params@pred_kernel, 
                                         3, params@dw_full * params@w_full * n_pp, "*"), dims = 2)
  }
  else {
    prey <- matrix(0, nrow = no_sp + 1, ncol = no_w_full)
    prey[1:no_sp, idx_sp] <- sweep(n, 2, params@w * params@dw, "*")
    prey[no_sp + 1, ] <- n_pp * params@w_full * params@dw_full
    ft <- array(rep(params@ft_pred_kernel_e, times = no_sp + 1) * rep(mvfft(t(prey)), each = no_sp), dim = c(no_sp, no_w_full, no_sp + 1))
    ft <- matrix(aperm(ft, c(2, 1, 3)), nrow = no_w_full)
    ae <- array(Re(mvfft(ft, inverse = TRUE)/no_w_full), 
                dim = c(no_w_full, no_sp, no_sp + 1))
    ae <- ae[idx_sp, , , drop = FALSE]
    ae <- aperm(ae, c(2, 1, 3))
    ae[ae < 1e-18] <- 0
    diet[, , 1:(no_sp + 1)] <- ae
  }
  inter <- cbind(params@interaction, params@species_params$interaction_resource)
  diet[, , 1:(no_sp + 1)] <- sweep(sweep(diet[, , 1:(no_sp + 
                                                       1), drop = FALSE], c(1, 3), inter, "*"), c(1, 2), params@search_vol, 
                                   "*")
  for (i in seq_along(params@other_encounter)) {
    diet[, , no_sp + 1 + i] <- do.call(params@other_encounter[[i]], 
                                       list(params = params, n = n, n_pp = n_pp, n_other = n_other, 
                                            component = names(params@other_encounter)[[i]]))
  }
  f <- getFeedingLevel(params, n, n_pp)
  fish_mask <- n > 0
  diet <- sweep(diet, c(1, 2), (1 - f) * fish_mask, "*")
  if (proportion) {
    total <- rowSums(diet, dims = 2)
    diet <- sweep(diet, c(1, 2), total, "/")
    diet[is.nan(diet)] <- 0
  }
  return(diet)
}


plotFeedingLevel2 <- function (object, species = NULL, time_range, highlight = NULL, 
          all.sizes = FALSE, include_critical = FALSE, return_data = FALSE, 
          ...) 
{
  if (is(object, "MizerSim")) {
    if (missing(time_range)) {
      time_range <- max(as.numeric(dimnames(object@n)$time))
    }
    params <- validParams(object@params)
    feed <- getFeedingLevel(object, time_range = time_range, 
                            drop = FALSE)
  }
  else {
    params <- validParams(object)
    feed <- getFeedingLevel(params, drop = FALSE)
  }
  if (length(dim(feed)) == 3) {
    feed <- apply(feed, c(2, 3), mean)
  }
  sel_sp <- valid_species_arg(params, species, return.logical = TRUE)
  species <- dimnames(params@initial_n)$sp[sel_sp]
  feed <- feed[sel_sp, , drop = FALSE]
  plot_dat <- data.frame(value = c(feed), Species = factor(dimnames(feed)$sp, 
                                                           levels = dimnames(feed)$sp), w = rep(params@w, each = length(species)))
  if (!all.sizes) {
    for (sp in species) {
      plot_dat$value[plot_dat$Species == sp & (plot_dat$w < 
                                                 params@species_params[sp, "w_min"] | plot_dat$w > 
                                                 params@species_params[sp, "w_inf"])] <- NA
    }
    plot_dat <- plot_dat[complete.cases(plot_dat), ]
  }
  if (include_critical) {
    feed_crit <- getCriticalFeedingLevel(params)[sel_sp, 
                                                 , drop = FALSE]
    plot_dat_crit <- data.frame(value = c(feed_crit), Species = factor(dimnames(feed)$sp, 
                                                                       levels = dimnames(feed)$sp), w = rep(params@w, each = length(species)))
    if (!all.sizes) {
      for (sp in species) {
        plot_dat_crit$value[plot_dat_crit$Species == 
                              sp & (plot_dat_crit$w < params@species_params[sp, 
                                                                            "w_min"] | plot_dat_crit$w > params@species_params[sp, 
                                                                                                                               "w_inf"])] <- NA
      }
      plot_dat_crit <- plot_dat_crit[complete.cases(plot_dat_crit), 
                                     ]
    }
    p <- ggplot() + geom_line(aes(x = w, y = value, colour = Species, 
                                  linetype = Species, size = Species, alpha = "actual"), 
                              data = plot_dat) + geom_line(aes(x = w, y = value, 
                                                               colour = Species, linetype = Species, alpha = "critical"), 
                                                           data = plot_dat_crit) + scale_discrete_manual("alpha", 
                                                                                                         name = "Feeding Level", values = c(actual = 1, critical = 0.5))
  }
  else {
    p <- ggplot() + geom_line(aes(x = w, y = value, colour = Species, 
                                  linetype = Species, size = Species), data = plot_dat)
  }
  linesize <- rep(0.8, length(params@linetype))
  names(linesize) <- names(params@linetype)
  linesize[highlight] <- 1.6
  p <- p + scale_x_continuous(name = "Size [g]", trans = "log10") + 
    scale_y_continuous(name = "Feeding Level", limits = c(0, 1)) + 
    scale_colour_manual(values = params@linecolour) + 
    scale_linetype_manual(values = params@linetype) + 
    scale_size_manual(values = linesize) +
    theme(panel.background = element_blank(),
          panel.grid.minor = element_line(color = "gray"),
          panel.border = element_rect(colour = "gray", fill=NA, size=.5),
          legend.key = element_rect(fill = "white"))
  
  if (return_data & include_critical) 
    return(list(plot_dat,plot_dat_crit))
  else if (return_data)
    return(plot_dat)
  else return(p)
}

## the following getError function combines the steps of the optimisastion above - this time with the multispecies model and output the predicted size spectrum
## update below with project_steady and saving the state from each iteration
#RF the function takes a bunch of RMax and compare the theoretical catches versus data
getError <- function(vary,params,dat,env=state,data_type="catch", tol = 0.1,timetorun=10) {
  
  #env$params@species_params$R_max[]<-10^vary[1:12]
  params@species_params$R_max[]<-10^vary[1:12]
  
  params <- setParams(params)
  # run to steady state and update params
  # env$params<- projectToSteady(env$params, distance_func = distanceSSLogN,
  #                 tol = tol, t_max = 200,return_sim = F)
  params<- projectToSteady(params, distance_func = distanceSSLogN,
                           tol = tol, t_max = 200,return_sim = F)
  
  # create sim object 
  
  sim <- project(params, effort = 1, t_max = timetorun) #Change t_max to determine how many years the model runs for
  
  # 
  # sim <- project(env$params, effort = 1, t_max = timetorun) #Change t_max to determine how many years the model runs for
  # 
  # env$params <-sim@params
  # 
  
  ## what kind of data and output do we have?
  if (data_type=="SSB") {
    output <-getSSB(sim)[timetorun,]   #could change to getBiomass if using survey, also check units.
  }
  
  if (data_type=="catch") {
    output <-getYield(sim)[timetorun,]/1e6 
    #' using n . w . dw so g per year per volume (i.e. North Sea since kappa is set up this way). 
    #'The data are in tonnes per year so converting to tonnes.
  }
  
  pred <- log(output)
  dat  <- log(dat)
  # sum of squared errors, here on log-scale of predictions and data (could change this or use other error or likelihood options)
  discrep <- pred - dat
  discrep <- (sum(discrep^2))
  
  # can use a strong penalty on the error to ensure we reach a minimum of 10% of the data (biomass or catch) for each species
  # if(any(pred < 0.1*dat)) discrep <- discrep + 1e10
  
  return(discrep)
}
