#' file that should be sourced at the beginning of every .Rmd
#' Contains libraries and functions
#' 

if(!require('devtools', quietly = TRUE, warn.conflicts = FALSE)) install.packages("devtools")
if(!require('mizer', quietly = TRUE, warn.conflicts = FALSE)) devtools::install_github("sizespectrum/mizer")
if(!require('mizerExperimental', quietly = TRUE, warn.conflicts = FALSE)) devtools::install_github("sizespectrum/mizerExperimental")
if(!require('tidyverse', quietly = TRUE, warn.conflicts = FALSE)) install.packages("tidyverse")
if(!require('plotly', quietly = TRUE, warn.conflicts = FALSE)) install.packages("plotly")
if(!require('tictoc', quietly = TRUE, warn.conflicts = FALSE)) install.packages("tictoc")
if(!require('shiny', quietly = TRUE, warn.conflicts = FALSE)) install.packages("shiny")
if(!require('shinyWidgets', quietly = TRUE, warn.conflicts = FALSE)) install.packages("shinyWidgets")
if(!require('parallel', quietly = TRUE, warn.conflicts = FALSE)) install.packages("parallel")
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

plotSummary <- function (x, y, power = 2, wlim = c(.1,NA), short = F, ...) 
{
  xlim = c(NA,10^log10(max(x@params@species_params$w_inf)))
  font_size = 7
  
  # need to display the legend at the bottom and only p1 has the background so using that one
  
  p1 <- plotSpectra(x, power = power, wlim = wlim, ...)
  p1 <- p1  + scale_x_continuous(limits = xlim, trans = "log10", name = "Individual size [g]") +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          text = element_text(size=font_size),
          panel.background = element_blank(),
          panel.grid.minor = element_line(color = "gray"),
          legend.position = "right", legend.key = element_rect(fill = "white"))
  # guides(color = guide_legend(nrow=2))
  
  mylegend<-g_legend(p1) # save the legend
  p1 <- p1 + theme(legend.position = "none") # now remove it from the plot itself
  
  p7 <- plotBiomass(x)
  p7 <- p7 + theme(legend.position = "none",
                   text = element_text(size=font_size),
                   panel.background = element_blank(),
                   panel.grid.minor = element_line(color = "gray"),)
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
    
    
    
    p2 <- plotFeedingLevel(x, include_critical = F, ...)
    p2 <- p2 + scale_x_continuous(limits = xlim, trans = "log10") +
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            text = element_text(size=font_size),
            legend.position = "none")
    p3 <- plotPredMort(x, ...)
    p3 <- p3 + scale_x_continuous(limits = xlim, trans = "log10") +
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            text = element_text(size=font_size),
            legend.position = "none")
    p4 <- plotFMort(x, ...)
    p4 <- p4 + scale_x_continuous(limits = xlim, trans = "log10", name = "Individual size [g]") +
      theme(legend.position = "none",
            text = element_text(size=font_size),)
    
    
    
    
    
    
    
    # yeild and ssb |
    
    bm <- getBiomassFrame(x)
    plot_dat <- filter(bm, Year == max(unique(bm$Year)))
    # plot_dat$w_inf <- x@params@species_params$w_inf
    yieldDat <- getYield(x)
    plot_dat$yield <- yieldDat[dim(yieldDat)[1],]
    plot_dat$Year <- NULL
    plot_dat <- melt(plot_dat,"Species")
    p5 <- ggplot(plot_dat) +
      geom_bar(aes(x = Species,y = value, fill = Species, alpha = variable), stat = "identity", position = position_dodge()) +
      coord_cartesian(ylim = c(0.5*min(plot_dat$value),NA)) +
      # geom_text(aes(x = Species, y = value, label = Species), check_overlap = T)+
      
      # geom_point(aes(x = w_inf, y = Biomass, color = species)) +
      # geom_point(aes(x = w_inf, y = yield, color = species), shape = "+", size = 5) +
      # scale_x_continuous(name = "SSB and Yield") +
      scale_y_continuous(trans = "log10", name = "SSB and Yield") + #, limits = c(0.5*min(plot_dat$value),NA)) +
      scale_fill_manual(name = "Species", values = x@params@linecolour) +
      scale_alpha_manual(name = "Stat", values = c(1, 0.5), labels = c("SSB","Yield")) +
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            text = element_text(size=font_size),
            legend.position = "bottom", legend.key = element_rect(fill = "white"))
    
    mylegend<-g_legend(p5) # save the legend
    p5 <- p5 + theme(legend.position = "none")
    
    # try yield divided by total biomass, we want to see the difference
    
    # r0
    
    plot_dat <- as.data.frame(getRDI(x@params)/getRDD(x@params))
    plot_dat$species <- factor(rownames(plot_dat),x@params@species_params$species)
    colnames(plot_dat)[1] <- "ratio"
    plot_dat$w_inf <- as.numeric(x@params@species_params$w_inf)
    
    # trying to have bars at their w_inf but on a continuous scale
    plot_dat$label <- plot_dat$species
    plot_dat2 <- plot_dat
    plot_dat2$ratio <- 0
    plot_dat2$label <- NA
    plot_dat <- rbind(plot_dat,plot_dat2)
    
    
    p6 <- ggplot(plot_dat) +
      geom_line(aes(x = w_inf, y = ratio, color = species), size = 15, alpha = .8) +
      geom_text(aes(x = w_inf, y = ratio, label = label),position = position_stack(vjust = 0.5), angle = 30, size = 3)+
      scale_color_manual(name = "Species", values = x@params@linecolour) +
      scale_y_continuous(name = "RDI/RDD") +
      scale_x_continuous(name = "Asymptotic size (g)", trans = "log10") +
      theme(legend.position = "none",
            text = element_text(size=font_size),) 
    
    
    
    
    
    p10 <- plot_grid(p1,p2,p3,p4, p5, p6, p7, mylegend, byrow = F,  
                     # rel_heights = c(1,1,1,2),
                     rel_widths = c(2,1),
                     nrow = 4, align = "v", axis = "l")
  }
  # p <- grid.arrange(p10,mylegend, nrow=2,heights=c(9.5,0.5))
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
  # make one gear per species so we can bary the effort per species
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
  dat = catchAvg$Catch_1419_tonnes
  font_size = 8
  xlim = c(NA,10^log10(max(sim@params@species_params$w_inf)))
  
  switch (stage,
          "1" = {
            p1 <- plotSpectra(sim, power = power, wlim = wlim)#, ...)
            p1 <- p1  + scale_x_continuous(limits = xlim, trans = "log10", name = "Individual size [g]") +
              theme(
                text = element_text(size=font_size),
                panel.background = element_blank(),
                panel.grid.minor = element_line(color = "gray"),
                legend.position = "right", legend.key = element_rect(fill = "white"))
            # guides(color = guide_legend(nrow=2))
            
            mylegend<-g_legend(p1) # save the legend
            p1 <- p1 + theme(legend.position = "none") # now remove it from the plot itself
            
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

getDietComp<- function(sim, n = sim@n[dim(sim@n)[1],,],  n_pp = sim@n_pp[dim(sim@n_pp)[1],], 
                       feedinglevel=getFeedingLevel(object, n = n,n_pp = n_pp)){
  
  # initialisation to get it working with latest Mizer version
  object <- sim@params
  no_sp <- dim(object@species_params)[1]
  no_w <- length(object@w)
  no_w_full <- length(object@w_full)
  
  
  pred_kernel <- array(0, dim = c(no_sp, no_w, no_w_full),
                       dimnames = list(sp = object@species_params$species,
                                       w_pred = signif(object@w, 3),
                                       w_prey = signif(object@w_full, 3)))
  diet_comp<-array(0, c(no_sp, no_w, no_sp + 1, no_w_full),
                   dimnames=list( predator=as.character(object@species_params$species), pred_size = object@w,
                                  prey = c(as.character(object@species_params$species), "background"),
                                  prey_size = object@w_full))
  
  # Filling pred_kernel  
  
  Beta <- log(object@species_params$beta)
  sigma <- object@species_params$sigma
  # w_full has the weights from the smallest relevant plankton, to the largest fish
  x_full <- log(object@w_full)
  # We choose the origin of the x axis to be at the smallest plankton size
  x_full <- x_full - x_full[1]
  dx <- x_full[2] - x_full[1]
  # rr is the maximal log predator/prey mass ratio
  rr <- Beta + 3 * sigma
  ri <- floor(rr / dx)
  # might need to add some initialisation here
  # res@ft_pred_kernel_e <- matrix(0, nrow = no_sp, ncol = length(x_full))
  for (i in 1:no_sp) {
    # print(i)
    # We compute the feeding kernel terms and their fft.
    phi <- exp(-(x_full - Beta[i])^2 / (2 * sigma[i]^2))
    phi[x_full > rr[i]] <- 0
    phi[1] <- 0
    # Fourier transform of feeding kernel for evaluating available energy
    # res@ft_pred_kernel_e[i, ] <- fft(phi)
    # Fourier transform of feeding kernel for evaluating predation rate
    phi_p <- rep(0, no_w_full)
    phi_p[(no_w_full - ri[i] + 1):no_w_full] <- phi[(ri[i] + 1):2]
    # res@ft_pred_kernel_p[i, ] <- fft(phi_p)
    # Full feeding kernel array
    
    min_w_idx <- no_w_full - no_w + 1
    for (k in seq_len(no_w)) 
      pred_kernel[i, k, (min_w_idx - 1 + k):1] <- phi[1:(min_w_idx - 1 + k)]
    
  }
  
  # Starting the function
  #Biomass by species;
  n_total_in_size_bins<- sweep(n, 2, object@dw , "*")
  b_tot <- sweep(n_total_in_size_bins, 2, object@w , "*")
  
  #Biomass of resource as prey; scaled to reflect pred size kernel; might have to change if we start using interaction with resource spectrum like Hartvig et al. 2011
  
  #Note that we multiply the amount available by the availability parameter in the species parameter file 
  b_background <- (sweep( pred_kernel[,,], c(3), object@dw_full*object@w_full*n_pp, "*")) 
  #Search rate *  feeding level * predator biomass
  b_background<- sweep(b_background, c(1,2), object@search_vol,"*") #Scale up by search volume
  b_background<- sweep(b_background, c(1,2), feedinglevel,"*") # Scale according to feeding level. Prey eaten: g prey / year / g predator
  b_background_tot<-sweep(b_background,c(1,2), b_tot, "*") # Prey eaten: total g prey/ year  (given predator biomass density)

  # Index of predator size classes 
  idx_sp<- object@w_full %in% object@w
  
  for(i in 1:no_w){
    for(j in 1:no_sp){    
      diet_comp[j,i,1:no_sp,idx_sp]<- sweep(sweep( b_tot, c(1), object@interaction[j, 1:no_sp], "*"), c(2), pred_kernel[j,i,idx_sp], "*")
    }
  }
  
  # Search rate *  feeding level * predator biomass
  diet_comp[,,1:no_sp,]<- sweep(sweep(sweep(diet_comp[,,1:no_sp,], c(1,2), object@search_vol,"*"), c(1,2),feedinglevel,"*"), c(1,2),b_tot,"*")  # Prey eaten: total g prey/ year  (given predator biomass density)
  
  # Store background eaten 
  diet_comp[,,no_sp+1,]<- b_background_tot

  return(diet_comp)
} 
