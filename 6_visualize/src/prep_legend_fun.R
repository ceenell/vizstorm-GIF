
prep_legend_fun <- function(precip_bins, legend_styles, timesteps_ind, storm_points_sf, DateTime=NA, x_pos = c('left', 'right'), y_pos = c('bottom','top'), legend_text_cfg){

  x_pos <- match.arg(x_pos)
  y_pos <- match.arg(y_pos)

  if(is.na(DateTime)) {
    timesteps <- readRDS(sc_retrieve(timesteps_ind, remake_file = getOption("scipiper.remake_file")))
    DateTime <- timesteps[1]
    rm(timesteps)
  }
  this_DateTime <- as.POSIXct(DateTime, tz = "UTC") # WARNING, IF WE EVER MOVE FROM UTC elsewhere, this will be fragile/bad.

  hurricane_col <- NA
  has_storm_track <- !is.null(storm_points_sf)
  if(has_storm_track) {
    this_dot <- filter(storm_points_sf, DateTime == this_DateTime)
    if(nrow(this_dot) > 0) {
      hurricane_col <- legend_styles$hurricane_cols[(this_dot$SS + 1)]
      hurricane_cat <- legend_styles$hurricane_col_names[(this_dot$SS + 1)]
    }
  }

  rm(storm_points_sf)

  plot_fun <- function(){

    # compute position info shared across multiple legend elements
    coord_space <- par()$usr
    bin_w_perc <- 0.02 # percentage of X domain
    bin_h_perc <- 0.035 # *also* percentage of X domain
    bin_w <- bin_w_perc * diff(coord_space[c(1,2)])
    bin_h <- bin_h_perc * diff(coord_space[c(1,2)])
    if (x_pos == 'left'){
      txt_pos = 4
      x_edge <- coord_space[1]
      shift_dir <- 1
    } else if (x_pos == 'right'){
      txt_pos = 2
      x_edge <- coord_space[2]
      shift_dir <- -1
    }
    ybottom <- coord_space[4]*.7
    dot_x <- x_edge+bin_w/2*shift_dir
    dot_txt_x <- x_edge+bin_w*0.7*shift_dir
    seg_x <- x_edge+bin_w/4*shift_dir
    center_to_txt_y <- strheight("A")/3 # height of character divided by three seems to do the trick

    # plot precip bins and precip label
    precip_txt_y <- ybottom+2*bin_h*0.4
    text(x_edge, precip_txt_y, labels = 'Total rainfall (in)', pos = txt_pos,
         cex=legend_text_cfg$cex, col=legend_text_cfg$col, family=legend_text_cfg$family, font = 3)
    text(x_edge, precip_txt_y-bin_h*0.5, labels = 'Data: NOAA', pos = txt_pos,
         cex=legend_text_cfg$cex, col=legend_text_cfg$col, family=legend_text_cfg$family, font = 3)
    if (x_pos == 'left'){
      bin_j <- 1:nrow(precip_bins)
      xright <- x_edge+bin_w*1.5
    } else if (x_pos == 'right'){
      bin_j <- nrow(precip_bins):1
      xright <- x_edge+(bin_w*0.5)
    }

    for (j in bin_j){
      col <- as.character(precip_bins$col[j])
      bin_bottom <- ybottom-nrow(precip_bins)*bin_h
      #text_col <- ifelse(any(col2rgb(col) < 130), 'white','black')
      rect(xleft = xright-bin_w, ybottom = bin_bottom+(bin_h*(j-1)), xright = xright-(bin_w*0.3), ytop = bin_bottom+bin_h*j, col = col, border = NA)
      lines(x=c(xright-bin_w,xright), y = c(bin_bottom+(bin_h*(j-1)), bin_bottom+(bin_h*(j-1))))
      text_char <- as.character(precip_bins$break_factor[j]) %>% stringr::word(1,1, sep =",") %>% gsub(pattern = "\\(|\\]", replacement = '')
      text(xright+bin_w*0.5, bin_bottom+bin_h*(j-1), text_char, col = "black", cex = 1.3)
    }

    # plot gage points legend
    gage_bottom <- coord_space[4]*0.5

    # add rect for title
    box_y_low = coord_space[4]*0.8
    box_y_up = gage_bottom+6.5*bin_h
    box_x_left = xright-bin_w
    box_x_right = coord_space[2]*0.35

    rect(xleft = box_x_left, xright = box_x_right, ybottom = box_y_low, ytop = box_y_up,
         col = alpha("white", 0.8), border = NA)
    text(box_x_left, coord_space[4]*0.875, labels = legend_text_cfg$storm_name, cex = legend_text_cfg$cex*2,
         col = legend_text_cfg$col)

    gage_caveat_y <- gage_bottom+5*bin_h
    text(x_edge, gage_caveat_y, labels = 'Select USGS streamgages', pos = txt_pos,
         cex=legend_text_cfg$cex, col=legend_text_cfg$col, family=legend_text_cfg$family)
    normal_y <- gage_bottom+4*bin_h
    points(dot_x, gage_caveat_y-(0.5*bin_h), pch = 21, bg = legend_styles$gage_norm_col, col = NA, cex = 2, lwd=2)
    text(dot_txt_x, gage_caveat_y-(0.5*bin_h), labels = 'streamgage', pos = txt_pos,
         cex=legend_text_cfg$cex, col=legend_text_cfg$col, family=legend_text_cfg$family)
    #flood_y <- ybottom+3*bin_h
    points(dot_x+bin_w*5, gage_caveat_y-(0.5*bin_h), pch = 21, bg = legend_styles$gage_norm_col, col = legend_styles$gage_flood_col, lwd = 4, cex = 2)
    text(dot_txt_x+bin_w*5, gage_caveat_y-(0.5*bin_h), labels = 'flooding', pos = txt_pos,
         cex=legend_text_cfg$cex, col=legend_text_cfg$col, family=legend_text_cfg$family)

    # plot storm legend
    hurricane_y <- gage_bottom+5.75*bin_h
    if(has_storm_track) {
      if(is.na(hurricane_col)) {
        # When the hurricane dot is no longer visible, switch to showing just the path
        text(dot_txt_x+(bin_w), hurricane_y, labels = paste("Path of", legend_styles$storm_name), pos = txt_pos,
             cex=legend_text_cfg$cex*2, col=legend_text_cfg$col, family=legend_text_cfg$family, font = 2)
        segments(xright-bin_w, hurricane_y+center_to_txt_y, dot_txt_x+(bin_w), lty = "dotted",
                 col = legend_styles$storm_line_col, lwd = 3)
      } else {
        text(dot_txt_x+(bin_w), hurricane_y, labels = sprintf(hurricane_cat, legend_styles$storm_name), pos = txt_pos,
             cex=legend_text_cfg$cex*2, col=legend_text_cfg$col, family=legend_text_cfg$family, font = 2)
        points(dot_x, hurricane_y+center_to_txt_y, pch = 21, bg = hurricane_col, col = NA, cex = 4)
      }
    }

  }
  return(plot_fun)
}
