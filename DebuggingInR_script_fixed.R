
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

Sys.setenv(TZ = "UTC")

library(data.table)
library(ggplot2)

##### Case 1: Calculate maximum wind speed in a certain wind direction-----

df = fread("data/weather_data_erken_2010.txt")

# u_wind: numeric vector, wind speed in m/s in West-East direction
# v_wind: numeric vector, wind speed in m/s in South-North direction
# min_dir: minimum wind direction to calculate maximum
# max_dir: maximum wind direction to calculate maximum
max_wind_in_dir = function(u_wind, v_wind, min_dir = 0, max_dir = 360){
  if(min_dir < 0) min_dir = min_dir + 360
  if(max_dir < 0) max_dir = max_dir + 360
  if(min_dir > 360) min_dir = min_dir - 360
  if(max_dir > 360) max_dir = max_dir - 360
  
  df_wind = data.table(wind_speed = sqrt(u_wind^2 + v_wind^2),
                       wind_dir_degrees = wind_vectors_to_direction(u_wind, v_wind))
  
  if(max_dir >= min_dir){
    df_wind = df_wind[wind_dir_degrees >= min_dir & wind_dir_degrees <= max_dir]
  }else{
    df_wind = df_wind[wind_dir_degrees >= min_dir | wind_dir_degrees <= max_dir]
  }
  
  if(nrow(df_wind) == 0L){
    return(NA)
  }else{
    return(max(df_wind[, wind_speed]))
  }
}

wind_vectors_to_direction = function(u_wind, v_wind){
  windspeed = sqrt(u_wind^2 + v_wind^2)
  wind_dir_rad = atan2(v_wind / windspeed, u_wind / windspeed) 
  wind_dir_degrees = wind_dir_rad * 180 / pi
  wind_dir_degrees = 270 - wind_dir_degrees
  wind_dir_degrees[wind_dir_degrees > 360 & !is.na(wind_dir_degrees)] =
    wind_dir_degrees[wind_dir_degrees > 360 & !is.na(wind_dir_degrees)] - 360
  return(wind_dir_degrees)
}

# Script:
# Function works on some example data
max_wind_in_dir(u_wind = c(2, 3, -3, 5, 0, -2, -1, -6),
                v_wind = c(2, 0, -1, 6, 4, 1, -5, -2),
                min_dir = 70,
                max_dir = 110)

# But error when applying to all data in our lake dataset
max_wind_in_dir(u_wind = df$u_wind, v_wind = df$v_wind,
                min_dir = 70, max_dir = 110)

# Calculate highest wind speed per wind direction, and plot
df_max_wind = data.table(wind_dir = factor(c("N", "NE", "E", "SE", "S", "SW", "W", "NW"),
                                           levels = c("N", "NE", "E", "SE", "S", "SW", "W", "NW")),
                         centr_wind = seq(0, 315, by = 45))
df_max_wind[, max_wind := sapply(centr_wind, function (x) max_wind_in_dir(df$u_wind,
                                                                          df$v_wind,
                                                                          min_dir = x - 22.5,
                                                                          max_dir = x + 22.5))]

ggplot(df_max_wind) +
  geom_col(aes(wind_dir, max_wind))

##### Case 2: Calculate and plot isotherms -----

df = fread("data/laketemp_data_erken_2010.txt")
df = melt(df, id.vars = "datetime", variable.name = "depth",
          value.name = "temp")
df[, depth := gsub("wtr_", "", depth)]
df[, depth := as.numeric(depth)]
setorder(df, datetime, depth)

# Function to calculate the isotherm depth from a vector of depths (dep, positive)
# and a vector of water temperatures (wtr). In the (rare) case that the isotherm
# is passed multiple times, it will pick the uppermost depth

calc_isotherm_depth = function(dep, wtr, isotherm, na.rm = FALSE){
  # Check arguments
  if(length(dep) != length(wtr)){
    stop("'dep' and 'wtr' should be the same length!")
  }
  if(length(dep) < 3){
    stop("You need at least three unique depths in order to use this function!")
  }
  if(any(dep < 0) | any(diff(dep) < 0)){
    stop("'dep' argument should always be positive and in ascending order!")
  }
  
  # Handle NA input values
  if(all(is.na(wtr)) | (any(is.na(wtr)) & !na.rm)){
    return(as.numeric(NA))
  }
  
  # Return NA if the isotherm is outside the range of provided temperatures
  if(isotherm < min(wtr, na.rm = na.rm) | isotherm > max(wtr, na.rm = na.rm)){
    return(as.numeric(NA))
  }
  
  # Find first depth where isotherm is intercepted
  depth_ind_passing_iso = which(wtr >= isotherm & shift(wtr, n = -1L) <= isotherm |
                                  wtr <= isotherm & shift(wtr, n = -1L) >= isotherm)
  
  if(length(depth_ind_passing_iso) == 0L) stop("Could not find depth at which ",
                                            "the isotherm is crossed!")
  depth_above = dep[depth_ind_passing_iso[1L]]
  depth_below = dep[depth_ind_passing_iso[1L] + 1L]
  
  # Linearly interpolate between depth_above and depth_below
  the_depth = approx(x = c(wtr[depth_ind_passing_iso[1L]],
                           wtr[depth_ind_passing_iso[1L] + 1L]),
                     y = c(depth_above, depth_below),
                     xout = isotherm)$y
  
  return(the_depth)
}

# Test:
calc_isotherm_depth(dep = 0:7,
                    wtr = c(12, 11.5, 11.5, 10.5, 9, 8, 7.5, 7.5),
                    isotherm = 11.2)

# Apply to all dates
df_iso_15 = df[, .(iso_depth = calc_isotherm_depth(depth,
                                                   temp,
                                                   isotherm = 15)),
               by = datetime]

# Let's look at the data and make some plots to see if the output is
# indeed the way we expect
ggplot(df_iso_15) +
  geom_line(aes(datetime, iso_depth)) +
  labs(title = "15-degree C isotherm over time",
       y = "Isotherm depth (m)") +
  scale_y_reverse() +
  theme_light()

# Lastly, we can plot it on top of a heat map and show several isotherms
df_iso = df[, .(iso_10 = calc_isotherm_depth(depth,
                                             temp,
                                             isotherm = 10),
                iso_15 = calc_isotherm_depth(depth,
                                             temp,
                                             isotherm = 15),
                iso_20 = calc_isotherm_depth(depth,
                                             temp,
                                             isotherm = 20)),
            by = datetime]


ggplot() +
  geom_tile(data = df, aes(datetime, depth, colour = temp)) +
  geom_line(data = df_iso, aes(datetime, iso_10, linetype = "ISO10")) +
  geom_line(data = df_iso, aes(datetime, iso_15, linetype = "ISO15")) +
  geom_line(data = df_iso, aes(datetime, iso_20, linetype = "ISO20")) +
  scale_colour_gradientn(colours = rev(heat.colors(8))) +
  scale_linetype_manual(name = "Isotherms",
                        values = c("ISO10" = "dotted",
                                   "ISO15" = "solid",
                                   "ISO20" = "dashed")) +
  scale_y_reverse() +
  labs(title = "Heatmap with isotherms",
       y = "Depth (m)") +
  theme_light()
