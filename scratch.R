
# scratch -----------------------------------------------------------------

library(scipiper)
library(tidyverse)

scmake('6_storm_gif_tasks.yml')

# test frame
scmake('6_visualize/tmp/gif_TEST_frame_a_20210916_12.png', '6_storm_gif_tasks.yml')

# real deal
scmake('6_visualize/tmp/gif_frame_a_202109165_12.png', '6_storm_gif_tasks.yml')

# run it all
scmake()

# troubleshooting ---------------------------------------------------------


# precip
scmake('1_fetch/out/precip_data.nc.ind', force = TRUE)
