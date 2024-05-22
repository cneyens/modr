#
# # tests --------
#
# tutorial_dr <- "C:/Users/casne/Desktop/r_test/flopy/tutorial01"
# fgrid <- file.path(tutorial_dr, 'mf6lake.dis.grb')
# fheads <- file.path(tutorial_dr, 'mf6lake.hds')
#
# grid <- read_grid(fgrid)
# hds <- read_heads(fheads, grid)
# df <- as_tibble(hds, grid, ilay = 1)
# wt <- at_water_table(hds, grid)
#
# ggplot(df, aes(x, y)) +
#   geom_polygon(aes(group = id, fill = value), colour='grey30', linewidth=0.1) +
#   geomtextpath::geom_textcontour(aes(z = value), breaks = seq(90, 99.8, 0.2), size = 2.5) +
#   coord_equal() +
#   scale_fill_viridis_c() +
#   theme_bw(base_size = 14)
#
# # synthetic valley -----
# synthetic_valley_dr <- "C:/WRDAPP/mf6.4.4/examples/ex-gwt-synthetic-valley/"
# fgrid <- file.path(synthetic_valley_dr, 'mf6gwf', 'flow.disv.grb')
# fhead <- file.path(synthetic_valley_dr, 'mf6gwf', 'flow.hds')
# fconc <- file.path(synthetic_valley_dr, 'mf6gwt', 'trans.ucn')
# fbudget <-  file.path(synthetic_valley_dr, 'mf6gwf', 'flow.cbc')
# fbudgettrans <- file.path(synthetic_valley_dr, 'mf6gwt', 'trans.cbc')
#
# grid <- read_grid(fgrid)
# hds <- read_heads(fhead, grid)
# hds_pts <- as_tibble(hds, grid, ilay = 1, as_points = TRUE)
# hds_tbl <- as_tibble(hds, grid, ilay = 1)
# conc <- read_concentrations(fconc, grid)
# conc_tbl <- as_tibble(conc, grid, ilay = 1, kstp = 60)
# cbc <- read_cellbudget(fbudget, grid)
# tcbc <- read_cellbudget(fbudgettrans, grid, totim = attr(conc, 'totim')[seq(10, 60, 10)])
#
# conc_totim = read_concentrations(fconc, grid, totim = 365.25 * c(1, seq(5, 30, 5)))
#
# wt <- at_water_table(hds, grid)
# wt_conc <- at_water_table(conc, grid, kstp=60)
#
# ggplot(hds_pts, aes(x, y)) +
#   geom_point(aes(colour = value)) +
#   coord_equal() +
#   scale_colour_viridis_c()
#
# ggplot(hds_tbl, aes(x, y)) +
#   geom_polygon(aes(group = id, fill = value)) +
#   coord_equal() +
#   scale_fill_viridis_c()
#
# ggplot(conc_tbl, aes(x, y)) +
#   geom_polygon(aes(group = id, fill = value), colour = 'grey70', linewidth = 0.05) +
#   coord_equal() +
#   scale_fill_viridis_c(option = 'magma', limits = c(0.01, NA), na.value = NA) +
#   theme_bw(base_size = 14)
#
# plot(attr(conc, 'totim'), conc[250,1,], type = 'l', xlab = 'Time (d)', ylab = bquote(C/C[0]~('-')), panel.first = grid())
#
# grid2 <- grid
# grid2$xorigin <- 210000
# grid2$yorigin <- 178000
#
# hds_spat = as_spatial(hds[,1,1], grid2, crs = 31370)
# conc_spat = as_spatial(conc[,1,60], grid)
#
# iso_hds <- get_isolines(hds[,1,1], grid2, crs=31370, levels = seq(0, 5, 0.2))
# isob_conc <- get_isobands(conc[,1,60], grid2, crs=31370, levels = 20)
#
# xmin <- grid2$xorigin
# xmax <- grid2$xorigin + max(grid$vertices[,1])
# ymin <- grid2$yorigin
# ymax <- 184100
# hds_raster <- as_raster(hds[,1,1], grid2, xout=seq(xmin, xmax, 25), yout = seq(ymin, ymax, 25), crs=31370)
# hds_raster_100m <- as_raster(hds[,1,1], grid2, delta=100, crs=31370)
#
# ggplot(conc_spat, aes(fill = value)) +
#   geom_sf() +
#   scale_fill_viridis_c(option = 'magma', limits = c(0.01, NA), na.value = NA)
#
# ggplot(isob_conc, aes(fill = high)) +
#   geom_sf() +
#   scale_fill_viridis_c(option = 'magma', limits = c(0.01, NA), na.value = NA)
#
#
# ggplot() +
#   geom_sf(data = hds_spat, aes(fill = value)) +
#   geom_sf(data = iso_hds) +
#   # geom_contour(data = as_tibble(hds[,1,1], grid, as_points = TRUE), aes(x, y, z=value)) +
#   scale_fill_viridis_c()
#
# terra::plot(hds_raster)
#
# hds_pts <- as_tibble(hds, grid, ilay = 1, as_points = TRUE)
# xout <- seq(0, 3800, length = 100)
# yout <- seq(0, 6100, length = 100)
# hds_resample <- resample_to_grid(hds[,1,1], grid, xout, yout)
# grid_resample <- expand.grid(x = xout, y = yout)
# grid_resample$value <- c(t(hds_resample[dim(hds_resample)[1]:1,]))
#
# ggplot() +
#   geom_polygon(data = hds_tbl, aes(x, y, group = id, fill = value)) +
#   # geom_contour(data = grid_resample, aes(x, y, z = value), colour = 'black', binwidth = 0.2, linewidth = 0.2) +
#   geomtextpath::geom_textcontour(data = grid_resample, aes(x, y, z = value), colour = 'black', binwidth = 0.2, linewidth = 0.2, size = 3) +
#   coord_equal(expand = FALSE) +
#   scale_fill_viridis_c() +
#   theme_bw(base_size = 14) +
#   guides(fill = guide_colorbar(barheight = 30, barwidth = 1))
#
# ggplot(grid_resample) +
#   geom_contour(aes(x, y, z=value)) +
#   coord_equal()
#
# contour(xout, yout, t(hds_resample[dim(hds_resample)[1]:1,]), nlevels = 20, asp = 1)
#
#
# # tests for convert_xyz_to_grid
# grd <- rep(0, grid$ncpl)
# n <- 1000
# coords <- data.frame(x = runif(n = n, min = -250, max = max(grid$vertices[,1]) + 250),
#                      y = runif(n = n, min = -250, max = max(grid$vertices[,2]) + 250))
#
# cellids <- convert_xyz_to_grid(coords$x, coords$y, grid=grid)
# grd[cellids] <- 1
#
# tblgrd = as_tibble(grd, grid)
#
# ggplot() +
#   geom_polygon(data = tblgrd, aes(x, y, group = id, fill = factor(value)),
#                alpha=0.7, colour='grey50', linewidth = 0.2, show.legend = FALSE) +
#   geom_point(data = coords, aes(x, y), shape = 4) +
#   coord_equal(expand = FALSE) +
#   theme_bw(base_size = 14) #+
# # coord_equal(xlim = c(1800, 2600), ylim = c(4800, 5800))
#
#
# # capture fraction ----
# capture_dr = file.path("C:/WRDAPP/mf6.4.4/examples/ex-gwf-capture/")
# fgrid = file.path(capture_dr, 'ex-gwf-capture.dis.grb')
# fheads = file.path(capture_dr, 'ex-gwf-capture.hds')
#
# grid = read_grid(fgrid)
# hds = read_heads(fheads, grid)
#
# hds_df = as_tibble(hds, grid, ilay = 1)
# hds_pts = as_tibble(hds, grid, ilay = 1, as_points = TRUE)
#
# ggplot() +
#   geom_polygon(data = hds_df, aes(x, y, group = id, fill = value)) +
#   geomtextpath::geom_textcontour(data = hds_pts, aes(x, y, z = value)) +
#   coord_equal(expand = FALSE) +
#   scale_fill_viridis_c(option = 'mako')
#
# hds_df_cross = as_tibble_cross(hds, grid, j = 15, limit_wt = T)
#
# ggplot(hds_df_cross) +
#   geom_polygon(aes(x = y, y = z, group = id, fill = value))
#
# hds_df_cross = as_tibble_cross(hds, grid, i = 28, limit_wt = T)
#
# wt = at_water_table(hds, grid)
#
# ggplot(hds_df_cross) +
#   geom_polygon(aes(x = x, y = z, group = id, fill = value)) +
#   geom_line(data = data.frame(x = cumsum(grid$delr) - grid$delr/2, hds = wt[28,]), aes(x, hds), col = 'red')
#
# # mt3dms p10 ----
# mt3d_dr = file.path("C:/WRDAPP/mf6.4.4/examples/ex-gwt-mt3dms-p10/")
# fgrid = file.path(mt3d_dr, 'gwf-p10-mf6.dis.grb')
# fheads = file.path(mt3d_dr, 'gwf-p10-mf6.hds')
# fconc = file.path(mt3d_dr, 'gwt-p10-mf6.ucn')
#
# grid = read_grid(fgrid)
# hds = read_heads(fheads, grid)
# conc = read_concentrations(fconc, grid, totim = 500)
#
# conc_df = as_tibble(conc[,,3,1], grid, as_points = TRUE)
# hds_tbl = as_tibble(hds, grid, ilay = 3, kstp = 2)
#
# ggplot(hds_tbl) +
#   geom_polygon(aes(x, y, group = id, fill = value)) +
#   coord_equal() +
#   # scale_fill_binned(n.breaks = 9) # simple default for binning a continuous scale; use scale_*_steps(2/n) for more control, or scale_*_fermenter
#   # scale_fill_stepsn(colours = hcl.colors(n = 9, palette = 'blues 3'), n.breaks = 9)
#   scale_fill_fermenter(palette = 'Blues', n.breaks = 9)
#
# ggplot(conc_df) +
#   geom_contour_filled(aes(x, y, z = value), breaks = seq(10, 45, 5)) +
#   geomtextpath::geom_textcontour(aes(x, y, z = value, label = after_stat(round(level, 2))), breaks = seq(10, 45, 5),
#                                  parse = TRUE, straight = TRUE, linecolour = 'white') +
#   coord_equal(xlim = c(4800, 6800), ylim = c(9000, 11300)) +
#   scale_fill_viridis_d(option = 'plasma')
#
# # tests for convert_xyz_to_grid
# grd <- grid$top * 0
# n <- 1000
# coords <- data.frame(x = runif(n = n, min = -250, max = sum(grid$delr) + 250),
#                      y = runif(n = n, min = -250, max = sum(grid$delc) + 250))
#
# cellids <- convert_xyz_to_grid(coords$x, coords$y, grid=grid)
# grd[cellids] <- 1
#
# tblgrd = as_tibble(grd, grid)
#
# ggplot() +
#   geom_polygon(data = tblgrd, aes(x, y, group = id, fill = factor(value)),
#                alpha=0.7, colour='grey50', linewidth = 0.2, show.legend = FALSE) +
#   geom_point(data = coords, aes(x, y), shape = 4) +
#   coord_equal(expand = FALSE) +
#   theme_bw(base_size = 14) +
#   coord_equal(xlim = c(4600, 7000), ylim = c(8600, 12000))
#
#
#
# # henry b ----
# # cross section
# henry_dr = file.path("C:/WRDAPP/mf6.4.4/examples/ex-gwt-henry-b")
# fgrid = file.path(henry_dr, 'flow.dis.grb')
# fhead = file.path(henry_dr, 'flow.hds')
# fconc = file.path(henry_dr, 'trans.ucn')
#
# grid = read_grid(fgrid)
# hds = read_heads(fhead, grid)
# conc = read_concentrations(fconc, grid, totim = c(0.5, 0.001))
#
# hds_cross = as_tibble_cross(hds, grid, kstp = 500, i = 1)
# conc_cross = as_tibble_cross(conc, grid, i = 1, kstp = 2)
# conc_cross_pts = as_tibble_cross(conc[,,,2, drop = FALSE], grid, i = 1, as_points = TRUE)
# hds_cross_pts = as_tibble_cross(hds, grid, kstp = 500, i = 1, as_points = TRUE)
#
# ggplot() +
#   geom_polygon(data = conc_cross, aes(x = x, y = z, group = id, fill = value)) +
#   geomtextpath::geom_textcontour(data = conc_cross_pts, aes(x, z, z = value), breaks = c(0.01, 0.1, 0.5, 0.9, 0.99) * 35) +
#   coord_cartesian(expand = FALSE) +
#   scale_fill_gradientn(colours = hcl.colors(n = 11,  'roma', rev = TRUE))
# # scale_fill_gradientn(colours = hcl.colors(n = 11,  'RdYlBu', rev = TRUE))
# # scale_fill_distiller(palette = 'RdYlBu')
# # scale_fill_viridis_c(option = 'turbo')
#
# ggplot() +
#   geom_polygon(data = hds_cross, aes(x = x, y = z, group = id, fill = value)) +
#   geomtextpath::geom_textcontour(data = hds_cross_pts, aes(x, z, z = value, label = after_stat(round(level, 3)))) +
#   coord_cartesian(expand = FALSE) +
#   scale_fill_gradientn(colours = hcl.colors(n = 11,  'mint', rev = FALSE))
#
# # salt lake ----
# # cross section
# salt_dr = file.path("C:/WRDAPP/mf6.4.4/examples/ex-gwt-saltlake")
# fgrid = file.path(salt_dr, 'flow.dis.grb')
# fhead = file.path(salt_dr, 'flow.hds')
# fconc = file.path(salt_dr, 'trans.ucn')
#
# grid = read_grid(fgrid)
# hds = read_heads(fhead, grid)
# conc = read_concentrations(fconc, grid)
#
# hds_cross = as_tibble_cross(hds, grid, kstp = 400, i = 1)
# hds_cross_pts = as_tibble_cross(hds, grid, kstp = 400, i = 1, as_points = TRUE)
# conc_cross = as_tibble_cross(conc, grid, kstp = 400, i = 1)
#
# ggplot() +
#   geom_polygon(data = conc_cross, aes(x = x, y = z, group = id, fill = value)) +
#   geomtextpath::geom_textcontour(data = hds_cross_pts, aes(x, z, z = value, label = after_stat(format(level, nsmall = 2))),
#                                  binwidth = 0.05, straight = TRUE, size = 3, colour = 'grey60') +
#   coord_cartesian(xlim = c(0, 75)) +
#   scale_fill_viridis_c(option = 'turbo')
#
# # curvilinear ----
# curv_dr = file.path("C:/WRDAPP/mf6.4.4/examples/ex-gwf-curvilin/")
# fgrid = file.path(curv_dr, 'ex-gwf-curvilin.disv.grb')
# fheads = file.path(curv_dr, 'ex-gwf-curvilin.hds')
#
# grid = read_grid(fgrid)
# hds = read_heads(fheads, grid)
#
# hds_tbl = as_tibble(hds, grid, ilay = 1)
#
# ggplot(hds_tbl) +
#   geom_polygon(aes(x, y, group = id, fill = value), colour = 'grey25', linewidth = 0.2) +
#   coord_equal() +
#   scale_fill_viridis_c(option = 'mako')
#
# # lgr ----
# lgr_dr = file.path("C:/WRDAPP/mf6.4.4/examples/ex-gwf-lgrv-lgr")
# fgrid_parent = file.path(lgr_dr, 'parent.dis.grb')
# fgrid_child = file.path(lgr_dr, 'child.dis.grb')
# fhead_parent = file.path(lgr_dr, 'parent.hds')
# fhead_child = file.path(lgr_dr, 'child.hds')
#
# grid_parent = read_grid(fgrid_parent)
# grid_child = read_grid(fgrid_child)
# hds_parent = read_heads(fhead_parent, grid_parent)
# hds_child = read_heads(fhead_child, grid_child)
#
# hds_df_parent = as_tibble(hds_parent, ilay = 1, grid_parent)
# hds_df_child = as_tibble(hds_child, ilay = 1, grid_child)
#
# ggplot() +
#   geom_polygon(data = hds_df_parent, aes(x, y, group = id, fill = value)) +
#   coord_equal(expand = FALSE) +
#   scale_fill_distiller(palette = 'YlGnBu')
#
# ggplot() +
#   geom_polygon(data = hds_df_child, aes(x, y, group = id, fill = value)) +
#   coord_equal(expand = FALSE, xlim = c(0, 5145), ylim = c(0, 4575)) +
#   scale_fill_distiller(palette = 'YlGnBu')
#
# ggplot() +
#   geom_polygon(data = hds_df_parent, aes(x, y, group = id, fill = value)) +
#   geom_polygon(data = hds_df_child, aes(x, y, group = id, fill = value)) +
#   coord_equal(expand = FALSE) +
#   scale_fill_distiller(palette = 'YlGnBu')
#
# xout = seq(0, 5145, length = 1000)
# yout = seq(0, 4575, length = 1000)
#
# hds_resampled_parent = resample_to_grid(hds_parent[,,1,1], grid = grid_parent, xout=xout, yout=yout)
# hds_resampled_child = resample_to_grid(hds_child[,,1,1], grid = grid_child, xout=xout, yout=yout)
#
# gr = expand.grid(x = xout, y = yout)
# gr$zp = c(t(hds_resampled_parent[1000:1,]))
# gr$zc = c(t(hds_resampled_child[1000:1,]))
# gr$z = ifelse(is.na(gr$zc), gr$zp, gr$zc)
# gr = gr[!is.na(gr$z),]
#
# ggplot() +
#   geom_polygon(data = hds_df_parent, aes(x, y, group = id, fill = value)) +
#   geom_polygon(data = hds_df_child, aes(x, y, group = id, fill = value)) +
#   geomtextpath::geom_textcontour(data = gr, aes(x, y, z = z)) +
#   coord_equal(expand = FALSE) +
#   scale_fill_distiller(palette = 'YlGnBu')
#
# # lgr global refined ----
# lgr_dr = file.path("C:/WRDAPP/mf6.4.4/examples/ex-gwf-lgrv-gr")
# fgrid = file.path(lgr_dr, 'parent.dis.grb')
# fhead = file.path(lgr_dr, "parent.hds")
#
# grid = read_grid(fgrid)
# hds = read_heads(fhead, grid)
#
# wt = at_water_table(hds, grid)
# hds_df = as_tibble(wt, grid)
#
# ggplot(hds_df) +
#   geom_polygon(aes(x, y, group = id, fill = value)) +
#   coord_equal(expand = FALSE) +
#   scale_fill_viridis_c()
#
#
# hds_cross = as_tibble_cross(hds, grid, j = 65)
# y = sort(unique(hds_cross$y), decreasing = TRUE)[-1] - grid$delc/2
# wthds = wt[,65]
#
# ggplot(hds_cross) +
#   geom_polygon(aes(x = y, y = z, fill = value, group = id)) +
#   geom_line(data = data.frame(y = y, wt = wthds), aes(y, wt))
#
# hds_cross = as_tibble_cross(hds, grid, j = 65, limit_wt = T)
#
# ggplot(hds_cross) +
#   geom_polygon(aes(x = y, y = z, fill = value, group = id)) +
#   geom_line(data = data.frame(y = y, wt = wthds), aes(y, wt))
#
# top_df = as_tibble(grid$top, grid)
# ggplot() +
#   geom_polygon(data = top_df, aes(x, y, group = id, fill = value)) +
#   coord_equal() +
#   scale_fill_stepsn(colours = hcl.colors(n = 11, 'terrain'), n.breaks = 9)
#
# # tests for convert_xyz_to_grid
# grd <- matrix(0, nrow = grid$nrow, ncol = grid$ncol)
# n <- 1000
# set.seed(42)
# coords <- data.frame(x = runif(n = n, min = -250, max = sum(grid$delr) + 250),
#                      y = runif(n = n, min = -250, max = sum(grid$delc) + 250))
#
# cellids <- convert_xyz_to_grid(coords$x, coords$y, grid=grid)
# grd[cellids] <- 1
#
# tblgrd = as_tibble(grd, grid)
#
# ggplot() +
#   geom_polygon(data = tblgrd, aes(x, y, group = id, fill = factor(value)),
#                alpha=0.7, colour='grey50', linewidth = 0.2, show.legend = FALSE) +
#   geom_point(data = coords, aes(x, y), shape = 4, size = 1) +
#   coord_equal(expand = FALSE) +
#   theme_bw(base_size = 14) +
#   coord_equal(xlim = c(-200, 1000), ylim = c(1500, 2500))
#
#
# # lgr gwf-gwf-xt3d ----
# lgrgwf_dr = file.path("C:/WRDAPP/mf6.4.4/examples/ex-gwf-u1gwfgwf-s4")
# fgrid_parent = file.path(lgrgwf_dr, 'outer.dis.grb')
# fgrid_child = file.path(lgrgwf_dr, 'inner.dis.grb')
# fhead_parent = file.path(lgrgwf_dr, 'outer.hds')
# fhead_child = file.path(lgrgwf_dr, 'inner.hds')
#
# grid_parent = read_grid(fgrid_parent)
# grid_child = read_grid(fgrid_child)
# hds_parent = read_heads(fhead_parent, grid_parent)
# hds_child = read_heads(fhead_child, grid_child)
#
# hds_df_parent = as_tibble(hds_parent, ilay = 1, grid_parent)
# hds_df_child = as_tibble(hds_child, ilay = 1, grid_child)
#
# ggplot() +
#   geom_polygon(data = hds_df_parent, aes(x, y, group = id, fill = value)) +
#   geom_polygon(data = hds_df_child, aes(x, y, group = id, fill = value)) +
#   coord_equal() +
#   scale_fill_gradientn(colors = hcl.colors(n = 11, 'Lajolla', rev = TRUE))
