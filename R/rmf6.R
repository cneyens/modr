

#' Read a MODFLOW 6 binary grid file
#'
#' [read_grid()] reads a MODFLOW 6 binary grid file and returns it as a mf6grid object
#'
#' @param file path to the binary grid file
#'
#' @return An object of class mf6grid which is a list containing the grid information for the MODFLOW 6 model
#' @export
#'
#' @examples
#'
read_grid <- function(file) {
  f <- file
  con <- file(f, open='rb')

  try({
    headerChars <- 50
    grid <- readChar(con, nchars = headerChars)
    version <- readChar(con, nchars = headerChars)
    ntxt <- readChar(con, nchars = headerChars)
    lentxt <- readChar(con, nchars = headerChars)

    gridtype <- trimws(gsub('GRID', '', grid))
    ntxt <- as.numeric(trimws(gsub('NTXT', '', ntxt)))
    lentxt <- as.numeric(trimws(gsub('LENTXT', '', lentxt)))

    if(gridtype == 'DIS') {
      vars <- list()
      types <- list()
      dims <- list()

      for(i in seq(ntxt)) {
        x <- readChar(con, nchars = lentxt)
        x <- strsplit(trimws(x), ' ')[[1]]
        vars[[x[1]]] <- as.numeric(x[length(x)])
        types[[x[1]]] <- x[2]
        dims[[x[1]]] <- as.numeric(x[4])
      }

      dat <- list()
      for(i in seq(ntxt)) {
        type <- tolower(types[[i]])
        n <- ifelse(dims[[i]] == 0, 1, vars[[i]])
        if(type == 'integer') {
          x <- readBin(con, what = type, n = n)
        } else {
          x <- readBin(con, what = type, size = 8, n = n)
        }
        dat[[i]] <- x
      }

      names(dat) <- tolower(names(vars))
      dat$gridtype <- gridtype

      dat$top <- matrix(dat$top, nrow = dat$nrow, ncol = dat$ncol, byrow = TRUE)
      dat$botm <- aperm(array(dat$botm, dim = c(dat$ncol, dat$nrow, dat$nlay)), c(2, 1, 3))
      dat$idomain <- aperm(array(dat$idomain, dim = c(dat$ncol, dat$nrow, dat$nlay)), c(2, 1, 3))
      dat$icelltype <- aperm(array(dat$icelltype, dim = c(dat$ncol, dat$nrow, dat$nlay)), c(2, 1, 3))

    } else if(gridtype == 'DISV') {
      vars <- list()
      types <- list()
      dims <- list()

      for(i in seq(ntxt)) {
        x <- readChar(con, nchars = lentxt)
        x <- strsplit(trimws(x), ' ')[[1]]
        vars[[x[1]]] <- as.numeric(x[length(x)])
        types[[x[1]]] <- x[2]
        dims[[x[1]]] <- as.numeric(x[4])
      }

      dat <- list()
      for(i in seq(ntxt)) {
        type <- tolower(types[[i]])
        n <- ifelse(dims[[i]] == 0, 1, vars[[i]])
        if(names(dims)[i] == 'VERTICES') n <- n * 2
        if(type == 'integer') {
          x <- readBin(con, what = type, n = n)
        } else {
          x <- readBin(con, what = type, size = 8, n = n)
        }
        dat[[i]] <- x
      }

      names(dat) <- tolower(names(vars))
      dat$gridtype <- gridtype

      dat$botm <- matrix(dat$botm, ncol = dat$nlay)
      dat$vertices <- matrix(dat$vertices, ncol = 2, byrow = TRUE)
      dat$idomain <- matrix(dat$idomain, ncol = dat$nlay)
      dat$icelltype <- matrix(dat$icelltype, ncol = dat$nlay)


    } else if(gridtype == 'DISU') {
      stop('DISU grid not yet supported', call. = FALSE)
    } else {
      stop('Unknown grid type', call. = FALSE)
    }


  })

  close(con)

  class(dat) <- 'mf6grid'
  return(dat)
}


# TODO kstpkper, matrix with columns kper and kstp, negative values means the last period/step

#' Helper function to read a MODFLOW 6 binary output variable
#'
#' @param file path to file with MODFLOW 6 binary output of dependent variable
#' @param grid mf6grid object
#' @param totim optional numeric vector with simulation times at which output is to be read
#' @param set_na logical, should dry cells and inactive cells be set to NA? Defaults to TRUE
#' @param text character with text as specified in the output file. "HEAD" or "CONCENTRATION"
#'
#' @return Array with the output values. The dimensions are NROW NCOL NLAY NSTPS for DIS,
#'    NCPL NLAY NSTPS for DISV and NCELLS NSTPS for DISU
#' @noRd
#'
#' @examples
read_dep_output_variable <- function(file, grid, totim = NULL, set_na = TRUE, text) {
  f <- file
  con <- file(f, open = 'rb')
  vars <- NULL
  inp_totim <- totim

  try({
    if(grid$gridtype == 'DIS') {
      # list with a matrix per time step
      vars <- list()

      # time attributes
      kstp <- kper <- vector('integer')
      pertim <- totim <- vector('numeric')

      # read first control record
      ikstp <- readBin(con, what = 'integer', n = 1)
      ikper <- readBin(con, what = 'integer', n = 1)
      ipertim <- readBin(con, what = 'double', n = 1)
      itotim <- readBin(con, what = 'double', n = 1)
      txt <- readChar(con, nchars = 16)

      # set-up loop, create empty array, as long as TEXT is present, continue loop
      i <- 0
      m <- array(NA, dim = c(grid$nrow, grid$ncol, grid$nlay))

      while(length(trimws(txt)) != 0) {

        # only read when requested TEXT is present in file
        if(toupper(trimws(txt)) != toupper(text)) {
          stop('Output file does not contain variable ', text, call. = FALSE)
        }

        # read unwanted variables (NCOL and NLAY), then read ILAY
        rc2 <- readBin(con, what = 'integer', n = 2)
        ilay <- readBin(con, what = 'integer')

        # if new time step is given, as denoted by ilay = 1, save output of previous step to vars if read_vars was TRUE
        # then append metadata, create new array
        # don't do this for very first time step (i = 0)
        save_var <- ifelse(ilay == 1 && i > 0, read_var, FALSE)

        if(save_var) {
          vars[[length(vars) + 1]] <- m

          kstp[length(kstp) + 1] <- timeattrs$ikstp
          kper[length(kper) + 1] <- timeattrs$ikper
          pertim[length(pertim) + 1] <- timeattrs$ipertim
          totim[length(totim) + 1] <- timeattrs$itotim
          m <- array(NA, dim = c(grid$nrow, grid$ncol, grid$nlay))
        }

        # check if new time step has to be read, and update read_var
        read_var <- is.null(inp_totim) || (itotim %in% inp_totim)

        # save timeattrs here, so they aren't overwritten at the final record
        timeattrs <- list(ikstp = ikstp, ikper = ikper, ipertim = ipertim, itotim = itotim)

        # increment loop contour, only matters that it is > 0
        i <- i + 1

        if(read_var) {
          # read values for this layer and time step
          vls <- readBin(con, what = 'double', n = grid$ncol * grid$nrow)
          im <- matrix(vls, nrow = grid$nrow, byrow = TRUE) # save in temp matrix
          m[,,ilay] <- c(im)

        } else {
          # skip reading values for this layer and time step
          invisible(readBin(con, what = 'double', n = grid$ncol * grid$nrow))
        }

        # read next control record
        ikstp <- readBin(con, what = 'integer', n = 1)
        ikper <- readBin(con, what = 'integer', n = 1)
        ipertim <- readBin(con, what = 'double', n = 1)
        itotim <- readBin(con, what = 'double', n = 1)

        # read new text, loop terminates when text is no longer present
        txt <- readChar(con, nchars = 16)

      }

      # add final time step to vars list based on read_var
      if(read_var) {
        vars[[length(vars) + 1]] <- m

        kstp[length(kstp) + 1] <- timeattrs$ikstp
        kper[length(kper) + 1] <- timeattrs$ikper
        pertim[length(pertim) + 1] <- timeattrs$ipertim
        totim[length(totim) + 1] <- timeattrs$itotim
      }

      # bind all time steps and set attributes
      vars <- abind::abind(vars, along = 4)
      attr(vars, 'dimnames') <- NULL
      attr(vars, 'kstp') <- kstp
      attr(vars, 'kper') <- kper
      attr(vars, 'pertim') <- pertim
      attr(vars, 'totim') <- totim

    } else if(grid$gridtype == 'DISV') {
      # list with a matrix per time step
      vars <- list()

      # time attributes
      kstp <- kper <- vector('integer')
      pertim <- totim <- vector('numeric')

      # read first control record
      ikstp <- readBin(con, what = 'integer', n = 1)
      ikper <- readBin(con, what = 'integer', n = 1)
      ipertim <- readBin(con, what = 'double', n = 1)
      itotim <- readBin(con, what = 'double', n = 1)
      txt <- readChar(con, nchars = 16)

      # set-up loop, create empty array, as long as TEXT is present, continue loop
      i <- 0
      m <- matrix(NA, nrow = grid$ncpl, ncol = grid$nlay)

      while(length(trimws(txt)) != 0) {

        # only read when requested TEXT is present in file
        if(toupper(trimws(txt)) != toupper(text)) {
          stop('Output file does not contain variable ', text, call. = FALSE)
        }

        # read unwanted variables (NCPL & 1), then read ILAY
        rc2 <- readBin(con, what = 'integer', n = 2)
        ilay <- readBin(con, what = 'integer')

        # if new time step is given, as denoted by ilay = 1, save output of previous step to vars if read_vars was TRUE
        # then append metadata, create new array
        # don't do this for very first time step (i = 0)
        save_var <- ifelse(ilay == 1 && i > 0, read_var, FALSE)

        if(save_var) {
          vars[[length(vars) + 1]] <- m

          kstp[length(kstp) + 1] <- timeattrs$ikstp
          kper[length(kper) + 1] <- timeattrs$ikper
          pertim[length(pertim) + 1] <- timeattrs$ipertim
          totim[length(totim) + 1] <- timeattrs$itotim
          m <- matrix(NA, nrow = grid$ncpl, ncol = grid$nlay)
        }

        # check if new time step has to be read, and update read_var
        read_var <- is.null(inp_totim) || (itotim %in% inp_totim)

        # save timeattrs here, so they aren't overwritten at the final record
        timeattrs <- list(ikstp = ikstp, ikper = ikper, ipertim = ipertim, itotim = itotim)

        # increment loop contour, only matters that it is > 0
        i <- i + 1

        if(read_var) {
          # read values for this layer and time step
          vls <- readBin(con, what = 'double', n = grid$ncpl)
          m[,ilay] <- vls
        } else {
          # skip reading values for this layer and time step
          invisible(readBin(con, what = 'double', n = grid$ncpl))
        }

        # read next control record
        ikstp <- readBin(con, what = 'integer', n = 1)
        ikper <- readBin(con, what = 'integer', n = 1)
        ipertim <- readBin(con, what = 'double', n = 1)
        itotim <- readBin(con, what = 'double', n = 1)

        # read new text, loop terminates when text is no longer present
        txt <- readChar(con, nchars = 16)

      }

      # add final time step to vars list based on read_var
      if(read_var) {
        vars[[length(vars) + 1]] <- m

        kstp[length(kstp) + 1] <- timeattrs$ikstp
        kper[length(kper) + 1] <- timeattrs$ikper
        pertim[length(pertim) + 1] <- timeattrs$ipertim
        totim[length(totim) + 1] <- timeattrs$itotim
      }

      # bind all time steps and set attributes
      vars <- abind::abind(vars, along = 3)
      attr(vars, 'dimnames') <- NULL
      attr(vars, 'kstp') <- kstp
      attr(vars, 'kper') <- kper
      attr(vars, 'pertim') <- pertim
      attr(vars, 'totim') <- totim

    } else if(grid$gridtype == 'DISU') {
      stop('Reading output from a DISU grid not yet supported', call. = FALSE)
    } else {
      stop('Unknown grid type', call. = FALSE)
    }

    # set na if required
    na_value <- 1e30
    if(set_na)  vars[abs(vars) == na_value] <- NA
  })

  close(con)

  return(vars)
}


#' Read a MODFLOW 6 output head file
#'
#' [read_head()] reads a MODFLOW 6 output head file
#'
#' @param file path to the output file containing the simulated heads
#' @param grid mf6grid object
#' @param totim optional numeric vector with simulation times at which output is to be read
#' @param set_na logical, should dry and inactive cells be set to NA? Defaults to TRUE
#'
#' @return Array with the head values. The dimensions are NROW NCOL NLAY NSTPS for DIS,
#'    NCPL NLAY NSTPS for DISV and NCELLS NSTPS for DISU
#' @export
#'
#' @examples
read_heads <- function(file, grid, totim = NULL, set_na = TRUE) {
  hds <- read_dep_output_variable(file, grid = grid, totim = totim, set_na = set_na, text = 'HEAD')
  return(hds)
}

#' Read a MODFLOW 6 output concentration file
#'
#' [read_concentration()] reads a MODFLOW 6 output concentration file
#'
#' @param file path to the output file containing the simulated concentrations
#' @param grid mf6grid object
#' @param totim optional numeric vector with simulation times at which output is to be read
#' @param set_na logical, should dry and inactive cells be set to NA? Defaults to TRUE
#'
#' @return Array with the concentration values. The dimensions are NROW NCOL NLAY NSTPS for DIS,
#'    NCPL NLAY NSTPS for DISV and NCELLS NSTPS for DISU
#'
#' @examples
read_concentrations <- function(file, grid, totim = NULL, set_na = TRUE) {
  conc <- read_dep_output_variable(file, grid = grid, totim = totim, set_na = set_na, text = 'CONCENTRATION')
  return(conc)
}

# grid is not yet used in this
# TODO cast arrays to model grid dimensions? Yes
# TODO when skipping data, perhaps better to use invisible(readBin(con, what = 'raw', n = nbytes_to_skip)) or seek() ?
# TODO only read certain time steps (kper, kstp), (totim) or (nstp, requires tdis info)
read_cellbudget <- function(file, grid, fluxes = 'all', totim = NULL) {
  f <- file
  con <- file(f, open = 'rb')
  cbc <- NULL
  fluxes <- tolower(fluxes)
  inp_totim <- totim

  try({
    # list with a matrix per time step
    cbc <- list()

    # read first control record
    kstp <- readBin(con, what = 'integer', n = 1)
    kper <- readBin(con, what = 'integer', n = 1)
    txt <- readChar(con, nchars = 16)
    flux <- trimws(tolower(txt))
    save_flux <- fluxes[1] == 'all' | flux %in% fluxes

    ndim1 <- readBin(con, what = 'integer', n = 1)
    ndim2 <- readBin(con, what = 'integer', n = 1)
    ndim3 <- readBin(con, what = 'integer', n = 1)

    # read second contol record
    imeth <- readBin(con, what = 'integer', n = 1)
    delt <- readBin(con, what = 'double', n = 1)
    pertim <- readBin(con, what = 'double', n = 1)
    totim <- readBin(con, what = 'double', n = 1)

    # loop for as long as records are available
    i <- 0
    while(length(flux) != 0) {

      if(!is.null(inp_totim) & !(totim %in% inp_totim)) save_flux <- FALSE

      if(imeth == 1) {
        if(save_flux) {
          dat <- readBin(con, what = 'double', n = prod(ndim1, ndim2, abs(ndim3)))
          dat <- matrix(dat, ncol = 1)
          # colnames(dat) <- flux
        } else {
          invisible(readBin(con, what = 'double', n = prod(ndim1, ndim2, abs(ndim3))))
        }


      } else if(imeth == 6) {
        if(save_flux) {
          txt1id1 <- readChar(con, nchars = 16) # GWF Model name
          txt1id2 <- readChar(con, nchars = 16) # Mostly GWF model name
          modelname <- readChar(con, nchars = 16) # Also GWF model name
          pckgname <- readChar(con, nchars = 16) # Package name
          modelname <- trimws(modelname)
          pckgname <- trimws(pckgname)

          ndat <- readBin(con, what = 'integer', n = 1)
          auxs <- vector('character', length = ndat - 1)
          if(length(auxs) > 0) {
            for(n in seq(length(auxs))) auxs[n] <- readChar(con, nchars = 16)
          }
          nlist <- readBin(con, what = 'integer', n = 1)

          id1 <- NULL # cell-id
          id2 <- NULL # bound number most of the time
          dat <- matrix(NA, nrow = nlist, ncol = ndat)

          if(nlist > 0) { # only read when nlist > 0, i.e. when data is saved to CBC
            for(n in seq(nlist)) {
              id <- readBin(con, what = 'integer', n = 2)
              id1 <- c(id1, id[1])
              id2 <- c(id2, id[2])
              vls <- readBin(con, what = 'double', n = ndat)
              dat[n,] <- vls
            }

            # handle certain flow types
            # and set attributes
            if(flux == 'data-spdis') {
              dat <- dat[,2:4]
              colnames(dat) <- tolower(trimws(auxs))
              pckgname <- flux

            } else if(flux == 'data-sat') {
              dat <- dat[,1, drop = FALSE]
              colnames(dat) <- flux
              pckgname <- flux

            } else if(ndat > 1) {
              colnames(dat) <- c(flux, tolower(trimws(auxs)))
            } else if(ndat == 1) {
              colnames(dat) <- flux
            }

            id_matrix <- cbind('id' = id1, 'id2' = id2)
            dat <- cbind(id_matrix, dat)

          }

        # save_flux <- FALSE # don't save when empty, TODO check this
        } else {
          invisible(readChar(con, nchars = 16))
          invisible(readChar(con, nchars = 16))
          invisible(readChar(con, nchars = 16))
          invisible(readChar(con, nchars = 16))
          ndat <- readBin(con, what = 'integer', n = 1)
          if((ndat - 1) > 0) {
            for(n in seq((ndat - 1))) invisible(readChar(con, nchars = 16))
          }
          nlist <- readBin(con, what = 'integer', n = 1)

          if(nlist > 0) {
            for(n in seq(nlist)) {
              invisible(readBin(con, what = 'integer', n = 2))
              invisible(readBin(con, what = 'double', n = ndat))
            }
          }
        }

      } else {
        stop('imeth value of ', imeth, ' not recognized', call. = FALSE)
      }

      # set only required fluxes in cbc list
      if(save_flux) {
        if(imeth == 1) { # does not have modelname

          if(!(flux %in% names(cbc))) {

            attr(dat, 'kstp') <- kstp
            attr(dat, 'kper') <- kper
            attr(dat, 'delt') <- delt
            attr(dat, 'pertim') <- pertim
            attr(dat, 'totim') <- totim
            cbc[[flux]] <- dat

          } else {
            # cols <- colnames(dat)
            # dims <- length(dim(dat))

            olddat <- cbc[[flux]]
            ikstp <- attr(olddat, 'kstp')
            ikper <- attr(olddat, 'kper')
            idelt <- attr(olddat, 'delt')
            ipertim <- attr(olddat, 'pertim')
            itotim <- attr(olddat, 'totim')
            #
            # olddat <- abind::abind(olddat, dat, along = dims + 1)
            # dimnames(olddat) <- NULL
            # if(dims == 1) {
            #   colnames(olddat) <- c(cols, 'nstp')
            # } else {
            #   colnames(olddat) <- cols
            # }

            olddat <- cbind(olddat, dat)
            attr(olddat, 'kstp') <- c(ikstp, kstp)
            attr(olddat, 'kper') <- c(ikper, kper)
            attr(olddat, 'delt') <- c(idelt, delt)
            attr(olddat, 'pertim') <- c(ipertim, pertim)
            attr(olddat, 'totim') <- c(itotim, totim)
            cbc[[flux]] <- olddat
          }

        } else if(imeth == 6) { # binds dataframes
          # TODO better to bind matrices and give modelname + packagename as attribute?
          # What would then be the name in the cbc list object?

          dat <- as.data.frame(dat)
          dat$kstp <- kstp
          dat$kper <- kper
          dat$delt <- delt
          dat$pertim <- pertim
          dat$totim <- totim
          dat$modelname <- modelname
          dat$packagename <- pckgname

          if(!(flux %in% names(cbc))) {
            if(!requireNamespace('tibble', quietly = TRUE)) {
              cbc[[flux]] <- dat
            } else {
              cbc[[flux]] <- tibble::as_tibble(dat)
            }
          } else {
            cbc[[flux]] <- rbind(cbc[[flux]], dat)
          }

        }
      }

      # read new control record
      i <- i + 1
      kstp <- readBin(con, what = 'integer', n = 1)
      kper <- readBin(con, what = 'integer', n = 1)
      txt <- readChar(con, nchars = 16)
      flux <- trimws(tolower(txt))
      save_flux <- fluxes[1] == 'all' | flux %in% fluxes

      ndim1 <- readBin(con, what = 'integer', n = 1)
      ndim2 <- readBin(con, what = 'integer', n = 1)
      ndim3 <- readBin(con, what = 'integer', n = 1)

      # read second control record
      imeth <- readBin(con, what = 'integer', n = 1)
      delt <- readBin(con, what = 'double', n = 1)
      pertim <- readBin(con, what = 'double', n = 1)
      totim <- readBin(con, what = 'double', n = 1)

    }
  })

  close(con)
  return(cbc)
}

# cbc <- read_cellbudget(fbudget, grid)
#
# cbc <- read_cellbudget(fbudget2, grid, fluxes='data-spdis')



#' Convert a MODFLOW 6 array to a tibble
#'
#' [as_tibble()] converts a MODFLOW 6 array to a tibble, either from the cell centers or the cell polygons
#'
#' @param array an array with dimensions 1D, 2D or 3D (DISV) or 2D, 3D or 4D (DIS)
#' @param grid mf6grid object
#' @param ilay layer integer specifying which layer to subset
#' @param kstp time step integer specifying which time step to subset
#' @param as_points logical, should a tibble with cell center coordinates (TRUE) or cell vertices (FALSE; default) be returned?
#'
#' @return A tibble with columns id (cell id), x and y (coordinates) and value
#' @export
#'
#' @examples
#'
as_tibble <- function(array, grid, ilay, kstp, as_points = FALSE) {

  ndim <- length(dim(array))

  if(grid$gridtype == 'DIS') {

    # subset array
    if(ndim == 4) {
      if(dim(array)[4] == 1) kstp <- 1 # if only 1 time step, use that
      array <- array[,,ilay, kstp]
    } else if(ndim == 3) {
      array <- array[,,ilay]
    } else if(ndim < 2 | ndim > 4) {
      stop('Please supply a 2D, 3D or 4D array for DIS grids', call. = FALSE)
    } else {
      ilay <- 1
    }

    # set up grid
    # from RMODFLOW::rmf_as_tibble.rmf_2d_array
    xy <- expand.grid(sum(grid$delc) - (cumsum(grid$delc) - grid$delc/2), cumsum(grid$delr) - grid$delr/2)
    names(xy) <- c('y','x')

    # TODO rotation
    xy$x <- xy$x + grid$xorigin
    xy$y <- xy$y + grid$yorigin

    # TODO mask
    mask <- array*0 + 1
    mask[which(mask == 0)] <- NA

    # TODO modflow cell id
    ids <- seq(grid$nrow * grid$ncol) + (ilay - 1) * grid$nrow * grid$ncol

    if(as_points) {
      positions <- data.frame(id = ids, x = xy$x, y = xy$y)
      values <- data.frame(id = ids, value = c(array*mask^2))
    } else {
      xWidth <- rep(grid$delr, each = grid$nrow)
      yWidth <- rep(grid$delc, grid$ncol)
      positions <- data.frame(id = rep(ids, each=4), x = rep(xy$x, each = 4), y = rep(xy$y, each = 4))
      positions$x[(seq(1, nrow(positions), 4))] <- positions$x[(seq(1, nrow(positions), 4))] - xWidth/2
      positions$x[(seq(2, nrow(positions), 4))] <- positions$x[(seq(2, nrow(positions), 4))] - xWidth/2
      positions$x[(seq(3, nrow(positions), 4))] <- positions$x[(seq(3, nrow(positions), 4))] + xWidth/2
      positions$x[(seq(4, nrow(positions), 4))] <- positions$x[(seq(4, nrow(positions), 4))] + xWidth/2
      positions$y[(seq(1, nrow(positions), 4))] <- positions$y[(seq(1, nrow(positions), 4))] - yWidth/2
      positions$y[(seq(2, nrow(positions), 4))] <- positions$y[(seq(2, nrow(positions), 4))] + yWidth/2
      positions$y[(seq(3, nrow(positions), 4))] <- positions$y[(seq(3, nrow(positions), 4))] + yWidth/2
      positions$y[(seq(4, nrow(positions), 4))] <- positions$y[(seq(4, nrow(positions), 4))] - yWidth/2
      values <- data.frame(id = ids, value = c(array*mask^2))
    }
    tbl <- tibble::as_tibble(merge(values, positions, by = c("id")))

  } else if(grid$gridtype == 'DISV') {

    # subset array
    if(ndim == 3) {
      if(dim(array)[3] == 1) kstp <- 1 # if only 1 time step, use that
      array <- array[,ilay, kstp]
    } else if(ndim == 2) {
      array <- array[,ilay]
    } else if(ndim > 2) {
      stop('Please supply a 1D, 2D or 3D grid for DISV models', call. = FALSE)
    } else {
      ilay <- 1
    }

    if(as_points) {
      tbl <- tibble::tibble(id = seq(grid$ncpl) + (ilay - 1) * grid$ncpl,
                            value = c(array),
                            x = grid$cellx + grid$xorigin,
                            y = grid$celly + grid$yorigin)
    } else {
      x <- y <- nvert <- NULL
      for(i in seq(grid$ncpl)) {
        nvert <- c(nvert, grid$iavert[i+1] - grid$iavert[i])
        # find vertex numbers
        vert <- seq(grid$iavert[i], grid$iavert[i+1] - 1)
        vertj <- grid$javert[vert]
        xVert <- grid$vertices[vertj, 1]
        yVert <- grid$vertices[vertj, 2]
        x <- c(x, xVert)
        y <- c(y, yVert)
      }

      tbl <- tibble::tibble(id = rep(seq(grid$ncpl) + (ilay - 1) * grid$ncpl, nvert),
                            value = rep(c(array), nvert),
                            x = x + grid$xorigin,
                            y = y + grid$yorigin)
    }

  } else if(grid$gridtype == 'DISU') {
    stop('Functions for a DISU grid not yet supported', call. = FALSE)
  } else {
    stop('Unknown grid type', call. = FALSE)
  }

  return(tbl)
}



#' Convert a MODFLOW 6 array to a tibble along a cross-section
#'
#' [as_tibble_Cross()] converts a MODFLOW 6 array to a tibble along a vertical
#' cross-section, either from the cell centers or the cell polygons
#'
#' @param array an array with dimensions 2D or 3D (DISV) or 3D or 4D (DIS)
#' @param i integer row index for a DIS grid to get the cross-section
#' @param j integer column index for a DIS grid to get the cross-section
#' @param xy matrix with two columns containing the x and y coordinates a cross-section line. Overwrites i and j for a DIS grid.
#' @param grid mf6grid object
#' @param kstp time step integer specifying which time step to subset
#' @param as_points logical, should a tibble with cell center coordinates (TRUE)
#'   or cell vertices (FALSE; default) be returned?
#' @param limit_wt logical, should the top of the cells be limited by the water table as computed from array? Defaults to `FALSE`.
#'
#' @return A tibble with columns id (cell id), x, y and z (coordinates), value and top and bottom elevation of the cell
#' @export
#'
#' @examples
as_tibble_cross <- function(array, grid, i, j, xy = NULL, kstp, as_points = FALSE, limit_wt = FALSE) {

  ndim <- length(dim(array))

  if(grid$gridtype == 'DIS') {

    # subset array
    if(ndim == 4) {
      if(dim(array)[4] == 1) kstp <- 1 # if only 1 time step, use that
      dim_org = dim(array)
      array <- array[,,, kstp, drop = FALSE]
      dim(array) <- dim_org[1:3]
    } else if(ndim <= 2 | ndim > 4) {
      stop('Please supply a 3D or 4D array for DIS grids', call. = FALSE)
    }

    if(limit_wt) { # TODO check this for nlay > 1
      wt <- at_water_table(array, grid)
      grid$top <- ifelse(grid$top > wt, wt, grid$top)
      if(grid$nlay > 1) {
        grid$botm[,,1:(grid$nlay-1)] <- ifelse(array[,,2:(grid$nlay)] < grid$botm[,,1:(grid$nlay-1)], array[,,2:(grid$nlay)], grid$botm[,,1:(grid$nlay-1)])
      }
    }

    if(is.null(xy)) {
      if((missing(i) + missing(j)) %in% c(0, 2)) stop('Specify either xy, i or j for a DIS grid', call. = FALSE)

      # set up grid
      # from RMODFLOW::rmf_as_tibble.rmf_3d_array
      xy <- NULL
      xy$x <- cumsum(grid$delr)-grid$delr/2
      rvdelc <- rev(grid$delc)
      xy$y <- rev(cumsum(rvdelc)-rvdelc/2) # reverse since plotting should be along increasing y axis

      # TODO rotation
      xy$x <- xy$x + grid$xorigin
      xy$y <- xy$y + grid$yorigin

      # TODO mask
      mask <- array*0 + 1
      mask[which(mask == 0)] <- NA

      # tops and centers
      tops <- array(0, dim = c(grid$nrow, grid$ncol, grid$nlay))
      tops[,,1] <- grid$top
      if(grid$nlay > 1) tops[,,2:grid$nlay] = grid$botm[,,1:(grid$nlay - 1)]
      thck <- tops - grid$botm
      centers <- grid$botm + thck / 2

      if(missing(i)) {
        # cross-section along column y-z

        ids <- seq(grid$nrow) + (grid$nrow * (j-1))
        if(grid$nlay > 1) ids <- rep(ids, grid$nlay) + c(rep(0, grid$nrow), rep(prod(grid$nrow, grid$ncol) * seq_len(grid$nlay - 1), each = grid$nrow))

        # x-values
        # TODO rotation
        cst_values <- xy$x[j]

        if(as_points) {
          positions <- data.frame(id = ids, x = xy$y, y = c(centers[,j,]), z = cst_values)
          values <- data.frame(id = ids, value = c((array[,j,]*mask[,j,]^2)))
        } else {
          xWidth <- rep(grid$delc,grid$nlay)
          yWidth <- thck[,j,]
          positions <- data.frame(id = rep(ids, each=4),x=rep(xy$y,each=4),y=rep(centers[,j,],each=4),z=rep(cst_values, each = 4))
          positions$x[(seq(1,nrow(positions),4))] <- positions$x[(seq(1,nrow(positions),4))] - xWidth/2
          positions$x[(seq(2,nrow(positions),4))] <- positions$x[(seq(2,nrow(positions),4))] - xWidth/2
          positions$x[(seq(3,nrow(positions),4))] <- positions$x[(seq(3,nrow(positions),4))] + xWidth/2
          positions$x[(seq(4,nrow(positions),4))] <- positions$x[(seq(4,nrow(positions),4))] + xWidth/2
          positions$y[(seq(1,nrow(positions),4))] <- positions$y[(seq(1,nrow(positions),4))] - yWidth/2
          positions$y[(seq(2,nrow(positions),4))] <- positions$y[(seq(2,nrow(positions),4))] + yWidth/2
          positions$y[(seq(3,nrow(positions),4))] <- positions$y[(seq(3,nrow(positions),4))] + yWidth/2
          positions$y[(seq(4,nrow(positions),4))] <- positions$y[(seq(4,nrow(positions),4))] - yWidth/2
          values <- data.frame(id = ids,value = c((array[,j,]*mask[,j,]^2)))
        }

        tbl <- tibble::as_tibble(merge(values, positions, by = c("id")))
        colnames(tbl) <- replace(colnames(tbl), match(c('x', 'y', 'z'), colnames(tbl)), c('y', 'z', 'x'))
        tbl <- tbl[,c('id', 'value', 'x', 'y', 'z')]

      } else if(missing(j)) {
        # cross-section along row x-z

        ids <- i + c(0, cumsum(rep(grid$nrow, grid$ncol))[-grid$ncol])
        if(grid$nlay > 1) ids <- rep(ids, grid$nlay) + c(rep(0, grid$ncol), rep(prod(grid$nrow, grid$ncol) * seq_len(grid$nlay - 1), each = grid$ncol))

        # y-values
        # TODO rotation
        cst_values <- xy$y[i]

        if(as_points) {
          positions <- data.frame(id = ids, x = xy$x, y = c(centers[i,,]), z = cst_values)
          values <- data.frame(id = ids, value = c((array[i,,]*mask[i,,]^2)))
        } else {
          xWidth <- rep(grid$delr,grid$nlay)
          yWidth <- thck[i,,]
          positions <- data.frame(id = rep(ids, each=4),x=rep(xy$x,each=4),y=rep(centers[i,,],each=4),z=rep(cst_values, each=4))
          positions$x[(seq(1,nrow(positions),4))] <- positions$x[(seq(1,nrow(positions),4))] - xWidth/2
          positions$x[(seq(2,nrow(positions),4))] <- positions$x[(seq(2,nrow(positions),4))] - xWidth/2
          positions$x[(seq(3,nrow(positions),4))] <- positions$x[(seq(3,nrow(positions),4))] + xWidth/2
          positions$x[(seq(4,nrow(positions),4))] <- positions$x[(seq(4,nrow(positions),4))] + xWidth/2
          positions$y[(seq(1,nrow(positions),4))] <- positions$y[(seq(1,nrow(positions),4))] - yWidth/2
          positions$y[(seq(2,nrow(positions),4))] <- positions$y[(seq(2,nrow(positions),4))] + yWidth/2
          positions$y[(seq(3,nrow(positions),4))] <- positions$y[(seq(3,nrow(positions),4))] + yWidth/2
          positions$y[(seq(4,nrow(positions),4))] <- positions$y[(seq(4,nrow(positions),4))] - yWidth/2
          values <- data.frame(id = ids,value = c((array[i,,]*mask[i,,]^2)))
        }

        tbl <- tibble::as_tibble(merge(values, positions, by = c("id")))
        colnames(tbl) <- replace(colnames(tbl), match(c('x', 'y', 'z'), colnames(tbl)), c('x', 'z', 'y'))
        tbl <- tbl[,c('id', 'value', 'x', 'y', 'z')]
      }

      # add top & botm
      tbl$top <- tops[tbl$id]
      tbl$botm <- grid$botm[tbl$id]
      # tbl$top <- (length_mlt * tops[tbl$id]) + z_ref
      # tbl$botm <- (length_mlt * botm[tbl$id]) + z_ref

    } else {
      # TODO, requires converting to spatial, creating a sfg line from xy and using st_intersect to get cell id's
      stop('xy cross-sections not yet implemented')

    }


  } else if(grid$gridtype == 'DISV') {
    if(is.null(xy)) stop('Cross-sections for DISV grids require an "xy" argument', call. = FALSE)

    # TODO, requires converting to spatial, creating a sfg line from xy and using st_intersect to get cell id's
    stop('xy cross-sections not yet implemented')

  } else if(grid$gridtype == 'DISU') {
    stop('Functions for a DISU grid not yet supported', call. = FALSE)
  } else {
    stop('Unknown grid type', call. = FALSE)
  }

  return(tbl)
}

#' Get the values of an array at the water-table
#'
#' [at_water_table()] obtains the first non-NA value along a vertical column in the array,
#'    or based on another array
#'
#' @param array array with values
#' @param grid mf6grid object
#' @param heads optional secondary array containing the water-table elevations
#' @param kstp time step integer required for subsetting arrays with a time-dimensions
#'
#' @return The array without the final dimension with the values in the uppermost active cells (first non-NA value along vertical column)
#' @export
#'
#' @examples
at_water_table <- function(array, grid, heads = NULL, kstp) {

  ndim <- length(dim(array))

  if(grid$gridtype == 'DIS') {

    # subset array
    if(ndim == 4) {
      if(dim(array)[4] == 1) kstp <- 1 # if only 1 time step, use that
      array <- array[,,,kstp]
    } else if(ndim <= 2 | ndim > 4) {
      stop('Please supply a 3D or 4D array for DIS grids', call. = FALSE)
    }

    if(is.null(heads)) {
      heads <- array
    } else {
      ndim <- length(dim(heads))

      # subset heads
      if(ndim == 4) {
        if(dim(heads)[4] == 1) kstp <- 1 # if only 1 time step, use that
        heads <- heads[,,,kstp]
      } else if(ndim <= 2 | ndim > 4) {
        stop('Please supply a 3D or 4D heads for DIS grids', call. = FALSE)
      }

      if(any(dim(heads) != dim(array))) stop('Dimensions of array and heads should be the same')
    }

    ncpl <- grid$nrow * grid$ncol
    ids <- apply(heads, c(1, 2), function(i) which(!is.na(i))[1])
    ids <- seq(ncpl) + (ids - 1) * ncpl
    wt <- matrix(c(array[ids]), nrow = grid$nrow, ncol = grid$ncol)

  } else if(grid$gridtype == 'DISV') {

    # subset array
    if(ndim == 3) {
      if(dim(array)[3] == 1) kstp <- 1 # if only 1 time step, use that
      array <- array[,, kstp]
    } else if(ndim > 2) {
      stop('Please supply a 2D or 3D grid for DISV models', call. = FALSE)
    }

    if(is.null(heads)) {
      heads <- array
    } else {
      ndim <- length(dim(heads))

      # subset heads
      if(ndim == 3) {
        if(dim(heads)[3] == 1) kstp <- 1 # if only 1 time step, use that
        heads <- heads[,, kstp]
      } else if(ndim > 2) {
        stop('Please supply a 2D or 3D grid for DISV models', call. = FALSE)
      }

      if(any(dim(heads) != dim(array))) stop('Dimensions of array and heads should be the same')
    }

    ncpl <- grid$ncpl
    ids <- apply(heads, 1, function(i) which(!is.na(i))[1])
    ids <- seq(ncpl) + (ids - 1) * ncpl
    wt <- array[ids]

  } else if(grid$gridtype == 'DISU') {
    stop('Functions for a DISU grid not yet supported', call. = FALSE)
  } else {
    stop('Unknown grid type', call. = FALSE)
  }

  return(wt)
}

#' Convert MODFLOW arrays to spatial vector data
#'
#' @param array a 1D (DISV) or 2D (DIS) array
#' @param grid mf6grid object
#' @param crs optional crs information as passed to [sf::st_set_crs()]. Defaults to NA.
#' @param as_points logical, should cell centers be returned (POINTS) or cell polygons (POLYGON, default)?
#'
#' @return A sf object of the array in POINT (`as_points = TRUE`) or POLYGON (`as_points = FALSE`) geometry with
#'     additional columns id (cell id) and value.
#' @export
#'
#' @examples
as_spatial <- function(array, grid, crs = NA, as_points = FALSE) {

  ndim <- length(dim(array))

  if(!requireNamespace('sf', quietly = TRUE)) {
    stop('Please install "sf" when using spatial functionality', call. = FALSE)
  }

  if(grid$gridtype == 'DIS') {
    if(ndim > 2 | ndim < 2) stop('Only array of dimension NROW by NCOL are supported for DIS grids', call. = FALSE)
    tbl <- as_tibble(array, grid, as_points = as_points)

    if(as_points) {
      shp <- sf::st_as_sf(tbl, coords = c('x', 'y'), crs = crs)
    } else {
      shp <- sfheaders::sf_polygon(tbl, x = 'x', y = 'y', polygon_id = 'id', keep = TRUE) # fast
      shp <- sf::st_set_crs(shp, crs)
    }

  } else if(grid$gridtype == 'DISV') {
    if(ndim > 1) stop('Only array of dimension NCPL are supported for DISV grids', call. = FALSE)
    tbl <- as_tibble(array, grid, as_points = as_points)

    if(as_points) {
      shp <- sf::st_as_sf(tbl, coords = c('x', 'y'), crs = crs)
    } else {
      shp <- sfheaders::sf_polygon(tbl, x = 'x', y = 'y', polygon_id = 'id', keep = TRUE) # fast
      shp <- sf::st_set_crs(shp, crs)
    }

  } else if(grid$gridtype == 'DISU') {
    stop('Functions for a DISU grid not yet supported', call. = FALSE)
  } else {
    stop('Unknown grid type', call. = FALSE)
  }
}


#' Resample values on a MODFLOW grid to a rectilinear output grid
#'
#' @param array  a 1D (DISV) or 2D (DIS) array
#' @param grid mf6grid object
#' @param xout numeric vector with marginal x-coordinates of output grid
#' @param yout numeric vector with marginal y-coordinates of output grid
#'
#' @return Matrix with interpolated values onto the output grid.
#' @export
#'
#' @examples
resample_to_grid <- function(array, grid, xout, yout) {
  ndim <- length(dim(array))

  if(grid$gridtype == 'DIS') {
    if(ndim > 2 | ndim < 2) stop('Only array of dimension NROW by NCOL are supported for DIS grids', call. = FALSE)
    obj <- list(x = (cumsum(grid$delr) - grid$delr/2) + grid$xorigin,
                y = (grid$yorigin + sum(grid$delc)) - (cumsum(grid$delc) - grid$delc/2),
                z = t(array))

    grid.list <- list(x = xout, y = yout)
    out <- fields::interp.surface.grid(obj, grid.list)
    gr <- t(out$z[, dim(out$z)[2]:1])

  } else if(grid$gridtype == 'DISV') {
    if(ndim > 1) stop('Only array of dimension NCPL are supported for DISV grids', call. = FALSE)

    x <- grid$cellx + grid$xorigin
    y <- grid$celly + grid$yorigin
    vls <- c(array)

    is_na <- is.na(vls)
    x <- x[!is_na]
    y <- y[!is_na]
    vls <- vls[!is_na]

    out <- interp::interp(x, y, vls, xo = xout, yo = yout)
    gr <- t(out$z[, dim(out$z)[2]:1])
  } else if(grid$gridtype == 'DISU') {
    stop('Functions for a DISU grid not yet supported', call. = FALSE)
  } else {
    stop('Unknown grid type', call. = FALSE)
  }

  return(gr)
}

#' Convert MODFLOW arrays to spatial raster data
#'
#' @param array a 1D (DISV) or 2D (DIS) array
#' @param grid mf6grid object
#' @param xout numeric vector with equally spaced marginal x-coordinates of output grid
#' @param yout numeric vector with equally spaced marginal y-coordinates of output grid
#' @param delta numeric grid spacing for rectangular grid. Overwrites `xout` and `yout`.
#' @param target \code{terra SpatRaster} object to obtain the required dimensions from. Defaults to `NULL`. Overwrites `delta` and `xout` and `yout`
#' @param crs optional crs information with epsg code. Defaults to NA.
#'
#' @return An object of class \code{SpatRaster} representing the MODFLOW array
#' @export
#'
#' @examples
as_raster <- function(array, grid, xout, yout, delta = NULL, target = NULL, crs = NA) {
  if(!requireNamespace('terra', quietly = TRUE)) stop('Conversion to spatial raster requires "terra"', call. = FALSE)
  if(!is.null(target)) {
    stopifnot(inherits(target, 'SpatRaster'))
    xout <- terra::xFromCol(target, 1:ncol(target))
    yout <- terra::yFromRow(target, 1:nrow(target))
  } else if(!is.null(delta)) {
    if(grid$gridtype == 'DIS') {
      xmin <- grid$xorigin
      xmax <- grid$xorigin + sum(grid$delr)
      ymin <- grid$yorigin
      ymax <- grid$yorigin + sum(grid$delc)
      xout <- seq(xmin, xmax, by = delta)
      yout <- rev(seq(ymin, ymax, by = delta))

    } else if(grid$gridtype == 'DISV') {
      # TODO support for rotated grids
      # is origin actually origin or just an offset?
      xmin <- grid$xorigin
      xmax <- grid$xorigin + max(grid$vertices[,1])
      ymin <- grid$yorigin
      ymax <- grid$yorigin + max(grid$vertices[,2])
      xout <- seq(xmin, xmax, by = delta)
      yout <- rev(seq(ymin, ymax, by = delta))

    } else if(grid$gridtype == 'DISU') {
      stop('Functions for a DISU grid not yet supported', call. = FALSE)
    } else {
      stop('Unknown grid type', call. = FALSE)
    }
    # ext <- c(min(xout) - udx[1]/2, max(xout) + udx[1]/2, min(yout) - udy[1]/2, max(yout) + udy[1]/2)
    ext <- c(min(xout), max(xout), min(yout), max(yout))

  } else {
    xout <- sort(xout)
    yout <- sort(yout, decreasing = TRUE)
    udx <- unique(diff(xout))
    udy <- -unique(diff(yout))
    if(length(udx) > 1 || length(udy) > 1) stop('xout and yout should have equal spacing', call. = FALSE)
    # ext <- c(min(xout) - udx[1]/2, max(xout) + udx[1]/2, min(yout) - udy[1]/2, max(yout) + udy[1]/2)
    ext <- c(min(xout), max(xout), min(yout), max(yout))

  }

  # resample to target grid
  rs <- resample_to_grid(array, grid, xout, rev(yout))

  # set values and crs
  if(!is.null(target)) {
    terra::values(target) <- rs
    rs <- target
  } else {
    if(!is.na(crs)) {
      crs <- paste('epsg', crs, sep = ':')
    }
    rs <- terra::rast(rs, type = '', crs = crs, extent = terra::ext(ext))
  }
  return(rs)
}

#' Compute isolines and isobands from a MODFLOW 6 array
#'
#' [get_isolines()] computes isolines and isobands from a MODFLOW 6 array by first resampling to the desired output grid
#'
#' @param array a 1D (DISV) or 2D (DIS) array
#' @param grid mf6grid object
#' @param ngr integer vector of length 1 or 2 with the desired number of grid points in x and y directions
#' @param levels either a numeric vector with the desired contouring levels or a single value with the desired (but not exact) number of evenly spaced levels
#' @param crs optional crs information as passed to [sf::st_set_crs()]. Defaults to NA.
#' @param as_spatial logical indicating if the return object should be an \code{sf} object.
#'
#' @rdname isolines
#' @return When \code{as_spatial = TRUE}, an \code{sf} object containing the isolines as \code{MULTILINESTRING} with column \code{level}.
#' For isobands, an \code{sf} object containing the \code{POLYGON} geometries of the bands with columns \code{low} and \code{high} corresponding to the lower and upper level bounds.
#' When \code{as_spatial = FALSE}, a list object containing lists with the x and y coordinates and an id vector for each level. The names of the list elements correspond to the contouring level (isolines) or interval (isobands).
#' @export
#'
#' @details
#' The [isoband::isolines()] algorithm is used for computing the isolines; [isoband::isobands()] is used for computing isobands.
#' When {as_spatial = TRUE}, [isoband::iso_to_sfg()] is used to get the geometries. For isolines an \code{sf MULTILINESTRING} object is created; for isobands, a \code{sf POLYGON} object.
#' Isoline/isoband computation from cross-sections is not yet supported.
#'
#' @examples
get_isolines <- function(array, grid, ngr = 100, levels = 20, crs = NA, as_spatial = TRUE) {
  if(length(ngr) == 1) ngr <- rep(ngr, 2)
  ndim <- length(dim(array))
  if(!requireNamespace('isoband', quietly = FALSE)) stop('isoline computation requires the "isoband" package', call. = FALSE)
  if(as_spatial && !requireNamespace('sf', quietly = FALSE)) stop('Spatial isoline computation requires the "sf" package', call. = FALSE)

  if(grid$gridtype == 'DIS') {
    if(ndim > 2 | ndim < 2) stop('Only array of dimension NROW by NCOL are supported for DIS grids', call. = FALSE)
    # TODO support for rotated grids
    xmin <- grid$xorigin
    xmax <- grid$xorigin + sum(grid$delr)
    ymin <- grid$yorigin
    ymax <- grid$yorigin + sum(grid$delc)
    xout <- seq(xmin, xmax, length = ngr[1])
    yout <- seq(ymin, ymax, length = ngr[2])

    gr <- resample_to_grid(array, grid, xout, yout)

  } else if(grid$gridtype == 'DISV') {
    if(ndim > 1) stop('Only array of dimension NCPL are supported for DISV grids', call. = FALSE)
    # TODO support for rotated grids
    # is origin actually origin or just an offset?
    xmin <- grid$xorigin
    xmax <- grid$xorigin + max(grid$vertices[,1])
    ymin <- grid$yorigin
    ymax <- grid$yorigin + max(grid$vertices[,2])
    xout <- seq(xmin, xmax, length = ngr[1])
    yout <- seq(ymin, ymax, length = ngr[2])

    gr <- resample_to_grid(array, grid, xout, yout)

  } else if(grid$gridtype == 'DISU') {
    stop('Functions for a DISU grid not yet supported', call. = FALSE)
  } else {
    stop('Unknown grid type', call. = FALSE)
  }

  if(length(levels) == 1) levels <- pretty(range(c(array), finite = TRUE), n = levels - 1)

  lines <- isoband::isolines(x = xout,
                             y = yout,
                             z = gr[dim(gr)[1]:1,],
                             levels = levels)
  if(!as_spatial) return(lines)
  lines_sf <- sf::st_sf(level = levels,
                        geometry = isoband::iso_to_sfg(lines),
                        crs = crs)
  return(lines_sf)
}

#' @rdname isolines
#' @export
#'
get_isobands <- function(array, grid, ngr = 100, levels = 20, crs = NA, as_spatial = TRUE) {
  if(length(ngr) == 1) ngr <- rep(ngr, 2)
  ndim <- length(dim(array))
  if(!requireNamespace('isoband', quietly = FALSE)) stop('isoline computation requires the "isoband" package', call. = FALSE)
  if(as_spatial && !requireNamespace('sf', quietly = FALSE)) stop('Spatial isoline computation requires the "sf" package', call. = FALSE)

  if(grid$gridtype == 'DIS') {
    if(ndim > 2 | ndim < 2) stop('Only array of dimension NROW by NCOL are supported for DIS grids', call. = FALSE)
    # TODO support for rotated grids
    xmin <- grid$xorigin
    xmax <- grid$xorigin + sum(grid$delr)
    ymin <- grid$yorigin
    ymax <- grid$yorigin + sum(grid$delc)
    xout <- seq(xmin, xmax, length = ngr[1])
    yout <- seq(ymin, ymax, length = ngr[2])

    gr <- resample_to_grid(array, grid, xout, yout)

  } else if(grid$gridtype == 'DISV') {
    if(ndim > 1) stop('Only array of dimension NCPL are supported for DISV grids', call. = FALSE)
    # TODO support for rotated grids
    # is origin actually origin or just an offset?
    xmin <- grid$xorigin
    xmax <- grid$xorigin + max(grid$vertices[,1])
    ymin <- grid$yorigin
    ymax <- grid$yorigin + max(grid$vertices[,2])
    xout <- seq(xmin, xmax, length = ngr[1])
    yout <- seq(ymin, ymax, length = ngr[2])

    gr <- resample_to_grid(array, grid, xout, yout)

  } else if(grid$gridtype == 'DISU') {
    stop('Functions for a DISU grid not yet supported', call. = FALSE)
  } else {
    stop('Unknown grid type', call. = FALSE)
  }

  if(length(levels) == 1) levels <- pretty(range(c(array), finite = TRUE), n = levels - 1)

  bands <- isoband::isobands(x = xout,
                             y = yout,
                             z = gr[dim(gr)[1]:1,],
                             levels_low = levels[-length(levels)],
                             levels_high = levels[-1])
  if(!as_spatial) return(bands)
  bands_sf <- sf::st_sf(low = levels[-length(levels)], high = levels[-1],
                        geometry = isoband::iso_to_sfg(bands),
                        crs = crs)
  return(bands_sf)
}

#' Determine if point is inside polygon
#'
#' [point_in_polygon()] determines if a point is inside a polygon. Points on the line are treated
#'    as inside. This is an implementation of a ray-casting algorithm.
#'
#' @param points numeric matrix with columns x and y (or numeric vector of length 2 to be coerced to a matrix) containing the point coordinates
#' @param polygon matrix with columns x and y containing the vertices of the polygon. It should not be closed.
#'
#' @return A logical vector of length `nrow(points)` indicating if the given point lies inside the polygon.
#' @noRd
#'
#' @examples
#' poly <- rbind(c(-0.25, 0.25), c(0.32, 0.64), c(-0.25, 1.5), c(1, 1.5), c(0.76, 0.3),
#'               c(0.25, -0.25), c(-0.25, -0.5))
#'
#' pinside <- c(0.5, 0.5) # point inside the poly
#' poutside <- c(1.2, 1) # point outside the poly
#' pedgeV <- c(-0.25, 0) # point at vertical edge
#' pedgeS <- c(0.035, 0.445) # point at sloped edge
#' pedgeH <- c(0.5, 1.5) # point at horizontal edge
#'
#' point_in_polygon(pinside, poly)
#' point_in_polygon(poutside, poly)
#' point_in_polygon(pedgeV, poly)
#' point_in_polygon(pedgeS, poly)
#' point_in_polygon(pedgeH, poly)
#'
#' n <- 1000
#' rpts <- cbind(x = runif(n, -0.5, 1.5), y = runif(n, -1, 2))
#' io <- point_in_polygon(rpts, polygon = poly)
#' df <- cbind(as.data.frame(rpts), col = ifelse(io, 'blue', 'red'))
#'
#' plot(df$x, df$y, col = df$col, xlim = c(-0.6, 1.6), ylim = c(-1.1, 2.1))
#' polygon(poly)
#' points(rbind(pinside, poutside, pedgeV, pedgeS, pedgeH), pch = 19)
#'
point_in_polygon <- function(points, polygon) {

  points <- matrix(points, ncol = 2)
  x <- points[,1]
  y <- points[,2]
  poly_x <- polygon[,1]
  poly_y <- polygon[,2]

  inside <- rep(FALSE, nrow(points))

  # search only for points that are already inside the bounding box,
  # this should speed up searching a lot of points
  outside_x <- x > max(poly_x) | x < min(poly_x)
  outside_y <- y > max(poly_y) | y < min(poly_y)
  to_check <- which(!(outside_x & outside_y))

  if(length(to_check) > 0) {
    x <- x[to_check]
    y <- y[to_check]
    inside_to_check <- inside[to_check]
    nvert <- length(poly_x)
    j <- nvert
    for (i in 1:nvert) {
      inside_to_check <- ifelse( (((poly_y[i] >= y) != (poly_y[j] >= y)) &
                                 (x < (poly_x[j] - poly_x[i]) * (y - poly_y[i]) / (poly_y[j] - poly_y[i]) + poly_x[i])),
                                !inside_to_check,
                                inside_to_check)
      j <- i
    }
    inside[to_check] <- inside_to_check
  }

  return(inside)
}

#' Convert ijk to id for a DIS grid
#'
#' [convert_ijk_to_id()] convert row-column-layer indices for a DIS grid to cell id's using R or MODFLOW numbering
#'
#' @param i vector of row numbers
#' @param j vector of column numbers
#' @param k vector of layer numbers
#' @param grid mf6grid object
#' @param type 'r' or 'modflow' specifying type of id. See details. Defaults to 'r'
#' @param ... ignored
#' @return cell ids, providing the place of the cell in an input file 3d array
#' @details a modflow id provides the place of the number in an input file 3d array using row-major numbering; R uses column-major numbering
#' @export
convert_ijk_to_id <- function(i, j, k, grid, type = c('r', 'modflow'), ...) {

  if(grid$gridtype != 'DIS') stop('Converting ijk to id is only supported for DIS grids', call. = FALSE)
  type <- match.arg(type)
  if(type == 'r') {
    id <- (k - 1) * grid$nrow * grid$ncol + (j - 1) * grid$nrow + i
  } else if (type == 'modflow') {
    id <- (k - 1) * grid$nrow * grid$ncol + (i - 1) * grid$ncol + j
  }
  return(id)
}

#' Convert id to ijk for a DIS grid
#'
#' [convert_id_to_ijk()] converts a cell id of a DIS grid to row-column-layer indices using R or MODFLOW numbering
#'
#' @param id cell id, providing the place of the number in an input file 3d array
#' @param grid mf6grid object
#' @param type 'r' or 'modflow' specifying type of id. See details. Defaults to 'r'
#' @param ... ignored
#' @return a data.frame with column i, j and k specifying the row, column and layer indices
#' @details a modflow id provides the place of the number in an input file 3d array using row-major numbering; R uses column-major numbering
#' @export
convert_id_to_ijk <- function(id, grid, type = c('r', 'modflow'), ...) {
  if(grid$gridtype != 'DIS') stop('Converting id to ijk is only supported for DIS grids', call. = FALSE)
  type <- match.arg(type)

  k <- (id - 1) %/% (grid$nrow * grid$ncol)
  id <- id - k * (grid$nrow * grid$ncol)
  if(type == 'r') {
    j <- (id - 1) %/% grid$nrow
    i <- id - j * grid$nrow
    df <- data.frame(i = i, j = j + 1, k = k + 1)
  } else if(type == 'modflow') {
    i <- (id - 1) %/% grid$ncol
    j <- id - i * grid$ncol
    df <- data.frame(i = i + 1,j = j,k = k + 1)
  }
  return(df)
}

#' Convert id to id for a DIS grid
#'
#' [convert_id_to_id()] converts a cell id of a DIS grid from R numbering
#' (column-major) to MODFLOW numbering (row-major), and vice versa.
#'
#' @param id cell id, providing the place of the number in an input file 2d or
#'   3d array
#' @param from 'r' or 'modflow'. The type of id to convert from. Defaults to
#'   'modflow'
#' @param to 'r' or 'modflow'. The type of id to convert to. Defaults to 'r'
#' @param grid mf6grid object
#' @param ... ignored
#'
#' @details a modflow id provides the place of the number in an input file 3d
#'   array using row-major numbering; R uses column-major numbering
#' @return integer vector like `id`, with the requested numbering type
#' @export
convert_id_to_id = function(id, grid, from = c('modflow', 'r'), to = c('r', 'modflow'), ...) {

  if(grid$gridtype != 'DIS') stop('Converting between id\'s is only supported for DIS grids', call. = FALSE)
  from <- match.arg(from)
  to <- match.arg(to)

  if(from == to) {
    idn <- id
  } else {
    ijk <- convert_id_to_ijk(id, grid, type = from)
    idn <- convert_ijk_to_id(i = ijk$i, j = ijk$j, k = ijk$k, grid, type = to)
  }

  return(idn)
}

#' Convert real-world coordinates to model cell id's, local coordinates or offsets
#'
#' [convert_xyz_to_grid()] converts real-world x-y (and optionally z) coordinates to cell id's,
#'    local grid coordinates and/or offsets for a MODFLOW 6 grid
#'
#' @param x numeric vector with real-world x-coordinate
#' @param y numeric vector with real-world y-coordinate
#' @param z optional numeric vector with real-world z-coordinate
#' @param grid mf6grid object
#' @param output character defining the output type, either `"id"` (for cell id's), `"xyz"` for local coordinates, `"offset"` for cell offsets or `"all"` for all of the above.
#'
#' @return For `output = "id"` a vector with the cell id for the cell containing the corresponding point. Points that fall on a cell boundary will return the largest cell id value.
#'    Points outside the model grid will return `NA`.
#'
#'    It is assumed the supplied real-world coordinates are in the same coordinate system as `grid`.
#' @export
#'
#' @examples
convert_xyz_to_grid <- function(x, y, z = NULL, grid, output = c('id', 'xyz', 'offset', 'all')) {
  output <- match.arg(output)

  # TODO different kinds of output besides id
  if(output != 'id') stop('Only output = "id" supported at the moment', call. = FALSE)

  # TODO z

  # TODO rotation + projection


  crds <- cbind(x = x, y = y, z = z)
  cellid <- rep(NA, nrow(crds))

  if(grid$gridtype == 'DIS') {
    # use existing RMODFLOW code
    # TODO rotation + projection
    cellfaces_x <- c(0, cumsum(grid$delr)) + grid$xorigin
    cellfaces_y <- sum(grid$delc) - c(0, cumsum(grid$delc)) + grid$yorigin

    # left-closed
    icol <- findInterval(crds[,'x'], cellfaces_x, rightmost.closed = TRUE)
    icol <- ifelse(icol < 1 | icol > grid$ncol, NA, icol)

    # findInterval requires vec to be increasing
    irow <- grid$nrow + 1 - findInterval(crds[,'y'], rev(cellfaces_y), left.open = TRUE, rightmost.closed = TRUE)
    irow <- ifelse(irow < 1 | irow > grid$nrow, NA, irow)

    ilay <- 1 # TODO: z
    cellid <- convert_ijk_to_id(irow, icol, ilay, grid, type = 'r') # TODO modflow id

  } else if(grid$gridtype == 'DISV') {

    # loop over all cells, construct the polygon (not closed, extra - 1) and call point-in-polygon
    for(icell in 1:grid$ncpl) {
      nvert <- grid$iavert[icell + 1] - grid$iavert[icell] - 1 # open polygon
      poly <- matrix(NA, nrow = nvert, ncol = 2)
      ipoly <- 0
      for(ipos in seq(grid$iavert[icell], grid$iavert[icell + 1] - 1 - 1)) { # open polygon
        ipoly <- ipoly + 1
        ivert <- grid$javert[ipos]
        xv <- grid$vertices[ivert, 1] # TODO rotation + projection
        yv <- grid$vertices[ivert, 2] # TODO rotation + projection
        poly[ipoly,] <- c(xv, yv)
      }
      pip <- point_in_polygon(crds[,1:2], polygon = poly) # this is slow
      cellid <- ifelse(pip, icell, cellid)
      if(all(!is.na(cellid))) break # exit loop if all points are found
    }

  } else if(grid$gridtype == 'DISU') {
    stop('Functions for a DISU grid not yet supported', call. = FALSE)
  } else {
    stop('Unknown grid type', call. = FALSE)
  }

  return(cellid)
}

# TODO FUNCTIONS:
# convert_grid_to_xyz
# get_ijk # only for DIS grids
# get_id
# cell_dimensions



