# Converts the archived INE data (RDS files under poblacion/ and seccionado/)
# into the static files the web app reads. Run once; the data is frozen at 2021.
#
#   docker run --rm -v "$PWD":/w -w /w rocker/geospatial:4.5.1 Rscript scripts/export-data.R
#
# Output (in build/raw/, simplified afterwards by scripts/build-data.sh):
#   geo-<year>.geojson            every census section, WGS84, properties id/m/p/n
#   pop/<year>/<province>.json    {s: [section ids], m: [[men per nationality]], w: [[women ...]]}
#   meta.json                     years, nationalities, provinces

suppressPackageStartupMessages({ library(sp); library(sf); library(jsonlite) })

out <- "build/raw"
dir.create(file.path(out, "pop"), recursive = TRUE, showWarnings = FALSE)
years <- as.integer(strsplit(Sys.getenv("YEARS", "2012:2021"), ":")[[1]]); years <- years[1]:years[length(years)]

fix_utf8 <- function(x) { x <- as.character(x); Encoding(x) <- "UTF-8"; enc2utf8(x) }

# INE renamed two groups over the years. Every year is mapped onto the 2021 names:
# "Total Africa" became "Total África" in 2018, and "Total Europa No Comunitaria"
# became "Total Resto de Europa" in 2021, when the United Kingdom left the EU total.
nationality_aliases <- c("Total Africa" = "Total África",
                         "Total Europa No Comunitaria" = "Total Resto de Europa")
canonical <- function(x) { x <- fix_utf8(x); i <- x %in% names(nationality_aliases); x[i] <- nationality_aliases[x[i]]; x }

nationalities <- NULL
provinces <- NULL

for (y in years) {
  message("year ", y)

  # Geometry
  s <- readRDS(sprintf("seccionado/%d/secciones.rds", y))
  g <- st_as_sf(s)
  g <- data.frame(
    id = as.character(g$CUSEC),
    m  = as.character(g$CUMUN),
    p  = as.character(g$CPRO),
    n  = fix_utf8(g$NMUN),
    geometry = st_geometry(g)
  ) |> st_as_sf()
  # Repairing invalid rings can leave stray lines next to the polygon; keep the polygons only.
  g <- st_make_valid(st_transform(g, 4326))
  g <- st_cast(st_collection_extract(g, "POLYGON"), "MULTIPOLYGON")
  stopifnot(!anyDuplicated(g$id))
  f <- file.path(out, sprintf("geo-%d.geojson", y))
  unlink(f)
  st_write(g, f, quiet = TRUE)

  if (is.null(provinces)) {
    pr <- unique(data.frame(code = as.character(s@data$CPRO), name = fix_utf8(s@data$NPRO)))
    provinces <- pr[order(pr$code), ]
  }

  # Population by nationality: keep men and women, the total is their sum
  read_sex <- function(sex) readRDS(sprintf("poblacion/%d/Nacionalidad%d%s.rds", y, y, sex))
  h <- read_sex("hombres"); w <- read_sex("mujeres"); a <- read_sex("ambos")
  if (is.null(nationalities)) nationalities <- canonical(levels(readRDS(sprintf(
    "poblacion/%d/Nacionalidad%dambos.rds", max(years), max(years)))$nacionalidad))
  stopifnot(setequal(canonical(levels(a$nacionalidad)), nationalities))

  to_matrix <- function(d) {
    d <- d[d$sección != "TOTAL", ]
    secs <- sort(unique(as.character(d$sección)))
    mat <- matrix(0L, nrow = length(secs), ncol = length(nationalities))
    v <- d$value; v[is.na(v)] <- 0
    mat[cbind(match(as.character(d$sección), secs), match(canonical(d$nacionalidad), nationalities))] <- as.integer(v)
    rownames(mat) <- secs
    mat
  }
  H <- to_matrix(h); W <- to_matrix(w); A <- to_matrix(a)
  secs <- sort(union(rownames(H), rownames(W)))
  pad <- function(M) { R <- matrix(0L, length(secs), ncol(M)); R[match(rownames(M), secs), ] <- M; R }
  H <- pad(H); W <- pad(W); A <- A[match(secs, rownames(A)), , drop = FALSE]
  bad <- sum(abs((H + W) - A), na.rm = TRUE)
  message("  sections=", length(secs), " men+women vs both, abs diff=", bad)
  stopifnot(bad == 0)

  dir.create(file.path(out, "pop", y), showWarnings = FALSE)
  prov <- substr(secs, 1, 2)
  for (p in unique(prov)) {
    i <- which(prov == p)
    write(toJSON(list(s = secs[i], m = unname(H[i, , drop = FALSE]), w = unname(W[i, , drop = FALSE])),
                 matrix = "rowmajor", auto_unbox = FALSE),
          file.path(out, "pop", y, paste0(p, ".json")))
  }
  rm(s, g, h, w, a, H, W, A); gc()
}

write(toJSON(list(years = years, nationalities = nationalities,
                  provinces = provinces), auto_unbox = TRUE, pretty = TRUE),
      file.path(out, "meta.json"))
message("done")
