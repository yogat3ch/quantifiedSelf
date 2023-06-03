#' directory path generation convenience functions
#' @param ... \code{(chr)} directory paths
#' @param mkpath \code{lgl} Whether to return a path regardless of whether the file/dir exists or not
#' @param ext \code{(chr)} file extension
#' @param mustWork \code{lgl} If `TRUE`, an error is given if there are no matching files.
#' @usage dirs$data()
#' @export
#' @examples dirs$data("mydata", ext = "csv")
dirs <-
list(css = function (..., ext = "", mkpath = FALSE, mustWork = FALSE) 
{
    .path <- fs::path("inst/app/www/css", ..., ext = ext)
    if (!mkpath) {
        .path <- stringr::str_remove(.path, "^inst\\/?")
        app_sys(mustWork = mustWork, .path)
    }
    else .path
}, data = function (..., ext = "", mkpath = FALSE, mustWork = FALSE) 
{
    .path <- fs::path("data", ..., ext = ext)
    if (!mkpath) {
        .path <- stringr::str_remove(.path, "^inst\\/?")
        app_sys(mustWork = mustWork, .path)
    }
    else .path
}, dev = function (..., ext = "", mkpath = FALSE, mustWork = FALSE) 
{
    .path <- fs::path("dev", ..., ext = ext)
    if (!mkpath) {
        .path <- stringr::str_remove(.path, "^inst\\/?")
        app_sys(mustWork = mustWork, .path)
    }
    else .path
}, extdata = function (..., ext = "", mkpath = FALSE, mustWork = FALSE) 
{
    .path <- fs::path("inst/extdata", ..., ext = ext)
    if (!mkpath) {
        .path <- stringr::str_remove(.path, "^inst\\/?")
        app_sys(mustWork = mustWork, .path)
    }
    else .path
}, img = function (..., ext = "", mkpath = FALSE, mustWork = FALSE) 
{
    .path <- fs::path("inst/app/www/img", ..., ext = ext)
    if (!mkpath) {
        .path <- stringr::str_remove(.path, "^inst\\/?")
        app_sys(mustWork = mustWork, .path)
    }
    else .path
}, inst = function (..., ext = "", mkpath = FALSE, mustWork = FALSE) 
{
    .path <- fs::path("inst", ..., ext = ext)
    if (!mkpath) {
        .path <- stringr::str_remove(.path, "^inst\\/?")
        app_sys(mustWork = mustWork, .path)
    }
    else .path
}, js = function (..., ext = "", mkpath = FALSE, mustWork = FALSE) 
{
    .path <- fs::path("inst/app/www/js", ..., ext = ext)
    if (!mkpath) {
        .path <- stringr::str_remove(.path, "^inst\\/?")
        app_sys(mustWork = mustWork, .path)
    }
    else .path
}, R = function (..., ext = "", mkpath = FALSE, mustWork = FALSE) 
{
    .path <- fs::path("R", ..., ext = ext)
    if (!mkpath) {
        .path <- stringr::str_remove(.path, "^inst\\/?")
        app_sys(mustWork = mustWork, .path)
    }
    else .path
}, renv = function (..., ext = "", mkpath = FALSE, mustWork = FALSE) 
{
    .path <- fs::path("renv", ..., ext = ext)
    if (!mkpath) {
        .path <- stringr::str_remove(.path, "^inst\\/?")
        app_sys(mustWork = mustWork, .path)
    }
    else .path
}, tests = function (..., ext = "", mkpath = FALSE, mustWork = FALSE) 
{
    .path <- fs::path("tests/testthat", ..., ext = ext)
    if (!mkpath) {
        .path <- stringr::str_remove(.path, "^inst\\/?")
        app_sys(mustWork = mustWork, .path)
    }
    else .path
}, top = function (..., ext = "", mkpath = FALSE, mustWork = FALSE) 
{
    .path <- fs::path(".", ..., ext = ext)
    if (!mkpath) {
        .path <- stringr::str_remove(.path, "^inst\\/?")
        app_sys(mustWork = mustWork, .path)
    }
    else .path
}, vault = function (..., ext = "", mkpath = FALSE, mustWork = FALSE) 
{
    .path <- fs::path("inst/vault", ..., ext = ext)
    if (!mkpath) {
        .path <- stringr::str_remove(.path, "^inst\\/?")
        app_sys(mustWork = mustWork, .path)
    }
    else .path
}, www = function (..., ext = "", mkpath = FALSE, mustWork = FALSE) 
{
    .path <- fs::path("inst/app/www", ..., ext = ext)
    if (!mkpath) {
        .path <- stringr::str_remove(.path, "^inst\\/?")
        app_sys(mustWork = mustWork, .path)
    }
    else .path
})
