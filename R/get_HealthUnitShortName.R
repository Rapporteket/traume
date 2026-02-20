#' Funksjon som gir navn på enhet basert på reaktivt ReshId (UnitId) basert
#' på innloggingsinfo i Falk. Denne funksjonen er basert på map_db_resh.
#' Denne kan brukes til plot f.eks., for å få rett enhet i tittel osv.
#'
#' UnitId => user$org()
#' map_data => map_db_resh
#'
#' Begge disse defineres i server.R
#'
#' @title get HealthUnitShortName
#' @export

get_HealthUnitShortName <- function (UnitId, map_data) {
  navn_reaktivt <- map_data |>
    dplyr::filter(UnitId == {{UnitId}}) |>
    dplyr::select(orgname)

  return(navn_reaktivt)
}

