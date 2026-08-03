#' Check Haul Depths for Errors
#'
#' Connects to the RACE database via gapindex, extracts unfinalized haul and 
#' event measurements for a specific cruise and region, and identifies hauls 
#' with depth anomalies (e.g., missing depths or gear depth exceeding bottom depth) for the data finalization steps.
#'
#' @param cruise Numeric or character. Cruise identifier (e.g., \code{202601}).
#' @param region Character. Survey region code (e.g., \code{"AI"}, \code{"GOA"}, \code{"EBS"}).
#' @param channel Oracle connection object. Optional. If \code{NULL}, establishes a 
#'   connection via \code{gapindex::get_connected()}.
#'
#' @return A data frame containing flagged haul records sorted by region, cruise, vessel, and haul.
#' @export
#'
#' @importFrom gapindex get_connected sql_query
#' @importFrom dplyr filter select arrange contains
#'
#' @examples
#' \dontrun{
#' check_haul_depths(cruise = 202601, region = "AI")
#' }
check_haul_depths <- function(cruise, region, channel = NULL) {
  # Local NULL bindings for NSE column names to pass R CMD check
  gear_depth <- bottom_depth <- haul_id <- station <- NULL
  performance <- vessel_id <- haul <- NULL
  
  # Map region codes to survey definition IDs
  survey_def_map <- list(
    "AI"  = c(52),
    "GOA" = c(39, 47),
    "EBS" = c(98),
    "BSS" = c(78),
    "NBS" = c(143)
  )
  
  reg_upper <- toupper(region)
  if (!reg_upper %in% names(survey_def_map)) {
    stop(sprintf("Invalid region '%s'. Must be one of: %s", region, paste(names(survey_def_map), collapse = ", ")))
  }
  target_def_ids <- survey_def_map[[reg_upper]]
  
  if (is.null(channel)) {
    channel <- gapindex::get_connected(check_access = FALSE)
  }
  
  query <- sprintf("
    SELECT 
      h.HAUL_ID,
      c.CRUISE,
      c.REGION,
      c.VESSEL_ID,
      h.HAUL,
      h.STATION,
      h.PERFORMANCE,
      h.EDIT_GEAR_DEPTH AS GEAR_DEPTH,
      h.EDIT_BOTTOM_DEPTH AS BOTTOM_DEPTH
    FROM RACE_DATA.EDIT_HAULS h
    JOIN RACE_DATA.V_CRUISES c ON h.CRUISE_ID = c.CRUISE_ID
    WHERE c.CRUISE = %s
      AND c.SURVEY_DEFINITION_ID IN (%s)
  ", cruise, paste(target_def_ids, collapse = ","))
  
  haul_data <- gapindex::sql_query(channel = channel, query = query)
  
  if (!is.data.frame(haul_data) || nrow(haul_data) == 0) {
    stop(sprintf("No records found for Cruise: %s in Region: %s", cruise, region))
  }
  names(haul_data) <- tolower(names(haul_data))
  
  haul_data |>
    dplyr::filter(
      gear_depth == 0 | 
        bottom_depth == 0 | 
        gear_depth >= bottom_depth
    ) |>
    dplyr::select(
      haul_id, 
      cruise:station, 
      performance, 
      dplyr::contains("depth")
    ) |>
    dplyr::arrange(region, cruise, vessel_id, haul)
}


#' Check AI/GOA Hauls for Abundance Status
#'
#' Queries RACE edit tables directly to extract haul data, catch weights, and tow details,
#' then flags operational irregularities (e.g., improper gear, bad performance, short tow 
#' duration relative to catch weight, or duplicate stations) for the AI/GOA data finalization steps. 
#'
#' @param cruise Numeric or character. Cruise identifier (e.g., \code{202601}).
#' @param region Character. Survey region code (e.g., \code{"AI"}, \code{"GOA"}).
#' @param channel Oracle connection object. Optional. If \code{NULL}, establishes a 
#'   connection via \code{gapindex::get_connected()}.
#'
#' @return A summary data frame listing flagged hauls and their specific issue 
#'   labels, sorted by region, cruise, vessel, and haul. These hauls will be designated non-abundance hauls for finalization.
#' @export
#'
#' @importFrom gapindex get_connected sql_query
#' @importFrom dplyr group_by mutate right_join join_by ungroup select add_count case_when filter arrange
#'
#' @examples
#' \dontrun{
#' check_haul_issues(cruise = 202601, region = "AI")
#' }
check_haul_abundance <- function(cruise, region, channel = NULL) {
  # Local NULL bindings for NSE column names to pass R CMD check
  haul_id <- total_weight <- haul_weight_t <- vessel_id <- haul <- NULL
  duration <- station <- stratum <- n_station <- accessories <- NULL
  gear <- haul_type <- performance <- issue <- NULL
  
  # Map region codes to survey definition IDs
  survey_def_map <- list(
    "AI"  = c(52),
    "GOA" = c(39, 47),
    "EBS" = c(98),
    "BSS" = c(78),
    "NBS" = c(143)
  )
  
  reg_upper <- toupper(region)
  if (!reg_upper %in% names(survey_def_map)) {
    stop(sprintf("Invalid region '%s'. Must be one of: %s", region, paste(names(survey_def_map), collapse = ", ")))
  }
  target_def_ids <- survey_def_map[[reg_upper]]
  
  if (is.null(channel)) {
    channel <- gapindex::get_connected(check_access = FALSE)
  }
  
  # Query 1: Extract haul information filtered by CRUISE and SURVEY_DEFINITION_ID
  haul_query <- sprintf("
    SELECT 
      h.HAUL_ID,
      c.CRUISE,
      c.REGION,
      c.VESSEL_ID,
      h.HAUL,
      h.STATION,
      h.STRATUM,
      h.HAUL_TYPE,
      h.PERFORMANCE,
      h.ACCESSORIES,
      h.GEAR,
      ROUND(m.EDIT_DURATION_OB_FB * 60, 2) AS DURATION
    FROM RACE_DATA.EDIT_HAULS h
    JOIN RACE_DATA.V_CRUISES c ON h.CRUISE_ID = c.CRUISE_ID
    LEFT JOIN RACE_DATA.EDIT_HAUL_MEASUREMENTS m ON h.HAUL_ID = m.HAUL_ID
    WHERE c.CRUISE = %s
      AND c.SURVEY_DEFINITION_ID IN (%s)
  ", cruise, paste(target_def_ids, collapse = ","))
  
  new_haul <- gapindex::sql_query(channel = channel, query = haul_query)
  
  if (!is.data.frame(new_haul) || nrow(new_haul) == 0) {
    stop(sprintf("No haul records found in edit tables for Cruise: %s in Region: %s", cruise, region))
  }
  names(new_haul) <- tolower(names(new_haul))
  
  # Clean valid haul_ids for SQL query
  valid_haul_ids <- stats::na.omit(unique(new_haul$haul_id))
  
  # Query 2: Extract catch species totals
  catch_query <- sprintf("
    SELECT 
      c.HAUL_ID,
      cs.SPECIES_CODE,
      cs.TOTAL_WEIGHT_IN_HAUL AS TOTAL_WEIGHT
    FROM RACE_DATA.EDIT_CATCH_SPECIES cs
    JOIN RACE_DATA.EDIT_CATCH_SAMPLES c 
      ON cs.CATCH_SAMPLE_ID = c.CATCH_SAMPLE_ID
    WHERE c.HAUL_ID IN (%s)
  ", paste(valid_haul_ids, collapse = ","))
  
  new_catch <- gapindex::sql_query(channel = channel, query = catch_query)
  
  # Calculate total haul weight per haul_id directly
  if (is.data.frame(new_catch) && nrow(new_catch) > 0) {
    names(new_catch) <- tolower(names(new_catch))
    
    catch_summary <- new_catch |>
      dplyr::group_by(haul_id) |>
      dplyr::summarize(
        haul_weight_t = round(sum(total_weight, na.rm = TRUE) / 1000, 1),
        .groups = "drop"
      )
  } else {
    catch_summary <- data.frame(
      haul_id = numeric(0),
      haul_weight_t = numeric(0)
    )
  }
  
  # Join catch weight back to hauls and evaluate operational issues
  new_haul |>
    dplyr::left_join(catch_summary, by = dplyr::join_by(haul_id)) |>
    dplyr::mutate(
      haul_weight_t = ifelse(is.na(haul_weight_t), 0, haul_weight_t)
    ) |>
    dplyr::select(cruise, region, vessel_id, haul:duration, haul_weight_t) |>
    unique() |>
    dplyr::group_by(cruise, region, station, stratum) |>
    dplyr::add_count(name = "n_station") |> 
    dplyr::mutate(issue = dplyr::case_when(
      !accessories %in% c(15, 130) ~ "improper accessories",
      !gear %in% c(44, 172) ~ "improper gear",
      haul_type != 3 ~ "haul type not 3",
      performance < 0 ~ "performance inadequate",
      duration < 10 & haul_weight_t < duration ~ "catch too small for tow duration",
      n_station > 1 & sum(haul_type == 3) > 1 & sum(performance >= 0) > 1 ~ "station duplicated",
      TRUE ~ "none"
    )) |>
    dplyr::select(-n_station) |>
    dplyr::filter(issue != "none") |>
    dplyr::arrange(region, cruise, vessel_id, haul)
}
