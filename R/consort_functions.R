SLEEP <- 0.5
NO_SPLIT <- "None"




# adds children
AddChildren <- function(criteria, r, var) {
  dat <- criteria$data[[r]]
  dat_var <- dat[[var]]
  vals <- unique(dat_var)
  eqs <- paste0(var, "=='", vals, "'")
  new_criteria <- tibble::tibble(label = eqs, 
                                 code = eqs, 
                                 parent = r, 
                                 color = "black", 
                                 hidden = F)
  new_datas <- purrr::map(new_criteria$code, function(x) {
      dplyr::filter(criteria$data[[r]], 
                    eval(rlang::parse_expr(x)))})
  new_criteria$data <- new_datas
  criteria <- dplyr::bind_rows(criteria, new_criteria)
  criteria$split_var[r] <- var
  return(criteria)
}

# initializes the criterion tribble
Initialize <- function(dat) {
  init <- tibble::tribble(~label, ~code, ~parent, ~data, ~color, ~hidden, ~split_var,
          "All", "T", 0, dat, "black", F, NO_SPLIT)
  return(init)
}

# renames node r
Rename <- function(criteria, r, new_name) {
  criteria$label[r] <- new_name
  return(criteria)
}

# selects node r
SelectNode <- function(criteria, r) {
  criteria$color <- "black"
  criteria$color[r] <- "blue"
  return(criteria)
}

# toggles hidden state of node r and its children
ToggleHidden <- function(criteria, r) {
  criteria$hidden[c(r, GetChildren(criteria, r))] <- !criteria$hidden[r]
  return(criteria)
}

GetChildren <- function(criteria, r) {
  children <- which(criteria$parent == r)
  for (cc in children) {
    children <- c(children, GetChildren(criteria, cc))
  }
  return(unique(children))
}

# deletes children of node r
DeleteChildren <- function(criteria, r) {
  children <- GetChildren(criteria, r)
  if (length(children) > 0) {
    criteria$num <- 1:nrow(criteria)
    criteria <- criteria[!criteria$num %in% children, ]
    dict <- c(0, 1:nrow(criteria))
    names(dict) <- c(0, criteria$num)
    criteria$parent <- dict[as.character(criteria$parent)]
    criteria$num <- NULL
  } 
  criteria$split_var[r] <- NO_SPLIT
  return(criteria)
}

GetParent <- function(criteria, r) {
  r_parent <- criteria$parent[r]
}

GetParents <- function(criteria, r) {
  parents <- c()
  r_parent <- GetParent(criteria, r)
  while(!r_parent == 0) {
    parents <- c(parents, r_parent)
    r_parent <- GetParent(criteria, r_parent)
  }
  return(parents)
}

RenderCriteriaHere <- function(criteria) {
  RenderCriteria(criteria, with_numbers = T) %>% DiagrammeR::render_graph() %>% print()
  Sys.sleep(SLEEP)
}

RenderCriteria <- function(criteria, with_numbers = F, with_hiding = F, with_hiding_colored = F) {
  if (with_hiding) {
    criteria$id <- 1:nrow(criteria)
    criteria <- criteria %>%
      dplyr::filter(!.data$hidden)
    dict <- 1:nrow(criteria)
    names(dict) <- criteria$id
    criteria$parent <- dict[as.character(criteria$parent)]
  }
  if (nrow(criteria) == 0) {
    unrendered <- DiagrammeR::create_graph(nodes_df = NULL, 
                               edges_df = NULL, 
                               attr_theme = "tb")
    return(unrendered)
  }
  criteria <- criteria %>%
    dplyr::mutate(label = gsub("'", "", .data$label, fixed = T))
  criteria_num_patients <- unlist(purrr::map(criteria$data, nrow))
  criteria <- criteria %>%
    dplyr::mutate(label = paste0(criteria$label, "\nN=", 
                          prettyNum(criteria_num_patients, big.mark = ",")), 
           tooltip = gsub("'", "", .data$code))

  n_nodes <- nrow(criteria)
  if (with_numbers) {
    labels <- paste0(1:n_nodes, ". ", criteria$label)
  } else {
    labels <- criteria$label
  }
  if (with_hiding_colored | with_hiding) {
    colors <- dplyr::if_else(criteria$hidden, "lightgrey", "black")
  } else {
    colors <- criteria$color
  }
  node_df <- DiagrammeR::create_node_df(n_nodes, 
                            label = labels, 
                            shape = "rectangle", 
                            tooltip = criteria$tooltip,
                            color = colors,
                            fixedsize = F,
                            fontsize = 6)
  if (n_nodes > 1) {
    child_rows <- 2:nrow(criteria)
    edge_df <- DiagrammeR::create_edge_df(from = criteria$parent[child_rows], 
                              to = child_rows, 
                              rel = "leading_to")
  } else {
    edge_df <- NULL
  }
  unrendered <- DiagrammeR::create_graph(nodes_df = node_df, 
                             edges_df = edge_df, 
                             attr_theme = "tb")
  return(unrendered)
}


#' Function to generate consort diagrams
#' 
#' @param metadata Metadata downloaded from shiny app
#' @param data Data uploaded to the app for generating consort diagram
#' @return graph created by DiagrammeR (graph object of class dgr_graph)
#' @examples 
#' data <- data.frame(a = c('m', 'm', 'n', 'n'), 
#'                    b = c('p', 'p', 'q', 'q'))
#' metadata <- data.frame(label = c('All', "a=='m'"), 
#'                        code = c(TRUE, "a=='m'"), 
#'                        parent = c(0, 1), 
#'                        color = c("black", "black"), 
#'                        hidden = c(FALSE, FALSE), 
#'                        split_var = c('a', NA))
#' consort_diagram <- consort_from_metadata(metadata, data)
#' 
#' @export
consort_from_metadata <- function(metadata, data) {
   metadata <- tibble::as_tibble(metadata)
   metadata$data <- list(NA)
   metadata$data[[1]] <- data
   for (r in 2:nrow(metadata)) {
     metadata$data[[r]] <- dplyr::filter(metadata$data[[metadata$parent[r]]], 
                                         eval(rlang::parse_expr(metadata$code[r])))
   }
   unrendered <- RenderCriteria(metadata, with_hiding = T)
   DiagrammeR::render_graph(unrendered) %>%
     print()
   return(unrendered)
}

# Test <- function(){
#   dat <- utils::read.csv("play_data.csv")
#   criteria <- Initialize(dat)
#   RenderCriteriaHere(criteria)
#   criteria <- AddChildren(criteria, 1, "SEX")
#   RenderCriteriaHere(criteria)
#   criteria <- AddChildren(criteria, 3, "HRISKCV2")
#   RenderCriteriaHere(criteria)
#   criteria <- AddChildren(criteria, 2, "DIAHIS10")
#   RenderCriteriaHere(criteria)
#   criteria <- DeleteChildren(criteria, 3)
#   RenderCriteriaHere(criteria)
#   criteria <- HideNode(criteria, r = 5)
#   RenderCriteriaHere(criteria)
#   criteria <- SelectNode(criteria, r = 2)
#   RenderCriteriaHere(criteria)
#   criteria <- Rename(criteria, r = 3, "Da Ladies")
#   RenderCriteriaHere(criteria)
# }