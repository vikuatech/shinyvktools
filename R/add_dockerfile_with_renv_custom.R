#' add_dockerfile_with_renv_custom
#'
#' @description Custom wrapper over golem::add_dockerfile_with_renv() that insludes .Renviron lines and .secrets in Dockerfile
#'
#' @param gcp_project name of GCP project
#' @param ... argument to pass to golem::add_dockerfile_with_renv_custom()
#'
#' @return Creates or replace deploy/ folder with corresponding resources
#'
#' @export
add_dockerfile_with_renv_custom <- function(gcp_project, ...){

  golem::add_dockerfile_with_renv(output_dir="deploy", from = "rocker/shiny-verse", open = F, ...)

  # Secret Service Account Token
  secrets_files <- list.files('.secrets', full.names = F)
  sat <- stringr::str_subset(secrets_files, '_sat.json')

  # Edit the Dockerfile `deploy/Dockerfile` and add these lines so the app can get the token
  env_lines <- readLines('.Renviron')
  docker_lines <- readLines('deploy/Dockerfile')

  new_env_lines <- paste('ENV', stringr::str_replace(env_lines, '.secrets/', '/root/'))
  copy_sat_line <- paste('COPY', sat, '/root/')

  writeLines(
    c(docker_lines[1], copy_sat_line, new_env_lines, docker_lines[-1]),
    'deploy/Dockerfile'
  )

  # Rename image name to GCP Build image
  readme_lines <- readLines('deploy/README')

  pkg_name <- stringr::str_extract(readme_lines[2], '(?<=-t ).*(?=:latest .)')
  image_name <- paste('gcr.io', gcp_project, pkg_name, sep = '/')
  new_readme_line <- stringr::str_replace(readme_lines[2], pkg_name, image_name)
  run_line <- stringr::str_replace(readme_lines[3], pkg_name, image_name)
  push_line <- sprintf('docker push %s:latest', image_name)

  writeLines(
    c(readme_lines[1], new_readme_line, run_line, readme_lines[4], push_line),
    'deploy/README'
  )

  # Copy the service account token in the deploy folder
  cat('Copying .secrets/', sat, 'into deploy/ \n')
  file.copy(paste0('.secrets/', sat), paste0('deploy/', sat))

}
