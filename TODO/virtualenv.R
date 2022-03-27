# Define virtual environment --------------------------------------------------------------------

PYTHON_DEPENDENCIES <- c("pip", "numpy", 
                         "pandas", "tensorflow", 
                         "datetime", "pyreadr")

virtualenv_dir <- Sys.getenv("VIRTUALENV_NAME")
python_path <- Sys.getenv("PYTHON_PATH")

# Create virtual env and install dependencies
reticulate::virtualenv_create(envname = virtualenv_dir, 
                              python = python_path)
reticulate::virtualenv_install(virtualenv_dir, 
                               packages = PYTHON_DEPENDENCIES, 
                               ignore_installed=TRUE)
reticulate::use_virtualenv(virtualenv_dir, 
                           required = T)
