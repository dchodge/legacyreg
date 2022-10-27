# load all dependences and source functions
source("scrips/setup.R")

# compile Stan model (won't do anything if already compiled)
model_A <- cmdstan_model("src/stan/two_line_model.stan",
                         stanc_options = list("O1"))

# fits model for all subsets of data and saves output
fit_model_all_subsets()