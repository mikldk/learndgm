#ifndef TCHERRY_UTILS_H
#define TCHERRY_UTILS_H

#include <Rcpp.h>

// [[Rcpp::plugins(cpp11)]]

#include <vector>

#include "class_CherryModel.h"

std::vector<CherryModelUnused> convert_initial_models_to_cpp(const Rcpp::List& models);
Rcpp::List convert_final_models_to_r(const std::vector<CherryModelUnused>& models, 
                                     const int n, 
                                     const int k);

// FIXME: Templated function?
std::vector<CherryModelMI> convert_models_to_MI_cpp(const Rcpp::List& models);

#endif