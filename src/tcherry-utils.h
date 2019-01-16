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

// FIXME: Templated functions? 
std::vector<CherryModelMI> convert_models_to_cpp_for_MI(const Rcpp::List& models);
Rcpp::List convert_final_MI_models_to_r(const std::vector<CherryModelMI>& models, 
                                        const int n, 
                                        const int k);


#endif