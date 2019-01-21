#ifndef TCHERRY_UTILS_H
#define TCHERRY_UTILS_H

#include <Rcpp.h>

// [[Rcpp::plugins(cpp11)]]

#include <vector>

#include "class_CherryModel.h"
#include "helper_hash.h"

int mat_indices_to_vec_index(int row, int column, int n, int size_upper_tri);

std::vector<int> cliques_to_upper_tri_adj_mat(
    const std::vector< std::vector<int> >& cliques,
    int n);

std::vector< std::vector<int> > r_list_to_vector_vector(const Rcpp::List& r_cliques);
std::vector< std::vector<int> > r_matrix_to_vector_vector(const Rcpp::IntegerMatrix& r_cliques);

//////////////////

int find_model_index(const Rcpp::List& models_haystack, const Rcpp::List& model_needle);

//////////////////

std::vector<CherryModelUnused> convert_initial_models_to_cpp(const Rcpp::List& models);
Rcpp::List convert_final_models_to_r(const std::vector<CherryModelUnused>& models, 
                                     const int n, 
                                     const int k);

//////////////////

// FIXME: Templated functions? 
std::vector<CherryModelMI> convert_models_to_cpp_for_MI(const Rcpp::List& models);
Rcpp::List convert_final_MI_models_to_r(const std::vector<CherryModelMI>& models, 
                                        const int n, 
                                        const int k);


#endif