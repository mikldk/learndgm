#include <Rcpp.h>

// [[Rcpp::plugins(cpp11)]]

#include <vector>

#include "helper_hash.h"

// X = (X_1, X_2, ..., X_k)
// I(X) = sum_X p(X) log[ p(X) / [ p(X_1) p(X_2) ... p(X_k) ]]
//      = sum_X p(X) log[ p(X) ] - p(X) sum [ log_i (p(X_i)) ]

/*
 * 
 * Hash-map indexing
 * [int: number_of_loci][vector<int>: variable_indices] -> double: MI
 * 
 * E.g.
 * [clique-size k = 3][1,2,3]
 */

typedef std::unordered_map< int, 
                            std::unordered_map< 
                              std::vector<int>, 
                              double, vechash, equal_to_intvec> > hashmap_mutual_info;

/*

// X = (X_1, X_2, ..., X_k)
// I(X) = sum_X p(X) log[ p(X) / [ p(X_1) p(X_2) ... p(X_k) ]]
//      = sum_X p(X) log[ p(X) ] - p(X) sum [ log_i (p(X_i)) ]
double mutual_information_cached(const Rcpp::IntegerMatrix& d, 
                                 const std::vector<int>& cols, 
                                 std::unordered_map< int, std::unordered_map< std::vector<int>, double, vechash> >& information_content,
                                 size_t& cache_hits) {
  
  auto search = information_content[cols.size()].find(cols);
  
  if (search != information_content[cols.size()].end()) {
    // I already calculated before, reuse
    cache_hits++;
    return search->second;
  } 
  
  // Counts NOT found
  
  double I = mutual_information_internal(d, cols);
  
  information_content[cols.size()][cols] = I;
  
  return I;
}

Rcpp::List cpp_annotate_mi() {
  
}
 */
