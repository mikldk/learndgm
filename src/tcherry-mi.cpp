#include <Rcpp.h>

// [[Rcpp::plugins(cpp11)]]

#include <vector>
#include <cmath> // log2

#include "helper_hash.h"
#include "class_CherryModel.h"
#include "tcherry-utils.h"

// X = (X_1, X_2, ..., X_k)
// I(X) = sum_X p(X) log[ p(X) / [ p(X_1) p(X_2) ... p(X_k) ]]
//      = sum_X p(X) log[ p(X) ] - p(X) sum [ log_i (p(X_i)) ]

/*
 * 
 * Hash-map indexing
 * [vector<int>: variable_indices] -> double: MI
 * 
 * E.g.
 * [clique-size k = 3][1,2,3]
 */

typedef std::unordered_map<std::vector<int>, double, 
                           vechash, equal_to_intvec> hashmap_mutual_info;



// C-indexing
double mutual_information_internal(
    const Rcpp::IntegerMatrix& d, 
    const std::vector<int>& cols) {
  
  std::unordered_map< std::vector<int>, size_t, vechash, equal_to_intvec> simul_obs;
  
  double Ndbl = (double)d.nrow();
  
  int k = cols.size();
  
  // These can be cached even further, but for now it should be okay
  std::vector< std::unordered_map< int, size_t> > marginal_obs(k);
  
  // FIXME: slow accessing by rows?
  for (size_t i = 0; i < d.nrow(); ++i) {
    std::vector<int> d_i(cols.size());
    for (size_t j = 0; j < k; ++j) {
      int val = d(i, cols[j]);
      d_i[j] = val;
      
      marginal_obs[j][val] += 1;
    }
    simul_obs[d_i] += 1;
  }
  
  /*
  Rcpp::Rcout << "-------------------" << std::endl;
  print_contingency_table(simul_obs);
  for (size_t j = 0; j < k; ++j) {
  Rcpp::print(Rcpp::wrap(marginal_obs[j]));
  }
  Rcpp::Rcout << "-------------------" << std::endl;
  */
  
  double I = 0.0;
  for (auto it : simul_obs) {
    size_t n_simul = it.second;
    
    // n_simul never 0...
    if (n_simul == 0) {
      Rcpp::stop("n_simul == 0");
    }
    
    double sum_marginal_log = 0.0;
    
    for (size_t j = 0; j < k; ++j) {
      size_t nj = marginal_obs[j][ it.first[j] ];
      
      if (nj == 0) {
        Rcpp::stop("nj == 0");
      }
      
      sum_marginal_log += std::log2((double)nj / Ndbl);
    }
    
    // I(X) = sum_X p(X) log[ p(X) ] - p(X) sum [ log_i (p(X_i)) ]
    double p = (double)n_simul / Ndbl;
    I += p * (std::log2(p) - sum_marginal_log);
  } 
  
  return I;
}

// X = (X_1, X_2, ..., X_k)
// I(X) = sum_X p(X) log[ p(X) / [ p(X_1) p(X_2) ... p(X_k) ]]
//      = sum_X p(X) log[ p(X) ] - p(X) sum [ log_i (p(X_i)) ]
double mutual_information_cached(const Rcpp::IntegerMatrix& d, 
                                 const std::vector<int>& cols, 
                                 hashmap_mutual_info& mi_table) {
  
  auto search = mi_table.find(cols);
  
  if (search != mi_table.end()) {
    return search->second;
  } 
  
  // Counts NOT found
  
  double I = mutual_information_internal(d, cols);
  
  mi_table[cols] = I;
  
  return I;
}

Rcpp::List cpp_annotate_mi(const Rcpp::List& modellist, 
                           const int n,
                           const int k,
                           const Rcpp::IntegerMatrix& d) {
  
  // FIXME: Check class learndgm_modelstructure_list
  // FIXME: Check that modellist["models"] exists (not null)
  
  std::vector<CherryModelMI> models = convert_models_to_cpp_for_MI(modellist["models"]);

  hashmap_mutual_info mi_table;
  
  for (int i = 0; i < models.size(); ++i) {
    const std::vector< std::vector<int> > cliques = models[i].get_cliques();
    const std::vector< std::vector<int> > seps = models[i].get_seps();
    
    std::vector<double> cliques_mi;
    cliques_mi.reserve(cliques.size());
    for (auto& v : cliques) {
      double mi = mutual_information_cached(d, v, mi_table);
      cliques_mi.push_back(mi);
    }
    models[i].set_cliques_mi(cliques_mi);
    
    std::vector<double> seps_mi;
    seps_mi.reserve(seps.size());
    for (auto& v : seps) {
      double mi = mutual_information_cached(d, v, mi_table);
      seps_mi.push_back(mi);
    }
    models[i].set_seps_mi(seps_mi);
  }
  
  Rcpp::List ret = convert_final_MI_models_to_r(models, n, k);
  
  return ret;
}

