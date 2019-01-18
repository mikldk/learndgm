#include <Rcpp.h>

// [[Rcpp::plugins(cpp11)]]

#include <vector>

#include "helper_hash.h"
#include "class_CherryModel.h"
#include "tcherry-utils.h"


// @param n Number of variables in the model, used for size of adjacency matrix
std::vector<CherryModelUnused> rcpp_new_remove_equal_models_worker(
    const std::vector<CherryModelUnused>& models,
    int n) {
  
  /* Saves the index of the model with a given hash.
  * Models with same hash not all saved, but that's fine
  * as they are equal. 
  * Hence, when done, values of this map are the indices of 
  * the models to keep.
  */
  
  hashmap_intvec_int model_map;
  
  for (int i = 0; i < models.size(); ++i) {
    CherryModelUnused m = models[i];
    
    std::vector<int> upper_tri = cliques_to_upper_tri_adj_mat(m.get_cliques(), n);
    
    auto iter = model_map.find(upper_tri);
    
    if (iter == model_map.end()) {
      auto pair = std::pair< std::vector<int>, int >{upper_tri, i};
      model_map.insert(pair);
    }
  }
  
  std::vector<CherryModelUnused> new_models;
  new_models.reserve(model_map.size());
  
  for (auto& v : model_map) {
    new_models.push_back(models[v.second]);
  }
  
  return new_models;
}

// @param n Number of variables in the model, used for size of adjacency matrix
std::vector<CherryModelUnused> rcpp_new_remove_equal_intermediatemodels_worker(
    const std::vector<CherryModelUnused>& models,
    int n) {
  
  /* Saves the index of the model with a given hash.
  * Models with same hash not all saved, but that's fine
  * as they are equal. 
  * Hence, when done, values of this map are the indices of 
  * the models to keep.
  */
  
  hashmap_intvec_int model_map;
  
  for (int i = 0; i < models.size(); ++i) {
    CherryModelUnused m = models[i];
    
    // seps?
    // Are unused at all necessary?
    std::vector<int> upper_tri = cliques_to_upper_tri_adj_mat(m.get_cliques(), n);
    std::vector<int> unused = m.get_unused();
    std::vector<int> model_hashvec;
    model_hashvec.reserve(upper_tri.size() + unused.size());
    
    for (int v : upper_tri) {
      model_hashvec.push_back(v);
    }
    for (int v : unused) {
      model_hashvec.push_back(v);
    }
    
    auto iter = model_map.find(model_hashvec);
    
    if (iter == model_map.end()) {
      auto pair = std::pair< std::vector<int>, int >{model_hashvec, i};
      model_map.insert(pair);
    }
  }
  
  std::vector<CherryModelUnused> new_models;
  new_models.reserve(model_map.size());
  
  for (auto& v : model_map) {
    new_models.push_back(models[v.second]);
  }
  
  return new_models;
}




std::vector< std::vector<int> > get_subsets(const Rcpp::IntegerMatrix& kmin1_subsets_idx) {
  int kmin1_subsets = kmin1_subsets_idx.ncol();
  std::vector< std::vector<int> > km1sets(kmin1_subsets);
  
  for (int i_sep = 0; i_sep < kmin1_subsets; ++i_sep) {
    Rcpp::IntegerVector v = kmin1_subsets_idx(Rcpp::_, i_sep);
    Rcpp::IntegerVector w = v - 1; // C++ indexing
    std::vector<int> u = Rcpp::as< std::vector<int> >(w);
    km1sets[i_sep] = u;
  }
  
  return km1sets;
}

//' Remove duplicate models
//' 
//' @export
// [[Rcpp::export]]
Rcpp::List rcpp_new_all_tcherries_worker(const Rcpp::List& initial_models, 
                                         const Rcpp::IntegerMatrix& kmin1_subsets_idx, 
                                         int n,
                                         int n_unused,
                                         bool verbose) {
  
  std::vector<CherryModelUnused> ret_models = convert_initial_models_to_cpp(initial_models);
  
  int km1 = kmin1_subsets_idx.nrow();
  int k = km1 + 1;
  std::vector< std::vector<int> > km1sets = get_subsets(kmin1_subsets_idx);
  int kmin1_subsets = km1sets.size();
  
  for (int iter = 1; iter <= n_unused; ++iter) {
    if (verbose) {
      Rcpp::Rcout << "Adding unused variable #" << iter << ".\n";
    }
    
    std::vector<CherryModelUnused> new_models;
    
    for (int i_m = 0; i_m < ret_models.size(); ++i_m) {
      Rcpp::checkUserInterrupt();
        
      CherryModelUnused model = ret_models[i_m];
      
      std::vector< std::vector<int> > cliques = model.get_cliques();
      std::vector< std::vector<int> > seps = model.get_seps();
      std::vector<int> parents = model.get_parents();
      std::vector<int> unused = model.get_unused();
      
      for (int i_unused = 0; i_unused < unused.size(); ++i_unused) {
        int x_unused = unused[i_unused];
        
        std::vector<int> new_unused = unused;
        new_unused.erase(new_unused.begin() + i_unused);
        
        for (int i_parent_clique = 0; i_parent_clique < cliques.size(); ++i_parent_clique) {
          std::vector<int> parent_clique = cliques[i_parent_clique];
          
          std::vector<int> new_parents = parents;
          new_parents.push_back(i_parent_clique + 1); // R-indexing
          
          for (int i_sep = 0; i_sep < kmin1_subsets; ++i_sep) {
            // New separator
            std::vector<int> sep_idx = km1sets[i_sep];
            std::vector<int> new_sep(km1);
            for (int i_new_sep = 0; i_new_sep < km1; ++i_new_sep) {
              new_sep[i_new_sep] = parent_clique[ sep_idx[i_new_sep] ]; 
            }
            
            // New clique
            std::vector<int> new_clique(k);
            for (int tmp_i = 0; tmp_i < km1; ++tmp_i) {
              new_clique[tmp_i] = new_sep[tmp_i];
            }
            new_clique[km1] = x_unused;
            
            // sort for comparing (removing duplicates) later in:
            // model_to_adjacency_matrix(): A[ clique[j1], clique[j2] ]
            std::sort(new_clique.begin(), new_clique.end());
            
            std::sort(new_sep.begin(), new_sep.end());
            
            
            std::vector< std::vector<int> > new_cliques = cliques;
            new_cliques.push_back(new_clique);
            
            std::vector< std::vector<int> > new_seps = seps;
            new_seps.push_back(new_sep);
            
            CherryModelUnused new_model = CherryModelUnused(new_cliques, new_seps, new_parents, new_unused);
            
            new_models.push_back(new_model);
          }
        }
      }
    }
    
    //ret_models = new_models;
    // remove_duplicates
    ret_models = rcpp_new_remove_equal_intermediatemodels_worker(new_models, n);
  }
  
  // remove_duplicates
  ret_models = rcpp_new_remove_equal_models_worker(ret_models, n);
  
  Rcpp::List r_ret_models = convert_final_models_to_r(ret_models, n, k);
  
  return r_ret_models;
}
