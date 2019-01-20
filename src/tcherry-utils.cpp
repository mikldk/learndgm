#include <Rcpp.h>

// [[Rcpp::plugins(cpp11)]]

#include <vector>

#include "class_CherryModel.h"


// [[Rcpp::export]]
int mat_indices_to_vec_index(int row, int column, int n, int size_upper_tri) {
  // Upper triangular: row <= column
  
  // https://stackoverflow.com/a/27088560
  // k = (n*(n-1)/2) - (n-i)*((n-i)-1)/2 + j - i - 1
  return size_upper_tri - (n - row)*((n - row) - 1)/2 + column - row - 1;
}

std::vector<int> cliques_to_upper_tri_adj_mat(
    const std::vector< std::vector<int> >& cliques,
    int n) {
  
  /*
  * There are n variables in the model, hence 
  * adjacency matrix is n x n.
  * But only store in upper triagonal (as symmetric).
  * Hence a vector of size n * (n - 1) / 2 entries
  */
  int size_upper_tri = n * (n - 1) / 2;
  
  std::vector<int> upper_tri(size_upper_tri);
  
  for (auto& v : cliques) {
    int clique_size = v.size();
    
    // Cliques are sorted; v[i1] <= v[i2], gives upper triangular
    for (int i1 = 0; i1 < (clique_size - 1); ++i1) {
      int var1_row = v[i1] - 1; // R -> C++ indexing
      
      for (int i2 = i1 + 1; i2 < clique_size; ++i2) {
        int var2_col = v[i2] - 1; // R -> C++ indexing
        
        int index = mat_indices_to_vec_index(var1_row, var2_col, n, size_upper_tri);
        
        upper_tri[index] = 1;
      }
    }
  }
  
  return upper_tri;
}

///////////////////////////////////////////////

// FIXME: IntegerMatrix cliques?
std::vector< std::vector<int> > r_list_to_vector_vector(const Rcpp::List& r_cliques) {
  std::vector< std::vector<int> > cliques(r_cliques.size());
  
  // Cliques
  for (int j = 0; j < r_cliques.size(); ++j) {
    Rcpp::IntegerVector cj = r_cliques[j];
    std::vector<int> c = Rcpp::as< std::vector<int> >(cj);
    cliques[j] = c;
  }
  
  return cliques;
}

///////////////////////////////////////////////

// [[Rcpp::export]]
Rcpp::IntegerVector find_model_index(const Rcpp::List& models_haystack, const Rcpp::List& cliques_needle) {
  if (!(Rf_inherits(models_haystack, "learndgm_modelstructure_list") || 
        Rf_inherits(models_haystack, "learndgm_mimodelstructure_list"))) {
    Rcpp::stop("models_haystack must be a learndgm_modelstructure_list or a learndgm_mimodelstructure_list");
  }
  
  int n = models_haystack["n"];
  
  std::vector<int> needle = cliques_to_upper_tri_adj_mat(
    r_list_to_vector_vector(cliques_needle), n);
  
  for (int i = 0; i < models_haystack.size(); ++i) {
    Rcpp::List m = models_haystack[i];
    Rcpp::List r_cliques = m["cliques"];
    std::vector< std::vector<int> > cliques = r_list_to_vector_vector(r_cliques);
    std::vector<int> upper_tri = cliques_to_upper_tri_adj_mat(cliques, n);
    
    // FIXME: equal_to_intvec
    if (needle.size() == upper_tri.size() && needle == upper_tri) {
      Rcpp::IntegerVector ret(1);
      ret[0] = i + 1; // R-based index
      return ret;
    }
  }
  
  Rcpp::IntegerVector ret(1);
  ret[0] = Rcpp::IntegerVector::get_na();
  return ret;
}

///////////////////////////////////////////////

std::vector<CherryModelUnused> convert_initial_models_to_cpp(const Rcpp::List& models) {
  std::vector<CherryModelUnused> ret_models;
  ret_models.reserve(models.size());
  
  for (int i = 0; i < models.size(); ++i) {
    Rcpp::List m = models[i];
    Rcpp::List cliques = m["cliques"];
    Rcpp::List seps = m["seps"];
    Rcpp::IntegerVector parents = m["parents"];
    Rcpp::IntegerVector unused = m["unused"];
    
    std::vector< std::vector<int> > new_cliques = r_list_to_vector_vector(cliques);
    std::vector< std::vector<int> > new_seps = r_list_to_vector_vector(seps);
    std::vector<int> new_parents = Rcpp::as< std::vector<int> >(parents);
    std::vector<int> new_unused = Rcpp::as< std::vector<int> >(unused);
    
    CherryModelUnused model = CherryModelUnused(new_cliques, new_seps, new_parents, new_unused);
    ret_models.push_back(model);
  }
  
  return ret_models;
}

Rcpp::List convert_final_models_to_r(const std::vector<CherryModelUnused>& models,
                                     const int n,
                                     const int k) {
  int n_models = models.size();
  Rcpp::List ret_models(n_models);
  
  for (int i = 0; i < n_models; ++i) {
    CherryModelUnused model = models[i];
    
    std::vector< std::vector<int> > cliques = model.get_cliques();
    std::vector< std::vector<int> > seps = model.get_seps();
    std::vector<int> parents = model.get_parents();
    std::vector<int> unused = model.get_unused();
    
    if (unused.size() != 0) {
      Rcpp::stop("unused.size() != 0");
    }
    
    int n_cliques = cliques.size();
    int n_seps = seps.size();
    
    if ((n_cliques - 1) != n_seps) {
      Rcpp::stop("(n_cliques - 1) != n_seps");
    }
    
    if (parents.size() != n_seps) {
      Rcpp::stop("parents.size() != n_seps");
    }
    
    Rcpp::IntegerMatrix r_cliques(cliques[0].size(), n_cliques);
    Rcpp::IntegerMatrix r_seps(cliques[0].size() - 1, n_seps);
    
    // Cliques
    for (int j = 0; j < n_cliques; ++j) {
      std::vector<int> v = cliques[j];
      Rcpp::IntegerVector w = Rcpp::wrap(v);
      r_cliques(Rcpp::_, j) = w;
    }
    
    // Seps
    for (int j = 0; j < n_seps; ++j) {
      std::vector<int> v = seps[j];
      Rcpp::IntegerVector w = Rcpp::wrap(v);
      r_seps(Rcpp::_, j) = w;
    }
    
    Rcpp::IntegerVector r_parents(parents.size() + 1);
    r_parents[0] = Rcpp::IntegerVector::get_na();
    for (int i = 0; i < parents.size(); ++i) {
      r_parents[i + 1] = parents[i]; 
    }
    
    Rcpp::List r_model;
    r_model["cliques"] = r_cliques;
    r_model["seps"] = r_seps;
    r_model["parents"] = r_parents;
    ret_models[i] = r_model;
  }
  
  Rcpp::List ret;
  ret["models"] = ret_models;
  ret["n"] = n;
  ret["k"] = k;
  ret["method"] = "C++";
  
  ret.attr("class") = Rcpp::CharacterVector::create("learndgm_modelstructure_list", "list");
  
  return ret;
}













std::vector<CherryModelMI> convert_models_to_cpp_for_MI(const Rcpp::List& models) {
  
  std::vector<CherryModelMI> ret_models;
  ret_models.reserve(models.size());
  
  for (int i = 0; i < models.size(); ++i) {
    Rcpp::List m = models[i];
    Rcpp::IntegerMatrix cliques = m["cliques"];
    Rcpp::IntegerMatrix seps = m["seps"];
    Rcpp::IntegerVector parents = m["parents"];
    
    std::vector< std::vector<int> > new_cliques(cliques.ncol());
    std::vector< std::vector<int> > new_seps(seps.ncol());
    std::vector<int> new_parents = Rcpp::as< std::vector<int> >(parents);
    
    // Cliques
    for (int j = 0; j < cliques.ncol(); ++j) {
      Rcpp::IntegerVector cj = cliques(Rcpp::_, j);
      std::vector<int> c = Rcpp::as< std::vector<int> >(cj);
      new_cliques[j] = c;
    }
    
    // Seps
    for (int j = 0; j < seps.ncol(); ++j) {
      Rcpp::IntegerVector cj = seps(Rcpp::_, j);
      std::vector<int> c = Rcpp::as< std::vector<int> >(cj);
      new_seps[j] = c;
    }
    
    CherryModelMI model = CherryModelMI(new_cliques, new_seps, new_parents);
    ret_models.push_back(model);
  }
  
  return ret_models;
}

Rcpp::List convert_models_to_r(const std::vector<CherryModelUnused>& models) {
  int n = models.size();
  Rcpp::List ret_models(n);
  
  for (int i = 0; i < n; ++i) {
    CherryModelUnused model = models[i];
    
    std::vector< std::vector<int> > cliques = model.get_cliques();
    std::vector< std::vector<int> > seps = model.get_seps();
    std::vector<int> parents = model.get_parents();
    std::vector<int> unused = model.get_unused();
    
    if (unused.size() != 0) {
      Rcpp::stop("unused.size() != 0");
    }
    
    int n_cliques = cliques.size();
    int n_seps = seps.size();
    
    if ((n_cliques - 1) != n_seps) {
      Rcpp::stop("(n_cliques - 1) != n_seps");
    }
    
    if (parents.size() != n_seps) {
      Rcpp::stop("parents.size() != n_seps");
    }
    
    /*
    Rcpp::List r_cliques(n_cliques);
    Rcpp::List r_seps(n_seps);
    
    // Cliques
    for (int j = 0; j < n_cliques; ++j) {
    std::vector<int> v = cliques[j];
    Rcpp::IntegerVector w = Rcpp::wrap(v);
    r_cliques[j] = w;
    }
    
    // Seps
    for (int j = 0; j < n_seps; ++j) {
    std::vector<int> v = seps[j];
    Rcpp::IntegerVector w = Rcpp::wrap(v);
    r_seps[j] = w;
    }
    */
    
    Rcpp::IntegerMatrix r_cliques(cliques[0].size(), n_cliques);
    Rcpp::IntegerMatrix r_seps(cliques[0].size() - 1, n_seps);
    
    // Cliques
    for (int j = 0; j < n_cliques; ++j) {
      std::vector<int> v = cliques[j];
      Rcpp::IntegerVector w = Rcpp::wrap(v);
      r_cliques(Rcpp::_, j) = w;
    }
    
    // Seps
    for (int j = 0; j < n_seps; ++j) {
      std::vector<int> v = seps[j];
      Rcpp::IntegerVector w = Rcpp::wrap(v);
      r_seps(Rcpp::_, j) = w;
    }
    
    Rcpp::IntegerVector r_parents(parents.size() + 1);
    r_parents[0] = Rcpp::IntegerVector::get_na();
    for (int i = 0; i < parents.size(); ++i) {
      r_parents[i + 1] = parents[i]; 
    }
    
    Rcpp::List r_model;
    r_model["cliques"] = r_cliques;
    r_model["seps"] = r_seps;
    r_model["parents"] = r_parents;
    ret_models[i] = r_model;
  }
  
  return ret_models;
}


Rcpp::List convert_final_MI_models_to_r(const std::vector<CherryModelMI>& models,
                                        const int n,
                                        const int k) {
  int n_models = models.size();
  Rcpp::List ret_models(n_models);
  Rcpp::NumericVector r_models_mi(n_models);
  
  for (int i = 0; i < n_models; ++i) {
    CherryModelMI model = models[i];
    
    std::vector< std::vector<int> > cliques = model.get_cliques();
    std::vector< std::vector<int> > seps = model.get_seps();
    std::vector<int> parents = model.get_parents();
    std::vector<double> cliques_mi = model.get_cliques_mi();
    std::vector<double> seps_mi = model.get_seps_mi();
    
    int n_cliques = cliques.size();
    int n_seps = seps.size();
    
    if ((n_cliques - 1) != n_seps) {
      Rcpp::Rcout << "n_cliques = " << n_cliques << "; n_seps = " << n_seps << std::endl;
      Rcpp::stop("MI: (n_cliques - 1) != n_seps");
    }
    
    // Now parents are of size cliques as the first is NA
    if (parents.size() != n_cliques) {
      Rcpp::print(Rcpp::wrap(parents));
      Rcpp::Rcout << "parents.size() = " << parents.size() << "; n_seps = " << n_seps << std::endl;
      Rcpp::stop("MI: parents.size() != n_cliques");
    }
    
    Rcpp::IntegerMatrix r_cliques(cliques[0].size(), n_cliques);
    Rcpp::IntegerMatrix r_seps(cliques[0].size() - 1, n_seps);
    
    // Cliques
    for (int j = 0; j < n_cliques; ++j) {
      std::vector<int> v = cliques[j];
      Rcpp::IntegerVector w = Rcpp::wrap(v);
      r_cliques(Rcpp::_, j) = w;
    }
    
    // Seps
    for (int j = 0; j < n_seps; ++j) {
      std::vector<int> v = seps[j];
      Rcpp::IntegerVector w = Rcpp::wrap(v);
      r_seps(Rcpp::_, j) = w;
    }
    
    // Parents already has NA in first
    Rcpp::IntegerVector r_parents = Rcpp::wrap(parents);
    if (r_parents.size() > 0) {
      if (!Rcpp::IntegerVector::is_na(r_parents[0])) {
        Rcpp::stop("Expected NA in first");
      }
    }

    
    Rcpp::NumericVector r_cliques_mi = Rcpp::wrap(cliques_mi);
    Rcpp::NumericVector r_seps_mi = Rcpp::wrap(seps_mi);
    
    double model_mi = std::accumulate(cliques_mi.begin(), cliques_mi.end(), 0.0);
    model_mi -= std::accumulate(seps_mi.begin(), seps_mi.end(), 0.0);
    
    Rcpp::List r_model;
    r_model["cliques"] = r_cliques;
    r_model["seps"] = r_seps;
    r_model["parents"] = r_parents;
    r_model["cliques_mi"] = r_cliques_mi;
    r_model["seps_mi"] = r_seps_mi;
    ret_models[i] = r_model;
    
    r_models_mi[i] = model_mi;
  }
  
  Rcpp::List ret;
  ret["models"] = ret_models;
  ret["models_mi"] = r_models_mi;
  ret["n"] = n;
  ret["k"] = k;
  ret["method"] = "C++";
  
  // FIXME: S3 class hierachy?
  
  ret.attr("class") = Rcpp::CharacterVector::create(
    "learndgm_mimodelstructure_list", 
    "learndgm_modelstructure_list",
    "list");
  
  return ret;
}