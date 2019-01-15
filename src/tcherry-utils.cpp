#include <Rcpp.h>

// [[Rcpp::plugins(cpp11)]]

#include <vector>

#include "class_CherryModel.h"

std::vector<CherryModelUnused> convert_models_to_cpp(const Rcpp::List& models) {
  std::vector<CherryModelUnused> ret_models;
  ret_models.reserve(models.size());
  
  for (int i = 0; i < models.size(); ++i) {
    Rcpp::List m = models[i];
    Rcpp::List cliques = m["cliques"];
    Rcpp::List seps = m["seps"];
    Rcpp::IntegerVector parents = m["parents"];
    Rcpp::IntegerVector unused = m["unused"];
    
    std::vector< std::vector<int> > new_cliques(cliques.size());
    std::vector< std::vector<int> > new_seps(seps.size());
    std::vector<int> new_parents = Rcpp::as< std::vector<int> >(parents);
    std::vector<int> new_unused = Rcpp::as< std::vector<int> >(unused);
    
    // Cliques
    for (int j = 0; j < cliques.size(); ++j) {
      Rcpp::IntegerVector cj = cliques[j];
      std::vector<int> c = Rcpp::as< std::vector<int> >(cj);
      new_cliques[j] = c;
    }
    
    // Seps
    for (int j = 0; j < cliques.size(); ++j) {
      Rcpp::IntegerVector cj = cliques[j];
      std::vector<int> c = Rcpp::as< std::vector<int> >(cj);
      new_cliques[j] = c;
    }
    
    CherryModelUnused model = CherryModelUnused(new_cliques, new_seps, new_parents, new_unused);
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













std::vector<CherryModelMI> convert_models_to_MI_cpp(const Rcpp::List& models) {
  std::vector<CherryModelMI> ret_models;
  ret_models.reserve(models.size());
  
  for (int i = 0; i < models.size(); ++i) {
    Rcpp::List m = models[i];
    Rcpp::List cliques = m["cliques"];
    Rcpp::List seps = m["seps"];
    Rcpp::IntegerVector parents = m["parents"];
    
    std::vector< std::vector<int> > new_cliques(cliques.size());
    std::vector< std::vector<int> > new_seps(seps.size());
    std::vector<int> new_parents = Rcpp::as< std::vector<int> >(parents);
    
    // Cliques
    for (int j = 0; j < cliques.size(); ++j) {
      Rcpp::IntegerVector cj = cliques[j];
      std::vector<int> c = Rcpp::as< std::vector<int> >(cj);
      new_cliques[j] = c;
    }
    
    // Seps
    for (int j = 0; j < cliques.size(); ++j) {
      Rcpp::IntegerVector cj = cliques[j];
      std::vector<int> c = Rcpp::as< std::vector<int> >(cj);
      new_cliques[j] = c;
    }
    
    CherryModelMI model = CherryModelMI(new_cliques, new_seps, new_parents);
    ret_models.push_back(model);
  }
  
  return ret_models;
}
