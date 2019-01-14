/*
 
 //' Remove duplicate models
 //' 
 //' @export
 // [[Rcpp::export]]
 Rcpp::List rcpp_all_tcherries_worker(const Rcpp::List& models, 
                                      const Rcpp::IntegerMatrix& kmin1_subsets_idx, 
int n_unused,
bool verbose) {

Rcpp::List ret_models = clone(models);

int km1 = kmin1_subsets_idx.nrow();
int k = km1 + 1;

int kmin1_subsets = kmin1_subsets_idx.ncol();
std::vector<Rcpp::IntegerVector> km1sets(kmin1_subsets);
for (int i_sep = 0; i_sep < kmin1_subsets; ++i_sep) {
Rcpp::IntegerVector v = kmin1_subsets_idx(Rcpp::_, i_sep);
km1sets[i_sep] = v - 1; // C++ indexing
}

for (int iter = 1; iter <= n_unused; ++iter) {
if (verbose) {
Rcpp::Rcout << "Adding unused variable #" << iter << ".\n";
}

Rcpp::List new_models;

for (int i_m = 0; i_m < ret_models.size(); ++i_m) {
Rcpp::List m = ret_models[i_m];

Rcpp::List cliques = m["cliques"];

Rcpp::IntegerVector last_clique = cliques[ cliques.size() - 1 ];

Rcpp::IntegerVector unused = m["unused"];

for (int i_unused = 0; i_unused < unused.size(); ++i_unused) {
int x_unused = unused[i_unused];

for (int i_sep = 0; i_sep < kmin1_subsets; ++i_sep) {
Rcpp::IntegerVector sep_idx = km1sets[i_sep];
Rcpp::IntegerVector sep = last_clique[sep_idx];

Rcpp::IntegerVector new_clique(k);
for (int tmp_i = 0; tmp_i < km1; ++tmp_i) {
new_clique[tmp_i] = sep[tmp_i];
}
new_clique[km1] = x_unused;

// sort for comparing (removing duplicates) later in:
// model_to_adjacency_matrix(): A[ clique[j1], clique[j2] ]
std::sort(new_clique.begin(), new_clique.end());

Rcpp::List new_model_cliques = cliques;
new_model_cliques.push_back(new_clique);

Rcpp::List new_model_seps = m["seps"];
new_model_seps.push_back(sep);

Rcpp::IntegerVector new_model_unused = unused;
new_model_unused.erase(i_unused);

Rcpp::List new_model;
new_model["cliques"] = new_model_cliques;
new_model["seps"] = new_model_seps;
new_model["unused"] = new_model_unused;

new_models.push_back(new_model);
}
}
}

ret_models = new_models;
}

return ret_models;
}

 
 
 //' Remove duplicate models
 //' 
 //' @export
 // [[Rcpp::export]]
 Rcpp::IntegerVector rcpp_remove_equal_models_worker(Rcpp::ListOf<Rcpp::IntegerVector> adj_mats_upper_tri) {
size_t n = adj_mats_upper_tri.size();

/ Saves the index of the model with a given hash.
 Models with same hash not all saved, but that's fine
 as they are equal. 
 Hence, when done, values of this map are the indices of 
 the models to keep.
/

adjmat_hashmap model_map;

for (int i = 0; i < n; ++i) {
  std::vector<int> upper_tri = Rcpp::as< std::vector<int> >(adj_mats_upper_tri[i]);
  //model_map.emplace(upper_tri, i);
  //model_map[upper_tri] = i; // insert if new, else skip
  
  auto iter = model_map.find(upper_tri);
  
  if (iter == model_map.end()) {
    auto pair = std::pair< std::vector<int>, int >{upper_tri, i};
    model_map.insert(pair);
  }
}

std::vector<int> indices_vec;
indices_vec.reserve(model_map.size());

for (auto& v : model_map) {
  indices_vec.push_back(v.second + 1); // R's 1-based indexing
}

return Rcpp::wrap(indices_vec);
 }
 
 
*/