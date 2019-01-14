#include <Rcpp.h>

// [[Rcpp::plugins(cpp11)]]

#include <vector>
#include <utility>
#include <unordered_map>

namespace{
// a little helper that should IMHO be standardized
template<typename T>
std::size_t make_hash(const T& v){
  return std::hash<T>()(v);
}

// adapted from boost::hash_combine
void hash_combine(std::size_t& h, const std::size_t& v){
  h ^= v + 0x9e3779b9 + (h << 6) + (h >> 2);
}

// hash any container
template<typename T>
struct hash_container{
  size_t operator()(const T& v) const{
    size_t h = 0;
    for( const auto& e : v ) {
      hash_combine(h, make_hash(e));
    }
    return h;
  }
};
}

namespace std{
// support for vector<T> if T is hashable
// (the T... is a required trick if the vector has a non-standard allocator)
template<>
struct hash< std::vector<int> > : hash_container< std::vector<int> > {};

struct equal_to_intvec : binary_function< std::vector<int>, std::vector<int>, bool> {
  bool operator() (const std::vector<int>& x, const std::vector<int>& y) const{
    if (x.size() != y.size()){
      return false;
    }
    
    int n = x.size();
    
    for (int i = 0; i < n; ++i) {
      if (x[i] != y[i]) {
        return false;
      }
    }
    
    return true;
  }
};

// the same for map<T,U> if T and U are hashable
template<typename... T>
struct hash<map<T...>> : hash_container<map<T...>> {};

// simply add more containers as needed
}

typedef std::unordered_map< std::vector<int> , 
                            int, 
                            std::hash< std::vector<int> >, 
                            std::equal_to_intvec > 
  adjmat_hashmap;
  











/////////////////////////////////////////////////////////////

class CherryModel {
  
private:
  std::vector< std::vector<int> > m_cliques;
  std::vector< std::vector<int> > m_seps;
  std::vector<int> m_unused;
  
public:
  CherryModel(const std::vector< std::vector<int> >& cliques,
              const std::vector< std::vector<int> >& seps,
              const std::vector<int>& unused) {
    m_cliques = cliques;
    m_seps = seps;
    m_unused = unused;
  }
  
  const std::vector< std::vector<int> > get_cliques() {
    return m_cliques;
  }
  
  const std::vector< std::vector<int> > get_seps() {
    return m_seps;
  }
  
  const std::vector<int> get_unused() {
    return m_unused; 
  }
};

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

// @param n Number of variables in the model, used for size of adjacency matrix
std::vector<CherryModel> rcpp_new_remove_equal_models_worker(const std::vector<CherryModel>& models,
                                                             int n) {
  /* Saves the index of the model with a given hash.
  * Models with same hash not all saved, but that's fine
  * as they are equal. 
  * Hence, when done, values of this map are the indices of 
  * the models to keep.
  */
  
  adjmat_hashmap model_map;
    
  for (int i = 0; i < models.size(); ++i) {
    CherryModel m = models[i];
    
    std::vector<int> upper_tri = cliques_to_upper_tri_adj_mat(m.get_cliques(), n);

    auto iter = model_map.find(upper_tri);
    
    if (iter == model_map.end()) {
      auto pair = std::pair< std::vector<int>, int >{upper_tri, i};
      model_map.insert(pair);
    }
  }
  
  std::vector<CherryModel> new_models;
  new_models.reserve(model_map.size());
  
  for (auto& v : model_map) {
    new_models.push_back(models[v.second]);
  }
  
  return new_models;
}


std::vector<CherryModel> convert_models_to_cpp(const Rcpp::List& models) {
  std::vector<CherryModel> ret_models;
  ret_models.reserve(models.size());
  
  for (int i = 0; i < models.size(); ++i) {
    Rcpp::List m = models[i];
    Rcpp::List cliques = m["cliques"];
    Rcpp::List seps = m["seps"];
    Rcpp::IntegerVector unused = m["unused"];
    
    std::vector< std::vector<int> > new_cliques(cliques.size());
    std::vector< std::vector<int> > new_seps(seps.size());
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
    
    CherryModel model = CherryModel(new_cliques, new_seps, new_unused);
    ret_models.push_back(model);
  }
  
  return ret_models;
}

Rcpp::List convert_models_to_r(const std::vector<CherryModel>& models) {
  int n = models.size();
  Rcpp::List ret_models(n);
  
  for (int i = 0; i < n; ++i) {
    CherryModel model = models[i];
    
    std::vector< std::vector<int> > cliques = model.get_cliques();
    std::vector< std::vector<int> > seps = model.get_seps();
    std::vector<int> unused = model.get_unused();
    
    if (unused.size() != 0) {
      Rcpp::stop("unused.size() != 0");
    }
    
    int n_cliques = cliques.size();
    int n_seps = seps.size();
    
    if ((n_cliques - 1) != n_seps) {
      Rcpp::stop("(n_cliques - 1) != n_seps");
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
    
    Rcpp::List r_model;
    r_model["cliques"] = r_cliques;
    r_model["seps"] = r_seps;
    ret_models[i] = r_model;
  }
  
  return ret_models;
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
Rcpp::List rcpp_new_all_tcherries_worker(const Rcpp::List& models, 
                                         const Rcpp::IntegerMatrix& kmin1_subsets_idx, 
                                         int n,
                                         int n_unused,
                                         bool verbose, 
                                         bool remove_duplicates) {
  
  std::vector<CherryModel> ret_models = convert_models_to_cpp(models);
  
  int km1 = kmin1_subsets_idx.nrow();
  int k = km1 + 1;
  std::vector< std::vector<int> > km1sets = get_subsets(kmin1_subsets_idx);
  int kmin1_subsets = km1sets.size();
  
  for (int iter = 1; iter <= n_unused; ++iter) {
    if (verbose) {
      Rcpp::Rcout << "Adding unused variable #" << iter << ".\n";
    }
    
    std::vector<CherryModel> new_models;
    
    for (int i_m = 0; i_m < ret_models.size(); ++i_m) {
      CherryModel model = ret_models[i_m];
      
      std::vector< std::vector<int> > cliques = model.get_cliques();
      std::vector< std::vector<int> > seps = model.get_seps();
      std::vector<int> last_clique = cliques.back();
      std::vector<int> unused = model.get_unused();
      
      for (int i_unused = 0; i_unused < unused.size(); ++i_unused) {
        int x_unused = unused[i_unused];
        
        std::vector<int> new_unused = unused;
        new_unused.erase(new_unused.begin() + i_unused);
        
        for (int i_sep = 0; i_sep < kmin1_subsets; ++i_sep) {
          // New separator
          std::vector<int> sep_idx = km1sets[i_sep];
          std::vector<int> new_sep(km1);
          for (int i_new_sep = 0; i_new_sep < km1; ++i_new_sep) {
            new_sep[i_new_sep] = last_clique[ sep_idx[i_new_sep] ]; 
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
          
          
          std::vector< std::vector<int> > new_cliques = cliques;
          new_cliques.push_back(new_clique);
          
          std::vector< std::vector<int> > new_seps = seps;
          new_seps.push_back(new_sep);
          
          CherryModel new_model = CherryModel(new_cliques, new_seps, new_unused);

          new_models.push_back(new_model);
        }
      }
    }
    
    ret_models = new_models;
  }
  
  if (remove_duplicates) {
    ret_models = rcpp_new_remove_equal_models_worker(ret_models, n);
  }
  
  Rcpp::List r_ret_models = convert_models_to_r(ret_models);
  
  return r_ret_models;
}
