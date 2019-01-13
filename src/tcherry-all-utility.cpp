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
  

//' Remove duplicate models
//' 
//' @export
// [[Rcpp::export]]
Rcpp::IntegerVector rcp__remove_equal_models_worker(Rcpp::ListOf<Rcpp::IntegerVector> adj_mats_upper_tri) {
  size_t n = adj_mats_upper_tri.size();
  
  /* Saves the index of the model with a given hash.
   * Models with same hash not all saved, but that's fine
   * as they are equal. 
   * Hence, when done, values of this map are the indices of 
   * the models to keep.
   */
  
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
