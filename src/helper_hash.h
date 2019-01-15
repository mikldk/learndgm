#ifndef HELPER_HASH_H
#define HELPER_HASH_H

#include <vector>
#include <unordered_map>

std::size_t hash_combine(std::size_t lhs, std::size_t rhs);

struct pairhash {
public:
  template <typename T, typename U>
  std::size_t operator()(const std::pair<T, U> &x) const
  {
    return hash_combine(x.first, x.second);
  }
};


struct vechash {
public:
  template <typename T>
  std::size_t operator()(const std::vector<T> &x) const
  {
    std::size_t h = 0;
    
    for (std::size_t i = 0; i < x.size(); ++i) {
      h = hash_combine(h, x[i]);
    }
    
    return h;
  }
};

struct equal_to_intvec : std::binary_function< std::vector<int>, std::vector<int>, bool> {
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


typedef std::unordered_map< std::vector<int> , 
                            int, 
                            vechash, 
                            equal_to_intvec > 
  hashmap_intvec_int;

#endif
