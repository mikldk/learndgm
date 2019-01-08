#include <Rcpp.h>

// [[Rcpp::plugins(cpp11)]]

#include <vector>

/* TODO:
 * -----
 * Must have:
 *  ?
 * 
 * Nice to have:
 *   - Sparse adjacency matrix
 */

void verify_prufer(Rcpp::IntegerVector prufer) {
  int n = prufer.size() + 2;
  
  for (int v : prufer) {
    if (v <= 0 || v > n) {
      Rcpp::stop("Prüfer sequence (prufer) of length n must have entries between 1 and n+2");
    }
  }
}

//' Prüfer sequence to tree
//' 
//' @export
// [[Rcpp::export]]
Rcpp::IntegerMatrix prufer_to_adjacency_matrix(Rcpp::IntegerVector prufer) {
  verify_prufer(prufer);
  
  int n = prufer.size() + 2;
  
  Rcpp::IntegerMatrix A(n, n);
  std::vector<int> degree(n);
  
  // for each node v: degree(v) = 1
  std::fill(degree.begin(), degree.end(), 1);
  
  /* 
   * NOTE:
   * -----
   * prufer has integers from 1 to n; convert them
   * to 0-based indices for the remaining algorithm
   */
  size_t m = prufer.size();
  for (int i = 0; i < m; ++i) {
    prufer[i] = prufer[i] - 1;
  }
  
  // for each value v in prufer: degree(v) = degree(v) + 1
  for (int i = 0; i < m; ++i) {
    degree[ prufer[i] ] += 1;
  }
  
  /* for each value v in prufer, find the first (lowest-numbered) node, j, 
   * with degree equal to 1, add the edge (j, v) to the tree, and decrement the 
   * degrees of j and v
   */
  for (int i = 0; i < m; ++i) {
    int v = prufer[i];
    
    for (int j = 0; j < n; ++j) {
      if (degree[j] == 1) {
        if (v < j) {
          A(v, j) = 1;
        } else {
          A(j, v) = 1;
        }
        //A(v, j) = 1;
        //A(j, v) = 1; // FIXME?

        degree[v] -= 1;
        degree[j] -= 1;
        break;
      }
    }
  }
  
  // Two nodes with degree 1 will remain: u, v. Add the edge (u,v) to the tree.
  int degree_left = std::accumulate(degree.begin(), degree.end(), 0);
  if (degree_left != 2) {
    Rcpp::stop("Unexpected degree_left != 2");
  }
  
  int u = -1, v = -1;
  
  for (int j = 0; j < n; ++j) {
    if (degree[j] == 1) {
      if (u == -1) {
        u = j;
      } else if (v == -1) {
        v = j;
      } else {
        Rcpp::stop("Unexpected degree = 1 when both u and v set");
      }
    }
  }
  
  if (u < v) {
    A(u, v) = 1;
  } else {
    A(v, u) = 1;
  }
  //A(u, v) = 1;
  //A(v, u) = 1; // FIXME?
  
  return A;
}
