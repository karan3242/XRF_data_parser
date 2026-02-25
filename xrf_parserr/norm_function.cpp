#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
DataFrame normalization_cpp_std(DataFrame df, CharacterVector cols) {
  // 1. Extract Lab_Id and target columns
  CharacterVector lab_id = df["Lab_Id"];
  int n_rows = df.nrows();
  int n_cols = cols.size();
  
  // Create a temporary matrix to hold numeric data for calculation
  NumericMatrix mat(n_rows, n_cols);
  for(int j = 0; j < n_cols; ++j) {
    mat(_, j) = as<NumericVector>(df[String(cols[j])]);
  }
  
  // 2. Row-wise Normalization
  for (int i = 0; i < n_rows; ++i) {
    double row_sum = 0;
    
    // First pass: Calculate sum of the row
    for (int j = 0; j < n_cols; ++j) {
      double val = mat(i, j);
      if (!NumericVector::is_na(val)) {
        row_sum += val;
      }
    }
    
    // Second pass: Divide by sum and multiply by 100
    for (int j = 0; j < n_cols; ++j) {
      if (row_sum == 0) {
        mat(i, j) = 0;
      } else {
        mat(i, j) = (mat(i, j) / row_sum) * 100.0;
      }
    }
  }
  
  // 3. Reconstruct the DataFrame
  List out_list(n_cols + 1);
  CharacterVector out_names(n_cols + 1);
  
  out_list[0] = lab_id;
  out_names[0] = "Lab_Id";
  
  for (int j = 0; j < n_cols; ++j) {
    out_list[j + 1] = mat(_, j);
    out_names[j + 1] = cols[j];
  }
  
  out_list.attr("names") = out_names;
  out_list.attr("class") = "data.frame";
  out_list.attr("row.names") = seq(1, n_rows);
  
  return out_list;
}