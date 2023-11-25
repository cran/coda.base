// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// pinv
arma::mat pinv(arma::mat X);
RcppExport SEXP _coda_base_pinv(SEXP XSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type X(XSEXP);
    rcpp_result_gen = Rcpp::wrap(pinv(X));
    return rcpp_result_gen;
END_RCPP
}
// c_variation_array
arma::mat c_variation_array(arma::mat X, bool include_means);
RcppExport SEXP _coda_base_c_variation_array(SEXP XSEXP, SEXP include_meansSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type X(XSEXP);
    Rcpp::traits::input_parameter< bool >::type include_means(include_meansSEXP);
    rcpp_result_gen = Rcpp::wrap(c_variation_array(X, include_means));
    return rcpp_result_gen;
END_RCPP
}
// alr_basis_default
arma::mat alr_basis_default(unsigned int dim);
RcppExport SEXP _coda_base_alr_basis_default(SEXP dimSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< unsigned int >::type dim(dimSEXP);
    rcpp_result_gen = Rcpp::wrap(alr_basis_default(dim));
    return rcpp_result_gen;
END_RCPP
}
// clr_basis_default
arma::mat clr_basis_default(unsigned int dim);
RcppExport SEXP _coda_base_clr_basis_default(SEXP dimSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< unsigned int >::type dim(dimSEXP);
    rcpp_result_gen = Rcpp::wrap(clr_basis_default(dim));
    return rcpp_result_gen;
END_RCPP
}
// ilr_basis_default
arma::mat ilr_basis_default(unsigned int dim);
RcppExport SEXP _coda_base_ilr_basis_default(SEXP dimSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< unsigned int >::type dim(dimSEXP);
    rcpp_result_gen = Rcpp::wrap(ilr_basis_default(dim));
    return rcpp_result_gen;
END_RCPP
}
// ilr_basis_simplex
arma::mat ilr_basis_simplex(unsigned int dim);
RcppExport SEXP _coda_base_ilr_basis_simplex(SEXP dimSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< unsigned int >::type dim(dimSEXP);
    rcpp_result_gen = Rcpp::wrap(ilr_basis_simplex(dim));
    return rcpp_result_gen;
END_RCPP
}
// ilr_to_alr
arma::mat ilr_to_alr(unsigned int dim);
RcppExport SEXP _coda_base_ilr_to_alr(SEXP dimSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< unsigned int >::type dim(dimSEXP);
    rcpp_result_gen = Rcpp::wrap(ilr_to_alr(dim));
    return rcpp_result_gen;
END_RCPP
}
// clr_coordinates
arma::mat clr_coordinates(arma::mat& X);
RcppExport SEXP _coda_base_clr_coordinates(SEXP XSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat& >::type X(XSEXP);
    rcpp_result_gen = Rcpp::wrap(clr_coordinates(X));
    return rcpp_result_gen;
END_RCPP
}
// inv_clr_coordinates
arma::mat inv_clr_coordinates(arma::mat clrX);
RcppExport SEXP _coda_base_inv_clr_coordinates(SEXP clrXSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type clrX(clrXSEXP);
    rcpp_result_gen = Rcpp::wrap(inv_clr_coordinates(clrX));
    return rcpp_result_gen;
END_RCPP
}
// alr_coordinates
arma::mat alr_coordinates(arma::mat& X, int denominator);
RcppExport SEXP _coda_base_alr_coordinates(SEXP XSEXP, SEXP denominatorSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat& >::type X(XSEXP);
    Rcpp::traits::input_parameter< int >::type denominator(denominatorSEXP);
    rcpp_result_gen = Rcpp::wrap(alr_coordinates(X, denominator));
    return rcpp_result_gen;
END_RCPP
}
// matrix_coordinates
arma::mat matrix_coordinates(arma::mat X, arma::mat B);
RcppExport SEXP _coda_base_matrix_coordinates(SEXP XSEXP, SEXP BSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type X(XSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type B(BSEXP);
    rcpp_result_gen = Rcpp::wrap(matrix_coordinates(X, B));
    return rcpp_result_gen;
END_RCPP
}
// sparse_coordinates
arma::mat sparse_coordinates(arma::mat X, arma::sp_mat B);
RcppExport SEXP _coda_base_sparse_coordinates(SEXP XSEXP, SEXP BSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type X(XSEXP);
    Rcpp::traits::input_parameter< arma::sp_mat >::type B(BSEXP);
    rcpp_result_gen = Rcpp::wrap(sparse_coordinates(X, B));
    return rcpp_result_gen;
END_RCPP
}
// coordinates_basis
arma::mat coordinates_basis(arma::mat X, arma::mat B, bool sparse);
RcppExport SEXP _coda_base_coordinates_basis(SEXP XSEXP, SEXP BSEXP, SEXP sparseSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type X(XSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type B(BSEXP);
    Rcpp::traits::input_parameter< bool >::type sparse(sparseSEXP);
    rcpp_result_gen = Rcpp::wrap(coordinates_basis(X, B, sparse));
    return rcpp_result_gen;
END_RCPP
}
// ilr_coordinates
arma::mat ilr_coordinates(arma::mat& X);
RcppExport SEXP _coda_base_ilr_coordinates(SEXP XSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat& >::type X(XSEXP);
    rcpp_result_gen = Rcpp::wrap(ilr_coordinates(X));
    return rcpp_result_gen;
END_RCPP
}
// inv_ilr_coordinates
arma::mat inv_ilr_coordinates(arma::mat ilrX);
RcppExport SEXP _coda_base_inv_ilr_coordinates(SEXP ilrXSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type ilrX(ilrXSEXP);
    rcpp_result_gen = Rcpp::wrap(inv_ilr_coordinates(ilrX));
    return rcpp_result_gen;
END_RCPP
}
// get_balance_using_pc
arma::vec get_balance_using_pc(arma::mat& X);
RcppExport SEXP _coda_base_get_balance_using_pc(SEXP XSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat& >::type X(XSEXP);
    rcpp_result_gen = Rcpp::wrap(get_balance_using_pc(X));
    return rcpp_result_gen;
END_RCPP
}
// find_PB
arma::mat find_PB(arma::mat& X);
RcppExport SEXP _coda_base_find_PB(SEXP XSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat& >::type X(XSEXP);
    rcpp_result_gen = Rcpp::wrap(find_PB(X));
    return rcpp_result_gen;
END_RCPP
}
// find_PB_using_pc
arma::mat find_PB_using_pc(arma::mat& X);
RcppExport SEXP _coda_base_find_PB_using_pc(SEXP XSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat& >::type X(XSEXP);
    rcpp_result_gen = Rcpp::wrap(find_PB_using_pc(X));
    return rcpp_result_gen;
END_RCPP
}
// find_PB_using_pc_recursively
arma::mat find_PB_using_pc_recursively(arma::mat& X);
RcppExport SEXP _coda_base_find_PB_using_pc_recursively(SEXP XSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat& >::type X(XSEXP);
    rcpp_result_gen = Rcpp::wrap(find_PB_using_pc_recursively(X));
    return rcpp_result_gen;
END_RCPP
}
// find_PB_using_pc_recursively_forcing_parents
arma::mat find_PB_using_pc_recursively_forcing_parents(arma::mat& X);
RcppExport SEXP _coda_base_find_PB_using_pc_recursively_forcing_parents(SEXP XSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat& >::type X(XSEXP);
    rcpp_result_gen = Rcpp::wrap(find_PB_using_pc_recursively_forcing_parents(X));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_coda_base_pinv", (DL_FUNC) &_coda_base_pinv, 1},
    {"_coda_base_c_variation_array", (DL_FUNC) &_coda_base_c_variation_array, 2},
    {"_coda_base_alr_basis_default", (DL_FUNC) &_coda_base_alr_basis_default, 1},
    {"_coda_base_clr_basis_default", (DL_FUNC) &_coda_base_clr_basis_default, 1},
    {"_coda_base_ilr_basis_default", (DL_FUNC) &_coda_base_ilr_basis_default, 1},
    {"_coda_base_ilr_basis_simplex", (DL_FUNC) &_coda_base_ilr_basis_simplex, 1},
    {"_coda_base_ilr_to_alr", (DL_FUNC) &_coda_base_ilr_to_alr, 1},
    {"_coda_base_clr_coordinates", (DL_FUNC) &_coda_base_clr_coordinates, 1},
    {"_coda_base_inv_clr_coordinates", (DL_FUNC) &_coda_base_inv_clr_coordinates, 1},
    {"_coda_base_alr_coordinates", (DL_FUNC) &_coda_base_alr_coordinates, 2},
    {"_coda_base_matrix_coordinates", (DL_FUNC) &_coda_base_matrix_coordinates, 2},
    {"_coda_base_sparse_coordinates", (DL_FUNC) &_coda_base_sparse_coordinates, 2},
    {"_coda_base_coordinates_basis", (DL_FUNC) &_coda_base_coordinates_basis, 3},
    {"_coda_base_ilr_coordinates", (DL_FUNC) &_coda_base_ilr_coordinates, 1},
    {"_coda_base_inv_ilr_coordinates", (DL_FUNC) &_coda_base_inv_ilr_coordinates, 1},
    {"_coda_base_get_balance_using_pc", (DL_FUNC) &_coda_base_get_balance_using_pc, 1},
    {"_coda_base_find_PB", (DL_FUNC) &_coda_base_find_PB, 1},
    {"_coda_base_find_PB_using_pc", (DL_FUNC) &_coda_base_find_PB_using_pc, 1},
    {"_coda_base_find_PB_using_pc_recursively", (DL_FUNC) &_coda_base_find_PB_using_pc_recursively, 1},
    {"_coda_base_find_PB_using_pc_recursively_forcing_parents", (DL_FUNC) &_coda_base_find_PB_using_pc_recursively_forcing_parents, 1},
    {NULL, NULL, 0}
};

RcppExport void R_init_coda_base(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
