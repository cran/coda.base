// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

// c_variation_array
arma::mat c_variation_array(arma::mat X, bool only_variation);
RcppExport SEXP _coda_base_c_variation_array(SEXP XSEXP, SEXP only_variationSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type X(XSEXP);
    Rcpp::traits::input_parameter< bool >::type only_variation(only_variationSEXP);
    rcpp_result_gen = Rcpp::wrap(c_variation_array(X, only_variation));
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
arma::mat clr_coordinates(arma::mat X);
RcppExport SEXP _coda_base_clr_coordinates(SEXP XSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type X(XSEXP);
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
// coordinates_alr2
arma::mat coordinates_alr2(arma::mat X, int denominator);
RcppExport SEXP _coda_base_coordinates_alr2(SEXP XSEXP, SEXP denominatorSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type X(XSEXP);
    Rcpp::traits::input_parameter< int >::type denominator(denominatorSEXP);
    rcpp_result_gen = Rcpp::wrap(coordinates_alr2(X, denominator));
    return rcpp_result_gen;
END_RCPP
}
// coordinates_alr
arma::mat coordinates_alr(arma::mat X, int denominator);
RcppExport SEXP _coda_base_coordinates_alr(SEXP XSEXP, SEXP denominatorSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type X(XSEXP);
    Rcpp::traits::input_parameter< int >::type denominator(denominatorSEXP);
    rcpp_result_gen = Rcpp::wrap(coordinates_alr(X, denominator));
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
arma::mat ilr_coordinates(arma::mat X);
RcppExport SEXP _coda_base_ilr_coordinates(SEXP XSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type X(XSEXP);
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
// find_PB_rnd_local_search
arma::mat find_PB_rnd_local_search(arma::mat M, int rep);
RcppExport SEXP _coda_base_find_PB_rnd_local_search(SEXP MSEXP, SEXP repSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type M(MSEXP);
    Rcpp::traits::input_parameter< int >::type rep(repSEXP);
    rcpp_result_gen = Rcpp::wrap(find_PB_rnd_local_search(M, rep));
    return rcpp_result_gen;
END_RCPP
}
// find_PB_pc_local_search
arma::mat find_PB_pc_local_search(arma::mat X);
RcppExport SEXP _coda_base_find_PB_pc_local_search(SEXP XSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type X(XSEXP);
    rcpp_result_gen = Rcpp::wrap(find_PB_pc_local_search(X));
    return rcpp_result_gen;
END_RCPP
}
// find_PB_log
arma::mat find_PB_log(arma::mat lX);
RcppExport SEXP _coda_base_find_PB_log(SEXP lXSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type lX(lXSEXP);
    rcpp_result_gen = Rcpp::wrap(find_PB_log(lX));
    return rcpp_result_gen;
END_RCPP
}
// find_PB
arma::mat find_PB(arma::mat X);
RcppExport SEXP _coda_base_find_PB(SEXP XSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type X(XSEXP);
    rcpp_result_gen = Rcpp::wrap(find_PB(X));
    return rcpp_result_gen;
END_RCPP
}
// find_PB2
arma::mat find_PB2(arma::mat M, int random, int optim);
RcppExport SEXP _coda_base_find_PB2(SEXP MSEXP, SEXP randomSEXP, SEXP optimSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type M(MSEXP);
    Rcpp::traits::input_parameter< int >::type random(randomSEXP);
    Rcpp::traits::input_parameter< int >::type optim(optimSEXP);
    rcpp_result_gen = Rcpp::wrap(find_PB2(M, random, optim));
    return rcpp_result_gen;
END_RCPP
}
// find_PB3
arma::mat find_PB3(arma::mat M, int steps, int random, int optim, int k);
RcppExport SEXP _coda_base_find_PB3(SEXP MSEXP, SEXP stepsSEXP, SEXP randomSEXP, SEXP optimSEXP, SEXP kSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type M(MSEXP);
    Rcpp::traits::input_parameter< int >::type steps(stepsSEXP);
    Rcpp::traits::input_parameter< int >::type random(randomSEXP);
    Rcpp::traits::input_parameter< int >::type optim(optimSEXP);
    Rcpp::traits::input_parameter< int >::type k(kSEXP);
    rcpp_result_gen = Rcpp::wrap(find_PB3(M, steps, random, optim, k));
    return rcpp_result_gen;
END_RCPP
}
// find_PB4
arma::mat find_PB4(arma::mat M);
RcppExport SEXP _coda_base_find_PB4(SEXP MSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type M(MSEXP);
    rcpp_result_gen = Rcpp::wrap(find_PB4(M));
    return rcpp_result_gen;
END_RCPP
}
// find_PB5
arma::mat find_PB5(arma::mat X);
RcppExport SEXP _coda_base_find_PB5(SEXP XSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type X(XSEXP);
    rcpp_result_gen = Rcpp::wrap(find_PB5(X));
    return rcpp_result_gen;
END_RCPP
}
// arma_sampling_without_replacement
arma::uvec arma_sampling_without_replacement(int n, int k);
RcppExport SEXP _coda_base_arma_sampling_without_replacement(SEXP nSEXP, SEXP kSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    Rcpp::traits::input_parameter< int >::type k(kSEXP);
    rcpp_result_gen = Rcpp::wrap(arma_sampling_without_replacement(n, k));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_coda_base_c_variation_array", (DL_FUNC) &_coda_base_c_variation_array, 2},
    {"_coda_base_alr_basis_default", (DL_FUNC) &_coda_base_alr_basis_default, 1},
    {"_coda_base_clr_basis_default", (DL_FUNC) &_coda_base_clr_basis_default, 1},
    {"_coda_base_ilr_basis_default", (DL_FUNC) &_coda_base_ilr_basis_default, 1},
    {"_coda_base_ilr_basis_simplex", (DL_FUNC) &_coda_base_ilr_basis_simplex, 1},
    {"_coda_base_ilr_to_alr", (DL_FUNC) &_coda_base_ilr_to_alr, 1},
    {"_coda_base_clr_coordinates", (DL_FUNC) &_coda_base_clr_coordinates, 1},
    {"_coda_base_inv_clr_coordinates", (DL_FUNC) &_coda_base_inv_clr_coordinates, 1},
    {"_coda_base_coordinates_alr2", (DL_FUNC) &_coda_base_coordinates_alr2, 2},
    {"_coda_base_coordinates_alr", (DL_FUNC) &_coda_base_coordinates_alr, 2},
    {"_coda_base_coordinates_basis", (DL_FUNC) &_coda_base_coordinates_basis, 3},
    {"_coda_base_ilr_coordinates", (DL_FUNC) &_coda_base_ilr_coordinates, 1},
    {"_coda_base_inv_ilr_coordinates", (DL_FUNC) &_coda_base_inv_ilr_coordinates, 1},
    {"_coda_base_find_PB_rnd_local_search", (DL_FUNC) &_coda_base_find_PB_rnd_local_search, 2},
    {"_coda_base_find_PB_pc_local_search", (DL_FUNC) &_coda_base_find_PB_pc_local_search, 1},
    {"_coda_base_find_PB_log", (DL_FUNC) &_coda_base_find_PB_log, 1},
    {"_coda_base_find_PB", (DL_FUNC) &_coda_base_find_PB, 1},
    {"_coda_base_find_PB2", (DL_FUNC) &_coda_base_find_PB2, 3},
    {"_coda_base_find_PB3", (DL_FUNC) &_coda_base_find_PB3, 5},
    {"_coda_base_find_PB4", (DL_FUNC) &_coda_base_find_PB4, 1},
    {"_coda_base_find_PB5", (DL_FUNC) &_coda_base_find_PB5, 1},
    {"_coda_base_arma_sampling_without_replacement", (DL_FUNC) &_coda_base_arma_sampling_without_replacement, 2},
    {NULL, NULL, 0}
};

RcppExport void R_init_coda_base(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
