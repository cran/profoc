#include <splines.h>

#include <misc.h>

#include <RcppArmadillo.h>
// include header file from splines2 package
#include <splines2Armadillo.h>

static arma::mat splines2_basis(const arma::vec &x,
                                const arma::vec &knots,
                                const unsigned int deg)
{
    splines2::BSpline bs_obj{x, deg, knots};
    return bs_obj.basis(true);
}

using namespace arma;

// [[Rcpp::export]]
arma::vec make_knots(const double &kstep, const double &a = 1, const int deg = 3, const bool &even = false)
{

    vec x;
    vec xa;
    vec xb;

    if (even)
    {
        x = arma::linspace((0 - deg * 2 * kstep), 1 - kstep, (1 / (2 * kstep) - 0.5) + deg + 1);
        xa = sign(x) % pow(arma::abs(x), a) / 2;
        xb = 1 - reverse(xa.subvec(0, xa.n_elem - 1));
    }
    else
    {
        x = arma::linspace(0 - deg * 2 * kstep, 1, 1 / (2 * kstep) + deg + 1);
        xa = sign(x) % pow(arma::abs(x), a) / 2;
        xb = 1 - reverse(xa.subvec(0, xa.n_elem - 2));
    }

    return join_cols(xa, xb);
}

// Expose splines::splineDesign to Rcpp
static mat splineDesign_rcpp(const vec &x, const vec &knots, const int &deg)
{
    Rcpp::Environment pkg = Rcpp::Environment::namespace_env("splines");
    Rcpp::Function f = pkg["splineDesign"];
    mat y = Rcpp::as<arma::mat>(f(knots, x, deg + 1, 0, true));
    return y;
}

static arma::mat make_difference_matrix(const arma::vec &knots, const int &bdiff, const int deg)
{
    int m = knots.n_elem - 2 * (deg)-2; // Number of inner knots
    const vec diag_vals = 1 / diff_cpp(knots, deg, 1);
    mat D = diff(diagmat(diag_vals), bdiff) / (m + 1) * deg;
    D = 0.5 * D.submat(1, 1, D.n_rows - 1, D.n_cols - 1) + 0.5 * D.submat(0, 0, D.n_rows - 2, D.n_cols - 2);
    return D;
}

// [[Rcpp::export]]
arma::sp_mat make_hat_matrix(
    const arma::vec &x,
    const double &kstep,
    const double &lambda,
    const double &bdiff,
    const int deg,
    const double &a,
    const bool &even)
{

    mat H;

    if (kstep <= 0.5)
    {
        vec knots = make_knots(kstep, a, deg, even);
        int m = knots.n_elem - 2 * (deg)-2; // Number of inner knots

        mat B = splines2_basis(x, knots, deg);

        mat P1(m + deg + 1, m + deg + 1);
        mat P2(m + deg + 1, m + deg + 1);
        mat P(m + deg + 1, m + deg + 1);

        mat D1 = make_difference_matrix(knots, 1, deg);
        P1 = D1.t() * D1;

        mat D2 = make_difference_matrix(knots, 2, deg);
        P2 = D2.t() * D2;

        P = (2 - bdiff) * P1 + (bdiff - 1) * P2;
        H = B * arma::pinv(B.t() * B + lambda * P) * B.t();
        H.clean(1E-10);
    }
    else
    {
        mat identity(x.n_elem, x.n_elem, fill::eye);
        H = identity;
    }

    arma::sp_mat sp_H = sp_mat(H); // Return hat matrix
    return sp_H;
}

// [[Rcpp::export]]
arma::sp_mat make_basis_matrix(const arma::vec &x, const double &kstep, const int deg, const double &a, const bool &even)
{
    mat B;

    // Will be passed to make_hat_matrix
    if (kstep <= 0.5)
    {
        vec knots = make_knots(kstep, a, deg, even);
        B = splines2_basis(x, knots, deg);
        // Remove columns without contribution
        B = B.cols(find(sum(B) >= 1E-6));
        B.clean(1E-10);
    }
    else
    {
        mat B_(x.n_elem, 1, fill::ones);
        B = B_;
    }

    sp_mat out(B);

    return out;
}

// [[Rcpp::export]]
arma::sp_mat make_basis_matrix2(const arma::vec &x,
                                const arma::vec &knots,
                                const unsigned int deg)
{
    mat B;

    if (knots.n_elem == 1)
    {
        mat B_(x.n_elem, 1, fill::ones);
        B = B_;
    }
    else
    {
        B = splines2_basis(x, knots, deg);
        // Remove columns without contribution
        B = B.cols(find(sum(B) >= 1E-6));
        B.clean(1E-10);
    }

    sp_mat out(B);

    return out;
}

// [[Rcpp::export]]
arma::sp_mat make_hat_matrix2(
    const arma::vec &x,
    const arma::vec &knots,
    const int deg,
    const double &bdiff,
    const double &lambda)
{

    mat H;

    int m = knots.n_elem - 2 * (deg)-2; // Number of inner knots

    mat B = splines2_basis(x, knots, deg);

    mat P1(m + deg + 1, m + deg + 1);
    mat P2(m + deg + 1, m + deg + 1);
    mat P(m + deg + 1, m + deg + 1);

    mat D1 = make_difference_matrix(knots, 1, deg);
    P1 = D1.t() * D1;

    mat D2 = make_difference_matrix(knots, 2, deg);
    P2 = D2.t() * D2;

    P = (2 - bdiff) * P1 + (bdiff - 1) * P2;
    H = B * arma::pinv(B.t() * B + lambda * P) * B.t();
    H.clean(1E-10);

    arma::sp_mat sp_H = sp_mat(H); // Return hat matrix
    return sp_H;
}
