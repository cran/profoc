#include <RcppArmadillo.h>
#include <progress.hpp>
#include <thread>

#include "misc.h"
#include "loss.h"
#include "clock.h"
#include "conline.h"
#include "profoc_types.h"

using namespace arma;

// conline class was exposed via "profoc_types.h"
// So we can use it here as input and output if necessary

// Constructor

// Getters

// Methods

void conline::set_defaults()
{
    // Expand tau if necessary
    if (tau.n_elem == 1)
    {
        tau.resize(P);
        tau.fill(tau(0));
    }

    // Initial params
    w0.ones(D, P, K);
    w0 /= K;

    R0.zeros(D, P, K);
}

void conline::set_grid_objects()
{

    opt_index.zeros(T + 1);
    past_performance.set_size(T);
    tmp_performance.zeros(X);
    cum_performance.zeros(X);

    predictions.zeros(T + T_E_Y, D, P);
    predictions_tmp.set_size(T + T_E_Y);
    weights_tmp.set_size(X);
    weights.set_size(T + 1);

    loss_for.zeros(T, D, P);
    loss_exp.set_size(T);

    V.set_size(X);
    E.set_size(X);
    k.set_size(X);
    eta.set_size(X);
    R.set_size(X);
    R_reg.set_size(X);
    beta.set_size(X);
    beta0field.set_size(X);

    for (unsigned int x = 0; x < X; x++)
    {
        clock.tick("init");
        unsigned int Pr = basis_pr(params["basis_pr_idx"](x) - 1).n_cols;
        unsigned int Dr = basis_mv(params["basis_mv_idx"](x) - 1).n_cols;

        // Learning parameters
        V(x).zeros(Dr, Pr, K);
        E(x).zeros(Dr, Pr, K);
        k(x).zeros(Dr, Pr, K);

        arma::cube eta_(Dr, Pr, K, fill::zeros);
        eta(x) = eta_;
        if (method == "ml_poly")
        {
            eta_.fill(exp(350));
            eta(x) = eta_;
        }

        R(x).set_size(Dr, Pr, K);
        R_reg(x).set_size(Dr, Pr, K);
        beta(x).set_size(Dr, Pr, K);
        weights_tmp(x).set_size(D, P, K);

        for (unsigned int d = 0; d < D; d++)
        {
            weights_tmp(x).row(d) = w0.row(d);
        }

        for (unsigned int k = 0; k < K; k++)
        {
            R(x).slice(k) = basis_mv(params["basis_mv_idx"](x) - 1).t() *
                            R0.slice(k) *
                            basis_pr(params["basis_pr_idx"](x) - 1);
            R_reg(x).slice(k) = basis_mv(params["basis_mv_idx"](x) - 1).t() *
                                R0.slice(k) *
                                basis_pr(params["basis_pr_idx"](x) - 1);
            beta(x).slice(k) = pinv(
                                   mat(basis_mv(params["basis_mv_idx"](x) - 1))) *
                               w0.slice(k) *
                               pinv(mat(basis_pr(params["basis_pr_idx"](x) - 1))).t();
        }
        beta0field(x) = beta(x);
    }

    // Predictions at t < lead_time using initial weights
    for (unsigned int t = 0; t < lead_time; t++)
    {

        weights(t).set_size(D, P, K);
        past_performance(t).set_size(D, P, X);
        predictions_tmp(t).set_size(D, P, X);

        for (unsigned int d = 0; d < D; d++)
        {
            // Save final weights weights_tmp
            weights(t).row(d) = weights_tmp(opt_index(t)).row(d);

            // Store expert predictions temporarily
            mat experts_mat = experts(t).row(d);

            for (unsigned int x = 0; x < X; x++)
            {

                mat weights_temp = weights_tmp(x).row(d);

                // Forecasters prediction
                vec predictions_temp = sum(weights_temp % experts_mat, 1);

                predictions_tmp(t)(span(d), span::all, span(x)) = predictions_temp;
            }

            // Final prediction
            predictions.tube(t, d) = vectorise(predictions_tmp(t)(span(d), span::all, span(opt_index(t))));
        }

        past_performance(t).fill(datum::nan);
    }

    start = lead_time;
    clock.tock("init");
}

void conline::learn()
{
    Progress prog(T, trace);
    clock.tick("core");
    for (unsigned int t = start; t < T; t++)
    {
        weights(t).set_size(D, P, K);
        past_performance(t).set_size(D, P, X);
        predictions_tmp(t).set_size(D, P, X);

        clock.tick("loss");
        for (unsigned int d = 0; d < D; d++)
        {
            // Save final weights weights_tmp
            weights(t).row(d) = weights_tmp(opt_index(t)).row(d);

            // Store expert predictions temporarily
            mat experts_mat = experts(t).row(d);

            // Predictions using different parameter values
            for (unsigned int x = 0; x < X; x++)
            {

                mat weights_temp = weights_tmp(x).row(d);
                vec predictions_temp = sum(weights_temp % experts_mat, 1);

                // Sort predictions if quantile_crossing is prohibited
                if (!allow_quantile_crossing)
                {
                    predictions_temp = arma::sort(predictions_temp, "ascend", 0);
                }
                predictions_tmp(t)(span(d), span::all, span(x)) = predictions_temp;
            }
            // Forecasters prediction
            predictions.tube(t, d) = vectorise(predictions_tmp(t)(span(d), span::all, span(opt_index(t))));
        }

        clock.tock("loss");
        for (unsigned int x = 0; x < X; x++)
        {
            clock.tick("regret");
            mat lexp_int(P, K); // Experts loss
            mat lexp_ext(P, K); // Experts loss
            mat lexp(P, K);     // Experts loss
            vec lfor(P);        // Forecasters loss
            cube regret_tmp(D, P, K);
            cube regret(basis_mv(params["basis_mv_idx"](x) - 1).n_cols,
                        basis_pr(params["basis_pr_idx"](x) - 1).n_cols,
                        K); // Dr x Pr x K

            for (unsigned int d = 0; d < D; d++)
            {
#pragma omp parallel for
                for (unsigned int p = 0; p < P; p++)
                {
                    // Evaluate the ex-post predictive performance
                    past_performance(t)(d, p, x) = loss(y(t, d),
                                                        predictions_tmp(t - lead_time)(d, p, x),
                                                        9999,           // where evaluate loss_gradient
                                                        loss_function,  // method
                                                        tau(p),         // tau
                                                        loss_parameter, // alpha
                                                        false);
                    if (params["loss_share"](x) != 1)
                    {
                        for (unsigned int k = 0; k < K; k++)
                        {
                            lexp_int(p, k) = loss(y(t, d),
                                                  experts(t)(d, p, k),
                                                  predictions_tmp(t - lead_time)(d, p, x), // where evaluate loss_gradient
                                                  loss_function,                           // method
                                                  tau(p),                                  // tau
                                                  loss_parameter,                          // alpha
                                                  loss_gradient);
                        }

                        if (params["loss_share"](x) == 0)
                        {
                            lexp.row(p) = lexp_int.row(p);
                        }
                        else
                        {
                            lexp_ext.row(p) = arma::vectorise(loss_array(t).tube(d, p)).t();
                            lexp.row(p) = (1 - params["loss_share"](x)) * lexp_int.row(p) + params["loss_share"](x) * lexp_ext.row(p);
                        }
                    }
                    else
                    {
                        lexp_ext.row(p) = arma::vectorise(loss_array(t).tube(d, p)).t();
                        lexp.row(p) = lexp_ext.row(p);
                    }
                    lfor(p) = loss(y(t, d),
                                   predictions_tmp(t - lead_time)(d, p, x),
                                   predictions_tmp(t - lead_time)(d, p, x), // where to evaluate loss_gradient
                                   loss_function,                           // method
                                   tau(p),                                  // tau
                                   loss_parameter,                          // alpha
                                   loss_gradient);
                }

                mat regret_int(P, K);
                mat regret_ext(P, K);

                if (params["regret_share"](x) != 1)
                {
                    regret_int = (lfor - lexp_int.each_col()).t();
                    regret_int *= double(basis_pr(params["basis_pr_idx"](x) - 1).n_cols) / double(P);

                    if (params["regret_share"](x) == 0)
                    {
                        regret_tmp.row(d) = regret_int.t();
                    }
                    else
                    {
                        regret_ext = regret_array(t).row(d);
                        regret_ext = regret_ext.t();
                        regret_ext *= double(basis_pr(params["basis_pr_idx"](x) - 1).n_cols) / double(P);
                        regret_tmp.row(d) = ((1 - params["regret_share"](x)) * regret_int + params["regret_share"](x) * regret_ext).t();
                    }
                }
                else
                {
                    regret_ext = regret_array(t).row(d);
                    regret_ext = regret_ext.t();
                    regret_ext *= double(basis_pr(params["basis_pr_idx"](x) - 1).n_cols) / double(P);
                    regret_tmp.row(d) = regret_ext.t();
                }
            }

#pragma omp parallel for
            for (unsigned int k = 0; k < K; k++)
            {
                regret.slice(k) = basis_mv(params["basis_mv_idx"](x) - 1).t() * regret_tmp.slice(k) * basis_pr(params["basis_pr_idx"](x) - 1);
            }

            clock.tock("regret");
            clock.tick("learning");
#pragma omp parallel for collapse(2)
            for (unsigned int dr = 0; dr < regret.n_rows; dr++)
            { // This is subject to change if D will be reduces using another basis
                for (unsigned int pr = 0; pr < regret.n_cols; pr++)
                {

                    vec r = regret.tube(dr, pr);

                    if (method == "ewa")
                    {
                        // Update the cumulative regret used by eta
                        R(x).tube(dr, pr) = vectorise(R(x).tube(dr, pr) * (1 - params["forget_regret"](x))) + r;
                        eta(x).tube(dr, pr).fill(params["gamma"](x));
                        beta(x).tube(dr, pr) = vectorise(beta0field(x).tube(dr, pr)).t() * K % softmax_r(params["gamma"](x) * vectorise(R(x).tube(dr, pr)).t());
                    }
                    else if (method == "ml_poly")
                    {
                        // Update the cumulative regret used by ML_Poly
                        R(x)
                            .tube(dr, pr) = vectorise(R(x).tube(dr, pr) * (1 - params["forget_regret"](x))) + r;

                        // Update the learning rate
                        eta(x).tube(dr, pr) = 1 / (1 / vectorise(eta(x).tube(dr, pr)).t() + square(r.t()));

                        beta(x).tube(dr, pr) = vectorise(beta0field(x).tube(dr, pr)).t() * K * params["gamma"](x) % vectorise(eta(x).tube(dr, pr)).t() % pmax_arma(vectorise(R(x).tube(dr, pr)).t(), exp(-700));
                        beta(x).tube(dr, pr) /= accu(beta(x).tube(dr, pr));
                    }
                    else if (method == "boa" || method == "bewa")
                    {
                        V(x).tube(dr, pr) = vectorise(V(x).tube(dr, pr)).t() * (1 - params["forget_regret"](x)) + square(r.t());

                        E(x).tube(dr, pr) = max(vectorise(E(x).tube(dr, pr)).t() * (1 - params["forget_regret"](x)), abs(r.t()));

                        eta(x).tube(dr, pr) =
                            pmin_arma(
                                min(1 / (2 * vectorise(E(x).tube(dr, pr))),
                                    sqrt(-log(vectorise(beta0field(x).tube(dr, pr))) / vectorise(V(x).tube(dr, pr)))),
                                exp(350));

                        vec r_reg = r - vectorise(eta(x).tube(dr, pr)) % square(r);

                        R_reg(x).tube(dr, pr) *= (1 - params["forget_regret"](x)); // forget
                        R_reg(x).tube(dr, pr) +=
                            0.5 * (r_reg + conv_to<colvec>::from(vectorise(eta(x).tube(dr, pr)) % r > 0.5) % (2 * vectorise(E(x).tube(dr, pr))));

                        if (method == "boa")
                        {
                            // Wintenberger
                            beta(x).tube(dr, pr) = vectorise(beta0field(x).tube(dr, pr)).t() * K % softmax_r(log(params["gamma"](x) * vectorise(eta(x).tube(dr, pr)).t()) + params["gamma"](x) * vectorise(eta(x).tube(dr, pr)).t() % vectorise(R_reg(x).tube(dr, pr)).t());
                        }
                        else
                        {
                            // Gaillard
                            beta(x).tube(dr, pr) = vectorise(beta0field(x).tube(dr, pr)).t() * K % softmax_r(params["gamma"](x) * vectorise(eta(x).tube(dr, pr)).t() % vectorise(R_reg(x).tube(dr, pr)).t());
                        }
                    }
                    else
                    {
                        Rcpp::stop("Choose 'boa', 'bewa', 'ml_poly' or 'ewa' as method.");
                    }

                    //   // Apply thresholds
                    if (params["soft_threshold"](x) > 0)
                    {
                        int best_k = beta(x).tube(dr, pr).index_max();

                        for (double &e : beta(x).tube(dr, pr))
                        {
                            threshold_soft(e, params["soft_threshold"](x));
                        }
                        if (accu(beta(x).tube(dr, pr)) == 0)
                        {
                            beta(x)(dr, pr, best_k) = 1;
                        }
                    }

                    if (params["hard_threshold"](x) > 0)
                    {
                        int best_k = beta(x).tube(dr, pr).index_max();
                        for (double &e : beta(x).tube(dr, pr))
                        {
                            threshold_hard(e, params["hard_threshold"](x));
                        }
                        if (accu(beta(x).tube(dr, pr)) == 0)
                        {
                            beta(x)(dr, pr, best_k) = 1;
                        }
                    }

                    // Add fixed_share
                    beta(x).tube(dr, pr) =
                        (1 - params["fixed_share"](x)) * vectorise(beta(x).tube(dr, pr)) +
                        (params["fixed_share"](x) / K);
                } // pr
            }     // dr
            clock.tock("learning");

#pragma omp parallel for
            // Smoothing
            for (unsigned int k = 0; k < K; k++)
            {
                clock.tick("smoothing");
                weights_tmp(x).slice(k) = hat_mv(params["hat_mv_idx"](x) - 1) *
                                          basis_mv(params["basis_mv_idx"](x) - 1) *
                                          beta(x).slice(k) *
                                          basis_pr(params["basis_pr_idx"](x) - 1).t() *
                                          hat_pr(params["hat_pr_idx"](x) - 1);
                clock.tock("smoothing");
            }

#pragma omp parallel for
            // Enshure that constraints hold
            for (unsigned int p = 0; p < P; p++)
            {
                for (unsigned int d = 0; d < D; d++)
                {
                    // Positivity
                    weights_tmp(x)(span(d), span(p), span::all) =
                        pmax_arma(weights_tmp(x)(span(d), span(p), span::all), exp(-700));

                    // // Affinity
                    weights_tmp(x)(span(d), span(p), span::all) /=
                        accu(weights_tmp(x)(span(d), span(p), span::all));
                }
            }
            tmp_performance(x) = accu(past_performance(t).slice(x));
            R_CheckUserInterrupt();
        }

        // Sum past_performance in each slice
        cum_performance = (1 - forget_past_performance) * cum_performance + tmp_performance;

        opt_index(t + 1) = cum_performance.index_min();
        prog.increment(); // Update progress

    } // t

    // Save Final Weights and Prediction
    weights(T) = weights_tmp(opt_index(T));

    // Predict residual expert forecasts if any are available
    for (unsigned int t = T; t < T + T_E_Y; t++)
    {
        for (unsigned int d = 0; d < D; d++)
        {
            mat experts_mat = experts(t).row(d);
            mat weights_temp = weights(T).row(d);
            vec predictions_temp = sum(weights_temp % experts_mat, 1);

            // Sort predictions if quantile_crossing is prohibited
            if (!allow_quantile_crossing)
            {
                predictions_temp = arma::sort(predictions_temp, "ascend", 0);
            }
            predictions.tube(t, d) = predictions_temp;
        }
    }

    // Save losses suffered by forecaster and experts
    clock.tick("loss_for_exp");
#pragma omp parallel for
    for (unsigned int t = 0; t < T; t++)
    {
        loss_exp(t).set_size(D, P, K);

        for (unsigned int d = 0; d < D; d++)
        {
            for (unsigned int p = 0; p < P; p++)
            {
                for (unsigned int k = 0; k < K; k++)
                {
                    loss_exp(t)(d, p, k) =
                        loss(y(t, d),
                             experts(t)(d, p, k),
                             9999,           // where to evaluate the loss_gradient
                             loss_function,  // method
                             tau(p),         // tau
                             loss_parameter, // alpha
                             false);         // loss_gradient
                }
                loss_for(t, d, p) = loss(y(t, d),
                                         predictions(t, d, p),
                                         9999,           // where to evaluate the loss_gradient
                                         loss_function,  // method
                                         tau(p),         // tau
                                         loss_parameter, // alpha
                                         false);         // loss_gradient;
            }
        }
    }
    clock.tock("loss_for_exp");
    clock.tock("core");
}

Rcpp::List conline::output()
{
    clock.tick("wrangle");

    // // 1-Indexing for R-Output
    opt_index = opt_index + 1;

    // Rcpp::NumericMatrix parametergrid_out = Rcpp::wrap(param_grid);

    Rcpp::List model_data = Rcpp::List::create(
        Rcpp::Named("y") = y,
        Rcpp::Named("experts") = experts,
        Rcpp::Named("tau") = tau);

    Rcpp::List model_parameters = Rcpp::List::create(
        Rcpp::Named("lead_time") = lead_time,
        Rcpp::Named("loss_function") = loss_function,
        Rcpp::Named("loss_parameter") = loss_parameter,
        Rcpp::Named("loss_gradient") = loss_gradient,
        Rcpp::Named("method") = method,
        Rcpp::Named("forget_past_performance") = forget_past_performance,
        Rcpp::Named("allow_quantile_crossing") = allow_quantile_crossing);

    Rcpp::List model_objects = Rcpp::List::create(
        Rcpp::Named("weights_tmp") = weights_tmp,
        Rcpp::Named("predictions_tmp") = predictions_tmp,
        Rcpp::Named("cum_performance") = cum_performance,
        Rcpp::Named("hat_pr") = hat_pr,
        Rcpp::Named("hat_mv") = hat_mv,
        Rcpp::Named("basis_pr") = basis_pr,
        Rcpp::Named("basis_mv") = basis_mv,
        Rcpp::Named("V") = V,
        Rcpp::Named("E") = E,
        Rcpp::Named("k") = k,
        Rcpp::Named("eta") = eta,
        Rcpp::Named("R") = R,
        Rcpp::Named("R_reg") = R_reg,
        Rcpp::Named("beta") = beta,
        Rcpp::Named("beta0field") = beta0field);

    Rcpp::List model_spec = Rcpp::List::create(
        Rcpp::Named("data") = model_data,
        Rcpp::Named("parameters") = model_parameters,
        Rcpp::Named("objects") = model_objects);

    Rcpp::List out = Rcpp::List::create(
        Rcpp::Named("predictions") = predictions,
        Rcpp::Named("weights") = weights,
        Rcpp::Named("forecaster_loss") = loss_for,
        Rcpp::Named("experts_loss") = loss_exp,
        Rcpp::Named("past_performance") = past_performance,
        // Rcpp::Named("chosen_parameters") = chosen_parameters,
        Rcpp::Named("opt_index") = opt_index,
        Rcpp::Named("parametergrid") = params,
        Rcpp::Named("params_basis_pr") = params_basis_pr,
        Rcpp::Named("params_basis_mv") = params_basis_mv,
        Rcpp::Named("params_hat_pr") = params_hat_pr,
        Rcpp::Named("params_hat_mv") = params_hat_mv,
        Rcpp::Named("specification") = model_spec);

    out.attr("class") = "online";

    clock.tock("wrangle");
    // Rcpp::List out;
    return out;
}

void conline::init_update(
    Rcpp::List &object,
    arma::mat &new_y,
    arma::field<arma::cube> &new_experts)
{

    clock.tick("init");

    // This creates a references not copies
    Rcpp::List specification = object["specification"];
    Rcpp::List model_parameters = specification["parameters"];
    Rcpp::List model_data = specification["data"];
    Rcpp::List model_objects = specification["objects"];

    // Data

    // Join old and new expert_predictions
    arma::field<cube> old_experts = model_data["experts"];
    experts.set_size(old_experts.n_rows + new_experts.n_rows);
    experts.rows(0, old_experts.n_rows - 1) = old_experts;
    if (new_experts.n_rows > 0)
    {
        experts.rows(old_experts.n_rows, experts.n_rows - 1) = new_experts;
    }

    y = Rcpp::as<arma::mat>(model_data["y"]);
    y.insert_rows(y.n_rows, new_y);
    start = T - new_y.n_rows;

    if (T_E_Y < 0)
        Rcpp::stop("Number of provided expert predictions has to match or exceed observations.");

    tau = Rcpp::as<arma::vec>(model_data["tau"]);

    params = Rcpp::as<std::map<std::string, arma::colvec>>(object["parametergrid"]);
    params_basis_pr = Rcpp::as<std::map<std::string, arma::colvec>>(object["params_basis_pr"]);
    params_basis_mv = Rcpp::as<std::map<std::string, arma::colvec>>(object["params_basis_mv"]);
    params_hat_pr = Rcpp::as<std::map<std::string, arma::colvec>>(object["params_hat_pr"]);
    params_hat_mv = Rcpp::as<std::map<std::string, arma::colvec>>(object["params_hat_mv"]);

    opt_index = Rcpp::as<arma::vec>(object["opt_index"]);
    // Zero indexing in C++
    opt_index -= 1;
    opt_index.resize(T + 1);

    past_performance.set_size(T);
    past_performance.rows(0, start - 1) =
        Rcpp::as<arma::field<cube>>(object["past_performance"]);

    tmp_performance.zeros(X);
    cum_performance = Rcpp::as<arma::vec>(model_objects["cum_performance"]);

    weights_tmp =
        Rcpp::as<arma::field<cube>>(model_objects["weights_tmp"]);

    predictions_tmp.set_size(T + T_E_Y);
    predictions_tmp.rows(0, old_experts.n_rows - 1) = Rcpp::as<arma::field<cube>>(model_objects["predictions_tmp"]);

    // // Output Objects
    predictions = Rcpp::as<arma::cube>(object["predictions"]);
    predictions.resize(T + T_E_Y, D, P);
    weights.set_size(T + 1);
    weights.rows(0, start) = Rcpp::as<arma::field<cube>>(object["weights"]);

    basis_pr = Rcpp::as<arma::field<arma::sp_mat>>(model_objects["basis_pr"]);
    basis_mv = Rcpp::as<arma::field<arma::sp_mat>>(model_objects["basis_mv"]);
    hat_pr = Rcpp::as<arma::field<arma::sp_mat>>(model_objects["hat_pr"]);
    hat_mv = Rcpp::as<arma::field<arma::sp_mat>>(model_objects["hat_mv"]);

    V = Rcpp::as<arma::field<cube>>(model_objects["V"]);
    E = Rcpp::as<arma::field<cube>>(model_objects["E"]);
    k = Rcpp::as<arma::field<cube>>(model_objects["k"]);
    eta = Rcpp::as<arma::field<cube>>(model_objects["eta"]);
    R = Rcpp::as<arma::field<cube>>(model_objects["R"]);
    R_reg = Rcpp::as<arma::field<cube>>(model_objects["R_reg"]);
    beta = Rcpp::as<arma::field<cube>>(model_objects["beta"]);
    beta0field = Rcpp::as<arma::field<cube>>(model_objects["beta0field"]);

    // //   // Misc parameters
    lead_time = model_parameters["lead_time"];
    loss_function = Rcpp::as<std::string>(model_parameters["loss_function"]);
    loss_parameter = model_parameters["loss_parameter"];
    loss_gradient = model_parameters["loss_gradient"];
    method = Rcpp::as<std::string>(model_parameters["method"]);

    forget_past_performance = model_parameters["forget_past_performance"];
    allow_quantile_crossing = model_parameters["allow_quantile_crossing"];

    loss_for.zeros(T, D, P);
    loss_for.rows(0, start - 1) =
        Rcpp::as<arma::cube>(object["forecaster_loss"]);

    loss_exp.set_size(T);
    loss_exp.rows(0, start - 1) =
        Rcpp::as<arma::field<cube>>(object["experts_loss"]);

    clock.tock("init");
}
