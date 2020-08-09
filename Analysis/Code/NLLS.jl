using Distributions
using DataFrames
using LinearAlgebra
using StatsBase
using NLSolversBase
using Optim
using CSVFiles

include("estimation_funcs.jl");

# test on simulated data
# X = simdata(700, 700)
# regressors = [:age, :discnew, :discount, :logreviews, :noreviews,
#               :negative, :mixed, :positive]
#
#
# solution = @time Optim.optimize(Optim.only_fg!((F, G, x) -> ssr_concentrated!(F, G, nothing, x, X)),
#                           β, LBFGS(), Optim.Options(allow_f_increases = true))
#
# solution = @time Optim.optimize(Optim.only_fgh!((F, G, H, x) -> ssr_concentrated!(F, G, H, x, X; hessian = true)),
#                         β, Newton(), Optim.Options(allow_f_increases = true))
#
# β̂ = solution.minimizer
# # create a value of z from β and map estimate_ψλ through the data
# varβ̂ = varβ(β̂; data = X)
# print_tests(testβ(β̂ , varβ̂))



panel = "/Users/Dmitry/Steam/Build/Output/AnalysisData/juliaData.csv" |>
        load |> DataFrame
#panel = panel[Not(in.(panel.id, Ref([508, 678, 1270, 1297]))), :]

# convert the Bools
panel.discNew = (x -> ifelse(x=="TRUE", true, false)).(panel.discNew)
panel.discNewLag = (x -> ifelse(x=="TRUE", true, false)).(panel.discNewLag)
#panel.exogDiscNew = (x -> ifelse(x=="TRUE", true, false)).(panel.exogDiscNew)
#panel.exogDiscNewLag = (x -> ifelse(x=="TRUE", true, false)).(panel.exogDiscNewLag)
panel.noScore = (x -> ifelse(x=="TRUE", true, false)).(panel.noScore)
panel.negative = (x -> ifelse(x=="TRUE", true, false)).(panel.negative)
panel.mPositive = (x -> ifelse(x=="TRUE", true, false)).(panel.mPositive)
panel.positive = (x -> ifelse(x=="TRUE", true, false)).(panel.positive)
panel.young = (x -> ifelse(x=="TRUE", true, false)).(panel.young)
panel.discSeason = (x -> ifelse(x=="TRUE", true, false)).(panel.discSeason)

# initialize entries of the data frame that will be used later
panel.z = zeros(size(panel,1))
panel.λ = zeros(size(panel,1))
panel.ψ = zeros(size(panel,1))
panel.error = zeros(size(panel,1))
const n = combine(groupby(panel, :id), nrow).id |> length;

# Setting up optimization
func = (F, G, H, x) -> ssr_concentrated!(F, G, H, x, panel; hessian = true)
stats = DataFrame(regressor = ["Seasonality", "Observations", "R2"])

# Model 1
regressors = [:price, :age, :young,
              :noScore, :negative, :mPositive, :positive, :lrev]
lower = repeat([-2.0], length(regressors))
upper = repeat([2.0], length(regressors))
initial_x = [-0.1, 0.1, 0.2, 0.1, -0.05, 0.05, 0.1, 0.1]

objective = TwiceDifferentiable(only_fgh!(func), initial_x)
constraints = TwiceDifferentiableConstraints(lower, upper)

solution1 = optimize(objective, constraints, initial_x, IPNewton(),
                    Optim.Options(allow_f_increases = true))

m1 = testLatexβ(solution1.minimizer, varβ(solution1.minimizer))

SS_total = var(panel[:, :number]) * size(panel,1)
r2 = 1 - 2 * solution1.minimum / SS_total
stats.m1 = [0, size(panel,1), round(r2, digits = 2)]

# Model 2
day_dummies = [:day_Tue, :day_Wed, :day_Thu, :day_Fri,
        :day_Sat, :day_Sun]

regressors = [regressors; day_dummies]

lower = repeat([-2.0], length(regressors))
upper = repeat([2.0], length(regressors))
initial_x = [-0.1, -0.1, 0.2, -0.1, -0.05, 0.1, 0.1, 0.1,
             0.1, 0.1, 0.1, 0.2, 0.2, 0.2]

objective = TwiceDifferentiable(only_fgh!(func), initial_x)
constraints = TwiceDifferentiableConstraints(lower, upper)

solution2 = optimize(objective, constraints, initial_x, IPNewton(),
                    Optim.Options(allow_f_increases = true))

m2 = testLatexβ(solution2.minimizer, varβ(solution2.minimizer))

r2 = 1 - 2 * solution2.minimum / SS_total
stats.m2 = [1, size(panel,1), round(r2, digits = 2)]


# Model 3
regressors = [regressors; [:discSeason]]

lower = repeat([-2.0], length(regressors))
upper = repeat([2.0], length(regressors))
initial_x = [-0.1, -0.1, 0.2, -0.1, -0.05, 0.1, 0.1, 0.1,
             0.1, 0.1, 0.1, 0.2, 0.2, 0.2, 0.1]

objective = TwiceDifferentiable(only_fgh!(func), initial_x)
constraints = TwiceDifferentiableConstraints(lower, upper)

solution3 = optimize(objective, constraints, initial_x, IPNewton(),
                    Optim.Options(allow_f_increases = true))

m3 = testLatexβ(solution3.minimizer, varβ(solution3.minimizer))

r2 = 1 - 2 * solution3.minimum / SS_total
stats.m3 = [1, size(panel,1), round(r2, digits = 2)]


# Reporting Results
coef_table = print_tests_latex(m1, m2, m3; omit = day_dummies)
save("/Users/Dmitry/Steam/Build/Output/AnalysisData/juliaResults.csv", coef_table)
save("/Users/Dmitry/Steam/Build/Output/AnalysisData/juliaResults2.csv", stats)

ψλz = by(panel, [:id, :t], psi = :ψ => identity, lambda = :λ => identity,
                           z = :z => identity)
save("/Users/Dmitry/Steam/Build/Output/AnalysisData/juliaResults3.csv", ψλz)

# Try different initial points
initial_x = zeros(length(regressors))
objective = TwiceDifferentiable(only_fgh!(func), initial_x)
constraints = TwiceDifferentiableConstraints(lower, upper)
solution = optimize(objective, constraints, initial_x, IPNewton(),
                    Optim.Options(allow_f_increases = true))
m5 = testβ(solution.minimizer, varβ(solution.minimizer));

initial_x = ones(length(regressors))
objective = TwiceDifferentiable(only_fgh!(func), initial_x)
constraints = TwiceDifferentiableConstraints(lower, upper)
solution = optimize(objective, constraints, initial_x, IPNewton(),
                    Optim.Options(allow_f_increases = true))
m6 = testβ(solution.minimizer, varβ(solution.minimizer));

initial_x = -1 .* ones(length(regressors))

objective = TwiceDifferentiable(only_fgh!(func), initial_x)
constraints = TwiceDifferentiableConstraints(lower, upper)

solution = optimize(objective, constraints, initial_x, IPNewton(),
                    Optim.Options(allow_f_increases = true))

m7 = testβ(solution.minimizer, varβ(solution.minimizer));

# The last specification converges to a different minimum, but with a higher
# value. The solution we find seems to be the global minimum.
print_tests(m4, m5, m6, m7; omit = day_dummies) |> print

# # Exogenous discounts: not enough power
# regressors = [[:exogDisc, :price, :discNew, :age, :young,
#               :noScore, :negative, :mPositive, :positive, :lrev, :discSeason];
#               day_dummies]
# lower = repeat([-2.0], length(regressors))
# upper = repeat([2.0], length(regressors))
# initial_x = [-0.4, -0.1, -0.2, -0.1, 0.2, -0.1, -0.05, 0.1, 0.1, 0.1, -0.2,
#              0.1, 0.1, 0.1, 0.2, 0.2, 0.2]
# objective = TwiceDifferentiable(only_fgh!(func), initial_x)
# constraints = TwiceDifferentiableConstraints(lower, upper)
# solution = optimize(objective, constraints, initial_x, IPNewton(),
#            Optim.Options(allow_f_increases = true))
# m5 = testβ(solution.minimizer, varβ(solution.minimizer));
#
#
# # Model 6
# regressors = [regressors; [:exogDiscNew]]
# lower = repeat([-2.0], length(regressors))
# upper = repeat([2.0], length(regressors))
# initial_x = [initial_x; [0.1]]
# objective = TwiceDifferentiable(only_fgh!(func), initial_x)
# constraints = TwiceDifferentiableConstraints(lower, upper)
# solution = optimize(objective, constraints, initial_x, IPNewton(),
#                     Optim.Options(allow_f_increases = true))
# m6 = testβ(solution.minimizer, varβ(solution.minimizer));
#
#
# # Model 7
# regressors = [regressors[1:(end-1)]; [:exogDiscNewLag]]
# lower = repeat([-2.0], length(regressors))
# upper = repeat([2.0], length(regressors))
# objective = TwiceDifferentiable(only_fgh!(func), initial_x)
# constraints = TwiceDifferentiableConstraints(lower, upper)
# solution = optimize(objective, constraints, initial_x, IPNewton(),
#                     Optim.Options(allow_f_increases = true))
# m7 = testβ(solution.minimizer, varβ(solution.minimizer));


# # Model 8: week time effects -- does not converge
# regressors = [:price, :discNew, :age, :young,
#               :noScore, :negative, :mPositive, :positive, :lrev]
# regressors = [regressors; names(panel)[24:(end-4)]]
#
# lower = repeat([-2.0], length(regressors))
# upper = repeat([2.0], length(regressors))
# initial_x = [-0.15, -0.05, 0.0, 0.05, 0.3, -0.05, 0.5, 0.7, -0.1,
#              0.1, 0.1, 0.1, 0.2, 0.2, 0.2]
# initial_x = [initial_x; repeat([0.0], length(regressors)-15)]
#
# objective = TwiceDifferentiable(only_fgh!(func), initial_x)
# constraints = TwiceDifferentiableConstraints(lower, upper)
#
# solution = optimize(objective, constraints, initial_x, IPNewton(),
#                     Optim.Options(allow_f_increases = true))


# Setting up optimization
func = (F, G, H, x) -> ssr_concentrated!(F, G, H, x, panel; hessian = true)

# Model 1
regressors = [:price, :age, :young,
              :noScore, :negative, :mPositive, :positive, :lrev]
day_dummies = [:day_Tue, :day_Wed, :day_Thu, :day_Fri,
    :day_Sat, :day_Sun]

regressors = [regressors; day_dummies]
lower = repeat([-2.0], length(regressors))
upper = repeat([2.0], length(regressors))
initial_x = [-0.1, 0.1, 0.2, 0.1, -0.05, 0.05, 0.1, -0.1,
             0.1, 0.1, 0.1, 0.2, 0.2, 0.2]

objective = TwiceDifferentiable(only_fgh!(func), initial_x)
constraints = TwiceDifferentiableConstraints(lower, upper)

solution1 = optimize(objective, constraints, initial_x, IPNewton(),
                    Optim.Options(allow_f_increases = true))

m1 = testβ(solution1.minimizer, varβ(solution1.minimizer))



regressors = [:price, :age, :young,
              :noScore, :negative, :mPositive, :realPositive, :vPositive, :ovPositive,
              :lrev]
day_dummies = [:day_Tue, :day_Wed, :day_Thu, :day_Fri, :day_Sat, :day_Sun]
regressors = [regressors; day_dummies]

lower = repeat([-2.0], length(regressors))
upper = repeat([2.0], length(regressors))
initial_x = [0.4, -0.01, 0.06, 0.2, -0.05, 0.3, 0.1, 0.1, 0.1, -0.1,
             0.1, 0.1, 0.1, 0.2, 0.2, 0.2]

objective = TwiceDifferentiable(only_fgh!(func), initial_x)
constraints = TwiceDifferentiableConstraints(lower, upper)

solution2 = optimize(objective, constraints, initial_x, IPNewton(),
                    Optim.Options(allow_f_increases = true))

m2 = testβ(solution2.minimizer, varβ(solution2.minimizer))




regressors = [:price, :age, :young,
              :noScore, :negative, :mPositive, :realPositive, :vPositive,
              :lrev, :score]
day_dummies = [:day_Tue, :day_Wed, :day_Thu, :day_Fri, :day_Sat, :day_Sun]
regressors = [regressors; day_dummies]

lower = repeat([-2.0], length(regressors))
upper = repeat([2.0], length(regressors))
initial_x = [0.4, -0.01, 0.06, 0.2, -0.05, 0.3, 0.1, 0.1, -0.1, 0.0,
             0.1, 0.1, 0.1, 0.2, 0.2, 0.2]


objective = TwiceDifferentiable(only_fgh!(func), initial_x)
constraints = TwiceDifferentiableConstraints(lower, upper)

solution3 = optimize(objective, constraints, initial_x, IPNewton(),
                    Optim.Options(allow_f_increases = true))

m3 = testβ(solution3.minimizer, varβ(solution3.minimizer))



regressors = [:price, :age, :young,
              :noScore, :negative, :mPositive, :positive, :score, :lrev]

lower = repeat([-2.0], length(regressors))
upper = repeat([2.0], length(regressors))
initial_x = [0.4, -0.01, 0.06, 0.2, -0.05, 0.3, 0.1, 0.1,
             0.1, 0.1, 0.1, 0.2, 0.2, 0.2]

objective = TwiceDifferentiable(only_fgh!(func), initial_x)
constraints = TwiceDifferentiableConstraints(lower, upper)

solution3 = optimize(objective, constraints, initial_x, IPNewton(),
                    Optim.Options(allow_f_increases = true))

m3b = testβ(solution3.minimizer, varβ(solution3.minimizer))


# Model 2
day_dummies = [:day_Tue, :day_Wed, :day_Thu, :day_Fri,
        :day_Sat, :day_Sun]

regressors = [regressors; day_dummies]

lower = repeat([-2.0], length(regressors))
upper = repeat([2.0], length(regressors))
initial_x = [-0.1, -0.1, 0.2, -0.1, -0.05, 0.1, 0.1, 0.0, -0.1,
             0.1, 0.1, 0.1, 0.2, 0.2, 0.2]

objective = TwiceDifferentiable(only_fgh!(func), initial_x)
constraints = TwiceDifferentiableConstraints(lower, upper)

solution4 = optimize(objective, constraints, initial_x, IPNewton(),
                    Optim.Options(allow_f_increases = true))

m4 = testβ(solution4.minimizer, varβ(solution4.minimizer))


# Model 3
regressors = [regressors; [:discSeason]]

lower = repeat([-2.0], length(regressors))
upper = repeat([2.0], length(regressors))
initial_x = [-0.1, -0.1, 0.2, -0.1, -0.05, 0.1, 0.1, 0.0, -0.1,
             0.1, 0.1, 0.1, 0.2, 0.2, 0.2, 0.1]

objective = TwiceDifferentiable(only_fgh!(func), initial_x)
constraints = TwiceDifferentiableConstraints(lower, upper)

solution5 = optimize(objective, constraints, initial_x, IPNewton(),
                    Optim.Options(allow_f_increases = true))

m5 = testβ(solution5.minimizer, varβ(solution5.minimizer))


print_tests(m1, m2, m3a, m3b, m4, m5; omit = day_dummies) |> print
