function simgame(id::Int64, t0::Int64, T::Int64, num0::Int64, β,
                 λ::Int64, ψ::Float64, q::Float64)

    # review parameters
    r = 0.05 # probability to be a reviewer
    rT = 7 # maximum number of days to leave a review
    reviewtime = 1:rT
    dateprobs = [0.6, 0.3, 0.02, 0.02, 0.02, 0.02, 0.02]

    # Initialize the arrays: allow extra space for convenience, to store reviewers
    # that won't realize in time
    posrevs = 1
    negrevs = 0
    posarrivals = zeros(Int64, T+rT)
    negarrivals = zeros(Int64, T+rT)

    # All the x regressors go in this data frame
    X = DataFrame(id=id, t=t0:(t0+T-1), number = zeros(Int64,T),
                  numberLag = zeros(Int64,T), age = 1:T, discnew = zeros(Bool,T),
                  discount = zeros(Float64,T), logreviews = zeros(Float64, T),
                  noreviews = zeros(Bool,T), negative = zeros(Bool,T),
                  mixed = zeros(Bool,T), positive = zeros(Bool,T))

    X[1, :number] = num0
    regressors = [:age, :discnew, :discount, :logreviews, :noreviews,
                  :negative, :mixed, :positive]
    # Discounts progressively get bigger, prob = 1/40, meaning 1 discount every
    # 40 days, on average
    disc_counter = 1
    disc_prob = 0.025
    disc_sizes = [0.1, 0.15, 0.25, 0.33, 0.5, 0.66, 0.75]
    disc_durations = [1, 4, 7, 14]


    function randomdiscount!(t::Int64)
    # Checks if there is a discount at t. If not, tosses a coin and fills
    # the entry of X
      if X[t, :discount] > 0
          return
      end
      if rand(Bernoulli(disc_prob)) == true
          X[t, :discnew] = true
          disc_counter == 6 ? nothing : disc_counter += rand([0,1])
          dur = rand(disc_durations)
          t_end = min(T, t+dur-1)
          X[t:(t_end),:discount].=rand(disc_sizes[disc_counter:(disc_counter+1)])
      end
      return
    end

    function fillreviews!(t)
    # use the number of positive and negative reviews to fill out the
    # reviews-related entries of X[t]
        total = posrevs + negrevs
        if total >= 10
            score = posrevs/total
            noreviews = false
            negative = score < 0.4
            mixed = 0.4 <= score < 0.7
            positive = 0.8 <= score
        else
            noreviews = true
            negative = mixed = positive = false
        end
        X[t, [:logreviews,:noreviews,:negative,:mixed,:positive]] .=
        log(total), noreviews, negative, mixed, positive
    end

    # update the entries of X
    fillreviews!(1)
    randomdiscount!(1)

    reviewers = rand(Binomial(X[1, :number], r))
    posreviewers = rand(Binomial(reviewers, q))
    negreviewers = reviewers - posreviewers
    timetoreview = reviewtime[rand(Categorical(dateprobs), reviewers)]

    posarrivals[(2):(1+rT)].=counts(timetoreview[1:posreviewers], reviewtime)
    negarrivals[(2):(1+rT)].=counts(timetoreview[(posreviewers+1):end],reviewtime)

    for t in 2:T
        # new day: reviews from previous days arrive
        posrevs += posarrivals[t]
        negrevs += negarrivals[t]

        # update the entries of X
        fillreviews!(t)
        randomdiscount!(t)

        # simulate new buyers and exiters for the next day
        arrivalrate = max(λ * (1 + X[t,regressors]⋅β), 1)
        buyers = rand(Poisson(arrivalrate))
        exiters = rand(Binomial(X[t-1, :number], 1-ψ))
        X[t, :number] = X[t-1, :number] + buyers - exiters

        if t+1 ≤ T
            # The number of reviewers is binomial with parameter r, and among them
            # the positive ones are determined by probability q
            reviewers = rand(Binomial(buyers, r))
            posreviewers = rand(Binomial(reviewers, q))
            negreviewers = reviewers - posreviewers

            # for every review take a random time to post the review. The first
            # (posreviewers) entries will be times for positive reviews
            timetoreview = reviewtime[rand(Categorical(dateprobs), reviewers)]

            posarrivals[(t+1):(t+rT)].=counts(timetoreview[1:posreviewers],
                                                reviewtime)
            negarrivals[(t+1):(t+rT)].=counts(timetoreview[(posreviewers+1):end],
                                                reviewtime)
        end

    end
    X.numberLag .= [-999; X.number[1:(end-1)]]

    return X[2:end, :]
end


function simdata(n::Int64, T::Int64 = 700, β = [-0.0003, 0.1, 0.5, 0.5, -0.05, -0.15, -0.1, 0.1])
    # simulate a data set of n games, with time in sample uniformly distributed
    # between 14 and T days
    # values of the parameters for different games
    num0_vals = [50*i for i = 1:n]
    λ_vals = [2*i for i = 1:n]
    ψ_vals = sample([i for i = 5:5:95]./100, n)
    q_vals = sample([i for i = 30:5:95]./100, n)
    T_vals = sample([t for t = 14:14:T], n) .+ 1


    X = simgame(1, 1, 301, num0_vals[1], λ_vals[1], ψ_vals[1], q_vals[1])
    for id in 2:n
        append!(X, simgame(id, 1, T_vals[id], num0_vals[id], λ_vals[id],
                           ψ_vals[id], q_vals[id]))
    end

    # initialize entries of the data frame that will be used later
    X.z = zeros(size(X,1))
    X.λ = zeros(size(X,1))
    X.ψ = zeros(size(X,1))
    X.error = zeros(size(X,1))

    # adjust the age scale for optimization
    X.age = X.age ./ 100
    return X
end

function estimate_λψ(dt; hessian = false)
    # calculate λᵢ and ψᵢ, then use those estimates to fill the errors
    # Afterwards, calculate the derivatives of λᵢ and ψᵢ w.r.t. β
    # and use those to calculate the variance of β̂
    n = length(dt.z)

    z2 = (dt.z).^2
    ylag2 = (dt.numberLag).^2

    z2sum = sum(z2)
    ylag2sum = sum(ylag2)
    zylagsum = dt.z ⋅ dt.numberLag

    # use the inverse of (X'X) derived by hand to solve X'X θ̂ = X'y,
    # where X = [z, numberLag], y = number
    invMatrix = [ylag2sum -zylagsum; -zylagsum z2sum]./
                (z2sum * ylag2sum - zylagsum^2)

    λψ = invMatrix * [dt.z ⋅ dt.number, dt.numberLag ⋅ dt.number]

    if hessian == false
        return (lambda = λψ[1] * ones(n), psi = λψ[2] * ones(n))
    else
        # if x(β) is a solution to A(β)x = b(β), then ∂x/∂β can be obtained from
        # ∂x/∂βᵢ = A^{-1}(∂b/∂βᵢ - ∂A/∂βᵢ x(β))
        k = length(regressors)
        X = convert(Array, dt[:, regressors]) |> transpose

        # ∂A is a k=dim(β) by 2 matrix where entry i,1 is ∂A/∂βᵢ[1,1] and
        # entry i,2 is ∂A/∂βᵢ[2,1] = ∂A/∂βᵢ[1,2]. ∂A/∂βᵢ[2,2] = 0.
        ∂A = X * [dt.z dt.numberLag]
        ∂x = zeros(Float64, 2, k)

        # ∂b is a k×1 vector with entries ∂b[i] = xᵢ ⋅ y, which we then use to
        # construct ∂b/∂βᵢ = [xᵢ ⋅ y, 0]'.
        ∂b = X * dt.number

        for i in 1:k
            ∂Aᵢ = [2 * ∂A[i, 1] ∂A[i, 2]; ∂A[i, 2] 0.0]
            ∂bᵢ = [∂b[i], 0]
            ∂x[:, i] .= invMatrix * (∂bᵢ - ∂Aᵢ * λψ)
        end

        ∂λ = ∂x[1, :]
        ∂ψ = ∂x[2, :]

        # To compute the Hessian of SSR(β) that is due to λᵢ and ψᵢ we need several values:
        # ∂SSR(β)/∂λᵢ∂β = λᵢ ∑ x_it * z_it - ∑ error_it * x_it = X(λᵢz -error)
        # Notice that Xz = ∂A[:, 1].
        #
        # ∂SSR(β)/∂ψᵢ∂β = λᵢ ∑ x_it * y_i(t-1) = X' λᵢ y.lag  = λᵢ∂A[:, 2]
        #
        # ∂SSR(β)/∂²β = -λᵢ²XX'
        # Hessian_i = ∂SSR(β)/∂²β + ∂SSR(β)/∂λᵢ∂β (∂λᵢ/∂β)' + ∂SSR(β)/∂ψᵢ∂β (∂ψᵢ/∂β)'

        ∂SSR1 = λψ[1] .* ∂A[:, 1] .- X * dt.error
        ∂SSR2 = λψ[1] .* ∂A[:, 2]
        ∂SSR3 = (λψ[1])^2 .* (X * transpose(X))

        return (lambda = λψ[1], psi = λψ[2],
                hessian = ∂SSR3 .+ ∂SSR1 * transpose(∂λ) .+ ∂SSR2 * transpose(∂ψ))
    end
end

function solve_λψ!(data, β; hessian = false)
    # this function fills the λᵢ and ψᵢ columns of data and returns the Hessian
    # matrix, associated with β, if hessian = true

    # create a value of z from β and map estimate_ψλ through the data
    data[:, :z] .= convert(Array, data[:,regressors]) * β .+ 1
    grdata = groupby(data, :id)

    if hessian == false
        # save the values of λ and ψ in the data
        data.λ, data.ψ = (dt -> (dt.lambda, dt.psi))(combine(estimate_λψ, grdata))
        return nothing
    else
        # if we need a Hessian, then map through the data frame manually
        hessian = zeros(length(regressors), length(regressors))
            for (i, dt) in enumerate(grdata)
                λ, ψ, hes = estimate_λψ(dt; hessian = true)
                grdata[i].λ .= λ
                grdata[i].ψ .= ψ
                hessian .+= hes
            end

        data[:,[:λ, :ψ]] .= DataFrame(grdata)[:,[:λ, :ψ]]

        return hessian
    end
end

function calc_error(dt)
    return dt.number .- dt.ψ .* dt.numberLag .- dt.λ .* dt.z
end

function ssr_concentrated!(F, G, H, β, data; hessian = false)
    # calculates the sum of squared residuals and the gradient

    # estimate the values of λ and ψ, and return the Hessian matrix H, if needed
    Hes = solve_λψ!(data, β; hessian = hessian)

    grdata = groupby(data, :id)
    data.error .= combine(grdata, calc_error)[:, :x1]

    if G != nothing
        # Optim.jl checks if the gradient is required. Convenient for methods
        # that use the gradient from time to time.
        G .= -1 .* data[:, regressors] |>
        x -> convert(Array, x) |>
        transpose |>
        x -> *(x, data.λ .* data.error)
    end

    if H != nothing
        H .= Hes
    end

    if F != nothing
      # compute objective function
      return sum(data[:,:error].^2) / 2
    end

    return nothing
end

function variance_one_game!(dt::DataFrame)
    # calculate λᵢ and ψᵢ, then use those estimates to fill the errors
    # Afterwards, calculate the derivatives of λᵢ and ψᵢ w.r.t. β
    # and use those to calculate the variance of β̂
    z2 = (dt.z).^2
    ylag2 = (dt.numberLag).^2

    z2sum = sum(z2)
    ylag2sum = sum(ylag2)
    zylagsum = dt.z ⋅ dt.numberLag

    # use the inverse of (X'X) derived by hand to solve X'X θ̂ = X'y,
    # where X = [z, numberLag], y = number
    invMatrix = [ylag2sum -zylagsum; -zylagsum z2sum]./
                (z2sum * ylag2sum - zylagsum^2)

    λψ = invMatrix * [dt.z ⋅ dt.number, dt.numberLag ⋅ dt.number]

    # if x(β) is a solution to A(β)x = b(β), then ∂x/∂β can be obtained from
    # ∂x/∂βᵢ = A^{-1}(∂b/∂βᵢ - ∂A/∂βᵢ x(β))
    n = length(dt.z)
    k = length(regressors)
    X = convert(Array, dt[:, regressors]) |> transpose

    # ∂A is a k=dim(β) by 2 matrix where entry i,1 is ∂A/∂βᵢ[1,1] and
    # entry i,2 is ∂A/∂βᵢ[2,1] = ∂A/∂βᵢ[1,2]. ∂A/∂βᵢ[2,2] = 0.
    ∂A = X * [dt.z dt.numberLag]
    ∂x = zeros(Float64, 2, k)

    # ∂b is a k×1 vector with entries ∂b[i] = xᵢ ⋅ y, which we then use to
    # construct ∂b/∂βᵢ = [xᵢ ⋅ y, 0]'.
    ∂b = X * dt.number

    for i in 1:k
        ∂Aᵢ = [2 * ∂A[i, 1] ∂A[i, 2]; ∂A[i, 2] 0.0]
        ∂bᵢ = [∂b[i], 0]
        ∂x[:, i] .= invMatrix * (∂bᵢ - ∂Aᵢ * λψ)
    end

    ∂λ = ∂x[1, :]
    ∂ψ = ∂x[2, :]
    # Let i here denote one period for game j. Then we have
    # yᵢ = ψⱼy.lagᵢ + λⱼ(1 + xᵢ'β) + uᵢ. The conditional expectation
    # function is m(β) = ψⱼ(β)y.lagᵢ + λⱼ(β)(1 + xᵢ'β), and its derivative is
    # ∂m/∂β = ∂ψⱼ/∂β y.lagᵢ + ∂λⱼ/∂β(1 + xᵢ'β) + λⱼxᵢ
    # We have calculated ∂λⱼ/∂β  and ∂ψⱼ/∂β in the columns of ∂x. The
    # matrix with column i being  ∂m/∂β(obsᵢ, β) is below

    ∂m = ∂ψ  * transpose(dt.numberLag) + ∂λ * transpose(dt.z) + λψ[1] .* X
    ∂m_e = ∂m * diagm(dt.error)

    return (sum_m2 = ∂m * transpose(∂m), sum_m2e = ∂m_e * transpose(∂m_e))
end

function varβ(β; data = panel)
    # use the value of β to calculate z_it = 1 + x_it β and the
    data[:, :z] .= convert(Array, data[:,regressors]) * β .+ 1

    sum_m2, sum_m2e = variance_one_game!(data[in.(data.id, Ref([1])), :])

    for i in 2:n
        one_game = variance_one_game!(data[in.(data.id, Ref([i])), :])
        sum_m2, sum_m2e = (sum_m2 .+ one_game.sum_m2, sum_m2e .+ one_game.sum_m2e)
    end

    nT =size(data, 1)
    E∂m2 = sum_m2 ./ nT
    E∂m2e = sum_m2e ./ nT

    E∂m2_inv = inv(E∂m2)

    #return V_β̂, not the asymptotic variance of β̂ defined by √n(β̂-β₀)∼N(0, Vᵦ),
    # by dividing by the number of observations
    return (E∂m2_inv * E∂m2e * E∂m2_inv) ./ nT
end


function testβ(β, var_β̂; digits = 3)
    # Calculates standard errors and prettifies the results into a
    # DataFrame

    table = DataFrame(regressor = String[], coef = String[], value = String[])

    beta = round.(β, digits = digits)
    errors = round.(sqrt.(diag(var_β̂)), digits = digits)
    t = (β ./ (sqrt.(diag(var_β̂))))
    p = @. 2 * (1 - cdf(Normal(), abs(t))) |> x -> round(x, digits = digits)

    for i in 1:length(regressors)
        if p[i] ≤ 0.01
            stars = "ˣˣˣ"
        elseif p[i] ≤ 0.05
            stars = "ˣˣ"
        elseif p[i] ≤ 0.1
            stars = "ˣ"
        else
            stars = ""
        end

        beta_str = string(beta[i]) * stars

        push!(table, (string.(regressors[i]), "est", beta_str))
        push!(table, (string.(regressors[i]), "s.e.", "(" * string(errors[i]) * ")"))
    end

    return table;
end


function testLatexβ(β, var_β̂)
    # Calculates standard errors and prettifies the results into a
    # DataFrame

    table = DataFrame(regressor = String[], coef = String[], value = Float64[])

    errors = sqrt.(diag(var_β̂))
    t = (β ./ errors)
    p = @. 2 * (1 - cdf(Normal(), abs(t)))

    for i in 1:length(regressors)
        push!(table, (string.(regressors[i]), "est",  β[i]))
        push!(table, (string.(regressors[i]), "s.e.", errors[i]))
        push!(table, (string.(regressors[i]), "pval", p[i]))
    end

    return table
end

function print_tests(tables...; omit = [])
    # assembles several outputs of testβ into a nicely looking
    # regression table
    if length(tables) > 1
        table = join(tables..., on = [:regressor, :coef], kind = :outer; makeunique = true)
    else
        table = tables[1]
    end

    omit = string.(omit)
    table = table[Not((1:end)[in.(table.regressor, Ref(omit))]), :]

    new_names = [Pair(Symbol("value_" * string(i)),
                      Symbol("(" * string(i+1) * ")")) for i in 1:(length(tables)-1)]
    new_names = [new_names; Pair(:value, Symbol("(1)"))]
    table = rename(table, new_names)

    table[[2*i for i in 1:(div(size(table,1), 2))], :regressor] .= ""

    return table[:, Not(:coef)]
end

function print_tests_latex(tables...; stats = nothing, omit = [])
    # assembles several outputs of testβ into a nicely looking
    # regression table
    if length(tables) > 1
        table = join(tables..., on = [:regressor, :coef], kind = :outer; makeunique = true)
    else
        table = tables[1]
    end

    omit = string.(omit)
    table = table[Not((1:end)[in.(table.regressor, Ref(omit))]), :]

    new_names = [Pair(Symbol("value_" * string(i)),
                      Symbol("(" * string(i+1) * ")")) for i in 1:(length(tables)-1)]
    new_names = [new_names; Pair(:value, Symbol("(1)"))]
    table = rename(table, new_names)

    table[[3*i for i in 1:(div(size(table,1), 3))], :regressor] .= "p-val"
    table[[3*i-1 for i in 1:(div(size(table,1), 3))], :regressor] .= "s.e."

    table = table[:, Not(:coef)]

    if stats != nothing
        table = [table; stats]
    end

    return table
end
