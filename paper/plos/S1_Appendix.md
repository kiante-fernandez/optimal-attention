---
title: "S1 Appendix. Derivations, policies, parameter search, and validation"
subtitle: "Fixation patterns in multi-response choice do not reflect optimal information sampling"
author: "Fernandez, Callaway, and Krajbich"
---

Equation numbers refer to the main text.

## A. Distribution of the posterior mean after $N$ samples (Eq 9)

Consider item $c$ with current belief $u^{(c)} \sim \mathcal{N}(\mu, 1/\lambda)$ and $N$ further samples $x_1, \dots, x_N$, each $\mathcal{N}(u^{(c)}, \sigma_x^2)$ given $u^{(c)}$. Applying Eq 2 $N$ times is the same as one update with the sample mean $\bar{x}$ at precision $\lambda_{\text{obs}} = N / \sigma_x^2$:
$$
\tilde{\lambda} = \lambda + \lambda_{\text{obs}}, \qquad \tilde{\mu} = w \bar{x} + (1 - w)\mu, \qquad w = \frac{\lambda_{\text{obs}}}{\lambda + \lambda_{\text{obs}}}.
$$
Before the samples are observed, $\bar{x}$ is the sum of the uncertain value and the sampling noise, so its marginal distribution under the current belief is $\mathcal{N}(\mu, 1/\lambda + 1/\lambda_{\text{obs}})$. The updated mean is a linear function of $\bar{x}$, hence
$$
\tilde{\mu} \sim \mathcal{N}\!\left(\mu, \sigma_{\tilde{\mu}}^2\right), \qquad \sigma_{\tilde{\mu}}^2 = w^2 \left(\frac{1}{\lambda} + \frac{1}{\lambda_{\text{obs}}}\right) = \frac{1}{\lambda} - \frac{1}{\lambda + \lambda_{\text{obs}}},
$$
which is Eq 9. The second form shows that the variance of the future mean equals the reduction in the variance of the belief, and that $\mathbb{E}[\tilde{\mu}] = \mu$: the posterior mean is a martingale. The beliefs about the other items do not change.

## B. Value of information for subset selection (Eq 10)

Let $\mu_{(1)} \ge \dots \ge \mu_{(n)}$ be the current means in descending order and $S$ the set of the $k$ items with the highest means, so that $R(b, \bot) = \sum_{i \in S} \mu^{(i)}$ (Eq 5). After $N$ samples of item $c$ only $\mu^{(c)}$ changes, to $\tilde{\mu}$; the new terminal reward is the sum of the $k$ largest elements of the multiset $\{\tilde{\mu}\} \cup \{\mu^{(i)} : i \neq c\}$. The value of information is $\text{VOI}_N(b, c) = \mathbb{E}[R(\tilde{b}, \bot)] - R(b, \bot)$, where $\tilde{b}$ is the belief after the samples.

*Item $c$ not selected* ($c \notin S$). The $k - 1$ largest means among the other items are $\mu_{(1)}, \dots, \mu_{(k-1)}$, all members of $S$, and the $k$-th largest is $\mu_{(k)}$, the lowest selected mean. The new reward is therefore $\sum_{i=1}^{k-1} \mu_{(i)} + \max(\tilde{\mu}, \mu_{(k)})$, and
$$
\text{VOI}_N(b, c) = \mathbb{E}\!\left[\max(\tilde{\mu}, \mu_{(k)})\right] - \mu_{(k)} = \mathbb{E}\!\left[(\tilde{\mu} - \mu_{(k)})^{+}\right].
$$

*Item $c$ selected* ($c \in S$). The $k - 1$ largest means among the other items are the other members of $S$, and the $k$-th largest is $\mu_{(k+1)}$, the highest unselected mean. The new reward is $\sum_{i \in S, i \neq c} \mu^{(i)} + \max(\tilde{\mu}, \mu_{(k+1)})$, so
$$
\text{VOI}_N(b, c) = \mathbb{E}\!\left[\max(\tilde{\mu}, \mu_{(k+1)})\right] - \mu^{(c)} = \mathbb{E}\!\left[(\mu_{(k+1)} - \tilde{\mu})^{+}\right],
$$
using $\max(a, x) = x + (a - x)^{+}$ and $\mathbb{E}[\tilde{\mu}] = \mu^{(c)}$.

Both expectations are integrals of a Gaussian tail. For $X \sim \mathcal{N}(\mu, \sigma^2)$ and $z = (a - \mu)/\sigma$,
$$
\mathbb{E}\!\left[(X - a)^{+}\right] = \sigma\left[\phi(z) - z\,(1 - \Phi(z))\right], \qquad \mathbb{E}\!\left[(a - X)^{+}\right] = \sigma\left[\phi(z) + z\,\Phi(z)\right],
$$
where $\phi$ and $\Phi$ are the standard normal density and distribution functions. The first identity follows from $\int_a^\infty (x - a)\,\mathcal{N}(x; \mu, \sigma^2)\,dx = (\mu - a)(1 - \Phi(z)) + \sigma \phi(z)$, and the second from $(a - X)^{+} = (X - a)^{+} - (X - a)$ together with $\mathbb{E}[X - a] = -\sigma z$. Substituting $X = \tilde{\mu}$, $\sigma = \sigma_{\tilde{\mu}}$, and $a = \text{cv}(c)$ gives Eq 10. In the code the same quantity is computed as $\mathbb{E}[\max(\tilde{\mu}, \text{cv})]$ with a truncated normal, minus the baseline, which is algebraically identical.

For $k = 1$ only the first case arises, with $\text{cv}(c) = \mu_{(1)}$ for every non-leading item, and the second case reduces to the value of sampling the leader against the runner-up. These are the quantities that drive attention to the top two items in Callaway et al. (2021).

## C. Directed cognition bounds the value of computation from below

The plan "sample item $c$ exactly $N$ times, then terminate" is a feasible policy from belief $b$. Its expected total reward relative to terminating now is $\text{VOI}_N(b, c) - \text{cost}(c) - (N - 1)\gamma_{\text{sample}}$, because the switching cost is paid at most once, on the first sample. The optimal continuation after sampling $c$ once does at least as well as the best such plan, so
$$
\text{VOC}(b, c) \;\ge\; \max_{N \ge 1}\left[\text{VOI}_N(b, c) - \text{cost}(c) - (N - 1)\gamma_{\text{sample}}\right] = \text{VOC}_{\text{DC}}(b, c).
$$
A policy that terminates when $\max_c \text{VOC}_{\text{DC}}(b, c) \le 0$ may therefore stop in states where the optimal policy would continue, in particular where the value of sampling lies in a sequence that alternates between items. The maximization over $N$ treats $N$ as continuous on $[1, 100]$ and uses golden-section search; the maximand is unimodal in $N$ in every case we examined.

## D. Selecting $k$ items is rejecting $n - k$

Let $\pi^*_{n,k}$ be an optimal policy for the problem with $n$ items and $k$ to be selected. Write the terminal reward as
$$
R_k(b, \bot) = \sum_{i=1}^{k} \mu_{(i)} = \sum_{i=1}^{n} \mu^{(i)} - \sum_{i=k+1}^{n} \mu_{(i)}.
$$
The first term is the sum of all posterior means. Under any policy, each $\mu_t^{(i)}$ is a martingale with respect to the information gathered (Appendix A), and the policy's choice of what to sample does not change the expected value of a martingale at a stopping time, so $\mathbb{E}[\sum_i \mu_T^{(i)}] = \sum_i \mu_1^{(i)}$ for every policy and every stopping rule. Maximizing the expected value of $R_k(b_T, \bot)$ minus costs is therefore the same as maximizing the expected value of $-\sum_{i=k+1}^{n} \mu_{(i)}$ minus costs.

Now define the reflected belief $\bar{b} = (-\boldsymbol{\mu}, \boldsymbol{\lambda}, \text{last})$. Negating every observation, $x_t \mapsto -x_t$, maps the belief trajectory of a problem with values $u$ onto the trajectory of a problem with values $-u$ under the same policy (Eq 2 is linear in $x_t$ and $\mu$, and the precision update does not involve them), and the costs (Eq 3) do not depend on the observations. The $n - k$ lowest means of $\boldsymbol{\mu}$ are the $n - k$ highest means of $-\boldsymbol{\mu}$, so $-\sum_{i=k+1}^{n} \mu_{(i)} = R_{n-k}(\bar{b}, \bot)$. Hence $\pi^*_{n,k}(b) = \pi^*_{n,n-k}(\bar{b})$: the optimal policy for selecting $k$ at belief $b$ is the optimal policy for selecting $n - k$ at the reflected belief.

The directed-cognition approximation has the same property exactly. Under reflection, the item with the $k$-th highest mean becomes the item with the $(n-k+1)$-th lowest, selected items become unselected items, and the competing value $\mu_{(k)}$ (lowest selected) becomes $-\mu_{(k)}$, which is the highest unselected mean of the reflected problem; the two cases of Eq 10 exchange, with $z \mapsto -z$, and the identity $\phi(-z) + (-z)\Phi(-z) = \phi(z) - z(1 - \Phi(z))$ shows that the values coincide. For $n = 4$ and $k = 3$ the policy therefore allocates to the two lowest estimates exactly what the $k = 1$ policy allocates to the two highest (Fig 2).

## E. The response policies as algorithms

All three policies use the same primitive, $\mathrm{Step}(b, A, k')$: evaluate $\text{VOC}_{\text{DC}}(b, c)$ for each $c \in A$ in the problem with $|A|$ items of which $k'$ are to be chosen (beliefs restricted to $A$; $\text{last}$ as in $b$, counting as a switch if the last-fixated item is no longer in $A$), draw $c$ from the softmax over $A \cup \{\bot\}$ with $\text{VOC}_{\text{DC}}(b, \bot) = 0$, and if $c \neq \bot$ sample item $c$ and update $b$ by Eqs 1 and 2.

*Simultaneous.* Set $A = \{1, \dots, n\}$. Repeat $\mathrm{Step}(b, A, k)$ until it returns $\bot$. Report the $k$ items of $A$ with the highest means in descending order of mean; all responses are made at the step of termination.

*Best-first.* Set $A_1 = \{1, \dots, n\}$. For $r = 1, \dots, k$: repeat $\mathrm{Step}(b, A_r, 1)$ until it returns $\bot$; commit $i_r = \arg\max_{i \in A_r} \mu^{(i)}$ at the current step; set $A_{r+1} = A_r \setminus \{i_r\}$. When $|A_r| = k - r + 1$ the remaining items are committed without sampling.

*Commit when certain.* Set $A = \{1, \dots, n\}$ and $k' = k$. While $k' > 0$: if $|A| = k'$, commit all of $A$ and stop. Otherwise let $S$ be the $k'$ items of $A$ with the highest means, $s$ the member of $S$ with the lowest mean, and $U = A \setminus S$; compute $v_c = \text{VOC}_{\text{DC}}(b, c)$ for $c \in A$ in the problem $(|A|, k')$; let $F = \{i \in S : v_i \le 0 \text{ and } (i \neq s \text{ or } \max_{j \in U} v_j \le 0)\}$. If $F$ is nonempty, commit its members in descending order of mean at the current step, set $A = A \setminus F$ and $k' = k' - |F|$, and continue without sampling. If $F$ is empty, run one $\mathrm{Step}(b, A, k')$; if it returns $\bot$, commit all of $S$ in descending order of mean and stop.

In every policy, consecutive samples of the same item are concatenated into one fixation, each sample lasts 100 ms, and the response times are the sample counts at which items are committed.

## F. Parameter search

Each policy has three parameters, $\theta = (\sigma_x, \gamma_{\text{sample}}, \gamma_{\text{switch}})$, in units of the standard deviation of the participant's ratings. Stage 1 drew 200 configurations log-uniformly from $\sigma_x \in [1, 6]$, $\gamma_{\text{sample}} \in [0.0005, 0.03]$, and $\gamma_{\text{switch}} \in [0.001, 0.3]$. Stage 2 drew 150 configurations log-uniformly from the box spanned by the ten best stage-1 configurations, widened by a factor of 1.3 on each side. Every configuration was simulated once on each even-numbered trial with usable response time and gaze data (9857 trials), with the true values set by Eq 11, and scored by Eq 12. The best configuration of stage 2 was then simulated five times on every usable trial for the figures, which show the odd-numbered trials. Because the simulation is stochastic, the loss surface is noisy; the ten best stage-2 configurations of each policy lie within a factor of about 1.5 of each other on every parameter, and their predictions are visually indistinguishable in the figures.

## G. Validation of the directed-cognition approximation against the exact optimum

The belief space of the Gaussian model is continuous and seven-dimensional, so the optimal policy cannot be computed exactly. Following the validation in Callaway et al. (2021), we therefore compared directed cognition with the exact optimal policy in a Bernoulli version of the subset-selection problem, in which the belief space is discrete. Each of four items has an unknown success probability $p_i \sim \text{Beta}(1, 1)$; a sample of item $c$ is a Bernoulli$(p_c)$ outcome; the belief about an item is its count of successes and failures plus one, with posterior mean $\alpha_i / (\alpha_i + \beta_i)$; the terminal reward is the sum of the $k$ highest posterior means; the costs are as in Eq 3; and at most $T = 12$ samples may be taken per trial, a cap known to the optimal policy. The optimal value $V^*$ follows by backward induction over the reachable count vectors. Directed cognition is defined as in Eq 8, with $\text{VOI}_N$ computed by summing Eq 10's gain over the beta-binomial predictive distribution of the successes in $N$ samples and with $N$ capped by the samples remaining. The value of the directed-cognition policy was computed exactly by evaluating it on the same state space, and the best-first and commit-when-certain policies were evaluated by simulation (20000 trials; the simulated value of the directed-cognition policy agreed with its exact value to within 0.003 in every case).

Table G1 gives the results for selection sizes 1 to 3 at two sampling costs and two switching costs. Directed cognition earned between 96.7% and 99.6% of the optimal expected reward, least when switching was costly, where committing to a plan of samples of one item forgoes the option to alternate. For comparison, the policy-search approximation of Callaway et al. (2021) achieved 92% of the optimal reward in the worst case of its Bernoulli validation. The exact optimum also confirms the symmetry of the main text: at every cost setting, $V^*$ for $k = 3$ exceeded $V^*$ for $k = 1$ by exactly 1.0000, which is the identity $V^*_{4,3} = V^*_{4,1} + \mathbb{E}[\sum_i p_i] - 1$ implied by Appendix D with $\mathbb{E}[\sum_i p_i] = 2$. Measured against the true optimum, the best-first decomposition cost nothing when two items were chosen and between 0.5% and 3.5% of the optimal reward when three were, more when switching was costly, which is the same ordering as in the Gaussian model (Table 3); the larger losses in the Gaussian model arise at the fitted switching costs, which are far larger relative to the sampling cost than the values used here.

Table G1. Expected reward of the exact optimal policy and of the three approximate policies in the Bernoulli model with four items and at most 12 samples per trial.

| k | γ_sample | γ_switch | V* (exact optimum) | Directed cognition | DC / V* | Best-first / V* | Commit when certain / V* |
|---|---|---|---|---|---|---|---|
| 1 | 0.002 | 0.000 | 0.7274 | 0.7201 | 0.990 | 0.988 | 0.988 |
| 1 | 0.002 | 0.020 | 0.6860 | 0.6672 | 0.973 | 0.972 | 0.972 |
| 1 | 0.010 | 0.000 | 0.6707 | 0.6621 | 0.987 | 0.987 | 0.987 |
| 1 | 0.010 | 0.020 | 0.6342 | 0.6133 | 0.967 | 0.967 | 0.967 |
| 2 | 0.002 | 0.000 | 1.3017 | 1.2916 | 0.992 | 0.994 | 0.990 |
| 2 | 0.002 | 0.020 | 1.2431 | 1.2191 | 0.981 | 0.979 | 0.973 |
| 2 | 0.010 | 0.000 | 1.2338 | 1.2289 | 0.996 | 0.987 | 0.985 |
| 2 | 0.010 | 0.020 | 1.1756 | 1.1533 | 0.981 | 0.979 | 0.980 |
| 3 | 0.002 | 0.000 | 1.7274 | 1.7201 | 0.996 | 0.982 | 0.993 |
| 3 | 0.002 | 0.020 | 1.6860 | 1.6672 | 0.989 | 0.966 | 0.987 |
| 3 | 0.010 | 0.000 | 1.6707 | 1.6623 | 0.995 | 0.978 | 0.992 |
| 3 | 0.010 | 0.020 | 1.6342 | 1.6133 | 0.987 | 0.974 | 0.988 |

## H. Parameter and policy recovery

To check that the fitting objective (Eq 12) identifies the parameters and distinguishes the policies, we treated the simulation of each policy at its fitted parameters (one repetition on the even-numbered trials with gaze, the same trials used for fitting) as data, computed the same summary statistics, and ran the same two-stage search for each of the three policies. Table H1 gives the loss of the best configuration of each fitted policy on the data generated by each policy. Each generating policy was best fitted by itself, and the margins were large: the losses on the diagonal were between 0.5 and 2.5, and the smallest loss off the diagonal was 6.8. The objective therefore separates the three response policies, which differ most in the timing of the first response and in the growth of fixation time with the selection size. Table H2 compares the generating parameters with the recovered ones, the mean and standard deviation of the ten best configurations of the refinement stage. All three parameters were recovered to within about 10% of their generating values. The spread of the recovered sampling costs reflects the trade-off between $\sigma_x$ and $\gamma_{\text{sample}}$ noted by Callaway et al. (2021): a noisier signal with a cheaper sample produces nearly the same number of samples per trial.

Table H1. Policy recovery. Loss (Eq 12) of the best configuration of each fitted policy (columns) on the data generated by each policy (rows).

| Generating policy | Fitted: Simultaneous | Fitted: Best-first | Fitted: Commit when certain | Best-fitting policy |
|---|---|---|---|---|
| Simultaneous | 0.7 | 16.2 | 17.5 | Simultaneous |
| Best-first | 20.3 | 2.5 | 33.2 | Best-first |
| Commit when certain | 6.8 | 21.1 | 0.5 | Commit when certain |

Table H2. Parameter recovery. Generating values are the best-fitting parameters of each policy (Table 2 of the main text); recovered values are the mean ± SD of the ten best configurations of the refinement stage when the policy is fitted to its own simulated data.

| Policy | σ_x generating | σ_x recovered | γ_sample generating | γ_sample recovered | γ_switch generating | γ_switch recovered |
|---|---|---|---|---|---|---|
| Simultaneous | 1.91 | 1.89 ± 0.28 | 0.0011 | 0.0013 ± 0.0005 | 0.146 | 0.143 ± 0.037 |
| Best-first | 2.46 | 2.54 ± 0.34 | 0.0034 | 0.0038 ± 0.0016 | 0.118 | 0.115 ± 0.053 |
| Commit when certain | 1.96 | 1.94 ± 0.35 | 0.0015 | 0.0018 ± 0.0009 | 0.072 | 0.072 ± 0.016 |

## I. Eye-movement descriptives

Table I1 gives the full set of trial-level behavioral and eye-movement measures by selection size, including the rows omitted from Table 1 of the main text. Fixations are contiguous runs of gaze samples on one item (Methods); first, middle, and last refer to position within the trial; a refixation is a fixation on an item already fixated in the trial; gaze entropy is computed over the four items' shares of fixation time.

Table I1. Behavior and eye movements by selection size. Means (SDs) over participants.

| Measure | Choose 1 | Choose 2 | Choose 3 |
|---|---|---|---|
| Trials analyzed | 6637 | 6457 | 6620 |
| Response time, last keypress (ms) | 1980 (751) | 2650 (1033) | 3084 (1318) |
| Chose exactly the top-$k$ set | 0.57 (0.16) | 0.47 (0.15) | 0.58 (0.15) |
| First response is the best item | n/a | 0.50 (0.13) | 0.46 (0.13) |
| Responses in rank order | n/a | 0.61 (0.11) | 0.29 (0.12) |
| Fixations per trial | 2.93 (1.21) | 3.64 (1.61) | 4.05 (2.08) |
| Items fixated at least once (of 4) | 2.16 (0.62) | 2.38 (0.64) | 2.51 (0.69) |
| Total fixation time (ms) | 1210 (540) | 1679 (772) | 1989 (991) |
| Fraction of trial time on items | 0.61 (0.13) | 0.62 (0.12) | 0.63 (0.11) |
| Fixation duration, all (ms) | 422 (152) | 484 (199) | 524 (233) |
| First fixation duration (ms) | 521 (223) | 638 (358) | 696 (389) |
| Middle fixation duration (ms) | 311 (90) | 340 (84) | 372 (108) |
| Last fixation duration (ms) | 502 (226) | 634 (341) | 673 (357) |
| Refixations (item seen before in the trial) | 0.34 (0.13) | 0.41 (0.14) | 0.43 (0.15) |
| All four items in the first four fixations | 0.13 (0.12) | 0.12 (0.12) | 0.13 (0.12) |
| First fixation on the best item | 0.25 (0.05) | 0.25 (0.05) | 0.26 (0.05) |
| Last fixation on a chosen item | 0.37 (0.13) | 0.59 (0.09) | 0.76 (0.06) |
| Gaze entropy (bits; 2 is uniform) | 0.74 (0.37) | 0.85 (0.38) | 0.91 (0.41) |
| Dwell share, best item | 0.270 (0.051) | 0.264 (0.045) | 0.242 (0.035) |
| Dwell share, worst item | 0.233 (0.046) | 0.233 (0.035) | 0.249 (0.035) |
