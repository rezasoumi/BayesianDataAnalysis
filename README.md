# Bayesian Workflow on Time to Reach Global Top 20 in Tennis 🎾

This repository contains the materials and code for our Bayesian Data Analysis project, completed as part of the Bayesian Data Analysis course instructed by **Aki Vehtari** at **Aalto University**. 

The project investigates the time it takes for professional tennis players to reach the global top 20 rankings, using Bayesian modeling techniques. We utilized **Gamma** and **Gaussian** models to explore regional and country-level effects.

---

## Project Overview 📊

### Problem Definition
- **Goal**: Analyze the time it takes for tennis players to reach the global top 20 and identify regional/country-level effects on their progression.
- **Dataset**: Sourced from Kaggle, containing 106,716 rows with 49 variables such as:
  - Match-level: Date, Surface, Duration
  - Player stats: Name, Nationality, Age
  - Rank info: Winner Rank, Loser Rank, Rank Points

### Key Insights
1. Regional and country-level effects significantly influence the time to top rankings.
2. **Gamma models** performed better than Gaussian models in capturing variability, despite some challenges with rare rapid risers.
3. Weakly informative priors ensured generalizability while balancing model stability.

---

## Files and Structure 📂

- **`data/`**: Contains the dataset (ensure appropriate licensing for redistribution).
- **`src/`**: Scripts for preprocessing, model fitting, and visualizations.
- **`docs/`**: Beamer presentation and associated materials.
- **`images/`**: Visualizations used in the presentation.
- **`models/`**: Code and outputs for Gamma and Gaussian models.

---

## Methods 🔧

### Statistical Models
1. **Gamma Model**:
   - Designed for skewed, positive data like time-to-event.
   - Formula: 
     \[
     y_i \sim \text{Gamma}(\lambda_i, \theta), \quad 
     \log(\lambda_i \theta) = \alpha + u_{\text{region}[i]} + u_{\text{country}[i]}
     \]
2. **Gaussian Model**:
   - Symmetric model assuming normally distributed data.
   - Formula:
     \[
     y_i \sim \mathcal{N}(\mu_i, \sigma), \quad 
     \mu_i = \alpha + u_{\text{region}[i]} + u_{\text{country}[i]}
     \]

### Priors
- Weakly informative priors were selected to ensure stable convergence:
  - Gamma model intercept: \( \text{Normal}(7, 1) \) (log-scale).
  - Gaussian model intercept: \( \text{Normal}(1000, 350) \).

---

## Results 📈

1. **Model Performance**:
   - Gamma model: Better generalization with \( \text{elpd\_loo} = -1543.9 \).
   - Gaussian model: Higher sampling efficiency but less reliable predictions.

2. **Posterior Predictive Checks**:
   - Gamma model outperformed in capturing variability of rare rapid risers.
   - Gaussian model over-predicted negative times (unrealistic for this problem).

3. **Key Findings**:
   - Players from Sweden and Russia exhibit shorter times to reach the top 20.
   - Asian players perform well on clay courts.

---

## Usage 🛠️

1. **Requirements**:
   - R with `brms` and `ggplot2` packages installed.
   - LaTeX distribution for rendering Beamer presentations.

2. **How to Run**:
   - Clone the repository:
     ```bash
     git clone https://github.com/<username>/<repository>.git
     ```
   - Run preprocessing and modeling script from the top of the code
     ```

---

## Authors 👥

- **Reza Soumi** - [GitHub](https://github.com/rezasoumi)
- **Alireza Honarvar** - [GitHub](https://github.com/aezexa)

---

## Acknowledgments 🙏

- **Instructor**: Aki Vehtari for his guidance during the Bayesian Data Analysis course.
- **Tools**: R, `brms`, and Beamer for LaTeX presentations.
- **Dataset**: Kaggle's tennis dataset.

---

## License 📜

This project is licensed under the MIT License. See `LICENSE` for details.

---

## Contact ✉️

For questions or collaborations, feel free to reach out:
- Reza Soumi: reza.soumi@aalto.fi
- Alireza Honarvar: alireza.honarvar@aalto.fi
