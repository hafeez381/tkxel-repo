# Continuous Integration Pipeline for R Analysis

This repository contains a Continuous Integration (CI) pipeline for automating the process of code linting, building, and packaging an R-based analysis project. The pipeline leverages GitHub Actions to streamline workflow automation. This was made as an assignment submission during my internship at Tkxel in the DevOps department. 

## CI Pipeline Overview

The pipeline automates the following steps:
1. **Operating System Setup**: Uses `ubuntu-latest` as the runtime environment.
2. **Code Checkout**: Checks out the code from the repository.
3. **Install Dependencies**: Installs required R packages.
4. **Code Linting**: Performs syntax checks on R code using `lintr`.
5. **Code Execution**: Executes the `CI-Analysis.R` script.
6. **Code Zipping**: Archives the repository code into a ZIP file.
7. **Artifact Upload**: Saves the ZIP file as a GitHub artifact for later use.

## CI Pipeline Configuration (`ci-pipeline.yml`)

Below is the YAML configuration for the GitHub Actions workflow:

```yaml
name: CI Pipeline

on:
  push:
    branches: [ "main" ]
  pull_request:
    branches: [ "main" ]

jobs:
  build:
    runs-on: ubuntu-latest

    steps:
    - name: Checkout code
      uses: actions/checkout@v3

    - name: Set up R
      uses: r-lib/actions/setup-r@v2
      with:
        r-version: '4.4.1'

    - name: Install R dependencies
      run: |
        R -e "install.packages(c('ggplot2', 'mosaic', 'dplyr', 'lintr'), repos='http://cran.rstudio.com/')"

    - name: Lint R code
      run: |
        R -e 'lintr::lint_dir(path = "R")'
          
    - name: Run R script
      run: |
        Rscript CI-Analysis.R

    - name: ZIP code
      run: |
        zip -r code.zip .

    - name: Upload Artifact
      uses: actions/upload-artifact@v3
      with:
        name: code-artifact
        path: code.zip
```


## Prerequisites

To set up and use this CI pipeline, ensure the following prerequisites are met:

1. **GitHub Repository**:
   - Ensure your project is hosted on GitHub and has a proper repository structure similar to the one mentioned in the **Repository Structure** section.

2. **R Installation**:
   - This pipeline requires R version `4.4.1` or higher. You can download and install R from [CRAN](https://cran.r-project.org/).

3. **Required R Packages**:
   - The pipeline installs the following R packages automatically, but ensure they are compatible with your analysis:
     - `ggplot2`
     - `mosaic`
     - `dplyr`
     - `lintr`

4. **Script and Dataset Configuration**:
   - Make sure the `CI-Analysis.R` script is correctly set up to process the datasets in the `data/` directory.

5. **GitHub Actions Enabled**:
   - GitHub Actions must be enabled in your repository. You can enable it in the repository settings under the **Actions** tab.

---

## How to Use

Follow these steps to set up and run the CI pipeline:

1. **Clone the Repository**:
   - Clone this repository to your local machine:
     ```bash
     git clone https://github.com/<your-username>/<your-repo>.git
     ```

2. **Push Changes to the `main` Branch**:
   - Make your changes to the codebase, commit them, and push them to the `main` branch to trigger the CI pipeline:
     ```bash
     git add .
     git commit -m "Your commit message"
     git push origin main
     ```

3. **Monitor the Workflow**:
   - Go to the **Actions** tab of your GitHub repository to view the running pipeline. The pipeline will execute the steps defined in the `ci-pipeline.yml`.

4. **Download the Artifact**:
   - Once the workflow completes, the zipped code (`code.zip`) will be available as an artifact in the GitHub Actions summary. You can download it for further use.

## Notes on the Analysis

The CI-Analysis.R script is designed to process the datasets in the data/ directory and perform specific data analysis tasks. Ensure the following:
  - Data is properly formatted for the script.
  - Any changes to the data structure are reflected in the script.

