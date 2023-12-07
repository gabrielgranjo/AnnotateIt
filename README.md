# AnnotateIt

## Overview

AnnotateIt is a simple and user-friendly R package designed to streamline the process of text data annotation for classification tasks. Leveraging the power of Shiny, a web application framework for R, AnnotateIt creates an interactive annotation environment, ideal for projects that require accurate, human-labeled text data for training machine learning models. This package boosts the productivity of small groups in annotation tasks.

## Installation

### Install devtools if you haven't already

```R
if (!requireNamespace("devtools", quietly = TRUE)) {
    install.packages("devtools")
}
```

### Install AnnotateIt from GitHub

```R
devtools::install_github("gabrielgranjo/AnnotateIt")
```

## Usage

After installation, you can load AnnotateIt as follows:

```R
library(AnnotateIt)
```
