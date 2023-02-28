# UC-AI-Thinkathon-2023

Our entry for https://itch.io/jam/thinkathon-ia

Add a description here

[Read more here](LINK PDF)

To run the scripts, just clone the repo, and make sure you have the openai python module (`pip install openai`).

Then you can run the `run_prompt_array.py` script which will go through the conditions, questions and question wrappers described in the `.tsv` files in the `input/` directory.


To run the R files and create plots, the following modules need to be installed in your system: (tidyverse, lme4, sjPlot, conflicted).

The `classify_responses.R` script classifies the responses according as "affirmation", "noncommital" and "negation". This only works for known responses.

The `analysis.R` file was used for our analysis, as well as the included Excel file.
