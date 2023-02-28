# LLM-Alignment-Hackathon-2022

**🔥🔥 Winning Entry 🔥🔥** for https://itch.io/jam/llm-hackathon

This investigation highlights the importance of guiding scripts for LLMs that will interact with human audiences, in particular how certain common, desirable traits may be in contention with the truth, as shown with "friendly" and "agreeable", even though our models all included in the script that they are "always truthful" and "always correct non-factual statements", the frequency of affirmative responses to non-factual claims increased just by the addition of an additional trait such as aggreeableness.

[Read more here](https://github.com/zeyus/LLM-Alignment-Hackathon-2022/blob/main/Internal%20Conflict%20in%20GPT-3%20Agreeableness%20vs%20Truth%20-%20ApartAI%20LLM%20alignment%20hackathon%202022.pdf)

To run the scripts, just clone the repo, and make sure you have the openai python module (`pip install openai`).

Then you can run the `run_prompt_array.py` script which will go through the conditions, questions and question wrappers described in the `.tsv` files in the `input/` directory.
