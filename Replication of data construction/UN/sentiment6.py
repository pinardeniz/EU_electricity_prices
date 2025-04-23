import os
import pandas as pd
import re
import nltk
from nltk.tokenize import sent_tokenize
from transformers import BertTokenizer, BertForSequenceClassification, pipeline

# Ensure NLTK tokenizer is downloaded
nltk.download('punkt')

# Load FinBERT model and tokenizer
finbert = BertForSequenceClassification.from_pretrained('yiyanghkust/finbert-tone', num_labels=3)
tokenizer = BertTokenizer.from_pretrained('yiyanghkust/finbert-tone')

# If use GPU if available (If no GPU, it runs on CPU), add this: , device=0
nlp = pipeline("sentiment-analysis", model=finbert, tokenizer=tokenizer)

# Define the folder containing text files (single folder)
folder_path = r"C:\Users\user\Google Drive\KONFERANS 2023-2024 CALISMALAR\GUELPH_RESEARCH2023\ENERGY PRICES\REGIONAL\IMF and UN_common\UN\UNtexts"

# Initialize a DataFrame to store results
results = []

# Function to clean text
def clean_text(text):
    # Remove everything before "//-->"
    text = re.sub(r".*//-->", "", text, flags=re.DOTALL)

    # Remove footer sentences like "Problems viewing this page? If so, please contact..."
    text = re.sub(r"Problems viewing this page\?.*?$", "", text, flags=re.DOTALL)

    return text.strip()

# Function to split, tokenize, filter, and truncate sentences
def split_and_filter_sentences(text, min_words=5, max_words=150, max_tokens=512):
    sentences = sent_tokenize(text)  # Use NLTK to split sentences
    filtered_sentences = []

    for s in sentences:
        # Tokenize sentence FIRST before checking word count
        tokens = tokenizer.tokenize(s)

        # 🚨 If sentence exceeds max tokens, truncate it immediately
        if len(tokens) > max_tokens:
            tokens = tokens[:max_tokens]  # Keep only the first 512 tokens
            s = tokenizer.convert_tokens_to_string(tokens)  # Convert back to string

        word_count = len(s.split())

        # ⚠️ Skip very short sentences (≤5 words) and extremely long ones (>150 words)
        if word_count < min_words or word_count > max_words:
            continue  

        filtered_sentences.append(s)

    return filtered_sentences

# Process each text file in the single folder
for file_name in os.listdir(folder_path):
    if file_name.endswith(".txt"):  # Ensure only text files are processed
        file_path = os.path.join(folder_path, file_name)

        with open(file_path, "r", encoding="utf-8", errors="ignore") as file:
            text = file.read()

        # Clean text (remove irrelevant sections)
        text = clean_text(text)

        # Split sentences (keep only those ≤150 words and ≥5 words)
        sentences = split_and_filter_sentences(text)

        total_negative = 0
        total_neutral = 0
        total_positive = 0
        total_sentences = len(sentences)

        #  Process sentences in batches (FASTER)
        if total_sentences > 0:
            batch_results = nlp(sentences)

            for result in batch_results:
                label = result["label"]
                score = result["score"]

                # Correct label assignment
                if label == "Positive":
                    total_positive += score
                elif label == "Negative":
                    total_negative += score
                elif label == "Neutral":
                    total_neutral += score

            # Compute sentiment intensity for the entire document
            avg_negative = total_negative / total_sentences
            avg_neutral = total_neutral / total_sentences
            avg_positive = total_positive / total_sentences
        else:
            avg_negative = avg_neutral = avg_positive = 0  # No valid sentences

        # Store results
        results.append([file_name, avg_negative, avg_neutral, avg_positive, total_sentences])

# Convert results into a DataFrame
df_results = pd.DataFrame(results, columns=["FileName", "AvgNegative", "AvgNeutral", "AvgPositive", "TotalSentences"])

# Save results to a CSV file
output_csv = os.path.join(folder_path, "sentiment.csv")
df_results.to_csv(output_csv, index=False, encoding="utf-8")

print(f"\n Processing completed. Results saved to {output_csv}")