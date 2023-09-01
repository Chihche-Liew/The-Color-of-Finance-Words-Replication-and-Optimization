import torch
import pandas as pd
from transformers import BertForSequenceClassification, BertTokenizerFast
import shap
import re
import nltk
from nltk.corpus import stopwords


device = torch.device('cuda:0' if torch.cuda.is_available() else 'cpu')
model = BertForSequenceClassification.from_pretrained("bert-base-uncased", num_labels=1)
model.load_state_dict(torch.load('./bert_regression_conversation.pth'), False)
model.to(device)
tokenizer = BertTokenizerFast.from_pretrained("bert-base-uncased")


class WrappedModel:
    def __init__(self, model):
        self.model = model
        self.tokenizer = tokenizer

    def predict(self, text_list):
        input_ids = []
        attention_masks = []
        for text in text_list:
            inputs = self.tokenizer(text,
                                    padding='max_length',
                                    truncation=True,
                                    max_length=256,
                                    return_tensors="pt")
            input_ids.append(inputs["input_ids"])
            attention_masks.append(inputs["attention_mask"])
        input_ids = torch.cat(input_ids)
        attention_masks = torch.cat(attention_masks)
        input_ids = input_ids.to(device)
        attention_masks = attention_masks.to(device)
        with torch.no_grad():
            outputs = self.model(input_ids, attention_mask=attention_masks)
        return outputs.logits


wrapped_model = WrappedModel(model)

data = pd.read_pickle('./data/con_data.pkl', compression='zip')


def preprocess_text(x):
    x = x.lower()

    x = re.sub(r'[^\w\s]', '', x)

    words = nltk.word_tokenize(x)

    stop_words = set(stopwords.words("english"))
    words = [word for word in words if word not in stop_words]

    processed_text = " ".join(words)

    return processed_text


data['transcript'] = data['transcript'].apply(preprocess_text)


def split_text(text, max_length):
    words = text.split()
    chunks = []
    current_chunk = ""

    for word in words:
        if len(current_chunk) + len(word) + 1 <= max_length:
            if current_chunk:
                current_chunk += " "
            current_chunk += word
        else:
            chunks.append(current_chunk)
            current_chunk = word

    if current_chunk:
        chunks.append(current_chunk)

    return chunks


chunk_size = 256
new_rows = []
for index, row in data.iterrows():
    text_chunks = split_text(row['transcript'], chunk_size)
    for chunk in text_chunks:
        new_row = row.copy()
        new_row['transcript'] = chunk
        new_rows.append(new_row)

data = pd.DataFrame(new_rows)
del chunk_size, new_rows

importance = pd.DataFrame(columns=['word', 'importance'])

explainer = shap.Explainer(wrapped_model.predict,
                           masker=shap.maskers.Text(tokenizer))

transcript_texts = data['transcript'].iloc[:].tolist()

num_parts = 10
part_size = len(transcript_texts) // num_parts
transcript_parts = [transcript_texts[i: i + part_size]
                    for i in range(0, len(transcript_texts), part_size)]


for i, part in enumerate(transcript_parts):
    shap_values = explainer(part)
    df = pd.DataFrame([shap_values[0].data, shap_values[0].values]).T.rename({0: 'word', 1: 'importance'}, axis=1)
    df['importance'] = df['importance'].apply(lambda x: x[0])
    df = df[df['word'] != '']
    df.to_csv('./word_importance_x_' + str(i) + '.csv')
    importance = pd.concat([importance, df], ignore_index=True)
