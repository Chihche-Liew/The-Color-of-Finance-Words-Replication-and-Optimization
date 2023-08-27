import torch
import pandas as pd
from transformers import BertForSequenceClassification, BertTokenizer
import shap
#%%
device = torch.device('cuda:0' if torch.cuda.is_available() else 'cpu')
model = BertForSequenceClassification.from_pretrained("bert-base-uncased", num_labels=1)
model.load_state_dict(torch.load('./bert_regression_conversation.pth'), False)
model.to(device)

tokenizer = BertTokenizer.from_pretrained("bert-base-uncased")


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


def split_text(text, chunk_size):
    return [text[i: i + chunk_size] for i in range(0, len(text), chunk_size)]


data = pd.read_csv('./data/con_data.csv')
chunk_size = 256
new_rows = []
for index, row in data.iterrows():
    text_chunks = split_text(row['transcript'], chunk_size)
    for chunk in text_chunks:
        new_row = row.copy()
        new_row['transcript'] = chunk
        new_rows.append(new_row)

data = pd.DataFrame(new_rows)
transcript_texts = data['transcript'].tolist()
del chunk_size, new_rows

explainer = shap.Explainer(wrapped_model.predict, masker=shap.maskers.Text(tokenizer))

shap_values = explainer(transcript_texts)

importance = pd.DataFrame([shap_values[0].data, shap_values[0].values]).T.rename({0: 'word', 1: 'importance'}, axis=1)
importance['importance'] = importance['importance'].apply(lambda x: x[0])
importance = importance.loc[importance['word'] != '']

importance.to_csv('./word_importance_bert.csv')
