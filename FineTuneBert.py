import pandas as pd
import torch
import tqdm
import re
import nltk
from nltk.corpus import stopwords
from transformers import BertTokenizer, BertForSequenceClassification

# Preprocessing Text Data
df = pd.read_pickle('./data/con_data.pkl', compression='zip')


def preprocess_text(x):
    x = x.lower()

    x = re.sub(r'[^\w\s]', '', x)

    words = nltk.word_tokenize(x)

    stop_words = set(stopwords.words("english"))
    words = [word for word in words if word not in stop_words]

    processed_text = " ".join(words)

    return processed_text


df['transcript'] = df['transcript'].apply(preprocess_text)


def split_text(text, chunk_size):
    return [text[i:i + chunk_size] for i in range(0, len(text), chunk_size)]


chunk_size = 256
new_rows = []

for index, row in df.iterrows():
    text_chunks = split_text(row['transcript'], chunk_size)
    for chunk in text_chunks:
        new_row = row.copy()
        new_row['transcript'] = chunk
        new_rows.append(new_row)

df = pd.DataFrame(new_rows)
del chunk_size, new_rows

# Tokenizing and Encoding
texts = df["transcript"].tolist()
returns = df["ret"].tolist()

tokenizer = BertTokenizer.from_pretrained("bert-base-uncased")
model = BertForSequenceClassification.from_pretrained("bert-base-uncased", num_labels=1)
input_ids = []
attention_masks = []

for text in tqdm.tqdm(texts):
    encoded_dict = tokenizer.encode_plus(
        text,
        add_special_tokens=True,
        max_length=256,
        padding="max_length",
        return_attention_mask=True,
        return_tensors="pt"
    )

    input_ids.append(encoded_dict["input_ids"])
    attention_masks.append(encoded_dict["attention_mask"])

# Padding Tensors
input_ids = [ids.T for ids in input_ids]
max_length = max(tensor.shape[0] for tensor in input_ids)
input_ids = [torch.cat([ids, torch.zeros(max_length - ids.shape[0], 1)], dim=0)
             for ids in input_ids]
input_ids = [ids.T for ids in input_ids]
attention_masks = [mask.T for mask in attention_masks]
max_length = max(tensor.shape[0] for tensor in attention_masks)
attention_masks = [torch.cat([masks, torch.zeros(max_length - masks.shape[0], 1)], dim=0)
                   for masks in attention_masks]
attention_masks = [mask.T for mask in attention_masks]
input_ids = torch.cat(input_ids, dim=0)
attention_masks = torch.cat(attention_masks, dim=0)
returns = torch.tensor(returns, dtype=torch.float32)

# Generating Dataset
dataset = torch.utils.data.TensorDataset(input_ids, attention_masks, returns)
data_loader = torch.utils.data.DataLoader(dataset, batch_size=32, shuffle=True)

# Model Setting
## Optimizer and Loss Function
optimizer = torch.optim.AdamW(model.parameters(), lr=2e-5)
loss_fn = torch.nn.MSELoss()
## CUDA Setting
device = torch.device('cuda:0' if torch.cuda.is_available() else 'cpu')
model.to(device)
print(model.device)
## Epochs Setting
num_epochs = 10
loss_history = []

# Training
for epoch in range(num_epochs):
    model.train()
    total_loss = 0

    for batch in tqdm.tqdm(data_loader):
        batch = [item.to(device) for item in batch]
        input_ids, attention_mask, labels = batch
        input_ids = input_ids.to(torch.long)
        attention_mask = attention_mask.to(torch.long)
        input_ids = input_ids.to(device)
        attention_mask = attention_mask.to(device)

        optimizer.zero_grad()
        outputs = model(input_ids, attention_mask=attention_mask)
        preds = outputs["logits"].squeeze()

        loss = loss_fn(preds, labels)
        loss.backward()
        optimizer.step()

        total_loss += loss.item()

    average_loss = total_loss / len(data_loader)
    loss_history.append({'Epoch': epoch + 1, 'Loss': average_loss})

    print(f"Epoch {epoch + 1}, Loss: {total_loss}")

# Saving Parameters and Epoch-loss History
torch.save(model.state_dict(), './bert_regression_conversation.pth')
loss_history = pd.DataFrame(loss_history)
loss_history.to_csv('./loss_history.csv')
