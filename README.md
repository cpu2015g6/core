#2015年度CPU実験6班コア
- 担当 umedaikiti

## アーキテクチャ
プログラムとデータでメモリを分ける。アドレスは独立。
- プログラムメモリ: 32bit * 2^14
- データメモリ: SRAM

## 実装
Tomasuloのアルゴリズムによるアウトオブオーダー実行を行う。以下のように分けて説明する。
- Fetch & Decode
- Read & Issue
- Execute
- Write
- Commit

### Fetch & Decode
命令をプログラムメモリからフェッチしてデコードする。

### Read & Issue
レジスタからデータを読み込み、ROB(Re-Order Buffer)と実行ユニットのRS(Resevation Station)に書き込む。

### Execute
各実行ユニットでデータの揃った命令から実行していく。

### Write
実行の完了した命令の結果をCDB(Common Data Bus)で全体に通知する。

### Commit
ROBを利用してin-orderにレジスタに結果を書き込む。

## 実装した命令

## プログラムローダー
ロードするバイナリの形式は
- size of .text (4 byte)
- size of .data (4 byte)
- .text section
- .data section
