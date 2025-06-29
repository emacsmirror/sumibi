
## 編集プロセス

- sumibi.el を編集した後は、必ず agent-lisp-paren-aid-deno を実行して、閉じ括弧が合っているか確認してください。

もし括弧が整合していない場合は、修正すべき行番号を教えてくれます。

```
agent-lisp-paren-aid-deno lisp/sumibi.el
```

もし不整合が検出されたら他の編集作業はせず、一旦指摘された行番号に括弧を補う修正のみを行って、
再度 agent-lisp-paren-aid-deno を実行するようにしてください。
LLMはLisp括弧を数えるのが苦手なため、自分で数えず、このツールを使うようにしてください。

