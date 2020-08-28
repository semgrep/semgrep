
val adapt_tokens_pp:
  tokenizer:(Common.filename -> Parser_php.token list) ->
  orig_filename:Common.filename ->
  Parser_php.token list -> Parser_php.token list
