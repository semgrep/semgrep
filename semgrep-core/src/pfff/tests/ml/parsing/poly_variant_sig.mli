val info_of_category :
  category ->
  [> `BACKGROUND of string
  | `FOREGROUND of string
  | `SCALE of [> `LARGE | `MEDIUM | `XX_LARGE | `X_LARGE ]
  | `STRIKETHROUGH of bool
  | `STYLE of [> `ITALIC ]
  | `UNDERLINE of [> `DOUBLE | `SINGLE ]
  | `WEIGHT of [> `BOLD ] ]
    list
