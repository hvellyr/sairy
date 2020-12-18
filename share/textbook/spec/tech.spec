@c -*-textbook-*-

@c ---------------------------------------------------------------------------
@c technical tags
@tag{acr}        {%text}                      {EMPTY}   @c acronym
@tag{app}        {%text}                      {EMPTY}   @c application
@tag{dfn}        {%text}                      {EMPTY}   @c term
@tag{file}       {%text}                      {EMPTY}   @c file name
@tag{opt}        {%text}                      {EMPTY}   @c option
@tag{val}        {%text}                      {EMPTY}   @c value
@tag{var}        {%text}                      {EMPTY}   @c variable
@tag{prm}        {%text}                      {EMPTY}   @c parameter
@tag{code}       {%text}                      {EMPTY}   @c code
@tag{op}         {%text}                      {EMPTY}   @c operator

@tag{ty}         {%text}                      {EMPTY}   @c type
@tag{class}      {%text}                      {EMPTY}   @c class
@tag{fun}        {%text}                      {EMPTY}   @c fun
@tag{keyw}       {%text}                      {EMPTY}   @c keyword
@tag{const}      {%text}                      {EMPTY}   @c constant
@tag{tag}        {%text}                      {EMPTY}   @c tag in a markup lang


@tag{result}     {}                           {EMPTY}
@tag{print}      {}                           {EMPTY}
@tag{expansion}  {}                           {EMPTY}
@tag{equiv}      {}                           {EMPTY}
@tag{error}      {}                           {EMPTY}


@c ---------------------------------------------------------------------------
@c grammar
@tag{grammar}    {}                           {#TEXT | ANY}
  @tag{productions}{}                           {#TEXT | ANY}

  @tag{rule}       {%metaid, %deflist}          {EMPTY}
  @c a | b | c
  @c op:
  @c  ?    -> [a | b | c]
  @c  *    -> {a | b | c}
  @c  1    -> (a | b | c)
  @c ' '   ->  a | b | c
  @tag{choice}     {%elements, op?}             {EMPTY}
  @c a, b, c
  @c op:
  @c  ?    -> [a , b , c]
  @c  *    -> {a , b , c}
  @c  1    -> (a , b , c)
  @c ' '   ->  a , b , c
  @tag{seq}        {%elements, op?}             {EMPTY}

  @c syntax extension
  @tag{synext}     {%text, op?}                 {EMPTY}

  @c non terminal
  @tag{nt}         {%text, op?}                 {EMPTY}

  @c terminal symbol (in grammar)
  @tag{term}       {%text, op?}                 {EMPTY}
