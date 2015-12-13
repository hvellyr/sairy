@c -*-textbook-*-

@c ---------------------------------------------------------------------------

@c the info section encodes metadata on the current document
@tag{info}       {}                           {ANY}
  @tag{lang}       {%value}                   {EMPTY}
  @tag{docid}      {%value}                   {EMPTY}

  @tag{author}     {%name, %email?}           {EMPTY}
  @tag{creator}    {%name, %email?}           {EMPTY}
  @tag{editor}     {%name, %email?}           {EMPTY}

  @tag{extent}     {%text}                    {EMPTY}
  @tag{copyright}  {%text}                    {EMPTY}

  @tag{license}    {}                         {#TEXT | ANY}

  @tag{revision}   {}                         {#TEXT | ANY}
  @tag{source}     {}                         {#TEXT | ANY}

  @tag{abstract}   {}                         {#TEXT | ANY}
  @c comma separated keywords
  @tag{subject}    {%value}                   {EMPTY}

  @tag{relation}   {%value}                   {EMPTY}
  @tag{publdate}   {%value}                   {EMPTY}
  @tag{publisher}  {%value}                   {EMPTY}

@c ---------------------------------------------------------------------------

@tag{definitions}{}                           {#TEXT | ANY}
@tag{list}       {}                           {#TEXT | ANY}
@tag{enumerate}  {}                           {#TEXT | ANY}
@tag{itemize}    {}                           {#TEXT | ANY}

@tag{item}       {%text?}                     {EMPTY}{P}
@tag{itemx}      {%text?}                     {EMPTY}

@tag{example}    {}                           {#TEXT | ANY}
@tag{verbatim}   {}                           {#TEXT}

@tag{admon}      {%title}                     {#TEXT | ANY}

@tag{quote}      {}                           {#TEXT | ANY}

@tag{p}          {}                           {#TEXT | ANY}

@c ---------------------------------------------------------------------------
@c inline tags
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

@tag{em}         {%text}                      {EMPTY}   @c emphasis
@tag{b}          {%text}                      {EMPTY}   @c bold
@tag{r}          {%text}                      {EMPTY}   @c normal font

@tag{result}     {}                           {EMPTY}
@tag{print}      {}                           {EMPTY}
@tag{expansion}  {}                           {EMPTY}
@tag{equiv}      {}                           {EMPTY}
@tag{error}      {}                           {EMPTY}
@tag{dots}       {}                           {EMPTY}


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


@c ---------------------------------------------------------------------------
@c cross reference, any other reference
@tag{xref}       {ref}                        {EMPTY}

@c reference to IDs inside the same document
@tag{ref}        {ref}                        {EMPTY}

@c reference to man page
@tag{mref}       {%name, %section}            {EMPTY}

@c if alt is not given display url
@tag{url}        {%url, %alt?}                {EMPTY}

@tag{email}      {%email}                     {EMPTY}

@c bibliographic reference
@tag{bibref}     {}                           {ANY}
@tag{title}      {%text}                      {EMPTY}
@tag{arttitle}   {%text}                      {EMPTY}
@tag{place}      {%text}                      {EMPTY}
@tag{publ}       {%text}                      {EMPTY}
@tag{date}       {%text}                      {EMPTY}
@tag{journal}    {%text}                      {EMPTY}
@tag{issueno}    {%text}                      {EMPTY}
@tag{pages}      {%text}                      {EMPTY}
@tag{volume}     {%text}                      {EMPTY}
