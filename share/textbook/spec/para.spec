@c -*-textbook-*-

@c ---------------------------------------------------------------------------

@tag{definitions}{}                           {#TEXT | ANY}
@tag{list}       {}                           {#TEXT | ANY}
@tag{enumerate}  {}                           {#TEXT | ANY}
@tag{itemize}    {}                           {#TEXT | ANY}

@tag{item}       {%text?}                     {EMPTY}{P}
@tag{itemx}      {%text?}                     {EMPTY}

@tag{example}    {%title?}                    {#TEXT | ANY}
@tag{verbatim}   {}                           {#TEXT}
@tag{display}    {}                           {#TEXT | ANY}

@tag{admon}      {%title}                     {#TEXT | ANY}

@tag{quote}      {}                           {#TEXT | ANY}

@tag{p}          {}                           {#TEXT | ANY}

@c ---------------------------------------------------------------------------
@c Definitions:

@c define function
@c declln = used as headline
@c declsum = used as the title of references
@tag{deffn}      {%declln, %declsum?, ID?}    {#TEXT | ANY}
@c kind = function, special, class, type, etc.
@c declsum = declaration summary, used as headline
@tag{def}        {%kind, %declsum, ID?}       {#TEXT | ANY}

@c pre-formatted declaration part, takes %inline
@tag{decl}       {%index?, ID?}               {#TEXT | ANY}

@c mark optional parts in declarations ([])
@tag{?}          {%optionalexpr}              {EMPTY}


@c ---------------------------------------------------------------------------

@c defines a figure; depending on stylesheet inline, out-of-line, %title gives
@c the figure description.  ID can be used to ref to it.  To put the image
@c inside use an embedded @tag{@@img}.  %position hints where the renderer
@c should put the figure: top, inline
@tag{figure}     {%desc, %position?, ID?}        {ANY}

  @tag{img}      {FREF}                       {EMPTY}

