@c -*-textbook-*-
@textbook{1.0}

@include{common.spec}

@c ---------------------------------------------------------------------------
@c the toplevel node
@c @tag{manpage}    {name, description, section?}{#TEXT | ANY}
@tag{book}       {}                           {ANY}

@tag{titlepage}  {}                           {ANY}
@c title image / cover
@tag{cover}      {FREF}                       {EMPTY}

@tag{title}      {%value}                     {EMPTY}
@tag{subtitle}   {%value}                     {EMPTY}


@c ---------------------------------------------------------------------------
@c paragraphs

@tag{tome}       {%title, ID?}                {#TEXT | ANY}
@tag{part}       {%title, ID?}                {#TEXT | ANY}
@tag{chapter}    {%title, ID?}                {#TEXT | ANY}
@tag{appendix}   {%title, ID?}                {#TEXT | ANY}
@tag{preface}    {%title, ID?}                {#TEXT | ANY}
@tag{introduction} {%title, ID?}              {#TEXT | ANY}
@tag{section}    {%title, ID?}                {#TEXT | ANY}
@tag{subsection} {%title, ID?}                {#TEXT | ANY}
@tag{subsubsection} {%title, ID?}             {#TEXT | ANY}
@tag{paragraph}  {%title, ID?}                {#TEXT | ANY}

@c the place where to render the table of content; put into separate
@c chapter etc. for title.
@tag{showtoc}    {}                           {EMPTY}{NOP}
@c the place where to render the index; put into separate chapter etc. for
@c title.  @prm{type} gives on which index type to filter (e.g. fun, key)
@tag{showindex}  {type?}                      {EMPTY}{NOP}

@c like chapter, but specific for contents
@tag{content}    {%title?}                     {#TEXT | ANY}

@c like chapter, but specific for indices
@tag{index}      {%title?}                     {#TEXT | ANY}

@c a separator line between two paragraphs
@tag{asterisk}   {}                           {EMPTY}{NOP}

@tag{head}       {%text}                       {EMPTY}

@c paragraph separator (empty line)
@tag{parsep}     {}                           {EMPTY}{NOP}

@c ---------------------------------------------------------------------------
@tag{poem}       {%title?, %subtitle?}        {#TEXT | ANY}
@tag{stanza}     {}                           {EMPTY}{NOP}

@c verse line
@tag{ln}         {indent?}                    {EMPTY}{P}

@tag{center}     {%text}                      {EMPTY}

@c quoting letters in books
@tag{letter}     {}                           {#TEXT | ANY}

@c separator for a letters mainparts
@tag{letterdate} {%text}                      {EMPTY}
@tag{address}    {}                           {EMPTY}{NOP}
@tag{letterbody} {}                           {EMPTY}{NOP}
@tag{signature}  {}                           {EMPTY}{NOP}
@tag{postscript} {}                           {EMPTY}{NOP}

@c ---------------------------------------------------------------------------
@tag{fn}         {%text}                      {EMPTY}

@c add text to index, but not into text
@tag{idx}        {%text, type?}               {EMPTY}
@c visual index item: put into text and index
@tag{vidx}        {%text, type?}              {EMPTY}

@c ---------------------------------------------------------------------------
@c Definitions:

@c define function
@tag{deffn}      {%declln}                    {#TEXT | ANY}
@c kind = function, special, class, type, etc.
@c declsum = declaration summary, used as headline
@tag{def}        {%kind, %declsum, ID?}       {#TEXT | ANY}

@c pre-formatted declaration part, takes %inline
@tag{decl}       {%index?, ID?}               {#TEXT | ANY}

@c mark optional parts in declarations ([])
@tag{?}          {%optionalexpr}              {EMPTY}
