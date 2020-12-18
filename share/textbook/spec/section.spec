@c -*-textbook-*-

@c ---------------------------------------------------------------------------

@tag{tome}       {%title, ID?, show-number?}  {#TEXT | ANY}
@tag{part}       {%title, ID?, show-number?}  {#TEXT | ANY}
@tag{chapter}    {%title, ID?, show-number?}  {#TEXT | ANY}
@tag{appendix}   {%title, ID?, show-number?}  {#TEXT | ANY}
@tag{preface}    {%title, ID?, show-number?}  {#TEXT | ANY}
@tag{introduction} {%title, ID?, show-number?} {#TEXT | ANY}
@tag{section}    {%title, ID?, show-number?}  {#TEXT | ANY}
@tag{subsection} {%title, ID?, show-number?}  {#TEXT | ANY}
@tag{subsubsection} {%title, ID?, show-number?} {#TEXT | ANY}
@tag{paragraph}  {%title, ID?, show-number?}  {#TEXT | ANY}

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

