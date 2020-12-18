@c -*-textbook-*-
@textbook{1.0}

@include{common.spec}

@tag{vortaro}    {}                           {ANY}

  @include{info.spec}
  @include{frontmatter.spec}

  @include{section.spec}
  @include{para.spec}
  @include{lit-para.spec}
  @include{ref.spec}

  @include{common-inline.spec}

  @include{tech.spec}


@tag{lxshowwords} {%lang, %trllang, %words, ID?}        {EMPTY}
@tag{lxshowlexicon} {%lang, %trllang, %letters, ID?}    {EMPTY}{NOP}
@tag{lxshowletter}  {%title, %lang, %trllang, ID?}      {EMPTY}{NOP}

@tag{lexiconcollation} {%lang, %coll}                   {EMPTY}


@c ---------------------------------------------------------------------------
@c for lexicon makers
@tag{lxentry}    {%title?, ID?}               {#TEXT | ANY}
@tag{lxwd}       {%text, ID?}                 {EMPTY}
@tag{lxuse}      {%text}                      {EMPTY}
@tag{lxexp}      {%text}                      {EMPTY}
@tag{lxphon}     {%text}                      {EMPTY}
@tag{lxgram}     {%text}                      {EMPTY}
@tag{lxgraph}    {%text}                      {EMPTY}

@tag{lexicon}    {%title, ID?}                {#TEXT | ANY}
@tag{lxitem}     {}                           {EMPTY}{P}

@tag{lxdef}      {%title?, ID?}               {#TEXT | ANY}

@tag{lxword}     {%form, %transl?}            {EMPTY}
@tag{lxtransf}   {%from, %to}                 {EMPTY}


