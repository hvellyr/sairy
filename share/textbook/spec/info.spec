@c -*-textbook-*-

@c the info section encodes metadata on the current document
@tag{info}         {}                         {ANY}
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
