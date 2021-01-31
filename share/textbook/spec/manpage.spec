@c -*-textbook-*-
@textbook{1.0}

@c EXAMPLE:
@c
@c @manpage{ls}{list directory contents}{7}
@c @info
@c @author{FreeBSD}
@c @copyright{Copyright (c) ??? xyz}
@c @end info
@c
@c @synopsis
@c @app{ls} [@op{-ABCFGHLOPRSTUW@abcdefghiklmnopqrstuwx1}] [@val{file} ...]
@c @end synopsis
@c
@c @section{Description}
@c ...
@c @end section
@c
@c @seealso
@c @mref{chflags}{1}
@c @mref{chmod}{1}
@c @mref{sort}{1}
@c @end seealso
@c
@c @end manpage

@tag{manpage}    {name, desc, section?}{modules?}       {#TEXT | ANY}

  @include{info.spec}

  @include{para.spec}
  @include{ref.spec}
  @include{common-inline.spec}
  @include{tech.spec}

  @c give the commands or functions synopsis
  @tag{synopsis}   {ID?}                        {#TEXT | ANY}

  @c a specific see also section for manpages
  @tag{seealso}    {}                           {ANY}

  @tag{section}    {%title, ID?}                {#TEXT | ANY}
