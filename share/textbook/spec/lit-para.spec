@c -*-textbook-*-

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




@c Tags for Performance Texts

@c @c structure

@c @dr:act          {%title}                     {#TEXT | ANY}
@c @dr:scene        {%title}                     {#TEXT | ANY}
@c @epilogue
@c @prologue

@c @dr:actor        {%text}
@c @dr:role         {%title, desc?}
@c @dr:cast         {}
@c @dr:castgroup    {%title}

@c @dr:set          {%title}                     {#TEXT | ANY}

@c @dr:view         {%title}                     {#TEXT | ANY}

@c @c describes a screen caption in a @dr:view
@c @dr:caption      {%title}

@c @dr:sp           {}                           {#TEXT | ANY}
@c @dr:speaker      {%text}{refid}

@c @c describes the stage setting
@c @dr:stage

@c <actor>   Nom d'un acteur apparaissant dans une distribution.
@c <castItem> (personnage) dans une liste de personnages, entrée décrivant un rôle en particulier ou une liste de rôles muets.
@c <castGroup> (liste de personnages ) dans une distribution, catégorie qui rassemble un ou plusieurs personnages.
@c <castList> (distribution ) contient la distribution ou la liste des personnages.

@c <castItem>
@c  <role>Mrs Saunders</role>
@c  <roleDesc>la logeuse</roleDesc>
@c  <actor>Sylvia Maryott</actor>
@c </castItem>

@c <castGroup>
@c  <head>Messagers</head>
@c  <castItem>
@c   <actor>Jean-Claude Islert</actor>, <actor>Michel Sausin</actor>.</castItem>
@c </castGroup>
@c <castGroup>
@c  <head>Servantes troyennes</head>
@c  <castItem>
@c   <actor>Dominique Jayr</actor>, <actor>Annie Seurat</actor>,<actor> Hélène
@c      Augier</actor>
@c  </castItem>
@c </castGroup>bibliography

@c <role> (rôle) le nom d'un rôle au théâtre tel qu’il est donné dans la distribution.
@c <roleDesc> (description du rôle ) décrit le rôle d'un personnage dans une pièce de théâtre.
@c <set> (contexte) contient une description du décor, du temps, du lieu, de l'ordre d'entrée en scène, etc., de l'action de la pièce, qu'on trouve généralement dans les préliminaires d’un texte imprimé d'une pièce de théâtre. (Ce n'est pas une indication scénique).

@c <set>
@c  <head>Une représentation à l'hôtel de Bourgogne</head>
@c  <p>La salle de l'Hôtel de Bourgogne, en 1640. Sorte de hangar de jeu de paume aménagé et
@c    embelli pour des représentations.</p>
@c  <p>La salle est un carré long ; on la voit en biais, de sorte qu'un de ses côtés forme le
@c    fond qui part du premier plan, à droite, et va au dernier plan, à gauche, faire angle
@c    avec la scène qu'on aperçoit en pan coupé.</p>
@c </set>bibliography

@c <view> (vue) décrit le contexte visuel d'une partie d'un scénario selon la vision du spectateur, généralement indépendamment de tout dialogue.
@c <sound> (son) décrit un effet sonore ou un morceau de musique indiqué dans un scénario pour le cinéma ou la radio.
@c <camera> (angle de prise de vue) décrit un angle de prise de vue ou un plan dans un scénario.

@c <view>
@c  <camera type="plan">Plan américain (très légère plongée)</camera> de la plate-forme
@c  bondée. <camera type="mouvement">Pano</camera> sur Klein qui se glisse dans le couloir
@c  intérieur du véhicule, jusqu'à une jeune femme qui se tient debout. 
@c </view>bibliography


@c <caption> (sous-titre) texte d'une légende ou tout autre texte affiché, qui fait partie du script ou du scénario

@c <view>
@c  <camera>Plongée générale de la cage d'escalier (en colimaçon)</camera>. Deux lycéens,
@c  Eric et Serge, âgés de 16 ans, en manteaux noirs, montent. <camera>En surimpression sur
@c    cette image, passent successivement trois cartons de générique</camera>
@c  <note place="foot"> Le générique complet se déroule à la fin du film</note>. <sound>Reprise de
@c    la musique leitmotiv. Coup de sonnette.</sound>
@c  <caption> 1er carton : UN FILS UNIQUE</caption>
@c  <caption>2e carton : UN FILM de MICHEL POLAC</caption>
@c  <caption> 3e carton : IMAGES ERIC DALMAT</caption>
@c </view>bibliography

@c <epilogue> (épilogue) contient l'épilogue d’une pièce de théâtre, généralement récité par un acteur qui n'a pas de rôle, éventuellement associé à une représentation ou un lieu particulier
@c <prologue> (prologue) contient le prologue d’une pièce de théâtre, généralement récité par un acteur hors rôle, éventuellement associé à une représentation ou à un lieu particulier.

@c <epilogue>
@c  <head>Stances en forme d'épilogue. </head>
@c  <sp>
@c   <speaker>Alidor.</speaker>
@c   <lg type="stances">
@c    <l>Que par cette retraite elle me favorise !</l>
@c    <l>Alors que mes desseins cedent à mes amours,</l>
@c    <l> Et qu'ils ne sçauroient plus defendre ma franchise</l>
@c    <l> Sa haine, et ses refus viennent à leur secours.</l>
@c    <l> J'avois beau la trahir, une secrette amorce</l>
@c    <l> R'allumoit dans mon coeur l'amour par la pitié,</l>
@c    <l> Mes feux en recevoient une nouvelle force,</l>
@c    <l> Et tousjours leur ardeur en croissoit de moitié.</l>
@c   </lg>
@c  </sp>
@c </epilogue>bibliography

@c <move> (mouvement) signale l'entrée ou la sortie d'un ou de plusieurs personnages sur la scène.

@c <performance xml:id="fr_perf1">
@c  <p>Première apparition</p>
@c  <castList>
@c   <castItem>
@c    <role xml:id="fr_clar">Clarence</role>
@c   </castItem>
@c  </castList>
@c </performance>
@c <stage type="entrance">
@c  <move who="#fr_clar" type="enter" perf="#perf1"/>(Entre Clarence,
@c  entouré de gardes).
@c </stage>

@c <performance> (représentation) contient une partie de la préface ou de la postface décrivant comment la pièce de théâtre doit être jouée normalement ou comment elle a été jouée à telle ou telle occasion particulière.

@c <stage type="setting">La scène est dans une place de ville.</stage>

@c <speaker> forme particulière de titre ou de marque qui donne le nom d'un ou de plusieurs locuteurs dans un texte ou dans un fragment de texte écrit pour le théâtre.

@c <sp> (langue orale) monologue dans un texte écrit pour la scène ou un passage présenté sous cette forme dans un texte en prose ou en vers.
@c <spGrp> (speech group) contains a group of speeches or songs in a performance text presented in a source as constituting a single unit or ‘number’.

@c @tag{scene}

@c @tag{sp}
@c @tag{speaker}


@c @c (indication technique de mise en scène )
@c @tag{tech}
