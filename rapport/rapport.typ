#let sorb_blue = rgb("#152b6a")
#let gray_bg = luma(245)
#let blk(content) = block(fill: gray_bg, width: 100%, inset: (x: 2pt, y: 8pt), radius: 4pt, content)

#show heading: set text(fill: sorb_blue)
#set text(font: "Nimbus Sans", lang: "en", size: 12pt, hyphenate: false)
#set par(justify: true, leading: 0.52em, linebreaks: "simple")
#set heading(numbering: "1.")
#set list(indent: 0.1em, body-indent: 1em)

#show image: it => {align(center, it)}
#show raw.where(block: false): it=>box(
  fill: gray_bg,
  inset: (x: 3pt, y: 0pt),
  outset: (y: 3pt),
  radius: 2pt,
  text(it)
)

#show raw.where(block: true): it=>block(
  fill: gray_bg,
  width: 100%,
  inset: (x: 10pt, y: 10pt),
  outset: (y: 3pt),
  radius: 12pt,
  text(it)
)

#page(
  margin: (x: 10pt, y: 10pt), rect(
    width: 100%, height: 100%, stroke: 10pt + sorb_blue, [
      #set align(center)
      #v(70pt)
      #image("sorbonne-university.svg", width: 35%)

      #block([
        #set text(3em, weight: "bold", fill: sorb_blue)
        #text(tracking: .7pt, [SORBONNE])
        #v(-30pt)
        UNIVERSITÉ
      ])
      #v(32pt)
      #text(19pt, [ *Rapport du projet* ])

      #text(26pt, lang: "en", fill: sorb_blue, [ *Evaluateur-Typeur de Lambda-Calcul* ])

      #v(3em)
      #text(16pt, [ Saïd Mohammad ZUHAIR (21204924) ])

      #v(5em)
      November 2024
    ],
  ),
)

#pagebreak()

#set page(
  header: [
    #box(height: 30pt, move(dy: 10pt, image("SU_small.jpg")))
    #h(1fr)
    #box(height: 30pt, move(dy: 8pt, text(12pt)[Evaluation-Typeur de Lambda-Calcul]))
    #line(stroke: sorb_blue, length: 100%)
  ], header-ascent: 25%,
  margin: (top: 2.5cm, bottom: 1.5cm, x: 2cm),
  number-align: right, 
  numbering: "1",
)

= Introduction

Ce rapport présente le projet d'évaluation et de typage de lambda-calcul.
J'ai réalisé ce projet avec le langage ocaml, un utilisant une librairie de test qui s'appelle `alcotest` et le build tool `dune`.

= Structure du projet

Le projet est structuré comme suit:
- `bin/`
  - `eval.ml`     (binaire qui prend un fichier en entrée et affiche le résultat de l'évaluation)
  - `type.ml`     (binaire qui prend un fichier en entrée et affiche le type de l'expression)
- `lib/`
  - `ast.ml`      (définition de l'arbre syntaxique abstrait)
  - `eval.ml`     (évaluateur)
  - `type.ml`     (typeur)
  - `parser.mly`  (parseur)
  - `lexer.mll`   (lexer)
- `test/`
  - `lib/`        (module utilitaire pour les tests)
  - `prog/`       (liste de programmes de tester le parseur et le lexer)
  - `eval2.ml`    (tests de la section 2)
  - `eval4.ml`    (tests d'évaluation de la section 4)
  - `eval5.ml`    (tests d'évaluation de la section 5)
  - `type3.ml`    (tests de la section 3)
  - `type4.ml`    (tests de typage de la section 4)
  - `type5.ml`    (tests de typage de la section 5)

= Évaluateur

- J'ai réussi à implémenter l'évaluateur jusqu'à la section 5.
- L'évaluateur implémente une stratégie d'évaluation *left-to-right call-by-value*.
- La mémoire est representée par une liste d'associations d'un entier avec des pterm.

= Typeur
- J'ai réussi à implémenter le typeur jusqu'à la section 5.2 (il manque la section 5.3).
- Les equations de types sont representées par une liste de (ptype \* ptype).
- J'ai notamment passé beaucoup de temps à implémenter la règle `fixe`.

= Parseur/Lexer
- J'ai implémenté un parseur et un lexer pour tous les constructeurs de l'évaluateur.
- J'ai fait le choix de construire un `fixe` que par un `let` qui est représenté par `let rec` dans le parseur.
- J'ai aussi implementé un `fun` (lambda) qui peut prendre plusieurs arguments.
- Je n'ai pas eu le temps de faire des tests propre pour le parseur et le lexer mais j'ai essayé de lancer 
  les programmes dans le dossier `test/prog/` et ils ont tous donné le résultat attendu.

#pagebreak()

= Execution
- Pour exécuter en mode evaluation, il faut executer `dune exec bin/eval.exe <fichier>`
- Pour exécuter en mode typage, il faut executer `dune exec bin/type.exe <fichier>`
- Pour exécuter les tests, il faut executer `dune test`
- Pour exécuter les tests d'une section, il faut executer `dune exec test/<fichier>.exe`
