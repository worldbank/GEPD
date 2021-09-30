library(DiagrammeR)
library(tidyverse)

# students in a teachers class
p1 <- grViz("
digraph dot {

graph [layout = dot]

node [shape = circle,
      style = filled,
      color = grey,
      label = '']

node [fillcolor = green,
      label = 'Schools']
s11 

node [fillcolor = yellow,
      label = 'Teachers']
t11 

node [fillcolor = orange,
      label = 'Students']

edge [color = grey]
s11 -> t11
t11 -> {e f g h i j}
}")

p1

# Schools under district
p2 <- grViz("
digraph dot {

graph [layout = circo]

node [shape = circle,
      style = filled,
      color = grey,
      label = '']

node [fillcolor = red,
      label = 'District Office']
a

node [fillcolor = green,
      label = 'Schools']
b c d

node [fillcolor = orange,
      label = 'Students']

edge [color = grey]
a -> {b c d}
b -> {e f g h i j}
c -> {k l m n o p}
d -> {q r s t u v}
}")

p2
#overall diagram for the GEPD presentation
p3 <- grViz("
digraph dot {

graph [layout = circo]

node [shape = circle,
      style = filled,
      color = grey,
      label = '',
      fontsize = 20]

node [fillcolor = blue,
      label = 'Central Office',
      fontcolor = white]
prime

node [fillcolor = red,
      label = 'District Office'
      fontcolor = black]
dist1 dist2 dist3

node [fillcolor = green,
      label = 'Schools']
s11 s12 s13 s21 s22 s23 s31 s32 s33

node [fillcolor = orange,
      label = 'Students']

edge [color = grey]
prime -> {dist1 dist2 dist3}
dist1 -> {s11 s12 s13}
s11 -> {e f g h i j}
s12 -> {k l m n o p}
s13 -> {q r s t u v}
dist2 -> {s21 s22 s23}
s21 -> {1 2 3 4 5 6}
s22 -> {7 8 9 10 11 12}
s23 -> {13 14 15 16 17 18}
dist3 -> {s31 s32 s33}
s31 -> {19 20 21 22 23 24}
s32 -> {25 26 27 28 29 30}
s33 -> {31 32 33 34 35 36}
}")

p3