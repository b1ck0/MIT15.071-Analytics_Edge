edges = read.csv("edges.csv")
users = read.csv("users.csv")

nrow(users)

str(users)
str(edges)

mean(apply(table(edges),1,sum))

table(users$locale)

table(users$locale, users$gender)

library(igraph)
g = graph.data.frame(edges, FALSE, users)

sum(degree(g)>=10)

V(g)$size = degree(g)/2+2
plot(g, vertex.label=NA)

max(V(g)$size)
min(V(g)$size)

###################### coloring based on gender
V(g)$color = "black"
V(g)$color[V(g)$gender == "A"] = "red"
V(g)$color[V(g)$gender == "B"] = "gray"

plot(g, vertex.label=NA)

###################### coloring based on school
V(g)$color = "black"
V(g)$color[V(g)$school == "A"] = "red"
V(g)$color[V(g)$school == "B"] = "gray"
V(g)$color[V(g)$school == "AB"] = "green"

plot(g, vertex.label=NA)

###################### coloring based on locale
V(g)$color = "black"
V(g)$color[V(g)$locale == "A"] = "red"
V(g)$color[V(g)$locale == "B"] = "gray"
plot(g, vertex.label=NA)