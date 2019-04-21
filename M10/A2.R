edges = read.csv("edges.csv")

users = read.csv("users.csv")
nrow(users)
table(users$locale, users$school)
table(users$gender, users$school)
plot(g, vertex.size=5, vertex.label=NA)
table(degree(g))
table(degree(g) >= 10)
V(g)$size = degree(g)/2+2
table(degree(g))
summary(degree(g))
18/2+2=11
0/2+2=2
V(g)$color = "black"
V(g)$color[V(g)$gender == "A"] = "red"
V(g)$color[V(g)$gender == "B"] = "gray"
plot(g, vertex.label=NA)
V(g)$color = "black"
V(g)$color[V(g)$school == "A"] = "red"
V(g)$color[V(g)$school == "AB"] = "gray"
plot(g, vertex.label=NA)
V(g)$color = "black"
V(g)$color[V(g)$locale == "A"] = "red"
V(g)$color[V(g)$locale == "B"] = "gray"
plot(g, vertex.label=NA)
