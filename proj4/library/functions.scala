def functionCompose[T,U,V](f: U => V, g: T => U): T => V = (x: T) => f(g(x));
