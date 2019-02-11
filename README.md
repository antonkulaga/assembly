# assembly
library to ease cloning and gene synthesis. 
Contains classes for:
* restriction enzymes
* GC checking
* golden gate assembly
* random sequence generation

Adding to dependencies
----------------------

add the following to you build.sbt
```scala
resolvers += sbt.Resolver.bintrayRepo("comp-bio-aging", "main")
libraryDependencies += "group.aging-research" %% "assembly" % "0.0.7"
```
