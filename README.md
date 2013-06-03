# Hypergraph-based Supercompilation

Hypergraph-based supercompilation is a kind of multi-result 
supercompilation based on hypergraph transformation. It is described
in the preprint ["Supercompilation by hypergraph transformation"](http://library.keldysh.ru/preprint.asp?id=2013-26&lg=e).
This project is a set of components for building such supercompilers,
currently limited to a lazy first-order functional language.
It also contains an experimental supercompiler that can prove equivalence of
functions.

## Experimental equivalence prover

Hypergraph-based supercompilation is good at proving equivalences.
To demonstrate this the project contains an equivalence prover.
It is written in Scala, you can build it using Simple Build Tool 
(see [here](http://www.scala-sbt.org/release/docs/Getting-Started/Setup.html) 
how to install it).
You can either use the one-jar plugin or the start-script plugin to create
an executable (you can also just build the project and run the class `graphsc.app.EqProverApp` by hand).
Start-script seems to be easier, just type:

    sbt start-script

This command will build the project and create a script `./target/start` which can be used to launch
the equivalence prover. We will call it just `eqprover`:

    alias eqprover="./target/start" 
 
Alternatively you can create a jar file using sbt-onejar plugin:

    sbt one-jar

This command will create a jar with a name like this: `./target/scala-2.9.2/graphsc_2.9.2-0.1-SNAPSHOT-one-jar.jar`.
Then you will need an alias like this:

    alias eqprover="java -jar ./target/scala-2.9.2/graphsc_2.9.2-0.1-SNAPSHOT-one-jar.jar"

The equivalence prover should be provided with a file containing definitions and propositions to prove:

    eqprover --prove samples/idle

You can use the flag `-v` to make the prover more verbose (for example, 
it will then print what expressions it is merging by graph isomorphism).
Note also that some examples may need adjustment of parameters:

    eqprover --prove -a4 -v  samples/add-assoc

(`-a4` increases the arity limit to 4)

## Creating an eclipse project

If you want to create an eclipse project, use the following command:

    sbt eclipse


