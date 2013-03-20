# Hypergraph-based Supercompilation

Hypergraph-based supercompilation is a kind of multi-result 
supercompilation based on hypergraph transformation.
This project is a set of components for building such supercompilers,
currently limited to a lazy first-order functional language.
It also contains an experimental supercompiler that can prove equivalence of
functions.

## Experimental equivalence prover

Hypergraph-based supercompilation is good at proving equivalences.
To demonstrate this the project contains an equivalence prover. 
To try it out you should first build the project and create a start script using sbt:

    sbt start-script

This command will create a script `./target/start` which runs the equivalence prover.
The equivalence prover should be provided with a file containing definitions and (optionally)
a task in the form `foo=bar`:

    ./target/start -t constz=idle samples/idle

If the first line of the file contains a comment with a task (in the same format) then you can pass 
`auto` instead:

    ./target/start -tauto samples/idle

You can use the flag `-v` to make the prover more verbose (for example, 
it will then print what expressions it is merging by graph isomorphism).
Note also that some examples may need adjustment of parameters:

    ./target/start -tauto -a4 -v  samples/add-assoc

(`-a4` increases the arity limit to 4)

