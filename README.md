# Hypergraph-based Supercompilation

Although the name of this project is "supercompilation-hypergraph", 
we currently consider it to be much closer to Equality Saturation than to Supercompilation.
And what we call here Hypergraph is (almost) the same thing as an E-graph in some other fields. 
Sorry for this terminological mess.

Hypergraph-based supercompilation is a kind of multi-result 
supercompilation based on hypergraph transformation and equality saturation. It is described
in the preprint ["Supercompilation by hypergraph transformation"](http://library.keldysh.ru/preprint.asp?id=2013-26&lg=e).
This project is a set of components for building such supercompilers,
currently limited to a lazy first-order functional language.
It also contains an experimental inductive prover that can prove equivalence of functions 
written in a first-order lazy functional language.

## Experimental equivalence prover

Hypergraph-based supercompilation is good at proving equivalences.
To demonstrate this the project contains an equivalence prover.
It is written in Scala, you can build it using Simple Build Tool 
(see [here](http://www.scala-sbt.org/release/docs/Getting-Started/Setup.html) 
how to install it).
You can use the start-script plugin to create
an executable. You can also just build the project and run the class `graphsc.app.MainApp` by hand.
Start-script seems to be easier, just type:

    sbt start-script

This command will build the project and create a script `./target/start` which can be used to launch
the equivalence prover. You can also use the script `./graphsc` which just invokes `./target/start`. 

The equivalence prover should be provided with a file containing definitions and propositions to prove:

    ./graphsc --prove samples/idle

You can use the flag `-v` to make the prover more verbose (for example, 
it will then print what expressions it is merging by graph isomorphism).
Note also that some examples may need adjustment of parameters:

    ./graphsc -p -v  samples/add-assoc

You can find some examples in the directory `./samples/`.

## Creating an eclipse project

If you want to create an eclipse project, use the following command:

    sbt eclipse

## Benchmarking

There are a bunch of scripts to run several provers on test sets. Currently we support our prover,
[HOSC](https://github.com/ilya-klyuchnikov/hosc), [MRSC](https://github.com/ilya-klyuchnikov/mrsc),
[HipSpec](https://github.com/danr/hipspec), and [Zeno](http://hackage.haskell.org/package/zeno). 
If you want to run HOSC or MRSC, you will need my
forked versions of them: [forked hosc](https://github.com/sergei-grechanik/hosc), 
[forked mrsc](https://github.com/sergei-grechanik/mrsc). Build them with the same 
`sbt start-script` command. Note that the wrapper for HOSC (`./scripts/hosc`) assumes that you've
cloned hosc into `../hosc/` (relative to the root of your local copy of supercompilation-hypergraph).

Now you need to reformat the tests from your test-set (you don't need to do it if you want to
run only graphsc and/or mrsc, other provers use different input formats). 
To do this run the following command (from here on we assume that 
your current directory is the root of your local copy):

    ./scripts/reformat.sh samples/december.set

You can use your own set of samples in place of `samples/december.set`. This command will 
(hopefully) generate reformatted samples in `./reformatted/` (this is assumed by the wrapper scripts!).
Now you can run the test using something like this:

    ./scripts/runset.sh -s samples/december.set -t60 -o "reports/december-$(date +%h-%d-%H.%M)" -k -r3 -- \
        "./scripts/hipspec-total" \
        "./scripts/hipspec-partial" \
        "./scripts/hipspec-total --pvars" \
        "./scripts/zeno-total" \
        "./scripts/zeno-partial" \
        "./graphsc -p -v --stat"  \
        "./scripts/hosc hosc15" \
        "./scripts/hosc fasthlsc" \
        "../mrsc/mrsc-cli --sc sc3" \
        "../mrsc/mrsc-cli --sc sc4"
        
This command will run the enumerated tools with the specified parameters on the december test set 
(`-s samples/december.set`), giving each tool
60 seconds (`-t60`), keeping temporary files (`-k`, may be useful for debugging failed tests),
repeating each combination of a test and a command 3 times (`-r3`), and putting the report, logs
and stuff in `reports/december-blah-blah`. The difference between `hipspec-total` and `hipspec-partial` is
that `hipspec-total` runs hipspec on normal test, and `hipspec-partial` runs it on distorted tests to
emulate partiality. The same applies to zeno.

Now there will be a report in 
`reports/december-blah-blah/report.xml`. To convert it into a human-readable table, use this command:

    ./scripts/mktable.py reports/december-blah-blah/report.xml

By default it will output average user time for each unfailed combination. You can use other 
fields as well:

    ./scripts/mktable.py -f elapsed reports/december-blah-blah/report.xml

