# Name matching using automatically discovered rules

## Installing the dependencies

In order to install Python dependencies, run:
```
pip3 install -r requirements.txt
```

The code also requires the HFST command-line tools. See the
[project page](http://hfst.github.io/) for installation instructions.

## Running

Once you have HFST and Morle installed, you can run:
```
make all
```

## Results

The result is two files in the directory `data/processed/maciejjan/`:
* `names_to_vd17.fsm` is a transducer that maps name variants to the names occurring in VD17. It can be used with HFST command-line tools (e.g. `hfst-lookup`).
* `vd17_to_vd17.txt` is the result of lookup of all names occurring in VD17 in the above transducer - they should be mapped at least to themselves, but also to other similar variants. The first column is the input (query) string, the second the output string, and the third weight - which is always 0 for successful mappings, as the transducer is unweighted.

## Description

The code in this folder attempts to match similar names with slight
spelling differences using the following approach:
* find string-similar names (e.g. `Schertzer ~ Scherzer`),
* extract the difference between them in form of a rule (e.g. `:/:t/:`, i.e. insert a `t` somewhere inside the word),
* group the pairs of similar names by rule and find frequent rules.

Further, the code constructs an FST (Finite State Transducer) that maps
the names to their other spelling variants. It is possible to manually
edit the list of rules before compiling the transducer.

The code here uses parts of [morle](https://github.com/maciejjan/morle),
and is based on material from Maciej Janicki's PhD thesis (see references below).
Originally the approach was meant for unsupervised learning of morphology, so
it might not be entirely designed for the task here, but it's worth
trying. It is also necessary to note that the `morle` codebase was a
testbed for experiments under constant development and never became a
ready-to-use tool. The current experiment uses only a small part of it.

## References

* Maciej Janicki. [*Statistical and computational methods for Whole Word Morphology*](http://macjanicki.eu/assets/publications/phd-janicki.pdf). PhD thesis, University of Leipzig, 2019. (esp. chap. 3: *Morphology as a system of string transformations*, which describes what the command `morle preprocess` does).
* Maciej Janicki. [Finite State Transducer Calculus for Whole Word Morphology.](https://aclanthology.org/W19-3107/) In: *Proceedings of the 14th International Conference on Finite-State Methods and Natural Language Processing (FSMNLP)*, Dresden, Germany, September 2019.
