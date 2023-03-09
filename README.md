# why3-tools

Tools to interact with why3 sessions.

The [Why3](https://why3.lri.fr/) verifier stores proofs in _session files_ alongside the verified code. 
These sessions contain the tree of transformations and proof attempts applied to solve the proof obligations in your code. 
However, creating and manipulating a session requires using the Why3 IDE, and is in general not possible to do from the command line. 
Furthermore, Why3 provides few tools to interact with and extract data from these sessions. 

This project provides an executable `why3-tools` which attempts to remedy some of these issuess.

For the moment it provides two commands:

- `why3-tools csv-export` which outputs information about every successful proof node in a session in CSV format.
- `why3-tools regenerate` which regenerates a proof session using a provided _strategy_. This will erase the whole session and attempt to solve it from scratch. It will not try to preserve valid subtrees of the proof session. 
