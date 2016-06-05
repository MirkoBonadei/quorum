# quorum
Quorum aims to ensure consensus among a group of erlang processes using the raft consensus algorithm.

This is a side project to let me increase my knowledge of raft, therefore it is not production ready but my goal in the next few months is to complete it proving its correctness.

[Here](https://raft.github.io/raft.pdf) you can find the pdf of the original raft paper.

There are also other implementations of raft in erlang (since they are older than quorum you should give them a look):
- rafter by Andrew Stone ([link](https://github.com/andrewjstone/rafter))
- zraft_lib by Gunin Alexander ([link](https://github.com/dreyk/zraft_lib))
- eraft by Uwe Dauernheim ([link](https://github.com/djui/eraft))
- huckleberry by Jakob Sievers ([link](https://github.com/cannedprimates/huckleberry))
- rafterl by Eric Moritz ([link](https://github.com/ericmoritz/rafterl))

## Backward compatibility

quorum is developed on Erlang/OTP 19 and make use of the gen_statem behaviour so it can be used only with Erlang/OTP versions greater than or equal to 19.

## License

Distributed under the Apache License 2.0, the same of Erlang/OTP (after OTP 18).
