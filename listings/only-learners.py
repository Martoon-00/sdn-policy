On Propose(policy) [fast round, by acceptor]:
    decision = this.cstruct.addUnstable(policy)
    for l in learners:
        send Learn(decision) to l
...
