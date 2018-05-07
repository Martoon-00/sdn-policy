On Propose(policy) [fast round, by acceptor]:
    decision = this.cstruct.addUnstable(policy)
    for l in targetLearners(policy):
        send Learn(decision) to l
...
On Learn(decision) [fast round, by leader]:
    for l in learners - targetLearners(decision):
        send Learn(decision) to l
