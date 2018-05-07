On Propose(policy) [fast round, by acceptor]:
    decision <- local.config.addUnstable(policy)

    for l in learners:
        send Learn(decision) to l

When configuration (config) gets committed by
quorum of acceptors [classic round, by leader]:
    local.config.extendCore(config)
