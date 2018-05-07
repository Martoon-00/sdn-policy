On Propose(policy) [fast round, by acceptor]:
    local.nextProposal <- policy

    for l in learners:
        send Learn(local.config) to l

When configuration (config) gets committed by
quorum of acceptors [classic round, by leader]:
    local.config.extend(config)
