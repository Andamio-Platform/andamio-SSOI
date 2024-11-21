# Technical Brief: Hydra and Concurrency Challenges in Andamio SSOI

## Introduction

In the development of the Andamio Self-Sovereign On-chain Identity (SSOI) system, we are addressing the challenge of ensuring unique identification during the minting of new SSOI IDs. This is being accomplished through a linked list structure in the smart contract, which helps maintain the integrity and verification of uniqueness for each new ID.

However, as the system grows, concurrency challenges arise, particularly during the early stages when the linked list is relatively small. This document explores the potential use of **Hydra** to address these concurrency issues, while also highlighting some of the limitations and challenges that still need to be solved in the context of SSOI ID minting.

## Minting SSOI ID: Linked List and Concurrency

The minting process of SSOI IDs involves creating a linked list of IDs in the smart contract. Each newly minted ID is added to the list, with verification checks to ensure that no two identical IDs are created. This approach ensures that the system remains trustworthy and tamper-resistant, a core principle of Self-Sovereign Identity.

However, as more SSOI IDs are minted over time, the process of updating and verifying the linked list could encounter **concurrency challenges**, especially during the early stages when there are fewer participants. The early minting process may see competing requests for the creation of IDs, which could result in inefficiencies, race conditions, or delays in processing.

### Impact of Concurrency in Early Stages

At the early stage of Andamio SSOI's adoption, the number of users will be relatively low, but the linked list will still be small, increasing the likelihood of concurrency issues. These issues can result in:

- Delays in minting
- Possible conflicts or errors in minting when multiple requests are processed simultaneously

As the system matures and the linked list grows, these concurrency concerns become less critical. The larger the linked list, the less frequent the chances of simultaneous minting requests for the same ID. This reduces the need for specialized mechanisms to resolve concurrency issues.

## Hydra: A Potential Solution

Hydra, the layer 2 scaling solution for Cardano, offers the potential to solve some of these concurrency issues during the early stage of the Andamio SSOI system. Hydra provides fast and scalable off-chain channels that can process multiple transactions in parallel, alleviating pressure on the Cardano mainnet.

Hydra is particularly useful for scenarios where multiple transactions are happening at once, as it allows for the execution of many transactions off-chain while maintaining security and finality on-chain. In the case of SSOI ID minting, Hydra can help by allowing multiple participants to interact with the linked list smart contract concurrently, without waiting for each transaction to be processed sequentially on the main chain.

### Challenges with Hydra

Despite its potential, Hydra also comes with limitations and challenges that must be considered:

1. **Hydra Head Setup and ADA Locking**:
   - For Hydra to be used in minting SSOI IDs, a **Hydra head** must be opened, and a certain amount of **ADA** must be locked for the duration of the Hydra head’s usage.
   - This locked ADA incurs **network fees** for the locking and unlocking operations, adding costs to the minting process, which could be a barrier for users during the early stages when there may be fewer transactions to justify the costs.

2. **Limit on Number of Participants**:
   - A Hydra head has a limit of **25 participants** due to data storage constraints as datum on-chain. This limit could restrict the scalability of the Hydra head for larger user bases in the future.
   - As the Andamio SSOI system grows, there could be more than 25 participants requesting to mint new SSOI IDs, which would mean multiple Hydra heads or more complex coordination of resources.

3. **Uncertainty in Hydra Head Usage**:
   - The biggest challenge in using Hydra for minting is the **uncertainty** in predicting when a minting event will occur. Before a new SSOI ID can be minted, we must know that there is a request to do so, triggering the opening of a Hydra head.
   - Since we cannot predict with certainty when a minting request will happen, there may be delays in the opening of a Hydra head. This causes a **cumbersome process** for users, as they might have to wait for the head to open, increasing latency and adding complexity to the minting process.

## Conclusion

The Andamio SSOI system may faces concurrency challenges in the early stages of minting new IDs. Hydra offers a potential solution to alleviate these challenges by enabling faster, parallelized transactions off-chain. However, limitations related to Hydra head setup, ADA locking, participant limits, and uncertain timing for Hydra head usage pose challenges that need to be addressed.

As the Andamio SSOI ecosystem matures and Cardano’s infrastructure evolves, Hydra’s role in solving these concurrency issues will likely become more significant, providing a scalable and efficient method for minting SSOI IDs even during the system's early stages.
