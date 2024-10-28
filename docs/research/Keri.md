# **Keri**

**KERI** (Key Event Receipt Infrastructure) is a decentralized, blockchain-agnostic protocol designed for managing self-sovereign identity (SSI) through **key management and rotation**. It provides a secure and scalable framework for creating and controlling cryptographic identifiers (like DIDs) using key pairs, which allows users or entities to manage their digital identities independently of any central authority.

## Key Features of KERI

1. **Key Event Logs (KELs)**:
    KERI's core concept revolves around **key events**, which are recorded in an immutable **Key Event Log (KEL)**. Each key event documents an action related to the identity, such as the creation of the identifier, a key rotation, or a delegation event. These logs are append-only and cryptographically signed by the relevant private keys to ensure their integrity and authenticity.

2. **Key Rotation**:
    One of the most crucial aspects of KERI is its **support for key rotation**. It allows users to change their cryptographic keys over time without changing the identifier (DID). This is vital for security because it enables users to rotate their private keys regularly, or if the key is compromised, without losing control of the identity.

3. **Decentralized Trust Model**:
    KERI is **blockchain-agnostic** and can be implemented without relying on any specific blockchain. Instead of requiring consensus from a global ledger, KERI uses a model of **witnessing** where trusted parties (called **witnesses**) attest to key events, ensuring the integrity of identity operations without the need for heavy blockchain reliance.

4. **Self-Certifying Identifiers**:
    The identifier in KERI is **self-certifying**, meaning that it is derived from the cryptographic public key itself, ensuring that whoever holds the corresponding private key controls the identity. This provides a high level of security and ensures that the identity cannot be forged or impersonated.

5. **Receipts and Witnesses**:
    **Witnesses** play a key role in KERI’s decentralized architecture. Witnesses are responsible for recording and attesting to key events by providing **receipts**, which are signed proofs that key events occurred as recorded. These receipts add a layer of verifiability and trust without relying on a single central authority.

6. **Interoperability**:
    KERI can work alongside other DID and identity standards, such as those defined by the W3C, making it highly **interoperable** with other decentralized identity systems. This flexibility allows KERI-based identities to function across various platforms and ecosystems.

## How KERI Works (Simplified)

1. **Create a KERI Identifier**:
    When a new KERI identity is created, it generates a public/private key pair. The **public key** is used to generate a **self-certifying identifier** (like a DID). This identifier is then associated with a **key event log (KEL)**, which records key events related to that identifier.

2. **Key Events and Rotations**:
    As the user manages their identity, various key events are recorded in the KEL. The most important type of event is **key rotation**, where the user replaces the old key pair with a new one, ensuring ongoing control of the identifier.

3. **Witnesses and Receipts**:
    When key events occur, **witnesses** (which can be other users, servers, or decentralized services) sign receipts that verify the key events. These receipts are added to the KEL, providing decentralized proof that the key event occurred correctly.

4. **Recovery and Security**:
    If a user's private key is compromised or lost, they can use key rotation to update the public key associated with the identifier, mitigating the damage and ensuring ongoing control of the identity. The integrity of the KEL and the receipts from witnesses ensure that these actions can be trusted.

## Why KERI Matters

KERI addresses several important challenges in the realm of self-sovereign identity and decentralized identity management:

- **Security**: By supporting key rotation, KERI ensures that identities remain secure even if cryptographic keys are compromised.
- **Decentralization**: KERI does not rely on any centralized authority or specific blockchain for managing identities. It can work across different platforms and systems, making it highly flexible.
- **Interoperability**: KERI can be integrated with existing identity standards like W3C DIDs, making it a valuable addition to the broader decentralized identity ecosystem.
- **Scalability**: Because KERI doesn’t rely on a single, global consensus mechanism, it is scalable and can be adopted in various identity management scenarios.

## Use Cases for KERI

1. **Self-Sovereign Identity**: KERI can be used as the foundation for managing decentralized, self-sovereign identities where individuals or organizations control their identities without the need for centralized authorities.
2. **IoT Device Management**: KERI’s key rotation and decentralized architecture make it well-suited for securely managing IoT devices that need to rotate cryptographic keys and prove ownership or control over time.
3. **Enterprise Identity Management**: Large organizations can use KERI to securely manage employee or device identities, where secure key rotation and decentralized control are important.

## Conclusion

**KERI** (Key Event Receipt Infrastructure) is a key management protocol designed for decentralized identity systems, offering features like key rotation, witness-based verification, and interoperability with other DID standards. It aims to provide a scalable, secure, and decentralized solution for managing self-sovereign identities across various platforms, with strong security through cryptographic mechanisms and decentralized trust models.

## Resources

[Nuttawut Kongsuwan The Hitchhiker’s Guide to KERI. Part 1](https://medium.com/finema/the-hitchhikers-guide-to-keri-part-1-51371f655bba)

[Nuttawut Kongsuwan The Hitchhiker’s Guide to KERI. Part 2](https://medium.com/finema/the-hitchhikers-guide-to-keri-part-2-what-exactly-is-keri-e46a649ac54c)

[Nuttawut Kongsuwan The Hitchhiker’s Guide to KERI. Part 3](https://medium.com/finema/the-hitchhikers-guide-to-keri-part-3-how-do-you-use-keri-2d1724afa432)