---
CIP: ???
Title: SSOI Standard CIP
Category: ???
Status: Proposed
Authors: Andamio Team
Implementors: []
Discussions:
    - https://github.com/Andamio-Platform/andamio-SSOI/blob/master/docs/project/overview.md
    - https://github.com/Andamio-Platform/andamio-SSOI/blob/master/docs/research/Atala-Prism.md
    - https://github.com/Andamio-Platform/andamio-SSOI/blob/master/docs/research/Keri.md
    - https://github.com/Andamio-Platform/andamio-SSOI/blob/master/docs/research/hydra.md
Created: ????-??-??
License: CC-BY-4.0
---
## Abstract

The Andamio Self-Sovereign On-Chain Identity (SSOI) is a decentralized identity management framework built to enable secure, self-sovereign identity solutions on the Cardano blockchain. This document outlines how Andamio SSOI advances existing SSID standards by bridging the identity and blockchain layers, creating a versatile, open-source solution adaptable for diverse use cases. The SSOI employs the W3C DID data structure standards and introduces a more inclusive identity model, solving key challenges associated with blockchain integration in identity management and empowering developers to build self-sovereign identity solutions on Cardano.

## Motivation: why is this CIP necessary?

Existing self-sovereign identity (SSID) solutions are often limited by centralized control, interoperability challenges, and constrained by privacy concerns that prevent full autonomy over personal identity data. Andamio SSOI aims to:

- Enhance Decentralization and Control: Empower users to control their own data and identity verification processes without relying on a third party.
- Enable Secure Blockchain Integration: Facilitate seamless integration of identity solutions on Cardano's blockchain, supporting cryptographic proofs and on-chain data structures.
- Advance Interoperability and Standards Compliance: Use W3C SSID standards to ensure global interoperability, while building upon Cardano’s infrastructure.
- Generalize Andamio's Closed-Source Access Token: Provide an open-source alternative of Andamio's access token, removing exclusive restrictions while still maintaining core functionalities.

### Use-cases and Stakeholders

- **dApp Developers**: Integrate SSOI for secure, decentralized user onboarding and authentication.
- **Financial Institutions**: Enable compliant, self-sovereign digital identities for secure transactions.
- **Education and Healthcare**: Support secure, verified credentials and sensitive data management.
- **Government and Regulatory Bodies**: Deploy decentralized identity verification for public services.
- **Users**: Maintain control over their identities, with cryptographic proofs of data integrity.

## Specification

### Definitions

- **SSOI (Self-Sovereign On-Chain Identity)**: A decentralized identity protocol enabling users to manage their identities autonomously on the blockchain.
- **W3C DID**: The W3C Decentralized Identifier standard providing a global standard for self-sovereign identity solutions.
- **RBAC (Role-Based Access Control)**: A model for identity access management based on user roles and permissions.
- **KERI (Key Event Receipt Infrastructure)**: A cryptographic framework for managing decentralized keys and proofs.
- **BuiltinData**: Cardano’s blockchain on-chain data type for efficiently storing and referencing data on-chain.

### On-chain SSOI

Andamio SSOI defines a standardized data structure for DIDs, anchored on-chain for identity verification. This structure:

- Conforms to W3C standards, enabling secure key management through structures like Ed25519VerificationKey2020.
- Supports RBAC mechanisms, allowing developers to define role-specific access permissions.
- Integrates KERI to secure identity lifecycle events like creation, rotation, and revocation.
- Uses BuiltinData on Cardano to enable seamless data linking and access control within smart contracts.

### Off-chain SSOI

Andamio SSOI includes an off-chain component for handling identity-related processes that do not require on-chain interactions. Key off-chain processes:

- **Data Storage and Retrieval**: Securely store sensitive information off-chain, with only necessary hashes or references stored on-chain.
- **User-controlled Permissions**: Users define and control access permissions for third-party access to their off-chain data.
- **Interoperable Identity Anchors**: Enable identity verification off-chain through trusted, standards-based identifiers aligned with W3C DID standards.

### Technical Dependencies

### SSOI Template

#### Schema

The Andamio SSOI schema will follow W3C SSID conventions while including unique Cardano-native attributes such as currency symbols to support dApp integrations.

##### SSOI properties

<table>
    <thead>
        <tr>
        <th>Property</th>
        <th >Required?</th>
        <th >Value constraints</th>
        </tr>
    </thead>
    <tbody>
        <tr>
            <td align="Left"><code>id</code></td>
            <td align="center">yes</td>
            <td align="Left">A <code>CurrencySymbol</code> that identify DID and conforms to the Andamio DID Syntax.</td>
        </tr>
        <tr>
            <td align="Left"><code>alsoKnownAs</code></td>
            <td align="center">yes</td>
            <td align="Left">A <code>BuiltinString</code> that identify user of the DID and conform to the rules of [RFC3986] for URIs.</td>
        </tr>
        <tr>
            <td align="Left"><code>controller</code></td>
            <td align="center">no</td>
            <td align="Left">An <code>Address</code> that is linked to the owner of id token.</td>
        </tr>
        <tr>
            <td align="Left"><code>verificationMethod</code></td>
            <td align="center">yes</td>
            <td align="Left">A data <code>VerificationMethod</code> that conform to the rules in Verification Method properties.</td>
        </tr>
    </tbody>
</table>


##### Verification Method properties

<table>
    <thead>
        <tr>
        <th>Property</th>
        <th >Required?</th>
        <th >Value constraints</th>
        </tr>
    </thead>
    <tbody>
        <tr>
            <td align="Left"><code>verifier</code></td>
            <td align="center">yes</td>
            <td align="Left">"A `<code>CurrencySymbol</code>` that specifies the verification ownership mechanism used to secure the DID (Decentralized Identifier)."</td>
        </tr>
        <tr>
            <td align="Left"><code>additionalData</code></td>
            <td align="center">no</td>
            <td align="Left">A <code>BuiltinData</code> structure that can hold additional information about the verification methods, such as the algorithm used or the verification time.</td>
    </tbody>
</table>

#### Technical Details

```haskell
data SSOISchema = SSOISchema
    {   id                  :: CurrencySymbol
    ,   alsoKnownAs         :: BuiltinString
    ,   controller          :: Maybe Address
    ,   verificationMethod  :: [VerificationMethod]
    }

data VerificationMethod = VerificationMethod
    {   verifier        :: CurrencySymbol
    ,   additionalData  :: Maybe BuiltinData
    }
```

## Rationale: how does this CIP achieve its goals?

Andamio SSOI introduces a self-sovereign identity solution compatible with decentralized applications (dApps) and services on Cardano, designed for robust user authentication, authorization, and data integrity. The SSOI combines:

- **BuiltinData Utilization**: Leveraging Cardano’s BuiltinData for data anchoring, which simplifies linking identity data directly on-chain.
- **Support for Role-Based Access Control (RBAC)**: Enabling permission layers for identity data access and management.
- **Integration with Key Event Receipt Infrastructure (KERI)**: Ensuring secure, scalable identity key management with cryptographic proofs.
- **Alignment with Atala PRISM**: Leveraging PRISM’s DID capabilities for Cardano, enabling flexible, standards-based identity schemas.

## Path to Active

### Acceptance Criteria

To successfully transition Andamio Self-Sovereign On-Chain Identity (SSOI) to active status, the following acceptance criteria must be met:

1. **Technical Validation**:
   - All smart contracts have been deployed and tested on the Cardano testnet.
   - Comprehensive unit and integration tests achieve a minimum of 95% test coverage.
   - Security audits by an external party confirm no critical or high-severity vulnerabilities.

2. **Functionality Requirements**:
   - Identity registration and verification processes are operational and meet defined performance benchmarks.
   - Integration with at least one dApp to demonstrate real-world usability.
   - Role-Based Access Control (RBAC) functionality is fully implemented and tested.

3. **User Experience (UX) Goals**:
   - A user-friendly interface for identity management is available and tested for accessibility compliance.
   - Clear documentation for developers and end-users, including setup guides and API references.

4. **Stakeholder Buy-In**:
   - Key stakeholders, including developers, partners, and potential adopters, provide feedback and confirm alignment with the project’s goals.

5. **Governance Framework**:
   - A transparent governance model for ongoing updates and community contributions is established and documented.

### Implementation Plan

- [] Engage with an external auditor to perform a comprehensive security review.
- [] Address all vulnerabilities identified in the audit.
- [] Ensure compliance with applicable regulations, such as GDPR for data protection.
- [] Host developer workshops to familiarize the community with SSOI’s implementation and integration.
- [] Announce the launch to the community and provide support channels for feedback and issue resolution.
- [] Regularly review and update the governance model to align with evolving needs.

## Copyright

This CIP is licensed under [CC-BY-4.0](https://creativecommons.org/licenses/by/4.0/legalcode).
