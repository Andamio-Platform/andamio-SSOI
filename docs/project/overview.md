# Overview of Andamio Self-Sovereign On-Chain Identity (SSOI)

## Abstract

The Andamio Self-Sovereign On-Chain Identity (SSOI) represents a cutting-edge, decentralized identity management framework. Built on the Cardano blockchain, this solution bridges the identity and blockchain layers, enhancing existing self-sovereign identity (SSID) standards. By introducing an open-source, inclusive identity model, Andamio SSOI addresses challenges in blockchain-integrated identity management and empowers developers to implement self-sovereign identity solutions tailored to diverse use cases.

## The Need for Andamio SSOI

Existing SSID solutions face significant limitations, including centralized control, interoperability barriers, and privacy concerns that hinder autonomy over personal identity data. Andamio SSOI seeks to address these challenges by:

- **Enhancing Decentralization and Control**: Giving users autonomy over their data and identity verification processes, free from third-party dependency.
- **Facilitating Secure Blockchain Integration**: Enabling seamless integration of identity solutions with Cardano, leveraging cryptographic proofs and robust on-chain data structures.
- **Generalizing Andamio’s Access Token**: Providing an open-source version of the proprietary access token, retaining its core functionality while removing exclusivity.

## Achieving the Goals of Andamio SSOI

The Andamio SSOI framework achieves its objectives through:

- **Utilization of BuiltinData**: Leveraging Cardano’s BuiltinData for efficient and secure data anchoring directly on-chain.
- **Role-Based Access Control (RBAC)**: Implementing permission layers for granular identity data management and access control.

### Stakeholders and Use Cases

The Andamio SSOI system caters to diverse stakeholders:

- **dApp Developers**: Streamlining decentralized user onboarding and authentication.
- **Financial Institutions**: Supporting compliant, secure self-sovereign identities for transactions.
- **Education and Healthcare**: Enabling secure credential verification and sensitive data handling.
- **Government and Regulatory Bodies**: Facilitating decentralized identity verification for public services.
- **Individual Users**: Empowering users to control their identities with cryptographic proof of data integrity.

## Specification

### Schema

Andamio SSOI utilizes a standardized on-chain data structure for DIDs, ensuring robust identity verification. Detailed schema representations and diagrams are available on the dedicated Miro board:

[***SSOI Schema Miro Board***](https://miro.com/app/board/uXjVLDsFWko=/)

### SSOI Properties

The following table outlines key properties of the Andamio SSOI:

| Property            | Required? | Value Constraints                                                                                       |
|---------------------|-----------|---------------------------------------------------------------------------------------------------------|
| `id`                | Yes       | A `CurrencySymbol` to retrieve the `TokenName` of the SSOI ID, adhering to Andamio DID syntax.         |
| `alsoKnownAs`          | Yes        | A `BuiltinString` identifying the user of the DID.                                                     |
| `controller`        | No        | An `Address` linked to the owner of the ID token.                                                      |
| `verificationMethod`| Yes       | A `CurrencySymbol` pointing to the script containing `Verification Method` data.                       |
| `AdditionalData`    | Yes       | A `BuiltinData` field holding additional credential data.                                              |
