# Andamio SSOI Overview

## Abstract

The Andamio Self-Sovereign On-Chain Identity (SSOI) is a decentralized identity management framework built to enable secure, self-sovereign identity solutions on the Cardano blockchain. This document outlines how Andamio SSOI advances existing SSID standards by bridging the identity and blockchain layers, creating a versatile, open-source solution adaptable for diverse use cases. The SSOI employs data structure standards that introduces a more inclusive identity model, solving key challenges associated with blockchain integration in identity management and empowering developers to build self-sovereign identity solutions on Cardano.

## Motivation: why is this SSOI necessary?

Existing self-sovereign identity (SSID) solutions are often limited by centralized control, interoperability challenges, and constrained by privacy concerns that prevent full autonomy over personal identity data. Andamio SSOI aims to:

- **Enhance Decentralization and Control**: Empower users to control their own data and identity verification processes without relying on a third party.
- **Enable Secure Blockchain Integration**: Facilitate seamless integration of identity solutions on Cardano's blockchain, supporting cryptographic proofs and on-chain data structures.
- **Generalize Andamio's Closed-Source Access Token**: Provide an open-source alternative of Andamio's access token, removing exclusive restrictions while still maintaining core functionalities.

## Rationale: how does this SSOI achieve its goals?

Andamio SSOI introduces a self-sovereign identity solution compatible with decentralized applications (dApps) and services on Cardano, designed for robust user authentication, authorization, and data integrity. The SSOI combines:

- **BuiltinData Utilization**: Leveraging Cardano’s BuiltinData for data anchoring, which simplifies linking identity data directly on-chain.
- **Support for Role-Based Access Control (RBAC)**: Enabling permission layers for identity data access and management.

### Use-cases and Stakeholders

- **dApp Developers**: Integrate SSOI for secure, decentralized user onboarding and authentication.
- **Financial Institutions**: Enable compliant, self-sovereign digital identities for secure transactions.
- **Education and Healthcare**: Support secure, verified credentials and sensitive data management.
- **Government and Regulatory Bodies**: Deploy decentralized identity verification for public services.
- **Users**: Maintain control over their identities, with cryptographic proofs of data integrity.

## Specification

### Definitions

- **SSOI (Self-Sovereign On-Chain Identity)**: A decentralized identity protocol enabling users to manage their identities autonomously on the blockchain.
- **CurrencySymbol**: Represents a unique identifier for a currency or token on the Cardano blockchain.
- **BuiltinString**: Represents textual data directly understood by the Cardano smart contract platform (Plutus).
- **Address**: Represents a Cardano address used to store assets, associate with DIDs, or verify ownership.
- **BuiltinData**: Cardano’s blockchain on-chain data type for efficiently storing and referencing data on-chain.


### Schema

Andamio SSOI defines a standardized on-chain data structure for DIDs, anchored on-chain for identity verification. For a detailed visual representation of the SSOI schema, including various figures and diagrams, please refer to the following Miro board:

[***SSOI Schema Miro Board***](https://miro.com/app/board/uXjVLDsFWko=/) 

This Miro board provides an in-depth look at the schema and various components, helping to visualize how the SSOI system is structured and interacts with other components within the Andamio ecosystem.


### SSOI properties

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
            <td align="Left">A <code>CurrencySymbol</code> to retrieve the <code>TokenName</code> of the SSOI ID, which is used to identify the DID and conforms to the Andamio DID Syntax.</td>
        </tr>
        <tr>
            <td align="Left"><code>username</code></td>
            <td align="center">no</td>
            <td align="Left">A <code>BuiltinString</code> that identify user of the DID.</td>
        </tr>
        <tr>
            <td align="Left"><code>controller</code></td>
            <td align="center">no</td>
            <td align="Left">An <code>Address</code> that is linked to the owner of id token.</td>
        </tr>
        <tr>
            <td align="Left"><code>verificationMethod</code></td>
            <td align="center">yes</td>
            <td align="Left">A <code>CurrencySymbol</code> that points to the script which contain <code>Verification Method</code> data and that conform to the rules in Verification Method properties.</td>
        </tr>
        <tr>
            <td align="Left"><code>AdditionalData</code></td>
            <td align="center">yes</td>
            <td align="Left">A <code>BuiltinData</code> that can hole additional data for the credential.</td>
        </tr>
    </tbody>
</table>

### Technical Details

```haskell

data SSOISchema = SSOISchema
    {   id                  :: CurrencySymbol
    ,   username            :: BuiltinString
    ,   controller          :: Maybe Address
    ,   verificationMethod  :: [VerificationMethod]
    }

data VerificationMethod = VerificationMethod
    { verifier :: CurrencySymbol
    , additionalData :: Maybe BuiltinData
    }

```
