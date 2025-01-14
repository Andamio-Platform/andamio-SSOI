# Test Transactions Report

## Get an SSOI ID Transaction

This transaction illustrates the use of smart contracts and user wallets to manage unique identifiers. By utilizing a linked-list structure and minting tokens with specific prefixes, the transaction ensures the creation and maintenance of globally unique SSOI IDs. This approach is essential for identity management on the blockchain, offering a secure and efficient method for tracking and updating identifiers.

### Transaction Components

#### 1. Global SSOI ID Policy

The Global SSOI ID Policy ensures the uniqueness of each token name by combining a global SSOI ID currency symbol with a token name. This unique identification is crucial for managing identities on the blockchain.

#### 2. Linked List Structure

Each UTxO is associated with a token and a datum, forming a linked list. Each datum is a tuple containing data and a reference to the next element in the list. This structure allows for efficient tracking and updating of identifiers.

### Transaction Process

#### Step 1: Input from Index Validator

The transaction begins by taking input from the index validator, which includes a linked list element that needs updating.

#### Step 2: Minting Tokens

Three tokens are minted during this transaction:

1. **Token Name (tn) Initialization**: Initially set to empty.  
2. **Token Name Prefixes**:  
   - `"222"` as a prefix for one token.  
   - `"100"` as a prefix for another token.  

#### Step 3: Outputs

The transaction results in four outputs:

1. **Two Outputs to Index Validator**: These outputs include the updated linked list elements.  
2. **One Output to Global SSOI ID Validator**: This output uses the `"100"` prefix and includes the `GlobalIDState`, which consists of:
   - Global SSOI ID policy ID  
   - User identifier (e.g., `"bob"`)  
   - A list of policy IDs  
3. **One Output to Wallet Owner**: The final output is directed to the wallet owner of the token.

#### Step 4: Updating Linked List

The linked list is updated as follows:

- Current element: `"a"` with the next element `"bob"`  
- Updated element: `"bob"` with the next element `"c"`  

---

## Mint Verification Method Transaction

This transaction demonstrates the intricate process of token minting and verification. By utilizing smart contracts and adhering to both local and global SSOI verification policies, the transaction ensures secure and compliant token management.

### Transaction Components

#### Local and Global SSOI ID Verifier

- **Local SSOI ID Verifier**: Represents the unique identifier for the local SSOI verifier involved in the transaction.
- **Global SSOI ID Verifier**: Contains the global SSOI ID policy ID, the token name (e.g., "bob"), and a list of local SSOI ID policy IDs.

#### Tokens

- **100 ID Token**: The initial token representing ownership, used as input in the transaction.
- **222 ID Token**: The user-provided token to prove ownership of the 100 ID token.
- **Minted Token**: A new token minted during the transaction, sharing the same name as the 100 ID token but without the prefix.

### Transaction Process

#### Input from Global SSOI ID Verifier

- The transaction begins with the input of the 100 ID token and the global SSOI ID verifier datum.
- The user provides the 222 ID token to prove ownership of the 100 ID token.

#### Minting Process

- A new token is minted with the same name as the 100 ID token, excluding the prefix.
- This process involves verifying the ownership and adhering to the local SSOI ID verifier policy rules.

#### Outputs

1. **Global SSOI ID Verifier Validator**:
   - The minted token is sent back to the global SSOI ID verifier validator with a "100" prefix.
   - The GlobalIDState is updated to include the local SSOI ID verifier policy ID in its list.
2. **User**:
   - The 222 ID token is returned to the user.
3. **Local SSOI ID Verifier Validator**:
   - The newly minted local SSOI ID verifier is sent to the local SSOI ID verifier validator for further verification and management.

---

## Issue Credentials Transaction

This transaction plays a crucial role in maintaining an accurate and up-to-date record of credentials on the Cardano blockchain. It ensures that all credentials are verified and authorized by a trusted issuer, thereby enhancing the security and reliability of the system.

### Transaction Components

- **Local SSOI ID Verifier ID**: A unique identifier representing the current state of the credentials within the blockchain system.
- **Local SSOI ID Verifier Datum**: The data structure that holds the credentials. Initially, it may contain existing credentials (e.g., `[(cred 1)]`).

### Transaction Process

1. **Input from Local SSOI ID Verifier Policy ID**:  
   The transaction begins by referencing the current local SSOI ID verifier state. This state contains the existing credentials that are to be updated.

2. **Issuer's Role**:  
   The issuer, which can be a wallet, token, or an entire contract system, plays a crucial role in this transaction. The issuer must sign the transaction to authorize the issuance of new credentials.

3. **Credential Issuance**:  
   Upon successful authorization by the issuer, a new credential is added to the local SSOI ID verifier datum. This addition follows a predefined schema specific to the local SSOI ID verifier, ensuring consistency and validity of the data.

4. **Output Back to Local SSOI ID Verifier Policy ID**:  
   The updated local SSOI ID verifier datum, now containing the newly issued credential, is output back to the same local SSOI ID verifier validator. This ensures that the state is consistently updated and reflects the latest credentials.

---

## Prove Credential Ownership Transaction

This transaction demonstrates a secure method for verifying credential ownership. By utilizing SSOI ID verifier data, credential verification processes, and smart contract validation, the transaction ensures that only authorized users can claim ownership of the credentials.

### Transaction Components

#### Local SSOI ID Verifier

- **Local SSOI ID Verifier Policy ID**: A unique identifier for the local SSOI ID verifier involved in the transaction.
- **Local SSOI ID Verifier Datum**: Contains credentials, specifically `cred 1`, which are used to verify ownership.

#### Input and Output

- **Input from Local SSOI ID Verifier Policy ID**: The transaction uses input from the local SSOI ID verifier policy ID, which includes the credential in the datum.
- **Output Datum**: The result of the transaction, which confirms the credential ownership.

#### Credential Verification

- **Credential 222**: This credential is crucial for proving ownership. It is present in the transaction to authenticate the user's claim.
- **Token Name Matching**: The local SSOI ID verifier token name must match the `222` token name (without prefix) to validate the transaction.

### Transaction Process

1. **Initialization**: The transaction begins with the identification of the local SSOI ID verifier policy ID and the associated datum containing `cred 1`.
2. **Credential Input**: The input is taken from the local SSOI ID verifier policy ID, ensuring that the credential in the datum is included.
3. **Verification**: Credential `222` is also present in the transaction to prove ownership. The system checks if the local SSOI ID verifier token name matches the `222` token name (without prefix).
4. **Validation**: The transaction is validated through a series of checks involving the validator, redeemer, and policy components.
5. **Output Generation**: Once validated, the transaction generates an output datum confirming the credential ownership.
