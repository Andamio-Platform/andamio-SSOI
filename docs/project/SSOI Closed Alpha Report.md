# SSOI Closed Alpha Report

## Andamio Platform

**Date:** 2025/01/31
**Version:** 0.1.12
**Prepared by:** Andamio Technical Circle
**Confidentiality Level:** Internal

---

## Table of Contents  

- [SSOI Closed Alpha Report](#ssoi-closed-alpha-report)
  - [Andamio Platform](#andamio-platform)
  - [Table of Contents](#table-of-contents)
  - [1. Executive Summary](#1-executive-summary)
  - [2. Objectives of the Closed Alpha](#2-objectives-of-the-closed-alpha)
  - [3. System Overview](#3-system-overview)
    - [3.1. Architecture](#31-architecture)
    - [3.2. How SSOI Works](#32-how-ssoi-works)
  - [4. Testing Methodology](#4-testing-methodology)
    - [4.1. Test Environment](#41-test-environment)
    - [4.2. Security Testing](#42-security-testing)
  - [5. Key Findings \& Results](#5-key-findings--results)
    - [5.1. Functional Testing Results](#51-functional-testing-results)
    - [5.2. Performance Testing](#52-performance-testing)
    - [5.4. User Feedback Summary](#54-user-feedback-summary)
      - [5.4.1. Positive Feedback](#541-positive-feedback)
      - [5.4.2. Areas for Improvement](#542-areas-for-improvement)
      - [5.4.3. Summary of Feedback](#543-summary-of-feedback)
  - [6. Issues Identified \& Next Steps](#6-issues-identified--next-steps)
    - [6.1. Major Issues Identified](#61-major-issues-identified)
    - [6.2. Planned Enhancements for Beta](#62-planned-enhancements-for-beta)
  - [7. Conclusion \& Recommendations](#7-conclusion--recommendations)
    - [Next Steps](#next-steps)

---

## 1. Executive Summary

The SSOI (Self-Sovereign On-Chain Identity) system is a decentralized identity (DID) solution developed for the Andamio platform. This report outlines the closed alpha testing phase, including key findings, test results, security assessments, and feedback from participants.  

SSOI aims to provide a secure, seamless, and decentralized authentication experience for users of Andamio while leveraging blockchain technology for identity verification. The closed alpha test focused on functionality, security, user experience, and integration with the broader Web3 ecosystem.

The Andamio platform has already developed and integrated the SSOI system, which served as the foundation for this closed alpha test.

---

## 2. Objectives of the Closed Alpha

The primary objectives of this testing phase were:

- **Functional Validation:** Ensure core features, such as SSOI ID creation, authentication, and authorization, work as intended.
- **Security Assessment:** Identify potential vulnerabilities in the identity system and test robustness against attacks.  
- **User Experience (UX) Evaluation:** Gather feedback on usability, onboarding, and overall experience.  
- **Performance Testing:** Assess system efficiency and responsiveness under varying loads.  
- **Interoperability:** Test integration with other Web3 applications and Cardano-based platforms.

---

## 3. System Overview  

### 3.1. Architecture  

- **Blockchain Network:** Cardano Pre-Prod Testnet
- **SSOI Components:**  
  - SSOI ID Creation & Management
  - Authentication Mechanism  
  - Smart Contract Interactions  
  - User Interface & Integration  
- **Technology Stack:** Haskell, Plutus, Bash scripting, markdown, Atlas, Cardano node, Cardano CLI.

### 3.2. How SSOI Works  

- Users mint a SSOI DID linked to their discord account.  
- Authentication occurs via signed transactions and approval of from their discord account.
- Smart contracts validate identity claims.  
- Interoperability with Web3 applications ensures seamless cross-platform logins.

---

## 4. Testing Methodology  

### 4.1. Test Environment  

- **Blockchain Testnet:** Cardano Preprod and Cardano private testnet
- **Number of Participants:** 23
- **Devices & Platforms:** Desktop, Chrome browser, Eternal wallet  
- **Test Scenarios:**  
  - SSOI ID Registration & Authentication  
  - Multi-Device Sign-In  
  - Smart Contract Interactions  
  - Performance Under Load

### 4.2. Security Testing  

- **Penetration Testing:** Conducted by in-house team.  
- **Threat Model Considerations:**
  - Off-Chain Authentication: SSOI system utilizes Discord accounts for user registration and login. User IDs are stored in a centralized database, and session management is handled through cookies and JWTs.
  - On-Chain Authentication: SSOI system employs a specialized NFT called an "access token," secured by a linked-list smart contract system. This system ensures the uniqueness of usernames. Once a user acquires the access token in their blockchain wallet, SSOI system queries the wallet to verify token ownership, enabling authentication or authorization for specific platform features. Access tokens are particularly essential for project managers.
  - Admin Panel and Future Plans: Currently, SSOI system assigns project manager roles through an admin panel that is not publicly accessible. However, there are plans to make this functionality available to organizations, allowing them to independently manage their project managers and facilitators. Additionally, SSOI system has plans to implement new login methods in the future to enhance user accessibility and security. Also, we use [NextAuth.js](https://www.npmjs.com/package/next-auth) which is an authentication solution for Next.js applications.
- **API Security:**
  Our entire SSOI system API is public and accessible through a tool called the Andamio SDK. We also utilize third-party blockchain explorers such as Blockfrost and Maestro. The API authentication keys for these services are securely stored on the server.

---

## 5. Key Findings & Results  

### 5.1. Functional Testing Results  

| **Feature**| **Test Case** | **Expected Outcome** | **Actual Outcome** | **Status**|
|----|----|----|----|----|
| SSOI ID Creation | User generates a new self-sovereign identity (SSOI ID). | SSOI ID is successfully created and stored on-chain. | SSOI ID created successfully. | ✅ |
| SSOI ID Creation | User attempts to create a SSOI ID with invalid input (e.g., empty fields). | System rejects the request and displays an error message. | Error message displayed as expected. | ✅ |
| Identity Attestation | Smart contract verifies a user’s SSOI ID. | Attestation is recorded on-chain, and user’s SSOI ID status is updated. | Attestation recorded successfully. | ✅ |
| Identity Attestation | Smart contract attempts to verify an invalid SSOI ID. | System rejects the attestation request and displays an error. | Error message displayed as expected. | ✅ |
| Interoperability | SSOI ID is used to interact with a Cardano smart contract. | Smart contract recognizes and processes the SSOI ID correctly. | SSOI ID recognized and processed successfully. | ✅ |
| Interoperability | SSOI ID is used with a non-compatible Cardano wallet. | System displays an error message indicating incompatibility. | Error message displayed as expected. | ✅ |
| Scalability | Simulate 100 simultaneous SSOI ID creation requests. | System processes all requests within acceptable time limits. | 76/100 processed within 109 seconds | ❌ |
| User Experience| Non-technical user completes SSOI ID creation and attestation. | User successfully completes the process without assistance. | User completed the process in 11 minutes with 5 errors. | ❌ |
| Security | Attempt to forge a SSOI ID attestation using unauthorized credentials (discord account). | System rejects the forged attestation and logs the attempt. | Forged attestation rejected successfully. | ✅ |

### 5.2. Performance Testing  

- **Average Authentication Time:** 38s till 96s when network is busy
- **Load Test Result:** Handled up to 12 concurrent logins with no major failures.  

### 5.4. User Feedback Summary  

#### 5.4.1. Positive Feedback

1. **Ease of Use**:  
   - *"The SSOI ID creation process was incredibly straightforward. I didn’t need any technical knowledge to get started, and the step-by-step guide was very helpful."* – Tester #3  
   - *"The interface is clean and intuitive. I was able to create and attest my SSOI ID in under 5 minutes."* – Tester #8  

2. **Responsiveness**:  
   - *"The system was fast and responsive. I didn’t experience any lag during SSOI ID creation or attestation."* – Tester #12  
   - *"Transactions were confirmed quickly, and the on-chain updates were almost instant."* – Tester #15  

3. **Security Features**:  
   - *"I felt confident using the platform because of the clear security measures."* – Tester #7  
   - *"The attestation process felt secure, and I liked that I could see the verification status on-chain."* – Tester #10  

4. **Interoperability**:  
   - *"I was able to use my SSOI ID with a Cardano wallet seamlessly. It’s great to see how well it integrates with other tools."* – Tester #5  
   - *"The platform works perfectly with Cardano’s ecosystem. I tested it with a smart contract, and it recognized my SSOI ID without issues."* – Tester #9  

5. **Documentation**:  
   - *"The documentation was clear and detailed. I especially appreciated the FAQs and troubleshooting section."* – Tester #2  
   - *"The video tutorials were a lifesaver! They made it easy to understand how to use the platform."* – Tester #14  

#### 5.4.2. Areas for Improvement

1. **Onboarding Process**:  
   - *"The onboarding process felt a bit lengthy. I had to go through too many steps before I could create my SSOI ID."* – Tester #6  
   - *"The initial setup could be simplified. I got stuck at the SSOI ID username generation step because the instructions weren’t clear enough."* – Tester #11  

2. **User Interface (UI)**:  
   - *"The attestation UI needs tooltips or more explanations. I wasn’t sure what some of the terms meant."* – Tester #12  
   - *"The minting process was confusing. I couldn’t find the button to mint my SSOI ID at first."* – Tester #4  

3. **Documentation Gaps**:  
   - *"The documentation didn’t cover how to use SSOI IDs with third-party apps. I had to figure it out on my own."* – Tester #5  
   - *"There’s no guide for troubleshooting common errors. I got stuck when my attestation failed, and I didn’t know what to do."* – Tester #9  

4. **Scalability Concerns**:  
   - *"When I tried to create multiple SSOI IDs at once, the system slowed down significantly. This needs to be optimized."* – Tester #7  
   - *"The platform struggled with high-volume transactions. I think it needs better scalability for mass adoption."* – Tester #10  

#### 5.4.3. Summary of Feedback

Overall, testers praised the platform’s ease of use, responsiveness, and security features. However, there were consistent concerns about the onboarding process, and UI clarity. Addressing these issues will be critical for improving user satisfaction and preparing for the public beta release.

---

## 6. Issues Identified & Next Steps  

### 6.1. Major Issues Identified  

| Issue | Severity | Status | Mitigation Plan |
|-------|---------|--------|----------------|
| Lack of proper rate limiting and insufficient API authentication measures | High | Open | Enforce OAuth2.0, implement JWT expiration checks, and add API rate limiting to prevent abuse. |
|  SSOI ID Revocation Complexity – No clear mechanism for SSOI ID recovery or revocation in case of compromise | Medium | Open | Develop a robust key rotation and SSOI ID revocation system, possibly using multi-signature. |
|   Slow Transaction Confirmation Times – Performance delays due to blockchain congestion | Medium | Open | Optimize smart contract gas usage, integrate Layer-2 scaling solutions, and explore transaction batching. |
|   Interoperability Challenges – Compatibility issues with external Web3 applications | Medium | Open | Improve adherence to W3C SSOI ID standards, collaborate with other projects for cross-chain interoperability testing. |
|   UX and Onboarding Friction – Users find the SSOI ID registration process difficult and non-intuitive | Medium | Open | Redesign onboarding with improved UI/UX, add guided tutorials, and integrate wallet connect solutions for ease of access. |

### 6.2. Planned Enhancements for Beta  

| Planned Enhancement | Description | Expected Impact |
|---------------------|-------------|----------------|
| **Enhanced Security Measures** | Implement multi-signature authentication, improved key management, and enhanced smart contract security audits. | Strengthens security, reducing risks of unauthorized access and vulnerabilities. |
| **Optimized Performance & Scalability** | Introduce Layer-2 scaling solutions and optimize smart contract gas consumption. | Improves transaction speed and reduces network congestion impact. |
| **Improved SSOI ID Recovery & Revocation** | Develop a more user-friendly key recovery system using social recovery and multi-factor authentication. | Enhances user confidence by ensuring account recoverability in case of key loss. |
| **Seamless Web3 & Cardano Integration** | Improve interoperability with other Web3 applications and ensure seamless integration with Cardano-based dApps. | Expands usability and adoption across decentralized ecosystems. |
| **Refined User Experience & Onboarding** | Enhance the UI/UX with guided tutorials, tooltips, and an intuitive SSOI ID registration process. | Reduces onboarding friction and improves user satisfaction. |

---

## 7. Conclusion & Recommendations  

The closed alpha test successfully validated core functionalities of the SSOI system, providing critical insights into security, performance, and usability. While some issues were identified, the platform demonstrates strong potential for a secure and decentralized authentication solution for Web3 applications.  

### Next Steps

- Address identified security issues.  
- Improve UX based on participant feedback.  
- Conduct beta testing with a broader user base.  
- Perform a full security audit before the mainnet release.  

---
