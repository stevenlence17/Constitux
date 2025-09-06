# 🏛️ Constitux - Constitutional Proposal System

> 🔀 Forkable governance protocols for decentralized decision-making

## 📖 Overview

Constitux is a revolutionary governance system built on Stacks that enables communities to create, fork, and manage their own constitutional frameworks. Unlike traditional governance systems, Constitux allows for **forkable governance** - meaning communities can branch off and create their own governance rules while maintaining connections to parent systems.

## ✨ Key Features

- 🌱 **Genesis Fork Creation** - Start your own governance system from scratch
- 🔀 **Governance Forking** - Branch off from existing systems with custom rules  
- 🗳️ **Weighted Voting** - Stake-based voting power for fair representation
- 📊 **Flexible Thresholds** - Customizable approval rates for different governance styles
- 👥 **Membership Management** - Join forks with stake requirements
- 📝 **Proposal System** - Create and vote on constitutional proposals
- 🔍 **Transparency** - All votes and proposals are publicly auditable

## 🚀 Getting Started

### Prerequisites

- [Clarinet](https://github.com/hirosystems/clarinet) installed
- Basic understanding of Clarity smart contracts

### Installation

1. Clone this repository
2. Navigate to the project directory
3. Deploy the contract using Clarinet

```bash
clarinet deploy
```

## 🎯 Core Functions

### 🏗️ Creating Governance Systems

#### Create Genesis Fork
```clarity
(contract-call? .constitux create-genesis-fork "My DAO" "A decentralized community" u60)
```
- Creates a new root governance system
- Set custom voting thresholds (1-100%)
- Become the founding member

#### Fork Existing Governance
```clarity
(contract-call? .constitux fork-governance u1 "Reformed DAO" "A better approach" u75)
```
- Branch off from existing governance (fork-id: 1)
- Inherit base rules but customize thresholds
- Start fresh with new membership

### 👥 Membership & Participation

#### Join a Fork
```clarity
(contract-call? .constitux join-fork u1 u5000000)
```
- Stake tokens to join governance fork
- Gain voting power proportional to stake
- Participate in proposals and decisions

#### Create Proposals
```clarity
(contract-call? .constitux create-proposal u1 "Treasury Allocation" "Proposal to allocate 10% to development" "financial")
```
- Submit constitutional proposals
- Requires minimum stake in the fork
- Set proposal type and detailed description

### 🗳️ Voting Process

#### Cast Your Vote
```clarity
(contract-call? .constitux vote-on-proposal u1 true)
```
- Vote for (true) or against (false) proposals
- Voting power based on your stake weight
- One vote per proposal per member

#### Execute Approved Proposals
```clarity
(contract-call? .constitux execute-proposal u1)
```
- Execute proposals that meet threshold requirements
- Only after voting period ends
- Permanently marks proposal as executed

## 📊 Query Functions

### Get Proposal Details
```clarity
(contract-call? .constitux get-proposal u1)
```

### Check Fork Information
```clarity
(contract-call? .constitux get-fork u1)
```

### View Proposal Status
```clarity
(contract-call? .constitux get-proposal-status u1)
```

### Check Membership
```clarity
(contract-call? .constitux get-fork-member u1 'SP1234...)
```

## 🔧 Configuration

### Default Settings
- **Minimum Proposal Stake**: 1,000,000 micro-STX
- **Voting Period**: 1,440 blocks (~10 days)
- **Voting Threshold**: Customizable per fork (1-100%)


