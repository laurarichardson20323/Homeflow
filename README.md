# 🏠 Homeflow - Smart Contract Escrow for Home Sales

A secure escrow smart contract built on Stacks blockchain that locks buyer funds until property verification is complete. Perfect for real estate transactions requiring trust and verification! 🔐

## ✨ Features

- 💰 **Secure Fund Locking**: Buyer funds are safely locked in escrow until verification
- 🔍 **Property Verification**: Authorized verifiers can validate property conditions
- ⏰ **Time-based Expiration**: Automatic escrow expiration to prevent indefinite locks
- 🤝 **Dispute Resolution**: Built-in dispute mechanism with owner arbitration
- 📊 **Transaction Tracking**: Complete visibility into escrow status and history

## 🚀 Quick Start

### Creating an Escrow

```clarity
(contract-call? .Homeflow create-escrow 
  'SP2J6ZY48GV1EZ5V2V5RB9MP66SW86PYKKNRV9EJ7  ;; seller address
  u1000000                                        ;; amount in microSTX
  "property-123-main-st"                         ;; property identifier
  u1000)                                         ;; duration in blocks
```

### Verifying a Property

```clarity
(contract-call? .Homeflow verify-property u1)  ;; escrow ID
```

### Completing the Sale

```clarity
(contract-call? .Homeflow complete-sale u1)    ;; escrow ID
```

## 🔧 Core Functions

### Public Functions

| Function | Description | Who Can Call |
|----------|-------------|--------------|
| `create-escrow` | 🏗️ Create new escrow with locked funds | Buyers |
| `verify-property` | ✅ Mark property as verified | Authorized verifiers |
| `complete-sale` | 🎉 Release funds to seller | Buyer or seller (after verification) |
| `cancel-escrow` | ❌ Cancel and refund buyer | Buyer, seller, or after expiration |
| `dispute-escrow` | ⚖️ Flag escrow for dispute | Buyer or seller |
| `add-verifier` | 👤 Add authorized verifier | Contract owner only |

### Read-Only Functions

| Function | Description |
|----------|-------------|
| `get-escrow` | 📋 Get escrow details |
| `get-escrow-status` | 📊 Check escrow status |
| `is-escrow-expired` | ⏰ Check if escrow expired |
| `is-verifier` | 🔍 Check if address is authorized verifier |

## 📝 Escrow Lifecycle

1. **🏠 Creation**: Buyer creates escrow with funds locked
2. **🔍 Verification**: Authorized verifier inspects and approves property
3. **✅ Completion**: Funds released to seller after verification
4. **🔄 Alternative**: Cancellation returns funds to buyer

## 🛡️ Security Features

- **Fund Safety**: All funds locked in contract until resolution
- **Authorization**: Only verified inspectors can approve properties
- **Time Limits**: Automatic expiration prevents indefinite locks
- **Dispute Resolution**: Owner can resolve conflicts fairly

## 🎯 Use Cases

- 🏘️ **Residential Sales**: Traditional home purchases with inspection periods
- 🏢 **Commercial Real Estate**: Business property transactions
- 🏗️ **New Construction**: Escrow until construction milestones met
- 🔄 **Property Flips**: Secure transactions between investors

## ⚠️ Error Codes

| Code | Description |
|------|-------------|
| `u100` | Not authorized |
| `u101` | Escrow not found |
| `u102` | Invalid amount |
| `u103` | Escrow already exists |
| `u104` | Insufficient funds |
| `u105` | Escrow not pending |
| `u106` | Not buyer or seller |
| `u107` | Verification failed |
| `u108` | Escrow expired |

## 🧪 Testing

Deploy with Clarinet and test all functions:

```bash
clarinet console
```

## 📄 License

MIT License - Build amazing real estate solutions! 🚀


