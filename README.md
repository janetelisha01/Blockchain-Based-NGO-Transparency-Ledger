# 🌍 NGO Transparency Ledger

A blockchain-based smart contract system for tracking NGO donations and expenses, ensuring complete transparency in how charitable funds are utilized.

## 🚀 Features

- 📝 **NGO Registration**: Register verified NGOs on the blockchain
- 💰 **Donation Tracking**: Record all donations with purpose and donor information
- 📊 **Expense Management**: Track how NGOs spend their funds
- 🔍 **Transparency Score**: Calculate transparency metrics for each NGO
- ✅ **Verification System**: Admin verification for legitimate NGOs
- 💳 **Balance Tracking**: Real-time balance monitoring for each NGO

## 🛠️ Installation

```bash
git clone <your-repo>
cd ngo-transparency-ledger
clarinet check
```

## 📋 Usage

### Register an NGO
Only contract owner can register NGOs:

```bash
clarinet console
(contract-call? .ngo-transparency register-ngo "Red Cross Foundation" 'ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM)
```

### Make a Donation
Anyone can donate to registered NGOs:

```bash
(contract-call? .ngo-transparency make-donation u1 u1000000 "Emergency Relief Fund")
```

### Record an Expense
NGOs can record their expenses:

```bash
(contract-call? .ngo-transparency record-expense u1 u500000 "Medical" "Purchased medical supplies for disaster relief" "MedSupply Corp")
```

### Check NGO Information
View NGO details and transparency:

```bash
(contract-call? .ngo-transparency get-ngo u1)
(contract-call? .ngo-transparency get-ngo-balance u1)
(contract-call? .ngo-transparency get-ngo-transparency-score u1)
```

## 🔧 Contract Functions

### Public Functions
- `register-ngo` - Register a new NGO (admin only)
- `make-donation` - Donate STX to an NGO
- `record-expense` - Record NGO expenses (NGO wallet only)
- `verify-ngo` / `unverify-ngo` - Manage NGO verification status

### Read-Only Functions
- `get-ngo` - Get NGO information
- `get-donation` - Get donation details
- `get-expense` - Get expense details
- `get-ngo-balance` - Get current NGO balance
- `get-ngo-transparency-score` - Calculate transparency percentage
- `is-ngo-verified` - Check verification status

## 🎯 Core Concepts

### Transparency Score
The transparency score represents the percentage of funds that remain unspent, providing donors with insight into fund utilization efficiency.

### Verification System
Only verified NGOs can receive donations, ensuring legitimacy and preventing fraud.

### Immutable Records
All donations and expenses are permanently recorded on the blockchain, creating an unchangeable audit trail.

## 🔒 Security Features

- Admin-only NGO registration
- NGO wallet authorization for expense recording
- Balance validation before expense recording
- Input validation for all parameters

## 📈 Getting Started

1. Deploy the contract using Clarinet
2. Register your first NGO as contract owner
3. Start accepting donations and recording expenses
4. Monitor transparency scores and build donor trust

## 🤝 Contributing

Feel free to submit issues and enhancement requests!

## 📄 License

This project is open source and available under the MIT License.
```
