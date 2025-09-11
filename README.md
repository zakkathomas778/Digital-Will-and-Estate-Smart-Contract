# Digital Will and Estate Smart Contract

A Clarity smart contract that enables users to create digital wills and automate asset distribution after proof of death.

## Features

- Create and manage digital wills
- Add multiple beneficiaries with specific asset allocations
- Set up asset distribution rules
- Executor management system
- Proof of death verification
- Automated asset distribution

## Contract Functions

### Administrative
- `initialize-contract`: Set up the contract initially
- `set-executor`: Assign contract executor
- `verify-death`: Confirm death status

### Will Management  
- `create-will`: Create a new digital will
- `add-beneficiary`: Add beneficiary with allocation
- `update-beneficiary`: Modify beneficiary details
- `remove-beneficiary`: Remove a beneficiary
- `get-will-details`: View will information

### Asset Distribution
- `distribute-assets`: Execute asset distribution
- `claim-inheritance`: Beneficiary claim function

## Usage

1. Deploy the contract
2. Initialize with executor address
3. Create will and add beneficiaries
4. Set up distribution rules
5. Upon death verification, assets are distributed

## Testing

```bash
clarinet test
```

## Development

```bash
clarinet console