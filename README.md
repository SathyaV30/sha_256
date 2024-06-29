# Simplified SHA-256 Module

## Overview

This module implements a simplified version of the SHA-256 cryptographic hash algorithm. It processes a configurable number of 32-bit words and includes a testbench for validation.

## Module: `simplified_sha256`

### Parameters
- `NUM_OF_WORDS`: Number of 32-bit words to process (default: 20).

### Inputs
- `clk`: Clock signal.
- `reset_n`: Active low reset.
- `start`: Start signal.
- `message_addr`: Starting address of the message.
- `output_addr`: Address to store the hash result.

### Outputs
- `done`: Indicates completion.
- `mem_clk`: Memory clock.
- `mem_we`: Memory write enable.
- `mem_addr`: Memory address.
- `mem_write_data`: Data to write to memory.
- `mem_read_data`: Data read from memory.

### Functionality
The module reads a message from memory, processes it using the SHA-256 algorithm, and writes the hash result back to memory.

## Testbench: `tb_simplified_sha256`

### Parameters
- `NUM_OF_WORDS`: Number of 32-bit words to process (default: 20).

### Functionality
The testbench initializes the module, sets the message location, generates a test message, starts the hash process, and compares the computed hash against expected results.

## Usage

1. **Reset and Start**: Initialize the module using `reset_n` and start the hashing process using the `start` signal.
2. **Memory Interaction**: The module interacts with memory through `mem_addr`, `mem_we`, `mem_write_data`, and `mem_read_data` signals.
3. **Completion**: The `done` signal indicates when the hashing process is complete.
