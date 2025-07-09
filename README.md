# OmniXtend Chisel Implementation

<div align="center">
  <img src="images/omnixtend-logo.png" alt="OmniXtend Logo" width="300">
</div>

A Chisel implementation of the OmniXtend protocol, providing a hardware-accelerated TileLink over Ethernet (TLOE) communication framework.

## Overview

This project implements the OmniXtend protocol in Chisel, enabling high-performance TileLink-based communication over Ethernet networks in hardware. It provides both master and slave endpoint implementations with support for Ethernet transport fabric, designed for FPGA and ASIC implementations.

## Features

- **TileLink Protocol Support**: Full implementation of TileLink protocol over Ethernet
- **Hardware Acceleration**: Chisel-based hardware implementation for FPGA/ASIC
- **Ethernet Transport**: Native Ethernet mode with MAC layer support
- **Master/Slave Architecture**: Support for both master and slave endpoint roles
- **Flow Control**: Credit-based flow control mechanism
- **Reliable Communication**: Sequence number management and retransmission support
- **Memory Operations**: Read/write memory operations with configurable parameters
- **High Performance**: Hardware-optimized for low-latency, high-throughput communication
- **Configurable Design**: Parameterizable design for different deployment scenarios

## Project Structure

```
├── src/main/scala/
│   ├── OX.scala                    # Main OmniXtend module
│   ├── tloe_endpoint.scala         # Main endpoint implementation
│   ├── connection.scala            # Connection management
│   ├── Packet.scala                # Packet formatting and parsing
│   ├── TLoEPacket.scala            # TLOE-specific packet handling
│   ├── tloe_ether.scala            # Ethernet transport layer
│   ├── tloe_transmitter.scala      # Transmission logic
│   ├── tloe_receiver.scala         # Reception logic
│   ├── tloe_seq_mgr.scala          # Sequence number management
│   ├── flowcontrol.scala           # Flow control implementation
│   ├── tloe_retansmission.scala    # Retransmission logic
│   ├── tilelink_handler.scala      # TileLink protocol handler
│   ├── tilelink_message.scala      # TileLink message structures
│   ├── tloe_timer.scala            # Timer management
│   └── memory_manager.scala        # Memory management
├── images/                         # Project images and logos
└── LICENSE                         # License information
```

## Building

### Prerequisites

- Scala 2.12 or later
- SBT (Scala Build Tool)
- Chisel 3.x
- Verilator (for simulation)

### Compilation

```bash
# Basic build
sbt compile

# Generate Verilog
sbt "runMain tloe_endpoint.TLoEEndpoint"

# Run tests
sbt test

# Generate documentation
sbt doc
```

### Build Targets

- `compile`: Compile Scala sources
- `test`: Run unit tests
- `run`: Generate Verilog output
- `doc`: Generate API documentation

## Usage

### Hardware Generation

The Chisel implementation generates synthesizable Verilog code:

```scala
// Generate TLOE endpoint
val endpoint = Module(new TLoEEndpoint(params))
```

### Configuration Parameters

```scala
case class TLoEParams(
  dataWidth: Int = 64,
  addrWidth: Int = 32,
  maxOutstanding: Int = 8,
  windowSize: Int = 16,
  timeoutCycles: Int = 1000
)
```

### Interface Signals

The TLOE endpoint provides the following interfaces:

- **TileLink Interface**: Standard TileLink A/B/C/D/E channels
- **Ethernet Interface**: MAC layer interface for network communication
- **Control Interface**: Configuration and status signals
- **Memory Interface**: Direct memory access for high-performance operations

## Protocol Details

### Frame Structure

TLOE frames include:
- Header with sequence numbers, acknowledgments, and channel information
- Payload data
- Flow control credits

### Channel Types

- Channel 0: Control messages
- Channel A: TileLink requests
- Channel B: TileLink responses  
- Channel C: TileLink data
- Channel D: TileLink acknowledgments
- Channel E: Reserved

### Flow Control

Credit-based flow control prevents buffer overflow:
- Credits are exchanged during connection establishment
- Credits are consumed when sending messages
- Credits are replenished when receiving acknowledgments

## Hardware Implementation

### Key Components

1. **Packet Processing**: Hardware-accelerated packet parsing and generation
2. **Connection Management**: State machine for connection establishment and maintenance
3. **Flow Control**: Credit-based flow control with hardware counters
4. **Retransmission**: Automatic retransmission with timeout mechanisms
5. **Memory Interface**: Direct memory access for TileLink operations

### Performance Characteristics

- **Latency**: Sub-microsecond packet processing
- **Throughput**: Multi-gigabit per second data transfer
- **Reliability**: Hardware-level error detection and correction
- **Scalability**: Configurable for different network sizes

## Development

### Code Style

- Chisel 3.x coding standards
- Consistent naming conventions
- Comprehensive parameterization
- Modular design for reusability

### Testing

```bash
# Run unit tests
sbt test

# Run specific test
sbt "testOnly tloe_endpoint.TLoEEndpointTest"

# Generate test waveforms
sbt "testOnly tloe_endpoint.TLoEEndpointTest -- -DwriteVcd=1"
```

### Simulation

```bash
# Run simulation
sbt "runMain tloe_endpoint.TLoESimulator"

# Generate waveforms
sbt "runMain tloe_endpoint.TLoESimulator -- -DwriteVcd=1"
```

## Deployment

### FPGA Implementation

The generated Verilog can be synthesized for various FPGA platforms:

- Xilinx FPGAs (Artix, Kintex, Virtex series)
- Intel FPGAs (Cyclone, Arria, Stratix series)
- Lattice FPGAs

### ASIC Implementation

The design is suitable for ASIC implementation with standard cell libraries.

## License

This project is licensed under the Apache License, Version 2.0. See the LICENSE file for details.

## Contributing

1. Fork the repository
2. Create a feature branch
3. Make your changes
4. Add tests if applicable
5. Submit a pull request

## Acknowledgments

- Based on the OmniXtend protocol specification
- Implements TileLink over Ethernet communication in hardware
- Designed for high-performance, reliable network communication
- Built with Chisel hardware construction language

---

<div align="center">
  <img src="images/ETRI_CI_01.png" alt="ETRI Logo" width="200">
  <br><br>
  <strong>This project is developed and maintained by <a href="https://www.etri.re.kr/">ETRI (Electronics and Telecommunications Research Institute)</a></strong>
</div>