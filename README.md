# TokenX Connect (Wired) / Integration Hub - Delphi Implementation

A comprehensive Delphi implementation for integrating Point of Sale (POS) systems with payment terminals through the TokenX Connect (Wired) integration, also known as Integration Hub.

## Overview

This project provides a clean and robust Delphi wrapper for the TokenX Connect (Integration Hub) library, allowing seamless communication between POS applications and payment terminals. The integration handles the complexities of device communication, allowing developers to focus on the business logic instead of low-level protocol implementations.

## Features

- **Dynamic DLL Loading**: Automatically loads required DLLs at runtime
- **Event-driven Architecture**: Uses callback-based device state monitoring
- **Asynchronous Communication**: Non-blocking operations with proper thread synchronization
- **Comprehensive Error Handling**: Robust error reporting and recovery mechanisms
- **Auto-reconnection**: Automatically attempts to reestablish connection on failure
- **Thread-safe UI Updates**: All UI operations are properly marshaled to the main thread

## Key Functionality

- **Basket/Cart Processing**: Send item lists and amounts to payment terminals
- **Payment Processing**: Process various payment types (credit, debit, cash, etc.)
- **Fiscal Information**: Retrieve fiscal data from connected devices
- **Detailed Logging**: Comprehensive logging for debugging and audit purposes

## Technical Implementation

- **Language**: Object Pascal (Delphi)
- **UI Framework**: Visual Component Library (VCL)
- **Design Pattern**: Singleton for POS communication manager
- **External Dependencies**:
  - IntegrationHubCpp.dll - Main integration library
  - libcrypto-3.dll - Cryptography support library
  - libusb-1.0.dll - USB communication library
  - zlib1.dll - Compression library

## Project Structure

- **Unit1.pas**: Main implementation file containing both UI and business logic
- **Unit1.dfm**: Form definition file for the user interface
- **TPOSCommunication Class**: Handles all communication with payment terminals
- **TForm1 Class**: Provides user interface and event handling

## Requirements

- Windows operating system
- Delphi IDE (developed and tested with Delphi 10.x+)
- TokenX Connect (Wired) / Integration Hub installation
- Connected payment terminal (for actual usage)

## Setup Instructions

1. Clone this repository
2. Ensure all required DLLs are available in the application directory or Win32\Debug subdirectory:
   - IntegrationHubCpp.dll
   - libcrypto-3.dll
   - libusb-1.0.dll
   - zlib1.dll
3. Open the project in Delphi IDE
4. Build and run the application

## Usage Example

The demo application provides a simple interface for testing the integration:

1. **Connection**: The application automatically attempts to connect to available payment terminals
2. **Send Basket**: Sends a sample basket of items to the payment terminal
3. **Send Payment**: Initiates a payment transaction on the terminal
4. **Get Fiscal Info**: Retrieves fiscal information from the connected device

## Customization

To integrate with your own POS system:

1. Modify the company name in the `FormCreate` method:
   ```delphi
   FPOSComm := TPOSCommunication.Create(Self, 'YourCompanyName');
   ```

2. Customize the basket and payment JSON structures to match your business requirements

3. Implement additional error handling and recovery as needed for your environment

## Troubleshooting

- **DLL Loading Issues**: Ensure all required DLLs are available in the application directory or Win32\Debug
- **Connection Problems**: Check physical connection to the terminal and verify device drivers
- **Transaction Errors**: Check the log output for specific error codes and messages
