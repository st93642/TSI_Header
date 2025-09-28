/**
 * Solidity Code Base Generator
 * Generates boilerplate Solidity smart contracts
 */

function generateSolidityCodeBase() {
    return `\n// SPDX-License-Identifier: MIT\npragma solidity ^0.8.0;\n\n/**\n * @title HelloWorld\n * @dev Basic Solidity contract\n */\ncontract HelloWorld {\n    // State variables\n    string public message;\n    address public owner;\n    uint256 public createdAt;\n    \n    // Events\n    event MessageChanged(string newMessage, address changedBy);\n    \n    // Constructor\n    constructor() {\n        message = "Hello, World!";\n        owner = msg.sender;\n        createdAt = block.timestamp;\n    }\n    \n    // Main function - view the message\n    function getMessage() public view returns (string memory) {\n        return message;\n    }\n    \n    // Function to change the message (only owner)\n    function setMessage(string memory _newMessage) public {\n        require(msg.sender == owner, "Only owner can change message");\n        message = _newMessage;\n        emit MessageChanged(_newMessage, msg.sender);\n    }\n    \n    // Function to get contract info\n    function getInfo() public view returns (string memory, address, uint256) {\n        return (message, owner, createdAt);\n    }\n    \n    // Fallback function\n    receive() external payable {\n        // Handle plain Ether transfers\n    }\n}\n`;
}

module.exports = {
    generateSolidityCodeBase
};