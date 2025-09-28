/**
 * Assembly Code Base Generator
 * Generates boilerplate Assembly code
 */

function generateAssemblyCodeBase() {
    return `\n; Basic Assembly program (x86-64 Linux)\n; Note: This is a basic assembly program template\n\nsection .data\n    hello db 'Hello, World!', 0xA  ; String with newline\n    hello_len equ $ - hello         ; Length of string\n    \n    message db 'This is a basic Assembly program.', 0xA\n    message_len equ $ - message\n\nsection .text\n    global _start\n\n_start:\n    ; Write "Hello, World!" to stdout\n    mov rax, 1          ; syscall: write\n    mov rdi, 1          ; file descriptor: stdout\n    mov rsi, hello      ; pointer to string\n    mov rdx, hello_len  ; length of string\n    syscall\n    \n    ; Write message to stdout\n    mov rax, 1          ; syscall: write\n    mov rdi, 1          ; file descriptor: stdout\n    mov rsi, message    ; pointer to message\n    mov rdx, message_len ; length of message\n    syscall\n    \n    ; Exit program\n    mov rax, 60         ; syscall: exit\n    xor rdi, rdi        ; exit code 0\n    syscall\n\n; Example function (uncomment to use)\n; greet:\n;     ; Function to print greeting\n;     ; Parameters would be passed in registers\n;     ret\n`;
}

module.exports = {
    generateAssemblyCodeBase
};