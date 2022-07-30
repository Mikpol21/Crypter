# Crypter
## Encrypting your confidential files
### Usage
```
Usage: ./Crypter flags files
Flags: 
    -e -> encrypt file
    -c -> show contents of encrypted file
    -d -> decrypt encrypted file
    -r -> work recursively on directories
    -h -> guide
```
### Example usage
![Example usage](Demonstration/Example.png)
### Details
Encrypting files using AES256 together with a key derivation function PBKDF2
### Disclaimer
At this moment Crypter is unsafe. Having warned you, I do not take any responsibility for any damage caused by Crypter.
### Warnings
- Do not modify encrypted files. Even smallest changes like saving a file without changing anything might lead to losing the data inside them
- Be careful not to encrypt a file twice
- There is possibility of retrieving forgotten password
