#### Test for hashtable ####
setwd('/Users/bruce/Desktop/BST 262/pset1')
source("hashtable.R")

### Test Case 1: No Collision ###

# Create a hash table with a fixed size
myHash <- hashing(5)

# Inserting values that should hash to different indices
myHash <- store(myHash, 3)  # 3 %% 5 + 1 = 4
myHash <- store(myHash, 7)  # 7 %% 5 + 1 = 3

# Checking if values are correctly inserted without collision
print(lookup(myHash, 3))  # Expected output: TRUE
print(lookup(myHash, 7))  # Expected output: TRUE
print(lookup(myHash, 10)) # Expected output: FALSE, 10 was not stored

### Test Case 2: Collision ###

# Create a hash table with a fixed size
myHash <- hashing(5)

# Inserting values that should hash to the same index
myHash <- store(myHash, 5)   # index: 5 %% 5 + 1 = 1
myHash <- store(myHash, 10)  # index: 10 %% 5 + 1 = 1

# Checking if both values are present at the same index (collision handled)
print(lookup(myHash, 5))   # Expected output: TRUE
print(lookup(myHash, 10))  # Expected output: TRUE
print(lookup(myHash, 15))  # Expected output: FALSE, 15 was not stored

#### Test Case 1 ####
source('cusum.R')

#### Test Case 1 ####
myCUSUM <- simplecpt(c(1, 8, 3))
myCUSUM@a # Expected output: 1 8 3

sapply(1: (length(myCUSUM@a) - 1), function(i) abs(mean(c(1, 8, 3)[1:i])-mean(c(1, 8, 3)[-(1:i)])))

checkCpt(myCUSUM, 0.01) # Expected output: TRUE
checkCpt(myCUSUM, 100) # Expected output: FALSE

#### Test Case 2 ####
myCUSUM <- simplecpt()
myCUSUM <- addData(myCUSUM, c(2, 4, 6, 10, 12))
myCUSUM@a # Expected output: 2 4 6 10 12

sapply(1: (length(myCUSUM@a) - 1), function(i) abs(mean(c(2, 4, 6, 10, 12)[1:i])-mean(c(2, 4, 6, 10, 12)[-(1:i)])))

checkCpt(myCUSUM, 0.5)   # Expected output: TRUE
checkCpt(myCUSUM, 5)     # Expected output: TRUE
checkCpt(myCUSUM, 20)    # Expected output: FALSE