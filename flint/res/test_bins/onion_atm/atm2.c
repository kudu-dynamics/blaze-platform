#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define NAME_SIZE 32
#define DESC_SIZE 128
#define CARD_NUM_SIZE 17  // 16 digits + null terminator

//
// Data Structures
//

typedef struct Card {
    char card_number[CARD_NUM_SIZE];
    struct Card *next;
} Card;

typedef struct History {
    char desc[DESC_SIZE];
    struct History *next;
} History;

typedef struct Account {
    int id;
    double balance;
    char name[NAME_SIZE];
    Card *cards;  // Each account has its own linked list of cards
} Account;

//
// Global History List (shared for simplicity)
//

History *historyHead = NULL;

//
// Command/Event Loop Structures
//

typedef enum {
    CMD_DEPOSIT,
    CMD_WITHDRAW,
    CMD_UPDATE_NAME,    // Update account name using a vulnerable sprintf
    CMD_ADD_CARD,       // Add a card; the card number is processed via sprintf (vuln)
    CMD_REMOVE_CARD_USER,  // User removal of a card (free but does not remove pointer)
    CMD_REMOVE_CARD_ADMIN, // Admin removal of a card (free again if same card was removed)
    CMD_SHOW_HISTORY,
    CMD_SHOW_CARDS,
    CMD_EXIT
} CommandType;

typedef struct {
    CommandType type;
    char arg[256];  // Used for card numbers, new account name, etc.
    double amount;  // Used for deposit/withdraw commands
} Command;

//
// API Functions
//

// Adds a history record.
void addHistory(const char *desc) {
    History *entry = malloc(sizeof(History));
    if (!entry) {
        fprintf(stderr, "Error: Memory allocation failed for history.\n");
        exit(EXIT_FAILURE);
    }
    memset(entry, 0, sizeof(History));
    strncpy(entry->desc, desc, DESC_SIZE - 1);
    entry->next = historyHead;
    historyHead = entry;
}

// Clears the history records.
void clearHistory() {
    History *curr = historyHead;
    while (curr) {
        History *temp = curr;
        curr = curr->next;
        free(temp);
    }
    historyHead = NULL;
}

// Initializes an account.
// Vulnerability: Copies more bytes (128) than the destination (32) for the name.
void initAccount(Account *acct) {
    char aux[128];
    memset(aux, 0, sizeof(aux));
    memcpy(acct->name, aux, sizeof(aux));
    acct->id = rand();
    acct->balance = 0.0;
    acct->cards = NULL;
}

// Deposits an amount into the account.
void deposit(Account *acct, double amount) {
    acct->balance += amount;
    addHistory("Deposit made.");
}

// Withdraws an amount from the account.
void withdraw(Account *acct, double amount) {
    acct->balance -= amount;
    addHistory("Withdrawal made.");
}

// Updates the account name using sprintf with the supplied string as a format string.
// Vulnerability: If newName contains format specifiers, the sprintf call is exploitable.
void updateAccountName(Account *acct, const char *newName) {
    char buffer[128];
    sprintf(buffer, newName);  // Vulnerable usage: newName is used as the format string.
    strncpy(acct->name, buffer, NAME_SIZE - 1);
    acct->name[NAME_SIZE - 1] = '\0';
    addHistory("Account name updated.");
}

// Adds a card to the account. The card number is processed through sprintf,
// so if card_number contains format specifiers, it could be exploited.
void addCard(Account *acct, const char *card_number) {
    Card *newCard = malloc(sizeof(Card));
    if (!newCard) {
        fprintf(stderr, "Error: Memory allocation failed for card.\n");
        return;
    }
    memset(newCard, 0, sizeof(Card));
    char buf[64];
    sprintf(buf, card_number);  // Vulnerable usage: card_number is used as the format string.
    strncpy(newCard->card_number, buf, CARD_NUM_SIZE - 1);
    newCard->card_number[CARD_NUM_SIZE - 1] = '\0';
    // Insert new card at the head of the account's card list.
    newCard->next = acct->cards;
    acct->cards = newCard;
    addHistory("Card added.");
}

// Removes a card via a user request. It searches the account's card list and frees the card.
// Vulnerability: The card pointer is freed but not unlinked from the list.
void removeCardUser(Account *acct, const char *card_number) {
    Card *curr = acct->cards;
    while (curr) {
        if (strcmp(curr->card_number, card_number) == 0) {
            addHistory("Card removed (user).");
            free(curr);
            printf("User removal: Card %s removed.\n", card_number);
            return;
        }
        curr = curr->next;
    }
    printf("User removal: Card %s not found.\n", card_number);
}

// Removes a card via an admin request. Searches the same card list and frees the card.
// Vulnerability: If removeCardUser() has already been called for the same card,
// this function will free the same pointer again.
void removeCardAdmin(Account *acct, const char *card_number) {
    Card *curr = acct->cards;
    while (curr) {
        if (strcmp(curr->card_number, card_number) == 0) {
            addHistory("Card cancelled (admin).");
            free(curr);
            printf("Admin removal: Card %s cancelled.\n", card_number);
            return;
        }
        curr = curr->next;
    }
    printf("Admin removal: Card %s not found.\n", card_number);
}

// Displays the transaction history.
void showHistory() {
    History *curr = historyHead;
    printf("Transaction History:\n");
    while (curr) {
        printf(" - %s\n", curr->desc);
        curr = curr->next;
    }
}

// Displays the list of cards linked to the account.
void showCards(Account *acct) {
    Card *curr = acct->cards;
    printf("Registered Cards for account %d:\n", acct->id);
    while (curr) {
        printf(" - %s\n", curr->card_number);
        curr = curr->next;
    }
}

// Clears all cards linked to the account.
void clearCards(Account *acct) {
    Card *curr = acct->cards;
    while (curr) {
        Card *temp = curr;
        curr = curr->next;
        free(temp);
    }
    acct->cards = NULL;
}

//
// Event Loop
//

// Processes an array of commands until CMD_EXIT is encountered.
void eventLoop(Account *acct, Command *commands, int numCommands) {
    for (int i = 0; i < numCommands; i++) {
        switch (commands[i].type) {
            case CMD_DEPOSIT:
                deposit(acct, commands[i].amount);
                break;
            case CMD_WITHDRAW:
                withdraw(acct, commands[i].amount);
                break;
            case CMD_UPDATE_NAME:
                updateAccountName(acct, commands[i].arg);
                break;
            case CMD_ADD_CARD:
                addCard(acct, commands[i].arg);
                break;
            case CMD_REMOVE_CARD_USER:
                removeCardUser(acct, commands[i].arg);
                break;
            case CMD_REMOVE_CARD_ADMIN:
                removeCardAdmin(acct, commands[i].arg);
                break;
            case CMD_SHOW_HISTORY:
                showHistory();
                break;
            case CMD_SHOW_CARDS:
                showCards(acct);
                break;
            case CMD_EXIT:
                return;
            default:
                break;
        }
    }
}

#ifdef DEMO
//
// Demo Main Function
// (Compile with -DDEMO to include a demonstration. In production, simply
//  link against eventLoop() and the API functions.)
//

int main(void) {
    Account user;
    initAccount(&user);
    
    // Build a demo sequence of commands.
    Command commands[] = {
        {CMD_DEPOSIT, "", 200.0},
        {CMD_WITHDRAW, "", 50.0},
        {CMD_UPDATE_NAME, "NewName: %s", 0},  // Vulnerable if malicious format specifiers are provided.
        {CMD_ADD_CARD, "1111222233334444", 0},
        {CMD_ADD_CARD, "%x %x %x", 0},         // Vulnerable: card number used as format string.
        {CMD_REMOVE_CARD_USER, "1111222233334444", 0},
        {CMD_REMOVE_CARD_ADMIN, "1111222233334444", 0},  // Triggers double free vulnerability.
        {CMD_SHOW_HISTORY, "", 0},
        {CMD_SHOW_CARDS, "", 0},
        {CMD_EXIT, "", 0}
    };
    
    int numCommands = sizeof(commands) / sizeof(commands[0]);
    eventLoop(&user, commands, numCommands);
    
    clearHistory();
    clearCards(&user);
    return 0;
}
#endif
