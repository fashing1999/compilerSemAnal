#include "symbolTable.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
// This file is for reference only, you are not required to follow the implementation. //

int HASH(char * str) {
	int idx=0;
	while (*str){
		idx = idx << 1;
		idx+=*str;
		str++;
	}
	return (idx & (HASH_TABLE_SIZE-1));
}

SymbolTable symbolTable;

SymbolTableEntry* newSymbolTableEntry(int nestingLevel)
{
    SymbolTableEntry* symbolTableEntry = (SymbolTableEntry*)malloc(sizeof(SymbolTableEntry));
    symbolTableEntry->nextInHashChain = NULL;
    symbolTableEntry->prevInHashChain = NULL;
    symbolTableEntry->nextInSameLevel = NULL;
    symbolTableEntry->sameNameInOuterLevel = NULL;
    symbolTableEntry->attribute = NULL;
    symbolTableEntry->name = NULL;
    symbolTableEntry->nestingLevel = nestingLevel;
    return symbolTableEntry;
}

void removeFromHashTrain(int hashIndex, SymbolTableEntry* entry)
{
    if(entry->prevInHashChain)  //not head
        entry->prevInHashChain->nextInHashChain = entry->nextInHashChain;
    else                        //head
        symbolTable.hashTable[hashIndex] = entry->nextInHashChain;
    if(entry->nextInHashChain)  //not tail
        entry->nextInHashChain->prevInHashChain = entry->prevInHashChain;

    entry->nextInHashChain = NULL;
    entry->prevInHashChain = NULL;
}

void enterIntoHashTrain(int hashIndex, SymbolTableEntry* entry)
{
    SymbolTableEntry* tmp = symbolTable.hashTable[hashIndex];
    if(tmp){  //the chain has entries
        tmp->prevInHashChain = entry;
        entry->nextInHashChain = tmp;
    }
    symbolTable.hashTable[hashIndex] = entry;
}

void initializeSymbolTable()
{
    symbolTable.currentLevel = 0;
    symbolTable.scopeDisplayElementCount = 16;
    symbolTable.scopeDisplay = (SymbolTableEntry**)malloc(symbolTable.scopeDisplayElementCount * sizeof(SymbolTableEntry*));

    for(int i = 0; i < HASH_TABLE_SIZE; ++i)
        symbolTable.hashTable[i] = NULL;

    for(int i = 0; i < symbolTable.scopeDisplayElementCount; ++i)
        symbolTable.scopeDisplay[i] = NULL;
    
    //reserved words
    SymbolAttribute* voidAttr = (SymbolAttribute*)malloc(sizeof(SymbolAttribute));
    voidAttr->attributeKind = TYPE_ATTRIBUTE;
    voidAttr->attr.typeDescriptor = (TypeDescriptor*)malloc(sizeof(TypeDescriptor));
    voidAttr->attr.typeDescriptor->kind = SCALAR_TYPE_DESCRIPTOR;
    voidAttr->attr.typeDescriptor->properties.dataType = VOID_TYPE;
    enterSymbol(SYMBOL_TABLE_VOID_NAME, voidAttr);

    SymbolAttribute* intAttr = (SymbolAttribute*)malloc(sizeof(SymbolAttribute));
    intAttr->attributeKind = TYPE_ATTRIBUTE;
    intAttr->attr.typeDescriptor = (TypeDescriptor*)malloc(sizeof(TypeDescriptor));
    intAttr->attr.typeDescriptor->kind = SCALAR_TYPE_DESCRIPTOR;
    intAttr->attr.typeDescriptor->properties.dataType = INT_TYPE;
    enterSymbol(SYMBOL_TABLE_INT_NAME, intAttr);

    SymbolAttribute* floatAttr = (SymbolAttribute*)malloc(sizeof(SymbolAttribute));
    floatAttr->attributeKind = TYPE_ATTRIBUTE;
    floatAttr->attr.typeDescriptor = (TypeDescriptor*)malloc(sizeof(TypeDescriptor));
    floatAttr->attr.typeDescriptor->kind = SCALAR_TYPE_DESCRIPTOR;
    floatAttr->attr.typeDescriptor->properties.dataType = FLOAT_TYPE;
    enterSymbol(SYMBOL_TABLE_FLOAT_NAME, floatAttr);

    SymbolAttribute* readAttr = NULL;
    readAttr = (SymbolAttribute*)malloc(sizeof(SymbolAttribute));
    readAttr->attributeKind = FUNCTION_SIGNATURE;
    readAttr->attr.functionSignature = (FunctionSignature*)malloc(sizeof(FunctionSignature));
    readAttr->attr.functionSignature->returnType = INT_TYPE;
    readAttr->attr.functionSignature->parameterList = NULL;
    readAttr->attr.functionSignature->parametersCount = 0;
    enterSymbol(SYMBOL_TABLE_SYS_LIB_READ, readAttr);

    SymbolAttribute* freadAttr = NULL;
    freadAttr = (SymbolAttribute*)malloc(sizeof(SymbolAttribute));
    freadAttr->attributeKind = FUNCTION_SIGNATURE;
    freadAttr->attr.functionSignature = (FunctionSignature*)malloc(sizeof(FunctionSignature));
    freadAttr->attr.functionSignature->returnType = FLOAT_TYPE;
    freadAttr->attr.functionSignature->parameterList = NULL;
    freadAttr->attr.functionSignature->parametersCount = 0;
    enterSymbol(SYMBOL_TABLE_SYS_LIB_FREAD, freadAttr);
}

void symbolTableEnd()
{
    // do nothing
}

SymbolTableEntry* retrieveSymbol(char* symbolName)
{
    SymbolTableEntry* tmp = symbolTable.hashTable[HASH(symbolName)];
    while(tmp){
        if(!strcmp(tmp->name, symbolName))
            return tmp;
        else
            tmp = tmp->nextInHashChain;
    }
    return NULL;
}

SymbolTableEntry* enterSymbol(char* symbolName, SymbolAttribute* attribute)
{
    SymbolTableEntry* newEntry = newSymbolTableEntry(symbolTable.currentLevel);
    newEntry->attribute = attribute;
    newEntry->name = symbolName;

    int hashIndex = HASH(symbolName);
    SymbolTableEntry* tmp = symbolTable.hashTable[hashIndex];
    while(tmp){
        if(!strcmp(tmp->name, symbolName)){
            //different level -> make branch
            removeFromHashTrain(hashIndex, tmp);
            newEntry->sameNameInOuterLevel = tmp;
            break;
        }
        else {
            tmp = tmp->nextInHashChain;
        }
    }

    enterIntoHashTrain(hashIndex, newEntry);
    newEntry->nextInSameLevel = symbolTable.scopeDisplay[symbolTable.currentLevel];
    symbolTable.scopeDisplay[symbolTable.currentLevel] = newEntry;
    
    return newEntry;
}

//remove the symbol from the current scope
void removeSymbol(char* symbolName)
{
    int hashIndex = HASH(symbolName);
    SymbolTableEntry* tmp = symbolTable.hashTable[hashIndex];
    while(tmp){
        if(!strcmp(tmp->name, symbolName)){
            if(tmp->nestingLevel != symbolTable.currentLevel){
                printf("[DEBUG] Invalid removeSymbol(): name \'%s\' found in one of the outer scopes.\n", symbolName);
                return;
            }
            else{
                removeFromHashTrain(hashIndex, tmp);
                if(tmp->sameNameInOuterLevel)
                    enterIntoHashTrain(hashIndex, tmp->sameNameInOuterLevel);
                break;
            }
        }
        else
            tmp = tmp->nextInHashChain;
    }

    if(!tmp){
        printf("[DEBUG] Invalid removeSymbol(): name \'%s\' not found in symbol table.\n", symbolName);
        return;
    }

    SymbolTableEntry* PrevChainElement = NULL;
    SymbolTableEntry* scopeChain = symbolTable.scopeDisplay[symbolTable.currentLevel];
    while(scopeChain){
        if(!strcmp(scopeChain->name, symbolName)){
            if(PrevChainElement)
                PrevChainElement->nextInSameLevel = scopeChain->nextInSameLevel;
            else
                symbolTable.scopeDisplay[symbolTable.currentLevel] = scopeChain->nextInSameLevel;
            free(scopeChain);
            break;
        }
        else{
            PrevChainElement = scopeChain;
            scopeChain = scopeChain->nextInSameLevel;
        }
    }
}

int declaredLocally(char* symbolName)
{
    SymbolTableEntry* tmp = symbolTable.hashTable[HASH(symbolName)];
    while(tmp){
        if(!strcmp(tmp->name, symbolName)){
            return (tmp->nestingLevel == symbolTable.currentLevel)? 1 : 0; 
        }
        tmp = tmp->nextInHashChain;
    }
    return 0;
}

void openScope()
{
    symbolTable.currentLevel++;
    if (symbolTable.currentLevel >= symbolTable.scopeDisplayElementCount) {
        SymbolTableEntry **origScopeDisplay = symbolTable.scopeDisplay;
        symbolTable.scopeDisplay = (SymbolTableEntry **)malloc(
            2 * symbolTable.scopeDisplayElementCount * sizeof(SymbolTableEntry *)
        );

        for (int i = 0; i < symbolTable.scopeDisplayElementCount; i++) {
            symbolTable.scopeDisplay[i] = origScopeDisplay[i];
            symbolTable.scopeDisplay[i + symbolTable.scopeDisplayElementCount] = NULL;
        }

        free(origScopeDisplay);
        symbolTable.scopeDisplayElementCount *= 2;
    }
}

void closeScope()
{
    if(symbolTable.currentLevel < 0){
        printf("[DEBUG] Invalid closeScope(): currentLevel < 0.\n");
        return;
    }

    SymbolTableEntry* scopeChain = symbolTable.scopeDisplay[symbolTable.currentLevel];
    while(scopeChain){
        int hashIndex = HASH(scopeChain->name);
        removeFromHashTrain(hashIndex, scopeChain);
        if(scopeChain->sameNameInOuterLevel)
            enterIntoHashTrain(hashIndex, scopeChain->sameNameInOuterLevel);

        scopeChain = scopeChain->nextInSameLevel;
    }
    
    symbolTable.scopeDisplay[symbolTable.currentLevel] = NULL;
    symbolTable.currentLevel--;
}
