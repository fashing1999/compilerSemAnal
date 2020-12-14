#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "header.h"
#include "symbolTable.h"
// This file is for reference only, you are not required to follow the implementation. //
// You only need to check for errors stated in the hw4 document. //
int g_anyErrorOccur = 0;

extern char **typeNameStrings;

DATA_TYPE getBiggerType(DATA_TYPE dataType1, DATA_TYPE dataType2);
void processProgramNode(AST_NODE *programNode);
void processDeclarationNode(AST_NODE* declarationNode);
void declareIdList(AST_NODE* typeNode, SymbolAttributeKind isVariableOrTypeAttribute, int ignoreArrayFirstDimSize);
void declareFunction(AST_NODE* returnTypeNode);
void processDeclDimList(AST_NODE* variableDeclDimList, TypeDescriptor* typeDescriptor, int ignoreFirstDimSize);
void processTypeNode(AST_NODE* typeNode);
void processBlockNode(AST_NODE* blockNode);
void processStmtNode(AST_NODE* stmtNode);
void processGeneralNode(AST_NODE *node);
void checkAssignOrExpr(AST_NODE* assignOrExprRelatedNode);
void checkWhileStmt(AST_NODE* whileNode);
void checkForStmt(AST_NODE* forNode);
void checkAssignmentStmt(AST_NODE* assignmentNode);
void checkIfStmt(AST_NODE* ifNode);
void checkWriteFunction(AST_NODE* functionCallNode);
void checkReadFunction(AST_NODE* functionCallNode);
void checkFunctionCall(AST_NODE* functionCallNode);
void processExprRelatedNode(AST_NODE* exprRelatedNode);
void checkParameterPassing(Parameter* formalParameter, AST_NODE* actualParameter);
void checkReturnStmt(AST_NODE* returnNode);
void processExprNode(AST_NODE* exprNode);
void processVariableLValue(AST_NODE* idNode);
void processVariableRValue(AST_NODE* idNode);
void processConstValueNode(AST_NODE* constValueNode);
void getExprOrConstValue(AST_NODE* exprOrConstNode, int* iValue, float* fValue);
void evaluateExprValue(AST_NODE* exprNode);


typedef enum ErrorMsgKind
{
    SYMBOL_IS_NOT_TYPE,
    SYMBOL_REDECLARE,
    SYMBOL_UNDECLARED,
    NOT_FUNCTION_NAME,
    TRY_TO_INIT_ARRAY,
    EXCESSIVE_ARRAY_DIM_DECLARATION,
    RETURN_ARRAY,
    VOID_VARIABLE,
    TYPEDEF_VOID_ARRAY,
    PARAMETER_TYPE_UNMATCH,
    TOO_FEW_ARGUMENTS,
    TOO_MANY_ARGUMENTS,
    RETURN_TYPE_UNMATCH,
    INCOMPATIBLE_ARRAY_DIMENSION,
    INCOMPATIBLE_ARRAY_DIMENSION_TOO_FEW,
    NOT_ASSIGNABLE,
    NOT_ARRAY,
    IS_TYPE_NOT_VARIABLE,
    IS_FUNCTION_NOT_VARIABLE,
    STRING_OPERATION,
    ARRAY_SIZE_NOT_INT,
    ARRAY_SIZE_NEGATIVE,
    ARRAY_SUBSCRIPT_NOT_INT,
    PASS_ARRAY_TO_SCALAR,
    PASS_SCALAR_TO_ARRAY
} ErrorMsgKind;

void printErrorMsgSpecial(AST_NODE* node1, DATA_TYPE type2, ErrorMsgKind errorMsgKind) // [Msg] 應該沒錯吧
{
    g_anyErrorOccur = 1;
    printf("Error found in line %d\n", node1->linenumber);
    switch(errorMsgKind){
        case PASS_ARRAY_TO_SCALAR: // [Ass] 3.c)
        case PASS_SCALAR_TO_ARRAY:
            printf("invalid conversion from \'%s\' to \'%s\'.\n", typeNameStrings[node1->dataType], typeNameStrings[type2]);
            break;
        default:
            printf("[DEBUG] Unhandled case in printErrorMsgSpecial()\n");
            break;
    }
}

void printErrorMsg(AST_NODE* node, ErrorMsgKind errorMsgKind) // [Msg] del(n) := "如果不用留的話，有n個地方要刪掉"
{
    g_anyErrorOccur = 1;
    printf("Error found in line %d\n", node->linenumber);
    char* name = node->semantic_value.identifierSemanticValue.identifierName;
    switch(errorMsgKind){
        case SYMBOL_IS_NOT_TYPE: // [Msg] 補足reserved word當成id的部分，例：a a = 5;。但是好像不可能會有，可能可以刪掉
            printf("ID \'%s\' is not a type name.\n",
                node->semantic_value.identifierSemanticValue.identifierName);
            break;
        case SYMBOL_REDECLARE: //[Ass] 1.b)
            printf("redeclaration of \'%s %s\'.\n", typeNameStrings[node->dataType], name);
            break;
        case SYMBOL_UNDECLARED: //[Ass] 1.a)
            printf("\'%s\' was not declared in this scope.\n", name);
            break;
        case NOT_FUNCTION_NAME: //[Ass] Extra 2
            printf("called object \'%s\' is not a function or function pointer.\n", name);
            break;
        case TRY_TO_INIT_ARRAY: // [Msg] del(1), reference: [Msg] 不行喔?
            printf("Cannot initialize array \'%s\'.\n",
                node->semantic_value.identifierSemanticValue.identifierName);
            break;
        case TOO_FEW_ARGUMENTS: //[Ass] 2.a)
            printf("too few arguments to function \'%s\'.\n", name);
            break;
        case TOO_MANY_ARGUMENTS: //[Ass] 2.a)
            printf("too many arguments to function \'%s\'.\n", name);
            break;
        case RETURN_TYPE_UNMATCH: // [Ass] 2.b) [Msg] 但是gcc好像允許這個功能!!?
            printf("no warning generated.\n");
            break;
        case NOT_ARRAY: // [Ass] 3.c), 不太確定
            printf("[DEBUG] ID \'%s\' is not array.\n",
                node->semantic_value.identifierSemanticValue.identifierName);
            break;
        case ARRAY_SIZE_NOT_INT: // [Ass] 3.b)
            printf("array subscript is not an integer\n");
            break;
        case ARRAY_SIZE_NEGATIVE: //[Ass] Extra 1
            printf("size of array \'%s\' is negative.\n",name);
            break;
        case ARRAY_SUBSCRIPT_NOT_INT: //[Ass] 3.b)
            printf("array subscript is not an integer.\n");
            break;
        case INCOMPATIBLE_ARRAY_DIMENSION_TOO_FEW: //[Ass] 3.a)
            printf("pointer references shouldn't appear in expressions.\n");
            break;
        case INCOMPATIBLE_ARRAY_DIMENSION: //[Ass] 3.a)
            printf("subscripted value is neither array nor pointer nor vector\n");
            break;
        case NOT_ASSIGNABLE: //[Ass] 3.a)
            printf("assignment to expression with array type");
            break;
        default:
            printf("[DEBUG] Unhandled case in printErrorMsg()\n");
            break;
    }
}

void semanticAnalysis(AST_NODE *root)
{
    processProgramNode(root);
}

DATA_TYPE getBiggerType(DATA_TYPE dataType1, DATA_TYPE dataType2)
{
    if(dataType1 == FLOAT_TYPE || dataType2 == FLOAT_TYPE) {
        return FLOAT_TYPE;
    } else {
        return INT_TYPE;
    }
}

/*****************************************************************************************************/

void processProgramNode(AST_NODE *programNode)
{
    AST_NODE *node = programNode->child;
    while(node){
        if(node->nodeType == VARIABLE_DECL_LIST_NODE)   // [Inf] global variable
            processGeneralNode(node);
        else                                            // [Inf] function declaration
            processDeclarationNode(node);
        
        if(node->dataType == ERROR_TYPE)
            programNode->dataType = ERROR_TYPE;
        
        node = node->rightSibling;
    }
}

void processDeclarationNode(AST_NODE* declarationNode)
{
    AST_NODE *firstChild = declarationNode->child;
    processTypeNode(firstChild); // [Inf] check firstChild is a type
    if(firstChild->dataType == ERROR_TYPE){
        declarationNode->dataType = ERROR_TYPE;
        return;
    }
    
    switch(declarationNode->semantic_value.declSemanticValue.kind){
        case VARIABLE_DECL:
            declareIdList(declarationNode, VARIABLE_ATTRIBUTE, 0);
            break;
        case TYPE_DECL:
            declareIdList(declarationNode, TYPE_ATTRIBUTE, 0);
            break;
        case FUNCTION_DECL:
            declareFunction(declarationNode);
            break;
        case FUNCTION_PARAMETER_DECL:
            declareIdList(declarationNode, VARIABLE_ATTRIBUTE, 1);
            break;
    }
}

void processTypeNode(AST_NODE* idNodeAsType)
{
    char* idName = idNodeAsType->semantic_value.identifierSemanticValue.identifierName;
    SymbolTableEntry* entry = retrieveSymbol(idName);
    if(!entry || (entry->attribute->attributeKind != TYPE_ATTRIBUTE)){
        printErrorMsg(idNodeAsType, SYMBOL_IS_NOT_TYPE); // [Inf] for example: i i = 5; 
        idNodeAsType->dataType = ERROR_TYPE;
    }
    else {
        idNodeAsType->semantic_value.identifierSemanticValue.symbolTableEntry = entry;
        TypeDescriptor *idNodeTypeDescr = entry->attribute->attr.typeDescriptor;
        
        switch(idNodeTypeDescr->kind){
            case SCALAR_TYPE_DESCRIPTOR:
                idNodeAsType->dataType = idNodeTypeDescr->properties.dataType;
                break;
            case ARRAY_TYPE_DESCRIPTOR:
                idNodeAsType->dataType = idNodeTypeDescr->properties.arrayProperties.elementType;
                break;
        }
    }
}

void declareIdList(AST_NODE* declarationNode, SymbolAttributeKind isVariableOrTypeAttribute, int ignoreArrayFirstDimSize) // [Msg] 改很少 相似度極高
{
    AST_NODE* typeNode = declarationNode->child;
    TypeDescriptor *typeNodeTypeDescr = typeNode->semantic_value.identifierSemanticValue.symbolTableEntry->attribute->attr.typeDescriptor;
    AST_NODE* IDNode = typeNode->rightSibling;
    while(IDNode){
        if(declaredLocally(IDNode->semantic_value.identifierSemanticValue.identifierName)){
            printErrorMsg(IDNode, SYMBOL_REDECLARE); // [Ass] 1.a) redeclared
            IDNode->dataType = ERROR_TYPE;
            declarationNode->dataType = ERROR_TYPE;
        }
        else{
            SymbolAttribute* newAttribute = (SymbolAttribute*)malloc(sizeof(SymbolAttribute));
            newAttribute->attributeKind = isVariableOrTypeAttribute;
            switch(IDNode->semantic_value.identifierSemanticValue.kind){
                case NORMAL_ID:
                    newAttribute->attr.typeDescriptor = typeNodeTypeDescr;
                    break;
                case ARRAY_ID:
                    newAttribute->attr.typeDescriptor = (TypeDescriptor*)malloc(sizeof(TypeDescriptor));
                    ArrayProperties newArrayIDProperties = newAttribute->attr.typeDescriptor->properties.arrayProperties;
                    processDeclDimList(IDNode, newAttribute->attr.typeDescriptor, ignoreArrayFirstDimSize);
                    if(IDNode->dataType == ERROR_TYPE){
                        free(newAttribute->attr.typeDescriptor);
                        declarationNode->dataType = ERROR_TYPE;
                    }
                    else if(typeNodeTypeDescr->kind == SCALAR_TYPE_DESCRIPTOR){
                        newArrayIDProperties.elementType = 
                            typeNodeTypeDescr->properties.dataType;
                    }
                    else if(typeNodeTypeDescr->kind == ARRAY_TYPE_DESCRIPTOR){
                        int typeArrayDimension = typeNodeTypeDescr->properties.arrayProperties.dimension;
                        int idArrayDimension = newArrayIDProperties.dimension;
                        newArrayIDProperties.elementType =\
                            typeNodeTypeDescr->properties.arrayProperties.elementType;
                        newArrayIDProperties.dimension =\
                            typeArrayDimension + idArrayDimension;
                        // [inf] to handle cases like: typedef int[5][10] FiveTenArray
                        for(int indexType = 0, indexId = idArrayDimension; indexId < idArrayDimension + typeArrayDimension; indexType++, indexId++){
                            newArrayIDProperties.sizeInEachDimension[indexId] = 
                                typeNodeTypeDescr->properties.arrayProperties.sizeInEachDimension[indexType];  
                        }
                    }
                    break;
                case WITH_INIT_ID:
                    if(typeNodeTypeDescr->kind == ARRAY_TYPE_DESCRIPTOR){
                        printErrorMsg(IDNode, TRY_TO_INIT_ARRAY); // [Msg] 不行喔? (ans: parser 裡好像就沒允許這種)
                        IDNode->dataType = ERROR_TYPE;
                        declarationNode->dataType = ERROR_TYPE;
                    }
                    else
                        newAttribute->attr.typeDescriptor = typeNodeTypeDescr;
                    break;
                default:
                    printf("[DEBUG] Unhandled case in declareIdList()\n");
                    IDNode->dataType = ERROR_TYPE;
                    declarationNode->dataType = ERROR_TYPE;
                    break;
            }

            if(IDNode->dataType == ERROR_TYPE){
                free(newAttribute);
                declarationNode->dataType = ERROR_TYPE;
            }
            else {
                IDNode->semantic_value.identifierSemanticValue.symbolTableEntry =
                    enterSymbol(IDNode->semantic_value.identifierSemanticValue.identifierName, newAttribute);
            }
        }
        IDNode = IDNode->rightSibling;
    }
}

void declareFunction(AST_NODE* declarationNode) // [Msg] 還沒改 腦袋卡住了 (ans: 有看過了)
{
    AST_NODE* returnTypeNode = declarationNode->child;
    AST_NODE* functionNameID = returnTypeNode->rightSibling;
    int errorFlag = 0;
    
    if(declaredLocally(functionNameID->semantic_value.identifierSemanticValue.identifierName))
    {
        printErrorMsg(functionNameID, SYMBOL_REDECLARE);
        functionNameID->dataType = ERROR_TYPE;
        errorFlag = 1;
    }
    
    SymbolAttribute* functionAttribute = (SymbolAttribute*)malloc(sizeof(SymbolAttribute));
    functionAttribute->attributeKind = FUNCTION_SIGNATURE;
    functionAttribute->attr.functionSignature = (FunctionSignature*)malloc(sizeof(FunctionSignature));
    functionAttribute->attr.functionSignature->returnType = returnTypeNode->dataType;
    functionAttribute->attr.functionSignature->parameterList = NULL;

    if(!errorFlag) // [msg] removed "symbol table entry inserted" flag
    {
        enterSymbol(functionNameID->semantic_value.identifierSemanticValue.identifierName, functionAttribute);
    }

    openScope();

    AST_NODE *parameterListNode = functionNameID->rightSibling;
    AST_NODE *curParameterNode = parameterListNode->child;
    int parametersCount = 0;
    if(curParameterNode)
    {
        parametersCount++;
        processDeclarationNode(curParameterNode);
        AST_NODE *parameterID = curParameterNode->child->rightSibling;
        if(curParameterNode->dataType == ERROR_TYPE)
        {
            errorFlag = 1;
        }
        else if(!errorFlag)
        {
            Parameter *parameter = (Parameter*)malloc(sizeof(Parameter));
            parameter->next = NULL;
            parameter->parameterName = parameterID->semantic_value.identifierSemanticValue.identifierName;
            parameter->type = parameterID->semantic_value.identifierSemanticValue.symbolTableEntry->attribute->attr.typeDescriptor;
            functionAttribute->attr.functionSignature->parameterList = parameter;
        }
        curParameterNode = curParameterNode->rightSibling;
    }

    Parameter *prevParameterPtr = functionAttribute->attr.functionSignature->parameterList;
    
    while(curParameterNode)
    {
        parametersCount++;
        processDeclarationNode(curParameterNode);
        AST_NODE *parameterID = curParameterNode->child->rightSibling;
        if(curParameterNode->dataType == ERROR_TYPE)
        {
            errorFlag = 1;
        }
        else if(!errorFlag)
        {
            Parameter *parameter = (Parameter*)malloc(sizeof(Parameter));
            parameter->next = NULL;
            parameter->parameterName = parameterID->semantic_value.identifierSemanticValue.identifierName;
            parameter->type = parameterID->semantic_value.identifierSemanticValue.symbolTableEntry->attribute->attr.typeDescriptor;
            prevParameterPtr->next = parameter;
            prevParameterPtr = parameter;
        }
        curParameterNode = curParameterNode->rightSibling;
    }
    functionAttribute->attr.functionSignature->parametersCount = parametersCount;

    if (errorFlag) // [msg] deleted condition: && (functionAttribute != NULL)
    {
        Parameter* curParameterPtr = functionAttribute->attr.functionSignature->parameterList;
        Parameter* nextParameterPtr = NULL; 
        while(curParameterPtr)
        {
            nextParameterPtr = curParameterPtr->next;
            free(curParameterPtr);
            curParameterPtr = nextParameterPtr;
        }
        free(functionAttribute->attr.functionSignature);
        free(functionAttribute);

        // [msg] moved error reporting and symbol table entry deletion to here
        removeSymbol(functionNameID->semantic_value.identifierSemanticValue.identifierName);
        declarationNode->dataType = ERROR_TYPE;
    } 
    else 
    { // [inf] process function contents
        AST_NODE *blockNode = parameterListNode->rightSibling;
        AST_NODE *curBlockContent = blockNode->child;
        while(curBlockContent)
        {
            processGeneralNode(curBlockContent);
            curBlockContent = curBlockContent->rightSibling;
        }
    }

    closeScope();
}

/*****************************************************************************************************/
void checkIfStmt(AST_NODE* ifNode)
{   
    // [msg] 已看過
    AST_NODE* checkExpr = ifNode->child;
    checkAssignOrExpr(checkExpr);
    AST_NODE* blockNode = checkExpr->rightSibling;
    processStmtNode(blockNode);
    AST_NODE* elseNode = blockNode->rightSibling;
    processStmtNode(elseNode);
}

void checkWhileStmt(AST_NODE* whileNode)
{
    // [msg] 已看過
    AST_NODE* checkExpr = whileNode->child;
    checkAssignOrExpr(checkExpr);
    AST_NODE* blockNode = checkExpr->rightSibling;
    processStmtNode(blockNode);
}

void checkForStmt(AST_NODE* forNode)
{
    // [msg] 已看過
    AST_NODE* initExpr = forNode->child;
    processGeneralNode(initExpr);
    AST_NODE* checkExpr = initExpr->rightSibling;
    processGeneralNode(checkExpr);
    AST_NODE* actExpr = checkExpr->rightSibling;
    processGeneralNode(actExpr);
    AST_NODE* blockNode = actExpr->rightSibling;
    processStmtNode(blockNode);
}

void checkAssignOrExpr(AST_NODE* assignOrExprRelatedNode)
{
    // [msg] 已看過
    if(assignOrExprRelatedNode->nodeType == STMT_NODE) {
        switch (assignOrExprRelatedNode->semantic_value.stmtSemanticValue.kind)
        {
            case ASSIGN_STMT:
                checkAssignmentStmt(assignOrExprRelatedNode);
                break;
            case FUNCTION_CALL_STMT:
                checkFunctionCall(assignOrExprRelatedNode);
                break;
        }
    }
    else {
        processExprRelatedNode(assignOrExprRelatedNode);
    }
}

void checkAssignmentStmt(AST_NODE* assignmentNode)
{
    AST_NODE* opL = assignmentNode->child, *opR = opL->rightSibling;
    processVariableLValue(opL);
    processExprRelatedNode(opR);
    if(opL->dataType == ERROR_TYPE || opR->dataType == ERROR_TYPE)
        assignmentNode->dataType = ERROR_TYPE;
    
    // [msg] type conversion 可以先不用做？
    assignmentNode->dataType = getBiggerType(opL->dataType, opR->dataType);
}

void checkWriteFunction(AST_NODE* functionCallNode) // [Inf] just for write()
// [msg] don't we have to check read() and fread() also?
{
    AST_NODE* funcIDNode = functionCallNode->child;
    AST_NODE* paraList = funcIDNode->rightSibling;
    processGeneralNode(paraList);
    
    int paraCount = 0;
    AST_NODE* paraPtr = paraList->child;
    while(paraPtr){
        paraCount++;
        if(paraPtr->dataType == ERROR_TYPE)
            functionCallNode->dataType = ERROR_TYPE;
        paraPtr = paraPtr->rightSibling;
    }
    
    if(paraCount > 1){
        printErrorMsg(funcIDNode, TOO_MANY_ARGUMENTS);
        functionCallNode->dataType = ERROR_TYPE;
    }
    else if(paraCount < 1){
        printErrorMsg(funcIDNode, TOO_FEW_ARGUMENTS);
        functionCallNode->dataType = ERROR_TYPE;
    }
    else
        functionCallNode->dataType = VOID_TYPE;
}

void checkReadFunction(AST_NODE* functionCallNode) {
    AST_NODE* funcIDNode = functionCallNode->child;
    AST_NODE* paraList = funcIDNode->rightSibling;
    AST_NODE* paraPtr = paraList->child;

    if (paraPtr->nodeType != NUL_NODE) {
        printErrorMsg(funcIDNode, TOO_MANY_ARGUMENTS);
        functionCallNode->dataType = ERROR_TYPE;
    } else {
        functionCallNode->dataType = VOID_TYPE;
    }
}

void checkFunctionCall(AST_NODE* functionCallNode)
{
    AST_NODE* funcIDNode = functionCallNode->child;
    const char *funcIDString = funcIDNode->semantic_value.identifierSemanticValue.identifierName;
    if(strcmp(funcIDString, "write") == 0){
        checkWriteFunction(functionCallNode);
        return;
    } else if (strcmp(funcIDString, "read") == 0 || strcmp(funcIDString, "fread") == 0){ 
        checkReadFunction(functionCallNode);
        return;
    }

    SymbolTableEntry* entry = retrieveSymbol(funcIDString);
    funcIDNode->semantic_value.identifierSemanticValue.symbolTableEntry = entry;

    if(entry == NULL){ // [Ass] 1.a)
        printErrorMsg(funcIDNode, SYMBOL_UNDECLARED);
        funcIDNode->dataType = ERROR_TYPE;
        functionCallNode->dataType = ERROR_TYPE;
        return;
    }
    else if(entry->attribute->attributeKind != FUNCTION_SIGNATURE){
        printErrorMsg(funcIDNode, NOT_FUNCTION_NAME); // [Ass] Extra 2
        funcIDNode->dataType = ERROR_TYPE;
        functionCallNode->dataType = ERROR_TYPE;
        return;
    }

    AST_NODE* paraList = funcIDNode->rightSibling;
    processGeneralNode(paraList);

    AST_NODE* paraPtr = paraList->child;
    Parameter* formalPara = entry->attribute->attr.functionSignature->parameterList;

    int paraError = 0;
    while(paraPtr && formalPara){ // [Ass] 3.c)
        if(paraPtr->dataType == ERROR_TYPE)
            paraError = 1;
        else{
            checkParameterPassing(formalPara, paraPtr);
            if(paraPtr->dataType == ERROR_TYPE)
                paraError = 1;
        }
        paraPtr = paraPtr->rightSibling;
        formalPara = formalPara->next;
    }
    
    if(paraError)
        functionCallNode->dataType = ERROR_TYPE;
    if(paraPtr != NULL){
        printErrorMsg(funcIDNode, TOO_MANY_ARGUMENTS);
        functionCallNode->dataType = ERROR_TYPE;
    }
    else if(formalPara != NULL){
        printErrorMsg(funcIDNode, TOO_FEW_ARGUMENTS);
        functionCallNode->dataType = ERROR_TYPE;
    }
    else{
        functionCallNode->dataType = entry->attribute->attr.functionSignature->returnType;
    }
}

void checkParameterPassing(Parameter* formalParameter, AST_NODE* actualParameter)
{    
    if(formalParameter->type->kind == SCALAR_TYPE_DESCRIPTOR && 
        (actualParameter->dataType == INT_PTR_TYPE || actualParameter->dataType == FLOAT_PTR_TYPE)){
        printErrorMsgSpecial(actualParameter, formalParameter->type->properties.dataType, PASS_ARRAY_TO_SCALAR);
        actualParameter->dataType = ERROR_TYPE;
    }
    // [msg] 這邊你原本照上面那條 (***_PTR_TYPE) 應該錯了
    else if(formalParameter->type->kind == ARRAY_TYPE_DESCRIPTOR && 
        !(actualParameter->dataType == INT_TYPE || actualParameter->dataType == FLOAT_TYPE )){
        printErrorMsgSpecial(actualParameter, formalParameter->type->properties.dataType, PASS_SCALAR_TO_ARRAY);
        actualParameter->dataType = ERROR_TYPE;
    }
    else if(actualParameter->dataType == CONST_STRING_TYPE){
        printErrorMsg(actualParameter, PARAMETER_TYPE_UNMATCH);
        actualParameter->dataType = ERROR_TYPE;
    }
}

void checkReturnStmt(AST_NODE* returnNode)
{
    AST_NODE* parentNode = returnNode->parent;
    DATA_TYPE returnType = NONE_TYPE;
    while(parentNode){
        if(parentNode->nodeType == DECLARATION_NODE)
            if(parentNode->semantic_value.declSemanticValue.kind == FUNCTION_DECL)
                returnType = parentNode->child->dataType;
            break;
        parentNode = parentNode->parent;
    }

    int errorFlag = 0;
    if(returnNode->child->nodeType == NUL_NODE) //[Inf] return;
        if(returnType != VOID_TYPE)
            errorFlag = 1;
    else{
        processExprRelatedNode(returnNode->child); 
        if(returnType != returnNode->child->dataType) // [Msg] 這邊是不是不管怎樣都應該要有 RETURN_TYPE_UNMATCH
            if (!((returnType == FLOAT_TYPE && returnNode->child->dataType == INT_TYPE) || (returnType == INT_TYPE && returnNode->child->dataType == FLOAT_TYPE)))
                errorFlag = 1;
    }

    if(errorFlag){
        printErrorMsg(returnNode, RETURN_TYPE_UNMATCH);
        returnNode->dataType = ERROR_TYPE;
    }
    else
        returnNode->dataType = returnType;
}

/*****************************************************************************************************/

void getExprOrConstValue(AST_NODE* exprOrConstNode, int* iValue, float* fValue)
{
    if(exprOrConstNode->nodeType == CONST_VALUE_NODE) {
        if(exprOrConstNode->dataType == INT_TYPE) {
            if(fValue)
                *fValue = exprOrConstNode->semantic_value.const1->const_u.intval;
            else
                *iValue = exprOrConstNode->semantic_value.const1->const_u.intval;
        }
        else
            *fValue = exprOrConstNode->semantic_value.const1->const_u.fval;
    }
    else {
        if(exprOrConstNode->dataType == INT_TYPE) {
            if(fValue)
                *fValue = exprOrConstNode->semantic_value.exprSemanticValue.constEvalValue.iValue;
            else
                *iValue = exprOrConstNode->semantic_value.exprSemanticValue.constEvalValue.iValue;
        }
        else
            *fValue = exprOrConstNode->semantic_value.exprSemanticValue.constEvalValue.fValue;
    }
}

void evaluateExprValue(AST_NODE* exprNode)
// [msg] 這邊多改了：避免傳 NULL ptr 給 getExprOrConstValue() 以免出事
{
    if(exprNode->semantic_value.exprSemanticValue.kind == BINARY_OPERATION){
        AST_NODE *opL = exprNode->child;
        AST_NODE *opR = opL->rightSibling;
        if(opL->dataType == INT_TYPE && opR->dataType == INT_TYPE){
            int valL = 0, valR = 0, result;
            float fvalL, fvalR;
            getExprOrConstValue(opL, &valL, &fvalL);
            getExprOrConstValue(opR, &valR, &fvalR);
            exprNode->dataType = INT_TYPE;
            switch(exprNode->semantic_value.exprSemanticValue.op.binaryOp){
                case BINARY_OP_ADD: result = valL + valR;  break;
                case BINARY_OP_SUB: result = valL - valR;  break;
                case BINARY_OP_MUL: result = valL * valR;  break;
                case BINARY_OP_DIV: result = valL / valR;  break;
                case BINARY_OP_EQ:  result = valL == valR; break;
                case BINARY_OP_GE:  result = valL >= valR; break;
                case BINARY_OP_LE:  result = valL <= valR; break;
                case BINARY_OP_NE:  result = valL != valR; break;
                case BINARY_OP_GT:  result = valL > valR;  break;
                case BINARY_OP_LT:  result = valL < valR;  break;
                case BINARY_OP_AND: result = valL && valR; break;
                case BINARY_OP_OR:  result = valL || valR; break;
                default:
                    printf("[DEBUG] Unhandled case in evaluateExprValue()\n");
                    break;
            }
            exprNode->semantic_value.exprSemanticValue.constEvalValue.iValue = result;
        }
        else
        {
            float fvalL = 0, fvalR = 0, result;
            int valL, valR;
            getExprOrConstValue(opL, &valL, &fvalL);
            getExprOrConstValue(opR, &valR, &fvalR);
            exprNode->dataType = FLOAT_TYPE;
            switch(exprNode->semantic_value.exprSemanticValue.op.binaryOp){
                case BINARY_OP_ADD: result = fvalL + fvalR;  break;
                case BINARY_OP_SUB: result = fvalL - fvalR;  break;
                case BINARY_OP_MUL: result = fvalL * fvalR;  break;
                case BINARY_OP_DIV: result = fvalL / fvalR;  break;
                case BINARY_OP_EQ:  result = fvalL == fvalR; break;
                case BINARY_OP_GE:  result = fvalL >= fvalR; break;
                case BINARY_OP_LE:  result = fvalL <= fvalR; break;
                case BINARY_OP_NE:  result = fvalL != fvalR; break;
                case BINARY_OP_GT:  result = fvalL > fvalR;  break;
                case BINARY_OP_LT:  result = fvalL < fvalR;  break;
                case BINARY_OP_AND: result = fvalL && fvalR; break;
                case BINARY_OP_OR:  result = fvalL || fvalR; break;
                default:
                    printf("[DEBUG] Unhandled case in evaluateExprValue()\n");
                    break;
            }
            exprNode->semantic_value.exprSemanticValue.constEvalValue.fValue = result;
        }
    }
    else{
        AST_NODE* operand = exprNode->child;
        if(operand->dataType == INT_TYPE){
            int result = 0;
            float fholder;
            getExprOrConstValue(operand, &result, &fholder);
            exprNode->dataType = INT_TYPE;
            switch(exprNode->semantic_value.exprSemanticValue.op.unaryOp){
                case UNARY_OP_POSITIVE: break;
                case UNARY_OP_NEGATIVE: result = -result; break;
                case UNARY_OP_LOGICAL_NEGATION: result = !result; break;
                default:
                    printf("[DEBUG] Unhandled case in evaluateExprValue()\n");
                    break;
            }
            exprNode->semantic_value.exprSemanticValue.constEvalValue.iValue = result;
        }
        else
        {
            float result = 0;
            int iholder;
            getExprOrConstValue(operand, &iholder, &result);
            exprNode->dataType = FLOAT_TYPE;
            switch(exprNode->semantic_value.exprSemanticValue.op.unaryOp){
                case UNARY_OP_POSITIVE: break;
                case UNARY_OP_NEGATIVE: result = -result; break;
                case UNARY_OP_LOGICAL_NEGATION: result = !result; break;
                default:
                    printf("[DEBUG] Unhandled case in evaluateExprValue()\n");
                    break;
            }
            exprNode->semantic_value.exprSemanticValue.constEvalValue.fValue = result;
        }
    }
}

void processExprNode(AST_NODE* exprNode)
{
    if(exprNode->semantic_value.exprSemanticValue.kind == BINARY_OPERATION){
        AST_NODE *opL = exprNode->child;
        AST_NODE *opR = opL->rightSibling;
        processExprRelatedNode(opL);
        processExprRelatedNode(opR);
        if(opL->dataType == ERROR_TYPE || opR->dataType == ERROR_TYPE)
            exprNode->dataType = ERROR_TYPE;
        
        if(exprNode->dataType != ERROR_TYPE)
            exprNode->dataType = getBiggerType(opL->dataType, opR->dataType);

        if((exprNode->dataType != ERROR_TYPE) &&
           (opL->nodeType == CONST_VALUE_NODE || (opL->nodeType == EXPR_NODE && opL->semantic_value.exprSemanticValue.isConstEval)) &&
           (opR->nodeType == CONST_VALUE_NODE || (opR->nodeType == EXPR_NODE && opR->semantic_value.exprSemanticValue.isConstEval))){
            evaluateExprValue(exprNode);
            exprNode->semantic_value.exprSemanticValue.isConstEval = 1;
        }
    }
    else{ // [Inf] unary op
        AST_NODE* operand = exprNode->child;
        processExprRelatedNode(operand);
        
        if(operand->dataType == ERROR_TYPE)
            exprNode->dataType = ERROR_TYPE;
        else
            exprNode->dataType = operand->dataType;
        
        if((exprNode->dataType != ERROR_TYPE) &&
           (operand->nodeType == CONST_VALUE_NODE || (operand->nodeType == EXPR_NODE && operand->semantic_value.exprSemanticValue.isConstEval))){
            evaluateExprValue(exprNode);
            exprNode->semantic_value.exprSemanticValue.isConstEval = 1;
        }

    }
}

void processExprRelatedNode(AST_NODE* exprRelatedNode)
{
    // [msg] 看過了
    switch(exprRelatedNode->nodeType){
        case EXPR_NODE:
            processExprNode(exprRelatedNode); 
            break;
        case STMT_NODE:
            checkFunctionCall(exprRelatedNode); 
            break;
        case IDENTIFIER_NODE:
            processVariableRValue(exprRelatedNode); 
            break;
        case CONST_VALUE_NODE:
            processConstValueNode(exprRelatedNode); 
            break;
        default:
            printf("[DEBUG] Unhandle case in processExprRelatedNode()\n");
            exprRelatedNode->dataType = ERROR_TYPE;
    }
}

void processVariableLValue(AST_NODE* idNode)// [Msg] 改很少 相似度極高 // [msg] 我大改過 加了 3.a)
{
    IdentifierSemanticValue IDSemanticValue = idNode->semantic_value.identifierSemanticValue;
    SymbolTableEntry *entry = retrieveSymbol(IDSemanticValue.identifierName);
    if(!entry){
        printErrorMsg(idNode, SYMBOL_UNDECLARED); // [Ass] 1.a)
        idNode->dataType = ERROR_TYPE;
        return;
    }
    IDSemanticValue.symbolTableEntry = entry;
   
    TypeDescriptor *varTypeDescr = IDSemanticValue.symbolTableEntry->attribute->attr.typeDescriptor;        
    if(IDSemanticValue.kind == NORMAL_ID){
        idNode->dataType = varTypeDescr->properties.dataType;
    }
    else if(IDSemanticValue.kind == ARRAY_ID){
        if(varTypeDescr->kind == SCALAR_TYPE_DESCRIPTOR){ // [Inf] e.g., int a; a[2][4] = 5;
            printErrorMsg(idNode, NOT_ARRAY);
            idNode->dataType = ERROR_TYPE;
        }
        int dimension = 0;
        AST_NODE *dimPtr = idNode->child;
        while(dimPtr){
            dimension++;
            processExprRelatedNode(dimPtr);
            if(dimPtr->dataType == ERROR_TYPE)
                idNode->dataType = ERROR_TYPE;
            else if(dimPtr->dataType == FLOAT_TYPE){ // [Ass] 3.b)
                printErrorMsg(idNode, ARRAY_SUBSCRIPT_NOT_INT);
                idNode->dataType = ERROR_TYPE;
            }
            dimPtr = dimPtr->rightSibling;
        }
        // [Ass/Msg] implement checkpoint 3.a) here
        int correctDims = entry->attribute->attr.typeDescriptor->properties.arrayProperties.dimension;
        if (dimension < correctDims) {
            printErrorMsg(idNode, NOT_ASSIGNABLE); // [Ass] 3.a)
            idNode->dataType = ERROR_TYPE;
        }
        else if (dimension > correctDims) {
            printErrorMsg(idNode, INCOMPATIBLE_ARRAY_DIMENSION); // [Ass] 3.a)
            idNode->dataType = ERROR_TYPE;
        }
        else {
            idNode->dataType = varTypeDescr->properties.arrayProperties.elementType;
        }
    }
}

void processVariableRValue(AST_NODE* idNode)// [Msg] 改很少 相似度極高 // [msg] 我大改過 加了 3.a)
{   
    IdentifierSemanticValue IDSemanticValue = idNode->semantic_value.identifierSemanticValue;
    SymbolTableEntry *entry = retrieveSymbol(IDSemanticValue.identifierName);
    
    IDSemanticValue.symbolTableEntry = entry;
    if(!entry) {
        printErrorMsg(idNode, SYMBOL_UNDECLARED);
        idNode->dataType = ERROR_TYPE;
        return;
    }

    TypeDescriptor *typeDescriptor = IDSemanticValue.symbolTableEntry->attribute->attr.typeDescriptor;
        
    if(IDSemanticValue.kind == NORMAL_ID){ // [msg] pointer arithmetic 應該不用支援，算做 error？ (不過後面也沒)
        if(typeDescriptor->kind == ARRAY_TYPE_DESCRIPTOR) {
            printErrorMsg(idNode, INCOMPATIBLE_ARRAY_DIMENSION_TOO_FEW);
            idNode->dataType = ERROR_TYPE;
            // if(typeDescriptor->properties.arrayProperties.elementType == INT_TYPE)
            //     idNode->dataType = INT_PTR_TYPE;
            // else
            //     idNode->dataType = FLOAT_PTR_TYPE;
        }
        else {
            idNode->dataType = typeDescriptor->properties.dataType;
        }
    }
    else if(IDSemanticValue.kind == ARRAY_ID){
        if(typeDescriptor->kind == SCALAR_TYPE_DESCRIPTOR){
            printErrorMsg(idNode, NOT_ARRAY);
            idNode->dataType = ERROR_TYPE;
        }
        else{
            int dimension = 0;
            AST_NODE *dimPtr = idNode->child;
            while(dimPtr){
                dimension++;
                processExprRelatedNode(dimPtr);
                if(dimPtr->dataType == ERROR_TYPE)
                    idNode->dataType = ERROR_TYPE;
                else if(dimPtr->dataType == FLOAT_TYPE){
                    printErrorMsg(idNode, ARRAY_SUBSCRIPT_NOT_INT);
                    idNode->dataType = ERROR_TYPE;
                }
                dimPtr = dimPtr->rightSibling;
            }
            if(idNode->dataType != ERROR_TYPE){
                int correctDims = typeDescriptor->properties.arrayProperties.dimension;
                if (dimension < correctDims) {
                    printErrorMsg(idNode, INCOMPATIBLE_ARRAY_DIMENSION_TOO_FEW); // [Ass] 3.a)
                    idNode->dataType = ERROR_TYPE;
                }
                else if (dimension > correctDims) {
                    printErrorMsg(idNode, INCOMPATIBLE_ARRAY_DIMENSION); // [Ass] 3.a)
                    idNode->dataType = ERROR_TYPE;
                }
                else {
                    idNode->dataType = typeDescriptor->properties.arrayProperties.elementType;
                }
            }
        }
    }
}

void processConstValueNode(AST_NODE* constValueNode)
{
    switch(constValueNode->semantic_value.const1->const_type){
        case INTEGERC:
            constValueNode->dataType = INT_TYPE;
            constValueNode->semantic_value.exprSemanticValue.constEvalValue.iValue =
                constValueNode->semantic_value.const1->const_u.intval;
            break;
        case FLOATC:
            constValueNode->dataType = FLOAT_TYPE;
            constValueNode->semantic_value.exprSemanticValue.constEvalValue.fValue =
                constValueNode->semantic_value.const1->const_u.fval;
            break;
        case STRINGC:
            constValueNode->dataType = CONST_STRING_TYPE;
            break;
        default:
            printf("[DEBUG] Unhandle case in processConstValueNode()\n");
            constValueNode->dataType = ERROR_TYPE;
            break;
    }
}

void processBlockNode(AST_NODE* blockNode)
{
    openScope();
    AST_NODE *tmp = blockNode->child;
    while(tmp){
        processGeneralNode(tmp);
        tmp = tmp->rightSibling;
    }
    closeScope();
}

void processStmtNode(AST_NODE* stmtNode)
{
    if(stmtNode->nodeType == NUL_NODE)
        return;
    else if(stmtNode->nodeType == BLOCK_NODE)
        processBlockNode(stmtNode);
    else
        switch(stmtNode->semantic_value.stmtSemanticValue.kind){
            case WHILE_STMT:
                checkWhileStmt(stmtNode); break;
            case FOR_STMT:
                checkForStmt(stmtNode); break;
            case ASSIGN_STMT:
                checkAssignmentStmt(stmtNode); break;
            case IF_STMT:
                checkIfStmt(stmtNode); break;
            case FUNCTION_CALL_STMT:
                checkFunctionCall(stmtNode); break;
            case RETURN_STMT:
                checkReturnStmt(stmtNode); break;
            default:
                printf("Unhandle case in void processStmtNode(AST_NODE* stmtNode)\n");
                stmtNode->dataType = ERROR_TYPE;
                break;
        }
}

void processGeneralNode(AST_NODE *node) // [Msg] 改很多 有可會crash
{
    AST_NODE *ptr = node->child;
    while(ptr){
        switch (node->nodeType){
            case VARIABLE_DECL_LIST_NODE:
                processDeclarationNode(ptr);
                break;
            case STMT_LIST_NODE:
                processStmtNode(ptr);
                break;
            case NONEMPTY_ASSIGN_EXPR_LIST_NODE:
                checkAssignOrExpr(ptr);
                break;
            case NONEMPTY_RELOP_EXPR_LIST_NODE:
                processExprRelatedNode(ptr);
                break;
            case NUL_NODE:
                break;
            default:
                printf("[DEBUG] Unhandle case in processGeneralNode()\n");
                node->dataType = ERROR_TYPE;
                break;
        }
        if(ptr->dataType == ERROR_TYPE)
            node->dataType = ERROR_TYPE;
        ptr = ptr->rightSibling;
    }
}

void processDeclDimList(AST_NODE* idNode, TypeDescriptor* typeDescriptor, int ignoreFirstDimSize)
{
    typeDescriptor->kind = ARRAY_TYPE_DESCRIPTOR;
    AST_NODE* dimList = idNode->child;
    AST_NODE* curDimNode = dimList;
    int dimension = 0;
    ArrayProperties newArrayProperties = typeDescriptor->properties.arrayProperties;
    if(ignoreFirstDimSize && curDimNode->nodeType == NUL_NODE){ // [Inf] int arr[][10], ...
        newArrayProperties.sizeInEachDimension[dimension++] = 0;
        curDimNode = curDimNode->rightSibling;
    }
    while(curDimNode){
        processExprRelatedNode(curDimNode);
        if(curDimNode->dataType == ERROR_TYPE)
            idNode->dataType = ERROR_TYPE;
        else if(curDimNode->dataType == FLOAT_TYPE){ // [Ass] 3.b)
            printErrorMsg(curDimNode->parent, ARRAY_SIZE_NOT_INT);
            idNode->dataType = ERROR_TYPE;
        }
        else if(curDimNode->semantic_value.exprSemanticValue.isConstEval &&
            curDimNode->semantic_value.exprSemanticValue.constEvalValue.iValue < 0){ // [Ass] Extra 1
            printErrorMsg(curDimNode->parent, ARRAY_SIZE_NEGATIVE);
            idNode->dataType = ERROR_TYPE;
        }
        else
            newArrayProperties.sizeInEachDimension[dimension] = 
                curDimNode->semantic_value.exprSemanticValue.constEvalValue.iValue;

        dimension++;
        curDimNode = curDimNode->rightSibling;
    }

    newArrayProperties.dimension = dimension;
}
