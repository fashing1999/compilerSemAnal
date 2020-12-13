#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "header.h"
#include "symbolTable.h"
// This file is for reference only, you are not required to follow the implementation. //
// You only need to check for errors stated in the hw4 document. //
int g_anyErrorOccur = 0;

const char **typeNameStrings = {
    'int', 'float', 'void', 'int *', 'float *', 'char *', 'unnamed', 'unnamed'
};

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
        case EXCESSIVE_ARRAY_DIM_DECLARATION: // [Msg] del(2)
            printf("ID \'%s\' array dimension cannot be greater than %d\n",
                node->semantic_value.identifierSemanticValue.identifierName,
                MAX_ARRAY_DIMENSION);
            break;
        case RETURN_ARRAY: // [Msg] del(1)
            printf("Function \'%s\' cannot return array.\n",
                node->rightSibling->semantic_value.identifierSemanticValue.identifierName);
            break;
        case VOID_VARIABLE: // [Msg] del(1)
            printf("Type \'%s\' cannot be a variable's type.\n",
                node->semantic_value.identifierSemanticValue.identifierName);
            break;
        case TYPEDEF_VOID_ARRAY: // [Msg] del(1)
            printf("Declaration of \'%s\' as array of voids.\n",
                node->semantic_value.identifierSemanticValue.identifierName);
            break;
        case PARAMETER_TYPE_UNMATCH: // [Msg] del(2)
            printf("Parameter is incompatible with parameter type.\n");
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
        case INCOMPATIBLE_ARRAY_DIMENSION: // [Msg] del(7)
            printf("Incompatible array dimensions.\n");
            break;
        case NOT_ASSIGNABLE: // [Msg] del(0), 沒呼叫過
            printf("ID \'%s\' is not assignable.\n",
                node->semantic_value.identifierSemanticValue.identifierName);
            break;
        case NOT_ARRAY: // [Msg] del(2), 好像是正常不會發生的情況
            printf("[DEBUG] ID \'%s\' is not array.\n",
                node->semantic_value.identifierSemanticValue.identifierName);
            break;
        case IS_TYPE_NOT_VARIABLE: // [Msg] del(2)
            printf("ID \'%s\' is a type, not a variable's name.\n",
                node->semantic_value.identifierSemanticValue.identifierName);
            break;
        case IS_FUNCTION_NOT_VARIABLE: // [Msg] del(1)
            printf("ID \'%s\' is a function, not a variable's name.\n",
                node->semantic_value.identifierSemanticValue.identifierName);
            break;
        case STRING_OPERATION: // [Msg] del(3)
            printf("String operation is unsupported.\n");
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
    else{
        idNodeAsType->semantic_value.identifierSemanticValue.symbolTableEntry = entry;
        
        switch(entry->attribute->attr.typeDescriptor->kind){
            case SCALAR_TYPE_DESCRIPTOR:
                idNodeAsType->dataType = entry->attribute->attr.typeDescriptor->properties.dataType;
                break;
            case ARRAY_TYPE_DESCRIPTOR:
                idNodeAsType->dataType = entry->attribute->attr.typeDescriptor->properties.arrayProperties.elementType;
                break;
        }
    }
}

void declareIdList(AST_NODE* declarationNode, SymbolAttributeKind isVariableOrTypeAttribute, int ignoreArrayFirstDimSize) // [Msg] 改很少 相似度極高
{
    AST_NODE* typeNode = declarationNode->child;
    TypeDescriptor *typeDescriptorOfTypeNode = typeNode->semantic_value.identifierSemanticValue.symbolTableEntry->attribute->attr.typeDescriptor;
    if((isVariableOrTypeAttribute == VARIABLE_ATTRIBUTE) && 
       (typeDescriptorOfTypeNode->kind == SCALAR_TYPE_DESCRIPTOR) &&
       (typeDescriptorOfTypeNode->properties.dataType == VOID_TYPE)){
        printErrorMsg(typeNode, VOID_VARIABLE);
        typeNode->dataType = ERROR_TYPE;
        return;
    }

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
                    newAttribute->attr.typeDescriptor = typeNode->semantic_value.identifierSemanticValue.symbolTableEntry->attribute->attr.typeDescriptor;
                    break;
                case ARRAY_ID:
                    if((isVariableOrTypeAttribute == TYPE_ATTRIBUTE) && 
                    (typeDescriptorOfTypeNode->kind == SCALAR_TYPE_DESCRIPTOR) &&
                    (typeDescriptorOfTypeNode->properties.dataType == VOID_TYPE)){
                        printErrorMsg(IDNode, TYPEDEF_VOID_ARRAY);
                        IDNode->dataType = ERROR_TYPE;
                        declarationNode->dataType = ERROR_TYPE;
                        break;
                    }

                    newAttribute->attr.typeDescriptor = (TypeDescriptor*)malloc(sizeof(TypeDescriptor));
                    processDeclDimList(IDNode, newAttribute->attr.typeDescriptor, ignoreArrayFirstDimSize);
                    if(IDNode->dataType == ERROR_TYPE){
                        free(newAttribute->attr.typeDescriptor);
                        declarationNode->dataType = ERROR_TYPE;
                    }
                    else if(typeNode->semantic_value.identifierSemanticValue.symbolTableEntry->attribute->attr.typeDescriptor->kind == SCALAR_TYPE_DESCRIPTOR){
                        newAttribute->attr.typeDescriptor->properties.arrayProperties.elementType = 
                            typeNode->semantic_value.identifierSemanticValue.symbolTableEntry->attribute->attr.typeDescriptor->properties.dataType;
                    }
                    else if(typeNode->semantic_value.identifierSemanticValue.symbolTableEntry->attribute->attr.typeDescriptor->kind == ARRAY_TYPE_DESCRIPTOR){
                        int typeArrayDimension = typeNode->semantic_value.identifierSemanticValue.symbolTableEntry->attribute->attr.typeDescriptor->properties.arrayProperties.dimension;
                        int idArrayDimension = newAttribute->attr.typeDescriptor->properties.arrayProperties.dimension;
                        if((typeArrayDimension + idArrayDimension) > MAX_ARRAY_DIMENSION){
                            printErrorMsg(IDNode, EXCESSIVE_ARRAY_DIM_DECLARATION);
                            free(newAttribute->attr.typeDescriptor);
                            IDNode->dataType = ERROR_TYPE;
                            declarationNode->dataType = ERROR_TYPE;
                        }
                        else{
                            newAttribute->attr.typeDescriptor->properties.arrayProperties.elementType = 
                                typeNode->semantic_value.identifierSemanticValue.symbolTableEntry->attribute->attr.typeDescriptor->properties.arrayProperties.elementType;
                            newAttribute->attr.typeDescriptor->properties.arrayProperties.dimension = 
                                typeArrayDimension + idArrayDimension;
                            for(int indexType = 0, indexId = idArrayDimension; indexId < idArrayDimension + typeArrayDimension; ++indexType, ++indexId){
                                newAttribute->attr.typeDescriptor->properties.arrayProperties.sizeInEachDimension[indexId] = 
                                    typeNode->semantic_value.identifierSemanticValue.symbolTableEntry->attribute->attr.typeDescriptor->properties.arrayProperties.sizeInEachDimension[indexType];
                            }
                        }
                    }
                    break;
                case WITH_INIT_ID:
                    if(typeNode->semantic_value.identifierSemanticValue.symbolTableEntry->attribute->attr.typeDescriptor->kind == ARRAY_TYPE_DESCRIPTOR){
                        printErrorMsg(IDNode, TRY_TO_INIT_ARRAY); // [Msg] 不行喔?
                        IDNode->dataType = ERROR_TYPE;
                        declarationNode->dataType = ERROR_TYPE;
                    }
                    else
                        newAttribute->attr.typeDescriptor = typeNode->semantic_value.identifierSemanticValue.symbolTableEntry->attribute->attr.typeDescriptor;
                    break;
                default:
                    printf("[DEBUG] Unhandle case in declareIdList()\n");
                    IDNode->dataType = ERROR_TYPE;
                    declarationNode->dataType = ERROR_TYPE;
                    break;
            }
            if(IDNode->dataType == ERROR_TYPE){
                free(newAttribute);
                declarationNode->dataType = ERROR_TYPE;
            }
            else{
                IDNode->semantic_value.identifierSemanticValue.symbolTableEntry =
                    enterSymbol(IDNode->semantic_value.identifierSemanticValue.identifierName, newAttribute);
            }
        }
        IDNode = IDNode->rightSibling;
    }
}

void declareFunction(AST_NODE* declarationNode) // [Msg] 還沒改 卡住了
{
    AST_NODE* returnTypeNode = declarationNode->child;

    int errorOccur = 0;
    if(returnTypeNode->semantic_value.identifierSemanticValue.symbolTableEntry->attribute->attr.typeDescriptor->kind == ARRAY_TYPE_DESCRIPTOR)
    {
        printErrorMsg(returnTypeNode, RETURN_ARRAY);
        returnTypeNode->dataType = ERROR_TYPE;
        errorOccur = 1;
    }

    AST_NODE* functionNameID = returnTypeNode->rightSibling;
    if(declaredLocally(functionNameID->semantic_value.identifierSemanticValue.identifierName))
    {
        printErrorMsg(functionNameID, SYMBOL_REDECLARE);
        functionNameID->dataType = ERROR_TYPE;
        errorOccur = 1;
    }
    
    SymbolAttribute* attribute = NULL;
    attribute = (SymbolAttribute*)malloc(sizeof(SymbolAttribute));
    attribute->attributeKind = FUNCTION_SIGNATURE;
    attribute->attr.functionSignature = (FunctionSignature*)malloc(sizeof(FunctionSignature));
    attribute->attr.functionSignature->returnType = returnTypeNode->dataType;
    attribute->attr.functionSignature->parameterList = NULL;

    int enterFunctionNameToSymbolTable = 0;
    if(!errorOccur)
    {
        enterSymbol(functionNameID->semantic_value.identifierSemanticValue.identifierName, attribute);
        enterFunctionNameToSymbolTable = 1;
    }

    openScope();

    AST_NODE *parameterListNode = functionNameID->rightSibling;
    AST_NODE *traverseParameter = parameterListNode->child;
    int parametersCount = 0;
    if(traverseParameter)
    {
        ++parametersCount;
        processDeclarationNode(traverseParameter);
        AST_NODE *parameterID = traverseParameter->child->rightSibling;
        if(traverseParameter->dataType == ERROR_TYPE)
        {
            errorOccur = 1;
        }
        else if(!errorOccur)
        {
            Parameter *parameter = (Parameter*)malloc(sizeof(Parameter));
            parameter->next = NULL;
            parameter->parameterName = parameterID->semantic_value.identifierSemanticValue.identifierName;
            parameter->type = parameterID->semantic_value.identifierSemanticValue.symbolTableEntry->attribute->attr.typeDescriptor;
            attribute->attr.functionSignature->parameterList = parameter;
        }
        traverseParameter = traverseParameter->rightSibling;
    }

    Parameter *parameterListTail = attribute->attr.functionSignature->parameterList;
    
    while(traverseParameter)
    {
        ++parametersCount;
        processDeclarationNode(traverseParameter);
        AST_NODE *parameterID = traverseParameter->child->rightSibling;
        if(traverseParameter->dataType == ERROR_TYPE)
        {
            errorOccur = 1;
        }
        else if(!errorOccur)
        {
            Parameter *parameter = (Parameter*)malloc(sizeof(Parameter));
            parameter->next = NULL;
            parameter->parameterName = parameterID->semantic_value.identifierSemanticValue.identifierName;
            parameter->type = parameterID->semantic_value.identifierSemanticValue.symbolTableEntry->attribute->attr.typeDescriptor;
            parameterListTail->next = parameter;
            parameterListTail = parameter;
        }
        traverseParameter = traverseParameter->rightSibling;
    }
    attribute->attr.functionSignature->parametersCount = parametersCount;

    if(errorOccur && (attribute != NULL))
    {
        Parameter* traverseParameter = attribute->attr.functionSignature->parameterList;
        Parameter* next = NULL; 
        while(traverseParameter)
        {
            next = traverseParameter->next;
            free(traverseParameter);
            traverseParameter = next;
        }
        free(attribute->attr.functionSignature);
        free(attribute);
    }

    if(!errorOccur)
    {
        AST_NODE *blockNode = parameterListNode->rightSibling;
        AST_NODE *traverseListNode = blockNode->child;
        while(traverseListNode)
        {
            processGeneralNode(traverseListNode);
            traverseListNode = traverseListNode->rightSibling;
        }
    }

    closeScope();

    if(errorOccur && enterFunctionNameToSymbolTable)
    {
        declarationNode->dataType = ERROR_TYPE;
        if(enterFunctionNameToSymbolTable)
        {
            removeSymbol(functionNameID->semantic_value.identifierSemanticValue.identifierName);
        }
    }
}

/*****************************************************************************************************/

void checkIfStmt(AST_NODE* ifNode)
{
    AST_NODE* checkExpr = ifNode->child;
    checkAssignOrExpr(checkExpr);
    AST_NODE* blockNode = checkExpr->rightSibling;
    processStmtNode(blockNode);
    AST_NODE* elseNode = blockNode->rightSibling;
    processStmtNode(elseNode);
}

void checkWhileStmt(AST_NODE* whileNode)
{
    AST_NODE* checkExpr = whileNode->child;
    checkAssignOrExpr(checkExpr);
    AST_NODE* bloockNode = checkExpr->rightSibling;
    processStmtNode(bloockNode);
}

void checkForStmt(AST_NODE* forNode)
{
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
    if(assignOrExprRelatedNode->nodeType == STMT_NODE)
        if(assignOrExprRelatedNode->semantic_value.stmtSemanticValue.kind == ASSIGN_STMT)
            checkAssignmentStmt(assignOrExprRelatedNode);
        else if(assignOrExprRelatedNode->semantic_value.stmtSemanticValue.kind == FUNCTION_CALL_STMT)
            checkFunctionCall(assignOrExprRelatedNode);
    else
        processExprRelatedNode(assignOrExprRelatedNode);
}

void checkAssignmentStmt(AST_NODE* assignmentNode)
{
    AST_NODE* opL = assignmentNode->child, *opR = opL->rightSibling;
    processVariableLValue(opL);
    processExprRelatedNode(opR);
    if(opL->dataType == ERROR_TYPE || opR->dataType == ERROR_TYPE)
        assignmentNode->dataType = ERROR_TYPE;
    
    if(opR->dataType == INT_PTR_TYPE || opR->dataType == FLOAT_PTR_TYPE){ // [Msg] 這邊不能是else if
        printErrorMsg(opR, INCOMPATIBLE_ARRAY_DIMENSION);
        assignmentNode->dataType = ERROR_TYPE;
    }
    else if(opR->dataType == CONST_STRING_TYPE){
        printErrorMsg(opR, STRING_OPERATION);
        assignmentNode->dataType = ERROR_TYPE;
    }
    else
        assignmentNode->dataType = getBiggerType(opL->dataType, opR->dataType);
}

void checkWriteFunction(AST_NODE* functionCallNode) // [Inf] just for write()
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
        else if(paraPtr->dataType != INT_TYPE &&
                paraPtr->dataType != FLOAT_TYPE &&
                paraPtr->dataType != CONST_STRING_TYPE){
            printErrorMsg(paraPtr, PARAMETER_TYPE_UNMATCH);
            functionCallNode->dataType = ERROR_TYPE;
        }
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

void checkFunctionCall(AST_NODE* functionCallNode)
{
    AST_NODE* funcIDNode = functionCallNode->child;
    if(strcmp(funcIDNode->semantic_value.identifierSemanticValue.identifierName, "write") == 0){
        checkWriteFunction(functionCallNode);
        return;
    }

    SymbolTableEntry* entry = retrieveSymbol(funcIDNode->semantic_value.identifierSemanticValue.identifierName);
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
    while(paraPtr && formalPara){
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
    else if(formalParameter->type->kind == ARRAY_TYPE_DESCRIPTOR && 
        !(actualParameter->dataType == INT_PTR_TYPE || actualParameter->dataType == FLOAT_PTR_TYPE)){
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
        if(returnType != returnNode->child->dataType)
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
    if(exprOrConstNode->nodeType == CONST_VALUE_NODE)
        if(exprOrConstNode->dataType == INT_TYPE)
            if(fValue)
                *fValue = exprOrConstNode->semantic_value.const1->const_u.intval;
            else
                *iValue = exprOrConstNode->semantic_value.const1->const_u.intval;
        else
            *fValue = exprOrConstNode->semantic_value.const1->const_u.fval;
    else
        if(exprOrConstNode->dataType == INT_TYPE)
            if(fValue)
                *fValue = exprOrConstNode->semantic_value.exprSemanticValue.constEvalValue.iValue;
            else
                *iValue = exprOrConstNode->semantic_value.exprSemanticValue.constEvalValue.iValue;
        else
            *fValue = exprOrConstNode->semantic_value.exprSemanticValue.constEvalValue.fValue;
}

void evaluateExprValue(AST_NODE* exprNode)
{
    if(exprNode->semantic_value.exprSemanticValue.kind == BINARY_OPERATION){
        AST_NODE* opL = exprNode->child, *opR = opL->rightSibling;
        if(opL->dataType == INT_TYPE && opR->dataType == INT_TYPE){
            int valL = 0, valR = 0, result;
            getExprOrConstValue(opL, &valL, NULL);
            getExprOrConstValue(opR, &valR, NULL);
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
            float valL = 0, valR = 0, result;
            getExprOrConstValue(opL, NULL, &valL);
            getExprOrConstValue(opR, NULL, &valR);
            exprNode->dataType = FLOAT_TYPE;
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
            exprNode->semantic_value.exprSemanticValue.constEvalValue.fValue = result;
        }
    }
    else{
        AST_NODE* operand = exprNode->child;
        if(operand->dataType == INT_TYPE){
            int result = 0;
            getExprOrConstValue(operand, &result, NULL);
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
            getExprOrConstValue(operand, NULL, &result);
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
        AST_NODE* opL = exprNode->child, *opR = opL->rightSibling;
        processExprRelatedNode(opL);
        processExprRelatedNode(opR);
        
        if(opL->dataType == INT_PTR_TYPE || opL->dataType == FLOAT_PTR_TYPE){
            printErrorMsg(opL, INCOMPATIBLE_ARRAY_DIMENSION);
            exprNode->dataType = ERROR_TYPE;
        }
        if(opR->dataType == INT_PTR_TYPE || opR->dataType == FLOAT_PTR_TYPE){
            printErrorMsg(opL, INCOMPATIBLE_ARRAY_DIMENSION);
            exprNode->dataType = ERROR_TYPE;
        }
        if(opL->dataType == CONST_STRING_TYPE || opR->dataType == CONST_STRING_TYPE){
            printErrorMsg(exprNode, STRING_OPERATION);
            exprNode->dataType = ERROR_TYPE;
        }
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
        if(operand->dataType == INT_PTR_TYPE || operand->dataType == FLOAT_PTR_TYPE){
            printErrorMsg(operand, INCOMPATIBLE_ARRAY_DIMENSION);
            exprNode->dataType = ERROR_TYPE;
        }
        else if(operand->dataType == CONST_STRING_TYPE){
            printErrorMsg(exprNode, STRING_OPERATION);
            exprNode->dataType = ERROR_TYPE;
        }
        else if(operand->dataType == ERROR_TYPE)
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
    switch(exprRelatedNode->nodeType){
        case EXPR_NODE:
            processExprNode(exprRelatedNode); break;
        case STMT_NODE:
            checkFunctionCall(exprRelatedNode); break;
        case IDENTIFIER_NODE:
            processVariableRValue(exprRelatedNode); break;
        case CONST_VALUE_NODE:
            processConstValueNode(exprRelatedNode); break;
        default:
            printf("[DEBUG] Unhandle case in processExprRelatedNode()\n");
            exprRelatedNode->dataType = ERROR_TYPE;
            break;
    }
}

void processVariableLValue(AST_NODE* idNode)// [Msg] 改很少 相似度極高
{
    SymbolTableEntry *entry = retrieveSymbol(idNode->semantic_value.identifierSemanticValue.identifierName);
    if(!entry){
        printErrorMsg(idNode, SYMBOL_UNDECLARED); // [Ass] 1.a)
        idNode->dataType = ERROR_TYPE;
        return;
    }
    idNode->semantic_value.identifierSemanticValue.symbolTableEntry = entry;

    if(entry->attribute->attributeKind == TYPE_ATTRIBUTE){
        printErrorMsg(idNode, IS_TYPE_NOT_VARIABLE);
        idNode->dataType = ERROR_TYPE;
        return;
    }
    else if(entry->attribute->attributeKind == FUNCTION_SIGNATURE){
        printErrorMsg(idNode, IS_FUNCTION_NOT_VARIABLE);
        idNode->dataType = ERROR_TYPE;
        return;
    }
    
    TypeDescriptor *typeDescriptor = idNode->semantic_value.identifierSemanticValue.symbolTableEntry->attribute->attr.typeDescriptor;
        
    if(idNode->semantic_value.identifierSemanticValue.kind == NORMAL_ID){
        if(typeDescriptor->kind == ARRAY_TYPE_DESCRIPTOR){
            printErrorMsg(idNode, INCOMPATIBLE_ARRAY_DIMENSION);
            idNode->dataType = ERROR_TYPE;
        }
        else
            idNode->dataType = typeDescriptor->properties.dataType;
    }
    else if(idNode->semantic_value.identifierSemanticValue.kind == ARRAY_ID){
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
        if(typeDescriptor->kind == SCALAR_TYPE_DESCRIPTOR){
            printErrorMsg(idNode, NOT_ARRAY);
            idNode->dataType = ERROR_TYPE;
        }
        else{
            if(dimension == typeDescriptor->properties.arrayProperties.dimension)
                idNode->dataType = typeDescriptor->properties.arrayProperties.elementType;
            else{
                printErrorMsg(idNode, INCOMPATIBLE_ARRAY_DIMENSION);
                idNode->dataType = ERROR_TYPE;
            }
        }
    }
}

void processVariableRValue(AST_NODE* idNode)// [Msg] 改很少 相似度極高
{
    SymbolTableEntry *entry = retrieveSymbol(idNode->semantic_value.identifierSemanticValue.identifierName);
    
    idNode->semantic_value.identifierSemanticValue.symbolTableEntry = entry;
    if(!entry){
        printErrorMsg(idNode, SYMBOL_UNDECLARED);
        idNode->dataType = ERROR_TYPE;
        return;
    }
    if(entry->attribute->attributeKind == TYPE_ATTRIBUTE){
        printErrorMsg(idNode, IS_TYPE_NOT_VARIABLE);
        idNode->dataType = ERROR_TYPE;
        return;
    }

    TypeDescriptor *typeDescriptor = idNode->semantic_value.identifierSemanticValue.symbolTableEntry->attribute->attr.typeDescriptor;
        
    if(idNode->semantic_value.identifierSemanticValue.kind == NORMAL_ID){
        if(typeDescriptor->kind == ARRAY_TYPE_DESCRIPTOR)
            if(typeDescriptor->properties.arrayProperties.elementType == INT_TYPE)
                idNode->dataType = INT_PTR_TYPE;
            else
                idNode->dataType = FLOAT_PTR_TYPE;
        else
            idNode->dataType = typeDescriptor->properties.dataType;
    }
    else if(idNode->semantic_value.identifierSemanticValue.kind == ARRAY_ID){
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
                if(dimension == typeDescriptor->properties.arrayProperties.dimension)
                    idNode->dataType = typeDescriptor->properties.arrayProperties.elementType;
                else if(dimension > typeDescriptor->properties.arrayProperties.dimension){
                    printErrorMsg(idNode, INCOMPATIBLE_ARRAY_DIMENSION);
                    idNode->dataType = ERROR_TYPE;
                }
                else if(typeDescriptor->properties.arrayProperties.elementType == INT_TYPE)
                    idNode->dataType = INT_PTR_TYPE;
                else
                    idNode->dataType = FLOAT_PTR_TYPE;
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
    AST_NODE* dimPtr = dimList;
    int dimension = 0;
    if(ignoreFirstDimSize && dimPtr->nodeType == NUL_NODE){ // [Inf] int arr[][10], ...
        typeDescriptor->properties.arrayProperties.sizeInEachDimension[dimension++] = 0;
        dimPtr = dimPtr->rightSibling;
    }
    while(dimPtr){
        if(dimension >= MAX_ARRAY_DIMENSION){
            printErrorMsg(dimList->parent, EXCESSIVE_ARRAY_DIM_DECLARATION);
            idNode->dataType = ERROR_TYPE;
            break;
        }

        processExprRelatedNode(dimPtr);
        if(dimPtr->dataType == ERROR_TYPE)
            idNode->dataType = ERROR_TYPE;
        else if(dimPtr->dataType == FLOAT_TYPE){ // [Ass] 3.b)
            printErrorMsg(dimPtr->parent, ARRAY_SIZE_NOT_INT);
            idNode->dataType = ERROR_TYPE;
        }
        else if(dimPtr->semantic_value.exprSemanticValue.isConstEval &&
            dimPtr->semantic_value.exprSemanticValue.constEvalValue.iValue < 0){ // [Ass] Extra 1
            printErrorMsg(dimPtr->parent, ARRAY_SIZE_NEGATIVE);
            idNode->dataType = ERROR_TYPE;
        }
        else
            typeDescriptor->properties.arrayProperties.sizeInEachDimension[dimension] = 
                dimPtr->semantic_value.exprSemanticValue.constEvalValue.iValue;

        dimension++;
        dimPtr = dimPtr->rightSibling;
    }

    typeDescriptor->properties.arrayProperties.dimension = dimension;
}
