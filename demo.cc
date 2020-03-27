#include <iostream>
#include <map>
#include <climits>
#include <cstdint>
#include "compiler.h"
#include "lexer.h"

using namespace std;
LexicalAnalyzer lexer;
Token t;

//Map for storing user input by variable name
map<string, int> inputMap;
//Global program body.
struct InstructionNode* body;
//Parser methods
void parse_program();
void parse_var_section();
void parse_id_list();
InstructionNode * parse_body();
InstructionNode * parse_stmt_list();
InstructionNode * parse_stmt();
InstructionNode * parse_assign_stmt();
InstructionNode * parse_expr();
int parse_primary();
TokenType parse_op();
InstructionNode * parse_io_stmt();
InstructionNode * parse_while_stmt();
InstructionNode * parse_if_stmt();
InstructionNode * parse_condition();
TokenType parse_relop();
InstructionNode * parse_switch_stmt(InstructionNode* emptyNode);
InstructionNode * parse_for_stmt();
InstructionNode * parse_case_list(int holder, InstructionNode* emptyNode);
InstructionNode * parse_case(int holder);
InstructionNode * parse_default_case();
void parse_inputs();
void parse_num_list();

Token peek(){
    t = lexer.GetToken();
    lexer.UngetToken(t);
    return t;
}

void syntax_error(){
    cout << "SYNTAX ERROR !!!\n";
    exit(1);
}

Token expect(TokenType expected_type){
    t = lexer.GetToken();
    if (t.token_type != expected_type)
        syntax_error();
    return t;
}

struct InstructionNode * parse_generate_intermediate_representation()
{
    parse_program();
    return body;
}

//program → var_section body inputs
void parse_program(){
    t = peek();
    if(t.token_type != ID) {
        syntax_error();
    }
    parse_var_section();

    t = peek();
    if(t.token_type != LBRACE){
        syntax_error();
    }
    body = parse_body();

    t = peek();
    if(t.token_type != NUM){
        syntax_error();
    }
    parse_inputs();
}

void parse_var_section(){
    parse_id_list();
    expect(SEMICOLON);
}

void parse_id_list(){
    t = expect(ID);
    inputMap[t.lexeme] = next_available;
    //relocate the variable mem by initialize the variable to 0
    mem[next_available++] = 0;

    t = lexer.GetToken();
    if(t.token_type == COMMA){
        parse_id_list();
    } else{
        lexer.UngetToken(t);
    }
}

struct InstructionNode* parse_body(){
    struct InstructionNode* instl = NULL;
    expect(LBRACE);

    instl = parse_stmt_list();
    expect(RBRACE);
    return instl;
}

struct InstructionNode* parse_stmt_list(){
    struct InstructionNode* inst1;
    struct InstructionNode* inst2;
    inst1 = parse_stmt();

    auto endList = inst1;
    t = peek();
    //stmt → assign_stmt | while_stmt | if_stmt | switch_stmt | for_stmt
    //stmt → output_stmt | input_stmt
    switch(t.token_type){
        case ID:
        case NUM:
        case SWITCH:
        case WHILE:
        case IF:
        case FOR:
        case OUTPUT:
        case INPUT:
            //Put other statements at the end of the previous statement.
            inst2 = parse_stmt_list();
            while(endList->next != NULL){
                endList = endList->next;
            }
            
            endList->next = inst2;

        default:
            break;
    }
    
    return inst1;
}

struct InstructionNode* parse_stmt(){
    struct InstructionNode* inst = NULL;
    struct InstructionNode* endList;
    auto* emptyNode = new InstructionNode;

    t = peek();
    switch(t.token_type){
        case ID:
            inst = parse_assign_stmt();
            break;

        case WHILE:
            inst = parse_while_stmt();
            break;

        case IF:
            inst = parse_if_stmt();
            break;

        case SWITCH:
            //the escaping emptyNode for THIS scope of switch statement
            emptyNode->type = NOOP;
            emptyNode->next = NULL;
            //Transfer the outer statement block empty node to the switch statement for JMP
            inst = parse_switch_stmt(emptyNode);
            endList = inst;
            while(endList->next != NULL)
                endList = endList->next;

            endList->next = emptyNode;
            break;

        case FOR:
            inst = parse_for_stmt();
            break;

        //Just for convenient purpose, I put OUTPUT and INPUT together
        case OUTPUT:
        case INPUT:
            inst = parse_io_stmt();
            break;

        default:
            syntax_error();
    }

    return inst;
}

//It could be ID EQUAL primary SEMICOLON or ID EQUAL expr SEMICOLON
struct InstructionNode* parse_assign_stmt(){
    auto* assign = new InstructionNode;
    assign->type = ASSIGN;

    t = expect(ID);
    assign->assign_inst.left_hand_side_index = inputMap[t.lexeme];
    expect(EQUAL);

    Token t1 = lexer.GetToken();
    Token t2 = lexer.GetToken();
    lexer.UngetToken(t2);
    lexer.UngetToken(t1);

    if (t1.token_type == ID || t1.token_type == NUM){
        //Handle second situation
        if (t2.token_type == PLUS || t2.token_type == MINUS || t2.token_type == MULT || t2.token_type == DIV) {
            auto temp = parse_expr();
            assign->assign_inst.operand1_index = temp ->assign_inst.operand1_index;
            assign->assign_inst.op = temp->assign_inst.op;
            assign->assign_inst.operand2_index = temp->assign_inst.operand2_index;

        //Handle first situation
        } else if(t2.token_type == SEMICOLON){
            assign->assign_inst.op = OPERATOR_NONE;
            assign->assign_inst.operand1_index = parse_primary();

        } else{
            syntax_error();
        }

    } else{
        syntax_error();
    }
    
    expect(SEMICOLON);
    assign->next = NULL;
    return assign;
}

struct InstructionNode* parse_expr(){
    auto *getInfo = new InstructionNode;
    getInfo->assign_inst.operand1_index = parse_primary();
    //Debug!
    TokenType type = parse_op();
    switch (type){
        case PLUS:
            getInfo->assign_inst.op = OPERATOR_PLUS;
            break;

        case MINUS:
            getInfo->assign_inst.op = OPERATOR_MINUS;
            break;

        case MULT:
            getInfo->assign_inst.op = OPERATOR_MULT;
            break;

        case DIV:
            getInfo->assign_inst.op = OPERATOR_DIV;
            break;

        //should not happen
        default:
            break;
    }

    getInfo->assign_inst.operand2_index = parse_primary();
    return getInfo;
}

//primary → ID | NUM
int parse_primary(){
    int index;
    t = lexer.GetToken();
    if(t.token_type != ID && t.token_type != NUM){
        syntax_error();
    }

    if(t.token_type == ID){
        index = inputMap[t.lexeme];
    } else{
        index = next_available;
        mem[next_available++] = stoi(t.lexeme);
    }

    return index;
}

TokenType parse_op(){
    t = lexer.GetToken();
    //Check by enum int value.
    if (t.token_type < 11 || t.token_type > 14){
        syntax_error();
    }

    return t.token_type;
}

InstructionNode* parse_io_stmt(){
    auto *inst = new InstructionNode;
    t = lexer.GetToken();
    //output_stmt → output ID SEMICOLON
    if (t.token_type == OUTPUT){
        inst->type = OUT;
        t = expect(ID);
        inst->output_inst.var_index = inputMap[t.lexeme];

    //input_stmt → input ID SEMICOLON
    } else if (t.token_type == INPUT){
        inst->type = IN;
        t = expect(ID);
        inst->input_inst.var_index = inputMap[t.lexeme];

    } else{
        syntax_error();
    }

    inst->next = NULL;
    expect(SEMICOLON);

    return inst;
}

struct InstructionNode* parse_while_stmt(){
    auto *whileStmt = new InstructionNode;
    expect(WHILE);
    //setLabel
    whileStmt->type = CJMP;
    struct InstructionNode* condition = parse_condition();

    //if statement
    whileStmt->cjmp_inst.operand1_index = condition->cjmp_inst.operand1_index;
    whileStmt->cjmp_inst.condition_op = condition->cjmp_inst.condition_op;
    whileStmt->cjmp_inst.operand2_index = condition->cjmp_inst.operand2_index;

    t = peek();
    if(t.token_type != LBRACE){
        syntax_error();
    }

    //stmt_list
    whileStmt->next = parse_body();

    //goto label
    auto* jmp = new InstructionNode;
    jmp->type = JMP;
    jmp->jmp_inst.target = whileStmt;

    auto* noop = new InstructionNode;
    noop->type = NOOP;
    noop->next = NULL;

    //get out of the while block
    struct InstructionNode* getLast = whileStmt;
    while(getLast->next != NULL){
        getLast = getLast->next;
    }

    getLast->next = jmp;
    jmp->next = noop;
    whileStmt->cjmp_inst.target = noop;

    return whileStmt;
}

struct InstructionNode* parse_if_stmt(){
    auto *ifStmt = new InstructionNode;
    expect(IF);

    //evaluate condition
    ifStmt->type = CJMP;
    struct InstructionNode* temp = parse_condition();

    ifStmt->cjmp_inst.operand1_index = temp->cjmp_inst.operand1_index;
    ifStmt->cjmp_inst.condition_op = temp->cjmp_inst.condition_op;
    ifStmt->cjmp_inst.operand2_index = temp->cjmp_inst.operand2_index;

    t = peek();
    if(t.token_type != LBRACE){
        syntax_error();
    }

    ifStmt->next = parse_body();

    auto* noop = new InstructionNode;
    noop->type = NOOP;
    noop->next = NULL;

    struct InstructionNode* endList = ifStmt;
    while(endList->next != NULL){
        endList = endList->next;
    }

    endList->next = noop;
    ifStmt->cjmp_inst.target = noop;

    return ifStmt;
}

//condition → primary relop primary
struct InstructionNode* parse_condition(){
    auto* condition = new InstructionNode;

    t = peek();
    if(t.token_type != ID && t.token_type != NUM){
        syntax_error();
    }

    condition->cjmp_inst.operand1_index = parse_primary();

    t = peek();

    TokenType type = parse_relop();
    switch(type){
        case GREATER:
            condition->cjmp_inst.condition_op = CONDITION_GREATER;
            break;

        case LESS:
            condition->cjmp_inst.condition_op = CONDITION_LESS;
            break;

        case NOTEQUAL:
            condition->cjmp_inst.condition_op = CONDITION_NOTEQUAL;
            break;

        default:
            syntax_error();
            break;
    }

    t = peek();
    if (t.token_type != ID && t.token_type != NUM) {
        syntax_error();
    }
    
    condition->cjmp_inst.operand2_index = parse_primary();
    return condition;
}

TokenType parse_relop(){
    t = lexer.GetToken();
    //init.
    TokenType relop = END_OF_FILE;
    if(t.token_type == GREATER || t.token_type == LESS || t.token_type == NOTEQUAL){
        relop = t.token_type;
    } else{
        syntax_error();
    }

    return relop;
}

//for_stmt → FOR LPAREN assign_stmt condition SEMICOLON assign_stmt RPAREN body
struct InstructionNode* parse_for_stmt(){
    struct InstructionNode* forStmt;
    struct InstructionNode* assign;
    expect(FOR);
    expect(LPAREN);
    
    t = peek();
    if(t.token_type != ID){
        syntax_error();
    }

    //parsing first assigning condition
    forStmt = parse_assign_stmt();
    auto* temp = new InstructionNode;
    temp->type = CJMP;

    //parsing satisfying condition
    struct InstructionNode* temp2 = parse_condition();
    temp->cjmp_inst.operand1_index = temp2->cjmp_inst.operand1_index;
    temp->cjmp_inst.condition_op = temp2->cjmp_inst.condition_op;
    temp->cjmp_inst.operand2_index = temp2->cjmp_inst.operand2_index;

    expect(SEMICOLON);
    t = peek();
    if(t.token_type != ID) {
        syntax_error();
    }

    //parsing changes, will affect the determinant.
    assign = parse_assign_stmt();
    assign->next = NULL;
    expect(RPAREN);

    //end of parsing condition in LPAREN & RPAREN
    t = peek();
    if (t.token_type != LBRACE){
        syntax_error();
    }

    //Parsing for loop body.
    temp->next = parse_body();
    
    auto* addStmt = temp->next;
    while(addStmt->next != NULL){
        addStmt = addStmt->next;
    }
    addStmt->next = assign;

    auto* jmp = new InstructionNode;
    jmp->type = JMP;
    jmp->jmp_inst.target = temp;

    auto* emptyNode = new InstructionNode;
    emptyNode->type = NOOP;
    emptyNode->next = NULL;

    jmp->next = emptyNode;

    struct InstructionNode* endList = temp;
    while(endList->next != NULL){
        endList = endList->next;
    }

    //link the loop with an empty node after.
    endList->next = jmp;
    temp->cjmp_inst.target = emptyNode;
    forStmt->next = temp;

    return forStmt;
}

/*
a, b, c;
{
    a = 2;
    b = 0;
    c = 0;
    SWITCH a {
        CASE 1:
        {
            b = 100;
            c = 100;
            WHILE (b > 100){
                c = c + 1;
            }
        }

        CASE 2:
        {
            b = 900;
        }

        CASE 3:
        {
            b = 800;
        }

        DEFAULT:
        {
            b = 999;
        }
    }

    output b;
}
1 2 3 4 5
*/

//switch_stmt → SWITCH ID LBRACE case_list RBRACE
//switch_stmt → SWITCH ID LBRACE case_list default_case RBRACE
struct InstructionNode * parse_switch_stmt(InstructionNode* emptyNode){
    struct InstructionNode* switchStmt;
    expect(SWITCH);

    t = expect(ID);
    int holder = inputMap[t.lexeme];
    expect(LBRACE);

    t = peek();
    if(t.token_type != CASE){
        syntax_error();
    }

    //check more cases
    switchStmt = parse_case_list(holder, emptyNode);

    t = peek();
    //switch_stmt → SWITCH ID LBRACE case_list default_case RBRACE
    if(t.token_type == DEFAULT){
        auto endList = switchStmt;
        while(endList->next->next != NULL){
            endList = endList->next;
        }
        //default will always be the last element in a switch_case statement.
        //because of the parsing grammar.
        endList->next = parse_default_case();
        expect(RBRACE);

    } else if(t.token_type == RBRACE){
        t = lexer.GetToken();
        return switchStmt;

    } else{
        syntax_error();
    }

    return switchStmt;
}

struct InstructionNode* parse_case_list(int holder, InstructionNode* emptyNode){
    struct InstructionNode* caseNode;
    struct InstructionNode* caseList = NULL;

    t = peek();
    if(t.token_type != CASE){
        syntax_error();
    }

    caseNode = parse_case(holder);

    //switch case should have two JMP at the end
    //One is for escaping the case, and one is for escaping whatever the other body is.
    auto* jmp = new InstructionNode;
    jmp->type = JMP;

    //Escaping the case statement
    //Cannot use global emptyNode apparently.
    //Every single statement's escaping node should be defined in scope.
    jmp->jmp_inst.target = emptyNode;

    struct InstructionNode* endList = caseNode->cjmp_inst.target;
    //Considering for the conditions that WHILE or FOR loop in a case statement.
    while(endList->next->next != NULL){
        endList = endList->next;
    }
    endList->next = jmp;

    t = peek();
    if(t.token_type == CASE){
        caseList = parse_case_list(holder, emptyNode);

        auto endList = caseNode;
        while(endList->next->next != NULL){
            endList = endList->next;
        }

        endList->next = caseList;
    }
    
    return caseNode;
}

struct InstructionNode* parse_case(int holder){
    auto* castStmt = new InstructionNode;
    expect(CASE);
    
    castStmt->type = CJMP;
    //temp storage
    castStmt->cjmp_inst.operand1_index = holder;
    castStmt->cjmp_inst.condition_op = CONDITION_NOTEQUAL;
    t = expect(NUM);

    int index = next_available;
    mem[next_available++] = stoi(t.lexeme);
    castStmt->cjmp_inst.operand2_index = index;

    expect(COLON);
    t = peek();
    if (t.token_type != LBRACE){
        syntax_error();
    }

    //Parsing case statement body
    castStmt->cjmp_inst.target = parse_body();
    auto* noop = new InstructionNode;
    noop->type = NOOP;
    noop->next = NULL;

    auto endList = castStmt->cjmp_inst.target;
    while(endList->next != NULL){
        endList = endList->next;
    }

    castStmt->next = noop;
    endList->next = castStmt->next;
    return castStmt;
}

struct InstructionNode * parse_default_case(){
    struct InstructionNode*  defaultInst;
    expect(DEFAULT);
    expect(COLON);

    t = peek();
    if (t.token_type != LBRACE){
        syntax_error();
    }

    defaultInst = parse_body();
    return defaultInst;
}

void parse_inputs(){
    parse_num_list();
}

void parse_num_list(){
    t = expect(NUM);
    inputs.push_back(stoi(t.lexeme));

    t = peek();
    if(t.token_type == NUM){
        parse_num_list();

    //No syntax error, they said lol.
    } else{
        return;
    }
}