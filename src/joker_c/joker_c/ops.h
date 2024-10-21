#pragma once

#ifndef __joker_ops_h__
#define __joker_ops_h__

typedef enum Ops {
    ADD,                        // +
    SUB,                        // -
    MUL,                        // *
    DIV,                        // /
    MOD,                        // %
    POW,                        // ^
    EQ,                         // ==
    NEQ,                        //!=
    GT,                         // >
    LT,                         // <
    GTE,                        // >=
    LTE,                        // <=
    AND,                        // &&
    OR,                         // ||
    XOR,                        // ^|
    SHL,                        // <<
    SHR,                        // >>
    NOT,                        // !
    ASSIGN,                     // =
    PLUS_ASSIGN,                // +=
    MINUS_ASSIGN,               // -=
    MUL_ASSIGN,                 // *=
    DIV_ASSIGN,                 // /=
    MOD_ASSIGN,                 // %=
    POW_ASSIGN,                 // ^=
    AND_ASSIGN,                 // &=
    OR_ASSIGN,                  // |=
    XOR_ASSIGN,                 // ^=
    LEFT_SHIFT,                 // <<
    RIGHT_SHIFT,                // >>
    LEFT_SHIFT_ASSIGN,          // <<=
    RIGHT_SHIFT_ASSIGN,         // >>=
    BIT_AND,                    // &
    BIT_OR,                     // |
    BIT_XOR,                    // ^
    BIT_NOT,                    // ~
    UNARY_PLUS,                 // +
    UNARY_MINUS,                // -
    INCREMENT,                  // ++
    DECREMENT,                  // --
} Ops;


#define macro_is_binary_op(op) ((op) >= ADD && (op) <= MOD)
#define macro_is_assign_op(op) ((op) >= ASSIGN && (op) <= POW_ASSIGN)
#define macro_is_unary_op(op)  ((op) >= UNARY_PLUS && (op) <= DECREMENT)

#define macro_ops_to_string(op) \
    ((op) == ADD? "+" : \
     (op) == SUB? "-" : \
     (op) == MUL? "*" : \
     (op) == DIV? "/" : \
     (op) == MOD? "%" : \
     (op) == POW? "^" : \
     (op) == EQ ? "==" : \
     (op) == NEQ? "!=" : \
     (op) == GT ? ">" : \
     (op) == LT ? "<" : \
     (op) == GTE? ">=" : \
     (op) == LTE? "<=" : \
     (op) == AND? "&&" : \
     (op) == OR ? "||" : \
     (op) == XOR? "^|" : \
     (op) == SHL? "<<" : \
     (op) == SHR? ">>" : \
     (op) == NOT? "!" : \
     (op) == ASSIGN? "=" : \
     (op) == PLUS_ASSIGN? "+=" : \
     (op) == MINUS_ASSIGN? "-=" : \
     (op) == MUL_ASSIGN? "*=" : \
     (op) == DIV_ASSIGN? "/=" : \
     (op) == MOD_ASSIGN? "%=" : \
     (op) == POW_ASSIGN? "^=" : \
     (op) == AND_ASSIGN? "&=" : \
     (op) == OR_ASSIGN ? "|=" : \
     (op) == XOR_ASSIGN? "^=" : \
     (op) == LEFT_SHIFT? "<<" : \
     (op) == RIGHT_SHIFT? ">>" : \
     (op) == LEFT_SHIFT_ASSIGN? "<<=" : \
     (op) == RIGHT_SHIFT_ASSIGN? ">>=" : \
     (op) == BIT_AND? "&" : \
     (op) == BIT_OR ? "|" : \
     (op) == BIT_XOR? "^" : \
     (op) == BIT_NOT? "~" : \
     (op) == UNARY_PLUS? "+" : \
     (op) == UNARY_MINUS? "-" : \
     (op) == INCREMENT ? "++" : \
     (op) == DECREMENT ? "--" : \
     "unknown operator")



#endif // __joker_ops_h__
