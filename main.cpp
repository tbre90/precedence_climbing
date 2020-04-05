#pragma warning(disable: 26812 26495)

#include <cmath>
#include <iostream>
#include <string>
#include <exception>
#include <string_view>

enum class Associativity {
    LEFT,
    RIGHT
};

struct OperatorPrecedence {
    int prec;
    Associativity assoc;
};

enum TokenType {
    ADD = 0,
    SUBTRACT = 1,
    MULTIPLY = 2,
    DIVIDE = 3,
    POWER = 4,
    NUMBER,
    LEFT_PAREN,
    RIGHT_PAREN,
    ILLEGAL_CHARACTER,
    END_OF_FILE,
};

OperatorPrecedence OperatorMap[] = {
    OperatorPrecedence { 1, Associativity::LEFT }, // TokenType::ADD
    OperatorPrecedence { 1, Associativity::LEFT }, // TokenType::SUBTRACT
    OperatorPrecedence { 2, Associativity::LEFT }, // TokenType::MULTIPLY
    OperatorPrecedence { 2, Associativity::LEFT }, // TokenType::DIVIDE
    OperatorPrecedence { 3, Associativity::RIGHT }, // TokenType::POWER
};

struct Token {
    std::string_view string;
    TokenType type;
};

static void
print_token(Token t) {
    std::string token_type =
        t.type == TokenType::ADD ? "ADD" :
        t.type == TokenType::MULTIPLY ? "MULTIPLY" :
        t.type == TokenType::NUMBER ? "NUMBER" :
        t.type == TokenType::END_OF_FILE ? "END_OF_FILE" :
        "ILLEGAL";

    printf("{ '%.*s', %s }\n", (int)t.string.length(), t.string.data(), token_type.c_str());
}

struct Lexer {
    std::string source;

    size_t current_position;
    size_t end;

    Lexer(std::string source) {
        this->source = source;
        this->current_position = 0;
        this->end = source.length();
    }

    Token
    make_simple_token(TokenType type) {
        return Token{
            std::string_view(&source[current_position], 1),
            type
        };
    }

    bool is_at_end() {
        return current_position >= end;
    }

    char peek() {
        return is_at_end() ? '\0' : source[current_position];
    }

    char peek_next() {
        return is_at_end() ? '\0' : source[current_position + 1];
    }

    char read_next() {
        return is_at_end() ? '\0' : source[current_position++];
    }

    void skip_whitespace() {
        for (;;) {
            char c = peek();

            switch (c) {
                case '\n':
                case '\t':
                case '\r':
                case ' ': {
                    read_next();
                    continue;
                }
            }

            break;
        }
    }

    Token 
    number() {

        size_t start = current_position;
        read_next();

        for (;;) {
            char c = peek();
            if (!isdigit(c)) break;
            else read_next();
        }

        return Token{ std::string_view(&source[start], ((long long)current_position - start)), TokenType::NUMBER };
    }

    Token next_token() {
        skip_whitespace();

        char c = peek();

        Token t;

        if (isdigit(c)) {
            t = number();
        } else {
            switch (c) {
                case '+' : { t = make_simple_token(TokenType::ADD); read_next(); } break;
                case '-' : { t = make_simple_token(TokenType::SUBTRACT); read_next(); } break;
                case '*' : {
                    if (peek_next() == '*') {
                        t = Token{
                            std::string_view(&source[current_position], 2),
                            TokenType::POWER
                        };
                        read_next();
                    } else {
                        t = make_simple_token(TokenType::MULTIPLY); 
                    }
                    read_next();
                } break;
                case '/' : { t = make_simple_token(TokenType::DIVIDE); read_next(); } break;
                case '(' : { t = make_simple_token(TokenType::LEFT_PAREN); read_next(); } break;
                case ')' : { t = make_simple_token(TokenType::RIGHT_PAREN); read_next(); } break;

                default: {
                    t = 
                        c == '\0' ?
                            make_simple_token(TokenType::END_OF_FILE) :
                            make_simple_token(TokenType::ILLEGAL_CHARACTER);
                    read_next();
                } break;
            }
        }

        return t;
    }
};

struct Parser {

    Lexer lexer;
    Token token;

    Parser(Lexer l) : lexer(l) {}

    void next_token() {
        token = lexer.next_token();
    }

    double parse() {
        return compute_expr(1);
    }

    double compute_op(Token t, double lhs, double rhs) {
        switch (t.type) {
            case TokenType::ADD: return lhs + rhs;
            case TokenType::SUBTRACT: return lhs - rhs;
            case TokenType::MULTIPLY: return lhs * rhs;
            case TokenType::DIVIDE: return lhs / rhs;
            case TokenType::POWER: return pow(lhs, rhs);
            default: report_error("Unknown operator:\n");
        }
    }

    double compute_atom() {
        next_token();
        if (token.type == TokenType::LEFT_PAREN) {
            double val = compute_expr(1);

            if (token.type != TokenType::RIGHT_PAREN) report_error("Unmatched '(':\n");

            next_token();
            return val;
        }

        if (token.type == TokenType::END_OF_FILE) {
            report_error("Unexpected end of expression: \n");
        }

        if (token.type != TokenType::NUMBER) {
            //throw ParserException("Unexpected character: " + std::string(token.string));
            report_error("Unexpected character: \n");
        }

        double val = std::strtod(token.string.data(), nullptr);
        next_token();
        return val;
    }

    double compute_expr(int minimum_precedence) {
        auto atom_lhs = compute_atom();

        while (true) {
            auto cur = token;
            if ((cur.type > TokenType::POWER || cur.type < TokenType::ADD)
                || OperatorMap[cur.type].prec < minimum_precedence) {

                if (cur.type == TokenType::ILLEGAL_CHARACTER) {
                    report_error("Unknown operator:\n");
                }

                break;
            }

            auto op_prec = OperatorMap[cur.type];

            auto next_min_prec =
                op_prec.assoc == Associativity::LEFT ? op_prec.prec + 1 : op_prec.prec;

            auto atom_rhs = compute_expr(next_min_prec);
            atom_lhs = compute_op(cur, atom_lhs, atom_rhs);
        }

        return atom_lhs;
    }

    // error handling
    void report_error(std::string err) {
        auto error = err + show_error_location();
        throw ParserException(error);
    }

    // append the location of the parser error
    std::string show_error_location() {

        char const* const source_start = &lexer.source[0];

        char const* start_of_line = token.string.data();
        while (*start_of_line != '\n' && start_of_line > source_start) {
            start_of_line--;
        }

        if (*start_of_line == '\0') {
            start_of_line++;
        }

        char const* end_of_line = start_of_line;
        while (*end_of_line != '\n' && *end_of_line != '\0') {
            end_of_line++;
        }

        if (*(end_of_line - 1) == '\r') {
            end_of_line--;
        }

        size_t source_line_len = end_of_line - start_of_line;

        size_t space_before_caret = token.string.data() - start_of_line;

        size_t total_length = source_line_len + space_before_caret + 3;

        std::string error_str;
        //error_str.reserve(total_length);

        size_t offset = 0;
        while (source_line_len--) {
            //error_str[offset++] 
            error_str += source_start[offset++];
        }

        error_str += '\n';

        while (space_before_caret--) {
            error_str += ' ';
        }

        error_str += '^';
        //error_str[offset] = '\0';

        return error_str;
    }

    struct ParserException : public std::exception {

        std::string error;

        ParserException(std::string err) : error(err) {
        }

        const char* what() const throw() {
            return error.c_str();
        }
    };
};

int main()
{
    std::string s;

    char buffer[64] = {0};

    for (;;) {
        printf("> ");
        std::getline(std::cin, s);

        if (s == ":quit") {
            break;
        }

        Lexer lexer(s);
        Parser parser(lexer);

        try {
            auto value = parser.parse();
            snprintf(buffer, sizeof(buffer), "%g", value);
            printf(" = %s\n", buffer);

        } catch (Parser::ParserException& e) {
            printf("%s\n", e.what());
        }
    }
}
