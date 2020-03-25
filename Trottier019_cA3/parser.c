/* Filename: parser.c
/* PURPOSE:
*    PARSER.C: Parses a platypus file
*    as required for CST8152, Assignment #3
*    parser(void) must be called before using the scanner.
*    created by: Joseph Trottier (040866019)
*	  
*	  Date: 19 April 2019
*/
#include "parser.h"

void parser(void) {
	lookahead = malar_next_token();
	program(); match(SEOF_T, NO_ATTR);
	gen_incode("PLATY: Source file parsed");
}
void match(int pr_token_code, int pr_token_attribute) {
	if (lookahead.code != pr_token_code) {
		syn_eh(pr_token_code);
	}
	if (pr_token_code == KW_T && lookahead.attribute.get_int != pr_token_attribute) {
		syn_eh(pr_token_code);
	}
	else if (pr_token_code == LOG_OP_T && lookahead.attribute.get_int != pr_token_attribute){
		syn_eh(pr_token_code);
	}
	else if (pr_token_code == ART_OP_T && lookahead.attribute.get_int != pr_token_attribute){
		syn_eh(pr_token_code);
	}
	else if (pr_token_code == REL_OP_T && lookahead.attribute.get_int != pr_token_attribute) {
		syn_eh(pr_token_code);
	}
	else if (pr_token_code == SEOF_T)return;
	lookahead = malar_next_token();
	if (lookahead.code == ERR_T) {
		syn_printe();
		lookahead = malar_next_token();
		synerrno++;
		return;
	}
}

void syn_eh(int sync_token_code) {
	syn_printe();
	synerrno++;
	while (lookahead.code != sync_token_code) {
		if (lookahead.code == SEOF_T && sync_token_code != SEOF_T) {
			exit(synerrno);
		}
		lookahead = malar_next_token();
	}
	if (sync_token_code == SEOF_T)return;
	return;
}

void gen_incode(char * str) {
	printf("%s\n", str);
}
/*
<program>  -> PLATYPUS { <opt_statements> }
FIRST(<program>)= {KW_T (PLATYPUS)}
*/
void program() {
	match(KW_T, PLATYPUS); match(LBR_T, NO_ATTR); 
	opt_statements();
	match(RBR_T, NO_ATTR);
	gen_incode("PLATY: Program parsed");
}

/*
<opt_statements> -> <statements> | epsilon
FIRST(<opt_statements>)= {AVID_T , SVID_T , KW_T(IF) , KW_T(WHILE) , KW_T(READ) , KW_T(WRITE), epsilon}
*/
void opt_statements() {
	if (lookahead.code == AVID_T
		|| lookahead.code == SVID_T
		|| (lookahead.code == KW_T && lookahead.attribute.get_int == IF)
		|| (lookahead.code == KW_T && lookahead.attribute.get_int == WHILE)
		|| (lookahead.code == KW_T && lookahead.attribute.get_int == READ)
		|| (lookahead.code == KW_T && lookahead.attribute.get_int == WRITE))
		statements();
	else
		gen_incode("PLATY: Opt_statements parsed");
}
/*
<statements> -> <statement> <statements’>
FIRST(<statements>)= {AVID_T , SVID_T , KW_T(IF) , KW_T(WHILE) , KW_T(READ) , KW_T(WRITE)}
*/
void statements() {
	statement(); statements_prime();
	//gen_incode("PLATY: Output statement parsed");
}
/*
<statements’> -><statement> <statement’> | eps
FIRST(<statements’>)= {AVID_T , SVID_T , KW_T(IF) , KW_T(WHILE) , KW_T(READ) , KW_T(WRITE), eps}
*/
void statements_prime() {
	if (lookahead.code == AVID_T
		|| lookahead.code == SVID_T
		|| (lookahead.code == KW_T && lookahead.attribute.get_int == IF)
		|| (lookahead.code == KW_T && lookahead.attribute.get_int == WHILE)
		|| (lookahead.code == KW_T && lookahead.attribute.get_int == READ)
		|| (lookahead.code == KW_T && lookahead.attribute.get_int == WRITE))
		statements();
}
/*
<statement> ->
<assignment statement> 
| <selection statement>
| <iteration statement>
| <input statement>
| <output statement>
FIRST(<statement>)= {AVID_T , SVID_T , KW_T(IF) , KW_T(WHILE) , KW_T(READ) , KW_T(WRITE)}
*/
void statement() {
	if (lookahead.code == AVID_T || lookahead.code == SVID_T)assignment_statement();
	else if (lookahead.code == KW_T && lookahead.attribute.get_int == IF)selection_statement();
	else if (lookahead.code == KW_T && lookahead.attribute.get_int == WHILE)iteration_statement();
	else if (lookahead.code == KW_T && lookahead.attribute.get_int == READ)input_statement();
	else if (lookahead.code == KW_T && lookahead.attribute.get_int == WRITE)output_statement();
	else syn_printe();


}
void assignment_statement() {
	assignment_expression(); match(EOS_T, NO_ATTR);
	gen_incode("PLATY: Assignment statement parsed");

}
/*
< assignment expression>->AVID = <arithmetic expression> | SVID = <string expression>
FIRST(< assignment expression>) = { AVID_T , SVID_T }
*/
void assignment_expression() {
	if (lookahead.code == AVID_T) {
		match(AVID_T, NO_ATTR);
		match(ASS_OP_T, NO_ATTR); 
		arithmetic_expression();
		gen_incode("PLATY: Assignment expression (arithmetic) parsed");
	}
	else if (lookahead.code == SVID_T) {
		match(SVID_T, NO_ATTR);
		match(ASS_OP_T, NO_ATTR);
		string_expression();
		gen_incode("PLATY: Assignment expression (string) parsed");
	}
	else syn_printe();

}
/*
<selection statement> ->
IF <pre-condition>  (<conditional expression>) THEN { <opt_statements> } ELSE { <opt_statements> } ;
FIRST(<selection statement>)= {KW_T(IF)}
*/
void selection_statement() {
	match(KW_T, IF); pre_condition(); match(LPR_T, NO_ATTR); logical_OR_expression(); match(RPR_T,NO_ATTR);
	match(KW_T, THEN); match(LBR_T, NO_ATTR); opt_statements(); match(RBR_T, NO_ATTR);
	match(KW_T, ELSE); match(LBR_T, NO_ATTR); opt_statements(); match(RBR_T, NO_ATTR);
	match(EOS_T, NO_ATTR);
	gen_incode("PLATY: Selection statement parsed");
}
/*
<iteration statement> -> WHILE <pre-condition> (<conditional expression>) REPEAT { <statements>};
FIRST(<iteration statement>)= {KW_T(WHILE)}
*/
void iteration_statement() {
	match(KW_T, WHILE); pre_condition(); match(LPR_T, NO_ATTR); logical_OR_expression(); match(RPR_T, NO_ATTR);
	match(KW_T, REPEAT); match(LBR_T, NO_ATTR); statements(); match(RBR_T, NO_ATTR); match(EOS_T, NO_ATTR);
	gen_incode("PLATY: Iteration statement parsed");
}
/*
<pre-condition> -> TRUE | FALSE
FIRST(<pre-condition>)= {KW_T(TRUE) , KW_T (FALSE )}
*/
void pre_condition() {
	if (lookahead.code == KW_T && lookahead.attribute.get_int == TRUE)match(KW_T, TRUE);
	else if (lookahead.code == KW_T && lookahead.attribute.get_int == FALSE)match(KW_T, FALSE);
	else syn_printe();

}
/*
<input statement> -> READ (<variable list>);
FIRST(<input statement>)= {KW_T(READ)}
*/
void input_statement() {
	match(KW_T, READ); match(LPR_T, NO_ATTR); variable_list(); match(RPR_T, NO_ATTR); match(EOS_T, NO_ATTR);
	gen_incode("PLATY: Input statement parsed");
}
/*
<variable list> -> AVID_T<variable list’> | SVID_T<variable list’>
FIRST(<variable list>)= {AVID_T ,SVID_T}
*/
void variable_list() {
	variable_identifier(); variable_list_prime();
	gen_incode("PLATY: Variable list parsed");
}
/*
<variable list’>-> ,<variable identifier><variable list’>|  eps
FIRST(<variable list’>)= {COM_T, eps}
*/
void variable_list_prime() {
	if (lookahead.code != COM_T)
		return;
	match(COM_T, NO_ATTR); variable_identifier(); variable_list_prime();
}
void variable_identifier() {
	if (lookahead.code == AVID_T)match(AVID_T, NO_ATTR);
	else if (lookahead.code == SVID_T)match(SVID_T, NO_ATTR);
	else syn_printe();
}
/*
<output statement> -> WRITE (<output list>);
FIRST(<output statement>)= {KW_T(WRITE)}
*/
void output_statement() {
	match(KW_T, WRITE); match(LPR_T, NO_ATTR); output_list(); match(RPR_T, NO_ATTR); match(EOS_T, NO_ATTR);
	gen_incode("PLATY: Output statement parsed");
}
/*
<output list> -> <variable list> | STR_T | eps
FIRST(<output list>)= {STR_T , AVID_T ,SVID_T, eps}
*/
void output_list() {
	if (lookahead.code == AVID_T || lookahead.code == SVID_T)variable_list();
	else if(lookahead.code == STR_T) {
		match(STR_T, NO_ATTR); gen_incode("PLATY: Output list (string literal) parsed");
	}
	else {
		gen_incode("PLATY: Output list (empty) parsed");
		return;
	}
	
}
/*
<arithmetic expression> - > <unary arithmetic expression>  | <additive arithmetic expression>
FIRST(<arithmetic expression>)= { ART_OP_T( - ), ART_OP_T( + ), AVID_T , FPL_T , INL_T , LPR_T}
*/
void arithmetic_expression() {
	if ((lookahead.code == ART_OP_T && lookahead.attribute.get_int == MINUS) || (lookahead.code == ART_OP_T && lookahead.attribute.get_int == PLUS)) {
		unary_arithmetic_expression();
	}
	else if (lookahead.code == AVID_T || lookahead.code == FPL_T || lookahead.code == LPR_T) {
		additive_arithmetic_expression();
	}
	else syn_printe();
	gen_incode("PLATY: Arithmetic expression parsed");
}
/*
<unary arithmetic expression> -> - <primary arithmetic expression> | + <primary arithmetic expression>
FIRST(<unary arithmetic expression>)= {ART_OP_T( - ), ART_OP_T( + )}
*/
void unary_arithmetic_expression() {
	if (lookahead.code == ART_OP_T && lookahead.attribute.get_int == MINUS) {
		match(ART_OP_T, MINUS); primary_arithmetic_expression();
	}
	else if (lookahead.code == ART_OP_T && lookahead.attribute.get_int == PLUS) {
		match(ART_OP_T, PLUS); primary_arithmetic_expression();
	}
		gen_incode("PLATY: Unary arithmetic expression parsed");
}
/*
<multiplicative arithmetic expression><additive arithmetic expression’>
FIRST(<additive arithmetic expression>)= { AVID_T , FPL_T , INL_T , LPR_T}
*/
void additive_arithmetic_expression() {
	multiplicative_arithmetic_expression(); additive_arithmetic_expression_prime();
	
}
/*
+  <multiplicative arithmetic expression><additive arithmetic expression’>
| -  <multiplicative arithmetic expression><additive arithmetic expression’> | eps
FIRST(<additive arithmetic expression’>)= {ART_OP_T(+) , ART_OP_T(-) , eps}
*/
void additive_arithmetic_expression_prime() {
	if (lookahead.code == ART_OP_T && lookahead.attribute.get_int == PLUS) {
		match(ART_OP_T, PLUS);
		multiplicative_arithmetic_expression(); additive_arithmetic_expression_prime();
	}
	else if (lookahead.code == ART_OP_T && lookahead.attribute.get_int == MINUS) {
		match(ART_OP_T, MINUS);
		multiplicative_arithmetic_expression(); additive_arithmetic_expression_prime();
	}
	else { return; }
	gen_incode("PLATY: Additive arithmetic expression parsed");
}
/*
<primary arithmetic expression><multiplicative arithmetic expression’>
FIRST(<multiplicative arithmetic expression>)= { AVID_T , FPL_T , INL_T , KW_T(LPR_T)}
*/
void multiplicative_arithmetic_expression() {
	primary_arithmetic_expression(); multiplicative_arithmetic_expression_prime();
	
}
/*
*<primary arithmetic expression><multiplicative arithmetic expression’>
| /<primary arithmetic expression><multiplicative arithmetic expression’>
|  eps
FIRST(<multiplicative arithmetic expression’>)= {ART_OP_T( * ) , ART_OP_T( / ) , eps}
*/
void multiplicative_arithmetic_expression_prime() {
	if (lookahead.code == ART_OP_T && lookahead.attribute.get_int == MULT) {
		match(ART_OP_T, MULT);
		primary_arithmetic_expression(); multiplicative_arithmetic_expression_prime();
	}
	else if (lookahead.code == ART_OP_T && lookahead.attribute.get_int == DIV) {
		match(ART_OP_T, DIV);
		primary_arithmetic_expression(); multiplicative_arithmetic_expression_prime();
	}
	else {
	 return;
	}
	gen_incode("PLATY: Multiplicative arithmetic expression parsed");
}
/*
<primary arithmetic expression> -> AVID_T | FPL_T | INL_T | (<arithmetic expression>)
FIRST(<primary arithmetic expression>)= {AVID_T , FPL_T , INL_T , LPR_T}
*/
void primary_arithmetic_expression() {
	if (lookahead.code == AVID_T)match(AVID_T, NO_ATTR);
	else if (lookahead.code == FPL_T)match(FPL_T, NO_ATTR);
	else if (lookahead.code == INL_T)match(INL_T, NO_ATTR);
	else if (lookahead.code == LPR_T) {match(LPR_T, NO_ATTR);
	arithmetic_expression(); match(RPR_T, NO_ATTR);}
	else syn_printe();
	gen_incode("PLATY: Primary arithmetic expression parsed");
}
/*
<string expression>-> <primary string expression><string expression’>
FIRST(<string expression>)= {SVID_T , STR_T}
*/
void string_expression() {
	primary_string_expression();string_expression_prime();
	gen_incode("PLATY: String expression parsed");
}
/*
<string expression’>-> << <primary string expression><string expression’> |  eps
FIRST(<string expression’>)= {SCC_OP_T , eps}
*/
void string_expression_prime() {
	if (lookahead.code == SCC_OP_T) {
		match(SCC_OP_T, NO_ATTR); 
		primary_string_expression(); 
		string_expression_prime();
	} 
}
/*
<primary string expression> -> SVID_T | STR_T
FIRST(<primary string expression>)= {SVID_T , STR_T}
*/
void primary_string_expression() {
	if (lookahead.code == SVID_T)match(SVID_T, STR_T);
	else if (lookahead.code == STR_T)match(STR_T, NO_ATTR);
	else syn_printe();
	gen_incode("PLATY: Primary string expression parsed");
}
/*
<logical OR expression> -> <logical AND expression><logical OR expression’>
FIRST(<logical OR expression>)= { AVID_T , FPL_T, INL_T, SVID_T , STR_T}
*/
void logical_OR_expression() {
	logical_AND_expression(); logical_OR_expression_prime();
	gen_incode("PLATY: Conditional expression parsed");

}
/*
<logical OR expression’>-> .OR. <logical AND expression><logical OR expression’> |  eps
FIRST(<logical OR expression’>)={ LOG_OP_T(.OR.) , eps}
*/
void logical_OR_expression_prime() {
	if (lookahead.code != LOG_OP_T || lookahead.attribute.get_int != OR)return;
	match(LOG_OP_T, OR); logical_AND_expression();logical_OR_expression_prime();
	gen_incode("PLATY: Logical OR expression parsed");
}
/*
<logical AND expression> -> <relational expression><logical AND expression’>
FIRST(<logical AND expression>)= { AVID_T , FPL_T, INL_T, SVID_T , STR_T}
*/
void logical_AND_expression() {
	relational_expression(); logical_AND_expression_prime();
	//gen_incode("PLATY: Logical AND expression parsed");
}
/*
<logical AND expression’>-> .AND.  <relational expression><logical AND expression’> | eps
FIRST(<logical AND expression’>)= {LOG_OP_T(.AND.) , eps}
*/
void logical_AND_expression_prime() {
	if (lookahead.code != LOG_OP_T || lookahead.attribute.get_int != AND) { return; }/*return epsilon*/
	match(LOG_OP_T, AND); relational_expression(); gen_incode("PLATY: Logical AND expression parsed"); logical_AND_expression_prime();
}
/*
<relational expression> ->
<primary a_relational expression> <primary a_relational expression’>
|<primary s_relational expression> <primary s_relational expression’>
<primary a_relational expression> -> AVID_T | FPL_T | INL_T
First (<relational expression>)= { AVID_T , FPL_T, INL_T, SVID_T , STR_T }
*/
void relational_expression() {
	if (lookahead.code == AVID_T || lookahead.code == FPL_T || lookahead.code == INL_T) { primary_arelational_expression(); primary_arelational_expression_prime(); }
	else if (lookahead.code == SVID_T || lookahead.code == STR_T) { primary_srelational_expression(); primary_srelational_expression_prime(); }
	else syn_printe();
	gen_incode("PLATY: Relational expression parsed");
}
/*
<primary a_relational expression’> ->
==  <primary a_relational expression>
| <>  <primary a_relational  expression>
| >  <primary a_relational  expression>
| <  <primary a_relational  expression>
FIRST(<primary a_relational expression’>)={REL_OP_T( == ), REL_OP_T(<>), REL_OP_T( > ), REL_OP_T( < )}
*/
void primary_arelational_expression_prime() {
	if(lookahead.code == REL_OP_T && lookahead.attribute.get_int == EQ){ match(REL_OP_T, EQ); primary_arelational_expression(); }
	else if (lookahead.code == REL_OP_T && lookahead.attribute.get_int == NE) { match(REL_OP_T, NE); primary_arelational_expression(); }
	else if (lookahead.code == REL_OP_T && lookahead.attribute.get_int == GT) { match(REL_OP_T, GT); primary_arelational_expression(); }
	else if (lookahead.code == REL_OP_T && lookahead.attribute.get_int == LT) { match(REL_OP_T, LT); primary_arelational_expression(); }
	else syn_printe();
}
/*
<primary s_relational expression’> ->
==  <primary s_relational  expression>
| <>  <primary s_relational  expression>
| >  <primary s_relational  expression>
| <  <primary s_relational  expression>
First (<primary s_relational expression’>)={ REL_OP_T( == ), REL_OP_T(<>), REL_OP_T( > ), REL_OP_T( < )}
*/
void primary_srelational_expression_prime() {
	if (lookahead.code == REL_OP_T && lookahead.attribute.get_int == EQ) { match(REL_OP_T, EQ); primary_srelational_expression();}
	else if (lookahead.code == REL_OP_T && lookahead.attribute.get_int == NE) { match(REL_OP_T, NE); primary_srelational_expression();}
	else if (lookahead.code == REL_OP_T && lookahead.attribute.get_int == GT) { match(REL_OP_T, GT); primary_srelational_expression();}
	else if (lookahead.code == REL_OP_T && lookahead.attribute.get_int == LT) { match(REL_OP_T, LT); primary_srelational_expression();}
	else syn_printe();
}
/*
<primary a_relational expression> -> AVID_T | FPL_T | INL_T
FIRST(<primary a_relational expression>)={AVID_T , FPL_T, INL_T}
*/
void primary_arelational_expression() {
	if(lookahead.code == AVID_T)match(AVID_T, NO_ATTR);
	else if (lookahead.code == FPL_T)match(FPL_T, NO_ATTR);
	else if (lookahead.code == INL_T)match(INL_T, NO_ATTR);
	else syn_printe();
	gen_incode("PLATY: Primary a_relational expression parsed");
}
/*
<primary s_relational expression> -> <primary string expression>
FIRST(<primary s_relational expression>)={SVID_T , STR_T}
*/
void primary_srelational_expression() {primary_string_expression();
gen_incode("PLATY: Primary s_relational expression parsed");
}
/* error printing function for Assignment 3 (Parser), W19 */
void syn_printe() {
	Token t = lookahead;
	printf("PLATY: Syntax error:  Line:%3d\n", line);
	printf("*****  Token code:%3d Attribute: ", t.code);
	switch (t.code) {
	case  ERR_T: /* ERR_T     0   Error token */
		printf("%s\n", t.attribute.err_lex);
		break;
	case  SEOF_T: /*SEOF_T    1   Source end-of-file token */
		printf("SEOF_T\t\t%d\t\n", t.attribute.seof);
		break;
	case  AVID_T: /* AVID_T    2   Arithmetic Variable identifier token */
	case  SVID_T:/* SVID_T    3  String Variable identifier token */
		printf("%s\n", t.attribute.vid_lex);
		break;
	case  FPL_T: /* FPL_T     4  Floating point literal token */
		printf("%5.1f\n", t.attribute.flt_value);
		break;
	case INL_T: /* INL_T      5   Integer literal token */
		printf("%d\n", t.attribute.get_int);
		break;
	case STR_T:/* STR_T     6   String literal token */
		b_mark(str_LTBL, t.attribute.str_offset);
		printf("%s\n", b_location(str_LTBL));
		break;

	case SCC_OP_T: /* 7   String concatenation operator token */
		printf("NA\n");
		break;

	case  ASS_OP_T:/* ASS_OP_T  8   Assignment operator token */
		printf("NA\n");
		break;
	case  ART_OP_T:/* ART_OP_T  9   Arithmetic operator token */
		printf("%d\n", t.attribute.get_int);
		break;
	case  REL_OP_T: /*REL_OP_T  10   Relational operator token */
		printf("%d\n", t.attribute.get_int);
		break;
	case  LOG_OP_T:/*LOG_OP_T 11  Logical operator token */
		printf("%d\n", t.attribute.get_int);
		break;

	case  LPR_T: /*LPR_T    12  Left parenthesis token */
		printf("NA\n");
		break;
	case  RPR_T: /*RPR_T    13  Right parenthesis token */
		printf("NA\n");
		break;
	case LBR_T: /*    14   Left brace token */
		printf("NA\n");
		break;
	case RBR_T: /*    15  Right brace token */
		printf("NA\n");
		break;

	case KW_T: /*     16   Keyword token */
		printf("%s\n", kw_table[t.attribute.get_int]);
		break;

	case COM_T: /* 17   Comma token */
		printf("NA\n");
		break;
	case EOS_T: /*    18  End of statement *(semi - colon) */
		printf("NA\n");
		break;
	default:
		printf("PLATY: Scanner error: invalid token code: %d\n", t.code);
	}/*end switch*/
}/* end syn_printe()*/