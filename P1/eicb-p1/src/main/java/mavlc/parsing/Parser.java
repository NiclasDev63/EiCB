/*******************************************************************************
 * Copyright (c) 2016-2019 Embedded Systems and Applications Group
 * Department of Computer Science, Technische Universitaet Darmstadt,
 * Hochschulstr. 10, 64289 Darmstadt, Germany.
 *
 * All rights reserved.
 *
 * This software is provided free for educational use only.
 * It may not be used for commercial purposes without the
 * prior written permission of the authors.
 ******************************************************************************/
package mavlc.parsing;

import mavlc.errors.SyntaxError;
import mavlc.syntax.SourceLocation;
import mavlc.syntax.expression.*;
import mavlc.syntax.function.FormalParameter;
import mavlc.syntax.function.Function;
import mavlc.syntax.module.Module;
import mavlc.syntax.record.RecordElementDeclaration;
import mavlc.syntax.record.RecordTypeDeclaration;
import mavlc.syntax.statement.*;
import mavlc.syntax.type.*;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Deque;
import java.util.List;

import static mavlc.parsing.Token.TokenType.*;
import static mavlc.syntax.expression.Compare.Comparison.*;

/* TODO enter group information
 *
 * EiCB group number: 35
 * Names and matriculation numbers of all group members:
 * Niclas Gregor
 * Matias Heredia Novillo
 * Anastasia Paschalidou
 */

/**
 * A recursive-descent parser for MAVL.
 */
public final class Parser {
	
	private final Deque<Token> tokens;
	private Token currentToken;
	
	/**
	 * @param tokens A token stream that was produced by the {@link Scanner}.
	 */
	public Parser(Deque<Token> tokens) {
		this.tokens = tokens;
		currentToken = tokens.poll();
	}
	
	/**
	 * Parses the MAVL grammar's start symbol, Module.
	 *
	 * @return A {@link Module} node that is the root of the AST representing the tokenized input program.
	 * @throws SyntaxError to indicate that an unexpected token was encountered.
	 */
	public Module parse() {
		SourceLocation location = currentToken.sourceLocation;
		
		List<Function> functions = new ArrayList<>();
		List<RecordTypeDeclaration> records = new ArrayList<>();
		while(currentToken.type != EOF) {
			switch(currentToken.type) {
				case FUNCTION:
					functions.add(parseFunction());
					break;
				case RECORD:
					records.add(parseRecordTypeDeclaration());
					break;
				default:
					throw new SyntaxError(currentToken, FUNCTION, RECORD);
			}
		}
		return new Module(location, functions, records);
	}
	
	private String accept(Token.TokenType type) {
		Token t = currentToken;
		if(t.type != type)
			throw new SyntaxError(t, type);
		acceptIt();
		return t.spelling;
	}
	
	private void acceptIt() {
		currentToken = tokens.poll();
		if(currentToken == null || currentToken.type == ERROR)
			throw new SyntaxError(currentToken != null ? currentToken : new Token(EOF, null, -1, -1));
	}
	
	private Function parseFunction() {
		SourceLocation location = currentToken.sourceLocation;

		accept(FUNCTION);
		TypeSpecifier typeSpecifier = parseTypeSpecifier();
		String name = accept(ID);
		
		List<FormalParameter> parameters = new ArrayList<>();
		List<Statement> body = new ArrayList<>();
		
		accept(LPAREN);
		if(currentToken.type != RPAREN) {
			parameters.add(parseFormalParameter());
			while(currentToken.type != RPAREN) {
				accept(COMMA);
				parameters.add(parseFormalParameter());
			}
		}
		accept(RPAREN);
		
		accept(LBRACE);
		while(currentToken.type != RBRACE)
			body.add(parseStatement());
		accept(RBRACE);
		
		return new Function(location, name, typeSpecifier, parameters, body);
	}
	
	private FormalParameter parseFormalParameter() {
		SourceLocation location = currentToken.sourceLocation;
		
		TypeSpecifier typeSpecifier = parseTypeSpecifier();
		String name = accept(ID);
		
		return new FormalParameter(location, name, typeSpecifier);
	}
	
	private RecordTypeDeclaration parseRecordTypeDeclaration() {
		SourceLocation location = currentToken.sourceLocation;
		
		accept(RECORD);
		String name = accept(ID);
		accept(LBRACE);
		List<RecordElementDeclaration> elements = new ArrayList<>();
		// no empty records allowed
		elements.add(parseRecordElementDeclaration());
		while(currentToken.type != RBRACE) {
			elements.add(parseRecordElementDeclaration());
		}
		accept(RBRACE);
		
		return new RecordTypeDeclaration(location, name, elements);
	}
	
	private RecordElementDeclaration parseRecordElementDeclaration() {
		SourceLocation location = currentToken.sourceLocation;
		
		boolean isVariable;
		switch(currentToken.type) {
			case VAL:
				acceptIt();
				isVariable = false;
				break;
			case VAR:
				acceptIt();
				isVariable = true;
				break;
			default:
				throw new SyntaxError(currentToken, VAL, VAR);
		}
		
		TypeSpecifier typeSpecifier = parseTypeSpecifier();
		String name = accept(ID);
		accept(SEMICOLON);
		
		return new RecordElementDeclaration(location, isVariable, typeSpecifier, name);
	}
	
	private IteratorDeclaration parseIteratorDeclaration() {
		// TODO implement (task 1.5)
		SourceLocation location = currentToken.sourceLocation;
		boolean isVariable;
		if (currentToken.type == VAR){
			isVariable = true;
			acceptIt();
		} else if (currentToken.type == VAL){
			isVariable = false;
		} else {
			throw new SyntaxError(currentToken, currentToken.type, VAR, VAL);
		}
		acceptIt();
		TypeSpecifier typeSpecifier = parseTypeSpecifier();
		String id = accept(ID);

		return new IteratorDeclaration(location, id, typeSpecifier, isVariable);
	}
	
	private TypeSpecifier parseTypeSpecifier() {
		SourceLocation location = currentToken.sourceLocation;
		
		boolean vector = false;
		switch(currentToken.type) {
			case INT:
				acceptIt();
				return new IntTypeSpecifier(location);
			case FLOAT:
				acceptIt();
				return new FloatTypeSpecifier(location);
			case BOOL:
				acceptIt();
				return new BoolTypeSpecifier(location);
			case VOID:
				acceptIt();
				return new VoidTypeSpecifier(location);
			case STRING:
				acceptIt();
				return new StringTypeSpecifier(location);
			case VECTOR:
				accept(VECTOR);
				vector = true;
				break;
			case MATRIX:
				accept(MATRIX);
				break;
			case ID:
				String name = accept(ID);
				return new RecordTypeSpecifier(location, name);
			default:
				throw new SyntaxError(currentToken, INT, FLOAT, BOOL, VOID, STRING, VECTOR, MATRIX, ID);
		}
		
		accept(LANGLE);
		TypeSpecifier subtype;
		switch(currentToken.type) {
			case INT:
				subtype = new IntTypeSpecifier(currentToken.sourceLocation);
				break;
			case FLOAT:
				subtype = new FloatTypeSpecifier(currentToken.sourceLocation);
				break;
			default:
				throw new SyntaxError(currentToken, INT, FLOAT);
		}
		acceptIt();
		accept(RANGLE);
		accept(LBRACKET);
		Expression x = parseExpr();
		accept(RBRACKET);
		
		if(vector)
			return new VectorTypeSpecifier(location, subtype, x);
		
		accept(LBRACKET);
		Expression y = parseExpr();
		accept(RBRACKET);
		
		return new MatrixTypeSpecifier(location, subtype, x, y);
	}
	
	private Statement parseStatement() {
		switch(currentToken.type) {
			case VAL:
				return parseValueDef();
			case VAR:
				return parseVarDecl();
			case RETURN:
				return parseReturn();
			case ID:
				return parseAssignOrCall();
			case FOR:
				return parseFor();
			case FOREACH:
				return parseForEach();
			case IF:
				return parseIf();
			case SWITCH:
				return parseSwitch();
			case LBRACE:
				return parseCompound();
			default:
				throw new SyntaxError(currentToken, VAL, VAR, RETURN, ID, FOR, FOREACH, IF, SWITCH, LBRACE);
		}
	}
	
	private ValueDefinition parseValueDef() {
		// TODO DONE implement (task 1.1)
		var location = currentToken.sourceLocation;
		accept(VAL);
		var type = parseTypeSpecifier();
		var id = accept(ID);
		accept(ASSIGN);
		var expr = parseExpr();
		accept(SEMICOLON);

		return new ValueDefinition (location, type, id, expr);
	}
	
	private VariableDeclaration parseVarDecl() {
		// TODO DONE implement (task 1.1)
		var location = currentToken.sourceLocation;
		accept(VAR);
		var type = parseTypeSpecifier();
		var id = accept(ID);
		accept(SEMICOLON);

		return new VariableDeclaration (location, type, id);

	}
	
	private ReturnStatement parseReturn() {
		// TODO DONE implement (task 1.6)
		var location = currentToken.sourceLocation;
		accept(RETURN);
		var expr = parseExpr();
		accept(SEMICOLON);
		return new ReturnStatement(location, expr);
	}
	
	private Statement parseAssignOrCall() {
		// TODO DONE implement (task 1.6)
		var location = currentToken.sourceLocation;
		var id = accept(ID);
		Statement s;
		if (currentToken.type != LPAREN){
			s = parseAssign(id, location);
		} else {
			s = new CallStatement(location, parseCall(id, location));
		}

        accept(SEMICOLON);
	    return s;
	}
	
	private VariableAssignment parseAssign(String name, SourceLocation location) {
		// TODO DONE implement (task 1.1)

		if (currentToken.type == LBRACKET){
			acceptIt();
			var expr = parseExpr();
			accept(RBRACKET);
			
			if (currentToken.type == LBRACKET){
				acceptIt();
				var expr2 = parseExpr();
				accept(RBRACKET);
			}
		} else if (currentToken.type == AT){
			acceptIt();
			accept(ID);
		}
		accept(ASSIGN);

		return new VariableAssignment(location, new LeftHandIdentifier(location, name), parseExpr());


	}
	
	private CallExpression parseCall(String name, SourceLocation location) {
		// TODO DONE implement (task 1.6)
		accept(LPAREN);
		List<Expression> actualParameters = new ArrayList<>();
		actualParameters.add(parseExpr());
			while (currentToken.type == COMMA){
				acceptIt();
				actualParameters.add(parseExpr());
			}
		
		accept(RPAREN);
		return new CallExpression(location, name, actualParameters);

	}
	
	private ForLoop parseFor() {
		// TODO DONE implement (task 1.4)
		SourceLocation sourceLocation = currentToken.sourceLocation;
        
		accept(FOR);
		accept(LPAREN);
		String initVarName = accept(ID);
		accept(ASSIGN);
		Expression initExpression = parseExpr();
		accept(SEMICOLON);
		Expression loopCondition = parseExpr();
		accept(SEMICOLON);
		String incrVarName = accept(ID);
		accept(ASSIGN);
		Expression incrExpression = parseExpr();
		accept(RPAREN);
		Statement body = parseStatement();

		return new ForLoop(sourceLocation, initVarName, initExpression, loopCondition, incrVarName, incrExpression, body);
	}
	
	private ForEachLoop parseForEach() {
		// TODO DONE implement (task 1.5)
		SourceLocation sourceLocation = currentToken.sourceLocation;
		accept(FOREACH);
		accept(LPAREN);
		IteratorDeclaration iterator = parseIteratorDeclaration();
		accept(COLON);
		Expression struct = parseExpr();
		accept(RPAREN);
		Statement body = parseStatement();

		return new ForEachLoop(sourceLocation, iterator, struct, body);
	}
	
	private IfStatement parseIf() {
		SourceLocation location = currentToken.sourceLocation;
		accept(IF);
		accept(LPAREN);
		Expression test = parseExpr();
		accept(RPAREN);
		Statement then = parseStatement();
		if(currentToken.type == ELSE) {
			acceptIt();
			return new IfStatement(location, test, then, parseStatement());
		}
		return new IfStatement(location, test, then);
	}
	
	private SwitchStatement parseSwitch() {
		// TODO DONE implement (task 1.7)
		SourceLocation sourceLocation = currentToken.sourceLocation;
		accept(SWITCH);
		accept(LPAREN);
		Expression condition = parseExpr();
		accept(RPAREN);
		accept(LBRACE);
		List<Case> cases = new ArrayList<>();
		List<Default> defaults = new ArrayList<>();
		while (true){
			if (currentToken.type == CASE){
				cases.add(parseCase());
			} else if (currentToken.type == DEFAULT){
				defaults.add(parseDefault());
			} else {
				break;
			}
		}
		
		accept(RBRACE);

		return new SwitchStatement(sourceLocation, condition, cases, defaults);
	}
	
	private Case parseCase() {
		// TODO DONE implement (task 1.7)
		SourceLocation sourceLocation = currentToken.sourceLocation;
		accept(CASE);
		Expression condition = parseExpr();
		accept(COLON);
		Statement statement = parseStatement();

		return new Case(sourceLocation, condition, statement);
	}
	
	private Default parseDefault() {
		// TODO DONE implement (task 1.7)
		SourceLocation sourceLocation = currentToken.sourceLocation;
		accept(DEFAULT);
		accept(COLON);
		Statement statement = parseStatement();

		return new Default(sourceLocation, statement);
	}
	
	private CompoundStatement parseCompound() {
		SourceLocation location = currentToken.sourceLocation;
		
		List<Statement> statements = new ArrayList<>();
		accept(LBRACE);
		while(currentToken.type != RBRACE)
			statements.add(parseStatement());
		accept(RBRACE);
		
		return new CompoundStatement(location, statements);
	}
	
	private Expression parseExpr() {
		return parseSelect();
	}
	
	private Expression parseSelect() {
		SourceLocation location = currentToken.sourceLocation;
		
		Expression cond = parseOr();
		if(currentToken.type == QMARK) {
			acceptIt();
			Expression trueCase = parseOr();
			accept(COLON);
			Expression falseCase = parseOr();
			return new SelectExpression(location, cond, trueCase, falseCase);
		}
		return cond;
	}
	
	private Expression parseOr() {
		SourceLocation location = currentToken.sourceLocation;
		
		Expression x = parseAnd();
		while(currentToken.type == OR) {
			acceptIt();
			x = new Or(location, x, parseAnd());
		}
		return x;
	}
	
	private Expression parseAnd() {
		SourceLocation location = currentToken.sourceLocation;
		
		Expression x = parseNot();
		while(currentToken.type == AND) {
			acceptIt();
			x = new And(location, x, parseNot());
		}
		return x;
	}
	
	private Expression parseNot() {
		SourceLocation location = currentToken.sourceLocation;
		
		if(currentToken.type == NOT) {
			acceptIt();
			// TODO DONE replace null by call to the appropriate parse method (task 1.2)
			return new Not(location, parseCompare());
		}
		// TODO DONE replace null by call to the appropriate parse method (task 1.2)
		return parseCompare();
	}
	
	private Expression parseCompare() {
		// TODO DONE implement (task 1.2)
		SourceLocation location = currentToken.sourceLocation;

		Expression x = parseAddSub();

		Compare.Comparison comparison;

		switch (currentToken.type) {
			case LANGLE:
				acceptIt();
				comparison = LESS;
				x = new Compare(location, x, parseAddSub(), comparison);
				break;
			case RANGLE:
				acceptIt();
			    comparison = GREATER;
				x = new Compare(location, x, parseAddSub(), comparison);
				break;
			case CMPLE:
				acceptIt();
			    comparison = LESS_EQUAL;
				x = new Compare(location, x, parseAddSub(), comparison);
				break;
			case CMPGE:
				acceptIt();
			    comparison = GREATER_EQUAL;
				x = new Compare(location, x, parseAddSub(), comparison);
				break;
			case CMPEQ:
				acceptIt();
			    comparison = EQUAL;
				x = new Compare(location, x, parseAddSub(), comparison);
				break;
			case CMPNE:
				acceptIt();
			    comparison = NOT_EQUAL;
				x = new Compare(location, x, parseAddSub(), comparison);
				break;

		}

		return x;
	}
	
	private Expression parseAddSub() {
		// TODO DONE implement (task 1.2)
		SourceLocation location = currentToken.sourceLocation;

		Expression x = parseMulDiv();

		while(currentToken.type == ADD || currentToken.type == SUB){

			switch (currentToken.type) {
				case ADD:
					acceptIt();
					x = new Addition(location, x, parseMulDiv());
					break;
			    case SUB:
				    acceptIt();
					x = new Subtraction(location, x, parseMulDiv());
			}
		}

		return x;

	}
	
	private Expression parseMulDiv() {
		// TODO DONE implement (task 1.2)
		SourceLocation location = currentToken.sourceLocation;

		Expression x = parseUnaryMinus();

		while(currentToken.type == MULT || currentToken.type == DIV){
			switch (currentToken.type) {
				case MULT:
					acceptIt();
					x = new Multiplication(location, x, parseUnaryMinus());
					break;
			
				case DIV:
				    acceptIt();
					x = new Division(location, x, parseUnaryMinus());
					break;
			}
		}
		return x;
	}
	
	private Expression parseUnaryMinus() {
		SourceLocation location = currentToken.sourceLocation;
		
		if(currentToken.type == SUB) {
			acceptIt();
			// TODO DONE replace null by call to the appropriate parse method (task 1.2)
			return new UnaryMinus(location, parseExponentiation());
		}
		// TODO DONE replace null by call to the appropriate parse method (task 1.2)
		return parseExponentiation();
	}
	
	private Expression parseExponentiation() {
		// TODO DONE implement (task 1.2)
		SourceLocation location = currentToken.sourceLocation;

		Expression x = parseDotProd();
		if (currentToken.type == EXP){
			acceptIt();
			x = new Exponentiation(location, x, parseDotProd());
		}
		return x;
	}
	
	private Expression parseDotProd() {
		SourceLocation location = currentToken.sourceLocation;
		
		Expression x = parseMatrixMul();
		while(currentToken.type == DOTPROD) {
			acceptIt();
			x = new DotProduct(location, x, parseMatrixMul());
		}
		return x;
	}
	
	private Expression parseMatrixMul() {
		SourceLocation location = currentToken.sourceLocation;
		
		Expression x = parseTranspose();
		while(currentToken.type == MATMULT) {
			acceptIt();
			x = new MatrixMultiplication(location, x, parseTranspose());
		}
		return x;
	}
	
	private Expression parseTranspose() {
		SourceLocation location = currentToken.sourceLocation;
		
		if(currentToken.type == TRANSPOSE) {
			acceptIt();
			// TODO DONE replace null by call to the appropriate parse method (task 1.2)
			 return new MatrixTranspose(location, parseDim());
		}
		// TODO DONE replace null by call to the appropriate parse method (task 1.2)
		return parseDim();
	}
	
	private Expression parseDim() {
		SourceLocation location = currentToken.sourceLocation;
		
		Expression x = parseSubRange();
		switch(currentToken.type) {
			case ROWS:
				acceptIt();
				return new MatrixRows(location, x);
			case COLS:
				acceptIt();
				return new MatrixCols(location, x);
			case DIM:
				acceptIt();
				return new VectorDimension(location, x);
			default:
				return x;
		}
	}
	
	private Expression parseSubRange() {
		SourceLocation location = currentToken.sourceLocation;
		
		Expression x = parseElementSelect();
		
		if(currentToken.type == LBRACE) {
			acceptIt();
			Expression xStartIndex = parseExpr();
			accept(COLON);
			Expression xBaseIndex = parseExpr();
			accept(COLON);
			Expression xEndIndex = parseExpr();
			accept(RBRACE);
			if(currentToken.type != LBRACE)
				return new SubVector(location, x, xBaseIndex, xStartIndex, xEndIndex);
			
			accept(LBRACE);
			Expression yStartIndex = parseExpr();
			accept(COLON);
			Expression yBaseIndex = parseExpr();
			accept(COLON);
			Expression yEndIndex = parseExpr();
			accept(RBRACE);
			return new SubMatrix(location, x, xBaseIndex, xStartIndex, xEndIndex, yBaseIndex, yStartIndex, yEndIndex);
		}
		
		return x;
	}
	
	private Expression parseElementSelect() {
		// TODO DONE implement (task 1.3)
		SourceLocation location = currentToken.sourceLocation;
		Expression x = parseRecordElementSelect();
		while(currentToken.type == LBRACKET){
			acceptIt();
			Expression y = parseExpr();
			accept(RBRACKET);
			x = new ElementSelect(location, x, y);
		}
		return x;
	}
	
	private Expression parseRecordElementSelect() {
		SourceLocation location = currentToken.sourceLocation;
		
		Expression x = parseAtom();
		
		if(currentToken.type == AT) {
			accept(AT);
			String elementName = accept(ID);
			x = new RecordElementSelect(location, x, elementName);
		}
		
		return x;
	}
	
	private Expression parseAtom() {
		SourceLocation location = currentToken.sourceLocation;
		
		switch(currentToken.type) {
			case INTLIT:
				return new IntValue(location, parseIntLit());
			case FLOATLIT:
				return new FloatValue(location, parseFloatLit());
			case BOOLLIT:
				return new BoolValue(location, parseBoolLit());
			case STRINGLIT:
				return new StringValue(location, accept(STRINGLIT));
			default: /* check other cases below */
		}
		
		if(currentToken.type == ID) {
			String name = accept(ID);
			if(currentToken.type != LPAREN) {
				return new IdentifierReference(location, name);
				
			} else {
				return parseCall(name, location);
			}
		}
		
		if(currentToken.type == LPAREN) {
			acceptIt();
			Expression x = parseExpr();
			accept(RPAREN);
			return x;
		}
		
		if(currentToken.type == AT) {
			acceptIt();
			String name = accept(ID);
			return new RecordInit(location, name, parseInitializerList());
		}
		
		if(currentToken.type == LBRACKET) {
			return new StructureInit(location, parseInitializerList());
		}
		
		throw new SyntaxError(currentToken, INTLIT, FLOATLIT, BOOLLIT, STRINGLIT, ID, LPAREN, LBRACKET, AT);
	}
	
	private List<Expression> parseInitializerList() {
		List<Expression> elements = new ArrayList<>();
		
		accept(LBRACKET);
		elements.add(parseExpr());
		while(currentToken.type == COMMA) {
			accept(COMMA);
			elements.add(parseExpr());
		}
		accept(RBRACKET);
		
		return elements;
	}
	
	private int parseIntLit() {
		return Integer.parseInt(accept(INTLIT));
	}
	
	private float parseFloatLit() {
		return Float.parseFloat(accept(FLOATLIT));
	}
	
	private boolean parseBoolLit() {
		return Boolean.parseBoolean(accept(BOOLLIT));
	}
}
